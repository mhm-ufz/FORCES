!> \file mo_opt_eval_utils.f90
!> \copydoc mo_opt_eval_utils

!> \brief Type definitions and interfaces for optimization routines based on eval functions.
!> \author Maren Kaluza
!> \date Nov 2019
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_opt_eval_utils
  use mo_kind, only : i4, dp
  use mo_message, only : error_message
  use mo_string_utils, only : num2str

  IMPLICIT NONE

  public :: eval_interface
  public :: objective_interface
  public :: config_t
  public :: sim_data_t

  private

  !> \class   config_t
  !> \brief   Type to hold the configuration of an evaluation function.
  type config_t
    real(dp),    dimension(:), allocatable :: parameters  !< parameters for the evaluation function
    integer(i4), dimension(:), allocatable :: opti_indices  !< optimization indices for the evaluation function (used for MPI)
  end type config_t

  !> \class   sim_data_t
  !> \brief   Type to hold all the simulated data of an evaluation function.
  type sim_data_t
    type(sim_var_t), dimension(:), allocatable :: variables  !< array of all simulated variables
  contains
    procedure, public :: has => sim_data_has
    procedure, public :: add => sim_data_add
    procedure, public :: allocate => sim_data_allocate
    procedure, private :: get_id => sim_data_get_id
    ! Use fypp or 'assumed rank' to simplify these overloaded procedures.
    procedure, private :: sim_data_set_pointer_1d
    procedure, private :: sim_data_set_pointer_2d
    procedure, private :: sim_data_set_pointer_3d
    procedure, private :: sim_data_set_pointer_4d
    procedure, private :: sim_data_set_pointer_5d
    generic, public   :: set_pointer => sim_data_set_pointer_1d, sim_data_set_pointer_2d, &
      sim_data_set_pointer_3d, sim_data_set_pointer_4d, sim_data_set_pointer_5d
    procedure, private :: sim_data_set_data_1d
    procedure, private :: sim_data_set_data_2d
    procedure, private :: sim_data_set_data_3d
    procedure, private :: sim_data_set_data_4d
    procedure, private :: sim_data_set_data_5d
    generic, public   :: set_data => sim_data_set_data_1d, sim_data_set_data_2d, &
      sim_data_set_data_3d, sim_data_set_data_4d, sim_data_set_data_5d
    end type sim_data_t

  !> \class   sim_var_t
  !> \brief   Type to hold a single simulated variable of an evaluation function.
  !> \details Can hold data with 1 to 5 dimensions.
  type sim_var_t
    real(dp), dimension(:),             allocatable :: data_1d  !< 1D data array
    real(dp), dimension(:, :),          allocatable :: data_2d  !< 2D data array
    real(dp), dimension(:, :, :),       allocatable :: data_3d  !< 3D data array
    real(dp), dimension(:, :, :, :),    allocatable :: data_4d  !< 4D data array
    real(dp), dimension(:, :, :, :, :), allocatable :: data_5d  !< 5D data array
    character(:),                       allocatable :: name  !< name of the variable
    integer(i4)                                     :: ndim  !< number of dimensions
  contains
    procedure, public :: is_allocated => sim_var_is_allocated
    procedure, public :: data_shape => sim_var_data_shape
  end type sim_var_t

  !> \brief Interface for evaluation function.
  abstract interface
    subroutine eval_interface(config, sim_data)
      import config_t, sim_data_t
      type(config_t),                                    intent(in)    :: config  !< configuration
      type(sim_data_t), dimension(:), pointer, optional, intent(inout) :: sim_data  !< simulated data
    end subroutine
  end interface

  !> \brief Interface for objective function.
  !> \details The optional arguments are motivated by likelihood objective functions.
  interface
    function objective_interface(parameters, eval, arg1, arg2, arg3)
      use mo_kind, only : dp
      import eval_interface
      real(dp), intent(in), dimension(:) :: parameters  !< parameter set
      procedure(eval_interface), INTENT(IN), pointer :: eval  !< evaluation routine
      real(dp), optional, intent(in) :: arg1  !< optional argument 1
      real(dp), optional, intent(out) :: arg2  !< optional argument 2
      real(dp), optional, intent(out) :: arg3  !< optional argument 3
      real(dp) :: objective_interface
    end function objective_interface
  end interface

  contains

  !> \brief Check given data shape and ndim for consistency.
  subroutine check_data_shape(data_shape, ndim)
    integer(i4), dimension(:), intent(in)    :: data_shape  !< data shape
    integer(i4), intent(in)    :: ndim  !< number of dimensions
    if (size(data_shape) /= ndim) &
    call error_message( &
      'check_data_shape: given data-shape (size ', num2str(size(data_shape)), ') not matching ndim (', num2str(ndim), ').')
  end subroutine check_data_shape

  !> \brief Check given pointer dimension and ndim for consistency.
  subroutine check_pointer_ndim(pointer_ndim, data_ndim)
    integer(i4), intent(in) :: pointer_ndim  !< number of dimensions of the pointer
    integer(i4), intent(in) :: data_ndim  !< number of dimensions of the variable data
    if (pointer_ndim /= data_ndim) &
      call error_message("check_pointer_ndim: pointer is ", num2str(pointer_ndim),"D but data is ", num2str(data_ndim), "D.")
  end subroutine check_pointer_ndim

  !> \brief Check given values dimension and ndim for consistency.
  subroutine check_values_ndim(values_ndim, data_ndim)
    integer(i4), intent(in) :: values_ndim  !< number of dimensions of given values
    integer(i4), intent(in) :: data_ndim  !< number of dimensions of the variable data
    if (values_ndim /= data_ndim) &
      call error_message("check_values_ndim: values are ", num2str(values_ndim),"D but data is ", num2str(data_ndim), "D.")
  end subroutine check_values_ndim

  !> \brief Check given values shape for consistency.
  subroutine check_values_shape(values_shape, data_shape)
    integer(i4), dimension(:), intent(in) :: values_shape  !< shape of given values
    integer(i4), dimension(:), intent(in) :: data_shape  !< shape of the variable data
    if (size(values_shape) /= size(data_shape)) &
      call error_message( &
        "check_values_shape: values are ", num2str(size(values_shape)),"D but data is ", num2str(size(data_shape)), "D.")
    if (any(values_shape /= data_shape)) call error_message("check_values_shape: values and data have different shapes.")

  end subroutine check_values_shape

  !> \brief Check if variable is allocated.
  subroutine check_allocated(var)
    class(sim_var_t), intent(in) :: var  !< variable
    if (.not. var%is_allocated()) &
      call error_message('check_allocated: data for "', var%name ,'" is not allocated')
  end subroutine check_allocated

  !> \brief Check if variable is present in the simulated data.
  !> \return .True. or .False.
  logical function sim_data_has(this, name)
    class(sim_data_t), intent(in) :: this  !< simulated data
    character(*), intent(in)      :: name  !< variable name
    sim_data_has = this%get_id(name) > 0
  end function sim_data_has

  !> \brief Add a new variable to the simulated data.
  subroutine sim_data_add(this, name, ndim, data_shape)
    class(sim_data_t), intent(inout)                :: this  !< simulated data
    character(*), intent(in)                        :: name  !< variable name
    integer(i4), optional, intent(in)               :: ndim  !< number of dimensions
    integer(i4), dimension(:), optional, intent(in) :: data_shape  !< data shape

    type(sim_var_t) :: add_data
    integer(i4) :: ndim_

    if (this%has(name)) call error_message('sim_data_add: variable name "', trim(name), '" already present.')
    if (present(ndim) .eqv. present(data_shape)) then
      if (.not. present(ndim)) then
        call error_message('sim_data_add: either "ndim" or "data_shape" needed')
      else
        call check_data_shape(data_shape, ndim)
      endif
    endif
    if (present(ndim)) then
      ndim_ = ndim
    else
      ndim_ = size(data_shape)
    endif
    add_data%name = trim(name)
    add_data%ndim = ndim_

    if (allocated(this%variables)) then
      this%variables = [this%variables, add_data]
    else
      allocate(this%variables(1))
      this%variables(1)=add_data
    end if

    if (present(data_shape)) call this%allocate(name, data_shape)

  end subroutine sim_data_add

  !> \brief Allocate variable data by name in the simulated data.
  subroutine sim_data_allocate(this, name, data_shape)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)                 :: name  !< variable name
    integer(i4), dimension(:), intent(in)    :: data_shape  !< data shape

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    if (this%variables(i)%is_allocated()) call error_message('sim_data_allocate: data for "', trim(name) ,'" already allocated')
    select case (this%variables(i)%ndim)
      case(1)
        allocate(this%variables(i)%data_1d(data_shape(1)))
      case(2)
        allocate(this%variables(i)%data_2d(data_shape(1), data_shape(2)))
      case(3)
        allocate(this%variables(i)%data_3d(data_shape(1), data_shape(2), data_shape(3)))
      case(4)
        allocate(this%variables(i)%data_4d(data_shape(1), data_shape(2), data_shape(3), data_shape(4)))
      case(5)
        allocate(this%variables(i)%data_5d(data_shape(1), data_shape(2), data_shape(3), data_shape(4), data_shape(5)))
      case default
        call error_message('sim_data_allocate: Allocating simulated data with other dimensions than 1 to 5 is not implemented.')
    end select
  end subroutine sim_data_allocate

  !> \brief Get variable ID in the simulated data array.
  !> \return variable ID as integer
  integer(i4) function sim_data_get_id(this, name, raise)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)      :: name  !< variable name
    logical, intent(in), optional :: raise  !< flag to raise error if variable not present (default: .false.)

    logical :: raise_
    integer(i4) :: i, max_i

    raise_ = .false.
    if (present(raise)) raise_ = raise

    sim_data_get_id = -1
    max_i = 0
    if (allocated(this%variables)) max_i = size(this%variables)

    do i = 1, max_i
      if (trim(this%variables(i)%name) == trim(name)) then
        sim_data_get_id = i
        exit
      end if
    end do

    if (sim_data_get_id == -1 .and. raise_) &
      call error_message('sim_data: The simulated variable name "', trim(name), '" does not exist.')

  end function sim_data_get_id

  !> \brief Set a 1D pointer to variable data by name from the simulated data.
  subroutine sim_data_set_pointer_1d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:), pointer, intent(inout) :: ptr  !< pointer

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(1, this%variables(i)%ndim)
    ptr => this%variables(i)%data_1d
  end subroutine sim_data_set_pointer_1d

  !> \brief Set a 2D pointer to variable data by name from the simulated data.
  subroutine sim_data_set_pointer_2d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:), pointer, intent(inout) :: ptr  !< pointer

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(2, this%variables(i)%ndim)
    ptr => this%variables(i)%data_2d
  end subroutine sim_data_set_pointer_2d

  !> \brief Set a 3D pointer to variable data by name from the simulated data.
  subroutine sim_data_set_pointer_3d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:), pointer, intent(inout) :: ptr  !< pointer

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(3, this%variables(i)%ndim)
    ptr => this%variables(i)%data_3d
  end subroutine sim_data_set_pointer_3d

  !> \brief Set a 4D pointer to variable data by name from the simulated data.
  subroutine sim_data_set_pointer_4d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:,:), pointer, intent(inout) :: ptr  !< pointer

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(4, this%variables(i)%ndim)
    ptr => this%variables(i)%data_4d
  end subroutine sim_data_set_pointer_4d

  !> \brief Set a 5D pointer to variable data by name from the simulated data.
  subroutine sim_data_set_pointer_5d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:,:,:), pointer, intent(inout) :: ptr  !< pointer

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(5, this%variables(i)%ndim)
    ptr => this%variables(i)%data_5d
  end subroutine sim_data_set_pointer_5d

  !> \brief Set a 1D values to variable data by name from the simulated data.
  subroutine sim_data_set_data_1d(this, name, values)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:), intent(in) :: values  !< values to set

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_values_ndim(1, this%variables(i)%ndim)
    if (this%variables(i)%is_allocated()) call check_values_shape(shape(values), this%variables(i)%data_shape())
    this%variables(i)%data_1d = values
  end subroutine sim_data_set_data_1d

  !> \brief Set a 2D values to variable data by name from the simulated data.
  subroutine sim_data_set_data_2d(this, name, values)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:), intent(in) :: values  !< values to set

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_values_ndim(2, this%variables(i)%ndim)
    if (this%variables(i)%is_allocated()) call check_values_shape(shape(values), this%variables(i)%data_shape())
    this%variables(i)%data_2d = values
  end subroutine sim_data_set_data_2d

  !> \brief Set a 3D values to variable data by name from the simulated data.
  subroutine sim_data_set_data_3d(this, name, values)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:), pointer, intent(in) :: values  !< values to set

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_values_ndim(3, this%variables(i)%ndim)
    if (this%variables(i)%is_allocated()) call check_values_shape(shape(values), this%variables(i)%data_shape())
    this%variables(i)%data_3d = values
  end subroutine sim_data_set_data_3d

  !> \brief Set a 4D values to variable data by name from the simulated data.
  subroutine sim_data_set_data_4d(this, name, values)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:,:), pointer, intent(in) :: values  !< values to set

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_values_ndim(4, this%variables(i)%ndim)
    if (this%variables(i)%is_allocated()) call check_values_shape(shape(values), this%variables(i)%data_shape())
    this%variables(i)%data_4d = values
  end subroutine sim_data_set_data_4d

  !> \brief Set a 5D values to variable data by name from the simulated data.
  subroutine sim_data_set_data_5d(this, name, values)
    class(sim_data_t), target, intent(inout) :: this  !< simulated data
    character(*), intent(in)    :: name  !< variable name
    real(dp), dimension(:,:,:,:,:), pointer, intent(in) :: values  !< values to set

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_values_ndim(5, this%variables(i)%ndim)
    if (this%variables(i)%is_allocated()) call check_values_shape(shape(values), this%variables(i)%data_shape())
    this%variables(i)%data_5d = values
  end subroutine sim_data_set_data_5d

  !> \brief Check if variable data is allocated.
  !> \return .True. or .False.
  logical function sim_var_is_allocated(this)
    class(sim_var_t), intent(in) :: this  !< variable
    select case (this%ndim)
      case(1)
        sim_var_is_allocated = allocated(this%data_1d)
      case(2)
        sim_var_is_allocated = allocated(this%data_2d)
      case(3)
        sim_var_is_allocated = allocated(this%data_3d)
      case(4)
        sim_var_is_allocated = allocated(this%data_4d)
      case(5)
        sim_var_is_allocated = allocated(this%data_5d)
      case default
        call error_message('sim_var_is_allocated: ndim is greater than 5.')
    end select
  end function sim_var_is_allocated

  !> \brief Determine variable data shape.
  !> \return data shape
  function sim_var_data_shape(this)
    class(sim_var_t), intent(in) :: this  !< variable
    integer(i4), allocatable, dimension(:) :: sim_var_data_shape
    if (.not. this%is_allocated()) then
      allocate(sim_var_data_shape(0))
    else
      select case (this%ndim)
        case(1)
          sim_var_data_shape = shape(this%data_1d)
        case(2)
          sim_var_data_shape = shape(this%data_2d)
        case(3)
          sim_var_data_shape = shape(this%data_3d)
        case(4)
          sim_var_data_shape = shape(this%data_4d)
        case(5)
          sim_var_data_shape = shape(this%data_5d)
        case default
          call error_message('sim_var_data_shape: ndim is greater than 5.')
      end select
    end if
  end function sim_var_data_shape

END MODULE mo_opt_eval_utils
