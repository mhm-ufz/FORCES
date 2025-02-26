!> \file mo_optimization_types.f90
!> \brief \copybrief mo_optimization_types
!> \details \copydetails mo_optimization_types

!> \brief Type definitions for optimization routines
!> \author Maren Kaluza
!> \date Nov 2019
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_optimization_types
  use mo_kind, only : i4, dp
  use mo_message, only : error_message
  use mo_string_utils, only : num2str

  IMPLICIT NONE

  public :: optidata, optidata_sim, config_t, sim_data_t

  private

 ! type variables_optidata_sim
 !   integer(i4), public, dimension(:), allocatable     :: opti_domain_indices
 !   real(dp), public,    dimension(:, :), allocatable  :: runoff     !< dim1=time dim2=gauge
 !   type(optidata_sim), public, dimension(:), allocatable  :: smOptiSim         !< dim1=ncells, dim2=time
 !   type(optidata_sim), public, dimension(:), allocatable  :: neutronsOptiSim   !< dim1=ncells, dim2=time
 !   type(optidata_sim), public, dimension(:), allocatable  :: etOptiSim         !< dim1=ncells, dim2=time
 !   type(optidata_sim), public, dimension(:), allocatable  :: twsOptiSim        !< dim1=ncells, dim2=time
 !   real(dp), public, dimension(:, :), allocatable :: lake_level    !< dim1=time dim2=lake
 !   real(dp), public, dimension(:, :), allocatable :: lake_volume   !< dim1=time dim2=lake
 !   real(dp), public, dimension(:, :), allocatable :: lake_area     !< dim1=time dim2=lake
 !   real(dp), public, dimension(:, :), allocatable :: lake_spill    !< dim1=time dim2=lake
 !   real(dp), public, dimension(:, :), allocatable :: lake_outflow  !< dim1=time dim2=lake
 !   real(dp), public, dimension(:), allocatable :: BFI           !< baseflow index, dim1=domainID
 ! end type variables_optidata_sim

  type config_t
    real(dp),    dimension(:), allocatable :: parameters
    integer(i4), dimension(:), allocatable :: opti_indices
  end type config_t

! ToDo: documentation like mo_cli

  type sim_data_t
    type(sim_var_t), dimension(:), allocatable :: variables
  contains
    procedure, public :: has => sim_data_has
    procedure, public :: add => sim_data_add
    procedure, public :: allocate => sim_data_allocate
    procedure, private :: get_id => sim_data_get_id
    ! One could create these similar procedures
    ! with fypp, for example if more dimensions are needed.
    ! Or you could read about 'assumed rank'.
    procedure, private :: sim_data_set_pointer_1d
    procedure, private :: sim_data_set_pointer_2d
    procedure, private :: sim_data_set_pointer_3d
    procedure, private :: sim_data_set_pointer_4d
    procedure, private :: sim_data_set_pointer_5d
    ! ToDo: destructor
    final :: sim_data_destructor
    generic, public   :: set_pointer => sim_data_set_pointer_1d, sim_data_set_pointer_2d, &
      sim_data_set_pointer_3d, sim_data_set_pointer_4d, sim_data_set_pointer_5d
  end type sim_data_t

  type sim_var_t
    real(dp), dimension(:),             allocatable :: data_1d
    real(dp), dimension(:, :),          allocatable :: data_2d
    real(dp), dimension(:, :, :),       allocatable :: data_3d
    real(dp), dimension(:, :, :, :),    allocatable :: data_4d
    real(dp), dimension(:, :, :, :, :), allocatable :: data_5d
    character(:),                       allocatable :: name
    integer(i4)                                     :: ndim
    integer(i4)                                     :: time_avg_selector = 1_i4 !< time averaging: -3 yearly, -2 monthly, -1 daily,
                                                                                !< 0 total, n every n timestep
  contains
    procedure, public :: is_allocated => sim_var_is_allocated
    final :: sim_var_destructor
  end type sim_var_t

  !> \brief optional data, such as sm, neutrons, et, tws
  !> \details data type for observed data, providing metadata
  !! for simulated data
  !! dim1 = number grid cells L1
  !! dim2 = number of meteorological time steps
  type optidata
    real(dp), dimension(:, :), allocatable    :: dataObs !< observed data
    logical, dimension(:, :), allocatable     :: maskObs !< mask of observed data
    character(256)                            :: dir !< directory where to read opti data
    integer(i4)                               :: timeStepInput !< time step of optional data
    character(256)                            :: varname !< variable name
  end type optidata

  !> \brief type for simulated optional data
  type optidata_sim
    real(dp), dimension(:, :), allocatable    :: dataSim         !< simulation data
    integer(i4)                               :: averageTimestep !< the current timestep
                                                                 !< the simulated opti data is written to
    integer(i4)                               :: averageCounter  !< set to 0 on average, incremented on add

    contains
    procedure :: init => optidata_sim_init
    procedure :: destroy => optidata_sim_destroy
    procedure :: increment_counter => optidata_sim_increment_counter
    procedure :: add => optidata_sim_add
    procedure :: average => optidata_sim_average
    procedure :: average_per_timestep => optidata_sim_average_per_timestep
    procedure :: average_add => optidata_sim_average_add
  end type optidata_sim

  contains

  subroutine check_data_shape(data_shape, ndim)
    integer(i4), dimension(:), intent(in)    :: data_shape
    integer(i4), intent(in)    :: ndim
    if (size(data_shape) /= ndim) &
    call error_message( &
      'check_data_shape: given data-shape (size ', num2str(size(data_shape)), ') not matching ndim (', num2str(ndim), ').')
  end subroutine check_data_shape

  subroutine check_pointer_ndim(pointer_ndim, data_ndim)
    integer(i4), intent(in) :: pointer_ndim
    integer(i4), intent(in) :: data_ndim
    if (pointer_ndim /= data_ndim) &
      call error_message("check_pointer_ndim: pointer is ", num2str(pointer_ndim),"D but data is ", num2str(data_ndim), "D.")
  end subroutine check_pointer_ndim

  subroutine check_allocated(var)
    class(sim_var_t), intent(in) :: var
    if (.not. var%is_allocated()) &
      call error_message('check_allocated: data for "', var%name ,'" is not allocated')
  end subroutine check_allocated

  logical function sim_data_has(this, name)
    class(sim_data_t), intent(in) :: this
    character(*), intent(in)      :: name
    sim_data_has = this%get_id(name) > 0
  end function sim_data_has

  subroutine sim_data_add(this, name, ndim, data_shape, time_avg_selector)
    class(sim_data_t), intent(inout)                :: this
    character(*), intent(in)                        :: name
    integer(i4), optional, intent(in)               :: ndim
    integer(i4), dimension(:), optional, intent(in) :: data_shape
    integer(i4), optional, intent(in)               :: time_avg_selector

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
    if (present(time_avg_selector)) add_data%time_avg_selector = time_avg_selector
    ! ToDo: is the if case needed?
    ! Tested: the else case works
    if (allocated(this%variables)) then
      this%variables = [this%variables, add_data]
    else
      allocate(this%variables(1))
      this%variables(1)=add_data
    end if

    if (present(data_shape)) call this%allocate(name, data_shape)

  end subroutine sim_data_add

  subroutine sim_data_allocate(this, name, data_shape)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)                 :: name
    integer(i4), dimension(:), intent(in)    :: data_shape

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

  integer(i4) function sim_data_get_id(this, name, raise)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)      :: name
    logical, intent(in), optional :: raise

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

  subroutine sim_data_set_pointer_1d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)    :: name
    real(dp), dimension(:), pointer, intent(inout) :: ptr

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(1, this%variables(i)%ndim)
    ptr => this%variables(i)%data_1d
  end subroutine sim_data_set_pointer_1d

  subroutine sim_data_set_pointer_2d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)    :: name
    real(dp), dimension(:,:), pointer :: ptr

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(2, this%variables(i)%ndim)
    ptr => this%variables(i)%data_2d
  end subroutine sim_data_set_pointer_2d

  subroutine sim_data_set_pointer_3d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)    :: name
    real(dp), dimension(:,:,:), pointer, intent(inout) :: ptr

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(3, this%variables(i)%ndim)
    ptr => this%variables(i)%data_3d
  end subroutine sim_data_set_pointer_3d

  subroutine sim_data_set_pointer_4d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)    :: name
    real(dp), dimension(:,:,:,:), pointer, intent(inout) :: ptr

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(4, this%variables(i)%ndim)
    ptr => this%variables(i)%data_4d
  end subroutine sim_data_set_pointer_4d

  subroutine sim_data_set_pointer_5d(this, name, ptr)
    class(sim_data_t), target, intent(in) :: this
    character(*), intent(in)    :: name
    real(dp), dimension(:,:,:,:,:), pointer, intent(inout) :: ptr

    integer(i4) :: i

    i = this%get_id(name, raise=.true.)
    call check_allocated(this%variables(i))
    call check_pointer_ndim(5, this%variables(i)%ndim)
    ptr => this%variables(i)%data_5d
  end subroutine sim_data_set_pointer_5d

  logical function sim_var_is_allocated(this)
    class(sim_var_t), intent(in) :: this
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

  subroutine optidata_sim_init(this, data_shape)
    class(optidata_sim), intent(inout) :: this
    integer(i4), dimension(2) :: data_shape !< the shape of the simulated data
                                            !< may often be the dimensions of optidataObs%dataObs

    allocate(this%dataSim(data_shape(1), data_shape(2)))
    this%dataSim(:, :) = 0.0_dp ! has to be intialized with zero because later summation
    this%averageTimestep = 1
    this%averageCounter = 0
  end subroutine optidata_sim_init

  subroutine optidata_sim_destroy(this)
    class(optidata_sim), intent(inout) :: this

    deallocate(this%dataSim)
  end subroutine optidata_sim_destroy

  subroutine optidata_sim_increment_counter(this, timeStepInput, is_new_day, is_new_month, is_new_year)
    class(optidata_sim), intent(inout) :: this
    integer(i4),         intent(in)    :: timeStepInput
    logical,             intent(in)    :: is_new_day
    logical,             intent(in)    :: is_new_month
    logical,             intent(in)    :: is_new_year

    select case(timeStepInput)
    case(-1) ! daily
      if (is_new_day)   then
        this%averageTimestep = this%averageTimestep + 1
      end if
    case(-2) ! monthly
      if (is_new_month) then
        this%averageTimestep = this%averageTimestep + 1
      end if
    case(-3) ! yearly
      if (is_new_year)  then
        this%averageTimestep = this%averageTimestep + 1
      end if
    end select

  end subroutine optidata_sim_increment_counter

  subroutine optidata_sim_add(this, data_sim)
    class(optidata_sim),    intent(inout) :: this
    real(dp), dimension(:), intent(in)    :: data_sim

    this%dataSim(:, this%averageTimestep) = &
            this%dataSim(:, this%averageTimestep) + data_sim(:)
  end subroutine optidata_sim_add

  subroutine optidata_sim_average(this)
    class(optidata_sim), intent(inout) :: this

    this%dataSim(:, this%averageTimestep) = &
            this%dataSim(:, this%averageTimestep) / real(this%averageCounter, dp)
    this%averageTimestep = this%averageTimestep + 1
    this%averageCounter = 0
  end subroutine optidata_sim_average

  subroutine optidata_sim_average_per_timestep(this, timeStepInput, is_new_day, is_new_month, is_new_year)
    class(optidata_sim), intent(inout) :: this
    integer(i4),         intent(in)    :: timeStepInput
    logical,             intent(in)    :: is_new_day
    logical,             intent(in)    :: is_new_month
    logical,             intent(in)    :: is_new_year

    select case(timeStepInput)
    case(-1) ! daily
      if (is_new_day)   then
        call this%average()
      end if
    case(-2) ! monthly
      if (is_new_month) then
        call this%average()
      end if
    case(-3) ! yearly
      if (is_new_year)  then
        call this%average()
      end if
    end select
  end subroutine optidata_sim_average_per_timestep

  subroutine optidata_sim_average_add(this, data_sim)
    class(optidata_sim),    intent(inout) :: this
    real(dp), dimension(:), intent(in)    :: data_sim

    call this%add(data_sim(:))
    this%averageCounter = this%averageCounter + 1
  end subroutine optidata_sim_average_add

  subroutine sim_data_destructor(this)
    type(sim_data_t),    intent(inout) :: this

    if (allocated(this%variables)) deallocate(this%variables)

  end subroutine sim_data_destructor

  subroutine sim_var_destructor(this)
    type(sim_var_t),    intent(inout) :: this

    if (allocated(this%name)) deallocate(this%name)
    if (allocated(this%data_1d)) deallocate(this%data_1d)
    if (allocated(this%data_2d)) deallocate(this%data_2d)
    if (allocated(this%data_3d)) deallocate(this%data_3d)
    if (allocated(this%data_4d)) deallocate(this%data_4d)
    if (allocated(this%data_5d)) deallocate(this%data_5d)

  end subroutine sim_var_destructor

END MODULE mo_optimization_types
