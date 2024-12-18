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
    procedure :: has => opti_sim_t_has
    procedure :: add => opti_sim_t_add
    ! ToDo only interface public, other private
    procedure :: opti_sim_t_allocate_1d
    procedure :: opti_sim_t_allocate_2d
    procedure :: opti_sim_t_allocate_3d
    procedure :: opti_sim_t_allocate_4d
    procedure :: opti_sim_t_allocate_5d
    procedure :: opti_sim_t_set_pointer_1d
    procedure :: opti_sim_t_set_pointer_2d
    procedure :: opti_sim_t_set_pointer_3d
    procedure :: opti_sim_t_set_pointer_4d
    procedure :: opti_sim_t_set_pointer_5d
    ! ToDo: destructor
    generic   :: set_pointer => opti_sim_t_set_pointer_1d, opti_sim_t_set_pointer_2d, &
      opti_sim_t_set_pointer_3d, opti_sim_t_set_pointer_4d, opti_sim_t_set_pointer_5d
    generic   :: allocate => opti_sim_t_allocate_1d, opti_sim_t_allocate_2d, &
      opti_sim_t_allocate_3d, opti_sim_t_allocate_4d, opti_sim_t_allocate_5d
  end type sim_data_t

  type sim_var_t
    real(dp), dimension(:),             allocatable :: data_1d
    real(dp), dimension(:, :),          allocatable :: data_2d
    real(dp), dimension(:, :, :),       allocatable :: data_3d
    real(dp), dimension(:, :, :, :),    allocatable :: data_4d
    real(dp), dimension(:, :, :, :, :), allocatable :: data_5d
    character(256)                                  :: name
    integer(i4)                                     :: ndim
    integer(i4)                                     :: time_avg_selector = 1_i4 !< time averaging: -3 yearly, -2 monthly, -1 daily,
                                                                                !< 0 total, n every n timestep
    ! contains
    ! procedure :: add => opti_sim_single_t_add
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

  ! ToDo: When to charater(*) and character(256)?
  pure logical function opti_sim_t_has(this, name)
    class(sim_data_t), intent(in) :: this
    character(*), intent(in)    :: name

    integer(i4) :: i

    opti_sim_t_has = .false.

    do i = 1, size(this%variables)
      if (trim(this%variables(i)%name) == trim(name)) opti_sim_t_has = .true.
    end do
  end function opti_sim_t_has

  subroutine opti_sim_t_add(this, name, dim, time_avg_selector)
    class(sim_data_t), intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in) :: dim
    integer(i4), optional, intent(in) :: time_avg_selector

    type(sim_var_t) :: add_data

    ! ToDo: Why name in type 256 and in input var *?
    add_data%name = name
    add_data%ndim = dim
    if (present(time_avg_selector)) add_data%time_avg_selector = time_avg_selector
    ! ToDo: is the if case needed?
    ! Tested: the else case works
    if (allocated(this%variables)) then
      this%variables = [this%variables, add_data]
    else
      allocate(this%variables(1))
      this%variables(1)=add_data
    end if

  end subroutine opti_sim_t_add

  subroutine opti_sim_t_allocate_1d(this, name, dim1)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in)  :: dim1

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        allocate(this%variables(i)%data_1d(dim1))
      end if
    end do
  end subroutine opti_sim_t_allocate_1d

  subroutine opti_sim_t_allocate_2d(this, name, dim1, dim2)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in)  :: dim1
    integer(i4), intent(in)  :: dim2

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        allocate(this%variables(i)%data_2d(dim1, dim2))
      end if
    end do
  end subroutine opti_sim_t_allocate_2d

  subroutine opti_sim_t_allocate_3d(this, name, dim1, dim2, dim3)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in)  :: dim1
    integer(i4), intent(in)  :: dim2
    integer(i4), intent(in)  :: dim3

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        allocate(this%variables(i)%data_3d(dim1, dim2, dim3))
      end if
    end do
  end subroutine opti_sim_t_allocate_3d

  subroutine opti_sim_t_allocate_4d(this, name, dim1, dim2, dim3, dim4)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in)  :: dim1
    integer(i4), intent(in)  :: dim2
    integer(i4), intent(in)  :: dim3
    integer(i4), intent(in)  :: dim4

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        allocate(this%variables(i)%data_4d(dim1, dim2, dim3, dim4))
      end if
    end do
  end subroutine opti_sim_t_allocate_4d

  subroutine opti_sim_t_allocate_5d(this, name, dim1, dim2, dim3, dim4, dim5)
    class(sim_data_t), target, intent(inout) :: this
    character(*), intent(in)    :: name
    integer(i4), intent(in)  :: dim1
    integer(i4), intent(in)  :: dim2
    integer(i4), intent(in)  :: dim3
    integer(i4), intent(in)  :: dim4
    integer(i4), intent(in)  :: dim5

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        allocate(this%variables(i)%data_5d(dim1, dim2, dim3, dim4, dim5))
      end if
    end do
  end subroutine opti_sim_t_allocate_5d

  ! ToDo: generate with fypp
  ! ToDo: switch ptr with name
  subroutine opti_sim_t_set_pointer_1d(this, ptr, name)
    class(sim_data_t), target, intent(in) :: this
    real(dp), dimension(:), pointer, intent(inout) :: ptr
    character(*), intent(in)    :: name

    integer(i4) :: i

    ! ToDo: loop -> subroutine get_id
    ! i = this%get_id(name)
    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        ptr => this%variables(i)%data_1d
        ! ToDo: exit
      end if
    end do
  end subroutine opti_sim_t_set_pointer_1d

  subroutine opti_sim_t_set_pointer_2d(this, ptr, name)
    class(sim_data_t), target, intent(in) :: this
    real(dp), dimension(:,:), pointer :: ptr
    character(*), intent(in)    :: name

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        ptr => this%variables(i)%data_2d
      end if
    end do
  end subroutine opti_sim_t_set_pointer_2d

  subroutine opti_sim_t_set_pointer_3d(this, ptr, name)
    class(sim_data_t), target, intent(in) :: this
    real(dp), dimension(:,:,:), pointer, intent(inout) :: ptr
    character(*), intent(in)    :: name

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        ptr => this%variables(i)%data_3d
      end if
    end do
  end subroutine opti_sim_t_set_pointer_3d

  subroutine opti_sim_t_set_pointer_4d(this, ptr, name)
    class(sim_data_t), target, intent(in) :: this
    real(dp), dimension(:,:,:,:), pointer, intent(inout) :: ptr
    character(*), intent(in)    :: name

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        ptr => this%variables(i)%data_4d
      end if
    end do
  end subroutine opti_sim_t_set_pointer_4d

  subroutine opti_sim_t_set_pointer_5d(this, ptr, name)
    class(sim_data_t), target, intent(in) :: this
    real(dp), dimension(:,:,:,:,:), pointer, intent(inout) :: ptr
    character(*), intent(in)    :: name

    integer(i4) :: i

    do i = 1, size(this%variables)
      if (this%variables(i)%name == name) then
        ptr => this%variables(i)%data_5d
      end if
    end do
  end subroutine opti_sim_t_set_pointer_5d

  ! ToDo: Pass only shape instead of optidataObs
  subroutine optidata_sim_init(this, optidataObs)
    class(optidata_sim), intent(inout) :: this
    type(optidata),      intent(in)    :: optidataObs

    allocate(this%dataSim(size(optidataObs%dataObs, dim = 1), size(optidataObs%dataObs, dim = 2)))
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

END MODULE mo_optimization_types
