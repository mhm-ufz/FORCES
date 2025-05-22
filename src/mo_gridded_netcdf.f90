!> \file mo_gridded_netcdf.f90
!> \copydoc mo_gridded_netcdf

!> \brief Creates NetCDF input or output files for gridded data.
!> \details NetCDF is first initialized and later on variables are put to the NetCDF.
!! \par Examples
!! - \ref 02_nc_output.f90 : \copybrief 02_nc_output.f90
!!   \include 02_nc_output.f90
!> \changelog
!! - David Schaefer       Aug 2015
!!   - major rewrite
!! - Stephan Thober       Oct 2015
!!   - adapted to mRM
!! - O. Rakovec, R. Kumar Nov 2017
!!   - added project description for the netcdf outputs
!! - S. Mueller,          Dec 2022
!!   - unified module for mHM and mRM
!! - S. Mueller,          May 2025
!!   - rewrote for FORCES and use general grid datetime type
!!
!> \authors Matthias Zink
!> \authors Sebastian Müller
!> \date Apr 2013
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_gridded_netcdf

  use mo_kind, only : i4, dp, sp
  use mo_grid, only: grid_t, cartesian
  use mo_constants, only : nodata_dp, nodata_sp
  use mo_netcdf, only : NcDataset, NcDimension, NcVariable
  use mo_datetime, only : datetime, timedelta, delta_from_string
  use mo_list, only : list, key_list
  use mo_message, only : error_message, warn_message

  implicit none

  public :: var, output_dataset, time_units_delta

  private

  !> \name Time Step Indicators
  !> \brief Constants to indicate the time stepping used in the in-/output files.
  !!@{
  integer(i4), public, parameter :: hourly = 1_i4 !< hourly
  integer(i4), public, parameter :: no_time = 0_i4 !< no time dimension available
  integer(i4), public, parameter :: daily = -1_i4 !< daily
  integer(i4), public, parameter :: monthly = -2_i4 !< monthly
  integer(i4), public, parameter :: yearly = -3_i4 !< yearly
  integer(i4), public, parameter :: varying = -9999_i4 !< no uniform time step
  !!@}
  !> \name Time Stamp Locators
  !> \brief Constants to define the timestamp location for the respective time span the in-/output files.
  !!@{
  integer(i4), public, parameter :: start_timestamp = 0_i4 !< timestamp at start of time span
  integer(i4), public, parameter :: center_timestamp = 1_i4 !< timestamp at center of time span
  integer(i4), public, parameter :: end_timestamp = 2_i4 !< timestamp at end of time span
  !!@}

  !> \class var
  !> \brief variable metadata definition for a 2D variable
  type var
    character(:), allocatable :: name          !< variable name in the NetCDF file
    character(:), allocatable :: long_name     !< descriptive variable name
    character(:), allocatable :: standard_name !< standard variable name following CF-Conventions
    character(:), allocatable :: units         !< variable units
    logical :: static = .false.                !< static variable (without time dimension)
    logical :: avg = .false.                   !< average data (only for writing)
  end type var

  !> \class output_variable
  !> \brief netcdf output variable container for a 2D variable
  type, extends(var) :: output_variable
    type(NcVariable) :: nc                     !< NcDataset which contains the variable
    real(dp), allocatable :: data(:)           !< store the data between writes
    type(grid_t), pointer :: grid => null()    !< horizontal grid the data is defined on
    integer(i4) :: counter = 0_i4              !< count the number of updateVariable calls
    logical :: static_written = .false.        !< static variable was written
  contains
    procedure, public :: init => out_var_init
    procedure, public :: update => out_var_update
    procedure, public :: write => out_var_write
  end type output_variable

  !> \class output_dataset
  !> \brief netcdf output dataset handler for gridded data
  !> \details Output dataset handler for static and temporal data.
  !! \par Examples
  !! - \ref 02_nc_output.f90 : \copybrief 02_nc_output.f90
  type output_dataset
    type(grid_t), pointer :: grid => null()       !< horizontal grid the data is defined on
    character(:), allocatable :: path             !< path to the NetCDF file
    type(NcDataset) :: nc                         !< NcDataset to write
    type(output_variable), allocatable :: vars(:) !< store all created (dynamic) variables
    integer(i4) :: nvars                          !< number of variables
    logical :: static                             !< only static variables (without time dimension)
    integer(i4) :: counter = 0_i4                 !< count written time steps
    type(datetime) :: previous_time               !< previous time steps for bounds
    type(datetime) :: start_time                  !< start time for time units
    type(timedelta) :: delta                      !< time step in time units definition
    integer(i4) :: timestamp = end_timestamp      !< time stamp reference (0: begin, 1: center, 2: end of time interval)
    logical :: double_precision = .true.          !< output precision switch for nc files
    integer(i4) :: deflate_level = 6_i4           !< deflate level for compression
  contains
    procedure, public :: init => output_init
    procedure, public :: update => output_update
    procedure, public :: write => output_write
    procedure, public :: write_static => output_write_static
    procedure, public :: close => output_close
  end type output_dataset

contains

  !> \brief variable dtype for single or double precision.
  !> \return "f64" or "f32"
  character(3) function dtype(double_precision)
    implicit none
    logical, intent(in) :: double_precision !< flag to use double precision
    if ( double_precision ) then
      dtype = "f64"
    else
      dtype = "f32"
    end if
  end function dtype

  !> \brief Determine time units delta from time stepping and selected timestamp.
  !> \return "minutes", "hours" or "days"
  function time_units_delta(timestep, timestamp) result(res)
    integer(i4), intent(in), optional :: timestep !< time step (-3, -2, -1, 0, 1 (default), >1)
    integer(i4), intent(in), optional :: timestamp !< time stamp reference (0: begin, 1: center, 2: end of time span (default))
    character(:), allocatable :: res
    integer(i4) :: step = hourly
    integer(i4) :: stamp = end_timestamp
    if (present(timestep)) step = timestep
    if (present(timestamp)) stamp = timestamp
    res = "hours" ! default
    if (stamp == center_timestamp) then
      if (step > 0_i4 .and. mod(step, 2) == 1) res = "minutes"
    else
      if (step < 0_i4) res = "days"
      if (step > 0_i4 .and. mod(step, 24) == 0) res = "days"
    end if
  end function time_units_delta

  !> \brief generate values for the time-dimension depending on given datetimes.
  subroutine time_values(start_time,  previous_time, current_time, delta, timestamp, t_start, t_end, t_stamp)
    type(datetime), intent(in) :: start_time !< starting time in units
    type(datetime), intent(in) :: previous_time !< previous write-out time
    type(datetime), intent(in) :: current_time !< current write-out time
    type(timedelta), intent(in) :: delta !< time delta in units
    integer(i4), intent(in) ::  timestamp !< timestamp location selector
    integer(i4), intent(out) ::  t_start !< value for lower bound in time bounds
    integer(i4), intent(out) ::  t_end !< value for upper bound in time bounds
    integer(i4), intent(out) ::  t_stamp !< value for timestamp
    t_start = nint((previous_time - start_time) / delta, kind=i4)
    t_end = nint((current_time - start_time) / delta, kind=i4)
    ! maybe check if values are actually close the nearest integer (nint)
    select case( timestamp )
      case(start_timestamp)
        t_stamp = t_start
      case(center_timestamp)
        t_stamp = (t_start + t_end) / 2_i4
      case(end_timestamp)
        t_stamp = t_end
      case default
        call error_message("output_dataset%write: timestamp has no valid value.")
    end select
  end subroutine time_values

  !> \brief initialize output_variable
  subroutine out_var_init(self, meta, nc, grid, dims, double_precision, deflate_level)
    implicit none
    class(output_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable definition
    type(NcDataset), intent(in) :: nc !< NcDataset to write
    type(grid_t), pointer, intent(in) :: grid !< grid definition
    type(NcDimension), dimension(:), intent(in) :: dims !< dimensions in the file
    logical, intent(in) :: double_precision !< flag to use double precision
    integer(i4), intent(in) :: deflate_level !< deflate level for compression

    self%name = meta%name
    self%static = meta%static
    self%avg = meta%avg

    if (self%static) then
      self%nc = nc%setVariable(self%name, dtype(double_precision), dims(:2), deflate_level=deflate_level, shuffle=.true.)
    else
      self%nc = nc%setVariable(self%name, dtype(double_precision), dims(:3), deflate_level=deflate_level, shuffle=.true.)
    end if

    if (allocated(meta%long_name)) self%long_name = meta%long_name
    if (allocated(meta%standard_name)) self%standard_name = meta%standard_name
    if (allocated(meta%units)) self%units = meta%units

    if (allocated(self%long_name)) call self%nc%setAttribute("long_name", self%long_name)
    if (allocated(self%standard_name)) call self%nc%setAttribute("standard_name", self%standard_name)
    if (allocated(self%units)) call self%nc%setAttribute("units", self%units)

    if (double_precision) then
      call self%nc%setFillValue(nodata_dp)
      call self%nc%setAttribute("missing_value", nodata_dp)
    else
      call self%nc%setFillValue(nodata_sp)
      call self%nc%setAttribute("missing_value", nodata_sp)
    end if

    self%grid => grid
    allocate(self%data(self%grid%ncells), source=0.0_dp)
  end subroutine out_var_init

  !> \brief Update output_variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  !> \changelog
  !! - Robert Schweppe Jun 2018
  !!   - refactoring and reformatting
  !!
  !> \authors David Schaefer
  !> \date June 2015
  subroutine out_var_update(self, data)
    implicit none
    class(output_variable), intent(inout) :: self
    real(dp), intent(in), dimension(:) :: data !< data for current time step
    self%data = self%data + data
    self%counter = self%counter + 1_i4
  end subroutine out_var_update

  !> \brief Write timestep to file
  !> \details Write the content of the derived types's component 'data' to file, average if necessary
  !> \changelog
  !! - Robert Schweppe Jun 2018
  !!   - refactoring and reformatting
  !!
  !> \authors David Schafer
  !> \date June 2015
  subroutine out_var_write(self, time_index)
    implicit none
    class(output_variable), intent(inout) :: self
    !> index along the time dimension of the netcdf variable
    integer(i4), intent(in), optional :: time_index
    if (self%static .and. self%static_written) return
    if (self%counter == 0_i4) call error_message("out_variable: no data was added before writing: ", self%name)
    if (self%avg) then
      self%data = self%data / real(self%counter, dp)
    end if
    if (self%static) then
      call self%nc%setData(self%grid%unpack(self%data))
      self%static_written = .true.
    else
      if (.not.present(time_index)) call error_message("out_variable: no time index was given for temporal variable: ", self%name)
      call self%nc%setData(self%grid%unpack(self%data), [1_i4, 1_i4, time_index])
    end if
    self%data = 0.0_dp
    self%counter = 0_i4
  end subroutine out_var_write

  !> \brief Initialize output_dataset
  !> \details Create and initialize the output file. If new a new output
  !! variable needs to be written, this is the first of two
  !! procedures to change (second: updateDataset)
  !> \changelog
  !! - Robert Schweppe Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Mueller Jul 2020
  !!   - added output for river temperature
  !!
  !> \return type(output_dataset)
  !> \authors Matthias Zink
  !> \date Apr 2013
  subroutine output_init(self, path, grid, vars, start_time, delta, timestamp, double_precision, deflate_level)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< path to the file
    type(grid_t), pointer, intent(in) :: grid !< grid definition for this output file
    type(var), dimension(:), intent(in) :: vars !< variables of the output file
    type(datetime), intent(in), optional :: start_time !< reference time
    character(*), intent(in), optional :: delta !< time units delta ("minutes", "hours" (default), "days")
    integer(i4), intent(in), optional :: timestamp !< time stamp location in time span (0: begin, 1: center, 2: end (default))
    logical, intent(in), optional :: double_precision !< switch to select double precision variables (.true. by default)
    integer(i4), intent(in), optional :: deflate_level !< deflate level for compression

    type(output_variable), pointer :: variable
    character(:), allocatable :: units, units_dt
    type(NcDimension) :: t_dim, b_dim, x_dim, y_dim, dims(3)
    type(NcVariable) :: t_var
    integer(i4) :: i

    self%path = trim(path)
    self%nc = NcDataset(self%path, "w")
    self%grid => grid
    self%counter = 0_i4

    self%double_precision = .true.
    if (present(double_precision)) self%double_precision = double_precision
    self%deflate_level = 6_i4
    if (present(deflate_level)) self%deflate_level = deflate_level
    self%timestamp = end_timestamp
    if (present(timestamp)) self%timestamp = timestamp

    ! write grid specification to file
    call self%grid%to_netcdf(self%nc, double_precision=self%double_precision)

    self%nvars = size(vars)
    self%static = .true.
    do i = 1_i4, self%nvars
      self%static = self%static .and. vars(i)%static
    end do

    if (.not.self%static) then
      if (.not.present(start_time)) call error_message("output: if dataset is not static, a start_time is needed")
      units_dt = "hours"
      if (present(delta)) units_dt = trim(delta)
      self%previous_time = start_time
      self%start_time = start_time
      self%delta = delta_from_string(units_dt)
      units = units_dt // " since " // start_time%str()
      t_dim = self%nc%setDimension("time", 0)
      b_dim = self%nc%getDimension("bnds") ! added with grid
      t_var = self%nc%setVariable("time", "i32", [t_dim])
      call t_var%setAttribute("long_name", "time")
      call t_var%setAttribute("standard_name", "time")
      call t_var%setAttribute("axis", "T")
      call t_var%setAttribute("units", units)
      call t_var%setAttribute("bounds", "time_bnds")
      t_var = self%nc%setVariable("time_bnds", "i32", [b_dim, t_dim])
    end if

    if (self%grid%coordsys==cartesian) then
      x_dim = self%nc%getDimension("x")
      y_dim = self%nc%getDimension("y")
    else
      x_dim = self%nc%getDimension("lon")
      y_dim = self%nc%getDimension("lat")
    end if
    dims(:) = [x_dim, y_dim, t_dim]

    allocate(self%vars(self%nvars))
    do i = 1_i4, self%nvars
      call self%vars(i)%init(vars(i), self%nc, self%grid, dims, self%double_precision, self%deflate_level)
    end do
  end subroutine output_init

  !> \brief Update a variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  subroutine output_update(self, name, data)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), intent(in), dimension(:) :: data !< data for current time step
    integer(i4) :: i
    do i = 1_i4, self%nvars
      if (self%vars(i)%name == name) then
        call self%vars(i)%update(data)
        return
      end if
    end do
    call error_message("output%update: variable not present: ", name)
  end subroutine output_update

  !> \brief Write all accumulated data.
  !> \details Write all accumulated and potentially averaged data to disk.
  !> \changelog
  !! - Robert Schweppe Jun 2018
  !!   - refactoring and reformatting
  !!
  !> \authors David Schaefer
  !> \date June 2015
  subroutine output_write(self, current_time)
    implicit none
    class(output_dataset), intent(inout) :: self
    type(datetime), intent(in), optional :: current_time !< end time of the current time span
    integer(i4) :: i, t_start, t_end, t_stamp
    type(NcVariable) :: t_var

    self%counter = self%counter + 1_i4

    ! add to time variable
    if (.not.self%static) then
      if (.not.present(current_time)) call error_message("output: no time was given: ", self%path)
      call time_values(self%start_time, self%previous_time, current_time, self%delta, self%timestamp, &
                       t_start, t_end, t_stamp) ! intent(out)
      t_var = self%nc%getVariable("time")
      call t_var%setData(t_stamp, [self%counter])
      t_var = self%nc%getVariable("time_bnds")
      call t_var%setData(t_start, [1, self%counter])
      call t_var%setData(t_end, [2, self%counter])
      self%previous_time = current_time
    end if

    do i = 1_i4, self%nvars
      call self%vars(i)%write(self%counter)
    end do
  end subroutine output_write

  !> \brief Write all accumulated static data.
  subroutine output_write_static(self)
    implicit none
    class(output_dataset), intent(inout) :: self
    integer(i4) :: i
    do i = 1_i4, self%nvars
      if (self%vars(i)%static) call self%vars(i)%write()
    end do
  end subroutine output_write_static

  !> \brief Close the file
  subroutine output_close(self)
    implicit none
    class(output_dataset) :: self
    integer(i4) :: i
    do i = 1_i4, self%nvars
      ! check if variables have buffered data that was not written
      if (self%vars(i)%counter > 0_i4) call warn_message("output%close: unwritten buffered data for variable: ", self%vars(i)%name)
    end do
    call self%nc%close()
    deallocate(self%vars)
  end subroutine output_close

end module mo_gridded_netcdf
