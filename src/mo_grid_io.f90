!> \file mo_grid_io.f90
!> \copydoc mo_grid_io

!> \brief Creates NetCDF input or output files for gridded 2D data.
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
!!   - rewrote for FORCES and use general grid and datetime types
!!
!> \authors Matthias Zink
!> \authors Sebastian Müller
!> \date Apr 2013
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid_io

  use mo_kind, only : i4, i8, dp, sp
  use mo_constants, only : nodata_dp, nodata_sp, nodata_i4, nodata_i8
  use mo_grid, only: grid_t, cartesian, is_t_axis, check_uniform_axis, bottom_up
  use mo_netcdf, only : NcDataset, NcDimension, NcVariable
  use mo_datetime, only : datetime, timedelta, delta_from_string, decode_cf_time_units, one_day, one_hour
  use mo_message, only : error_message, warn_message
  use mo_utils, only: is_close, flip
  implicit none

  public :: var, output_dataset, input_dataset, time_units_delta, time_index, var_index, time_values

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
    character(:), allocatable :: name          !< variable name in the NetCDF file (required)
    character(:), allocatable :: long_name     !< descriptive variable name
    character(:), allocatable :: standard_name !< standard variable name following CF-Conventions
    character(:), allocatable :: units         !< variable units
    character(:), allocatable :: dtype         !< variable data type in file ('f32', 'f64' (default), 'i16', 'i32', 'i64')
    character(:), allocatable :: kind          !< kind of array for IO ('sp', 'dp' (real default), 'i4' (int default), 'i8')
    logical :: static = .false.                !< static variable (without time dimension)
    logical :: avg = .false.                   !< average data (only for writing)
  contains
    procedure, public :: meta => var_meta
  end type var

  !> \class output_variable
  !> \brief netcdf output variable container for a 2D variable
  type, extends(var) :: output_variable
    type(NcVariable) :: nc                     !< nc variable which contains the variable
    type(grid_t), pointer :: grid => null()    !< horizontal grid the data is defined on
    real(sp), allocatable :: data_sp(:)        !< store the data between writes (real)
    real(dp), allocatable :: data_dp(:)        !< store the data between writes (real)
    integer(i4), allocatable :: data_i4(:)     !< store the data between writes (integer)
    integer(i8), allocatable :: data_i8(:)     !< store the data between writes (integer)
    integer(i4) :: counter = 0_i4              !< count the number of updateVariable calls
    logical :: static_written = .false.        !< static variable was written
  contains
    procedure, public :: init => out_var_init
    procedure, private :: out_var_update_sp, out_var_update_dp, out_var_update_i4, out_var_update_i8
    generic, public :: update => out_var_update_sp, out_var_update_dp, out_var_update_i4, out_var_update_i8
    procedure, public :: write => out_var_write
  end type output_variable

  !> \class input_variable
  !> \brief netcdf input variable container for a 2D variable
  type, extends(var) :: input_variable
    type(NcVariable) :: nc                     !< nc variable which contains the variable
    type(grid_t), pointer :: grid => null()    !< horizontal grid the data is defined on
  contains
    procedure, public :: init => in_var_init
    procedure, private :: in_var_read_sp, in_var_read_dp, in_var_read_i4, in_var_read_i8
    generic, public :: read => in_var_read_sp, in_var_read_dp, in_var_read_i4, in_var_read_i8
    procedure, private :: in_var_read_chunk_sp, in_var_read_chunk_dp, in_var_read_chunk_i4, in_var_read_chunk_i8
    generic, public :: read_chunk => in_var_read_chunk_sp, in_var_read_chunk_dp, in_var_read_chunk_i4, in_var_read_chunk_i8
  end type input_variable

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
    integer(i4) :: deflate_level = 6_i4           !< deflate level for compression
  contains
    procedure, public :: init => output_init
    procedure, private :: output_update_sp, output_update_dp, output_update_i4, output_update_i8
    generic, public :: update => output_update_sp, output_update_dp, output_update_i4, output_update_i8
    procedure, public :: write => output_write
    procedure, public :: write_static => output_write_static
    procedure, public :: meta => output_meta
    procedure, public :: close => output_close
  end type output_dataset

  !> \class input_dataset
  !> \brief netcdf input dataset handler for gridded data
  !> \details Input dataset handler for static and temporal data.
  type input_dataset
    type(grid_t), pointer :: grid => null()       !< horizontal grid the data is defined on
    character(:), allocatable :: path             !< path to the NetCDF file
    type(NcDataset) :: nc                         !< NcDataset to write
    type(input_variable), allocatable :: vars(:)  !< store all created (dynamic) variables
    integer(i4) :: nvars                          !< number of variables
    logical :: static                             !< only static variables (without time dimension)
    logical :: flip_y                             !< whether to flip arrays along y-direction
    type(datetime) :: start_time                  !< start time for time units
    type(timedelta) :: delta                      !< time step in time units definition
    integer(i8) :: delta_sec                      !< time step in time units definition in seconds
    integer(i8) :: start_ord_sec                  !< ordinal seconds of start time
    integer(i4) :: timestep                       !< timestep in the file
    integer(i4), allocatable :: t_values(:)       !< time axis values for ends of time spans
    integer(i4), allocatable :: t_bounds(:)       !< time axis bound values
    type(datetime), allocatable :: times(:)       !< times for ends of time spans
  contains
    procedure, public :: init => input_init
    ! read
    procedure, private :: input_read_pack_sp, input_read_matrix_sp
    generic, public :: read => input_read_pack_sp, input_read_matrix_sp
    procedure, private :: input_read_pack_dp, input_read_matrix_dp
    generic, public :: read => input_read_pack_dp, input_read_matrix_dp
    procedure, private :: input_read_pack_i4, input_read_matrix_i4
    generic, public :: read => input_read_pack_i4, input_read_matrix_i4
    procedure, private :: input_read_pack_i8, input_read_matrix_i8
    generic, public :: read => input_read_pack_i8, input_read_matrix_i8
    ! read_chunk
    procedure, private :: input_read_chunk_pack_sp, input_read_chunk_matrix_sp
    generic, public :: read_chunk => input_read_chunk_pack_sp, input_read_chunk_matrix_sp
    procedure, private :: input_read_chunk_pack_dp, input_read_chunk_matrix_dp
    generic, public :: read_chunk => input_read_chunk_pack_dp, input_read_chunk_matrix_dp
    procedure, private :: input_read_chunk_pack_i4, input_read_chunk_matrix_i4
    generic, public :: read_chunk => input_read_chunk_pack_i4, input_read_chunk_matrix_i4
    procedure, private :: input_read_chunk_pack_i8, input_read_chunk_matrix_i8
    generic, public :: read_chunk => input_read_chunk_pack_i8, input_read_chunk_matrix_i8
    ! read_chunk_by_ids
    procedure, private :: input_read_chunk_by_ids_pack_sp, input_read_chunk_by_ids_matrix_sp
    generic, public :: read_chunk_by_ids => input_read_chunk_by_ids_pack_sp, input_read_chunk_by_ids_matrix_sp
    procedure, private :: input_read_chunk_by_ids_pack_dp, input_read_chunk_by_ids_matrix_dp
    generic, public :: read_chunk_by_ids => input_read_chunk_by_ids_pack_dp, input_read_chunk_by_ids_matrix_dp
    procedure, private :: input_read_chunk_by_ids_pack_i4, input_read_chunk_by_ids_matrix_i4
    generic, public :: read_chunk_by_ids => input_read_chunk_by_ids_pack_i4, input_read_chunk_by_ids_matrix_i4
    procedure, private :: input_read_chunk_by_ids_pack_i8, input_read_chunk_by_ids_matrix_i8
    generic, public :: read_chunk_by_ids => input_read_chunk_by_ids_pack_i8, input_read_chunk_by_ids_matrix_i8
    ! others
    procedure, public :: time_index => input_time_index
    procedure, public :: chunk_times => input_chunk_times
    procedure, public :: meta => input_meta
    procedure, public :: close => input_close
  end type input_dataset

contains

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
      if (step > no_time .and. mod(step, 2) == 1) res = "minutes"
    else
      if (step < no_time .and. step >= yearly ) res = "days"
      if (step > no_time .and. mod(step, 24) == 0) res = "days"
    end if
  end function time_units_delta

  !> \brief generate values for the time-dimension depending on given datetimes.
  subroutine time_values(start_time, previous_time, current_time, delta, timestamp, t_start, t_end, t_stamp)
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

  !> \brief determine time-stepping time-dimension depending on given datetimes.
  subroutine time_stepping(t_var, timestamp, start_time, delta, timestep, t_values, t_bounds)
    type(NcVariable), intent(in) :: t_var !< time variable
    integer(i4), intent(in) :: timestamp !< timestamp location selector
    type(datetime), intent(out) :: start_time !< starting time in units
    type(timedelta), intent(out) :: delta !< time delta in units
    integer(i4), intent(out) ::  timestep !< time step indicator
    integer(i4), allocatable, dimension(:), intent(out) :: t_values !< time axis values for end of time spans
    integer(i4), allocatable, dimension(:), intent(out) :: t_bounds !< time axis bound values
    integer(i4), allocatable, dimension(:) :: tmp_arr, t_diffs
    type(timedelta) :: loc_delta ! local time delta in units
    type(datetime) :: loc_date
    integer(i4) :: dt, i
    real(dp) :: dt_dp
    logical :: is_monthly, is_yearly
    character(len=256) :: tmp_str
    type(NcVariable) :: tb_var
    integer(i4), allocatable, dimension(:,:) :: t_bnds

    call t_var%getAttribute("units", tmp_str)
    call decode_cf_time_units(trim(tmp_str), delta, start_time)
    ! check bounds
    if (t_var%hasAttribute("bounds")) then
      call t_var%getAttribute("bounds", tmp_str)
      tb_var = t_var%parent%getVariable(trim(tmp_str))
      call tb_var%getData(t_bnds)
      t_values = t_bnds(2, :) ! upper bound as reference value
    else if (timestamp == end_timestamp) then
      call t_var%getData(t_values)
    else if (timestamp == start_timestamp) then
      call t_var%getData(tmp_arr)
      if (size(tmp_arr) == 1_i4) then
        ! assume same step as with initial value
        allocate(t_values(1), source=2*tmp_arr(1))
      else
        allocate(t_values(size(tmp_arr)))
        t_values(:size(tmp_arr)-1) = tmp_arr(2:)
        ! assume last time-step has same size as second last
        t_values(size(tmp_arr)) = 2 * tmp_arr(size(tmp_arr)) - tmp_arr(size(tmp_arr)-1)
      end if
    else ! center or others
      call error_message("time_stepping: can't convert center of time-span to output time values")
    end if

    ! check t_values for stepping
    if (size(t_values)==1) then
      if (allocated(t_bnds)) then
        loc_delta = (t_bnds(2,1) - t_bnds(1,1)) * delta
      else
        loc_delta = delta
      end if
      if (loc_delta == one_day()) then
        timestep = daily
      else if (loc_delta == one_hour()) then
        timestep = hourly
      else
        call error_message("time_stepping: could not determine time step size")
      end if
    else
      if (allocated(t_bnds)) then
        t_diffs = t_bnds(2,:) - t_bnds(1,:)
      else
        t_diffs = t_values(2:) - t_values(:size(t_values)-1)
      end if
      dt = t_diffs(1)
      if (all(t_diffs==dt)) then
        loc_delta = dt * delta
        if (loc_delta == one_day()) then
          timestep = daily
        else if (loc_delta == one_hour()) then
          timestep = hourly
        else
          dt_dp = loc_delta / one_hour()
          timestep = nint(dt_dp, i4)
          if (.not.is_close(dt_dp, real(timestep, dp))) call error_message("time_stepping: could not determine time step size")
        end if
      else
        is_yearly = .true.
        is_monthly = .true.
        do i = 1_i4, size(t_values)
          loc_date = start_time + t_values(i) * delta
          is_monthly = is_monthly .and. loc_date%is_new_month()
          is_yearly = is_yearly .and. loc_date%is_new_year()
        end do
        if (is_yearly) then
          timestep = yearly
        else if (is_monthly) then
          timestep = monthly
        else
          timestep = varying
        end if
      end if
    end if

    ! generate bounds values
    allocate(t_bounds(size(t_values) + 1))
    t_bounds(2:) = t_values
    if (allocated(t_bnds)) then
      t_bounds(1) = t_bnds(1,1)
    else
      ! guess lower bound
      loc_date = start_time + t_values(1) * delta
      select case(timestep)
        case(varying)
          t_bounds(1) = t_values(1) - 1_i4 ! one delta step before
        case(yearly)
          loc_delta = loc_date%previous_new_year() - start_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case(monthly)
          loc_delta = loc_date%previous_new_month() - start_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case(daily)
          loc_delta = loc_date%previous_new_day() - start_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case default ! hour based delta
          loc_delta = loc_date - timestep * one_hour() - start_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
      end select
    end if

  end subroutine time_stepping

  !> \brief get time index for current time
  integer(i4) function time_index(times, current_time, raise, found)
    type(datetime), dimension(:), intent(in) :: times !< available times
    type(datetime), intent(in) :: current_time !< current read time
    logical, intent(in), optional :: raise !< switch to raise error if time not found
    logical, intent(out), optional :: found !< time was found
    integer(i4) :: i
    logical :: found_ = .false.
    logical :: raise_ = .true.
    if (present(raise)) raise_ = raise
    do i = 1_i4, size(times)
      if (current_time == times(i)) then
        found_ = .true.
        exit
      end if
    end do
    if (.not.found_.and.raise_) call error_message("time_index: current time not available: ", current_time%str())
    if (.not.found_) i = 0_i4
    if (present(found)) found = found_
    time_index = i
  end function time_index

  !> \brief Get variable meta data.
  !> \return \ref var meta data definition
  type(var) function var_meta(this)
    class(var), intent(in) :: this
    if (allocated(this%name)) var_meta%name = this%name
    if (allocated(this%long_name)) var_meta%long_name = this%long_name
    if (allocated(this%standard_name)) var_meta%standard_name = this%standard_name
    if (allocated(this%units)) var_meta%units = this%units
    if (allocated(this%dtype)) var_meta%dtype = this%dtype
    if (allocated(this%kind)) var_meta%kind = this%kind
    var_meta%static = this%static
    var_meta%avg = this%avg
  end function var_meta

  !> \brief Get variable index in vars array.
  !> \return index
  integer(i4) function var_index(vars, name, method)
    class(var), dimension(:), intent(in) :: vars !< variables array
    character(*), intent(in) :: name !< name of the variable
    character(*), intent(in) :: method !< method calling this
    integer(i4) :: i
    var_index = 0_i4
    do i = 1_i4, size(vars)
      if (vars(i)%name == name) then
        var_index = i
        return
      end if
    end do
    call error_message(method, ": variable not present: ", name)
  end function var_index

  !> \brief initialize output_variable
  subroutine out_var_init(self, meta, nc, grid, dims, deflate_level)
    implicit none
    class(output_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable definition
    type(NcDataset), intent(in) :: nc !< NcDataset to write
    type(grid_t), pointer, intent(in) :: grid !< grid definition
    type(NcDimension), dimension(:), intent(in) :: dims !< dimensions in the file
    integer(i4), intent(in) :: deflate_level !< deflate level for compression

    self%name = meta%name
    self%static = meta%static
    self%avg = meta%avg
    self%dtype = "f64" ! default to double
    if (allocated(meta%dtype)) self%dtype = trim(meta%dtype)

    if (self%static) then
      self%nc = nc%setVariable(self%name, self%dtype, dims(:2), deflate_level=deflate_level, shuffle=.true.)
    else
      self%nc = nc%setVariable(self%name, self%dtype, dims(:3), deflate_level=deflate_level, shuffle=.true.)
    end if

    if (allocated(meta%long_name)) self%long_name = meta%long_name
    if (allocated(meta%standard_name)) self%standard_name = meta%standard_name
    if (allocated(meta%units)) self%units = meta%units

    if (allocated(self%long_name)) call self%nc%setAttribute("long_name", self%long_name)
    if (allocated(self%standard_name)) call self%nc%setAttribute("standard_name", self%standard_name)
    if (allocated(self%units)) call self%nc%setAttribute("units", self%units)

    self%grid => grid
    if (self%grid%has_aux_coords()) call self%nc%setAttribute("coordinates", "lat lon")

    ! input data is still either real(dp) or integer(i4)
    select case(self%dtype)
      case("f32")
        call self%nc%setFillValue(nodata_sp)
        call self%nc%setAttribute("missing_value", nodata_sp)
        self%kind = "dp" ! double precision as default for real data
      case("f64")
        call self%nc%setFillValue(nodata_dp)
        call self%nc%setAttribute("missing_value", nodata_dp)
        self%kind = "dp" ! double precision as default for real data
      case("i32")
        call self%nc%setFillValue(nodata_i4)
        call self%nc%setAttribute("missing_value", nodata_i4)
        self%kind = "i4" ! 4 byte integers as default for int data
      case("i64")
        call self%nc%setFillValue(nodata_i8)
        call self%nc%setAttribute("missing_value", nodata_i8)
        self%kind = "i4" ! 4 byte integers as default for int data
      case default
        call error_message("output_variable: unsupported dtype: ", self%name, ": ", self%dtype)
    end select
    if (allocated(meta%kind)) then
      self%kind = meta%kind
      if ((self%dtype(1:1) == "f" .and. meta%kind(2:2) /= "p") .or. (self%dtype(1:1) == "i" .and. meta%kind(1:1) /= "i")) &
        call warn_message("output_variable: variable dtype and array kind will result in conversion: ", &
                          self%name, ", dtype: ", self%dtype, ", kind:", self%kind)
    end if
    select case(self%kind)
      case("sp")
        allocate(self%data_sp(self%grid%ncells), source=0.0_sp)
      case("dp")
        allocate(self%data_dp(self%grid%ncells), source=0.0_dp)
      case("i4")
        allocate(self%data_i4(self%grid%ncells), source=0_i4)
      case("i8")
        allocate(self%data_i8(self%grid%ncells), source=0_i8)
      case default
        call error_message("output_variable: unsupported kind: ", self%name, ": ", self%kind)
    end select
  end subroutine out_var_init

  !> \brief Update output_variable
  subroutine out_var_update_sp(self, data)
    implicit none
    class(output_variable), intent(inout) :: self
    real(sp), intent(in), dimension(:) :: data !< data for current time step
    if (.not.allocated(self%data_sp)) call error_message("output_variable: wrong kind: ", self%name, ", ", self%kind, "=/=sp")
    self%data_sp = self%data_sp + data
    self%counter = self%counter + 1_i4
  end subroutine out_var_update_sp

  !> \brief Update output_variable
  subroutine out_var_update_dp(self, data)
    implicit none
    class(output_variable), intent(inout) :: self
    real(dp), intent(in), dimension(:) :: data !< data for current time step
    if (.not.allocated(self%data_dp)) call error_message("output_variable: wrong kind: ", self%name, ", ", self%kind, "=/=dp")
    self%data_dp = self%data_dp + data
    self%counter = self%counter + 1_i4
  end subroutine out_var_update_dp

  !> \brief Update output_variable
  subroutine out_var_update_i4(self, data)
    implicit none
    class(output_variable), intent(inout) :: self
    integer(i4), intent(in), dimension(:) :: data !< data for current time step
    if (.not.allocated(self%data_i4)) call error_message("output_variable: wrong kind: ", self%name, ", ", self%kind, "=/=i4")
    self%data_i4 = self%data_i4 + data
    self%counter = self%counter + 1_i4
  end subroutine out_var_update_i4

  !> \brief Update output_variable
  subroutine out_var_update_i8(self, data)
    implicit none
    class(output_variable), intent(inout) :: self
    integer(i8), intent(in), dimension(:) :: data !< data for current time step
    if (.not.allocated(self%data_i8)) call error_message("output_variable: wrong kind: ", self%name, ", ", self%kind, "=/=i8")
    self%data_i8 = self%data_i8 + data
    self%counter = self%counter + 1_i4
  end subroutine out_var_update_i8

  !> \brief Write timestep to file
  !> \details Write the content of the derived types's component 'data' to file, average if necessary
  !> \changelog
  !! - Robert Schweppe Jun 2018
  !!   - refactoring and reformatting
  !!
  !> \authors David Schafer
  !> \date June 2015
  subroutine out_var_write(self, t_index)
    implicit none
    class(output_variable), intent(inout) :: self
    !> index along the time dimension of the netcdf variable
    integer(i4), intent(in), optional :: t_index
    if (self%static .and. self%static_written) return
    if (self%counter == 0_i4) call error_message("output_variable: no data was added before writing: ", self%name)
    if (self%avg) then
      select case(self%kind)
        case("sp")
          self%data_sp = self%data_sp / real(self%counter, sp)
        case("dp")
          self%data_dp = self%data_dp / real(self%counter, dp)
        case("i4")
          self%data_i4 = self%data_i4 / self%counter ! this is rounding
        case("i8")
          self%data_i8 = self%data_i8 / int(self%counter, i8)  ! this is rounding
      end select
    end if
    if (self%static) then
      select case(self%kind)
        case("sp")
          call self%nc%setData(unpack(self%data_sp, self%grid%mask, nodata_sp))
        case("dp")
          call self%nc%setData(unpack(self%data_dp, self%grid%mask, nodata_dp))
        case("i4")
          call self%nc%setData(unpack(self%data_i4, self%grid%mask, nodata_i4))
        case("i8")
          call self%nc%setData(unpack(self%data_i8, self%grid%mask, nodata_i8))
      end select
      self%static_written = .true.
    else
      if (.not.present(t_index)) call error_message("output_variable: no time index was given for temporal variable: ", self%name)
      select case(self%kind)
        case("sp")
          call self%nc%setData(unpack(self%data_sp, self%grid%mask, nodata_sp), [1_i4, 1_i4, t_index])
        case("dp")
          call self%nc%setData(unpack(self%data_dp, self%grid%mask, nodata_dp), [1_i4, 1_i4, t_index])
        case("i4")
          call self%nc%setData(unpack(self%data_i4, self%grid%mask, nodata_i4), [1_i4, 1_i4, t_index])
        case("i8")
          call self%nc%setData(unpack(self%data_i8, self%grid%mask, nodata_i8), [1_i4, 1_i4, t_index])
      end select
    end if
    ! reset
    select case(self%kind)
      case("sp")
        self%data_sp = 0.0_sp
      case("dp")
        self%data_dp = 0.0_dp
      case("i4")
        self%data_i4 = 0_i4
      case("i8")
        self%data_i8 = 0_i8
    end select
    self%counter = 0_i4
  end subroutine out_var_write

  !> \brief initialize input_variable
  subroutine in_var_init(self, meta, nc, grid)
    implicit none
    class(input_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable definition
    type(NcDataset), intent(in) :: nc !< NcDataset to write
    type(grid_t), pointer, intent(in) :: grid !< grid definition
    type(NcDimension), dimension(:), allocatable :: dims
    type(NcVariable) :: t_var
    integer(i4) :: nx, ny, rnk
    character(len=256) :: tmp_str
    self%name = meta%name
    self%nc = nc%getVariable(self%name)
    self%grid => grid
    rnk = self%nc%getRank()
    if (rnk < 2_i4) call error_message("input_variable: given variable has too few dimensions: ", trim(nc%fname), ":", self%name)
    if (rnk > 3_i4) call error_message("input_variable: given variable has too many dimensions: ", trim(nc%fname), ":", self%name)
    dims = self%nc%getDimensions()
    nx = dims(1)%getLength()
    ny = dims(2)%getLength()
    if (nx /= grid%nx .or. ny /= grid%ny) call error_message("input_variable: variable not matching grid: ", self%name)

    self%static = meta%static
    if (rnk == 3_i4) then
      t_var = nc%getVariable(trim(dims(3)%getName()))
      if (.not.is_t_axis(t_var)) call error_message("input_variable: 3rd axis is not time: ", self%name)
      if (meta%static) call error_message("input_variable: variable not static as expected: ", self%name)
    else
      if (.not.meta%static) call error_message("input_variable: variable not temporal as expected: ", self%name)
    end if

    self%dtype = trim(self%nc%getDtype())
    select case(self%dtype(1:1))
      case("f")
        self%kind = "dp"
      case("i")
        self%kind = "i4"
      case default
        call error_message("input_variable: unsupported dtype: ", self%name, ": ", self%dtype)
    end select
    if (allocated(meta%dtype)) then
      if (meta%dtype/=self%dtype) &
        call warn_message("input_variable: variable dtype not as expected: ", &
                          self%name, ", ", meta%dtype, "=/=", self%dtype)
    end if

    if (allocated(meta%kind)) then
      self%kind = meta%kind
      if ((self%dtype(1:1) == "f" .and. meta%kind(2:2) /= "p") .or. (self%dtype(1:1) == "i" .and. meta%kind(1:1) /= "i")) &
        call warn_message("input_variable: variable dtype and array kind will result in conversion: ", &
                          self%name, ", dtype: ", self%dtype, ", kind:", self%kind)
    end if

    if (allocated(meta%standard_name)) self%standard_name = meta%standard_name
    if (self%nc%hasAttribute("standard_name")) then
      call self%nc%getAttribute("standard_name", tmp_str)
      self%standard_name = trim(tmp_str)
    end if
    if (allocated(meta%standard_name)) then
      if (meta%standard_name/=self%standard_name) &
        call warn_message("input_variable: variable standard name not as expected: ", &
                          self%name, ", ", meta%standard_name, "=/=", self%standard_name)
    end if

    if (allocated(meta%units)) self%units = meta%units
    if (self%nc%hasAttribute("units")) then
      call self%nc%getAttribute("units", tmp_str)
      self%units = trim(tmp_str)
    end if
    if (allocated(meta%units)) then
      if (meta%units/=self%units) &
        call warn_message("input_variable: variable units not as expected: ", &
                          self%name, ", ", meta%units, "=/=", self%units)
    end if

    ! don't check long-name
    if (allocated(meta%long_name)) self%long_name = meta%long_name
    if (self%nc%hasAttribute("long_name")) then
      call self%nc%getAttribute("long_name", tmp_str)
      self%long_name = trim(tmp_str)
    end if
  end subroutine in_var_init

  !> \brief read from input variable
  subroutine in_var_read_sp(self, flip_y, t_index, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in), optional :: t_index !< current time step
    real(sp), dimension(:, :), allocatable, intent(out) :: data !< read data
    if (self%static) then
      call self%nc%getData(data)
    else
      if (.not.present(t_index)) call error_message("input%read: temporal variable need a time: ", self%name)
      if (t_index==0_i4) call error_message("input%read: temporal variable need a time: ", self%name)
      call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, 1_i4])
    end if
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_sp

  !> \brief read from input variable
  subroutine in_var_read_dp(self, flip_y, t_index, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in), optional :: t_index !< current time step
    real(dp), dimension(:, :), allocatable, intent(out) :: data !< read data
    if (self%static) then
      call self%nc%getData(data)
    else
      if (.not.present(t_index)) call error_message("input%read: temporal variable need a time: ", self%name)
      if (t_index==0_i4) call error_message("input%read: temporal variable need a time: ", self%name)
      call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, 1_i4])
    end if
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_dp

  !> \brief read from input variable
  subroutine in_var_read_i4(self, flip_y, t_index, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in), optional :: t_index !< current time step
    integer(i4), dimension(:, :), allocatable, intent(out) :: data !< read data
    if (self%static) then
      call self%nc%getData(data)
    else
      if (.not.present(t_index)) call error_message("input%read: temporal variable need a time: ", self%name)
      if (t_index==0_i4) call error_message("input%read: temporal variable need a time: ", self%name)
      call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, 1_i4])
    end if
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_i4

  !> \brief read from input variable
  subroutine in_var_read_i8(self, flip_y, t_index, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in), optional :: t_index !< current time step
    integer(i8), dimension(:, :), allocatable, intent(out) :: data !< read data
    if (self%static) then
      call self%nc%getData(data)
    else
      if (.not.present(t_index)) call error_message("input%read: temporal variable need a time: ", self%name)
      if (t_index==0_i4) call error_message("input%read: temporal variable need a time: ", self%name)
      call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, 1_i4])
    end if
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_i8

  !> \brief read from input variable
  subroutine in_var_read_chunk_sp(self, flip_y, t_index, t_size, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in) :: t_index !< current time step
    integer(i4), intent(in) :: t_size !< chunk size
    real(sp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    if (self%static) call error_message("input%read_chunk: need temporal variable for chunk reading: ", self%name)
    call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, t_size])
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_chunk_sp

  !> \brief read from input variable
  subroutine in_var_read_chunk_dp(self, flip_y, t_index, t_size, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in) :: t_index !< current time step
    integer(i4), intent(in) :: t_size !< chunk size
    real(dp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    if (self%static) call error_message("input%read_chunk: need temporal variable for chunk reading: ", self%name)
    call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, t_size])
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_chunk_dp

  !> \brief read from input variable
  subroutine in_var_read_chunk_i4(self, flip_y, t_index, t_size, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in) :: t_index !< current time step
    integer(i4), intent(in) :: t_size !< chunk size
    integer(i4), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    if (self%static) call error_message("input%read_chunk: need temporal variable for chunk reading: ", self%name)
    call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, t_size])
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_chunk_i4

  !> \brief read from input variable
  subroutine in_var_read_chunk_i8(self, flip_y, t_index, t_size, data)
    implicit none
    class(input_variable), intent(inout) :: self
    logical, intent(in) :: flip_y !< flip data along y-axis
    integer(i4), intent(in) :: t_index !< current time step
    integer(i4), intent(in) :: t_size !< chunk size
    integer(i8), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    if (self%static) call error_message("input%read_chunk: need temporal variable for chunk reading: ", self%name)
    call self%nc%getData(data, start=[1_i4, 1_i4, t_index], cnt=[self%grid%nx, self%grid%ny, t_size])
    if (flip_y) call flip(data, idim=2)
  end subroutine in_var_read_chunk_i8

  !> \brief Initialize output_dataset
  !> \details Create and initialize the output file handler.
  !> \authors Matthias Zink
  !> \authors Robert Schweppe
  !> \authors Sebastian Müller
  !> \date Apr 2013
  subroutine output_init(self, path, grid, vars, start_time, delta, timestamp, deflate_level, grid_double_precision)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< path to the file
    type(grid_t), pointer, intent(in) :: grid !< grid definition for this output file
    type(var), dimension(:), intent(in) :: vars !< variables of the output file
    type(datetime), intent(in), optional :: start_time !< reference time
    character(*), intent(in), optional :: delta !< time units delta ("minutes", "hours" (default), "days")
    integer(i4), intent(in), optional :: timestamp !< time stamp location in time span (0: begin, 1: center, 2: end (default))
    integer(i4), intent(in), optional :: deflate_level !< deflate level for compression
    logical, optional, intent(in) :: grid_double_precision !< whether to use double precision to store grid (default .true.)

    character(:), allocatable :: units, units_dt
    type(NcDimension) :: t_dim, b_dim, x_dim, y_dim, dims(3)
    type(NcVariable) :: t_var
    integer(i4) :: i

    self%path = trim(path)
    self%nc = NcDataset(self%path, "w")
    self%grid => grid
    self%counter = 0_i4

    self%deflate_level = 6_i4
    if (present(deflate_level)) self%deflate_level = deflate_level
    self%timestamp = end_timestamp
    if (present(timestamp)) self%timestamp = timestamp

    ! write grid specification to file
    call self%grid%to_netcdf(self%nc, double_precision=grid_double_precision)

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
      call self%vars(i)%init(vars(i), self%nc, self%grid, dims, self%deflate_level)
    end do
  end subroutine output_init

  !> \brief Update a variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  subroutine output_update_sp(self, name, data)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), intent(in), dimension(:) :: data !< data for current time step
    call self%vars(var_index(self%vars, name, "output%update"))%update(data)
  end subroutine output_update_sp

  !> \brief Update a variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  subroutine output_update_dp(self, name, data)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), intent(in), dimension(:) :: data !< data for current time step
    call self%vars(var_index(self%vars, name, "output%update"))%update(data)
  end subroutine output_update_dp

  !> \brief Update a variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  subroutine output_update_i4(self, name, data)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), intent(in), dimension(:) :: data !< data for current time step
    call self%vars(var_index(self%vars, name, "output%update"))%update(data)
  end subroutine output_update_i4

  !> \brief Update a variable
  !> \details Add the array given as actual argument to the derived type's component 'data'
  subroutine output_update_i8(self, name, data)
    implicit none
    class(output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), intent(in), dimension(:) :: data !< data for current time step
    call self%vars(var_index(self%vars, name, "output%update"))%update(data)
  end subroutine output_update_i8

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
    ! write all variables
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

  !> \brief Get variable meta data.
  !> \return \ref var meta data definition
  type(var) function output_meta(self, name)
    implicit none
    class(output_dataset) :: self
    character(*), intent(in) :: name !< name of the variable
    output_meta = self%vars(var_index(self%vars, name, "output%meta"))%meta()
  end function output_meta

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

  !> \brief Initialize input_dataset
  !> \details Create and initialize the input file handler.
  subroutine input_init(self, path, vars, grid, timestamp, grid_init_var)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< path to the file
    type(grid_t), intent(in), pointer :: grid !< grid definition to check against
    type(var), dimension(:), intent(in) :: vars !< variables of the output file
    integer(i4), intent(in), optional :: timestamp !< time stamp location in time span (0: begin, 1: center, 2: end (default))
    !> nc variable name to determine the grid from (by default, grid is assumed to be already initialized)
    character(*), intent(in), optional :: grid_init_var
    type(NcDimension), allocatable :: dims(:)
    type(NcVariable) :: t_var
    integer(i4) :: i
    logical :: y_inc

    self%path = trim(path)
    self%nc = NcDataset(self%path, "r")
    self%grid => grid
    if (present(grid_init_var)) call self%grid%from_netcdf(self%nc, grid_init_var)
    self%nvars = size(vars)
    if (self%nvars == 0_i4) call error_message("input_dataset: no variables selected")

    allocate(self%vars(self%nvars))
    self%static = .true.
    self%timestep = no_time
    do i = 1_i4, self%nvars
      call self%vars(i)%init(vars(i), self%nc, self%grid)
      if (.not.self%vars(i)%static .and. self%static) then
        self%static = .false.
        dims = self%vars(i)%nc%getDimensions()
        t_var = self%nc%getVariable(trim(dims(3)%getName()))
        call time_stepping(t_var, timestamp, self%start_time, self%delta, self%timestep, self%t_values, self%t_bounds)
        self%times = [(self%start_time + self%t_values(i) * self%delta, i=1_i4,size(self%t_values))]
        self%delta_sec = self%delta%total_seconds()
        self%start_ord_sec = int(self%start_time%date_to_ordinal(), i8) * 86400_i8 + int(self%start_time%day_second(), i8)
      end if
    end do
    if (allocated(dims)) deallocate(dims)
    dims = self%vars(1)%nc%getDimensions()
    if (self%nc%hasVariable(trim(dims(2)%getName()))) then
      call check_uniform_axis(self%nc%getVariable(trim(dims(2)%getName())), increasing=y_inc)
      self%flip_y = y_inc.neqv.(self%grid%y_direction==bottom_up)
    else
      self%flip_y = .false.
    end if

  end subroutine input_init

  ! sp -----

  !> \brief Read an input variable for a single time step
  subroutine input_read_matrix_sp(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(self%grid%nx, self%grid%ny), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    real(sp), dimension(:,:), allocatable :: read_data
    integer(i4) :: t_index
    t_index = 0_i4 ! indicate missing current_time
    if (present(current_time) .and. allocated(self%times)) t_index = self%time_index(current_time)
    call self%vars(var_index(self%vars, name, "input%read"))%read(flip_y=self%flip_y, t_index=t_index, data=read_data)
    data = read_data
  end subroutine input_read_matrix_sp

  !> \brief Read an input variable for a single time step
  subroutine input_read_pack_sp(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(self%grid%ncells), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    real(sp), dimension(self%grid%nx, self%grid%ny) :: data_matrix !< read data
    call self%input_read_matrix_sp(name, data_matrix, current_time)
    data = pack(data_matrix, self%grid%mask)
  end subroutine input_read_pack_sp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_matrix_sp(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4) :: t_index, t_size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%input_read_chunk_by_ids_matrix_sp(name, data, t_index, t_size)
  end subroutine input_read_chunk_matrix_sp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_pack_sp(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    real(sp), dimension(:,:,:), allocatable :: data_matrix !< read data
    integer(i4) :: i, n
    call self%input_read_chunk_matrix_sp(name, data_matrix, timeframe_start, timeframe_end, times)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_pack_sp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_matrix_sp(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    allocate(data(self%grid%nx, self%grid%ny, t_size))
    call self%vars(var_index(self%vars, name, "input%read"))%read_chunk(flip_y=self%flip_y, t_index=t_index, t_size=t_size, data=data)
  end subroutine input_read_chunk_by_ids_matrix_sp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_pack_sp(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(sp), dimension(:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    real(sp), dimension(:,:,:), allocatable :: data_matrix
    integer(i4) :: i, n
    call self%input_read_chunk_by_ids_matrix_sp(name, data_matrix, t_index, t_size)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_by_ids_pack_sp

  ! dp -----

  !> \brief Read an input variable for a single time step
  subroutine input_read_matrix_dp(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(self%grid%nx, self%grid%ny), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    real(dp), dimension(:,:), allocatable :: read_data
    integer(i4) :: t_index
    t_index = 0_i4 ! indicate missing current_time
    if (present(current_time) .and. allocated(self%times)) t_index = self%time_index(current_time)
    call self%vars(var_index(self%vars, name, "input%read"))%read(flip_y=self%flip_y, t_index=t_index, data=read_data)
    data = read_data
  end subroutine input_read_matrix_dp

  !> \brief Read an input variable for a single time step
  subroutine input_read_pack_dp(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(self%grid%ncells), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    real(dp), dimension(self%grid%nx, self%grid%ny) :: data_matrix !< read data
    call self%input_read_matrix_dp(name, data_matrix, current_time)
    data = pack(data_matrix, self%grid%mask)
  end subroutine input_read_pack_dp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_matrix_dp(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4) :: t_index, t_size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%input_read_chunk_by_ids_matrix_dp(name, data, t_index, t_size)
  end subroutine input_read_chunk_matrix_dp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_pack_dp(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    real(dp), dimension(:,:,:), allocatable :: data_matrix !< read data
    integer(i4) :: i, n
    call self%input_read_chunk_matrix_dp(name, data_matrix, timeframe_start, timeframe_end, times)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_pack_dp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_matrix_dp(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    allocate(data(self%grid%nx, self%grid%ny, t_size))
    call self%vars(var_index(self%vars, name, "input%read"))%read_chunk(flip_y=self%flip_y, t_index=t_index, t_size=t_size, data=data)
  end subroutine input_read_chunk_by_ids_matrix_dp

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_pack_dp(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    real(dp), dimension(:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    real(dp), dimension(:,:,:), allocatable :: data_matrix
    integer(i4) :: i, n
    call self%input_read_chunk_by_ids_matrix_dp(name, data_matrix, t_index, t_size)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_by_ids_pack_dp

  ! i4 -----

  !> \brief Read an input variable for a single time step
  subroutine input_read_matrix_i4(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(self%grid%nx, self%grid%ny), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    integer(i4), dimension(:,:), allocatable :: read_data
    integer(i4) :: t_index
    t_index = 0_i4 ! indicate missing current_time
    if (present(current_time) .and. allocated(self%times)) t_index = self%time_index(current_time)
    call self%vars(var_index(self%vars, name, "input%read"))%read(flip_y=self%flip_y, t_index=t_index, data=read_data)
    data = read_data
  end subroutine input_read_matrix_i4

  !> \brief Read an input variable for a single time step
  subroutine input_read_pack_i4(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(self%grid%ncells), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    integer(i4), dimension(self%grid%nx, self%grid%ny) :: data_matrix !< read data
    call self%input_read_matrix_i4(name, data_matrix, current_time)
    data = pack(data_matrix, self%grid%mask)
  end subroutine input_read_pack_i4

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_matrix_i4(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4) :: t_index, t_size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%input_read_chunk_by_ids_matrix_i4(name, data, t_index, t_size)
  end subroutine input_read_chunk_matrix_i4

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_pack_i4(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4), dimension(:,:,:), allocatable :: data_matrix !< read data
    integer(i4) :: i, n
    call self%input_read_chunk_matrix_i4(name, data_matrix, timeframe_start, timeframe_end, times)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_pack_i4

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_matrix_i4(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    allocate(data(self%grid%nx, self%grid%ny, t_size))
    call self%vars(var_index(self%vars, name, "input%read"))%read_chunk(flip_y=self%flip_y, t_index=t_index, t_size=t_size, data=data)
  end subroutine input_read_chunk_by_ids_matrix_i4

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_pack_i4(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i4), dimension(:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    integer(i4), dimension(:,:,:), allocatable :: data_matrix
    integer(i4) :: i, n
    call self%input_read_chunk_by_ids_matrix_i4(name, data_matrix, t_index, t_size)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_by_ids_pack_i4

  ! i8 -----

  !> \brief Read an input variable for a single time step
  subroutine input_read_matrix_i8(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(self%grid%nx, self%grid%ny), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    integer(i8), dimension(:,:), allocatable :: read_data
    integer(i4) :: t_index
    t_index = 0_i4 ! indicate missing current_time
    if (present(current_time) .and. allocated(self%times)) t_index = self%time_index(current_time)
    call self%vars(var_index(self%vars, name, "input%read"))%read(flip_y=self%flip_y, t_index=t_index, data=read_data)
    data = read_data
  end subroutine input_read_matrix_i8

  !> \brief Read an input variable for a single time step
  subroutine input_read_pack_i8(self, name, data, current_time)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(self%grid%ncells), intent(out) :: data !< read data
    type(datetime), intent(in), optional :: current_time !< current time step
    integer(i8), dimension(self%grid%nx, self%grid%ny) :: data_matrix !< read data
    call self%input_read_matrix_i8(name, data_matrix, current_time)
    data = pack(data_matrix, self%grid%mask)
  end subroutine input_read_pack_i8

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_matrix_i8(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4) :: t_index, t_size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%input_read_chunk_by_ids_matrix_i8(name, data, t_index, t_size)
  end subroutine input_read_chunk_matrix_i8

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_pack_i8(self, name, data, timeframe_start, timeframe_end, times)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(:,:), allocatable, intent(out) :: data !< read data
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i8), dimension(:,:,:), allocatable :: data_matrix !< read data
    integer(i4) :: i, n
    call self%input_read_chunk_matrix_i8(name, data_matrix, timeframe_start, timeframe_end, times)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_pack_i8

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_matrix_i8(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(:,:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    if (self%static) call error_message("input%read_chunk: file has no time: ", self%path)
    allocate(data(self%grid%nx, self%grid%ny, t_size))
    call self%vars(var_index(self%vars, name, "input%read"))%read_chunk(flip_y=self%flip_y, t_index=t_index, t_size=t_size, data=data)
  end subroutine input_read_chunk_by_ids_matrix_i8

  !> \brief Read an input variable for a given time frame
  subroutine input_read_chunk_by_ids_pack_i8(self, name, data, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< name of the variable
    integer(i8), dimension(:,:), allocatable, intent(out) :: data !< read data
    integer(i4), intent(in) :: t_index !< start index of time frame
    integer(i4), intent(in) :: t_size !< chunk size
    integer(i8), dimension(:,:,:), allocatable :: data_matrix
    integer(i4) :: i, n
    call self%input_read_chunk_by_ids_matrix_i8(name, data_matrix, t_index, t_size)
    n = size(data_matrix, dim=3)
    allocate(data(self%grid%ncells, n))
    do i = 1_i4, n
      data(:,i) = pack(data_matrix(:,:,i), self%grid%mask)
    end do
  end subroutine input_read_chunk_by_ids_pack_i8

  ! others ---

  !> \brief Get times array for selected chunk time frame
  subroutine input_chunk_times(self, timeframe_start, timeframe_end, times, t_index, t_size)
    implicit none
    class(input_dataset), intent(inout) :: self
    type(datetime), intent(in) :: timeframe_start !< start of time frame (excluding)
    type(datetime), intent(in) :: timeframe_end !< end of time frame (including)
    type(datetime), dimension(:), allocatable, intent(out), optional :: times !< timestamps for the data stack
    integer(i4), intent(out), optional :: t_index !< starting index of time frame
    integer(i4), intent(out), optional :: t_size !< chunk size of time frame
    integer(i4) :: t_start, t_end, t_cnt, t_id
    if (self%static) call error_message("input%chunk_times: file has no time: ", self%path)
    if (timeframe_start < self%times(1)) then
      t_start = 0_i4
    else
      t_start = self%time_index(timeframe_start)
    end if
    if (timeframe_end > self%times(size(self%times))) then
      t_end = size(self%times)
    else
      t_end = self%time_index(timeframe_end)
    end if
    t_cnt = t_end - t_start
    t_id = t_start + 1_i4 ! times array has endpoints as references for time-spans, so we start with the next one
    if (present(t_index)) t_index = t_id
    if (present(t_size)) t_size = t_cnt
    if (present(times)) allocate(times(t_cnt), source=self%times(t_id:t_end))
  end subroutine input_chunk_times

  !> \brief Get time index for interval containing selected time
  integer(i4) function input_time_index(self, current_time)
    implicit none
    class(input_dataset), intent(in) :: self
    type(datetime), intent(in) :: current_time !< current read time
    integer(i8) :: current_delta_sec
    integer(i4) :: t_val
    if (.not.allocated(self%times)) call error_message("input%time_index: file is static and has no time dimension")
    ! seconds since start date
    current_delta_sec = int(current_time%date_to_ordinal(), i8) * 86400_i8 + int(current_time%day_second(), i8) - self%start_ord_sec
    ! calculate time value for current time for time units in file
    t_val = int(current_delta_sec / self%delta_sec, i4) ! division with remainder
    if (mod(current_delta_sec, self%delta_sec) > 0_i8) t_val = t_val + 1_i4 ! next step if remaining sub-step time
    ! locate the value in the time values of the file
    if (t_val < self%t_bounds(1)) call error_message("input%time_index: read time not covered by file.")
    do input_time_index = 1_i4, size(self%t_values)
      if (self%t_bounds(input_time_index) < t_val .and. t_val <= self%t_bounds(input_time_index+1_i4)) return
    end do
    call error_message("input%time_index: read time not covered by file.")
  end function input_time_index

  !> \brief Get variable meta data.
  !> \return \ref var meta data definition
  type(var) function input_meta(self, name)
    implicit none
    class(input_dataset) :: self
    character(*), intent(in) :: name !< name of the variable
    input_meta = self%vars(var_index(self%vars, name, "input%meta"))%meta()
  end function input_meta

  !> \brief Close the file
  subroutine input_close(self)
    implicit none
    class(input_dataset) :: self
    call self%nc%close()
    deallocate(self%vars)
    if (allocated(self%times)) deallocate(self%times)
    if (allocated(self%t_values)) deallocate(self%t_values)
  end subroutine input_close

end module mo_grid_io
