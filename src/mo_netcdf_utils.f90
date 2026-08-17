!> \file    mo_netcdf_utils.f90
!> \copydoc mo_netcdf_utils

!> \brief   Shared NetCDF helper routines.
!> \details This module contains NetCDF-specific helpers that are shared by
!!          structured grid and unstructured point IO modules.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_netcdf_utils

  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

  use mo_constants, only: nodata_dp, nodata_sp, nodata_i1, nodata_i2, nodata_i4, nodata_i8
  use mo_datetime, only: datetime, timedelta, decode_cf_time_units, one_hour, &
                         daily, monthly, yearly, varying, start_timestamp, center_timestamp, end_timestamp, &
                         infer_time_timestep_from_bounds, infer_time_timestep_from_values
  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_netcdf, only: NcVariable
  use mo_string_utils, only: num2str
  use mo_utils, only: optval

  implicit none

  private

  public :: var
  public :: add_var
  public :: var_index
  public :: time_stepping
  public :: read_units
  public :: netcdf_dtype_defaults

  interface read_integral_time_data
    module procedure read_integral_time_data_1d
    module procedure read_integral_time_data_2d
  end interface read_integral_time_data

  !> \class var
  !> \brief Variable metadata definition for NetCDF IO variables.
  type var
    character(:), allocatable :: name          !< variable name in the NetCDF file (required)
    character(:), allocatable :: long_name     !< descriptive variable name
    character(:), allocatable :: standard_name !< standard variable name following CF-Conventions
    character(:), allocatable :: units         !< variable units
    character(:), allocatable :: dtype         !< variable data type in file ('f32', 'f64' (default), 'i8', 'i16', 'i32', 'i64')
    character(:), allocatable :: kind          !< kind of array for IO ('sp', 'dp' (real def.), 'i1', 'i2', 'i4' (int def.), 'i8')
    logical :: static = .false.                !< static variable (without time dimension)
    logical :: allow_static = .false.          !< accept a static variable on input when static=.false.
    logical :: avg = .false.                   !< average data (only for writing)
    logical :: layered = .false.               !< variable is layered
  contains
    procedure, public :: meta => var_meta
  end type var

contains

  !> \brief Get variable metadata.
  !> \return \ref var metadata definition
  type(var) function var_meta(this)
    class(var), intent(in) :: this
    if (allocated(this%name)) var_meta%name = this%name
    if (allocated(this%long_name)) var_meta%long_name = this%long_name
    if (allocated(this%standard_name)) var_meta%standard_name = this%standard_name
    if (allocated(this%units)) var_meta%units = this%units
    if (allocated(this%dtype)) var_meta%dtype = this%dtype
    if (allocated(this%kind)) var_meta%kind = this%kind
    var_meta%static = this%static
    var_meta%allow_static = this%allow_static
    var_meta%avg = this%avg
    var_meta%layered = this%layered
  end function var_meta

  !> \brief Add variable metadata to a variable array.
  subroutine add_var(vars, new_var)
    type(var), allocatable, intent(inout) :: vars(:) !< variables array
    type(var), intent(in) :: new_var !< variable to add
    type(var), allocatable :: tmp(:)
    integer(i4) :: i, n
    if (allocated(vars)) then
      n = size(vars, kind=i4)
    else
      n = 0_i4
    end if
    allocate(tmp(n + 1_i4))
    do i = 1_i4, n
      tmp(i) = vars(i)
    end do
    tmp(n + 1_i4) = new_var
    call move_alloc(tmp, vars)
  end subroutine add_var

  !> \brief Get variable index in vars array.
  !> \return index
  integer(i4) function var_index(vars, name, method)
    class(var), dimension(:), intent(in) :: vars !< variables array
    character(*), intent(in) :: name !< name of the variable
    character(*), intent(in) :: method !< method calling this
    integer(i4) :: i
    var_index = 0_i4
    do i = 1_i4, size(vars)
      if (allocated(vars(i)%name)) then
        if (vars(i)%name == name) then
          var_index = i
          exit
        end if
      end if
    end do
    if (var_index == 0_i4) call error_message(method // ": variable not present: ", name)
  end function var_index

  !> \brief Read and trim the units attribute from a NetCDF variable.
  subroutine read_units(nc_var, units)
    type(NcVariable), intent(in) :: nc_var !< NetCDF variable
    character(:), allocatable, intent(out) :: units !< units attribute string
    character(len=256) :: tmp

    call nc_var%getAttribute("units", tmp)
    units = trim(tmp)
  end subroutine read_units

  !> \brief Map NetCDF dtype metadata to FORCES default Fortran kinds and missing values.
  subroutine netcdf_dtype_defaults(name, dtype, kind, nc, context)
    character(*), intent(in) :: name !< variable name for diagnostics
    character(*), intent(in) :: dtype !< NetCDF dtype
    character(:), allocatable, intent(out) :: kind !< default Fortran kind
    type(NcVariable), optional, intent(inout) :: nc !< NetCDF variable that receives fill metadata
    character(*), optional, intent(in) :: context !< diagnostic context (default: netcdf_dtype_defaults)
    character(:), allocatable :: context_

    context_ = "netcdf_dtype_defaults"
    if (present(context)) context_ = trim(context)
    select case(trim(dtype))
      case("f32")
        kind = "dp"
        if (present(nc)) then
          call nc%setFillValue(nodata_sp)
          call nc%setAttribute("missing_value", nodata_sp)
        end if
      case("f64")
        kind = "dp"
        if (present(nc)) then
          call nc%setFillValue(nodata_dp)
          call nc%setAttribute("missing_value", nodata_dp)
        end if
      case("i8")
        kind = "i4"
        if (present(nc)) then
          call nc%setFillValue(nodata_i1)
          call nc%setAttribute("missing_value", nodata_i1)
        end if
      case("i16")
        kind = "i4"
        if (present(nc)) then
          call nc%setFillValue(nodata_i2)
          call nc%setAttribute("missing_value", nodata_i2)
        end if
      case("i32")
        kind = "i4"
        if (present(nc)) then
          call nc%setFillValue(nodata_i4)
          call nc%setAttribute("missing_value", nodata_i4)
        end if
      case("i64")
        kind = "i4"
        if (present(nc)) then
          call nc%setFillValue(nodata_i8)
          call nc%setAttribute("missing_value", nodata_i8)
        end if
      case default
        call error_message(context_ // ": unsupported dtype: ", name, ": ", dtype)
    end select
  end subroutine netcdf_dtype_defaults

  !> \brief Read and validate an integer-representable time coordinate and its optional bounds.
  subroutine check_and_read_time(t_var, values, bounds)
    type(NcVariable), intent(in) :: t_var !< time coordinate variable
    integer(i4), allocatable, intent(out) :: values(:) !< checked time coordinate values
    integer(i4), allocatable, intent(out) :: bounds(:, :) !< checked time bounds, if present

    character(len=256) :: bounds_name
    type(NcVariable) :: bounds_var
    integer(i4) :: i

    call read_integral_time_data(t_var, values)
    if (size(values) < 1_i4) call error_message("time_stepping: empty time axis")
    if (.not.t_var%hasAttribute("bounds")) return

    call t_var%getAttribute("bounds", bounds_name)
    bounds_var = t_var%parent%getVariable(trim(bounds_name))
    call read_integral_time_data(bounds_var, bounds)
    if (size(bounds, 1) /= 2_i4 .or. size(bounds, 2) /= size(values)) &
      call error_message("time_stepping: invalid time bounds shape")
    do i = 1_i4, size(values)
      if (bounds(2_i4, i) <= bounds(1_i4, i)) &
        call error_message("time_stepping: invalid time bounds at entry ", trim(num2str(i)))
      if (values(i) < bounds(1_i4, i) .or. values(i) > bounds(2_i4, i)) &
        call error_message("time_stepping: time value outside bounds at entry ", trim(num2str(i)))
      if (i > 1_i4) then
        if (bounds(1_i4, i) /= bounds(2_i4, i - 1_i4)) call error_message( &
          "time_stepping: non-contiguous time bounds between entries ", trim(num2str(i - 1_i4)), &
          " and ", trim(num2str(i)))
      end if
    end do
  end subroutine check_and_read_time

  subroutine read_integral_time_data_1d(nc_var, values)
    type(NcVariable), intent(in) :: nc_var
    integer(i4), allocatable, intent(out) :: values(:)
    real(dp), allocatable :: real_values(:)

    select case(trim(nc_var%getDtype()))
      case("f32", "f64")
        call nc_var%getData(real_values)
        allocate(values(size(real_values)))
        call convert_integral_time_data(real_values, values, nc_var%getName())
      case("i8", "i16", "i32", "i64")
        call nc_var%getData(values)
      case default
        call error_message("time_stepping: unsupported time variable type: ", trim(nc_var%getDtype()))
    end select
  end subroutine read_integral_time_data_1d

  subroutine read_integral_time_data_2d(nc_var, values)
    type(NcVariable), intent(in) :: nc_var
    integer(i4), allocatable, intent(out) :: values(:, :)
    real(dp), allocatable :: real_values(:, :), real_flat(:)
    integer(i4), allocatable :: integer_flat(:)

    select case(trim(nc_var%getDtype()))
      case("f32", "f64")
        call nc_var%getData(real_values)
        allocate(real_flat(size(real_values)), source=reshape(real_values, [size(real_values)]))
        allocate(integer_flat(size(real_values)))
        call convert_integral_time_data(real_flat, integer_flat, nc_var%getName())
        allocate(values(size(real_values, 1), size(real_values, 2)), &
                 source=reshape(integer_flat, shape(real_values)))
      case("i8", "i16", "i32", "i64")
        call nc_var%getData(values)
      case default
        call error_message("time_stepping: unsupported time variable type: ", trim(nc_var%getDtype()))
    end select
  end subroutine read_integral_time_data_2d

  subroutine convert_integral_time_data(real_values, values, name)
    real(dp), intent(in) :: real_values(:)
    integer(i4), intent(out) :: values(:)
    character(*), intent(in) :: name
    real(dp) :: lower_i4, upper_i4
    integer(i4) :: i

    lower_i4 = -real(huge(0_i4), dp) - 1.0_dp
    upper_i4 = real(huge(0_i4), dp)
    do i = 1_i4, size(real_values)
      if (.not.ieee_is_finite(real_values(i))) &
        call error_message("time_stepping: non-finite value in time variable: ", trim(name))
      if (real_values(i) < lower_i4 .or. real_values(i) > upper_i4) &
        call error_message("time_stepping: value outside i4 range in time variable: ", trim(name))
      if (real_values(i) /= anint(real_values(i))) call error_message( &
        "time_stepping: time values must be integral in their declared units; use finer CF time units: ", trim(name))
      values(i) = nint(real_values(i), i4)
    end do
  end subroutine convert_integral_time_data

  subroutine infer_center_time_bounds(values, delta, ref_time, bounds)
    integer(i4), intent(in) :: values(:)
    type(timedelta), intent(in) :: delta
    type(datetime), intent(in) :: ref_time
    integer(i4), allocatable, intent(out) :: bounds(:, :)
    integer(i4) :: dt

    if (size(values) < 2_i4) &
      call error_message("time_stepping: can't infer bounds from a single center timestamp")
    if (calendar_center_bounds(values, delta, ref_time, yearly, bounds)) return
    if (calendar_center_bounds(values, delta, ref_time, monthly, bounds)) return

    dt = values(2_i4) - values(1_i4)
    if (dt <= 0_i4 .or. any(values(2_i4:) - values(:size(values) - 1_i4) /= dt)) &
      call error_message("time_stepping: can't infer bounds from varying center timestamps")
    if (mod(dt, 2_i4) /= 0_i4) &
      call error_message("time_stepping: center bounds need finer CF time units")
    if (allocated(bounds)) deallocate(bounds)
    allocate(bounds(2_i4, size(values)))
    bounds(1_i4, :) = values - dt / 2_i4
    bounds(2_i4, :) = values + dt / 2_i4
  end subroutine infer_center_time_bounds

  logical function calendar_center_bounds(values, delta, ref_time, timestep, bounds) result(valid)
    integer(i4), intent(in) :: values(:)
    type(timedelta), intent(in) :: delta
    type(datetime), intent(in) :: ref_time
    integer(i4), intent(in) :: timestep
    integer(i4), allocatable, intent(out) :: bounds(:, :)
    type(datetime) :: center_date, lower_date, upper_date
    integer(i4) :: i, lower, upper

    valid = .false.
    allocate(bounds(2_i4, size(values)))
    do i = 1_i4, size(values)
      center_date = ref_time + values(i) * delta
      select case(timestep)
        case(yearly)
          lower_date = center_date%year_start()
          upper_date = lower_date%next_new_year()
        case(monthly)
          lower_date = center_date%month_start()
          upper_date = lower_date%next_new_month()
        case default
          return
      end select
      if (.not.datetime_to_time_value(lower_date, ref_time, delta, lower)) return
      if (.not.datetime_to_time_value(upper_date, ref_time, delta, upper)) return
      if (2_i8 * int(values(i), i8) /= int(lower, i8) + int(upper, i8)) return
      if (i > 1_i4) then
        if (lower /= bounds(2_i4, i - 1_i4)) return
      end if
      bounds(:, i) = [lower, upper]
    end do
    valid = .true.
  end function calendar_center_bounds

  logical function datetime_to_time_value(time, ref_time, delta, value) result(valid)
    type(datetime), intent(in) :: time
    type(datetime), intent(in) :: ref_time
    type(timedelta), intent(in) :: delta
    integer(i4), intent(out) :: value
    type(timedelta) :: offset
    integer(i8) :: delta_seconds, seconds, value_i8

    valid = .false.
    offset = time - ref_time
    seconds = offset%total_seconds()
    delta_seconds = delta%total_seconds()
    if (mod(seconds, delta_seconds) /= 0_i8) return
    value_i8 = seconds / delta_seconds
    if (value_i8 < -int(huge(0_i4), i8) - 1_i8 .or. value_i8 > int(huge(0_i4), i8)) return
    value = int(value_i8, i4)
    valid = .true.
  end function datetime_to_time_value

  !> \brief Determine time stepping and bounds from a NetCDF time coordinate.
  subroutine time_stepping(t_var, ref_time, delta, timestep, t_values, t_bounds, timestamp)
    type(NcVariable), intent(in) :: t_var !< time variable
    type(datetime), intent(out) :: ref_time !< reference time in units
    type(timedelta), intent(out) :: delta !< time delta in units
    integer(i4), intent(out) :: timestep !< time step indicator
    integer(i4), allocatable, dimension(:), intent(out) :: t_values !< time axis values for end of time spans
    integer(i4), allocatable, dimension(:), intent(out) :: t_bounds !< time axis bound values
    integer(i4), optional, intent(in) :: timestamp !< timestamp selector when bounds are missing (default: \ref end_timestamp)

    integer(i4), allocatable, dimension(:) :: tmp_arr
    type(timedelta) :: loc_delta
    type(datetime) :: loc_date
    integer(i4) :: stamp
    character(len=256) :: tmp_str
    integer(i4), allocatable, dimension(:, :) :: t_bnds

    stamp = optval(timestamp, end_timestamp)

    call t_var%getAttribute("units", tmp_str)
    call decode_cf_time_units(trim(tmp_str), delta, ref_time)
    call check_and_read_time(t_var, tmp_arr, t_bnds)
    if (allocated(t_bnds)) then
      timestep = infer_time_timestep_from_bounds(t_bnds, delta, ref_time)
      t_values = t_bnds(2_i4, :)
    else if (stamp == end_timestamp) then
      timestep = infer_time_timestep_from_values(tmp_arr, delta, ref_time)
      allocate(t_values(size(tmp_arr)), source=tmp_arr)
    else if (stamp == start_timestamp) then
      timestep = infer_time_timestep_from_values(tmp_arr, delta, ref_time)
      if (size(tmp_arr) == 1_i4) then
        allocate(t_values(1), source=tmp_arr(1) + 1_i4)
      else
        allocate(t_values(size(tmp_arr)))
        t_values(:size(tmp_arr) - 1_i4) = tmp_arr(2:)
        loc_date = ref_time + tmp_arr(size(tmp_arr)) * delta
        select case(timestep)
          case(yearly)
            loc_delta = loc_date%next_new_year() - ref_time
            t_values(size(tmp_arr)) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
          case(monthly)
            loc_delta = loc_date%next_new_month() - ref_time
            t_values(size(tmp_arr)) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
          case(varying)
            ! Use one coordinate unit for the unknown outer edge of an unbounded varying axis.
            t_values(size(tmp_arr)) = tmp_arr(size(tmp_arr)) + 1_i4
          case default
            t_values(size(tmp_arr)) = 2_i4 * tmp_arr(size(tmp_arr)) - tmp_arr(size(tmp_arr) - 1_i4)
        end select
      end if
    else if (stamp == center_timestamp) then
      call infer_center_time_bounds(tmp_arr, delta, ref_time, t_bnds)
      timestep = infer_time_timestep_from_bounds(t_bnds, delta, ref_time)
      t_values = t_bnds(2_i4, :)
    else
      call error_message("time_stepping: invalid timestamp selector")
    end if

    allocate(t_bounds(size(t_values) + 1_i4))
    t_bounds(2:) = t_values
    if (allocated(t_bnds)) then
      t_bounds(1) = t_bnds(1, 1)
    else if (stamp == start_timestamp) then
      t_bounds(1) = tmp_arr(1)
    else
      loc_date = ref_time + t_values(1) * delta
      select case(timestep)
        case(varying)
          t_bounds(1) = t_values(1) - 1_i4
        case(yearly)
          loc_delta = loc_date%previous_new_year() - ref_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case(monthly)
          loc_delta = loc_date%previous_new_month() - ref_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case(daily)
          loc_delta = loc_date%previous_new_day() - ref_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
        case default
          loc_delta = loc_date - timestep * one_hour() - ref_time
          t_bounds(1) = int(loc_delta%total_seconds() / delta%total_seconds(), i4)
      end select
    end if
  end subroutine time_stepping

end module mo_netcdf_utils
