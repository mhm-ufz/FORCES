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

  use mo_datetime, only: datetime, timedelta, decode_cf_time_units, one_day, one_hour, &
                         hourly, daily, monthly, yearly, varying, end_timestamp, start_timestamp, &
                         infer_time_timestep_from_bounds, infer_time_timestep_from_values
  use mo_kind, only: i4
  use mo_message, only: error_message
  use mo_netcdf, only: NcVariable
  use mo_utils, only: optval

  implicit none

  private

  public :: var
  public :: add_var
  public :: var_index
  public :: time_stepping
  public :: read_units

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
    type(NcVariable) :: tb_var
    integer(i4), allocatable, dimension(:, :) :: t_bnds

    stamp = optval(timestamp, end_timestamp)

    call t_var%getAttribute("units", tmp_str)
    call decode_cf_time_units(trim(tmp_str), delta, ref_time)
    if (t_var%hasAttribute("bounds")) then
      call t_var%getAttribute("bounds", tmp_str)
      tb_var = t_var%parent%getVariable(trim(tmp_str))
      call tb_var%getData(t_bnds)
      t_values = t_bnds(2, :)
    else if (stamp == end_timestamp) then
      call t_var%getData(t_values)
    else if (stamp == start_timestamp) then
      call t_var%getData(tmp_arr)
      if (size(tmp_arr) == 1_i4) then
        allocate(t_values(1), source=2_i4 * tmp_arr(1))
      else
        allocate(t_values(size(tmp_arr)))
        t_values(:size(tmp_arr) - 1_i4) = tmp_arr(2:)
        t_values(size(tmp_arr)) = 2_i4 * tmp_arr(size(tmp_arr)) - tmp_arr(size(tmp_arr) - 1_i4)
      end if
    else
      call error_message("time_stepping: can't convert center of time-span to output time values")
    end if

    if (size(t_values) == 1_i4) then
      if (allocated(t_bnds)) then
        loc_delta = (t_bnds(2, 1) - t_bnds(1, 1)) * delta
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
        timestep = infer_time_timestep_from_bounds(t_bnds, delta, ref_time)
      else
        timestep = infer_time_timestep_from_values(t_values, delta, ref_time)
      end if
    end if

    allocate(t_bounds(size(t_values) + 1_i4))
    t_bounds(2:) = t_values
    if (allocated(t_bnds)) then
      t_bounds(1) = t_bnds(1, 1)
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
