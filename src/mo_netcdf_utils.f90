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

  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan

  use mo_constants, only: nodata_dp, nodata_sp, nodata_i1, nodata_i2, nodata_i4, nodata_i8
  use mo_datetime, only: datetime, timedelta, decode_cf_time_units, one_hour, &
                         daily, monthly, yearly, varying, start_timestamp, center_timestamp, end_timestamp, &
                         infer_time_timestep_from_bounds, infer_time_timestep_from_values
  use mo_kind, only: i4, i8, dp, sp
  use mo_message, only: error_message, warn_message
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
  public :: netcdf_packing
  public :: configure_output_packing
  public :: write_packing_attributes
  public :: discover_input_packing
  public :: read_cf_packed
  public :: write_cf_packed
  public :: validate_cf_integer

  interface read_integral_time_data
    module procedure read_integral_time_data_1d
    module procedure read_integral_time_data_2d
  end interface read_integral_time_data

  interface read_cf_packed
    module procedure read_cf_packed_1d_sp
    module procedure read_cf_packed_1d_dp
    module procedure read_cf_packed_2d_sp
    module procedure read_cf_packed_2d_dp
    module procedure read_cf_packed_3d_sp
    module procedure read_cf_packed_3d_dp
    module procedure read_cf_packed_4d_sp
    module procedure read_cf_packed_4d_dp
  end interface read_cf_packed

  interface write_cf_packed
    module procedure write_cf_packed_1d_sp
    module procedure write_cf_packed_1d_dp
    module procedure write_cf_packed_2d_sp
    module procedure write_cf_packed_2d_dp
  end interface write_cf_packed

  !> \class var
  !> \brief Variable metadata definition for NetCDF IO variables.
  type var
    character(:), allocatable :: name          !< variable name in the NetCDF file (required)
    character(:), allocatable :: long_name     !< descriptive variable name
    character(:), allocatable :: standard_name !< standard variable name following CF-Conventions
    character(:), allocatable :: units         !< variable units
    character(:), allocatable :: dtype         !< variable data type in file ('f32', 'f64' (default), 'i8', 'i16', 'i32', 'i64')
    character(:), allocatable :: kind          !< kind of array for IO ('sp', 'dp' (real def.), 'i1', 'i2', 'i4' (int def.), 'i8')
    real(dp), allocatable :: scale_factor      !< CF scale factor for packed integer data
    real(dp), allocatable :: add_offset        !< CF offset for packed integer data
    character(:), allocatable :: scale_dtype   !< arithmetic and attribute precision ('f32' or 'f64')
    logical :: static = .false.                !< static variable (without time dimension)
    logical :: allow_static = .false.          !< accept a static variable on input when static=.false.
    logical :: avg = .false.                   !< average data (only for writing)
    logical :: layered = .false.               !< variable is layered
  contains
    procedure, public :: meta => var_meta
  end type var

  !> \class netcdf_packing
  !> \brief Effective CF packing metadata used by the high-level IO modules.
  type netcdf_packing
    logical :: enabled = .false.
    character(:), allocatable :: name
    character(:), allocatable :: dtype
    character(:), allocatable :: scale_dtype
    character(:), allocatable :: kind
    real(dp) :: scale_factor = 1.0_dp
    real(dp) :: add_offset = 0.0_dp
    real(dp) :: fill_value = 0.0_dp
    real(dp), allocatable :: missing_values(:)
    logical :: has_valid_min = .false.
    logical :: has_valid_max = .false.
    real(dp) :: valid_min = 0.0_dp
    real(dp) :: valid_max = 0.0_dp
  end type netcdf_packing

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
    if (allocated(this%scale_factor)) var_meta%scale_factor = this%scale_factor
    if (allocated(this%add_offset)) var_meta%add_offset = this%add_offset
    if (allocated(this%scale_dtype)) var_meta%scale_dtype = this%scale_dtype
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

  !> \brief Resolve and validate CF packing requested for an output variable.
  subroutine configure_output_packing(variable, packing, context)
    class(var), intent(inout) :: variable
    type(netcdf_packing), intent(out) :: packing
    character(*), optional, intent(in) :: context
    character(:), allocatable :: context_

    context_ = "configure_output_packing"
    if (present(context)) context_ = trim(context)
    packing%enabled = allocated(variable%scale_factor) .or. allocated(variable%add_offset)
    if (.not.packing%enabled) return
    packing%name = variable%name
    if (.not.allocated(variable%dtype)) &
      call error_message(context_ // ": packed output requires an explicit integer dtype: ", variable%name)
    packing%dtype = trim(variable%dtype)
    if (.not.allocated(variable%kind)) variable%kind = "dp"
    if (variable%kind /= "sp" .and. variable%kind /= "dp") &
      call error_message(context_ // ": packed output requires kind sp or dp: ", variable%name)
    packing%kind = variable%kind
    if (.not.allocated(variable%scale_dtype)) then
      if (variable%kind == "sp") then
        variable%scale_dtype = "f32"
      else
        variable%scale_dtype = "f64"
      end if
    end if
    packing%scale_dtype = trim(variable%scale_dtype)
    call validate_packing_combination(packing%dtype, packing%scale_dtype, variable%name, context_)

    if (allocated(variable%scale_factor)) then
      if (packing%scale_dtype == "f32") variable%scale_factor = real(real(variable%scale_factor, sp), dp)
      packing%scale_factor = variable%scale_factor
    end if
    if (allocated(variable%add_offset)) then
      if (packing%scale_dtype == "f32") variable%add_offset = real(real(variable%add_offset, sp), dp)
      packing%add_offset = variable%add_offset
    end if
    call validate_transform(packing, variable%name, context_)
    packing%fill_value = packing_fill_value(packing%dtype)
  end subroutine configure_output_packing

  !> \brief Write explicitly requested CF packing attributes with their effective type.
  subroutine write_packing_attributes(variable, nc_var, packing)
    class(var), intent(in) :: variable
    type(NcVariable), intent(inout) :: nc_var
    type(netcdf_packing), intent(in) :: packing

    if (.not.packing%enabled) return
    if (packing%scale_dtype == "f32") then
      if (allocated(variable%scale_factor)) call nc_var%setAttribute("scale_factor", real(variable%scale_factor, sp))
      if (allocated(variable%add_offset)) call nc_var%setAttribute("add_offset", real(variable%add_offset, sp))
    else
      if (allocated(variable%scale_factor)) call nc_var%setAttribute("scale_factor", variable%scale_factor)
      if (allocated(variable%add_offset)) call nc_var%setAttribute("add_offset", variable%add_offset)
    end if
  end subroutine write_packing_attributes

  !> \brief Discover and validate CF packing metadata on an input variable.
  subroutine discover_input_packing(variable, nc_var, packing, context)
    class(var), intent(inout) :: variable
    type(NcVariable), intent(in) :: nc_var
    type(netcdf_packing), intent(out) :: packing
    character(*), optional, intent(in) :: context
    character(:), allocatable :: context_, scale_type, offset_type, requested_scale_dtype
    real(sp) :: value_sp
    real(dp) :: value_dp, requested_scale_factor, requested_add_offset, requested_value
    real(dp), allocatable :: missing(:)
    real(dp) :: valid_range(2)
    integer(i4) :: n_fill, n_missing, pos
    logical :: has_scale, has_offset, has_fill, has_missing
    logical :: requested_scale, requested_offset, requested_dtype

    context_ = "discover_input_packing"
    if (present(context)) context_ = trim(context)
    requested_scale = allocated(variable%scale_factor)
    requested_offset = allocated(variable%add_offset)
    requested_dtype = allocated(variable%scale_dtype)
    if (requested_scale) requested_scale_factor = variable%scale_factor
    if (requested_offset) requested_add_offset = variable%add_offset
    if (requested_dtype) requested_scale_dtype = variable%scale_dtype
    scale_type = ""
    offset_type = ""
    if (allocated(variable%scale_factor)) deallocate(variable%scale_factor)
    if (allocated(variable%add_offset)) deallocate(variable%add_offset)
    if (allocated(variable%scale_dtype)) deallocate(variable%scale_dtype)
    has_scale = nc_var%hasAttribute("scale_factor")
    has_offset = nc_var%hasAttribute("add_offset")
    packing%enabled = has_scale .or. has_offset
    if (.not.packing%enabled) then
      if (requested_scale) call warn_message(context_, &
        ": requested scale_factor is absent from the file and will be ignored: ", variable%name)
      if (requested_offset) call warn_message(context_, &
        ": requested add_offset is absent from the file and will be ignored: ", variable%name)
      if (requested_dtype) call warn_message(context_, &
        ": requested scale_dtype is absent from the file and will be ignored: ", variable%name)
      return
    end if
    packing%name = variable%name

    packing%dtype = trim(variable%dtype)
    if (has_scale) then
      call validate_scalar_attribute(nc_var, "scale_factor", variable%name, context_)
      scale_type = trim(nc_var%getAttributeDtype("scale_factor"))
      call validate_scale_dtype(scale_type, variable%name, context_)
    end if
    if (has_offset) then
      call validate_scalar_attribute(nc_var, "add_offset", variable%name, context_)
      offset_type = trim(nc_var%getAttributeDtype("add_offset"))
      call validate_scale_dtype(offset_type, variable%name, context_)
    end if
    if (has_scale .and. has_offset) then
      if (scale_type /= offset_type) &
        call error_message(context_ // ": scale_factor and add_offset must have the same dtype: ", variable%name)
      packing%scale_dtype = scale_type
    else if (has_scale) then
      packing%scale_dtype = scale_type
    else
      packing%scale_dtype = offset_type
    end if
    variable%scale_dtype = packing%scale_dtype
    call validate_packing_combination(packing%dtype, packing%scale_dtype, variable%name, context_)

    if (has_scale) then
      if (packing%scale_dtype == "f32") then
        call nc_var%getAttribute("scale_factor", value_sp)
        variable%scale_factor = real(value_sp, dp)
      else
        call nc_var%getAttribute("scale_factor", value_dp)
        variable%scale_factor = value_dp
      end if
      packing%scale_factor = variable%scale_factor
    end if
    if (has_offset) then
      if (packing%scale_dtype == "f32") then
        call nc_var%getAttribute("add_offset", value_sp)
        variable%add_offset = real(value_sp, dp)
      else
        call nc_var%getAttribute("add_offset", value_dp)
        variable%add_offset = value_dp
      end if
      packing%add_offset = variable%add_offset
    end if
    call validate_transform(packing, variable%name, context_)

    if (requested_dtype) then
      if (trim(requested_scale_dtype) /= packing%scale_dtype) call warn_message(context_, &
        ": requested scale_dtype differs from the file and will be ignored: ", variable%name, &
        ", requested=", requested_scale_dtype, ", file=", packing%scale_dtype)
    end if
    if (requested_scale) then
      if (.not.has_scale) then
        call warn_message(context_, &
          ": requested scale_factor is absent from the file and will be ignored: ", variable%name)
      else
        requested_value = requested_scale_factor
        if (packing%scale_dtype == "f32") requested_value = real(real(requested_value, sp), dp)
        if (requested_value /= variable%scale_factor) call warn_message(context_, &
          ": requested scale_factor differs from the file and will be ignored: ", variable%name, &
          ", requested=", num2str(requested_value), ", file=", num2str(variable%scale_factor))
      end if
    end if
    if (requested_offset) then
      if (.not.has_offset) then
        call warn_message(context_, &
          ": requested add_offset is absent from the file and will be ignored: ", variable%name)
      else
        requested_value = requested_add_offset
        if (packing%scale_dtype == "f32") requested_value = real(real(requested_value, sp), dp)
        if (requested_value /= variable%add_offset) call warn_message(context_, &
          ": requested add_offset differs from the file and will be ignored: ", variable%name, &
          ", requested=", num2str(requested_value), ", file=", num2str(variable%add_offset))
      end if
    end if

    has_fill = nc_var%hasAttribute("_FillValue")
    has_missing = nc_var%hasAttribute("missing_value")
    n_fill = merge(1_i4, 0_i4, has_fill)
    n_missing = 0_i4
    if (has_missing) n_missing = nc_var%getAttributeLength("missing_value")
    if (n_fill + n_missing > 0_i4) allocate(packing%missing_values(n_fill + n_missing))
    pos = 0_i4
    if (has_fill) then
      call validate_scalar_attribute(nc_var, "_FillValue", variable%name, context_)
      pos = pos + 1_i4
      call nc_var%getAttribute("_FillValue", packing%missing_values(pos))
      packing%fill_value = packing%missing_values(pos)
    end if
    if (has_missing) then
      if (n_missing < 1_i4) call error_message(context_ // ": empty missing_value attribute: ", variable%name)
      allocate(missing(n_missing))
      call nc_var%getAttribute("missing_value", missing)
      packing%missing_values(pos + 1_i4:pos + n_missing) = missing
    end if

    if (nc_var%hasAttribute("valid_range")) then
      if (nc_var%getAttributeLength("valid_range") /= 2_i4) &
        call error_message(context_ // ": valid_range must contain two values: ", variable%name)
      call nc_var%getAttribute("valid_range", valid_range)
      packing%valid_min = valid_range(1)
      packing%valid_max = valid_range(2)
      packing%has_valid_min = .true.
      packing%has_valid_max = .true.
    else
      if (nc_var%hasAttribute("valid_min")) then
        call validate_scalar_attribute(nc_var, "valid_min", variable%name, context_)
        call nc_var%getAttribute("valid_min", packing%valid_min)
        packing%has_valid_min = .true.
      end if
      if (nc_var%hasAttribute("valid_max")) then
        call validate_scalar_attribute(nc_var, "valid_max", variable%name, context_)
        call nc_var%getAttribute("valid_max", packing%valid_max)
        packing%has_valid_max = .true.
      end if
    end if
    if (packing%has_valid_min .and. packing%has_valid_max) then
      if (packing%valid_min > packing%valid_max) &
        call error_message(context_ // ": invalid packed validity limits: ", variable%name)
    end if
  end subroutine discover_input_packing

  subroutine validate_scalar_attribute(nc_var, attribute, name, context)
    type(NcVariable), intent(in) :: nc_var
    character(*), intent(in) :: attribute, name, context
    if (nc_var%getAttributeLength(attribute) /= 1_i4) &
      call error_message(context // ": " // attribute // " must be scalar: ", name)
  end subroutine validate_scalar_attribute

  subroutine validate_scale_dtype(scale_dtype, name, context)
    character(*), intent(in) :: scale_dtype, name, context
    if (trim(scale_dtype) /= "f32" .and. trim(scale_dtype) /= "f64") &
      call error_message(context // ": scale_factor/add_offset must use f32 or f64: ", name)
  end subroutine validate_scale_dtype

  subroutine validate_packing_combination(dtype, scale_dtype, name, context)
    character(*), intent(in) :: dtype, scale_dtype, name, context
    call validate_scale_dtype(scale_dtype, name, context)
    select case(trim(scale_dtype))
      case("f32")
        if (trim(dtype) /= "i8" .and. trim(dtype) /= "i16") &
          call error_message(context // ": f32 packing requires dtype i8 or i16: ", name)
      case("f64")
        if (trim(dtype) /= "i8" .and. trim(dtype) /= "i16" .and. trim(dtype) /= "i32") &
          call error_message(context // ": f64 packing requires dtype i8, i16, or i32: ", name)
    end select
  end subroutine validate_packing_combination

  subroutine validate_transform(packing, name, context)
    type(netcdf_packing), intent(in) :: packing
    character(*), intent(in) :: name, context
    if (.not.ieee_is_finite(packing%scale_factor) .or. packing%scale_factor == 0.0_dp) &
      call error_message(context // ": scale_factor must be finite and nonzero: ", name)
    if (.not.ieee_is_finite(packing%add_offset)) &
      call error_message(context // ": add_offset must be finite: ", name)
  end subroutine validate_transform

  real(dp) function packing_fill_value(dtype)
    character(*), intent(in) :: dtype
    packing_fill_value = 0.0_dp
    select case(trim(dtype))
      case("i8")
        packing_fill_value = real(nodata_i1, dp)
      case("i16")
        packing_fill_value = real(nodata_i2, dp)
      case("i32")
        packing_fill_value = real(nodata_i4, dp)
      case default
        call error_message("packing_fill_value: unsupported packed dtype: ", dtype)
    end select
  end function packing_fill_value

  pure elemental logical function packed_is_missing_sp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(sp), intent(in) :: value
    integer(i4) :: i
    packed_is_missing_sp = .false.
    if (allocated(packing%missing_values)) then
      do i = 1_i4, size(packing%missing_values)
        packed_is_missing_sp = packed_is_missing_sp .or. value == real(packing%missing_values(i), sp)
      end do
    end if
    if (packing%has_valid_min) packed_is_missing_sp = packed_is_missing_sp .or. value < real(packing%valid_min, sp)
    if (packing%has_valid_max) packed_is_missing_sp = packed_is_missing_sp .or. value > real(packing%valid_max, sp)
  end function packed_is_missing_sp

  pure elemental logical function packed_is_missing_dp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(dp), intent(in) :: value
    integer(i4) :: i
    packed_is_missing_dp = .false.
    if (allocated(packing%missing_values)) then
      do i = 1_i4, size(packing%missing_values)
        packed_is_missing_dp = packed_is_missing_dp .or. value == packing%missing_values(i)
      end do
    end if
    if (packing%has_valid_min) packed_is_missing_dp = packed_is_missing_dp .or. value < packing%valid_min
    if (packing%has_valid_max) packed_is_missing_dp = packed_is_missing_dp .or. value > packing%valid_max
  end function packed_is_missing_dp

  pure elemental real(sp) function unpack_value_sp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(sp), intent(in) :: value
    if (packed_is_missing_sp(packing, value)) then
      unpack_value_sp = nodata_sp
    else
      unpack_value_sp = value * real(packing%scale_factor, sp) + real(packing%add_offset, sp)
    end if
  end function unpack_value_sp

  pure elemental real(dp) function unpack_value_dp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(dp), intent(in) :: value
    if (packed_is_missing_dp(packing, value)) then
      unpack_value_dp = nodata_dp
    else
      unpack_value_dp = value * packing%scale_factor + packing%add_offset
    end if
  end function unpack_value_dp

  pure elemental real(sp) function pack_value_sp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(sp), intent(in) :: value
    if (ieee_is_nan(value) .or. value == nodata_sp) then
      pack_value_sp = real(packing%fill_value, sp)
    else
      pack_value_sp = anint((value - real(packing%add_offset, sp)) / real(packing%scale_factor, sp))
    end if
  end function pack_value_sp

  pure elemental real(dp) function pack_value_dp(packing, value)
    type(netcdf_packing), intent(in) :: packing
    real(dp), intent(in) :: value
    if (ieee_is_nan(value) .or. value == nodata_dp) then
      pack_value_dp = packing%fill_value
    else
      pack_value_dp = anint((value - packing%add_offset) / packing%scale_factor)
    end if
  end function pack_value_dp

  subroutine validate_pack_sp(packing, values)
    type(netcdf_packing), intent(in) :: packing
    real(sp), intent(in) :: values(:)
    real(sp) :: code, lower, upper
    integer(i4) :: i
    call storage_bounds_sp(packing%dtype, lower, upper)
    do i = 1_i4, size(values)
      if (ieee_is_nan(values(i)) .or. values(i) == nodata_sp) cycle
      if (.not.ieee_is_finite(values(i))) call error_message("CF packing: non-finite input value: ", packing%name)
      code = anint((values(i) - real(packing%add_offset, sp)) / real(packing%scale_factor, sp))
      if (.not.ieee_is_finite(code) .or. code < lower .or. code > upper) &
        call error_message("CF packing: packed integer overflow: ", packing%name)
      if (code == real(packing%fill_value, sp)) &
        call error_message("CF packing: value rounds to reserved fill code: ", packing%name)
    end do
  end subroutine validate_pack_sp

  subroutine validate_pack_dp(packing, values)
    type(netcdf_packing), intent(in) :: packing
    real(dp), intent(in) :: values(:)
    real(dp) :: code, lower, upper
    integer(i4) :: i
    call storage_bounds_dp(packing%dtype, lower, upper)
    do i = 1_i4, size(values)
      if (ieee_is_nan(values(i)) .or. values(i) == nodata_dp) cycle
      if (.not.ieee_is_finite(values(i))) call error_message("CF packing: non-finite input value: ", packing%name)
      code = anint((values(i) - packing%add_offset) / packing%scale_factor)
      if (.not.ieee_is_finite(code) .or. code < lower .or. code > upper) &
        call error_message("CF packing: packed integer overflow: ", packing%name)
      if (code == packing%fill_value) &
        call error_message("CF packing: value rounds to reserved fill code: ", packing%name)
    end do
  end subroutine validate_pack_dp

  subroutine validate_unpack_sp(packing, values)
    type(netcdf_packing), intent(in) :: packing
    real(sp), intent(in) :: values(:)
    real(sp) :: value
    integer(i4) :: i
    do i = 1_i4, size(values)
      if (packed_is_missing_sp(packing, values(i))) cycle
      if (.not.ieee_is_finite(values(i))) &
        call error_message("CF unpacking: non-finite packed value: ", packing%name)
      value = values(i) * real(packing%scale_factor, sp) + real(packing%add_offset, sp)
      if (.not.ieee_is_finite(value)) &
        call error_message("CF unpacking: non-finite transformed value: ", packing%name)
    end do
  end subroutine validate_unpack_sp

  subroutine validate_unpack_dp(packing, values)
    type(netcdf_packing), intent(in) :: packing
    real(dp), intent(in) :: values(:)
    real(dp) :: value
    integer(i4) :: i
    do i = 1_i4, size(values)
      if (packed_is_missing_dp(packing, values(i))) cycle
      if (.not.ieee_is_finite(values(i))) &
        call error_message("CF unpacking: non-finite packed value: ", packing%name)
      value = values(i) * packing%scale_factor + packing%add_offset
      if (.not.ieee_is_finite(value)) &
        call error_message("CF unpacking: non-finite transformed value: ", packing%name)
    end do
  end subroutine validate_unpack_dp

  subroutine storage_bounds_sp(dtype, lower, upper)
    character(*), intent(in) :: dtype
    real(sp), intent(out) :: lower, upper
    select case(trim(dtype))
      case("i8")
        lower = -128.0_sp
        upper = 127.0_sp
      case("i16")
        lower = -32768.0_sp
        upper = 32767.0_sp
      case default
        call error_message("CF packing: unsupported f32 storage dtype: ", dtype)
    end select
  end subroutine storage_bounds_sp

  subroutine storage_bounds_dp(dtype, lower, upper)
    character(*), intent(in) :: dtype
    real(dp), intent(out) :: lower, upper
    select case(trim(dtype))
      case("i8")
        lower = -128.0_dp
        upper = 127.0_dp
      case("i16")
        lower = -32768.0_dp
        upper = 32767.0_dp
      case("i32")
        lower = -2147483648.0_dp
        upper = 2147483647.0_dp
      case default
        call error_message("CF packing: unsupported f64 storage dtype: ", dtype)
    end select
  end subroutine storage_bounds_dp

  !> \brief Validate truncating unpacked real values into an integer application kind.
  subroutine validate_cf_integer(values, target_kind, name)
    real(dp), intent(in) :: values(:)
    character(*), intent(in) :: target_kind
    character(*), intent(in) :: name
    real(dp) :: lower, upper, nodata, truncated
    integer(i4) :: i

    select case(trim(target_kind))
      case("i1")
        lower = -128.0_dp
        upper = 127.0_dp
        nodata = real(nodata_i1, dp)
      case("i2")
        lower = -32768.0_dp
        upper = 32767.0_dp
        nodata = real(nodata_i2, dp)
      case("i4")
        lower = -2147483648.0_dp
        upper = 2147483647.0_dp
        nodata = real(nodata_i4, dp)
      case("i8")
        lower = -real(huge(0_i8), dp) - 1.0_dp
        upper = nearest(real(huge(0_i8), dp), -1.0_dp)
        nodata = real(nodata_i8, dp)
      case default
        call error_message("CF unpacking: unsupported integer destination kind: ", target_kind)
    end select

    do i = 1_i4, size(values)
      if (values(i) == nodata_dp) cycle
      if (.not.ieee_is_finite(values(i))) &
        call error_message("CF unpacking: non-finite integer conversion: ", name)
      truncated = aint(values(i))
      if (truncated < lower .or. truncated > upper) &
        call error_message("CF unpacking: destination integer overflow: ", name)
      if (truncated == nodata) &
        call error_message("CF unpacking: value converts to reserved destination nodata: ", name)
    end do
  end subroutine validate_cf_integer

  subroutine write_cf_packed_1d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(in) :: data(:)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:)
    real(dp), allocatable :: packed_dp(:)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp, source=data)
      call validate_pack_sp(packing, packed_sp)
      packed_sp = pack_value_sp(packing, packed_sp)
      call nc_var%setData(packed_sp, start=start, cnt=cnt)
    else
      allocate(packed_dp, source=real(data, dp))
      call validate_pack_dp(packing, packed_dp)
      packed_dp = pack_value_dp(packing, packed_dp)
      call nc_var%setData(packed_dp, start=start, cnt=cnt)
    end if
  end subroutine write_cf_packed_1d_sp

  subroutine write_cf_packed_1d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(in) :: data(:)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:)
    real(dp), allocatable :: packed_dp(:)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp, source=real(data, sp))
      call validate_pack_sp(packing, packed_sp)
      packed_sp = pack_value_sp(packing, packed_sp)
      call nc_var%setData(packed_sp, start=start, cnt=cnt)
    else
      allocate(packed_dp, source=data)
      call validate_pack_dp(packing, packed_dp)
      packed_dp = pack_value_dp(packing, packed_dp)
      call nc_var%setData(packed_dp, start=start, cnt=cnt)
    end if
  end subroutine write_cf_packed_1d_dp

  subroutine write_cf_packed_2d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(in) :: data(:, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :)
    real(dp), allocatable :: packed_dp(:, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp, source=data)
      call validate_pack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      packed_sp = pack_value_sp(packing, packed_sp)
      call nc_var%setData(packed_sp, start=start, cnt=cnt)
    else
      allocate(packed_dp, source=real(data, dp))
      call validate_pack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      packed_dp = pack_value_dp(packing, packed_dp)
      call nc_var%setData(packed_dp, start=start, cnt=cnt)
    end if
  end subroutine write_cf_packed_2d_sp

  subroutine write_cf_packed_2d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(in) :: data(:, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :)
    real(dp), allocatable :: packed_dp(:, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp, source=real(data, sp))
      call validate_pack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      packed_sp = pack_value_sp(packing, packed_sp)
      call nc_var%setData(packed_sp, start=start, cnt=cnt)
    else
      allocate(packed_dp, source=data)
      call validate_pack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      packed_dp = pack_value_dp(packing, packed_dp)
      call nc_var%setData(packed_dp, start=start, cnt=cnt)
    end if
  end subroutine write_cf_packed_2d_dp

  subroutine read_cf_packed_1d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(out) :: data(:)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:)
    real(dp), allocatable :: packed_dp(:)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, packed_sp)
      data = unpack_value_sp(packing, packed_sp)
    else
      allocate(packed_dp(size(data)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, packed_dp)
      data = real(unpack_value_dp(packing, packed_dp), sp)
    end if
  end subroutine read_cf_packed_1d_sp

  subroutine read_cf_packed_1d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(out) :: data(:)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:)
    real(dp), allocatable :: packed_dp(:)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, packed_sp)
      data = real(unpack_value_sp(packing, packed_sp), dp)
    else
      allocate(packed_dp(size(data)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, packed_dp)
      data = unpack_value_dp(packing, packed_dp)
    end if
  end subroutine read_cf_packed_1d_dp

  subroutine read_cf_packed_2d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(out) :: data(:, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :)
    real(dp), allocatable :: packed_dp(:, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = unpack_value_sp(packing, packed_sp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = real(unpack_value_dp(packing, packed_dp), sp)
    end if
  end subroutine read_cf_packed_2d_sp

  subroutine read_cf_packed_2d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(out) :: data(:, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :)
    real(dp), allocatable :: packed_dp(:, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = real(unpack_value_sp(packing, packed_sp), dp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = unpack_value_dp(packing, packed_dp)
    end if
  end subroutine read_cf_packed_2d_dp

  subroutine read_cf_packed_3d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(out) :: data(:, :, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :, :)
    real(dp), allocatable :: packed_dp(:, :, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2), size(data, 3)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = unpack_value_sp(packing, packed_sp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2), size(data, 3)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = real(unpack_value_dp(packing, packed_dp), sp)
    end if
  end subroutine read_cf_packed_3d_sp

  subroutine read_cf_packed_3d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(out) :: data(:, :, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :, :)
    real(dp), allocatable :: packed_dp(:, :, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2), size(data, 3)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = real(unpack_value_sp(packing, packed_sp), dp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2), size(data, 3)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = unpack_value_dp(packing, packed_dp)
    end if
  end subroutine read_cf_packed_3d_dp

  subroutine read_cf_packed_4d_sp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(sp), intent(out) :: data(:, :, :, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :, :, :)
    real(dp), allocatable :: packed_dp(:, :, :, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = unpack_value_sp(packing, packed_sp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = real(unpack_value_dp(packing, packed_dp), sp)
    end if
  end subroutine read_cf_packed_4d_sp

  subroutine read_cf_packed_4d_dp(packing, nc_var, data, start, cnt)
    type(netcdf_packing), intent(in) :: packing
    type(NcVariable), intent(in) :: nc_var
    real(dp), intent(out) :: data(:, :, :, :)
    integer(i4), optional, intent(in) :: start(:), cnt(:)
    real(sp), allocatable :: packed_sp(:, :, :, :)
    real(dp), allocatable :: packed_dp(:, :, :, :)
    if (packing%scale_dtype == "f32") then
      allocate(packed_sp(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))
      call nc_var%readInto(packed_sp, start=start, cnt=cnt)
      call validate_unpack_sp(packing, reshape(packed_sp, [size(packed_sp)]))
      data = real(unpack_value_sp(packing, packed_sp), dp)
    else
      allocate(packed_dp(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))
      call nc_var%readInto(packed_dp, start=start, cnt=cnt)
      call validate_unpack_dp(packing, reshape(packed_dp, [size(packed_dp)]))
      data = unpack_value_dp(packing, packed_dp)
    end if
  end subroutine read_cf_packed_4d_dp

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
