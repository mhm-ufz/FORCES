!> \file    mo_points_io.f90
!> \copydoc mo_points_io

!> \brief   NetCDF IO for data on unstructured point sets.
!> \details This module writes and reads variables defined on a \ref points_t.
!!          Data variables use either a time-slice layout or a station time-series
!!          layout. The module intentionally does not encode
!!          UGRID mesh topology; topology can be composed around point sets later.
!> \authors Sebastian Müller
!> \date Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_points_io

  use mo_constants, only: nodata_dp, nodata_sp, nodata_i1, nodata_i2, nodata_i4, nodata_i8
  use mo_datetime, only: datetime, timedelta, delta_from_string, time_values, time_units_delta, &
                         hourly, daily, monthly, yearly, varying, end_timestamp, start_timestamp, center_timestamp, &
                         instant_timestamp, no_time
  use mo_grid_helper, only: is_t_axis, is_x_axis, is_lon_coord
  use mo_kind, only: i1, i2, i4, i8, dp, sp
  use mo_message, only: error_message, warn_message
  use mo_netcdf, only: NcDataset, NcDimension, NcVariable, NF90_NOFILL
  use mo_netcdf_utils, only: var, add_var, var_index, time_stepping, read_units
  use mo_timeseries, only: time_t
  use mo_points, only: points_t, spherical
  use mo_string_utils, only: splitString
  use mo_utils, only: optval

  implicit none

  private

  public :: var
  public :: add_var
  public :: points_output_dataset
  public :: points_series_output_dataset
  public :: points_input_dataset
  public :: points_time_axis
  public :: points_id_index
  public :: points_id_indices

  interface points_id_index
    module procedure points_id_index_i4
    module procedure points_id_index_i8
  end interface points_id_index

  interface points_id_indices
    module procedure points_id_indices_i4
    module procedure points_id_indices_i8
  end interface points_id_indices

  !> \class points_output_variable
  !> \brief NetCDF output variable buffer for point-set data.
  type, extends(var) :: points_output_variable
    type(NcVariable) :: nc                             !< NetCDF variable handle
    type(points_t), pointer :: points => null()      !< point-set geometry
    real(sp), allocatable :: data_sp(:)          !< accumulated sp point data
    real(dp), allocatable :: data_dp(:)          !< accumulated dp point data
    integer(i1), allocatable :: data_i1(:)          !< accumulated i1 point data
    integer(i2), allocatable :: data_i2(:)          !< accumulated i2 point data
    integer(i4), allocatable :: data_i4(:)          !< accumulated i4 point data
    integer(i8), allocatable :: data_i8(:)          !< accumulated i8 point data
    logical :: time_series = .false.                   !< temporal variable uses station time-series layout
    logical :: static_written = .false.                !< static variable was already written
    integer(i4) :: counter = 0_i4                      !< number of accumulated updates
  contains
    procedure, public :: init => points_out_var_init
    procedure, private :: points_out_var_update_sp, points_out_var_update_dp, points_out_var_update_i1, points_out_var_update_i2,&
        & points_out_var_update_i4, points_out_var_update_i8
    generic, public :: update => points_out_var_update_sp, points_out_var_update_dp, points_out_var_update_i1,&
        & points_out_var_update_i2, points_out_var_update_i4, points_out_var_update_i8
    procedure, public :: write => points_out_var_write
  end type points_output_variable

  !> \class points_output_dataset
  !> \brief NetCDF writer for static or temporal variables on a point set.
  !! \par Examples
  !! - \ref 01_points_io.f90 : \copybrief 01_points_io.f90
  type :: points_output_dataset
    type(points_t), pointer :: points => null()          !< point-set geometry
    character(:), allocatable :: path                    !< output path
    type(NcDataset) :: nc                                !< NetCDF dataset
    type(points_output_variable), allocatable :: vars(:) !< output variable buffers
    integer(i4) :: nvars = 0_i4                          !< number of variables
    logical :: static = .true.                           !< all variables are static
    integer(i4) :: counter = 0_i4                        !< number of written time steps
    type(datetime) :: previous_time                      !< previous temporal write time
    type(datetime) :: start_time                         !< temporal output start time
    type(datetime) :: ref_time                           !< time-unit reference time
    type(timedelta) :: delta                             !< time-unit delta
    integer(i4) :: timestamp = end_timestamp             !< timestamp location selector
    integer(i4) :: deflate_level = 6_i4                  !< NetCDF deflate level
    logical :: time_series = .false.                     !< temporal variables use station time-series layout
  contains
    procedure, public :: init => points_output_init
    procedure, private :: points_output_update_sp, points_output_update_dp, points_output_update_i1, points_output_update_i2,&
        & points_output_update_i4, points_output_update_i8
    generic, public :: update => points_output_update_sp, points_output_update_dp, points_output_update_i1,&
        & points_output_update_i2, points_output_update_i4, points_output_update_i8
    procedure, public :: write => points_output_write
    procedure, public :: write_static => points_output_write_static
    procedure, public :: meta => points_output_meta
    procedure, public :: close => points_output_close
  end type points_output_dataset

  !> \class points_series_output_variable
  !> \brief NetCDF output variable for complete point time series.
  type, extends(var) :: points_series_output_variable
    type(NcVariable) :: nc                               !< NetCDF variable handle
    type(points_t), pointer :: points => null()          !< point-set geometry
    logical :: static_written = .false.                  !< static variable was already written
    logical, allocatable :: written(:)                   !< per-point write status
  contains
    procedure, public :: init => points_series_out_var_init
    procedure, private :: points_series_out_var_write_sp, points_series_out_var_write_dp, points_series_out_var_write_i1,&
        & points_series_out_var_write_i2, points_series_out_var_write_i4, points_series_out_var_write_i8
    generic, public :: write => points_series_out_var_write_sp, points_series_out_var_write_dp, points_series_out_var_write_i1,&
        & points_series_out_var_write_i2, points_series_out_var_write_i4, points_series_out_var_write_i8
    procedure, private :: points_series_out_var_write_static_sp, points_series_out_var_write_static_dp,&
        & points_series_out_var_write_static_i1, points_series_out_var_write_static_i2, points_series_out_var_write_static_i4,&
        & points_series_out_var_write_static_i8
    generic, public :: write_static => points_series_out_var_write_static_sp, points_series_out_var_write_static_dp,&
        & points_series_out_var_write_static_i1, points_series_out_var_write_static_i2, points_series_out_var_write_static_i4,&
        & points_series_out_var_write_static_i8
  end type points_series_output_variable

  !> \class points_series_output_dataset
  !> \brief NetCDF writer for complete point time series.
  !! \par Examples
  !! - \ref 03_points_timeseries_io.f90 : \copybrief 03_points_timeseries_io.f90
  type :: points_series_output_dataset
    type(points_t), pointer :: points => null()                 !< point-set geometry
    character(:), allocatable :: path                           !< output path
    type(NcDataset) :: nc                                       !< NetCDF dataset
    type(points_series_output_variable), allocatable :: vars(:) !< output variable handles
    integer(i4) :: nvars = 0_i4                                 !< number of variables
    type(time_t) :: time_axis                                    !< output time axis
    character(:), allocatable :: point_dim_name                  !< point dimension name
    integer(i4) :: deflate_level = 6_i4                         !< NetCDF deflate level
  contains
    procedure, public :: init => points_series_output_init
    procedure, private :: points_series_output_write_sp, points_series_output_write_dp, points_series_output_write_i1,&
        & points_series_output_write_i2, points_series_output_write_i4, points_series_output_write_i8
    generic, public :: write_series => points_series_output_write_sp, points_series_output_write_dp,&
        & points_series_output_write_i1, points_series_output_write_i2, points_series_output_write_i4, points_series_output_write_i8
    procedure, private :: points_series_output_write_static_sp, points_series_output_write_static_dp,&
        & points_series_output_write_static_i1, points_series_output_write_static_i2, points_series_output_write_static_i4,&
        & points_series_output_write_static_i8
    generic, public :: write_static => points_series_output_write_static_sp, points_series_output_write_static_dp,&
        & points_series_output_write_static_i1, points_series_output_write_static_i2, points_series_output_write_static_i4,&
        & points_series_output_write_static_i8
    procedure, private :: points_series_output_set_ids_i4, points_series_output_set_ids_i8
    generic, public :: set_ids => points_series_output_set_ids_i4, points_series_output_set_ids_i8
    procedure, public :: meta => points_series_output_meta
    procedure, public :: close => points_series_output_close
  end type points_series_output_dataset

  !> \class points_input_variable
  !> \brief NetCDF input variable handle for point-set data.
  type, extends(var) :: points_input_variable
    type(NcVariable) :: nc                         !< NetCDF variable handle
    type(points_t), pointer :: points => null()  !< point-set geometry
    integer(i4) :: rank = 0_i4                     !< NetCDF variable rank
    logical :: time_series = .false.               !< temporal variable uses station time-series layout
  contains
    procedure, public :: init => points_in_var_init
    procedure, private :: points_in_var_read_sp, points_in_var_read_dp, points_in_var_read_i1, points_in_var_read_i2,&
        & points_in_var_read_i4, points_in_var_read_i8
    generic, public :: read => points_in_var_read_sp, points_in_var_read_dp, points_in_var_read_i1, points_in_var_read_i2,&
        & points_in_var_read_i4, points_in_var_read_i8
    procedure, private :: points_in_var_read_chunk_sp, points_in_var_read_chunk_dp, points_in_var_read_chunk_i1,&
        & points_in_var_read_chunk_i2, points_in_var_read_chunk_i4, points_in_var_read_chunk_i8
    generic, public :: read_chunk => points_in_var_read_chunk_sp, points_in_var_read_chunk_dp, points_in_var_read_chunk_i1,&
        & points_in_var_read_chunk_i2, points_in_var_read_chunk_i4, points_in_var_read_chunk_i8
    procedure, private :: points_in_var_read_series_sp, points_in_var_read_series_dp, points_in_var_read_series_i1,&
        & points_in_var_read_series_i2, points_in_var_read_series_i4, points_in_var_read_series_i8
    generic, public :: read_series => points_in_var_read_series_sp, points_in_var_read_series_dp, points_in_var_read_series_i1,&
        & points_in_var_read_series_i2, points_in_var_read_series_i4, points_in_var_read_series_i8
  end type points_input_variable

  !> \class points_input_dataset
  !> \brief NetCDF reader for static or temporal variables on a point set.
  !! \par Examples
  !! - \ref 01_points_io.f90 : \copybrief 01_points_io.f90
  !! - \ref 02_points_nearest.f90 : \copybrief 02_points_nearest.f90
  !! - \ref 03_points_timeseries_io.f90 : \copybrief 03_points_timeseries_io.f90
  type :: points_input_dataset
    type(points_t), pointer :: points => null()         !< point-set geometry
    character(:), allocatable :: path                   !< input path
    type(NcDataset) :: nc                               !< NetCDF dataset
    type(points_input_variable), allocatable :: vars(:) !< input variable handles
    integer(i4) :: nvars = 0_i4                         !< number of variables
    logical :: static = .true.                          !< all variables are static
    type(datetime) :: start_time                        !< start of time frame in the file
    type(datetime) :: ref_time                          !< time-unit reference time
    type(timedelta) :: delta                            !< time-unit delta
    integer(i8) :: delta_sec                            !< time-unit delta in seconds
    integer(i8) :: ref_ord_sec                          !< reference time in ordinal seconds
    integer(i4) :: timestep = no_time                   !< detected file time step
    integer(i4), allocatable :: t_values(:)             !< time-axis values
    integer(i4), allocatable :: t_bounds(:)             !< time-axis bounds
    character(:), allocatable :: time_units             !< CF time units string
    type(datetime), allocatable :: times(:)             !< timestamps for time-span ends
    character(:), allocatable :: point_dim_name         !< inferred point dimension name
  contains
    procedure, public :: init => points_input_init
    procedure, private :: points_input_read_sp, points_input_read_dp, points_input_read_i1, points_input_read_i2,&
        & points_input_read_i4, points_input_read_i8
    generic, public :: read => points_input_read_sp, points_input_read_dp, points_input_read_i1, points_input_read_i2,&
        & points_input_read_i4, points_input_read_i8
    procedure, private :: points_input_read_chunk_sp, points_input_read_chunk_dp, points_input_read_chunk_i1,&
        & points_input_read_chunk_i2, points_input_read_chunk_i4, points_input_read_chunk_i8
    generic, public :: read_chunk => points_input_read_chunk_sp, points_input_read_chunk_dp, points_input_read_chunk_i1,&
        & points_input_read_chunk_i2, points_input_read_chunk_i4, points_input_read_chunk_i8
    procedure, private :: points_input_read_series_sp, points_input_read_series_dp, points_input_read_series_i1,&
        & points_input_read_series_i2, points_input_read_series_i4, points_input_read_series_i8
    generic, public :: read_series => points_input_read_series_sp, points_input_read_series_dp, points_input_read_series_i1,&
        & points_input_read_series_i2, points_input_read_series_i4, points_input_read_series_i8
    procedure, public :: time_index => points_input_time_index
    procedure, public :: time_axis => points_input_time_axis
    procedure, public :: chunk_times => points_input_chunk_times
    procedure, public :: meta => points_input_meta
    procedure, private :: points_input_get_ids_i4, points_input_get_ids_i8
    generic, public :: get_ids => points_input_get_ids_i4, points_input_get_ids_i8
    procedure, public :: close => points_input_close
  end type points_input_dataset

contains

  !> \brief Copy variable metadata into a concrete point IO variable.
  subroutine points_copy_var_meta(dst, src)
    class(var), intent(inout) :: dst !< destination metadata
    type(var), intent(in) :: src !< source metadata
    if (allocated(dst%name)) deallocate(dst%name)
    if (allocated(dst%long_name)) deallocate(dst%long_name)
    if (allocated(dst%standard_name)) deallocate(dst%standard_name)
    if (allocated(dst%units)) deallocate(dst%units)
    if (allocated(dst%dtype)) deallocate(dst%dtype)
    if (allocated(dst%kind)) deallocate(dst%kind)
    if (allocated(src%name)) dst%name = src%name
    if (allocated(src%long_name)) dst%long_name = src%long_name
    if (allocated(src%standard_name)) dst%standard_name = src%standard_name
    if (allocated(src%units)) dst%units = src%units
    if (allocated(src%dtype)) dst%dtype = src%dtype
    if (allocated(src%kind)) dst%kind = src%kind
    dst%static = src%static
    dst%allow_static = src%allow_static
    dst%avg = src%avg
    dst%layered = src%layered
  end subroutine points_copy_var_meta

  !> \brief Initialize a point output variable and create the NetCDF variable.
  subroutine points_out_var_init(self, meta, nc, points, points_dim, time_dim, deflate_level, time_series)
    class(points_output_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable metadata
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    type(NcDimension), intent(in) :: points_dim !< point dimension
    type(NcDimension), optional, intent(in) :: time_dim !< unlimited time dimension
    integer(i4), intent(in) :: deflate_level !< NetCDF deflate level
    logical, optional, intent(in) :: time_series !< use station time-series layout

    call points_copy_var_meta(self, meta)
    self%time_series = optval(time_series, .false.)
    self%dtype = "f64"
    if (allocated(meta%dtype)) self%dtype = trim(meta%dtype)
    self%points => points
    if (.not.associated(self%points)) call error_message("points_output_variable: points pointer not associated")
    if (self%static) then
      self%nc = nc%setVariable(self%name, self%dtype, [points_dim], deflate_level=deflate_level, shuffle=.true.)
    else
      if (.not.present(time_dim)) call error_message("points_output_variable: temporal variable needs time dimension: ", self%name)
      if (self%time_series) then
        self%nc = nc%setVariable(self%name, self%dtype, [time_dim, points_dim], deflate_level=deflate_level, shuffle=.true.)
      else
        self%nc = nc%setVariable(self%name, self%dtype, [points_dim, time_dim], deflate_level=deflate_level, shuffle=.true.)
      end if
    end if
    if (allocated(self%long_name)) call self%nc%setAttribute("long_name", self%long_name)
    if (allocated(self%standard_name)) call self%nc%setAttribute("standard_name", self%standard_name)
    if (allocated(self%units)) call self%nc%setAttribute("units", self%units)
    if (self%points%coordsys == spherical) then
      call self%nc%setAttribute("coordinates", "lon lat")
    else
      call self%nc%setAttribute("coordinates", "x y")
    end if
    call points_io_dtype_defaults(self%name, self%dtype, self%kind, self%nc)
    if (allocated(meta%kind)) self%kind = meta%kind
    call allocate_buffer(self)
  contains
    !> \brief Allocate the kind-specific accumulation buffer.
    subroutine allocate_buffer(self)
      class(points_output_variable), intent(inout) :: self
      select case(self%kind)
        case("sp")
          allocate(self%data_sp(self%points%n_points), source=0.0_sp)
        case("dp")
          allocate(self%data_dp(self%points%n_points), source=0.0_dp)
        case("i1")
          allocate(self%data_i1(self%points%n_points), source=0_i1)
        case("i2")
          allocate(self%data_i2(self%points%n_points), source=0_i2)
        case("i4")
          allocate(self%data_i4(self%points%n_points), source=0_i4)
        case("i8")
          allocate(self%data_i8(self%points%n_points), source=0_i8)
        case default
          call error_message("points_output_variable: unsupported kind: ", self%name, ": ", self%kind)
      end select
    end subroutine allocate_buffer
  end subroutine points_out_var_init

  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_sp(self, data)
    class(points_output_variable), intent(inout) :: self
    real(sp), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_sp)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_sp = self%data_sp + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_sp
  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_dp(self, data)
    class(points_output_variable), intent(inout) :: self
    real(dp), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_dp)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_dp = self%data_dp + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_dp
  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_i1(self, data)
    class(points_output_variable), intent(inout) :: self
    integer(i1), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_i1)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_i1 = self%data_i1 + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_i1
  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_i2(self, data)
    class(points_output_variable), intent(inout) :: self
    integer(i2), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_i2)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_i2 = self%data_i2 + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_i2
  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_i4(self, data)
    class(points_output_variable), intent(inout) :: self
    integer(i4), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_i4)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_i4 = self%data_i4 + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_i4
  !> \brief Accumulate one point vector for later writing.
  subroutine points_out_var_update_i8(self, data)
    class(points_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: data(:) !< point data for the current accumulation step
    if (.not.allocated(self%data_i8)) call error_message("points_output_variable: wrong kind for update: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_output_variable: data size mismatch: ", self%name)
    self%data_i8 = self%data_i8 + data
    self%counter = self%counter + 1_i4
  end subroutine points_out_var_update_i8

  !> \brief Write an accumulated point variable to disk.
  subroutine points_out_var_write(self, t_index)
    class(points_output_variable), intent(inout) :: self
    integer(i4), optional, intent(in) :: t_index !< time index for temporal variables

    if (self%static .and. self%static_written) return
    if (self%counter == 0_i4) call error_message("points_output_variable: no data was added before writing: ", self%name)
    select case(self%kind)
      case("sp")
        if (self%avg) self%data_sp = self%data_sp / real(self%counter, sp)
        if (self%static) then
          call self%nc%setData(self%data_sp)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_sp, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_sp, [1_i4, t_index])
          end if
        end if
        self%data_sp = 0.0_sp
      case("dp")
        if (self%avg) self%data_dp = self%data_dp / real(self%counter, dp)
        if (self%static) then
          call self%nc%setData(self%data_dp)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_dp, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_dp, [1_i4, t_index])
          end if
        end if
        self%data_dp = 0.0_dp
      case("i1")
        if (self%avg) self%data_i1 = self%data_i1 / int(self%counter, i1)
        if (self%static) then
          call self%nc%setData(self%data_i1)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_i1, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_i1, [1_i4, t_index])
          end if
        end if
        self%data_i1 = 0_i1
      case("i2")
        if (self%avg) self%data_i2 = self%data_i2 / int(self%counter, i2)
        if (self%static) then
          call self%nc%setData(self%data_i2)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_i2, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_i2, [1_i4, t_index])
          end if
        end if
        self%data_i2 = 0_i2
      case("i4")
        if (self%avg) self%data_i4 = self%data_i4 / int(self%counter, i4)
        if (self%static) then
          call self%nc%setData(self%data_i4)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_i4, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_i4, [1_i4, t_index])
          end if
        end if
        self%data_i4 = 0_i4
      case("i8")
        if (self%avg) self%data_i8 = self%data_i8 / int(self%counter, i8)
        if (self%static) then
          call self%nc%setData(self%data_i8)
        else
          if (.not.present(t_index)) call error_message("points_output_variable: missing time index: ", self%name)
          if (self%time_series) then
            call self%nc%setData(reshape(self%data_i8, [1_i4, int(self%points%n_points, i4)]), &
                                 [t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
          else
            call self%nc%setData(self%data_i8, [1_i4, t_index])
          end if
        end if
        self%data_i8 = 0_i8
      case default
        call error_message("points_output_variable: unsupported kind while writing: ", self%name, ": ", self%kind)
    end select
    if (self%static) self%static_written = .true.
    self%counter = 0_i4
  end subroutine points_out_var_write

  !> \brief Initialize a point-set output dataset.
  subroutine points_output_init(self, path, points, vars, start_time, ref_time, delta, timestamp, deflate_level, &
                               point_dim_name, points_double_precision, time_series)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< output file path
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    type(var), intent(in) :: vars(:) !< output variables
    type(datetime), optional, intent(in) :: start_time !< start of temporal write interval
    type(datetime), optional, intent(in) :: ref_time !< reference time for time units
    character(*), optional, intent(in) :: delta !< time units, for example "hours" or "days"
    integer(i4), optional, intent(in) :: timestamp !< timestamp location selector
    integer(i4), optional, intent(in) :: deflate_level !< NetCDF deflate level
    character(*), optional, intent(in) :: point_dim_name !< point dimension name
    logical, optional, intent(in) :: points_double_precision !< write point coordinates as double precision
    logical, optional, intent(in) :: time_series !< use station time-series layout for temporal variables

    character(:), allocatable :: dimname, units, units_dt
    type(NcDimension) :: points_dim, time_dim, bnds_dim
    type(NcVariable) :: t_var
    integer(i4) :: i

    self%path = trim(path)
    self%nc = NcDataset(self%path, "w")
    call self%nc%setFill(NF90_NOFILL)
    self%points => points
    self%counter = 0_i4
    self%deflate_level = optval(deflate_level, 6_i4)
    self%timestamp = optval(timestamp, end_timestamp)
    self%time_series = optval(time_series, .false.)
    dimname = optval(point_dim_name, "point")

    call self%points%to_netcdf(self%nc, point_dim_name=dimname, double_precision=points_double_precision)
    points_dim = self%nc%getDimension(dimname)

    self%nvars = size(vars)
    if (self%nvars == 0_i4) call error_message("points_output_dataset: no variables selected")
    self%static = .true.
    do i = 1_i4, self%nvars
      self%static = self%static .and. vars(i)%static
    end do
    if (.not.self%static) then
      if (.not.present(start_time)) call error_message("points_output_dataset: temporal output needs start_time")
      units_dt = trim(optval(delta, "hours"))
      self%previous_time = start_time
      self%start_time = start_time
      self%ref_time = start_time
      if (present(ref_time)) self%ref_time = ref_time
      self%delta = delta_from_string(units_dt)
      units = units_dt // " since " // self%ref_time%str()
      time_dim = self%nc%setDimension("time", 0_i4)
      bnds_dim = self%nc%setDimension("bnds", 2_i4)
      t_var = self%nc%setVariable("time", "i32", [time_dim])
      call t_var%setAttribute("long_name", "time")
      call t_var%setAttribute("standard_name", "time")
      call t_var%setAttribute("axis", "T")
      call t_var%setAttribute("units", units)
      call t_var%setAttribute("bounds", "time_bnds")
      t_var = self%nc%setVariable("time_bnds", "i32", [bnds_dim, time_dim])
    end if

    allocate(self%vars(self%nvars))
    do i = 1_i4, self%nvars
      if (vars(i)%static) then
        call self%vars(i)%init(vars(i), self%nc, self%points, points_dim, deflate_level=self%deflate_level)
      else
        call self%vars(i)%init(vars(i), self%nc, self%points, points_dim, time_dim, self%deflate_level, self%time_series)
      end if
    end do
  end subroutine points_output_init

  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_sp(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(sp), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_sp
  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_dp(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(dp), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_dp
  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_i1(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i1), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_i1
  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_i2(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i2), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_i2
  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_i4(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_i4
  !> \brief Accumulate one point vector for an output variable selected by name.
  subroutine points_output_update_i8(self, name, data)
    class(points_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: data(:) !< point data for the current accumulation step
    call self%vars(points_output_var_index(self, name))%update(data)
  end subroutine points_output_update_i8

  !> \brief Write the next temporal point-set output step.
  subroutine points_output_write(self, current_time)
    class(points_output_dataset), intent(inout) :: self
    type(datetime), optional, intent(in) :: current_time !< end time of the written interval
    type(NcVariable) :: t_var
    integer(i4) :: i, t_start, t_end, t_stamp

    self%counter = self%counter + 1_i4
    if (.not.self%static) then
      if (.not.present(current_time)) call error_message("points_output_dataset: missing current_time")
      call time_values(self%ref_time, self%previous_time, current_time, self%delta, self%timestamp, t_start, t_end, t_stamp)
      t_var = self%nc%getVariable("time")
      call t_var%setData(t_stamp, [self%counter])
      t_var = self%nc%getVariable("time_bnds")
      call t_var%setData(t_start, [1_i4, self%counter])
      call t_var%setData(t_end, [2_i4, self%counter])
      self%previous_time = current_time
    end if
    do i = 1_i4, self%nvars
      if ((.not.self%vars(i)%static) .or. self%vars(i)%counter > 0_i4) call self%vars(i)%write(self%counter)
    end do
  end subroutine points_output_write

  !> \brief Write all accumulated static point-set variables.
  subroutine points_output_write_static(self)
    class(points_output_dataset), intent(inout) :: self
    integer(i4) :: i
    do i = 1_i4, self%nvars
      if (self%vars(i)%static) call self%vars(i)%write()
    end do
  end subroutine points_output_write_static

  !> \brief Close a point-set output dataset.
  subroutine points_output_close(self)
    class(points_output_dataset), intent(inout) :: self
    integer(i4) :: i
    do i = 1_i4, self%nvars
      if (self%vars(i)%counter > 0_i4) call warn_message("points_output%close: unwritten buffered data: ", self%vars(i)%name)
    end do
    call self%nc%close()
    if (allocated(self%vars)) deallocate(self%vars)
  end subroutine points_output_close

  !> \brief Get effective point output variable metadata.
  type(var) function points_output_meta(self, name)
    class(points_output_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    points_output_meta = self%vars(points_output_var_index(self, name))%meta()
  end function points_output_meta

  !> \brief Initialize a point time-series output variable.
  subroutine points_series_out_var_init(self, meta, nc, points, points_dim, time_dim, deflate_level)
    class(points_series_output_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable metadata
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    type(NcDimension), intent(in) :: points_dim !< point dimension
    type(NcDimension), intent(in) :: time_dim !< fixed time dimension
    integer(i4), intent(in) :: deflate_level !< NetCDF deflate level

    call points_copy_var_meta(self, meta)
    self%dtype = "f64"
    if (allocated(meta%dtype)) self%dtype = trim(meta%dtype)
    self%points => points
    if (.not.associated(self%points)) call error_message("points_series_output_variable: points pointer not associated")
    if (self%static) then
      self%nc = nc%setVariable(self%name, self%dtype, [points_dim], deflate_level=deflate_level, shuffle=.true.)
    else
      self%nc = nc%setVariable(self%name, self%dtype, [time_dim, points_dim], deflate_level=deflate_level, shuffle=.true.)
    end if
    if (allocated(self%long_name)) call self%nc%setAttribute("long_name", self%long_name)
    if (allocated(self%standard_name)) call self%nc%setAttribute("standard_name", self%standard_name)
    if (allocated(self%units)) call self%nc%setAttribute("units", self%units)
    if (self%points%coordsys == spherical) then
      call self%nc%setAttribute("coordinates", "lon lat")
    else
      call self%nc%setAttribute("coordinates", "x y")
    end if
    call points_io_dtype_defaults(self%name, self%dtype, self%kind, self%nc)
    if (allocated(meta%kind)) self%kind = meta%kind
    if (.not.self%static) allocate(self%written(self%points%n_points), source=.false.)
  end subroutine points_series_out_var_init

  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_sp(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    real(sp), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "sp") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_sp

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_sp(self, data)
    class(points_series_output_variable), intent(inout) :: self
    real(sp), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "sp") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_sp
  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_dp(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    real(dp), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "dp") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_dp

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_dp(self, data)
    class(points_series_output_variable), intent(inout) :: self
    real(dp), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "dp") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_dp
  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_i1(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i1), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "i1") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_i1

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_i1(self, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i1), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "i1") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_i1
  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_i2(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i2), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "i2") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_i2

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_i2(self, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i2), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "i2") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_i2
  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_i4(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i4), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "i4") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_i4

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_i4(self, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i4), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "i4") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_i4
  !> \brief Write one complete point time series.
  subroutine points_series_out_var_write_i8(self, point_index, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i8), intent(in) :: data(:) !< complete point time series
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: point_index_
    integer(i8) :: n_times
    if (self%static) call error_message("points_series_output_variable: write_series needs temporal variable: ", self%name)
    if (self%kind /= "i8") call error_message("points_series_output_variable: wrong kind for write_series: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_series_output_variable: point index out of range: ", self%name)
    dims = self%nc%getDimensions()
    n_times = dims(1_i4)%getLength64()
    if (size(data, kind=i8) /= n_times) &
      call error_message("points_series_output_variable: time series size mismatch: ", self%name)
    point_index_ = int(point_index, i4)
    call self%nc%setData(data, start=[1_i4, point_index_], cnt=[int(n_times, i4), 1_i4])
    self%written(point_index_) = .true.
  end subroutine points_series_out_var_write_i8

  !> \brief Write one static point variable.
  subroutine points_series_out_var_write_static_i8(self, data)
    class(points_series_output_variable), intent(inout) :: self
    integer(i8), intent(in) :: data(:) !< static point data
    if (.not.self%static) call error_message("points_series_output_variable: write_static needs static variable: ", self%name)
    if (self%kind /= "i8") call error_message("points_series_output_variable: wrong kind for write_static: ", self%name)
    if (size(data, kind=i8) /= self%points%n_points) &
      call error_message("points_series_output_variable: static data size mismatch: ", self%name)
    call self%nc%setData(data)
    self%static_written = .true.
  end subroutine points_series_out_var_write_static_i8

  !> \brief Initialize a point time-series output dataset.
  subroutine points_series_output_init(self, path, points, vars, time_axis, point_dim_name, &
                                       points_double_precision, deflate_level)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< output file path
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    type(var), intent(in) :: vars(:) !< output variables
    type(time_t), intent(in) :: time_axis !< output time axis
    character(*), optional, intent(in) :: point_dim_name !< point dimension name
    logical, optional, intent(in) :: points_double_precision !< write point coordinates as double precision
    integer(i4), optional, intent(in) :: deflate_level !< NetCDF deflate level

    character(:), allocatable :: dimname
    character(:), allocatable :: time_units
    type(NcDimension) :: points_dim, time_dim, bnds_dim
    type(NcVariable) :: t_var, tb_var
    integer(i4), allocatable :: time_values(:), time_bnds(:, :)
    integer(i4) :: i
    logical :: has_time_bounds

    if (time_axis%n_times() < 1_i4) call error_message("points_series_output_dataset: empty time axis")
    call time_axis%to_cf(time_values, time_bnds, time_units)
    has_time_bounds = time_axis%has_bounds()
    self%path = trim(path)
    self%nc = NcDataset(self%path, "w")
    call self%nc%setFill(NF90_NOFILL)
    self%points => points
    call time_axis%copy_to(self%time_axis)
    self%deflate_level = optval(deflate_level, 6_i4)
    dimname = optval(point_dim_name, "point")
    self%point_dim_name = dimname

    call self%points%to_netcdf(self%nc, point_dim_name=dimname, double_precision=points_double_precision)
    points_dim = self%nc%getDimension(dimname)
    time_dim = self%nc%setDimension("time", size(time_values))
    t_var = self%nc%setVariable("time", "i32", [time_dim])
    call t_var%setAttribute("long_name", "time")
    call t_var%setAttribute("standard_name", "time")
    call t_var%setAttribute("axis", "T")
    call t_var%setAttribute("units", time_units)
    call t_var%setData(time_values)
    if (has_time_bounds) then
      bnds_dim = self%nc%setDimension("bnds", 2_i4)
      call t_var%setAttribute("bounds", "time_bnds")
      tb_var = self%nc%setVariable("time_bnds", "i32", [bnds_dim, time_dim])
      call tb_var%setData(time_bnds)
    end if

    self%nvars = size(vars)
    if (self%nvars == 0_i4) call error_message("points_series_output_dataset: no variables selected")
    allocate(self%vars(self%nvars))
    do i = 1_i4, self%nvars
      call self%vars(i)%init(vars(i), self%nc, self%points, points_dim, time_dim, self%deflate_level)
    end do
  end subroutine points_series_output_init

  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_sp(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    real(sp), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_sp

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_sp(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(sp), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_sp
  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_dp(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    real(dp), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_dp

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_dp(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(dp), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_dp
  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_i1(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i1), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_i1

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_i1(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i1), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_i1
  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_i2(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i2), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_i2

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_i2(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i2), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_i2
  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_i4(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i4), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_i4

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_i4(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_i4
  !> \brief Write one complete point time series for a variable selected by name.
  subroutine points_series_output_write_i8(self, name, point_index, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i8), intent(in) :: data(:) !< complete point time series
    call self%vars(points_series_output_var_index(self, name))%write(point_index, data)
  end subroutine points_series_output_write_i8

  !> \brief Write one static point variable for a variable selected by name.
  subroutine points_series_output_write_static_i8(self, name, data)
    class(points_series_output_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: data(:) !< static point data
    call self%vars(points_series_output_var_index(self, name))%write_static(data)
  end subroutine points_series_output_write_static_i8

  !> \brief Write point IDs using the point dimension name as variable name.
  subroutine points_series_output_set_ids_i4(self, ids, long_name)
    class(points_series_output_dataset), intent(inout) :: self
    integer(i4), intent(in) :: ids(:) !< point IDs
    character(*), optional, intent(in) :: long_name !< optional long name attribute

    type(NcDimension) :: points_dim
    type(NcVariable) :: id_var

    if (.not.associated(self%points)) call error_message("points_series_output%set_ids: points pointer not associated")
    if (.not.allocated(self%point_dim_name)) call error_message("points_series_output%set_ids: point dimension is unknown")
    if (size(ids, kind=i8) /= self%points%n_points) call error_message("points_series_output%set_ids: ID size mismatch")
    if (points_series_output_has_var(self, self%point_dim_name)) &
      call error_message("points_series_output%set_ids: ID name already declared as output variable: ", self%point_dim_name)
    if (self%nc%hasVariable(self%point_dim_name)) &
      call error_message("points_series_output%set_ids: ID variable already exists: ", self%point_dim_name)

    points_dim = self%nc%getDimension(self%point_dim_name)
    id_var = self%nc%setVariable(self%point_dim_name, "i32", [points_dim], &
                                 deflate_level=self%deflate_level, shuffle=.true.)
    if (present(long_name)) call id_var%setAttribute("long_name", long_name)
    if (self%points%coordsys == spherical) then
      call id_var%setAttribute("coordinates", "lon lat")
    else
      call id_var%setAttribute("coordinates", "x y")
    end if
    call id_var%setData(ids)
  end subroutine points_series_output_set_ids_i4

  !> \brief Write point IDs using the point dimension name as variable name.
  subroutine points_series_output_set_ids_i8(self, ids, long_name)
    class(points_series_output_dataset), intent(inout) :: self
    integer(i8), intent(in) :: ids(:) !< point IDs
    character(*), optional, intent(in) :: long_name !< optional long name attribute

    type(NcDimension) :: points_dim
    type(NcVariable) :: id_var

    if (.not.associated(self%points)) call error_message("points_series_output%set_ids: points pointer not associated")
    if (.not.allocated(self%point_dim_name)) call error_message("points_series_output%set_ids: point dimension is unknown")
    if (size(ids, kind=i8) /= self%points%n_points) call error_message("points_series_output%set_ids: ID size mismatch")
    if (points_series_output_has_var(self, self%point_dim_name)) &
      call error_message("points_series_output%set_ids: ID name already declared as output variable: ", self%point_dim_name)
    if (self%nc%hasVariable(self%point_dim_name)) &
      call error_message("points_series_output%set_ids: ID variable already exists: ", self%point_dim_name)

    points_dim = self%nc%getDimension(self%point_dim_name)
    id_var = self%nc%setVariable(self%point_dim_name, "i64", [points_dim], &
                                 deflate_level=self%deflate_level, shuffle=.true.)
    if (present(long_name)) call id_var%setAttribute("long_name", long_name)
    if (self%points%coordsys == spherical) then
      call id_var%setAttribute("coordinates", "lon lat")
    else
      call id_var%setAttribute("coordinates", "x y")
    end if
    call id_var%setData(ids)
  end subroutine points_series_output_set_ids_i8

  !> \brief Close a point time-series output dataset.
  subroutine points_series_output_close(self)
    class(points_series_output_dataset), intent(inout) :: self
    integer(i4) :: i
    do i = 1_i4, self%nvars
      if (self%vars(i)%static) then
        if (.not.self%vars(i)%static_written) &
          call warn_message("points_series_output%close: unwritten static variable: ", self%vars(i)%name)
      else if (allocated(self%vars(i)%written)) then
        if (.not.all(self%vars(i)%written)) &
          call warn_message("points_series_output%close: unwritten point series: ", self%vars(i)%name)
      end if
    end do
    call self%nc%close()
    if (allocated(self%vars)) deallocate(self%vars)
  end subroutine points_series_output_close

  !> \brief Get effective point-series output variable metadata.
  type(var) function points_series_output_meta(self, name)
    class(points_series_output_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    points_series_output_meta = self%vars(points_series_output_var_index(self, name))%meta()
  end function points_series_output_meta

  !> \brief Initialize a point-set input variable.
  subroutine points_in_var_init(self, meta, nc, points)
    class(points_input_variable), intent(inout) :: self
    type(var), intent(in) :: meta !< variable metadata
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    type(NcDimension), allocatable :: dims(:)
    type(NcVariable) :: t_var
    character(len=256) :: tmp_str
    logical :: first_is_point, second_is_point, first_is_time, second_is_time

    call points_copy_var_meta(self, meta)
    self%nc = nc%getVariable(self%name)
    self%points => points
    dims = self%nc%getDimensions()
    self%rank = size(dims)
    if (self%rank < 1_i4 .or. self%rank > 2_i4) call error_message("points_input_variable: rank mismatch: ", self%name)
    self%static = self%rank == 1_i4
    first_is_point = dims(1)%getLength64() == points%n_points
    if (self%static) then
      if (.not.first_is_point) call error_message("points_input_variable: point dimension mismatch: ", self%name)
      self%time_series = .false.
    else
      if (.not.nc%hasVariable(trim(dims(1)%getName()))) then
        first_is_time = .false.
      else
        t_var = nc%getVariable(trim(dims(1)%getName()))
        first_is_time = is_t_axis(t_var)
      end if
      if (.not.nc%hasVariable(trim(dims(2)%getName()))) then
        second_is_time = .false.
      else
        t_var = nc%getVariable(trim(dims(2)%getName()))
        second_is_time = is_t_axis(t_var)
      end if
      second_is_point = dims(2)%getLength64() == points%n_points
      if (first_is_point .and. second_is_time) then
        self%time_series = .false.
      else if (first_is_time .and. second_is_point) then
        self%time_series = .true.
      else
        call error_message("points_input_variable: could not identify point/time dimensions: ", self%name)
      end if
    end if
    if (meta%static .and. .not.self%static) call error_message("points_input_variable: expected static variable: ", self%name)
    if ((.not.meta%static) .and. self%static .and. .not.meta%allow_static) &
      call error_message("points_input_variable: expected temporal variable: ", self%name)
    self%dtype = trim(self%nc%getDtype())
    call points_io_dtype_defaults(self%name, self%dtype, self%kind)
    if (allocated(meta%kind)) self%kind = meta%kind
    if (self%nc%hasAttribute("standard_name")) then
      call self%nc%getAttribute("standard_name", tmp_str)
      self%standard_name = trim(tmp_str)
      if (allocated(meta%standard_name)) then
        if (meta%standard_name /= self%standard_name) &
          call error_message("points_input_variable: standard_name mismatch: ", self%name, ", ", meta%standard_name, "=/=", self%standard_name)
      end if
    end if
    if (self%nc%hasAttribute("units")) then
      call self%nc%getAttribute("units", tmp_str)
      self%units = trim(tmp_str)
      if (allocated(meta%units)) then
        if (meta%units /= self%units) &
          call error_message("points_input_variable: units mismatch: ", self%name, ", ", meta%units, "=/=", self%units)
      end if
    end if
    if (self%nc%hasAttribute("long_name")) then
      call self%nc%getAttribute("long_name", tmp_str)
      self%long_name = trim(tmp_str)
    end if
    self%allow_static = .false.
  end subroutine points_in_var_init

  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_sp(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    real(sp), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    real(sp), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_sp

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_sp(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    real(sp), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    real(sp), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_sp

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_sp(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    real(sp), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_sp
  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_dp(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    real(dp), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    real(dp), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_dp

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_dp(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    real(dp), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    real(dp), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_dp

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_dp(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    real(dp), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_dp
  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_i1(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    integer(i1), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    integer(i1), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_i1

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_i1(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i1), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i1), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_i1

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_i1(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i1), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_i1
  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_i2(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    integer(i2), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    integer(i2), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_i2

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_i2(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i2), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i2), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_i2

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_i2(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i2), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_i2
  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_i4(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    integer(i4), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    integer(i4), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_i4

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_i4(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i4), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_i4

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_i4(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i4), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_i4
  !> \brief Read one static or temporal point vector.
  subroutine points_in_var_read_i8(self, data, t_index)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(out) :: data(:) !< read point data
    integer(i4), optional, intent(in) :: t_index !< temporal index for non-static variables
    integer(i8), allocatable :: tmp(:, :)
    if (size(data, kind=i8) /= self%points%n_points) call error_message("points_input_variable: data size mismatch: ", self%name)
    if (self%static) then
      call self%nc%readInto(data)
    else
      if (.not.present(t_index)) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (t_index == 0_i4) call error_message("points_input_variable: temporal variable needs a time: ", self%name)
      if (self%time_series) then
        call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[1_i4, int(self%points%n_points, i4)])
        data = tmp(1, :)
      else
        call self%nc%readInto(data, [1_i4, t_index], [int(self%points%n_points, i4), 1_i4])
      end if
    end if
  end subroutine points_in_var_read_i8

  !> \brief Read a temporal point/time chunk.
  subroutine points_in_var_read_chunk_i8(self, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i8), allocatable :: tmp(:, :)
    if (self%static) call error_message("points_input_variable: chunk read needs temporal variable: ", self%name)
    if (self%time_series) then
      call self%nc%getData(tmp, start=[t_index, 1_i4], cnt=[t_size, int(self%points%n_points, i4)])
      allocate(data(self%points%n_points, t_size), source=transpose(tmp))
    else
      call self%nc%getData(data, start=[1_i4, t_index], cnt=[int(self%points%n_points, i4), t_size])
    end if
  end subroutine points_in_var_read_chunk_i8

  !> \brief Read one point time series.
  subroutine points_in_var_read_series_i8(self, point_index, data, t_index, t_size)
    class(points_input_variable), intent(inout) :: self
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i8), allocatable, intent(out) :: data(:) !< read point time series
    integer(i4), intent(in) :: t_index !< first temporal index
    integer(i4), intent(in) :: t_size !< number of temporal indices
    integer(i4) :: point_index_
    if (self%static) call error_message("points_input_variable: time series read needs temporal variable: ", self%name)
    if (point_index < 1_i8 .or. point_index > self%points%n_points) &
      call error_message("points_input_variable: point index out of range: ", self%name)
    point_index_ = int(point_index, i4)
    if (self%time_series) then
      call self%nc%getData(data, start=[t_index, point_index_], cnt=[t_size, 1_i4])
    else
      call self%nc%getData(data, start=[point_index_, t_index], cnt=[1_i4, t_size])
    end if
  end subroutine points_in_var_read_series_i8

  !> \brief Initialize a point-set input dataset.
  subroutine points_input_init(self, path, vars, points, timestamp, points_init_var)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: path !< input file path
    type(var), intent(in) :: vars(:) !< selected input variables
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    integer(i4), optional, intent(in) :: timestamp !< timestamp selector used if file time has no bounds
    character(*), optional, intent(in) :: points_init_var !< variable used to infer point coordinates

    type(NcDimension), allocatable :: dims(:)
    type(NcVariable) :: t_var
    integer(i4) :: i

    self%path = trim(path)
    self%nc = NcDataset(self%path, "r")
    self%points => points
    if (present(points_init_var)) call self%points%from_netcdf(self%nc, var=points_init_var)
    self%point_dim_name = points_input_infer_point_dim_name(self%nc, self%points, points_init_var)
    self%nvars = size(vars)
    allocate(self%vars(self%nvars))
    self%static = .true.
    self%timestep = no_time
    do i = 1_i4, self%nvars
      call self%vars(i)%init(vars(i), self%nc, self%points)
      if (.not.self%vars(i)%static .and. self%static) then
        self%static = .false.
        dims = self%vars(i)%nc%getDimensions()
        if (self%vars(i)%time_series) then
          t_var = self%nc%getVariable(trim(dims(1)%getName()))
        else
          t_var = self%nc%getVariable(trim(dims(size(dims))%getName()))
        end if
        call time_stepping(t_var, self%ref_time, self%delta, self%timestep, self%t_values, self%t_bounds, timestamp)
        call read_units(t_var, self%time_units)
        self%start_time = self%ref_time + self%t_bounds(1) * self%delta
        self%times = [(self%ref_time + self%t_values(i) * self%delta, i=1_i4,size(self%t_values))]
        self%delta_sec = self%delta%total_seconds()
        self%ref_ord_sec = int(self%ref_time%date_to_ordinal(), i8) * 86400_i8 + int(self%ref_time%day_second(), i8)
      end if
    end do
    if (allocated(dims)) deallocate(dims)
  end subroutine points_input_init

  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_sp(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(sp), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_sp

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_sp(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(sp), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_sp

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_sp(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    real(sp), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_sp
  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_dp(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(dp), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_dp

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_dp(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    real(dp), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_dp

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_dp(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    real(dp), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_dp
  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_i1(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i1), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_i1

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_i1(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i1), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_i1

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_i1(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i1), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_i1
  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_i2(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i2), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_i2

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_i2(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i2), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_i2

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_i2(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i2), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_i2
  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_i4(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_i4

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_i4(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_i4

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_i4(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i4), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_i4
  !> \brief Read one point vector from an input dataset.
  subroutine points_input_read_i8(self, name, data, current_time, t_index)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(out) :: data(:) !< read point data
    type(datetime), optional, intent(in) :: current_time !< time covered by the requested time interval
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    integer(i4) :: var_id, tidx
    var_id = points_input_var_index(self, name)
    tidx = points_input_resolve_time_index(self, var_id, current_time, t_index)
    call self%vars(var_id)%read(data, t_index=tidx)
  end subroutine points_input_read_i8

  !> \brief Read a point/time chunk selected by an inclusive time frame end.
  subroutine points_input_read_chunk_i8(self, name, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), allocatable, intent(out) :: data(:,:) !< read point data with shape (point,time)
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned chunk
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_chunk: need temporal variable: ", self%vars(var_id)%name)
    call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    call self%vars(var_id)%read_chunk(data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_chunk_i8

  !> \brief Read one point time series selected by an optional time frame.
  subroutine points_input_read_series_i8(self, name, point_index, data, timeframe_start, timeframe_end, times)
    class(points_input_dataset), intent(inout) :: self
    character(*), intent(in) :: name !< variable name
    integer(i8), intent(in) :: point_index !< one-based point index
    integer(i8), allocatable, intent(out) :: data(:) !< read point time series
    type(datetime), optional, intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), optional, intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps covered by the returned series
    integer(i4) :: t_index, t_size, var_id
    var_id = points_input_var_index(self, name)
    if (self%vars(var_id)%static) call error_message("points_input%read_series: need temporal variable: ", self%vars(var_id)%name)
    if (present(timeframe_start) .neqv. present(timeframe_end)) &
      call error_message("points_input%read_series: timeframe_start and timeframe_end must be given together")
    if (present(timeframe_start)) then
      call self%chunk_times(timeframe_start, timeframe_end, times, t_index, t_size)
    else
      t_index = 1_i4
      t_size = size(self%times)
      if (present(times)) allocate(times(t_size), source=self%times)
    end if
    call self%vars(var_id)%read_series(point_index, data, t_index=t_index, t_size=t_size)
  end subroutine points_input_read_series_i8

  !> \brief Read point IDs or return default one-based point indices.
  subroutine points_input_get_ids_i4(self, ids, id_name)
    class(points_input_dataset), intent(inout) :: self
    integer(i4), allocatable, intent(out) :: ids(:) !< point IDs
    character(*), optional, intent(in) :: id_name !< optional ID variable name
    type(NcVariable) :: id_var
    type(NcDimension), allocatable :: dims(:)
    character(:), allocatable :: id_var_name
    integer(i8) :: i
    if (.not.allocated(self%point_dim_name)) call error_message("points_input%get_ids: point dimension is unknown")
    id_var_name = self%point_dim_name
    if (present(id_name)) id_var_name = trim(id_name)
    if (.not.self%nc%hasVariable(id_var_name)) then
      if (present(id_name)) call error_message("points_input%get_ids: ID variable not present: ", id_var_name)
      allocate(ids(self%points%n_points))
      do i = 1_i8, self%points%n_points
        ids(i) = int(i, i4)
      end do
      return
    end if
    id_var = self%nc%getVariable(id_var_name)
    if (id_var%getRank() /= 1_i4) call error_message("points_input%get_ids: ID variable must be one-dimensional: ", id_var_name)
    dims = id_var%getDimensions()
    if (trim(dims(1)%getName()) /= self%point_dim_name) call error_message("points_input%get_ids: ID variable uses wrong&
        & dimension: ", id_var_name)
    if (dims(1)%getLength64() /= self%points%n_points) call error_message("points_input%get_ids: ID variable size mismatch: ",&
        & id_var_name)
    call id_var%getData(ids)
  end subroutine points_input_get_ids_i4

  !> \brief Read point IDs or return default one-based point indices.
  subroutine points_input_get_ids_i8(self, ids, id_name)
    class(points_input_dataset), intent(inout) :: self
    integer(i8), allocatable, intent(out) :: ids(:) !< point IDs
    character(*), optional, intent(in) :: id_name !< optional ID variable name
    type(NcVariable) :: id_var
    type(NcDimension), allocatable :: dims(:)
    character(:), allocatable :: id_var_name
    integer(i8) :: i
    if (.not.allocated(self%point_dim_name)) call error_message("points_input%get_ids: point dimension is unknown")
    id_var_name = self%point_dim_name
    if (present(id_name)) id_var_name = trim(id_name)
    if (.not.self%nc%hasVariable(id_var_name)) then
      if (present(id_name)) call error_message("points_input%get_ids: ID variable not present: ", id_var_name)
      allocate(ids(self%points%n_points))
      do i = 1_i8, self%points%n_points
        ids(i) = int(i, i8)
      end do
      return
    end if
    id_var = self%nc%getVariable(id_var_name)
    if (id_var%getRank() /= 1_i4) call error_message("points_input%get_ids: ID variable must be one-dimensional: ", id_var_name)
    dims = id_var%getDimensions()
    if (trim(dims(1)%getName()) /= self%point_dim_name) call error_message("points_input%get_ids: ID variable uses wrong&
        & dimension: ", id_var_name)
    if (dims(1)%getLength64() /= self%points%n_points) call error_message("points_input%get_ids: ID variable size mismatch: ",&
        & id_var_name)
    call id_var%getData(ids)
  end subroutine points_input_get_ids_i8

  !> \brief Get times and indices for a temporal chunk.
  subroutine points_input_chunk_times(self, timeframe_start, timeframe_end, times, t_index, t_size)
    class(points_input_dataset), intent(inout) :: self
    type(datetime), intent(in) :: timeframe_start !< start of time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of time frame, including
    type(datetime), allocatable, optional, intent(out) :: times(:) !< timestamps in chunk
    integer(i4), optional, intent(out) :: t_index !< first temporal index
    integer(i4), optional, intent(out) :: t_size !< chunk size
    integer(i4) :: t_start, t_end, t_cnt, t_id
    if (self%static) call error_message("points_input%chunk_times: file has no time: ", self%path)
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
    t_id = t_start + 1_i4
    if (present(t_index)) t_index = t_id
    if (present(t_size)) t_size = t_cnt
    if (present(times)) allocate(times(t_cnt), source=self%times(t_id:t_end))
  end subroutine points_input_chunk_times

  !> \brief Get the time index for the interval containing a selected time.
  integer(i4) function points_input_time_index(self, current_time)
    class(points_input_dataset), intent(in) :: self
    type(datetime), intent(in) :: current_time !< selected read time
    integer(i8) :: current_delta_sec
    integer(i4) :: t_val, left, right
    points_input_time_index = 0_i4
    if (.not.allocated(self%times)) call error_message("points_input%time_index: file is static and has no time dimension")
    current_delta_sec = int(current_time%date_to_ordinal(), i8) * 86400_i8 + int(current_time%day_second(), i8) - self%ref_ord_sec
    t_val = int(current_delta_sec / self%delta_sec, i4)
    if (mod(current_delta_sec, self%delta_sec) > 0_i8) t_val = t_val + 1_i4
    left = 1_i4
    right = size(self%t_values)
    if (t_val == self%t_bounds(left)) then
      points_input_time_index = 0_i4
      return
    else if ((t_val < self%t_bounds(left)) .or. (t_val > self%t_bounds(right+1_i4))) then
      call error_message("points_input%time_index: read time not covered by file: ", current_time%str(), ": ", self%path)
    end if
    do while (left <= right)
      points_input_time_index = (left + right) / 2_i4
      if (t_val <= self%t_bounds(points_input_time_index)) then
        right = points_input_time_index - 1_i4
      else if (t_val > self%t_bounds(points_input_time_index + 1_i4)) then
        left = points_input_time_index + 1_i4
      else
        return
      end if
    end do
  end function points_input_time_index

  !> \brief Return the parsed input time axis.
  type(time_t) function points_input_time_axis(self) result(axis)
    class(points_input_dataset), intent(in) :: self
    integer(i4), allocatable :: bounds(:, :)
    integer(i4) :: n_times

    if (self%static .or. .not.allocated(self%t_values)) call error_message("points_input%time_axis: input has no time axis")
    if (.not.allocated(self%time_units)) call error_message("points_input%time_axis: missing time units")
    n_times = size(self%t_values)
    allocate(bounds(2_i4, n_times))
    bounds(1_i4, :) = self%t_bounds(:n_times)
    bounds(2_i4, :) = self%t_bounds(2_i4:n_times + 1_i4)
    call axis%init_cf(self%t_values, self%time_units, bounds=bounds, timestamp=end_timestamp)
    axis%timestamp = end_timestamp
    axis%timestep = self%timestep
  end function points_input_time_axis

  !> \brief Get effective point input variable metadata.
  type(var) function points_input_meta(self, name)
    class(points_input_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    points_input_meta = self%vars(points_input_var_index(self, name))%meta()
  end function points_input_meta

  !> \brief Close a point-set input dataset.
  subroutine points_input_close(self)
    class(points_input_dataset), intent(inout) :: self
    call self%nc%close()
    if (allocated(self%vars)) deallocate(self%vars)
    if (allocated(self%times)) deallocate(self%times)
    if (allocated(self%t_values)) deallocate(self%t_values)
    if (allocated(self%t_bounds)) deallocate(self%t_bounds)
    if (allocated(self%time_units)) deallocate(self%time_units)
  end subroutine points_input_close

  !> \brief Infer the point dimension name from point coordinate variables.
  function points_input_infer_point_dim_name(nc, points, var) result(dimname)
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    character(*), optional, intent(in) :: var !< optional variable whose coordinates identify point dimension
    character(:), allocatable :: dimname
    type(NcVariable) :: coord_var, base_var
    type(NcDimension), allocatable :: dims(:), coord_dims(:)
    character(len=256) :: coords_attr
    character(len=256), allocatable :: coord_names(:)
    integer(i4) :: i

    if (present(var)) then
      base_var = nc%getVariable(trim(var))
      dims = base_var%getDimensions()
      if (base_var%hasAttribute("coordinates")) then
        call base_var%getAttribute("coordinates", coords_attr)
        coord_names = splitString(trim(coords_attr), " ")
        do i = 1_i4, size(coord_names)
          if (.not.nc%hasVariable(trim(coord_names(i)))) cycle
          coord_var = nc%getVariable(trim(coord_names(i)))
          if (is_x_axis(coord_var) .or. is_lon_coord(coord_var) .or. &
              trim(coord_names(i)) == "x" .or. trim(coord_names(i)) == "lon") then
            coord_dims = coord_var%getDimensions()
            if (size(coord_dims) /= 1_i4) call error_message("points_input: point coordinate variable must be one-dimensional")
            dimname = trim(coord_dims(1)%getName())
            call points_input_check_point_dim(nc, points, dimname, var)
            return
          end if
        end do
      end if
    end if

    if (nc%hasVariable("x")) then
      coord_var = nc%getVariable("x")
    else if (nc%hasVariable("lon")) then
      coord_var = nc%getVariable("lon")
    else if (nc%hasVariable("y")) then
      coord_var = nc%getVariable("y")
    else if (nc%hasVariable("lat")) then
      coord_var = nc%getVariable("lat")
    else
      call error_message("points_input: could not infer point dimension")
    end if
    coord_dims = coord_var%getDimensions()
    if (size(coord_dims) /= 1_i4) call error_message("points_input: point coordinate variable must be one-dimensional")
    dimname = trim(coord_dims(1)%getName())
    if (present(var)) call points_input_check_point_dim(nc, points, dimname, var)
  end function points_input_infer_point_dim_name

  !> \brief Check that a variable uses the inferred point dimension.
  subroutine points_input_check_point_dim(nc, points, dimname, var)
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    type(points_t), pointer, intent(in) :: points !< point-set geometry
    character(*), intent(in) :: dimname !< inferred point dimension name
    character(*), intent(in) :: var !< selected variable name
    type(NcVariable) :: base_var
    type(NcDimension) :: point_dim
    type(NcDimension), allocatable :: dims(:)
    logical :: found
    integer(i4) :: i

    base_var = nc%getVariable(trim(var))
    dims = base_var%getDimensions()
    found = .false.
    do i = 1_i4, size(dims)
      if (trim(dims(i)%getName()) == trim(dimname)) found = .true.
    end do
    if (.not.found) call error_message("points_input: selected variable does not use point coordinate dimension: ", var)
    point_dim = nc%getDimension(trim(dimname))
    if (point_dim%getLength64() /= points%n_points) call error_message("points_input: point dimension size mismatch: ", dimname)
  end subroutine points_input_check_point_dim

  !> \brief Generate integer CF time coordinate values and contiguous bounds.
  subroutine points_time_axis(timeframe_start, timeframe_end, values, bounds, units, timestep, delta, timestamp, ref_time)
    type(datetime), intent(in) :: timeframe_start !< start of first time frame
    type(datetime), intent(in) :: timeframe_end !< end of final time frame
    integer(i4), allocatable, intent(out) :: values(:) !< generated time coordinate values
    integer(i4), allocatable, intent(out) :: bounds(:, :) !< generated bounds with shape (2,time)
    character(:), allocatable, intent(out) :: units !< CF time units
    integer(i4), optional, intent(in) :: timestep !< time step indicator or positive hourly step
    character(*), optional, intent(in) :: delta !< time units, for example "hours" or "days"
    integer(i4), optional, intent(in) :: timestamp !< timestamp location selector
    type(datetime), optional, intent(in) :: ref_time !< reference time, defaults to timeframe_start

    character(:), allocatable :: units_dt
    type(datetime) :: ref_time_, previous_time, current_time
    type(timedelta) :: delta_
    integer(i4) :: timestep_, timestamp_, t_start, t_end, t_stamp, i, n_times

    if (timeframe_end <= timeframe_start) call error_message("points_time_axis: timeframe_end must be after timeframe_start")
    timestamp_ = optval(timestamp, end_timestamp)
    timestep_ = optval(timestep, daily)
    units_dt = trim(optval(delta, time_units_delta(timestep_, timestamp_)))
    delta_ = delta_from_string(units_dt)
    ref_time_ = timeframe_start
    if (present(ref_time)) ref_time_ = ref_time
    units = units_dt // " since " // ref_time_%str()

    n_times = 0_i4
    previous_time = timeframe_start
    do while(previous_time < timeframe_end)
      current_time = points_time_axis_next(previous_time, timestep_, delta, present(delta))
      if (current_time > timeframe_end) call error_message("points_time_axis: timestep does not align with timeframe_end")
      n_times = n_times + 1_i4
      previous_time = current_time
    end do
    allocate(values(n_times), bounds(2, n_times))
    previous_time = timeframe_start
    do i = 1_i4, n_times
      current_time = points_time_axis_next(previous_time, timestep_, delta, present(delta))
      call time_values(ref_time_, previous_time, current_time, delta_, timestamp_, t_start, t_end, t_stamp)
      bounds(:, i) = [t_start, t_end]
      values(i) = t_stamp
      previous_time = current_time
    end do
  end subroutine points_time_axis

  !> \brief Return the next contiguous time-axis edge.
  type(datetime) function points_time_axis_next(current_time, timestep, delta, use_delta) result(next_time)
    type(datetime), intent(in) :: current_time !< current time-axis edge
    integer(i4), intent(in) :: timestep !< time step indicator or positive hourly step
    character(*), optional, intent(in) :: delta !< explicit timedelta units
    logical, intent(in) :: use_delta !< use explicit delta instead of timestep

    if (use_delta) then
      if (.not.present(delta)) call error_message("points_time_axis_next: missing delta")
      next_time = current_time + delta_from_string(delta)
      return
    end if
    select case(timestep)
      case(yearly)
        next_time = current_time%next_new_year()
      case(monthly)
        next_time = current_time%next_new_month()
      case(daily)
        next_time = current_time%next_new_day()
      case(varying, no_time)
        call error_message("points_time_axis: invalid timestep for contiguous axis")
      case default
        if (timestep <= 0_i4) call error_message("points_time_axis: invalid timestep")
        next_time = current_time + timestep * delta_from_string("hours")
    end select
  end function points_time_axis_next

  !> \brief Return one-based index of an integer point ID.
  integer(i8) function points_id_index_i4(ids, id) result(idx)
    integer(i4), intent(in) :: ids(:) !< point IDs
    integer(i4), intent(in) :: id !< selected point ID
    integer(i8) :: i
    idx = 0_i8
    do i = 1_i8, size(ids, kind=i8)
      if (ids(i) == id) then
        idx = i
        return
      end if
    end do
    call error_message("points_id_index: ID not found")
  end function points_id_index_i4

  !> \brief Return one-based index of an integer point ID.
  integer(i8) function points_id_index_i8(ids, id) result(idx)
    integer(i8), intent(in) :: ids(:) !< point IDs
    integer(i8), intent(in) :: id !< selected point ID
    integer(i8) :: i
    idx = 0_i8
    do i = 1_i8, size(ids, kind=i8)
      if (ids(i) == id) then
        idx = i
        return
      end if
    end do
    call error_message("points_id_index: ID not found")
  end function points_id_index_i8

  !> \brief Return one-based indices of integer point IDs.
  function points_id_indices_i4(ids, selected_ids) result(indices)
    integer(i4), intent(in) :: ids(:) !< point IDs
    integer(i4), intent(in) :: selected_ids(:) !< selected point IDs
    integer(i8) :: indices(size(selected_ids))
    integer(i4) :: i
    do i = 1_i4, size(selected_ids)
      indices(i) = points_id_index_i4(ids, selected_ids(i))
    end do
  end function points_id_indices_i4

  !> \brief Return one-based indices of integer point IDs.
  function points_id_indices_i8(ids, selected_ids) result(indices)
    integer(i8), intent(in) :: ids(:) !< point IDs
    integer(i8), intent(in) :: selected_ids(:) !< selected point IDs
    integer(i8) :: indices(size(selected_ids))
    integer(i4) :: i
    do i = 1_i4, size(selected_ids)
      indices(i) = points_id_index_i8(ids, selected_ids(i))
    end do
  end function points_id_indices_i8

  !> \brief Map NetCDF dtype metadata to default Fortran kinds and missing values.
  subroutine points_io_dtype_defaults(name, dtype, kind, nc)
    character(*), intent(in) :: name !< variable name for diagnostics
    character(*), intent(in) :: dtype !< NetCDF dtype
    character(:), allocatable, intent(out) :: kind !< default Fortran kind
    type(NcVariable), optional, intent(inout) :: nc !< NetCDF variable that receives fill metadata
    select case(trim(dtype))
      case("f32")
        kind = "sp"
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
        kind = "i1"
        if (present(nc)) then
          call nc%setFillValue(nodata_i1)
          call nc%setAttribute("missing_value", nodata_i1)
        end if
      case("i16")
        kind = "i2"
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
        kind = "i8"
        if (present(nc)) then
          call nc%setFillValue(nodata_i8)
          call nc%setAttribute("missing_value", nodata_i8)
        end if
      case default
        call error_message("points_io: unsupported dtype: ", name, ": ", dtype)
    end select
  end subroutine points_io_dtype_defaults

  !> \brief Resolve temporal input from either explicit index or current time.
  integer(i4) function points_input_resolve_time_index(self, var_id, current_time, t_index)
    class(points_input_dataset), intent(in) :: self
    integer(i4), intent(in) :: var_id !< variable index
    type(datetime), optional, intent(in) :: current_time !< selected read time
    integer(i4), optional, intent(in) :: t_index !< explicit temporal index
    points_input_resolve_time_index = 0_i4
    if (self%vars(var_id)%static) then
      points_input_resolve_time_index = 0_i4
    else if (present(t_index)) then
      points_input_resolve_time_index = t_index
    else if (present(current_time)) then
      points_input_resolve_time_index = self%time_index(current_time)
    else
      call error_message("points_input%read: temporal variable needs current_time or t_index: ", self%vars(var_id)%name)
    end if
  end function points_input_resolve_time_index

  !> \brief Return the one-based index of an output variable.
  integer(i4) function points_output_var_index(self, name)
    class(points_output_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4) :: i
    points_output_var_index = 0_i4
    do i = 1_i4, self%nvars
      if (allocated(self%vars(i)%name)) then
        if (self%vars(i)%name == name) then
          points_output_var_index = i
          return
        end if
      end if
    end do
    call error_message("points_output%var_index: variable not present: ", name)
  end function points_output_var_index

  !> \brief Return the one-based index of a point-series output variable.
  integer(i4) function points_series_output_var_index(self, name)
    class(points_series_output_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4) :: i
    points_series_output_var_index = 0_i4
    do i = 1_i4, self%nvars
      if (allocated(self%vars(i)%name)) then
        if (self%vars(i)%name == name) then
          points_series_output_var_index = i
          return
        end if
      end if
    end do
    call error_message("points_series_output%var_index: variable not present: ", name)
  end function points_series_output_var_index

  !> \brief Return whether a point-series output variable was declared.
  logical function points_series_output_has_var(self, name)
    class(points_series_output_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4) :: i
    points_series_output_has_var = .false.
    do i = 1_i4, self%nvars
      if (allocated(self%vars(i)%name)) then
        if (self%vars(i)%name == name) then
          points_series_output_has_var = .true.
          return
        end if
      end if
    end do
  end function points_series_output_has_var

  !> \brief Return the one-based index of an input variable.
  integer(i4) function points_input_var_index(self, name)
    class(points_input_dataset), intent(in) :: self
    character(*), intent(in) :: name !< variable name
    integer(i4) :: i
    points_input_var_index = 0_i4
    do i = 1_i4, self%nvars
      if (allocated(self%vars(i)%name)) then
        if (self%vars(i)%name == name) then
          points_input_var_index = i
          return
        end if
      end if
    end do
    call error_message("points_input%var_index: variable not present: ", name)
  end function points_input_var_index

end module mo_points_io
