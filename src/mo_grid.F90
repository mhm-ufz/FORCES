!> \file    mo_grid.f90
!> \copydoc mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!!          In contrast to ascii grids, data will be assumed to follow xy axis order and increasing axis.
!!          Ascii grids actually represent data with a decreasing y-axis and in yx order.
!! \par Examples
!! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
!!   \include 01_regridding.f90
!! - \ref 02_nc_output.f90 : \copybrief 02_nc_output.f90
!! - \ref 03_nc_regridder.f90 : \copybrief 03_nc_regridder.f90
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_grid_constants, only: cartesian, spherical, keep_y, top_down, bottom_up, area_sum, area_full, area_count
  use mo_grid_helper, only: value_in_closed_interval, shift_longitude_near_query, quad_contains_point, &
                            arrays_match_2d, append_candidate_id, update_best_metric, intersection, calculate_coarse_extent, &
                            coarse_ij, id_bounds, dist_latlon, check_factor, read_ascii_grid, write_ascii_grid, &
                            read_ascii_header, data_t
#ifdef FORCES_WITH_NETCDF
  use mo_grid_helper, only: is_x_axis, is_y_axis, is_z_axis, is_t_axis, is_lon_coord, is_lat_coord, check_uniform_axis, &
                            mask_from_var, data_from_var
#endif
  use mo_kind, only: i1, i2, i4, i8, sp, dp
  use mo_constants, only : nodata_i1, nodata_i2, nodata_i4, nodata_i8, nodata_sp, nodata_dp, RadiusEarth_dp, deg2rad_dp
  use mo_utils, only: flip, optval, is_close
  use mo_message, only : error_message, warn_message, message
  use mo_spatial_index, only: spatial_index_t
  use mo_string_utils, only : num2str

  implicit none

  public :: write_ascii_grid
  public :: read_ascii_grid
  public :: read_ascii_header
  public :: coarse_ij
  public :: id_bounds
  public :: dist_latlon
  public :: check_factor
  public :: cartesian
  public :: spherical
  public :: keep_y
  public :: top_down
  public :: bottom_up
  public :: area_sum
  public :: area_full
  public :: area_count
  public :: data_t
#ifdef FORCES_WITH_NETCDF
  public :: is_x_axis
  public :: is_y_axis
  public :: is_z_axis
  public :: is_t_axis
  public :: is_lon_coord
  public :: is_lat_coord
  public :: check_uniform_axis
  public :: mask_from_var
  public :: data_from_var
#endif

  private
  integer(i8), parameter :: grid_spatial_index_parallel_min_n = 2048_i8

  !> \class   grid_t
  !> \brief   2D grid description with data in xy order.
  !> \details This type represents uniform grids with data in xy order with strictly increasing or decreasing y-axis.
  !!          ASCII grid files have the opposite behavior: yx order, with decreasing y-axis.
  !!          NetCDF files natively have yx order, but since Fortran arrays are column-major order,
  !!          the data read from .nc files is in xy order. The x-axis will always be increasing.
  !! \par Examples
  !! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
  type, public :: grid_t
    integer(i4) :: coordsys = cartesian !< Coordinate system for x and y. 0 -> Cartesian (default), 1 -> Spherical
    integer(i4) :: y_direction = top_down !< y-axis direction (either top_down (0, default) or bottom_up (1))
    ! general domain information
    integer(i4) :: nx        !< size of x-axis (number of cols in ascii grid file)
    integer(i4) :: ny        !< size of y-axis (number of rows in ascii grid file)
    integer(i8) :: ncells    !< number of cells in mask
    real(dp) :: xllcorner    !< x coordinate of the lowerleft corner
    real(dp) :: yllcorner    !< y coordinate of the lowerleft corner
    real(dp) :: cellsize     !< cellsize x = cellsize y
    real(dp), dimension(:), allocatable :: cell_area           !< area of the cell in sqare m, size (ncells)
    real(dp), dimension(:, :), allocatable :: lat              !< 2d longitude array (auxiliary coordinate for X axis), size (nx,ny)
    real(dp), dimension(:, :), allocatable :: lon              !< 2d latitude  array (auxiliary coordinate for Y axis), size (nx,ny)
    real(dp), dimension(:, :), allocatable :: lat_vertices     !< latitude coordinates or the grid nodes, size (nx+1,ny+1)
    real(dp), dimension(:, :), allocatable :: lon_vertices     !< longitude coordinates or the grid nodes, size (nx+1,ny+1)
    integer(i4), dimension(:, :), allocatable :: cell_ij       !< matrix IDs (i,j) per cell in mask, size (ncells, 2)
    logical, dimension(:, :), allocatable :: mask              !< the mask for valid cells in the original grid, size (nx,ny)
    integer(i8), dimension(:), allocatable :: mask_col_cnt     !< number of valid cells per column in mask, size (ny)
    integer(i8), dimension(:), allocatable :: mask_cum_col_cnt !< cumulative number of valid cells prior to mask column, size (ny)
  contains
    procedure, public :: init => grid_init
    procedure, public :: from_ascii_file => grid_from_ascii_file
    procedure, public :: to_ascii_file => grid_to_ascii_file
#ifdef FORCES_WITH_NETCDF
    procedure, private :: from_nc_dataset => grid_from_nc_dataset, from_nc_file => grid_from_nc_file
    generic, public :: from_netcdf => from_nc_dataset, from_nc_file
    procedure, private :: aux_from_nc_dataset => grid_aux_from_nc_dataset, aux_from_nc_file => grid_aux_from_nc_file
    generic, public :: aux_from_netcdf => aux_from_nc_dataset, aux_from_nc_file
    procedure, private :: to_nc_dataset => grid_to_nc_dataset, to_nc_file => grid_to_nc_file
    generic, public :: to_netcdf => to_nc_dataset, to_nc_file
    procedure, private :: to_restart_dataset => grid_to_restart_dataset, to_restart_file => grid_to_restart_file
    generic, public :: to_restart => to_restart_dataset, to_restart_file
    procedure, private :: from_restart_dataset => grid_from_restart_dataset, from_restart_file => grid_from_restart_file
    generic, public :: from_restart => from_restart_dataset, from_restart_file
#endif
    procedure, public :: extent => grid_extent
    procedure, public :: total_area => grid_total_area
    procedure, public :: id_matrix => grid_id_matrix
    procedure, public :: gen_id_matrix => grid_gen_id_matrix
    procedure, public :: cell_id => grid_cell_id
    procedure, private :: closest_cell_id_scalar => grid_closest_cell_id_scalar
    procedure, private :: closest_cell_id_batch => grid_closest_cell_id_batch
    generic, public :: closest_cell_id => closest_cell_id_scalar, closest_cell_id_batch
    procedure, public :: closest_cell_id_by_axes => grid_closest_cell_id_by_axes
    procedure, public :: build_spatial_index => grid_build_spatial_index
    procedure, public :: in_cell => grid_in_cell
    procedure, public :: x_center => grid_x_center
    procedure, public :: y_center => grid_y_center
    procedure, public :: normalize_longitude_near_domain => grid_normalize_longitude_near_domain
    procedure, public :: map_longitude_for_domain => grid_map_longitude_for_domain
    procedure, public :: x_axis => grid_x_axis
    procedure, public :: y_axis => grid_y_axis
    procedure, public :: x_vertices => grid_x_vertices
    procedure, public :: y_vertices => grid_y_vertices
    procedure, public :: x_bounds => grid_x_bounds
    procedure, public :: y_bounds => grid_y_bounds
    procedure, public :: upscale_aux_coords => grid_upscale_aux_coords
    procedure, public :: downscale_aux_coords => grid_downscale_aux_coords
    procedure, public :: estimate_aux_vertices => grid_estimate_aux_vertices
    procedure, public :: lat_bounds => grid_lat_bounds
    procedure, public :: lon_bounds => grid_lon_bounds
    procedure, public :: has_mask => grid_has_mask
    procedure, public :: any_missing => grid_any_missing
    procedure, public :: is_matching => grid_is_matching
    procedure, public :: check_is_covered_by => grid_check_is_covered_by
    procedure, public :: check_is_covering => grid_check_is_covering
    procedure, public :: check_is_filled_by => grid_check_is_filled_by
    procedure, public :: check_is_filling => grid_check_is_filling
    procedure, public :: has_aux_coords => grid_has_aux_coords
    procedure, public :: has_aux_vertices => grid_has_aux_vertices
    procedure, public :: calculate_cell_ids => grid_calculate_cell_ids
    procedure, public :: calculate_cell_area => grid_calculate_cell_area
    procedure, public :: is_periodic => grid_is_periodic
    procedure, private :: closest_cell_id_aux_scalar => grid_closest_cell_id_aux_scalar
    procedure, private :: closest_cell_id_regular_scalar => grid_closest_cell_id_regular_scalar
    procedure, private :: closest_cell_id_aux_lonlat => grid_closest_cell_id_aux_lonlat
    procedure, private :: closest_cell_id_regular_xy => grid_closest_cell_id_regular_xy
    procedure, private :: row_k_bounds => grid_row_k_bounds
    procedure, private :: row_candidate_ids => grid_row_candidate_ids
    procedure, private :: row_binary_search => grid_row_binary_search
    procedure, private :: nearest_y_index => grid_nearest_y_index
    procedure, private :: row_lower_bound => grid_row_lower_bound
    procedure, private :: cartesian_cell_metric => grid_cartesian_cell_metric
    procedure, private :: spherical_cell_metric => grid_spherical_cell_metric
    procedure, public :: derive_coarse_grid => grid_derive_coarse_grid
    procedure, public :: derive_fine_grid => grid_derive_fine_grid
    procedure, public :: gen_coarse_grid => grid_gen_coarse_grid
    procedure, public :: gen_fine_grid => grid_gen_fine_grid
    procedure, public :: derive_grid => grid_derive_grid
    procedure, public :: gen_grid => grid_gen_grid
    procedure, private :: read_data_dp => grid_read_data_dp, read_data_i4 => grid_read_data_i4
    generic, public :: read_data => read_data_dp, read_data_i4
    procedure, private :: check_shape => grid_check_shape
    procedure, private :: check_shape_packed => grid_check_shape_packed
    procedure, private :: pack_data_sp => grid_pack_data_sp, pack_data_dp => grid_pack_data_dp, &
                          pack_data_i1 => grid_pack_data_i1, pack_data_i2 => grid_pack_data_i2, &
                          pack_data_i4 => grid_pack_data_i4, pack_data_i8 => grid_pack_data_i8, &
                          pack_data_lgt => grid_pack_data_lgt
    procedure, private :: unpack_data_sp => grid_unpack_data_sp, unpack_data_dp => grid_unpack_data_dp, &
                          unpack_data_i1 => grid_unpack_data_i1, unpack_data_i2 => grid_unpack_data_i2, &
                          unpack_data_i4 => grid_unpack_data_i4, unpack_data_i8 => grid_unpack_data_i8, &
                          unpack_data_lgt => grid_unpack_data_lgt
    procedure, private :: pack_into_sp => grid_pack_into_sp, pack_into_dp => grid_pack_into_dp, &
                          pack_into_i1 => grid_pack_into_i1, pack_into_i2 => grid_pack_into_i2, &
                          pack_into_i4 => grid_pack_into_i4, pack_into_i8 => grid_pack_into_i8, &
                          pack_into_lgt => grid_pack_into_lgt
    procedure, private :: unpack_into_sp => grid_unpack_into_sp, unpack_into_dp => grid_unpack_into_dp, &
                          unpack_into_i1 => grid_unpack_into_i1, unpack_into_i2 => grid_unpack_into_i2, &
                          unpack_into_i4 => grid_unpack_into_i4, unpack_into_i8 => grid_unpack_into_i8, &
                          unpack_into_lgt => grid_unpack_into_lgt
    generic, public :: pack =>        pack_data_sp, pack_data_dp, &
                                      pack_data_i1, pack_data_i2, pack_data_i4, pack_data_i8, pack_data_lgt
    generic, public :: unpack =>      unpack_data_sp, unpack_data_dp, &
                                      unpack_data_i1, unpack_data_i2, unpack_data_i4, unpack_data_i8, unpack_data_lgt
    generic, public :: pack_into =>   pack_into_sp, pack_into_dp, &
                                      pack_into_i1, pack_into_i2, pack_into_i4, pack_into_i8, pack_into_lgt
    generic, public :: unpack_into => unpack_into_sp, unpack_into_dp, &
                                      unpack_into_i1, unpack_into_i2, unpack_into_i4, unpack_into_i8, unpack_into_lgt
  end type grid_t

contains

  ! ------------------------------------------------------------------

  ! ------------------------------------------------------------------

  !> \brief initialize grid from ascii header content
  !> \details initialize grid from standard ascii header content (nx (cols), ny (rows), cellsize, lower-left corner)
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_init(this, nx, ny, xllcorner, yllcorner, cellsize, coordsys, mask, y_direction)
    implicit none
    class(grid_t), intent(inout) :: this
    integer(i4), intent(in) :: nx !< Number of x-axis subdivisions
    integer(i4), intent(in) :: ny !< Number of y-axis subdivisions
    real(dp), optional, intent(in) :: xllcorner !< lower left corner (x) (default 0.0)
    real(dp), optional, intent(in) :: yllcorner !< lower left corner (y) (default 0.0)
    real(dp), optional, intent(in) :: cellsize !< cell size [m] or [deg] (default 1.0)
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (0 (default) for cartesian, 1 for spherical)
    logical, dimension(:,:), optional, intent(in) :: mask !< desired mask for the grid (default: all .true.)
    integer(i4), optional, intent(in) :: y_direction !< y-axis direction (0 (default) for top-down, 1 for bottom-up)
    integer(i4) :: j

    this%nx = nx
    this%ny = ny
    this%xllcorner = optval(xllcorner, 0.0_dp)
    this%yllcorner = optval(yllcorner, 0.0_dp)
    this%cellsize = optval(cellsize, 1.0_dp)
    ! check if coordsys is supported
    this%coordsys = optval(coordsys, cartesian)
    ! LCOV_EXCL_START
    if (.not.any(this%coordsys == [cartesian, spherical])) then
      call error_message("grid % init: unknown coordsys value: ", num2str(this%coordsys))
    end if
    ! LCOV_EXCL_STOP
    ! check if y-direction is supported
    this%y_direction = top_down
    if ( present(y_direction) ) then
      ! LCOV_EXCL_START
      if (.not.any(y_direction==[keep_y, bottom_up, top_down])) &
        call error_message("grid % init: unknown y_direction value: ", num2str(y_direction))
      ! LCOV_EXCL_STOP
      if (y_direction /= keep_y) this%y_direction = y_direction
    end if
    if ( present(mask) ) then
      ! LCOV_EXCL_START
      if (size(mask, dim=1) /= nx .or. size(mask, dim=2) /= ny) &
        call error_message("grid % init: mask has wrong shape: mask(", &
                           trim(adjustl(num2str(size(mask, dim=1)))), ",", &
                           trim(adjustl(num2str(size(mask, dim=2)))), ") =/= grid(", &
                           trim(adjustl(num2str(nx))), ",", &
                           trim(adjustl(num2str(ny))), ")")
      ! LCOV_EXCL_STOP
      allocate(this%mask(this%nx, this%ny))
      !$omp parallel do default(shared) schedule(static)
      do j = 1_i4, this%ny
        this%mask(:,j) = mask(:,j)
      end do
      !$omp end parallel do
    end if
    ! if no mask given, this will initialize the default mask
    call this%calculate_cell_ids()
    call this%calculate_cell_area()

  end subroutine grid_init

  !> \brief initialize grid from ascii grid file
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_from_ascii_file(this, path, coordsys, read_mask, y_direction, data)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (0 (default) for cartesian, 1 for lat-lon)
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given file (default: .true.)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) or 0 for top-down, 1 for bottom-up)
    type(data_t), intent(inout), optional :: data !< optional data container to read the variable data into

    integer(i4) :: nx, ny
    real(dp) :: xll, yll, cellsize
    real(dp), allocatable, dimension(:,:) :: dummy
    logical, allocatable, dimension(:,:) :: mask
    logical :: read_mask_
    integer(i4) :: y_dir

    read_mask_ = optval(read_mask, .true.)
    y_dir = optval(y_direction, keep_y)
    if (y_dir == keep_y) y_dir = top_down
    ! LCOV_EXCL_START
    if (.not.any(y_dir==[bottom_up, top_down])) &
      call error_message("grid % from_ascii_file: y-direction not valid: ", trim(num2str(y_dir)))
    ! LCOV_EXCL_STOP

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_header(path, nx ,ny, xll, yll, cellsize)

    if (read_mask_) then
      call read_ascii_grid(path, dummy, mask, y_direction=y_dir)
    else if (present(data)) then
      call read_ascii_grid(path, dummy, y_direction=y_dir)
    end if

    if (present(data)) then
      call data%deallocate()
      if (.not.allocated(data%dtype)) data%dtype = "f64"
      select case (data%dtype)
        case ("i8")
          data%data_i1 = int(dummy, i1)
        case ("i16")
          data%data_i2 = int(dummy, i2)
        case ("i32")
          data%data_i4 = int(dummy, i4)
        case ("i64")
          data%data_i8 = int(dummy, i8)
        case ("f32")
          data%data_sp = real(dummy, sp)
        case ("f64")
          call move_alloc(dummy, data%data_dp)
        case default
          call error_message("grid%from_ascii_file: Unsupported data type: ", data%dtype) ! LCOV_EXCL_LINE
      end select
    end if

    if (allocated(dummy)) deallocate(dummy)

    call this%init(nx, ny, xll, yll, cellsize, coordsys, mask, y_dir) ! un-allocated mask is interpreted as "not present" in init
    if (allocated(mask)) deallocate(mask)

  end subroutine grid_from_ascii_file

  !> \brief write grid to ascii grid file
  !> \details Writes the grid information to an ascii grid. If mask should be written, it will be stored as 1/nodata map.
  !!          If no mask should be written, only the header is stored.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_to_ascii_file(this, path, write_mask)
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    logical, optional, intent(in) :: write_mask !< Whether to write the mask to the given file (default: .true.)

    logical :: write_mask_
    integer(i4), allocatable, dimension(:,:) :: dummy

    write_mask_ = optval(write_mask, .true.)

    if (write_mask_) then
      allocate(dummy(this%nx, this%ny), source=nodata_i4)
      where (this%mask) dummy = 1_i4
    end if
    call write_ascii_grid( &
      path=path, &
      ncols=this%nx, &
      nrows=this%ny, &
      xllcorner=this%xllcorner, &
      yllcorner=this%yllcorner, &
      cellsize=this%cellsize, &
      nodata=nodata_i4, &
      data=dummy, &  ! unallocated dummy will result in present(data) == .false.
      y_direction=this%y_direction)
    if (allocated(dummy)) deallocate(dummy)
  end subroutine grid_to_ascii_file

  !> \brief Read data from ascii file conforming this grid
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine grid_read_data_dp(this, path, data)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    real(dp), intent(out), allocatable, dimension(:,:) :: data

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_grid(path, data, &
      ref_ncols=this%nx, &
      ref_nrows=this%ny, &
      ref_xllcorner=this%xllcorner, &
      ref_yllcorner=this%yllcorner, &
      ref_cellsize=this%cellsize, &
      y_direction=this%y_direction)
  end subroutine grid_read_data_dp

  !> \brief Read data from ascii file conforming this grid
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine grid_read_data_i4(this, path, data)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    integer(i4), intent(out), allocatable, dimension(:,:) :: data

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_grid(path, data, &
      ref_ncols=this%nx, &
      ref_nrows=this%ny, &
      ref_xllcorner=this%xllcorner, &
      ref_yllcorner=this%yllcorner, &
      ref_cellsize=this%cellsize, &
      y_direction=this%y_direction)
  end subroutine grid_read_data_i4

#ifdef FORCES_WITH_NETCDF

  !> \brief initialize grid from a netcdf file
  !> \details initialize grid from a netcdf file and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_from_nc_file(this, path, var, read_mask, read_aux, tol, y_direction, data)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)
    type(data_t), intent(inout), optional :: data !< optional data container to read the variable data into
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%from_nc_dataset(nc, var, read_mask, read_aux, tol, y_direction, data)
    call nc%close()
  end subroutine grid_from_nc_file

  !> \brief initialize grid from a netcdf dataset
  !> \details initialize grid from a netcdf dataset and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_from_nc_dataset(this, nc, var, read_mask, read_aux, tol, y_direction, data)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)
    type(data_t), intent(inout), optional :: data !< optional data container to read the variable data into

    type(NcVariable) :: ncvar, xvar, yvar
    type(NcDimension), dimension(:), allocatable :: dims

    integer(i4) :: rnk, i
    real(dp) :: cs_x, cs_y, tol_
    character(len=256) :: tmp_str
    character(len=256), allocatable, dimension(:) :: coords_str
    character(:), allocatable :: lat_name, lon_name
    logical :: y_inc, read_mask_, read_aux_, x_sph, y_sph, x_cart, y_cart, flip_y, found_lat, found_lon

    this%y_direction = optval(y_direction, keep_y)

    tol_ = optval(tol, 1.0e-7_dp)
    read_mask_ = optval(read_mask, .true.)
    read_aux_ = optval(read_aux, .true.)

    ncvar = nc%getVariable(var)
    rnk = ncvar%getRank()
    if (rnk < 2) call error_message("grid % from_netcdf: given variable has too few dimensions: ", trim(nc%fname), ":", var) ! LCOV_EXCL_LINE

    dims = ncvar%getDimensions()
    this%nx = dims(1)%getLength()
    this%ny = dims(2)%getLength()
    xvar = nc%getVariable(trim(dims(1)%getName()))
    yvar = nc%getVariable(trim(dims(2)%getName()))

    ! check if x/y axis are x/y/lon/lat by standard_name, units, axistype or long_name
    ! LCOV_EXCL_START
    if (is_x_axis(yvar).or.is_lon_coord(yvar).or.is_y_axis(xvar).or.is_lat_coord(xvar)) &
      call error_message("grid % from_netcdf: variable seems to have wrong axis order (not y-x): ", trim(nc%fname), ":", var)
    ! LCOV_EXCL_STOP

    x_cart = is_x_axis(xvar)
    y_cart = is_y_axis(yvar)
    x_sph = is_lon_coord(xvar)
    y_sph = is_lat_coord(yvar)

    ! LCOV_EXCL_START
    if (.not.(x_cart.or.x_sph)) &
      call error_message("grid % from_netcdf: can't determine coordinate system from x-axis: ", trim(nc%fname), ":", var)
    if (.not.(y_cart.or.y_sph)) &
      call error_message("grid % from_netcdf: can't determine coordinate system from y-axis: ", trim(nc%fname), ":", var)
    if (.not.(x_sph.eqv.y_sph)) &
      call error_message("grid % from_netcdf: x and y axis seem to have different coordinate systems: ", trim(nc%fname), ":", var)
    ! LCOV_EXCL_STOP

    this%coordsys = cartesian
    if (x_sph) this%coordsys = spherical

    ! check axis uniformity and monotonicity
    call check_uniform_axis(xvar, cellsize=cs_x, origin=this%xllcorner, tol=tol)
    call check_uniform_axis(yvar, cellsize=cs_y, origin=this%yllcorner, increasing=y_inc, tol=tol)
    if (this%y_direction == keep_y) then
      this%y_direction = top_down
      if (y_inc) this%y_direction = bottom_up
    end if
    ! check y_dir
    ! LCOV_EXCL_START
    if (.not.any(this%y_direction==[bottom_up, top_down])) &
      call error_message("grid % from_netcdf: y-direction not valid: ", trim(num2str(this%y_direction)))
    ! LCOV_EXCL_STOP

    ! warn about flipping if present axis is not in desired direction
    flip_y = y_inc.neqv.(this%y_direction==bottom_up)
    if (flip_y) then
      call warn_message("grid % from_netcdf: y axis direction is oposite to desired one (inefficient flipping). ", &
                        "You could flip the file beforehand with: 'cdo invertlat <ifile> <ofile>'. ", trim(nc%fname), ":", var)
    end if
    ! check cellsize in x and y direction
    ! LCOV_EXCL_START
    if (.not.is_close(cs_x, cs_y, rtol=0.0_dp, atol=tol_)) &
      call error_message("grid % from_netcdf: x and y axis have different cell sizes: ", trim(nc%fname), ":", var)
    ! LCOV_EXCL_STOP
    this%cellsize = cs_x

    ! get mask from variable mask (assumed to be constant over time)
    if (read_mask_) call mask_from_var(ncvar, this%mask, data, flip_y)
    if (present(data) .and. .not.read_mask_) call data_from_var(ncvar, data, flip_y)
    call this%calculate_cell_ids()
    call this%calculate_cell_area()

    if (read_aux_ .and. this%coordsys == cartesian .and. ncvar%hasAttribute("coordinates")) then
      call ncvar%getAttribute("coordinates", tmp_str)
      coords_str = splitString(trim(tmp_str), " ")
      ! search for lat-lon variables in given coordinates
      found_lat = .false.
      found_lon = .false.
      do i = 1_i4, size(coords_str)
        ncvar = nc%getVariable(trim(coords_str(i)))
        if (.not.found_lat) then
          if (is_lat_coord(ncvar)) then
            found_lat = .true.
            lat_name = trim(coords_str(i))
          end if
        end if
        if (.not.found_lon) then
          if (is_lon_coord(ncvar)) then
            found_lon = .true.
            lon_name = trim(coords_str(i))
          end if
        end if
      end do
      ! LCOV_EXCL_START
      if (.not.(found_lat.and.found_lon)) then
        call error_message( "grid % from_netcdf: could not find lat/lon auxilliar coordinates: ", &
                           trim(nc%fname), ":", var, " - ", trim(tmp_str))
      end if
      ! LCOV_EXCL_STOP
      call this%aux_from_netcdf(nc, lat=trim(coords_str(size(coords_str)-1)), lon=trim(coords_str(size(coords_str))))
    end if

  end subroutine grid_from_nc_dataset

  !> \brief read auxilliar coordinates from a netcdf file
  !> \details read auxilliar coordinates (lat, lon) from a netcdf file.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_aux_from_nc_file(this, path, lat, lon)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: lat !< nc variable name for latitude
    character(*), intent(in) :: lon !< nc variable name for longitude
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%aux_from_nc_dataset(nc, lat, lon)
    call nc%close()
  end subroutine grid_aux_from_nc_file

  !> \brief read auxilliar coordinates from a netcdf file
  !> \details read auxilliar coordinates (lat, lon) from a netcdf file.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_aux_from_nc_dataset(this, nc, lat, lon)
    use mo_netcdf, only : NcDataset, NcVariable
    use mo_utils, only : swap
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: lat !< nc variable name for latitude
    character(*), intent(in) :: lon !< nc variable name for longitude

    type(NcVariable) :: latvar, lonvar, latbvar, lonbvar
    integer(i4) :: rnk, i_ll, i_lr, i_ur, i_ul
    real(dp), allocatable, dimension(:,:) :: dummy
    real(dp), allocatable, dimension(:,:,:) :: latbnds, lonbnds
    logical :: y_inc, flip_v, flip_h, flip_y
    character(:), allocatable :: lon_, lat_
    character(len=256) :: name

    if (this%coordsys /= cartesian) &
      call error_message("grid % aux_from_netcdf: need projected axis to have auxilliar coordinates.") ! LCOV_EXCL_LINE

    lon_ = lon
    lat_ = lat

    latvar = nc%getVariable(lat_)
    if (is_lon_coord(latvar)) then
      ! be forgiving when coordinate order is wrong
      call warn_message("grid % aux_from_netcdf: auxilliar coordinates seem to be given in wrong order: ", &
                        trim(nc%fname), ":", lat, "/", lon)
      lat_ = lon
      lon_ = lat
      latvar = nc%getVariable(lat_)
    end if

    if (.not.is_lat_coord(latvar)) &
      call error_message("grid % aux_from_netcdf: auxilliar latitude coordinate is not valid: ", trim(nc%fname), ":", lat_) ! LCOV_EXCL_LINE
    rnk = latvar%getRank()
    if (rnk /= 2) &
      call error_message("grid % from_netcdf: auxilliar latitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lat_) ! LCOV_EXCL_LINE

    call latvar%getData(dummy)
    y_inc = .true.
    if (size(dummy, dim=2)>1) y_inc = dummy(1,1) < dummy(1,2)
    if (size(dummy, dim=1) /= this%nx .or. size(dummy, dim=2) /= this%ny) &
      call error_message("grid % from_netcdf: auxilliar latitude coordinate has wrong shape: ", trim(nc%fname), ":", lat_) ! LCOV_EXCL_LINE

    flip_y = y_inc.neqv.(this%y_direction==bottom_up)
    allocate(this%lat(this%nx, this%ny))
    if (flip_y) call flip(dummy, iDim=2)
    this%lat = dummy
    deallocate(dummy)

    lonvar = nc%getVariable(lon_)
    if (.not.is_lon_coord(lonvar)) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not valid: ", trim(nc%fname), ":", lon_) ! LCOV_EXCL_LINE
    rnk = lonvar%getRank()
    if (rnk /= 2) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lon_) ! LCOV_EXCL_LINE

    call lonvar%getData(dummy)
    if (size(dummy, dim=1) /= this%nx .or. size(dummy, dim=2) /= this%ny) &
      call error_message("grid % from_netcdf: auxilliar longitude coordinate has wrong shape: ", trim(nc%fname), ":", lon_) ! LCOV_EXCL_LINE
    allocate(this%lon(this%nx, this%ny))
    if (flip_y) call flip(dummy, iDim=2)
    this%lon = dummy
    deallocate(dummy)

    ! read aux vertices if present
    if (latvar%hasAttribute("bounds") .and. lonvar%hasAttribute("bounds")) then
      ! indices for lower-left, lower-right, upper-right and upper-left
      i_ll = 1_i4
      i_lr = 2_i4
      i_ur = 3_i4
      i_ul = 4_i4
      ! lat bounds (4, nx, ny)
      call latvar%getAttribute("bounds", name)
      latbvar = latvar%parent%getVariable(trim(name))
      call latbvar%getData(latbnds)
      if (.not.y_inc) call flip(latbnds, iDim=3) ! easier to use increasing bounds for calculations
      ! lon bounds (4, nx, ny)
      call lonvar%getAttribute("bounds", name)
      lonbvar = lonvar%parent%getVariable(trim(name))
      call lonbvar%getData(lonbnds)
      if (.not.y_inc) call flip(lonbnds, iDim=3) ! easier to use increasing bounds for calculations

      ! check if bounds need to be flipped (ACTUALLY UNEEDED IF CF CONFORMING)
      ! should look like this:      if y is decreasing:     if x is decreasing:     if x and y are decreasing:
      ! 4 -- 3   ul--ur             1 -- 2                  3 -- 4                  2 -- 1
      ! |    |   |    |             |    |                  |    |                  |    |
      ! 1 -- 2   ll--lr             4 -- 3                  2 -- 1                  3 -- 4
      flip_h = lonbnds(2,1,1) > lonbnds(1,1,1) ! .true. if x decreasing
      flip_v = latbnds(2,1,1) > latbnds(3,1,1) ! .true. if y decreasing
      if (flip_h) then
        call swap(i_ll, i_lr)
        call swap(i_ul, i_ur)
      end if
      if (flip_v) then
        call swap(i_ll, i_ul)
        call swap(i_lr, i_ur)
      end if
      allocate(this%lat_vertices(this%nx+1_i4, this%ny+1_i4))
      allocate(this%lon_vertices(this%nx+1_i4, this%ny+1_i4))
      ! lower left corner for matrix excluding right and upper side
      this%lat_vertices(1:this%nx, 1:this%ny) = latbnds(i_ll,:,:)
      this%lon_vertices(1:this%nx, 1:this%ny) = lonbnds(i_ll,:,:)
      ! lower right corner for right end of vertices map
      this%lat_vertices(this%nx+1_i4, 1:this%ny) = latbnds(i_lr,this%nx,:)
      this%lon_vertices(this%nx+1_i4, 1:this%ny) = lonbnds(i_lr,this%nx,:)
      ! upper left corner for upper end of vertices map
      this%lat_vertices(1:this%nx, this%ny+1_i4) = latbnds(i_ul,:,this%ny)
      this%lon_vertices(1:this%nx, this%ny+1_i4) = lonbnds(i_ul,:,this%ny)
      ! upper right corner of upper right cell for upper right end of vertices map
      this%lat_vertices(this%nx+1_i4, this%ny+1_i4) = latbnds(i_ur,this%nx,this%ny)
      this%lon_vertices(this%nx+1_i4, this%ny+1_i4) = lonbnds(i_ur,this%nx,this%ny)
      deallocate(lonbnds)
      deallocate(latbnds)
      ! vertices created from increasing axis, flip them if needed
      if (this%y_direction == top_down) then
        call flip(this%lat_vertices, 2)
        call flip(this%lon_vertices, 2)
      end if
    end if

  end subroutine grid_aux_from_nc_dataset

  !> \brief write grid to a restart file
  !> \details write grid to a restart file with additional grid information (cell ids).
  !! - mask_col_cnt
  !! - mask_cum_col_cnt (faster access to cell ids)
  !! - mask (optional, but also included in area, default: not included)
  !! - area (optional, default: included)
  !> \authors Sebastian Müller
  !> \date Feb 2026
  subroutine grid_to_restart_file(this, path, prefix, mask, area, append)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(in) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: prefix !< prefix for variable and dimension names (default: "")
    logical, optional, intent(in) :: mask !< whether to write mask variable (default: .false.)
    logical, optional, intent(in) :: area !< whether to write cell area variable (default: .false.)
    logical, optional, intent(in) :: append !< whether netcdf file should be opened in append mode (default .false.)
    type(NcDataset) :: nc
    character(1) :: fmode
    fmode = "w"
    if ( optval(append, .false.) ) fmode = "a"
    nc = NcDataset(path, fmode)
    call this%to_restart_dataset(nc, prefix, mask, area)
    call nc%close()
  end subroutine grid_to_restart_file

  !> \brief write grid to a restart dataset
  !> \details write grid to a restart dataset with additional grid information (cell ids):
  !! - mask_col_cnt
  !! - mask_cum_col_cnt (faster access to cell ids)
  !! - mask (optional, but also included in area, default: not included)
  !! - area (optional, default: included)
  !> \authors Sebastian Müller
  !> \date Feb 2026
  subroutine grid_to_restart_dataset(this, nc, prefix, mask, area)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    implicit none
    class(grid_t), intent(in) :: this
    type(NcDataset), intent(inout) :: nc !< NetCDF Dataset
    character(*), optional, intent(in) :: prefix !< prefix for variable and dimension names (default: "")
    logical, optional, intent(in) :: mask !< whether to write mask variable (default: .false., but included in area)
    logical, optional, intent(in) :: area !< whether to write cell area variable (default: .true.)
    character(:), allocatable :: x_name, y_name, lon_name, lat_name, pre
    integer(i1), allocatable, dimension(:,:) :: mask_data
    real(dp), allocatable, dimension(:,:) :: area_data
    integer(i4) :: i, j
    type(NcDimension) :: x_dim, y_dim
    type(NcVariable) :: var

    pre = optval(prefix, "")
    if (this%coordsys==cartesian) then
      x_name = pre//"x"
      y_name = pre//"y"
    else
      x_name = pre//"lon"
      y_name = pre//"lat"
    end if
    lon_name = pre//"lon"
    lat_name = pre//"lat"

    call this%to_nc_dataset(nc, x_name=x_name, y_name=y_name, aux_lon_name=lon_name, aux_lat_name=lat_name)

    x_dim = nc%getDimension(x_name)
    y_dim = nc%getDimension(y_name)

    var = nc%setVariable(pre//"mask_col_cnt", "i64", [y_dim])
    call var%setAttribute("long_name", "number of valid cells in each column")
    call var%setAttribute("units", "1")
    call var%setData(this%mask_col_cnt)

    var = nc%setVariable(pre//"mask_cum_col_cnt", "i64", [y_dim])
    call var%setAttribute("long_name", "cumulative number of valid cells prior to column")
    call var%setAttribute("units", "1")
    call var%setData(this%mask_cum_col_cnt)

    if (optval(mask, .false.)) then
      allocate(mask_data(this%nx, this%ny))
      !$omp parallel do private(i,j) schedule(static)
      do j = 1_i4, this%ny
        do i = 1_i4, this%nx
          if (this%mask(i,j)) then
            mask_data(i,j) = 1_i1
          else
            mask_data(i,j) = 0_i1
          end if
        end do
      end do
      !$omp end parallel do
      var = nc%setVariable(pre//"mask", "i8", [x_dim, y_dim])
      call var%setAttribute("long_name", "mask")
      call var%setAttribute("standard_name", "land_binary_mask")
      call var%setAttribute("units", "1")
      if (this%has_aux_coords()) call var%setAttribute("coordinates", lon_name//" "//lat_name)
      call var%setFillValue(0_i1)
      call var%setAttribute("missing_value", 0_i1)
      call var%setData(mask_data)
      deallocate(mask_data)
    end if

    if (optval(area, .true.) .and. allocated(this%cell_area)) then
      allocate(area_data(this%nx, this%ny))
      call this%unpack_into(this%cell_area, area_data)
      var = nc%setVariable(pre//"area", "f64", [x_dim, y_dim])
      call var%setAttribute("long_name", "cell area")
      call var%setAttribute("standard_name", "cell_area")
      call var%setAttribute("units", "m2") ! TODO: should be configurable
      if (this%has_aux_coords()) call var%setAttribute("coordinates", lon_name//" "//lat_name)
      call var%setFillValue(nodata_dp)
      call var%setAttribute("missing_value", nodata_dp)
      call var%setData(area_data)
      deallocate(area_data)
    end if

  end subroutine grid_to_restart_dataset

  !> \brief initialize grid from a restart file
  !> \details initialize grid from a restart file with additional grid information (cell ids).
  !> \authors Sebastian Müller
  !> \date Feb 2026
  subroutine grid_from_restart_file(this, path, prefix)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: prefix !< prefix for variable and dimension names (default: "")
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%from_restart_dataset(nc, prefix)
    call nc%close()
  end subroutine grid_from_restart_file

  !> \brief initialize grid from a restart dataset
  !> \details initialize grid from a restart dataset with additional grid information (cell ids).
  !> \authors Sebastian Müller
  !> \date Feb 2026
  subroutine grid_from_restart_dataset(this, nc, prefix)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), optional, intent(in) :: prefix !< prefix for variable and dimension names (default: "")
    character(:), allocatable :: x_name, y_name, lon_name, lat_name, pre
    logical :: y_inc
    type(NcVariable) :: xvar, yvar, var
    type(NcDimension) :: xdim, ydim
    integer(i4) :: i, j
    integer(i8) :: k
    type(data_t) :: data

    pre = optval(prefix, "")
    x_name = pre//"x"
    y_name = pre//"y"
    lon_name = pre//"lon"
    lat_name = pre//"lat"

    if (nc%hasDimension(x_name) .and. nc%hasDimension(y_name)) then
      xdim = nc%getDimension(x_name)
      ydim = nc%getDimension(y_name)
      xvar = nc%getVariable(x_name)
      yvar = nc%getVariable(y_name)
      this%coordsys = cartesian
    else if (nc%hasDimension(lon_name) .and. nc%hasDimension(lat_name)) then
      xdim = nc%getDimension(lon_name)
      ydim = nc%getDimension(lat_name)
      xvar = nc%getVariable(lon_name)
      yvar = nc%getVariable(lat_name)
      this%coordsys = spherical
    else
      call error_message("grid % from_restart_dataset: dimensions not found: ", trim(nc%fname), ":", x_name, ", ", y_name)
    end if
    this%nx = xdim%getLength()
    this%ny = ydim%getLength()
    call check_uniform_axis(xvar, cellsize=this%cellsize, origin=this%xllcorner)
    call check_uniform_axis(yvar, origin=this%yllcorner, increasing=y_inc)

    this%y_direction = top_down
    if (y_inc) this%y_direction = bottom_up

    var = nc%getVariable(pre//"mask_col_cnt")
    call var%getData(this%mask_col_cnt)

    var = nc%getVariable(pre//"mask_cum_col_cnt")
    call var%getData(this%mask_cum_col_cnt)

    this%ncells = this%mask_cum_col_cnt(this%ny) + this%mask_col_cnt(this%ny)

    data%dtype = "f64"
    if (nc%hasVariable(pre//"area")) then
      call mask_from_var(nc%getVariable(pre//"area"), this%mask, data)
    else if (nc%hasVariable(pre//"mask")) then
      call mask_from_var(nc%getVariable(pre//"mask"), this%mask)
    else
      if (allocated(this%mask)) deallocate(this%mask)
      allocate(this%mask(this%nx, this%ny))
      !$omp parallel do default(shared) schedule(static)
      do j = 1_i4, this%ny
        this%mask(:,j) = .true.
      end do
      !$omp end parallel do
    end if

    ! calculate cell ids from mask and cumulative column counts
    if (allocated(this%cell_ij)) deallocate(this%cell_ij)
    allocate(this%cell_ij(this%ncells, 2))
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i, j)) cycle
        k = k + 1_i8
        this%cell_ij(k, 1) = i
        this%cell_ij(k, 2) = j
      end do
    end do
    !$omp end parallel do

    ! set area after cell ids to ensure correct packing if area variable is present
    if (nc%hasVariable(pre//"area")) then
      if (allocated(this%cell_area)) deallocate(this%cell_area)
      allocate(this%cell_area(this%ncells))
      call this%pack_into(data%data_dp, this%cell_area)
      call data%deallocate()
    else
      call this%calculate_cell_area()
    end if

    ! read aux coordinates if present
    if (this%coordsys == cartesian .and. nc%hasVariable(lon_name) .and. nc%hasVariable(lat_name)) then
      call this%aux_from_netcdf(nc, lat=lat_name, lon=lon_name)
    end if

  end subroutine grid_from_restart_dataset

  !> \brief write grid to a netcdf file
  !> \details write grid to a netcdf file with possible data variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_to_nc_file(this, path, x_name, y_name, aux_lon_name, aux_lat_name, double_precision, append)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(in) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension (default: "x" (cartesian), "lon" (spherical))
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension (default: "y" (cartesian), "lat" (spherical))
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable (default: "lon")
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable (default: "lat")
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    logical, optional, intent(in) :: append !< whether netcdf file should be opened in append mode (default .false.)
    type(NcDataset) :: nc
    character(1) :: fmode
    fmode = "w"
    if ( optval(append, .false.) ) fmode = "a"
    nc = NcDataset(path, fmode)
    call this%to_nc_dataset(nc, x_name, y_name, aux_lon_name, aux_lat_name, double_precision)
    call nc%close()
  end subroutine grid_to_nc_file

  !> \brief write grid to a netcdf dataset
  !> \details write grid to a netcdf dataset with possible data variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_to_nc_dataset(this, nc, x_name, y_name, aux_lon_name, aux_lat_name, double_precision)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(grid_t), intent(in) :: this
    type(NcDataset), intent(inout) :: nc !< NetCDF Dataset
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension (default: "x" or "lon")
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension (default: "y" or "lat")
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable (default: "lon")
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable (default: "lat")
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    type(NcDimension) :: x_dim, y_dim, b_dim, v_dim
    type(NcVariable) :: x_var, y_var, xb_var, yb_var
    character(:), allocatable :: xname, yname, lonname, latname
    logical :: double_precision_
    character(3) :: dtype

    double_precision_ = optval(double_precision, .true.)

    dtype = "f32"
    if ( double_precision_ ) dtype = "f64"

    if (this%coordsys==cartesian) then
      xname = optval(x_name, "x")
      yname = optval(y_name, "y")
    else
      xname = optval(x_name, "lon")
      yname = optval(y_name, "lat")
    end if
    lonname = optval(aux_lon_name, "lon")
    latname = optval(aux_lat_name, "lat")

    x_dim = nc%setDimension(xname, this%nx)
    y_dim = nc%setDimension(yname, this%ny)
    if (nc%hasDimension("bnds")) then
      b_dim = nc%getDimension("bnds") ! check size
      ! LCOV_EXCL_START
      if (b_dim%getLength() /= 2_i4) then
        call error_message("grid % to_netcdf: bnds dim already present but with length =/= 2")
      end if
      ! LCOV_EXCL_STOP
    else
      b_dim = nc%setDimension("bnds", 2_i4)
    end if
    if (this%has_aux_vertices()) then
      if (nc%hasDimension("nv")) then
        v_dim = nc%getDimension("nv") ! check size
        ! LCOV_EXCL_START
        if (v_dim%getLength() /= 4_i4) then
          call error_message("grid % to_netcdf: nv dim already present but with length =/= 4")
        end if
        ! LCOV_EXCL_STOP
      else
        v_dim = nc%setDimension("nv", 4_i4)
      end if
    end if
    x_var = nc%setVariable(xname, dtype, [x_dim])
    y_var = nc%setVariable(yname, dtype, [y_dim])
    xb_var = nc%setVariable(xname // "_bnds", dtype, [b_dim, x_dim])
    yb_var = nc%setVariable(yname // "_bnds", dtype, [b_dim, y_dim])

    if (this%coordsys==cartesian) then
      call x_var%setAttribute("long_name", "x coordinate of projection")
      call x_var%setAttribute("standard_name", "projection_x_coordinate")
      call x_var%setAttribute("units", "m") ! TODO: this should be configurable
      call y_var%setAttribute("long_name", "y coordinate of projection")
      call y_var%setAttribute("standard_name", "projection_y_coordinate")
      call y_var%setAttribute("units", "m") ! TODO: this should be configurable
    else
      call x_var%setAttribute("long_name", "longitude")
      call x_var%setAttribute("standard_name", "longitude")
      call x_var%setAttribute("units", "degrees_east")
      call y_var%setAttribute("long_name", "latitude")
      call y_var%setAttribute("standard_name", "latitude")
      call y_var%setAttribute("units", "degrees_north")
    end if
    call x_var%setAttribute("axis", "X")
    call x_var%setAttribute("bounds", xname // "_bnds")
    call y_var%setAttribute("axis", "Y")
    call y_var%setAttribute("bounds", yname // "_bnds")

    if (double_precision_) then
      call x_var%setData(this%x_axis())
      call y_var%setData(this%y_axis())
      call xb_var%setData(this%x_bounds())
      call yb_var%setData(this%y_bounds())
    else
      call x_var%setData(real(this%x_axis(), sp))
      call y_var%setData(real(this%y_axis(), sp))
      call xb_var%setData(real(this%x_bounds(), sp))
      call yb_var%setData(real(this%y_bounds(), sp))
    end if

    if (this%has_aux_coords()) then
      x_var = nc%setVariable(lonname, dtype, [x_dim, y_dim])
      y_var = nc%setVariable(latname, dtype, [x_dim, y_dim])
      call x_var%setAttribute("long_name", "longitude")
      call x_var%setAttribute("standard_name", "longitude")
      call x_var%setAttribute("units", "degrees_east")
      call y_var%setAttribute("long_name", "latitude")
      call y_var%setAttribute("standard_name", "latitude")
      call y_var%setAttribute("units", "degrees_north")

      if (double_precision_) then
        call x_var%setData(this%lon)
        call y_var%setData(this%lat)
      else
        call x_var%setData(real(this%lon, sp))
        call y_var%setData(real(this%lat, sp))
      end if

      if (this%has_aux_vertices()) then
        call x_var%setAttribute("bounds", lonname // "_bnds")
        call y_var%setAttribute("bounds", latname // "_bnds")
        xb_var = nc%setVariable(lonname // "_bnds", dtype, [v_dim, x_dim, y_dim])
        yb_var = nc%setVariable(latname // "_bnds", dtype, [v_dim, x_dim, y_dim])
        if (double_precision_) then
          call xb_var%setData(this%lon_bounds())
          call yb_var%setData(this%lat_bounds())
        else
          call xb_var%setData(real(this%lon_bounds(), sp))
          call yb_var%setData(real(this%lat_bounds(), sp))
        end if
      end if
    end if

  end subroutine grid_to_nc_dataset

#endif

  !> \brief get grid extent
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_extent(this, x_min, x_max, y_min, y_max, x_size, y_size)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), optional, intent(out) :: x_min !< left bound (x)
    real(dp), optional, intent(out) :: x_max !< right bound (x)
    real(dp), optional, intent(out) :: y_min !< lower bound (y)
    real(dp), optional, intent(out) :: y_max !< upper bound (y)
    real(dp), optional, intent(out) :: x_size !< x extent
    real(dp), optional, intent(out) :: y_size !< y extent

    if ( present(x_min) ) x_min = this%xllcorner
    if ( present(x_max) ) x_max = this%xllcorner + this%nx * this%cellsize
    if ( present(y_min) ) y_min = this%yllcorner
    if ( present(y_max) ) y_max = this%yllcorner + this%ny * this%cellsize
    if ( present(x_size) ) x_size = this%nx * this%cellsize
    if ( present(y_size) ) y_size = this%ny * this%cellsize

  end subroutine grid_extent

  !> \brief Total domain area. NaN if cell_area not allocated.
  !> \return `real(dp) :: total_area`
  !> \authors Sebastian Müller
  !> \date Apr 2025
  real(dp) function grid_total_area(this) result(total_area)
    use mo_sentinel, only : set_sentinel
    implicit none
    class(grid_t), intent(in) :: this

    if (allocated(this%cell_area)) then
      total_area = sum(this%cell_area)
    else
      call set_sentinel(total_area)
    end if

  end function grid_total_area

  !> \brief Matrix of cell IDs.
  !> \return `integer(i8) :: id_matrix(nx,ny)`
  !> \authors Sebastian Müller
  !> \date Jun 2025
  function grid_id_matrix(this) result(id_matrix)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), dimension(this%nx, this%ny) :: id_matrix
    call this%gen_id_matrix(id_matrix)
  end function grid_id_matrix

  !> \brief Generate matrix of cell IDs.
  subroutine grid_gen_id_matrix(this, mat)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(out) :: mat(:, :)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(mat, kind=i4))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          mat(i,j) = k
        else
          mat(i,j) = nodata_i8
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_gen_id_matrix

  !> \brief Cell ID for given matrix indices.
  !> \return `integer(i8) :: cell_id`
  !> \authors Sebastian Müller
  !> \date Jun 2025
  integer(i8) function grid_cell_id(this, indices) result(cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: indices(2) !< matrix indices (x_i,y_i)
    if (indices(1) < 1_i4 .or. indices(1) > this%nx .or. indices(2) < 1_i4 .or. indices(2) > this%ny) &
      call error_message("grid%cell_id: given indices are out of bounds.") ! LCOV_EXCL_LINE
    if (.not.this%mask(indices(1), indices(2))) call error_message("grid%cell_id: given indices are masked.") ! LCOV_EXCL_LINE
    cell_id = this%mask_cum_col_cnt(indices(2)) + count(this%mask(1_i4:indices(1), indices(2)), kind=i8)
  end function grid_cell_id

  !> \brief Build a spatial index over active grid cell centers.
  !> \details Point ids in the resulting index match packed active-cell ids.
  subroutine grid_build_spatial_index(this, index, use_aux)
    implicit none
    class(grid_t), intent(in) :: this
    type(spatial_index_t), intent(out) :: index
    logical, intent(in), optional :: use_aux

    logical :: aux
    integer(i8) :: k
    integer(i4) :: i, j
    integer(i8), allocatable :: point_ids(:)
    real(dp), allocatable :: points(:, :)

    aux = optval(use_aux, .false.)
    if (aux .and. .not. this%has_aux_coords()) &
      call error_message("grid%build_spatial_index: no auxilliar coordniates defined.") ! LCOV_EXCL_LINE

    allocate(point_ids(this%ncells))
    allocate(points(this%ncells, 2))

    if (aux) then
      !$omp parallel do default(shared) private(k,i,j) schedule(static) if(this%ncells >= grid_spatial_index_parallel_min_n)
      do k = 1_i8, this%ncells
        point_ids(k) = k
        i = this%cell_ij(k, 1)
        j = this%cell_ij(k, 2)
        points(k, 1) = this%lon(i, j)
        points(k, 2) = this%lat(i, j)
      end do
      !$omp end parallel do
    else if (this%coordsys == spherical) then
      !$omp parallel do default(shared) private(k,i,j) schedule(static) if(this%ncells >= grid_spatial_index_parallel_min_n)
      do k = 1_i8, this%ncells
        point_ids(k) = k
        i = this%cell_ij(k, 1)
        j = this%cell_ij(k, 2)
        points(k, 1) = this%x_center(i)
        points(k, 2) = this%y_center(j)
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(k,i,j) schedule(static) if(this%ncells >= grid_spatial_index_parallel_min_n)
      do k = 1_i8, this%ncells
        point_ids(k) = k
        i = this%cell_ij(k, 1)
        j = this%cell_ij(k, 2)
        points(k, 1) = this%x_center(i)
        points(k, 2) = this%y_center(j)
      end do
      !$omp end parallel do
    end if

    if (aux .or. this%coordsys == spherical) then
      call index%init_lonlat(points, point_ids)
    else
      call index%init(points, point_ids)
    end if
  end subroutine grid_build_spatial_index

  !> \brief Closest cell ID for given coordinates.
  !> \return `integer(i8) :: closest_cell_id`
  !> \authors Sebastian Müller
  !> \date Mar 2026
  integer(i8) function grid_closest_cell_id_scalar(this, coords, use_aux) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(2) !< coordiantes (x,y) or (lon,lat)
    logical, intent(in), optional :: use_aux !< use auxilliar coordinates (lon,lat)
    logical :: aux

    aux = optval(use_aux, .false.)
    if (aux .and. .not. this%has_aux_coords()) call error_message("grid%closest_cell_id: no auxilliar coordniates defined.") ! LCOV_EXCL_LINE

    if (aux) then
      closest_cell_id = this%closest_cell_id_aux_scalar(coords)
    else
      closest_cell_id = this%closest_cell_id_regular_scalar(coords)
    end if
  end function grid_closest_cell_id_scalar

  !> \brief Closest cell IDs for given coordinates.
  !> \return `integer(i8) :: closest_cell_id(:)`
  !> \authors Sebastian Müller
  !> \date Mar 2026
  function grid_closest_cell_id_batch(this, coords, use_aux) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(:, :) !< coordiantes (x,y) or (lon,lat) per gauge
    logical, intent(in), optional :: use_aux !< use auxilliar coordinates (lon,lat)
    integer(i8) :: closest_cell_id(size(coords, 1))

    logical :: aux
    integer(i4) :: igauge
    type(spatial_index_t) :: aux_index

    if (size(coords, 2) /= 2_i4) call error_message("grid%closest_cell_id: coordinates need shape (:,2).") ! LCOV_EXCL_LINE

    aux = optval(use_aux, .false.)
    if (aux .and. .not. this%has_aux_coords()) call error_message("grid%closest_cell_id: no auxilliar coordniates defined.") ! LCOV_EXCL_LINE

    if (aux) then
      call this%build_spatial_index(aux_index, use_aux=.true.)
      closest_cell_id = aux_index%nearest_ids_lonlat(coords)
    else
      !$omp parallel do default(shared) private(igauge) schedule(static)
      do igauge = 1_i4, size(coords, 1)
        closest_cell_id(igauge) = this%closest_cell_id_regular_xy(coords(igauge, 1), coords(igauge, 2))
      end do
      !$omp end parallel do
    end if
  end function grid_closest_cell_id_batch

  integer(i8) function grid_closest_cell_id_aux_scalar(this, coords) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(2)

    closest_cell_id = this%closest_cell_id_aux_lonlat(coords(1), coords(2))
  end function grid_closest_cell_id_aux_scalar

  integer(i8) function grid_closest_cell_id_regular_scalar(this, coords) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(2)

    closest_cell_id = this%closest_cell_id_regular_xy(coords(1), coords(2))
  end function grid_closest_cell_id_regular_scalar

  integer(i8) function grid_closest_cell_id_aux_lonlat(this, lon_query, lat_query) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: lon_query
    real(dp), intent(in) :: lat_query

    integer(i8) :: k
    integer(i4) :: i, j
    real(dp) :: best_dist, cand_dist

    closest_cell_id = 0_i8
    if (this%ncells < 1_i8) return

    best_dist = huge(1.0_dp)
    do k = 1_i8, this%ncells
      i = this%cell_ij(k, 1)
      j = this%cell_ij(k, 2)
      cand_dist = dist_latlon(this%lat(i, j), this%lon(i, j), lat_query, lon_query)
      call update_best_metric(closest_cell_id, best_dist, k, cand_dist)
    end do
  end function grid_closest_cell_id_aux_lonlat

  integer(i8) function grid_closest_cell_id_regular_xy(this, x_query_raw, y_query) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: x_query_raw
    real(dp), intent(in) :: y_query

    logical :: is_spherical, is_periodic
    integer(i4) :: nearest_j, max_offset, row_offset, nrows, row_order(2), row_ids(2), irow, row_j, ncand, icand
    integer(i8) :: cand_ids(4), k_lb, k_ub
    real(dp) :: x_query, best_metric, row_metric(2), cand_metric

    closest_cell_id = 0_i8
    if (this%ncells < 1_i8) return

    is_spherical = this%coordsys == spherical
    is_periodic = is_spherical .and. this%is_periodic()

    x_query = x_query_raw
    if (is_spherical) x_query = this%normalize_longitude_near_domain(x_query)

    nearest_j = this%nearest_y_index(y_query)
    max_offset = max(nearest_j - 1_i4, this%ny - nearest_j)
    best_metric = huge(1.0_dp)

    search_rows: do row_offset = 0_i4, max_offset
      nrows = 0_i4

      if (nearest_j - row_offset >= 1_i4) then
        nrows = nrows + 1_i4
        row_ids(nrows) = nearest_j - row_offset
        row_metric(nrows) = this%row_lower_bound(row_ids(nrows), y_query, is_spherical)
      end if
      if (row_offset > 0_i4 .and. nearest_j + row_offset <= this%ny) then
        nrows = nrows + 1_i4
        row_ids(nrows) = nearest_j + row_offset
        row_metric(nrows) = this%row_lower_bound(row_ids(nrows), y_query, is_spherical)
      end if

      row_order(1) = 1_i4
      row_order(2) = 2_i4
      if (nrows == 2_i4) then
        if (row_metric(2) < row_metric(1) .or. (is_close(row_metric(2), row_metric(1)) .and. row_ids(2) < row_ids(1))) then
          row_order = [2_i4, 1_i4]
        end if
      end if

      do irow = 1_i4, nrows
        row_j = row_ids(row_order(irow))
        if (closest_cell_id > 0_i8) then
          if (row_metric(row_order(irow)) > best_metric .and. .not. is_close(row_metric(row_order(irow)), best_metric)) exit search_rows
        end if

        call this%row_k_bounds(row_j, k_lb, k_ub)
        if (k_lb > k_ub) cycle

        call this%row_candidate_ids(k_lb, k_ub, x_query, is_periodic, cand_ids, ncand)
        do icand = 1_i4, ncand
          if (is_spherical) then
            cand_metric = this%spherical_cell_metric(cand_ids(icand), x_query, y_query)
          else
            cand_metric = this%cartesian_cell_metric(cand_ids(icand), x_query, y_query)
          end if
          call update_best_metric(closest_cell_id, best_metric, cand_ids(icand), cand_metric)
        end do
      end do
    end do search_rows
  end function grid_closest_cell_id_regular_xy

  subroutine grid_row_k_bounds(this, j, k_lb, k_ub)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: j
    integer(i8), intent(out) :: k_lb
    integer(i8), intent(out) :: k_ub

    if (this%mask_col_cnt(j) < 1_i8) then
      k_lb = 1_i8
      k_ub = 0_i8
      return
    end if

    k_lb = this%mask_cum_col_cnt(j) + 1_i8
    k_ub = this%mask_cum_col_cnt(j) + this%mask_col_cnt(j)
  end subroutine grid_row_k_bounds

  subroutine grid_row_candidate_ids(this, k_lb, k_ub, x_query, periodic_wrap, cand_ids, ncand)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: k_lb
    integer(i8), intent(in) :: k_ub
    real(dp), intent(in) :: x_query
    logical, intent(in) :: periodic_wrap
    integer(i8), intent(out) :: cand_ids(4)
    integer(i4), intent(out) :: ncand

    real(dp) :: target_i
    integer(i8) :: left_k, right_k

    cand_ids = 0_i8
    ncand = 0_i4
    if (k_lb > k_ub) return

    target_i = (x_query - this%xllcorner) / this%cellsize + 0.5_dp
    call this%row_binary_search(k_lb, k_ub, target_i, left_k, right_k)

    call append_candidate_id(cand_ids, ncand, right_k, k_lb, k_ub)
    call append_candidate_id(cand_ids, ncand, left_k, k_lb, k_ub)
    if (periodic_wrap) then
      call append_candidate_id(cand_ids, ncand, k_lb, k_lb, k_ub)
      call append_candidate_id(cand_ids, ncand, k_ub, k_lb, k_ub)
    end if
  end subroutine grid_row_candidate_ids

  subroutine grid_row_binary_search(this, k_lb, k_ub, target_i, left_k, right_k)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: k_lb
    integer(i8), intent(in) :: k_ub
    real(dp), intent(in) :: target_i
    integer(i8), intent(out) :: left_k
    integer(i8), intent(out) :: right_k

    integer(i8) :: lo, hi, mid

    lo = k_lb
    hi = k_ub
    do while (lo <= hi)
      mid = lo + (hi - lo) / 2_i8
      if (real(this%cell_ij(mid, 1), dp) < target_i) then
        lo = mid + 1_i8
      else
        hi = mid - 1_i8
      end if
    end do

    left_k = hi
    right_k = lo
  end subroutine grid_row_binary_search

  pure integer(i4) function grid_nearest_y_index(this, y_raw) result(iy)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: y_raw

    select case (this%y_direction)
      case (bottom_up)
        iy = ceiling((y_raw - this%yllcorner) / this%cellsize, kind=i4)
      case (top_down)
        iy = this%ny - floor((y_raw - this%yllcorner) / this%cellsize, kind=i4)
      case default
        iy = 1_i4
    end select

    iy = max(1_i4, min(this%ny, iy))
  end function grid_nearest_y_index

  pure real(dp) function grid_row_lower_bound(this, j, y_query, is_spherical) result(metric)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: j
    real(dp), intent(in) :: y_query
    logical, intent(in) :: is_spherical

    real(dp) :: dy

    dy = abs(this%y_center(j) - y_query)
    if (is_spherical) then
      metric = RadiusEarth_dp * deg2rad_dp * dy
    else
      metric = dy * dy
    end if
  end function grid_row_lower_bound

  pure real(dp) function grid_cartesian_cell_metric(this, k, x_query, y_query) result(metric)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: k
    real(dp), intent(in) :: x_query
    real(dp), intent(in) :: y_query

    real(dp) :: dx, dy

    dx = this%x_center(this%cell_ij(k, 1)) - x_query
    dy = this%y_center(this%cell_ij(k, 2)) - y_query
    metric = dx * dx + dy * dy
  end function grid_cartesian_cell_metric

  pure real(dp) function grid_spherical_cell_metric(this, k, x_query, y_query) result(metric)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: k
    real(dp), intent(in) :: x_query
    real(dp), intent(in) :: y_query

    metric = dist_latlon(this%y_center(this%cell_ij(k, 2)), this%x_center(this%cell_ij(k, 1)), y_query, x_query)
  end function grid_spherical_cell_metric

  pure real(dp) function grid_x_center(this, i) result(x_center)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: i

    x_center = (real(i, dp) - 0.5_dp) * this%cellsize + this%xllcorner
  end function grid_x_center

  pure real(dp) function grid_y_center(this, j) result(y_center)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: j

    select case (this%y_direction)
      case (bottom_up)
        y_center = (real(j, dp) - 0.5_dp) * this%cellsize + this%yllcorner
      case (top_down)
        y_center = (real(this%ny - j + 1_i4, dp) - 0.5_dp) * this%cellsize + this%yllcorner
      case default
        y_center = (real(j, dp) - 0.5_dp) * this%cellsize + this%yllcorner
    end select
  end function grid_y_center

  pure real(dp) function grid_normalize_longitude_near_domain(this, x_raw) result(x_norm)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: x_raw

    real(dp) :: x_center

    x_center = this%xllcorner + 0.5_dp * real(this%nx, dp) * this%cellsize
    x_norm = modulo(x_raw - x_center + 180.0_dp, 360.0_dp) - 180.0_dp + x_center
  end function grid_normalize_longitude_near_domain

  pure real(dp) function grid_map_longitude_for_domain(this, x_raw) result(x_map)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: x_raw

    real(dp) :: x_delta, x_upper
    integer(i8) :: nturn

    x_map = this%normalize_longitude_near_domain(x_raw)
    if (this%is_periodic() .and. x_raw > this%xllcorner) then
      x_delta = x_raw - this%xllcorner
      x_upper = this%xllcorner + real(this%nx, dp) * this%cellsize
      nturn = nint(x_delta / 360.0_dp, kind=i8)
      if (nturn >= 1_i8 .and. is_close(x_delta, real(nturn, dp) * 360.0_dp)) then
        x_map = x_upper
      end if
    end if
  end function grid_map_longitude_for_domain

  !> \brief Check whether a point lies inside a specific cell.
  !> \details Uses regular axis-aligned cell bounds by default. With `aux=.true.`,
  !! the cell is interpreted as a quadrilateral from auxiliary lon/lat vertices.
  elemental logical function grid_in_cell(this, i, j, x, y, aux) result(in_cell)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: i
    integer(i4), intent(in) :: j
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    logical, intent(in), optional :: aux

    logical :: use_aux
    real(dp) :: cell_x(4), cell_y(4)
    real(dp) :: x_lower, x_upper, y_lower, y_upper, x_map

    use_aux = .false.
    if (present(aux)) use_aux = aux

    in_cell = .false.
    if (i < 1_i4 .or. i > this%nx .or. j < 1_i4 .or. j > this%ny) return

    if (use_aux) then
      if (.not. allocated(this%lat_vertices) .or. .not. allocated(this%lon_vertices)) return
      if (this%y_direction == bottom_up) then
        cell_x = [this%lon_vertices(i, j), this%lon_vertices(i + 1_i4, j), &
                  this%lon_vertices(i + 1_i4, j + 1_i4), this%lon_vertices(i, j + 1_i4)]
        cell_y = [this%lat_vertices(i, j), this%lat_vertices(i + 1_i4, j), &
                  this%lat_vertices(i + 1_i4, j + 1_i4), this%lat_vertices(i, j + 1_i4)]
      else
        cell_x = [this%lon_vertices(i, j + 1_i4), this%lon_vertices(i + 1_i4, j + 1_i4), &
                  this%lon_vertices(i + 1_i4, j), this%lon_vertices(i, j)]
        cell_y = [this%lat_vertices(i, j + 1_i4), this%lat_vertices(i + 1_i4, j + 1_i4), &
                  this%lat_vertices(i + 1_i4, j), this%lat_vertices(i, j)]
      end if
      cell_x = shift_longitude_near_query(cell_x, x)
      in_cell = quad_contains_point(cell_x, cell_y, x, y)
      return
    end if

    x_map = x
    if (this%coordsys == spherical) x_map = this%map_longitude_for_domain(x)

    x_lower = this%x_center(i) - 0.5_dp * this%cellsize
    x_upper = x_lower + this%cellsize
    y_lower = this%y_center(j) - 0.5_dp * this%cellsize
    y_upper = y_lower + this%cellsize

    in_cell = value_in_closed_interval(x_map, x_lower, x_upper) .and. &
              value_in_closed_interval(y, y_lower, y_upper)
  end function grid_in_cell

  !> \brief Closest cell ID for given coordinates from axis.
  !> \return `integer(i8) :: closest_cell_id(:)`
  !> \authors Sebastian Müller
  !> \date Feb 2026
  function grid_closest_cell_id_by_axes(this, coords) result(closest_cell_id)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(:,:) !< coordiantes (x,y) or (lon,lat) per gauge (shape: ngauges, 2)
    integer(i8) :: closest_cell_id(size(coords,1))
    integer(i4) :: igauge, ix, iy
    real(dp) :: x_raw, x_map, y_raw
    logical :: is_spherical
    ! integer(i8), allocatable :: cell_mat(:,:)
    ! allocate(cell_mat(this%nx, this%ny))
    ! call this%gen_id_matrix(cell_mat)

    is_spherical = this%coordsys == spherical

    !$omp parallel do default(shared) private(igauge,ix,iy,x_raw,x_map,y_raw) schedule(static)
    do igauge = 1_i4, size(coords,1)
      x_raw = coords(igauge,1)
      y_raw = coords(igauge,2)

      if (is_spherical) then
        x_map = this%map_longitude_for_domain(x_raw)
      else
        x_map = x_raw
      end if

      ix = ceiling((x_map - this%xllcorner) / this%cellsize, kind=i4)
      ix = min(max(ix, 1_i4), this%nx)

      iy = 1_i4
      select case (this%y_direction)
        case (bottom_up)
          iy = ceiling((y_raw - this%yllcorner) / this%cellsize, kind=i4)
        case (top_down)
          iy = this%ny - floor((y_raw - this%yllcorner) / this%cellsize, kind=i4)
        case default
          call error_message("grid % closest_cell_id_by_axes: y-direction not valid: ", trim(num2str(this%y_direction))) ! LCOV_EXCL_LINE
      end select
      iy = min(max(iy, 1_i4), this%ny)

      ! if the closest cell is masked, a nodata value is returned
      if (.not. this%mask(ix, iy)) then
        closest_cell_id(igauge) = nodata_i8
      else
        closest_cell_id(igauge) = this%mask_cum_col_cnt(iy) + count(this%mask(1:ix, iy), kind=i8)
      end if
      ! closest_cell_id(igauge) = cell_mat(ix, iy)
    end do
    !$omp end parallel do
  end function grid_closest_cell_id_by_axes

  !> \brief x-axis of the grid cell centers
  !> \return `real(dp), allocatable, dimension(:) :: x_axis`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_x_axis(this) result(x_axis)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: x_axis

    integer(i4) :: i

    x_axis = [ ((i-0.5_dp) * this%cellsize + this%xllcorner, i=1_i4, this%nx) ]

  end function grid_x_axis

  !> \brief y-axis of the grid cell centers
  !> \return `real(dp), allocatable, dimension(:) :: y_axis`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_y_axis(this, y_direction) result(y_axis)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:) :: y_axis

    integer(i4) :: i, y_dir

    y_dir = optval(y_direction, keep_y)
    if (y_dir == keep_y) y_dir = this%y_direction

    select case (y_dir)
      case (0)
        y_axis = [ ((i-0.5_dp) * this%cellsize + this%yllcorner, i=this%ny, 1_i4, -1_i4) ]
      case (1)
        y_axis = [ ((i-0.5_dp) * this%cellsize + this%yllcorner, i=1_i4, this%ny) ]
      case default
        call error_message("grid % y_axis: y-direction not valid: ", trim(num2str(y_dir))) ! LCOV_EXCL_LINE
    end select

  end function grid_y_axis

  !> \brief x-vertices of the grid cell edges
  !> \return `real(dp), allocatable, dimension(:) :: x_vertices`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_x_vertices(this) result(x_vertices)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: x_vertices

    integer(i4) :: i

    x_vertices = [ (i * this%cellsize + this%xllcorner, i=0_i4, this%nx) ]

  end function grid_x_vertices

  !> \brief y-vertices of the grid cell edges
  !> \return `real(dp), allocatable, dimension(:) :: y_vertices`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_y_vertices(this, y_direction) result(y_vertices)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:) :: y_vertices

    integer(i4) :: i, y_dir

    y_dir = optval(y_direction, keep_y)
    if (y_dir == keep_y) y_dir = this%y_direction

    select case (y_dir)
      case (0)
        y_vertices = [ (i * this%cellsize + this%yllcorner, i=this%ny, 0_i4, -1_i4) ]
      case (1)
        y_vertices = [ (i * this%cellsize + this%yllcorner, i=0_i4, this%ny) ]
      case default
        call error_message("grid % y_vertices: y-direction not valid: ", trim(num2str(y_dir))) ! LCOV_EXCL_LINE
    end select

  end function grid_y_vertices

  !> \brief x-bounds of the grid cell following cf-conventions (2, nx).
  !> \return `real(dp), allocatable, dimension(:,:) :: x_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_x_bounds(this) result(x_bounds)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:) :: x_bounds
    real(dp), allocatable, dimension(:) :: x_ax
    allocate(x_ax(this%nx+1_i4))
    x_ax = this%x_vertices()
    allocate(x_bounds(2, this%nx))
    x_bounds(1,:) = x_ax(1:this%nx)
    x_bounds(2,:) = x_ax(2:this%nx+1)
  end function grid_x_bounds

  !> \brief y-bounds of the grid cells following cf-conventions (2, ny).
  !> \return `real(dp), allocatable, dimension(:,:) :: y_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_y_bounds(this, y_direction) result(y_bounds)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:,:) :: y_bounds
    real(dp), allocatable, dimension(:) :: y_ax
    ! bounds follow axis direction
    allocate(y_ax(this%ny+1_i4))
    y_ax = this%y_vertices(y_direction)
    allocate(y_bounds(2, this%ny))
    y_bounds(1,:) = y_ax(1:this%ny)
    y_bounds(2,:) = y_ax(2:this%ny+1)
  end function grid_y_bounds

  !> \brief estimate auxilliar coordinates (lat, lon) from finer grid
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_upscale_aux_coords(this, fine_grid, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(in) :: fine_grid !< finer grid to estimate the auxilliar coordinates from
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: n_subcells
    integer(i4) :: i_ub, i_lb, j_lb, j_ub, i, j, factor
    call this%check_is_covering(fine_grid, tol=tol, check_mask=.false.)
    call check_factor(fine_grid%cellsize, this%cellsize, factor=factor, tol=tol)

    if (this % coordsys /= cartesian) &
      call error_message("grid % upscale_aux_coords: grids already use spherical coordinate system for axis.") ! LCOV_EXCL_LINE

    if (.not. fine_grid%has_aux_coords()) &
      call error_message("grid % upscale_aux_coords: fine grid has no auxilliar coordinates defined.") ! LCOV_EXCL_LINE

    if (this%has_aux_coords()) &
      call error_message("grid % upscale_aux_coords: grid already has auxilliar coordinates defined.") ! LCOV_EXCL_LINE

    allocate(this%lat(this%nx, this%ny))
    allocate(this%lon(this%nx, this%ny))
    do j = 1_i4, this%ny
      do i = 1_i4, this%nx
        ! coord. of all corners -> of finer scale
        call id_bounds(factor, i, j, &
          this%y_direction, this%ny, &
          fine_grid%y_direction, fine_grid%nx, fine_grid%ny, &
          i_lb, i_ub, j_lb, j_ub)
        n_subcells = real(size(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)), dp)
        ! estimate lat-lon coords by averaging sub-cell coordinates (even if fine grid is "a bit" smaller)
        this%lat(i, j) = sum(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)) / n_subcells
        this%lon(i, j) = sum(fine_grid%lon(i_lb:i_ub, j_lb:j_ub)) / n_subcells
      end do
    end do

  end subroutine grid_upscale_aux_coords

  !> \brief estimate auxilliar coordinates (lat, lon) from coarser grid
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_downscale_aux_coords(this, coarse_grid, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(inout) :: coarse_grid !< finer grid to estimate the auxilliar coordinates from
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: x, dx, fac
    integer(i4) :: i, j, ii, jj, i_start, i_end, j_start, j_end, k, factor, fac_i, fac_j
    logical :: reset_aux_vertices
    real(dp), allocatable, dimension(:,:) :: latlon_t, latlon_b, latlon_l, latlon_r

    call this%check_is_covered_by(coarse_grid, tol=tol, check_mask=.false.)
    call check_factor(this%cellsize, coarse_grid%cellsize, rounded=fac, factor=factor, tol=tol)

    if (this % coordsys /= cartesian) &
      call error_message("grid % downscale_aux_coords: grids already use spherical coordinate system for axis.") ! LCOV_EXCL_LINE

    if (.not. coarse_grid%has_aux_coords()) &
      call error_message("grid % downscale_aux_coords: fine grid has no auxilliar coordinates defined.") ! LCOV_EXCL_LINE

    if (this%has_aux_coords()) &
      call error_message("grid % downscale_aux_coords: grid already has auxilliar coordinates defined.") ! LCOV_EXCL_LINE

    if (coarse_grid%has_aux_vertices()) then
      reset_aux_vertices = .false.
    else
      reset_aux_vertices = .true.
      ! separate error message for single cell grid here?
      call coarse_grid%estimate_aux_vertices()
    end if

    allocate(latlon_t(factor, 2_i4))
    allocate(latlon_b(factor, 2_i4))
    allocate(latlon_l(factor, 2_i4))
    allocate(latlon_r(factor, 2_i4))

    allocate(this%lat(this%nx, this%ny))
    allocate(this%lon(this%nx, this%ny))
    do j = 1_i4, coarse_grid%ny
      do i = 1_i4, coarse_grid%nx
        ! calculate sub-points along vertice boundaries (top, left, right, bottom)
        ! lon
        x = coarse_grid%lon_vertices(i,j)
        dx = coarse_grid%lon_vertices(i+1,j) - x
        latlon_t(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i,j)
        dx = coarse_grid%lon_vertices(i,j+1) - x
        latlon_l(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i+1,j)
        dx = coarse_grid%lon_vertices(i+1,j+1) - x
        latlon_r(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i,j+1)
        dx = coarse_grid%lon_vertices(i+1,j+1) - x
        latlon_b(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        ! lat
        x = coarse_grid%lat_vertices(i,j)
        dx = coarse_grid%lat_vertices(i+1,j) - x
        latlon_t(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i,j)
        dx = coarse_grid%lat_vertices(i,j+1) - x
        latlon_l(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i+1,j)
        dx = coarse_grid%lat_vertices(i+1,j+1) - x
        latlon_r(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i,j+1)
        dx = coarse_grid%lat_vertices(i+1,j+1) - x
        latlon_b(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        ! generate latlon pairs in sub-cells
        call id_bounds(factor, i, j, &
          coarse_grid%y_direction, coarse_grid%ny, &
          this%y_direction, this%nx, this%ny, &
          i_start, i_end, j_start, j_end)
        fac_i = i_end - i_start + 1_i4
        fac_j = j_end - j_start + 1_i4

        do jj = 1_i4, fac_j
          do ii = 1_i4, fac_i
            call intersection( &
              latlon_t(ii,1), latlon_t(ii,2), latlon_b(ii,1), latlon_b(ii,2), &
              latlon_l(jj,1), latlon_l(jj,2), latlon_r(jj,1), latlon_r(jj,2), &
              this%lon(i_start+ii-1,j_start+jj-1), this%lat(i_start+ii-1,j_start+jj-1))
          end do
        end do

      end do
    end do

    if ( reset_aux_vertices ) then
      deallocate(coarse_grid%lat_vertices)
      deallocate(coarse_grid%lon_vertices)
    end if

  end subroutine grid_downscale_aux_coords

  !> \brief estimate vertices of auxilliar coordinate cells
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_estimate_aux_vertices(this)
    implicit none
    class(grid_t), intent(inout) :: this
    integer(i4) :: i, j

    if (.not. this%has_aux_coords()) &
      call error_message("grid % estimate_aux_vertices: grid has no auxilliar coordinates defined.") ! LCOV_EXCL_LINE

    if (this%has_aux_vertices()) &
      call error_message("grid % estimate_aux_vertices: grid already has auxilliar vertices defined.") ! LCOV_EXCL_LINE

    if (this%nx == 1_i4 .or. this%ny == 1_i4) &
      call error_message("grid % estimate_aux_vertices: grid needs at least 2 cells in both directions.") ! LCOV_EXCL_LINE

    allocate(this%lat_vertices(this%nx+1_i4, this%ny+1_i4))
    allocate(this%lon_vertices(this%nx+1_i4, this%ny+1_i4))
    ! first calculate the inner vertices as mean of surrounding cell centers
    do j = 2, this%ny
      do i = 2, this%nx
        this%lat_vertices(i, j) = sum(this%lat(i-1:i, j-1:j)) / 4.0_dp
        this%lon_vertices(i, j) = sum(this%lon(i-1:i, j-1:j)) / 4.0_dp
      end do
    end do
    ! these formulas seem off, but are correct: extrapolate from next inner point on line with mean of two connected cell centers.
    do j = 2, this%ny
      ! left side
      this%lat_vertices(1, j) = sum(this%lat(1, j-1:j)) - this%lat_vertices(2, j)
      this%lon_vertices(1, j) = sum(this%lon(1, j-1:j)) - this%lon_vertices(2, j)
      ! right side
      this%lat_vertices(this%nx+1, j) = sum(this%lat(this%nx, j-1:j)) - this%lat_vertices(this%nx, j)
      this%lon_vertices(this%nx+1, j) = sum(this%lon(this%nx, j-1:j)) - this%lon_vertices(this%nx, j)
    end do
    do i = 2, this%nx
      ! upper side
      this%lat_vertices(i, 1) = sum(this%lat(i-1:i, 1)) - this%lat_vertices(i, 2)
      this%lon_vertices(i, 1) = sum(this%lon(i-1:i, 1)) - this%lon_vertices(i, 2)
      ! lower side
      this%lat_vertices(i, this%ny+1) = sum(this%lat(i-1:i, this%ny)) - this%lat_vertices(i, this%ny)
      this%lon_vertices(i, this%ny+1) = sum(this%lon(i-1:i, this%ny)) - this%lon_vertices(i, this%ny)
    end do
    ! lower left corner
    this%lat_vertices(1, 1) = this%lat_vertices(2, 1) ! from right neighbor
    this%lon_vertices(1, 1) = this%lon_vertices(1, 2) ! from upper neighbor
    ! upper left corner
    this%lat_vertices(1, this%ny+1) = this%lat_vertices(2, this%ny+1) ! from right neighbor
    this%lon_vertices(1, this%ny+1) = this%lon_vertices(1, this%ny) ! from lower neighbor
    ! lower right corner
    this%lat_vertices(this%nx+1, 1) = this%lat_vertices(this%nx, 1) ! from left neighbor
    this%lon_vertices(this%nx+1, 1) = this%lon_vertices(this%nx+1, 2) ! from upper neighbor
    ! upper right corner
    this%lat_vertices(this%nx+1, this%ny+1) = this%lat_vertices(this%nx, this%ny+1) ! from left neighbor
    this%lon_vertices(this%nx+1, this%ny+1) = this%lon_vertices(this%nx+1, this%ny) ! from lower neighbor

  end subroutine grid_estimate_aux_vertices

  !> \brief lat-bounds of the grid cell following cf-conventions (4, nx, ny).
  !> \return `real(dp), allocatable, dimension(:,:,:) :: lat_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_lat_bounds(this) result(lat_bounds)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lat_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lat_bounds: grid has no auxilliar vertices defined.") ! LCOV_EXCL_LINE
    allocate(lat_bounds(4, this%nx, this%ny))
    if (this%y_direction == bottom_up) then
      ! lower-left corner of the cells
      lat_bounds(1,:,:) = this%lat_vertices(1:this%nx, 1:this%ny)
      ! lower-right corner of the cells
      lat_bounds(2,:,:) = this%lat_vertices(2:this%nx+1, 1:this%ny)
      ! upper-right corner of the cells
      lat_bounds(3,:,:) = this%lat_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-left corner of the cells
      lat_bounds(4,:,:) = this%lat_vertices(1:this%nx, 2:this%ny+1)
    else
      ! lower-left corner of the cells
      lat_bounds(1,:,:) = this%lat_vertices(1:this%nx, 2:this%ny+1)
      ! lower-right corner of the cells
      lat_bounds(2,:,:) = this%lat_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-right corner of the cells
      lat_bounds(3,:,:) = this%lat_vertices(2:this%nx+1, 1:this%ny)
      ! upper-left corner of the cells
      lat_bounds(4,:,:) = this%lat_vertices(1:this%nx, 1:this%ny)
    end if
  end function grid_lat_bounds

  !> \brief lon-bounds of the grid cell following cf-conventions (4, nx, ny).
  !> \return `real(dp), allocatable, dimension(:,:,:) :: lon_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function grid_lon_bounds(this) result(lon_bounds)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lon_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lon_bounds: grid has no auxilliar vertices defined.") ! LCOV_EXCL_LINE
    allocate(lon_bounds(4, this%nx, this%ny))
    if (this%y_direction == bottom_up) then
      ! lower-left corner of the cells
      lon_bounds(1,:,:) = this%lon_vertices(1:this%nx, 1:this%ny)
      ! lower-right corner of the cells
      lon_bounds(2,:,:) = this%lon_vertices(2:this%nx+1, 1:this%ny)
      ! upper-right corner of the cells
      lon_bounds(3,:,:) = this%lon_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-left corner of the cells
      lon_bounds(4,:,:) = this%lon_vertices(1:this%nx, 2:this%ny+1)
    else
      ! lower-left corner of the cells
      lon_bounds(1,:,:) = this%lon_vertices(1:this%nx, 2:this%ny+1)
      ! lower-right corner of the cells
      lon_bounds(2,:,:) = this%lon_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-right corner of the cells
      lon_bounds(3,:,:) = this%lon_vertices(2:this%nx+1, 1:this%ny)
      ! upper-left corner of the cells
      lon_bounds(4,:,:) = this%lon_vertices(1:this%nx, 1:this%ny)
    end if
  end function grid_lon_bounds

  !> \brief check if given grid has an allocated mask
  !> \return `logical :: has_mask`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function grid_has_mask(this) result(has_mask)
    implicit none
    class(grid_t), intent(in) :: this
    has_mask = allocated(this%mask)
  end function grid_has_mask

  !> \brief check if given grid has any missing cells (mask allocated and any value .false.)
  !> \return `logical :: any_missing`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function grid_any_missing(this) result(any_missing)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4) :: j
    any_missing = .false.
    if (.not. this%has_mask()) return
    !$omp parallel do default(shared) schedule(static)
    do j = 1_i4, this%ny
      !$omp flush(any_missing)
      if (any_missing) cycle ! no work for rest of the loop (exit not safe with omp)
      if (.not. all(this%mask(:, j))) then
        !$omp atomic write
        any_missing = .true.
      end if
    end do
    !$omp end parallel do
  end function grid_any_missing

  !> \brief check if given grid matches another grid
  !> \details Returns `.true.` if the structural grid metadata matches,
  !!          the mask matches, and optionally the auxiliary coordinates
  !!          and vertices match as well.
  !> \return `logical :: is_matching`
  !> \authors Sebastian Mueller
  !> \date Mar 2026
  logical function grid_is_matching(this, other, tol, aux) result(is_matching)
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: other !< grid to compare with
    real(dp), optional, intent(in) :: tol !< tolerance for comparisson of real values (default: 1.e-7)
    logical, optional, intent(in) :: aux !< whether to check if auxilliar coordinates and vertices match as well (default: .false.)

    real(dp) :: tol_
    logical :: check_aux

    is_matching = .false.
    tol_ = optval(tol, 1.0e-7_dp)
    check_aux = optval(aux, .false.)

    if (this%ncells /= other%ncells) return
    if (this%nx /= other%nx) return
    if (this%ny /= other%ny) return
    if (this%coordsys /= other%coordsys) return
    if (this%y_direction /= other%y_direction) return

    if (.not. is_close(this%xllcorner, other%xllcorner, tol_, tol_)) return
    if (.not. is_close(this%yllcorner, other%yllcorner, tol_, tol_)) return
    if (.not. is_close(this%cellsize, other%cellsize, tol_, tol_)) return

    if (check_aux) then
      if (allocated(this%lat) .neqv. allocated(other%lat)) return
      if (allocated(this%lon) .neqv. allocated(other%lon)) return
      if (allocated(this%lat_vertices) .neqv. allocated(other%lat_vertices)) return
      if (allocated(this%lon_vertices) .neqv. allocated(other%lon_vertices)) return

      if (allocated(this%lat)) then
        if (.not. arrays_match_2d(this%lat, other%lat, tol=tol_)) return
      end if
      if (allocated(this%lon)) then
        if (.not. arrays_match_2d(this%lon, other%lon, tol=tol_)) return
      end if
      if (allocated(this%lat_vertices)) then
        if (.not. arrays_match_2d(this%lat_vertices, other%lat_vertices, tol=tol_)) return
      end if
      if (allocated(this%lon_vertices)) then
        if (.not. arrays_match_2d(this%lon_vertices, other%lon_vertices, tol=tol_)) return
      end if
    end if

    if (allocated(this%mask) .neqv. allocated(other%mask)) return
    if (allocated(this%mask)) then
      if (.not. arrays_match_2d(this%mask, other%mask)) return
    end if

    is_matching = .true.
  end function grid_is_matching

  !> \brief check if given grid is covered by coarser grid
  !> \details check if given grid is compatible and covered by coarser grid and raise an error if this is not the case.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_check_is_covered_by(this, coarse_grid, tol, check_mask)
    use mo_utils, only: is_close
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: coarse_grid !< coarse grid that should cover this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask

    integer(i4) :: factor, i, j, i_lb, i_ub, j_lb, j_ub
    logical :: check_mask_
    real(dp) :: tol_

    check_mask_ = optval(check_mask, .true.)

    ! LCOV_EXCL_START
    if (this%coordsys /= coarse_grid%coordsys) then
      call error_message("grid % check_is_covered_by: grids don't use the same coordinate system.")
    end if
    ! LCOV_EXCL_STOP

    call check_factor(this%cellsize, coarse_grid%cellsize, factor=factor, tol=tol)

    ! LCOV_EXCL_START
    ! check ll corner (absolute and relative tolerance set to "tol")
    tol_ = optval(tol, 1.e-7_dp)
    if ( .not. ( is_close(this%xllcorner, coarse_grid%xllcorner, tol_, tol_) &
         .and.   is_close(this%yllcorner, coarse_grid%yllcorner, tol_, tol_) ) ) then
      call error_message("grid % check_is_covered_by: coarse grid lower-left corner is not matching.")
    end if
    ! check extent
    if (.not. ((coarse_grid%nx - 1) * factor <= this%nx .and. this%nx <= coarse_grid%nx * factor .and. &
               (coarse_grid%ny - 1) * factor <= this%ny .and. this%ny <= coarse_grid%ny * factor)) then
      call error_message("grid % check_is_covered_by: coarse grid extent is not matching.")
    end if
    ! LCOV_EXCL_STOP

    ! LCOV_EXCL_START
    if ( check_mask_ .and. coarse_grid%any_missing()) then
      if (.not. this%any_missing()) call error_message("grid % check_is_covered_by: coarse grid is masked, this grid not.") ! LCOV_EXCL_LINE
      !$omp parallel do default(shared) private(i,j,i_lb,i_ub,j_lb,j_ub) schedule(static)
      do j = 1_i4, coarse_grid%ny
        do i = 1_i4, coarse_grid%nx
          if ( coarse_grid%mask(i, j)) cycle
          call id_bounds(factor, i, j, &
            coarse_grid%y_direction, coarse_grid%ny, &
            this%y_direction, this%nx, this%ny, &
            i_lb, i_ub, j_lb, j_ub)
          if (any(this%mask(i_lb:i_ub, j_lb:j_ub))) &
            call error_message("grid % check_is_covered_by: fine cells outside of coarse mask.")
        end do
      end do
      !$omp end parallel do
    end if
    ! LCOV_EXCL_STOP
  end subroutine grid_check_is_covered_by

  !> \brief check if given grid is covering finer grid
  !> \details check if given grid is compatible with and covering finer grid and raise an error if this is not the case.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_check_is_covering(this, fine_grid, tol, check_mask)
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: fine_grid !< finer grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask
    call fine_grid%check_is_covered_by(coarse_grid=this, tol=tol, check_mask=check_mask)
  end subroutine grid_check_is_covering

  !> \brief check if given grid is filled by fine grid
  !> \details check if given grid is compatible and filled by finer grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_check_is_filled_by(this, fine_grid, tol, check_mask)
    use mo_utils, only: is_close
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: fine_grid !< fine grid that should fill this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask

    integer(i4) :: factor, i_lb, i_ub, j_lb, j_ub
    integer(i8) :: k
    logical :: check_mask_
    real(dp) :: tol_

    check_mask_ = optval(check_mask, .true.)

    ! LCOV_EXCL_START
    if (this%coordsys /= fine_grid%coordsys) then
      call error_message("grid % check_is_filled_by: grids don't use the same coordinate system.")
    end if
    ! LCOV_EXCL_STOP

    call check_factor(fine_grid%cellsize, this%cellsize, factor=factor, tol=tol)

    ! LCOV_EXCL_START
    ! check ll corner (absolute and relative tolerance set to "tol")
    tol_ = optval(tol, 1.e-7_dp)
    if ( .not. ( is_close(this%xllcorner, fine_grid%xllcorner, tol_, tol_) &
         .and.   is_close(this%yllcorner, fine_grid%yllcorner, tol_, tol_) ) ) then
      call error_message("grid % check_is_filled_by: fine grid lower-left corner is not matching.")
    end if
    ! check extent
    if (.not. ((this%nx - 1) * factor <= fine_grid%nx .and. fine_grid%nx <= this%nx * factor .and. &
               (this%ny - 1) * factor <= fine_grid%ny .and. fine_grid%ny <= this%ny * factor)) then
      call error_message("grid % check_is_filled_by: fine grid extent is not matching.")
    end if
    ! LCOV_EXCL_STOP

    ! LCOV_EXCL_START
    if ( check_mask_ .and. fine_grid%any_missing()) then
      !$omp parallel do default(shared) private(i_lb,i_ub,j_lb,j_ub) schedule(static)
      do k = 1_i8, this%ncells
        call id_bounds(factor, this%cell_ij(k,1), this%cell_ij(k,2), &
          this%y_direction, this%ny, &
          fine_grid%y_direction, fine_grid%nx, fine_grid%ny, &
          i_lb, i_ub, j_lb, j_ub)
        if (.not.any(fine_grid%mask(i_lb:i_ub, j_lb:j_ub))) then
          call error_message("grid % check_is_filled_by: coarse cells without any filling fine cells found.")
        end if
      end do
      !$omp end parallel do
    end if
    ! LCOV_EXCL_STOP
  end subroutine grid_check_is_filled_by

  !> \brief check if given grid is filling coarser grid
  !> \details check if given grid is compatible with and filling coarser grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_check_is_filling(this, coarse_grid, tol, check_mask)
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: coarse_grid !< coarser grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask
    call coarse_grid%check_is_filled_by(fine_grid=this, tol=tol, check_mask=check_mask)
  end subroutine grid_check_is_filling

  !> \brief check if given grid has auxilliar coordinates allocated.
  !> \return `logical :: has_aux_coords`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  pure logical function grid_has_aux_coords(this) result(has_aux_coords)
    implicit none
    class(grid_t), intent(in) :: this
    has_aux_coords = allocated(this%lat) .and. allocated(this%lon)
  end function grid_has_aux_coords

  !> \brief check if given grid has auxilliar vertices allocated.
  !> \return `logical :: has_aux_vertices`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  pure logical function grid_has_aux_vertices(this) result(has_aux_vertices)
    implicit none
    class(grid_t), intent(in) :: this
    has_aux_vertices = allocated(this%lat_vertices) .and. allocated(this%lon_vertices)
  end function grid_has_aux_vertices

  ! ------------------------------------------------------------------

  !> \brief   calculate grid properties regarding cell ids
  !> \details following tasks are performed:
  !!          -  cell ids & numbering
  !!          -  storage of cell cordinates (row and coloum id)
  !> \authors Rohini Kumar
  !> \date Jan 2013
  !> \changelog
  !! - Rohini Kumar & Matthias Cuntz, May 2014
  !!   - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !! - Matthias Cuntz, May 2014
  !!   - changed empirical distribution function so that doubles get the same value
  !! - Matthias Zink & Matthias Cuntz, Feb 2016
  !!   - code speed up due to reformulation of CDF calculation
  !! - Rohini Kumar, Mar 2016
  !!   - changes for handling multiple soil database options
  !! - Robert Schweppe, Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Müller, Mar 2024
  !!   - moved to FORCES
  subroutine grid_calculate_cell_ids(this)
    use mo_utils, only: prefix_sum
    implicit none
    class(grid_t), intent(inout) :: this

    integer(i4) :: i, j
    integer(i8) :: k

    ! if mask not allocated create one with only .true. values
    if (.not. allocated(this%mask)) then
      allocate(this%mask(this%nx, this%ny))
      !$omp parallel do default(shared) schedule(static)
      do j = 1_i4, this%ny
        this%mask(:,j) = .true.
      end do
      !$omp end parallel do
    end if

    ! allocate arrays for mask column counts and cumulative counts
    if (.not. allocated(this%mask_col_cnt)) allocate(this%mask_col_cnt(this%ny))
    if (.not. allocated(this%mask_cum_col_cnt)) allocate(this%mask_cum_col_cnt(this%ny))

    !$omp parallel do default(shared) schedule(static)
    do j = 1_i4, this%ny
      this%mask_col_cnt(j) = count(this%mask(:,j), kind=i8)
    end do
    !$omp end parallel do

    call prefix_sum(this%mask_col_cnt, this%mask_cum_col_cnt, shift=1_i8, start=0_i8)
    this%ncells = this%mask_cum_col_cnt(this%ny) + this%mask_col_cnt(this%ny)

    if (allocated(this%cell_ij)) deallocate(this%cell_ij)
    allocate(this%cell_ij(this%ncells, 2))

    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i, j)) cycle
        k = k + 1_i8
        this%cell_ij(k, 1) = i
        this%cell_ij(k, 2) = j
      end do
    end do
    !$omp end parallel do

  end subroutine grid_calculate_cell_ids

  ! ------------------------------------------------------------------

  !> \brief estimate cell area
  !> \details following tasks are performed:
  !!          - estimate cell area based on coordinate system
  !> \authors Rohini Kumar
  !> \date Jan 2013
  !> \changelog
  !! - Rohini Kumar & Matthias Cuntz, May 2014
  !!   - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !! - Matthias Cuntz, May 2014
  !!   - changed empirical distribution function so that doubles get the same value
  !! - Matthias Zink & Matthias Cuntz, Feb 2016
  !!   - code speed up due to reformulation of CDF calculation
  !! - Rohini Kumar, Mar 2016
  !!   - changes for handling multiple soil database options
  !! - Robert Schweppe, Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Müller, Mar 2024
  !!   - moved to FORCES
  subroutine grid_calculate_cell_area(this)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp) :: factor, cell_size_rad
    integer(i8) :: k
    real(dp), allocatable :: cell_area_lat(:)

    if (allocated(this%cell_area)) deallocate(this%cell_area)
    allocate(this%cell_area(this%ncells))

    ! regular X-Y coordinate system
    if(this%coordsys .eq. cartesian) then
      this%cell_area(:) = this%cellsize * this%cellsize

    ! regular lat-lon coordinate system
    else if(this%coordsys .eq. spherical) then

      ! A = R ** 2 * dx * (sin(lat1) - sin(lon2))
      !   = R ** 2 * dx * cos([lat1 + lat2] / 2) * sin(dy / 2) * 2
      !
      ! A ~ R ** 2 * dx * cos(cell_center_lat) * dy                (approx. since: sin x = x for small x)

      cell_size_rad = this%cellsize * deg2rad_dp
      factor = (RadiusEarth_dp * cell_size_rad) * (RadiusEarth_dp * sin(cell_size_rad / 2.0_dp) * 2.0_dp)
      allocate(cell_area_lat(this%ny), source=cos(this%y_axis() * deg2rad_dp) * factor)

      !$omp parallel do default(shared) schedule(static)
      do k = 1_i8, this%ncells
        this%cell_area(k) = cell_area_lat(this%cell_ij(k, 2))
      end do
      !$omp end parallel do

    else
      call error_message("estimate_cell_area: unknown coordsys value: ", num2str(this%coordsys)) ! LCOV_EXCL_LINE
    end if

  end subroutine grid_calculate_cell_area

  !> \brief check if given grid is a global lat-lon grid with periodic lon axis
  !> \return `logical :: is_periodic`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  pure logical function grid_is_periodic(this) result(is_periodic)
    implicit none
    class(grid_t), intent(in) :: this
    if (this%coordsys == cartesian) then
      is_periodic = .false.
    else
      is_periodic = is_close(360.0_dp, this%nx * this%cellsize)
    endif
  end function grid_is_periodic

  !> \brief Generate coarse grid from a fine grid by a given target resolution
  subroutine grid_gen_coarse_grid(this, coarse_grid, target_resolution, estimate_aux, estimate_area, area_method, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(out) :: coarse_grid !< resulting low resolution grid
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    !> method to estimate area: (0, default) from fine grid, (1) from cell extent, (2) from count fraction of valid fine cells
    integer(i4), intent(in), optional :: area_method
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    real(dp), dimension(:, :), allocatable :: fine_cell_area
    integer(i4) :: i_ub, i_lb, j_lb, j_ub
    integer(i4) :: i, j
    integer(i8) :: k
    integer(i4) :: factor, area_method_
    logical :: estimate_aux_, estimate_area_
    real(dp) :: sub_cell_cnt

    estimate_aux_ = optval(estimate_aux, .true.)
    estimate_area_ = optval(estimate_area, .true.)
    area_method_ = optval(area_method, area_sum)

    !--------------------------------------------------------
    ! 1) Estimate each variable locally for a given domain
    ! 2) Pad each variable to its corresponding global one
    !--------------------------------------------------------
    ! grid properties
    call calculate_coarse_extent( &
      nx_in=this%nx, &
      ny_in=this%ny, &
      xllcorner_in=this%xllcorner, &
      yllcorner_in=this%yllcorner, &
      cellsize_in=this%cellsize, &
      target_resolution=target_resolution, &
      nx_out=coarse_grid%nx, &
      ny_out=coarse_grid%ny, &
      xllcorner_out=coarse_grid%xllcorner, &
      yllcorner_out=coarse_grid%yllcorner, &
      cellsize_out=coarse_grid%cellsize, &
      tol=tol &
    )

    coarse_grid%coordsys = this%coordsys
    coarse_grid%y_direction = this%y_direction

    factor = nint(coarse_grid%cellsize / this%cellsize, i4)

    if (this%is_periodic().and.(.not.coarse_grid%is_periodic())) &
      call error_message("derive_coarse_grid: target resolution is not suitable for a periodic grid.") ! LCOV_EXCL_LINE

    ! create mask at coarse grid
    allocate(coarse_grid%mask(coarse_grid%nx, coarse_grid%ny))

    !$omp parallel do default(shared) private(i,j,i_lb,i_ub,j_lb,j_ub) schedule(static)
    do j = 1_i4, coarse_grid%ny
      do i = 1_i4, coarse_grid%nx
        call id_bounds(factor, i, j, &
          coarse_grid%y_direction, coarse_grid%ny, &
          this%y_direction, this%nx, this%ny, &
          i_lb, i_ub, j_lb, j_ub)
        coarse_grid%mask(i, j) = any(this%mask(i_lb : i_ub, j_lb : j_ub))
      end do
    end do
    !$omp end parallel do

    call coarse_grid%calculate_cell_ids()

    if (estimate_area_) then
      select case(area_method_)
        case(area_sum)
          ! lowres additional properties
          allocate(fine_cell_area(this%nx, this%ny))
          call this%unpack_into(this%cell_area, fine_cell_area)
          allocate(coarse_grid%cell_area(coarse_grid%ncells))
          !$omp parallel do default(shared) private(i,j,i_lb,i_ub,j_lb,j_ub) schedule(static)
          do k = 1_i8, coarse_grid%ncells
            i = coarse_grid%cell_ij(k, 1)
            j = coarse_grid%cell_ij(k, 2)
            call id_bounds(factor, i, j, &
              coarse_grid%y_direction, coarse_grid%ny, this%y_direction, this%nx, this%ny, &
              i_lb, i_ub, j_lb, j_ub)
            coarse_grid%cell_area(k) = sum(fine_cell_area(i_lb : i_ub, j_lb : j_ub), this%mask(i_lb : i_ub, j_lb : j_ub))
          end do
          !$omp end parallel do
          ! free space
          deallocate(fine_cell_area)
        case(area_full)
          call coarse_grid%calculate_cell_area()
        case(area_count)
          call coarse_grid%calculate_cell_area()
          sub_cell_cnt = real(factor * factor, dp)
          !$omp parallel do default(shared) private(i,j,i_lb,i_ub,j_lb,j_ub) schedule(static)
          do k = 1_i8, coarse_grid%ncells
            i = coarse_grid%cell_ij(k, 1)
            j = coarse_grid%cell_ij(k, 2)
            call id_bounds(factor, i, j, &
              coarse_grid%y_direction, coarse_grid%ny, &
              this%y_direction, this%nx, this%ny, &
              i_lb, i_ub, j_lb, j_ub)
            coarse_grid%cell_area(k) = coarse_grid%cell_area(k) * (real(count(this%mask(i_lb:i_ub, j_lb:j_ub)), dp) / sub_cell_cnt)
          end do
          !$omp end parallel do
        case default
          call error_message("derive_coarse_grid: 'area_method' needs to be 0, 1 or 2. Got: ", trim(num2str(area_method_))) ! LCOV_EXCL_LINE
      end select
    end if

    ! only estimate aux coords if we are on a projected grid
    if ( estimate_aux_ .and. this%coordsys == cartesian .and. this%has_aux_coords()) then
      call coarse_grid%upscale_aux_coords(this, tol=tol)
    end if

  end subroutine grid_gen_coarse_grid

  !> \brief Generate coarse grid from a fine grid by a given target resolution
  !> \return `type(grid_t) :: coarse_grid`
  function grid_derive_coarse_grid(this, target_resolution, estimate_aux, estimate_area, area_method, tol) result(coarse_grid)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    !> method to estimate area: (0, default) from fine grid, (1) from cell extent, (2) from count fraction of valid fine cells
    integer(i4), intent(in), optional :: area_method
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: coarse_grid !< resulting low resolution grid
    call this%gen_coarse_grid(coarse_grid, target_resolution, estimate_aux, estimate_area, area_method, tol)
  end function grid_derive_coarse_grid

  !> \brief Generate fine grid from a coarse grid by a given target resolution
  subroutine grid_gen_fine_grid(this, fine_grid, target_resolution, estimate_aux, estimate_area, area_method, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(out) :: fine_grid !< resulting high resolution grid
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from fine grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    integer(i4) :: i, j, ic, jc
    integer(i4) :: factor, area_method_
    logical :: estimate_aux_, estimate_area_

    estimate_aux_ = optval(estimate_aux, .true.)
    estimate_area_ = optval(estimate_area, .true.)
    area_method_ = optval(area_method, area_sum)

    call check_factor(target_resolution, this%cellsize, factor=factor, tol=tol)

    fine_grid%nx = this%nx * factor
    fine_grid%ny = this%ny * factor
    fine_grid%cellsize = target_resolution
    fine_grid%xllcorner = this%xllcorner
    fine_grid%yllcorner = this%yllcorner
    fine_grid%coordsys = this%coordsys
    fine_grid%y_direction = this%y_direction

    ! create mask at fine grid
    allocate(fine_grid%mask(fine_grid%nx, fine_grid%ny), source=.false.)
    do j = 1_i4, this%ny
      ! everything would be better with 0-based ids
      jc = (j-1_i4) * factor + 1_i4
      do i = 1_i4, this%nx
        if (.not. this%mask(i, j)) cycle
        ic = (i-1_i4) * factor + 1_i4
        fine_grid%mask(ic : ic + factor - 1_i4, jc :  jc + factor - 1_i4) = .true.
      end do
    end do

    call fine_grid%calculate_cell_ids()

    ! This doesn't preserve the total area if the coarse cells were fractioned.
    if (estimate_area_) call fine_grid%calculate_cell_area()

    ! only estimate aux coords if we are on a projected grid
    if ( estimate_aux_ .and. this%coordsys == cartesian .and. this%has_aux_coords()) then
      call fine_grid%downscale_aux_coords(this, tol=tol)
    end if

  end subroutine grid_gen_fine_grid

  !> \brief Generate fine grid from a coarse grid by a given target resolution
  !> \return `type(grid_t) :: fine_grid`
  function grid_derive_fine_grid(this, target_resolution, estimate_aux, estimate_area, area_method, tol) result(fine_grid)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from fine grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: fine_grid !< resulting high resolution grid
    call this%gen_fine_grid(fine_grid, target_resolution, estimate_aux, estimate_area, area_method, tol)
  end function grid_derive_fine_grid

  !> \brief Generate a derived grid by a given target resolution
  !> \return `type(grid_t) :: result_grid`
  subroutine grid_gen_grid(this, grid, target_resolution, downscaling_factor, upscaling_factor, estimate_aux, estimate_area, area_method, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(out) :: grid !< resulting grid
    real(dp), intent(in), optional :: target_resolution !< desired target resolution
    integer(i4), intent(in), optional :: downscaling_factor !< factor for finer grid
    integer(i4), intent(in), optional :: upscaling_factor !< factor for coarser grid
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate cell areas (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from other grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: resolution
    integer(i4) :: input_opt

    input_opt = 0_i4
    if (present(target_resolution)) then
      input_opt = input_opt + 1
      resolution = target_resolution
    end if
    if (present(downscaling_factor)) then
      input_opt = input_opt + 1
      resolution = this%cellsize / real(downscaling_factor, dp)
    end if
    if (present(upscaling_factor)) then
      input_opt = input_opt + 1
      resolution = this%cellsize * real(upscaling_factor, dp)
    end if
    ! LCOV_EXCL_START
    if (input_opt /= 1) then
      call error_message("derive_grid: only one of 'target_resolution', 'factor_up' or 'factor_down' can be given.")
    end if
    ! LCOV_EXCL_STOP

    if (resolution < this%cellsize) then
      call this%gen_fine_grid(grid, resolution, estimate_aux, estimate_area, area_method, tol)
    else
      call this%gen_coarse_grid(grid, resolution, estimate_aux, estimate_area, area_method, tol)
    end if

  end subroutine grid_gen_grid

  !> \brief Generate a derived grid by a given target resolution
  !> \return `type(grid_t) :: result_grid`
  function grid_derive_grid(this, target_resolution, downscaling_factor, upscaling_factor, estimate_aux, estimate_area, area_method, tol) result(derive_grid)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in), optional :: target_resolution !< desired target resolution
    integer(i4), intent(in), optional :: downscaling_factor !< factor for finer grid
    integer(i4), intent(in), optional :: upscaling_factor !< factor for coarser grid
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate cell areas (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from other grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: derive_grid !< resulting grid
    call this%gen_grid(derive_grid, target_resolution, downscaling_factor, upscaling_factor, estimate_aux, estimate_area, area_method, tol)
  end function grid_derive_grid

  !> \brief Check 2D data shape for grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine grid_check_shape(this, data_shape)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data_shape(2) !< (x,y)
    ! LCOV_EXCL_START
    if (data_shape(1) /= this%nx .or. data_shape(2) /= this%ny) then
      call error_message( &
        "grid data: data has wrong shape. Expected: (", &
        num2str(this%nx), ",", num2str(this%ny), "), got: (", &
        num2str(data_shape(1)), ",", num2str(data_shape(2)), ")")
    end if
    ! LCOV_EXCL_STOP
  end subroutine grid_check_shape

  !> \brief Check 1D packed data shape for grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine grid_check_shape_packed(this, data_shape)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data_shape(1) !< (n)
    ! LCOV_EXCL_START
    if (data_shape(1) /= this%ncells) then
      call error_message( &
        "grid data: packed data has wrong shape. Expected: (", &
        num2str(this%ncells), "), got: (", num2str(data_shape(1)), ")")
    end if
    ! LCOV_EXCL_STOP
  end subroutine grid_check_shape_packed

  !> \brief Pack 2D data with grid mask
  !> \return `real(sp) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_sp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:,:)
    real(sp), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_sp

  !> \brief Pack 2D data with grid mask
  !> \return `real(dp) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_dp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:,:)
    real(dp), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_dp

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i1) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_i1(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i1), intent(in) :: data(:,:)
    integer(i1), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_i1

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i2) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_i2(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i2), intent(in) :: data(:,:)
    integer(i2), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_i2

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i4) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_i4(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:,:)
    integer(i4), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_i4

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i8) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_i8(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:,:)
    integer(i8), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_i8

  !> \brief Pack 2D data with grid mask
  !> \return `logical :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_pack_data_lgt(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:,:)
    logical, allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i4))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function grid_pack_data_lgt

  !> \brief Unpack 1D data with grid mask
  !> \return `real(sp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_sp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:)
    real(sp), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_sp)
  end function grid_unpack_data_sp

  !> \brief Unpack 1D data with grid mask
  !> \return `real(dp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_dp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:)
    real(dp), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_dp)
  end function grid_unpack_data_dp

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i1) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_i1(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i1), intent(in) :: data(:)
    integer(i1), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i1)
  end function grid_unpack_data_i1

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i2) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_i2(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i2), intent(in) :: data(:)
    integer(i2), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i2)
  end function grid_unpack_data_i2

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i4) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_i4(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:)
    integer(i4), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i4)
  end function grid_unpack_data_i4

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i8) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_i8(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:)
    integer(i8), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i8)
  end function grid_unpack_data_i8

  !> \brief Unpack 1D data with grid mask
  !> \return `logical :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function grid_unpack_data_lgt(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:)
    logical, allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, .false.)
  end function grid_unpack_data_lgt

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_sp(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:,:)
    real(sp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_sp

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_dp(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:,:)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_dp

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_i1(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i1), intent(in) :: data(:,:)
    integer(i1), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_i1

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_i2(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i2), intent(in) :: data(:,:)
    integer(i2), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_i2

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_i4(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:,:)
    integer(i4), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_i4

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_i8(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:,:)
    integer(i8), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_i8

  !> \brief Pack 2D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_pack_into_lgt(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:,:)
    logical, intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(data, kind=i4))
    call this%check_shape_packed(shape(out_data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (.not.this%mask(i,j)) cycle
        k = k + 1_i8
        out_data(k) = data(i,j)
      end do
    end do
    !$omp end parallel do
  end subroutine grid_pack_into_lgt

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_sp(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:)
    real(sp), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_sp
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_sp

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_dp(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:)
    real(dp), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_dp
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_dp

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_i1(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i1), intent(in) :: data(:)
    integer(i1), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_i1
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_i1

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_i2(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i2), intent(in) :: data(:)
    integer(i2), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_i2
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_i2

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_i4(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:)
    integer(i4), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_i4
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_i4

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_i8(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:)
    integer(i8), intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = nodata_i8
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_i8

  !> \brief Unpack 1D data with grid mask into preallocated array
  !> \authors Sebastian Müller
  !> \date    Nov 2025
  subroutine grid_unpack_into_lgt(this, data, out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:)
    logical, intent(out) :: out_data(:,:)
    integer(i8) :: k
    integer(i4) :: i, j
    call this%check_shape(shape(out_data, kind=i4))
    call this%check_shape_packed(shape(data, kind=i8))
    !$omp parallel do default(shared) private(k,i) schedule(static)
    do j = 1_i4, this%ny
      k = this%mask_cum_col_cnt(j)
      do i = 1_i4, this%nx
        if (this%mask(i,j)) then
          k = k + 1_i8
          out_data(i,j) = data(k)
        else
          out_data(i,j) = .false.
        end if
      end do
    end do
    !$omp end parallel do
  end subroutine grid_unpack_into_lgt

end module mo_grid
