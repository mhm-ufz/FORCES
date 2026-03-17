!> \file    mo_grid_helper.F90
!> \copydoc mo_grid_helper

!> \brief   Stateless helpers for grid operations.
!> \details This module contains reusable grid-related helpers that do not
!!          depend on `grid_t`. This includes geometry kernels, generic
!!          indexing/distance helpers, ASCII I/O helpers, and independent
!!          NetCDF axis checks.
!> \authors Sebastian Mueller
!> \date    Mar 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid_helper

  use mo_kind, only: i1, i2, i4, i8, sp, dp
  use mo_constants, only : nodata_dp, RadiusEarth_dp, deg2rad_dp
  use mo_utils, only: flip, optval, is_close
  use mo_message, only : error_message, warn_message
  use mo_string_utils, only : num2str

  implicit none

  private

  integer(i4), parameter :: helper_top_down = 0_i4
  integer(i4), parameter :: helper_bottom_up = 1_i4

  public :: grid_value_in_closed_interval
  public :: grid_shift_longitude_near_query
  public :: grid_orient2d
  public :: grid_point_in_triangle
  public :: grid_quad_contains_point
  public :: grid_append_candidate_id
  public :: grid_update_best_metric
  public :: intersection
  public :: calculate_coarse_extent
  public :: coarse_ij
  public :: id_bounds
  public :: dist_latlon
  public :: check_factor
  public :: read_ascii_grid
  public :: write_ascii_grid
  public :: read_ascii_header
#ifdef FORCES_WITH_NETCDF
  public :: check_uniform_axis
  public :: is_x_axis
  public :: is_y_axis
  public :: is_z_axis
  public :: is_t_axis
  public :: is_lon_coord
  public :: is_lat_coord
#endif

  interface read_ascii_grid
    module procedure read_ascii_grid_i4, read_ascii_grid_dp
  end interface read_ascii_grid

  interface write_ascii_grid
    module procedure write_ascii_grid_i4, write_ascii_grid_dp
  end interface write_ascii_grid

contains

  pure logical function grid_value_in_closed_interval(value, lower, upper) result(in_interval)
    implicit none
    real(dp), intent(in) :: value
    real(dp), intent(in) :: lower
    real(dp), intent(in) :: upper

    in_interval = (value > lower .or. is_close(value, lower)) .and. &
                  (value < upper .or. is_close(value, upper))
  end function grid_value_in_closed_interval

  pure elemental real(dp) function grid_shift_longitude_near_query(lon, query_lon) result(lon_shifted)
    implicit none
    real(dp), intent(in) :: lon
    real(dp), intent(in) :: query_lon

    lon_shifted = modulo(lon - query_lon + 180.0_dp, 360.0_dp) - 180.0_dp + query_lon
  end function grid_shift_longitude_near_query

  pure real(dp) function grid_orient2d(ax, ay, bx, by, cx, cy) result(cross)
    implicit none
    real(dp), intent(in) :: ax, ay, bx, by, cx, cy

    cross = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  end function grid_orient2d

  pure logical function grid_point_in_triangle(px, py, x1, y1, x2, y2, x3, y3) result(is_inside)
    implicit none
    real(dp), intent(in) :: px, py
    real(dp), intent(in) :: x1, y1, x2, y2, x3, y3

    real(dp) :: c1, c2, c3

    c1 = grid_orient2d(x1, y1, x2, y2, px, py)
    c2 = grid_orient2d(x2, y2, x3, y3, px, py)
    c3 = grid_orient2d(x3, y3, x1, y1, px, py)

    is_inside = ((c1 > 0.0_dp .or. is_close(c1, 0.0_dp)) .and. &
                 (c2 > 0.0_dp .or. is_close(c2, 0.0_dp)) .and. &
                 (c3 > 0.0_dp .or. is_close(c3, 0.0_dp))) .or. &
                ((c1 < 0.0_dp .or. is_close(c1, 0.0_dp)) .and. &
                 (c2 < 0.0_dp .or. is_close(c2, 0.0_dp)) .and. &
                 (c3 < 0.0_dp .or. is_close(c3, 0.0_dp)))
  end function grid_point_in_triangle

  pure logical function grid_quad_contains_point(x_vertices, y_vertices, x, y) result(is_inside)
    implicit none
    real(dp), intent(in) :: x_vertices(4)
    real(dp), intent(in) :: y_vertices(4)
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y

    is_inside = grid_point_in_triangle(x, y, x_vertices(1), y_vertices(1), x_vertices(2), y_vertices(2), &
                                       x_vertices(3), y_vertices(3)) .or. &
                grid_point_in_triangle(x, y, x_vertices(1), y_vertices(1), x_vertices(3), y_vertices(3), &
                                       x_vertices(4), y_vertices(4))
  end function grid_quad_contains_point

  subroutine grid_append_candidate_id(cand_ids, ncand, cand_k, k_lb, k_ub)
    implicit none
    integer(i8), intent(inout) :: cand_ids(4)
    integer(i4), intent(inout) :: ncand
    integer(i8), intent(in) :: cand_k
    integer(i8), intent(in) :: k_lb
    integer(i8), intent(in) :: k_ub

    integer(i4) :: i

    if (cand_k < k_lb .or. cand_k > k_ub) return
    do i = 1_i4, ncand
      if (cand_ids(i) == cand_k) return
    end do

    ncand = ncand + 1_i4
    cand_ids(ncand) = cand_k
  end subroutine grid_append_candidate_id

  subroutine grid_update_best_metric(best_k, best_metric, cand_k, cand_metric)
    implicit none
    integer(i8), intent(inout) :: best_k
    real(dp), intent(inout) :: best_metric
    integer(i8), intent(in) :: cand_k
    real(dp), intent(in) :: cand_metric

    if (best_k < 1_i8) then
      best_k = cand_k
      best_metric = cand_metric
      return
    end if

    if (cand_metric < best_metric) then
      best_k = cand_k
      best_metric = cand_metric
      return
    end if

    if (is_close(cand_metric, best_metric) .and. cand_k < best_k) then
      best_k = cand_k
      best_metric = cand_metric
    end if
  end subroutine grid_update_best_metric

  subroutine intersection(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, x, y)
    real(dp), intent(in) :: p1x, p1y, p2x, p2y
    real(dp), intent(in) :: q1x, q1y, q2x, q2y
    real(dp), intent(out) :: x, y
    real(dp) :: denom, det1, det2

    denom = (p1x-p2x)*(q1y-q2y) - (p1y-p2y)*(q1x-q2x)
    det1 = p1x*p2y-p1y*p2x
    det2 = q1x*q2y-q1y*q2x
    x = (det1*(q1x-q2x) - det2*(p1x-p2x)) / denom
    y = (det1*(q1y-q2y) - det2*(p1y-p2y)) / denom
  end subroutine intersection

  subroutine calculate_coarse_extent(nx_in,  ny_in,  xllcorner_in,  yllcorner_in,  cellsize_in,  target_resolution, &
                                     nx_out, ny_out, xllcorner_out, yllcorner_out, cellsize_out, tol)
    implicit none

    integer(i4), intent(in) :: nx_in
    integer(i4), intent(in) :: ny_in
    real(dp), intent(in) :: xllcorner_in
    real(dp), intent(in) :: yllcorner_in
    real(dp), intent(in) :: cellsize_in
    real(dp), intent(in) :: target_resolution
    integer(i4), intent(out) :: nx_out
    integer(i4), intent(out) :: ny_out
    real(dp), intent(out) :: xllcorner_out
    real(dp), intent(out) :: yllcorner_out
    real(dp), intent(out) :: cellsize_out
    real(dp), intent(in), optional :: tol

    real(dp) :: cellFactor, rounded
    integer(i4) :: factor

    call check_factor(cellsize_in, target_resolution, cellFactor, rounded, factor, tol)

    cellsize_out = target_resolution
    ny_out = nint(real(ny_in, dp) / cellFactor)
    nx_out = nint(real(nx_in, dp) / cellFactor)

    if (ny_out * factor < ny_in) ny_out = ny_out + 1_i4
    if (nx_out * factor < nx_in) nx_out = nx_out + 1_i4

    yllcorner_out = yllcorner_in
    xllcorner_out = xllcorner_in
  end subroutine calculate_coarse_extent

#ifdef FORCES_WITH_NETCDF

  subroutine check_uniform_axis(var, cellsize, origin, increasing, tol)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    real(dp), optional, intent(out) :: cellsize
    real(dp), optional, intent(out) :: origin
    logical, optional, intent(out) :: increasing
    real(dp), intent(in), optional :: tol
    real(dp), dimension(:), allocatable :: axis
    real(dp), dimension(:,:), allocatable :: bounds
    real(dp) :: diff, tol_
    integer(i4) :: i_ub, i_lb
    logical :: has_bnds
    type(NcVariable) :: bnds
    character(len=256) :: name

    call var%getData(axis)
    if (var%hasAttribute("bounds")) then
      has_bnds = .true.
      call var%getAttribute("bounds", name)
      bnds = var%parent%getVariable(trim(name))
      call bnds%getData(bounds)
    else
      has_bnds = .false.
    end if
    name = var%getName()
    tol_ = optval(tol, 1.e-7_dp)

    if (size(axis) == 0_i4) &
      call error_message("check_uniform_axis: axis is empty: ", name) ! LCOV_EXCL_LINE

    if (size(axis) > 1_i4) then
      diff = (axis(size(axis)) - axis(1)) / real(size(axis) - 1_i4, dp)
      if (.not.all(is_close(axis(2:size(axis))-axis(1:size(axis)-1), diff, rtol=0.0_dp, atol=tol_))) &
        call error_message("check_uniform_axis: given axis is not uniform: ", name) ! LCOV_EXCL_LINE
    else
      if (.not. has_bnds) &
        call error_message("check_uniform_axis: can't check axis of size 1 when no bounds are given: ", name) ! LCOV_EXCL_LINE
      diff = bounds(2,1) - bounds(1,1)
    end if

    if (has_bnds) then
      i_lb = 1
      i_ub = 2
      if (size(bounds, dim=2)>1) then
        if (.not. is_close(bounds(2,1), bounds(1,2), rtol=0.0_dp, atol=tol_) &
            .and. is_close(bounds(1,1), bounds(2,2), rtol=0.0_dp, atol=tol_)) then
          call warn_message("check_uniform_axis: bounds actually have wrong monotonicity: ", name)
          i_lb = 2
          i_ub = 1
        end if
      end if
      if (.not.all(is_close(bounds(i_ub,:)-bounds(i_lb,:), diff, rtol=0.0_dp, atol=tol_))) &
        call error_message("check_uniform_axis: given bounds are not uniform: ", name) ! LCOV_EXCL_LINE
      if (.not.all(is_close(axis(:)-bounds(i_lb,:), 0.5_dp*diff, rtol=0.0_dp, atol=tol_))) &
        call error_message("check_uniform_axis: given bounds are not centered around axis points: ", name) ! LCOV_EXCL_LINE
    end if

    if (present(cellsize)) cellsize = abs(diff)
    if (present(origin)) origin = minval(axis) - 0.5_dp * abs(diff)
    if (present(increasing)) increasing = diff > 0.0_dp
  end subroutine check_uniform_axis

  logical function is_x_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_x_axis = .false.
    if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "projection_x_coordinate") is_x_axis = .true.
    else if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "X") is_x_axis = .true.
    else if (var%hasAttribute("_CoordinateAxisType")) then
      call var%getAttribute("_CoordinateAxisType", tmp_str)
      if (trim(tmp_str) == "GeoX") is_x_axis = .true.
    end if
  end function is_x_axis

  logical function is_y_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_y_axis = .false.
    if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "projection_y_coordinate") is_y_axis = .true.
    else if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "Y") is_y_axis = .true.
    else if (var%hasAttribute("_CoordinateAxisType")) then
      call var%getAttribute("_CoordinateAxisType", tmp_str)
      if (trim(tmp_str) == "GeoY") is_y_axis = .true.
    end if
  end function is_y_axis

  logical function is_z_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_z_axis = .false.
    if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "Z") is_z_axis = .true.
    else if (var%hasAttribute("positive")) then
      is_z_axis = .true.
    end if
  end function is_z_axis

  logical function is_t_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_t_axis = .false.
    if (var%hasAttribute("units")) then
      call var%getAttribute("units", tmp_str)
      if (index(tmp_str, "since") > 0) is_t_axis = .true.
    else if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "T") is_t_axis = .true.
    else if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "time") is_t_axis = .true.
    end if
  end function is_t_axis

  logical function is_lon_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_lon_coord = .false.
    if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "longitude") is_lon_coord = .true.
    else if (var%hasAttribute("units")) then
      call var%getAttribute("units", tmp_str)
      if (trim(tmp_str) == "degreeE") is_lon_coord = .true.
      if (trim(tmp_str) == "degree_E") is_lon_coord = .true.
      if (trim(tmp_str) == "degree_east") is_lon_coord = .true.
      if (trim(tmp_str) == "degreesE") is_lon_coord = .true.
      if (trim(tmp_str) == "degrees_E") is_lon_coord = .true.
      if (trim(tmp_str) == "degrees_east") is_lon_coord = .true.
    else if (var%hasAttribute("_CoordinateAxisType")) then
      call var%getAttribute("_CoordinateAxisType", tmp_str)
      if (trim(tmp_str) == "Lon") is_lon_coord = .true.
    else if (var%hasAttribute("long_name")) then
      call var%getAttribute("long_name", tmp_str)
      if (trim(tmp_str) == "longitude") is_lon_coord = .true.
    end if
  end function is_lon_coord

  logical function is_lat_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var
    character(len=256) :: tmp_str

    is_lat_coord = .false.
    if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "latitude") is_lat_coord = .true.
    else if (var%hasAttribute("units")) then
      call var%getAttribute("units", tmp_str)
      if (trim(tmp_str) == "degreeN") is_lat_coord = .true.
      if (trim(tmp_str) == "degree_N") is_lat_coord = .true.
      if (trim(tmp_str) == "degree_north") is_lat_coord = .true.
      if (trim(tmp_str) == "degreesN") is_lat_coord = .true.
      if (trim(tmp_str) == "degrees_N") is_lat_coord = .true.
      if (trim(tmp_str) == "degrees_north") is_lat_coord = .true.
    else if (var%hasAttribute("_CoordinateAxisType")) then
      call var%getAttribute("_CoordinateAxisType", tmp_str)
      if (trim(tmp_str) == "Lat") is_lat_coord = .true.
    else if (var%hasAttribute("long_name")) then
      call var%getAttribute("long_name", tmp_str)
      if (trim(tmp_str) == "latitude") is_lat_coord = .true.
    end if
  end function is_lat_coord

#endif

  subroutine check_factor(fine_cellsize, coarse_cellsize, cellfactor, rounded, factor, tol)
    real(dp), intent(in) :: fine_cellsize
    real(dp), intent(in) :: coarse_cellsize
    real(dp), optional, intent(out) :: cellfactor
    real(dp), optional, intent(out) :: rounded
    integer(i4), optional, intent(out) :: factor
    real(dp), intent(in), optional :: tol
    real(dp) :: cellfactor_, rounded_, tol_
    integer(i4) :: factor_

    tol_ = optval(tol, 1.e-7_dp)

    cellfactor_ = coarse_cellsize / fine_cellsize
    rounded_ = anint(cellfactor_)
    factor_ = nint(cellfactor_)

    if (abs(rounded_ - cellfactor_) > tol_) then
      call error_message( &
        'check_factor: Two resolutions size do not confirm (need to have an integer ratio): ', &
        trim(adjustl(num2str(nint(coarse_cellsize)))), &
        trim(adjustl(num2str(nint(fine_cellsize))))) ! LCOV_EXCL_LINE
    end if
    if (factor_ < 1_i4) call error_message("check_factor: cell factor needs to be >= 1 to setup an upscaler.") ! LCOV_EXCL_LINE
    if (present(cellfactor)) cellfactor = cellfactor_
    if (present(rounded)) rounded = rounded_
    if (present(factor)) factor = factor_
  end subroutine check_factor

  subroutine read_ascii_grid_dp(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    use mo_utils, only : eq
    implicit none

    character(len = *), intent(in) :: path
    real(dp), dimension(:, :), allocatable, intent(out) :: data
    logical, dimension(:, :), allocatable, intent(out), optional :: mask
    integer(i4), intent(in), optional :: ref_nrows
    integer(i4), intent(in), optional :: ref_ncols
    real(dp), intent(in), optional :: ref_xllcorner
    real(dp), intent(in), optional :: ref_yllcorner
    real(dp), intent(in), optional :: ref_cellsize
    integer(i4), intent(in), optional :: y_direction

    integer(i4) :: file_nrows, file_ncols
    real(dp) :: file_xllcorner, file_yllcorner, file_cellsize, file_nodata
    integer(i4) :: i, j, fileunit, hlines
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == helper_bottom_up

    call read_ascii_header( &
      path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata, &
      ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, hlines)

    allocate(data(file_ncols, file_nrows))

    open(newunit=fileunit, file=path, action='read', status='old', recl=48 * file_ncols)
    do i = 1_i4, hlines
      read(fileunit, *)
    end do
    do i = 1_i4, file_nrows
      read(fileunit, *) (data(j, i), j = 1_i4, file_ncols)
    end do
    close(fileunit)

    if (present(mask)) then
      allocate(mask(file_ncols, file_nrows), source=.true.)
      where (eq(data, file_nodata)) mask = .false.
      if (flip_y) call flip(mask, idim=2)
    end if

    if (flip_y) call flip(data, idim=2)
  end subroutine read_ascii_grid_dp

  subroutine read_ascii_grid_i4(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    implicit none

    character(len = *), intent(in) :: path
    integer(i4), dimension(:, :), allocatable, intent(out) :: data
    logical, dimension(:, :), allocatable, intent(out), optional :: mask
    integer(i4), intent(in), optional :: ref_nrows
    integer(i4), intent(in), optional :: ref_ncols
    real(dp), intent(in), optional :: ref_xllcorner
    real(dp), intent(in), optional :: ref_yllcorner
    real(dp), intent(in), optional :: ref_cellsize
    integer(i4), intent(in), optional :: y_direction

    integer(i4) :: file_nrows, file_ncols
    real(dp) :: file_xllcorner, file_yllcorner, file_cellsize, file_nodata
    integer(i4) :: i, j, fileunit, header_size
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == helper_bottom_up

    call read_ascii_header( &
      path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata, &
      ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, header_size)

    allocate(data(file_ncols, file_nrows))

    open(newunit=fileunit, file=path, action='read', status='old', recl=48 * file_ncols)
    do i = 1_i4, header_size
      read(fileunit, *)
    end do
    do i = 1_i4, file_nrows
      read(fileunit, *) (data(j, i), j = 1_i4, file_ncols)
    end do
    close(fileunit)

    if (present(mask)) then
      allocate(mask(file_ncols, file_nrows), source=.true.)
      where (data == int(file_nodata, i4)) mask = .false.
      if (flip_y) call flip(mask, idim=2)
    end if
    if (flip_y) call flip(data, idim=2)
  end subroutine read_ascii_grid_i4

  subroutine read_ascii_header( &
    path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, &
    ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, header_size)

    use mo_os, only : check_path_isfile
    use mo_string_utils, only : tolower
    implicit none

    character(len = *), intent(in) :: path
    integer(i4), intent(out) :: nrows
    integer(i4), intent(out) :: ncols
    real(dp), intent(out) :: xllcorner
    real(dp), intent(out) :: yllcorner
    real(dp), intent(out) :: cellsize
    real(dp), intent(out), optional :: nodata
    integer(i4), intent(in), optional :: ref_nrows
    integer(i4), intent(in), optional :: ref_ncols
    real(dp), intent(in), optional :: ref_xllcorner
    real(dp), intent(in), optional :: ref_yllcorner
    real(dp), intent(in), optional :: ref_cellsize
    integer(i4), optional, intent(out) :: header_size

    real(dp) :: file_nodata
    character(12) :: attribute
    integer(i4) :: io, fileunit

    call check_path_isfile(path=path, raise=.true.)
    open(newunit=fileunit, file=path, status='old')
    read(fileunit, *) attribute, ncols
    read(fileunit, *) attribute, nrows
    read(fileunit, *) attribute, xllcorner
    read(fileunit, *) attribute, yllcorner
    read(fileunit, *) attribute, cellsize
    if (present(nodata) .or. present(header_size)) then
      read(fileunit, *, iostat=io) attribute, file_nodata
      if (io < 0) then
        if (present(nodata)) nodata = nodata_dp
        if (present(header_size)) header_size = 5_i4
      else if (tolower(attribute) == "nodata_value") then
        if (present(nodata)) nodata = file_nodata
        if (present(header_size)) header_size = 6_i4
      else
        if (present(nodata)) nodata = nodata_dp
        if (present(header_size)) header_size = 5_i4
      end if
    end if
    close(fileunit)

    if (present(ref_ncols)) then
      if (ncols .ne. ref_ncols) &
        call error_message('read_ascii: header not matching with reference header: ncols') ! LCOV_EXCL_LINE
    end if
    if (present(ref_nrows)) then
      if (nrows .ne. ref_nrows) &
        call error_message('read_ascii: header not matching with reference header: nrows') ! LCOV_EXCL_LINE
    end if
    if (present(ref_xllcorner)) then
      if (abs(xllcorner - ref_xllcorner) .gt. tiny(1.0_dp)) &
        call error_message('read_ascii: header not matching with reference header: xllcorner') ! LCOV_EXCL_LINE
    end if
    if (present(ref_yllcorner)) then
      if (abs(yllcorner - ref_yllcorner) .gt. tiny(1.0_dp)) &
        call error_message('read_ascii: header not matching with reference header: yllcorner') ! LCOV_EXCL_LINE
    end if
    if (present(ref_cellsize)) then
      if (abs(cellsize - ref_cellsize) .gt. tiny(1.0_dp)) &
        call error_message('read_ascii: header not matching with reference header: cellsize') ! LCOV_EXCL_LINE
    end if
  end subroutine read_ascii_header

  subroutine write_ascii_grid_dp(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    character(len=*), intent(in) :: path
    integer(i4), intent(in) :: ncols
    integer(i4), intent(in) :: nrows
    real(dp), intent(in) :: xllcorner
    real(dp), intent(in) :: yllcorner
    real(dp), intent(in) :: cellsize
    real(dp), intent(in) :: nodata
    real(dp), intent(in), optional :: data(:,:)
    integer(i4), intent(in), optional :: y_direction
    logical, intent(in), optional :: is_xy

    integer(i4) :: i, j, io, ierr
    logical :: is_bottom_up, is_xy_

    is_bottom_up = .true.
    if (present(y_direction)) is_bottom_up = y_direction == helper_bottom_up
    is_xy_ = optval(is_xy, .true.)

    if (present(data)) then
      if (is_xy_) then
        if (size(data,1) /= ncols .or. size(data,2) /= nrows) then
          call error_message('Error: data array dimensions mismatch (expected: ncols x nrows)') ! LCOV_EXCL_LINE
        end if
      else
        if (size(data,1) /= nrows .or. size(data,2) /= ncols) then
          call error_message('Error: data array dimensions mismatch (expected: nrows x ncols)') ! LCOV_EXCL_LINE
        end if
      end if
    end if

    open(newunit=io, file=path, status='replace', action='write', form='formatted', iostat=ierr)
    if (ierr /= 0) then
      call error_message('Error opening file: ', path) ! LCOV_EXCL_LINE
    end if

    write(io,'(A,I0)') 'ncols         ', ncols
    write(io,'(A,I0)') 'nrows         ', nrows
    write(io,'(A,F0.10)') 'xllcorner     ', xllcorner
    write(io,'(A,F0.10)') 'yllcorner     ', yllcorner
    write(io,'(A,F0.10)') 'cellsize      ', cellsize
    write(io,'(A,F0.10)') 'NODATA_value  ', nodata

    if (present(data)) then
      if (is_bottom_up) then
        if (is_xy_) then
          do i = nrows, 1_i4, -1_i4
            write(io, '(*(F0.10,1X))') (data(j,i), j=1_i4,ncols)
          end do
        else
          do i = nrows, 1_i4, -1_i4
            write(io, '(*(F0.10,1X))') (data(i,j), j=1_i4,ncols)
          end do
        end if
      else
        if (is_xy_) then
          do i = 1_i4, nrows
            write(io, '(*(F0.10,1X))') (data(j,i), j=1_i4,ncols)
          end do
        else
          do i = 1_i4, nrows
            write(io, '(*(F0.10,1X))') (data(i,j), j=1_i4,ncols)
          end do
        end if
      end if
    end if

    close(io)
  end subroutine write_ascii_grid_dp

  subroutine write_ascii_grid_i4(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    character(len=*), intent(in) :: path
    integer(i4), intent(in) :: ncols
    integer(i4), intent(in) :: nrows
    real(dp), intent(in) :: xllcorner
    real(dp), intent(in) :: yllcorner
    real(dp), intent(in) :: cellsize
    integer(i4), intent(in) :: nodata
    integer(i4), intent(in), optional :: data(:,:)
    integer(i4), intent(in), optional :: y_direction
    logical, intent(in), optional :: is_xy

    integer(i4) :: i, j, io, ierr
    logical :: is_bottom_up, is_xy_

    is_bottom_up = .false.
    if (present(y_direction)) is_bottom_up = y_direction == helper_bottom_up
    is_xy_ = optval(is_xy, .true.)

    if (present(data)) then
      if (is_xy_) then
        if (size(data,1) /= ncols .or. size(data,2) /= nrows) then
          call error_message('Error: data array dimensions mismatch (expected: ncols x nrows)') ! LCOV_EXCL_LINE
        end if
      else
        if (size(data,1) /= nrows .or. size(data,2) /= ncols) then
          call error_message('Error: data array dimensions mismatch (expected: nrows x ncols)') ! LCOV_EXCL_LINE
        end if
      end if
    end if

    open(newunit=io, file=path, status='replace', action='write', form='formatted', iostat=ierr)
    if (ierr /= 0) then
      call error_message('Error opening file: ', path) ! LCOV_EXCL_LINE
    end if

    write(io,'(A,I0)') 'ncols         ', ncols
    write(io,'(A,I0)') 'nrows         ', nrows
    write(io,'(A,F0.10)') 'xllcorner     ', xllcorner
    write(io,'(A,F0.10)') 'yllcorner     ', yllcorner
    write(io,'(A,F0.10)') 'cellsize      ', cellsize
    write(io,'(A,I0)') 'NODATA_value  ', nodata

    if (present(data)) then
      if (is_bottom_up) then
        if (is_xy_) then
          do i = nrows, 1_i4, -1_i4
            write(io, '(*(I0,1X))') (data(j,i), j=1_i4,ncols)
          end do
        else
          do i = nrows, 1_i4, -1_i4
            write(io, '(*(I0,1X))') (data(i,j), j=1_i4,ncols)
          end do
        end if
      else
        if (is_xy_) then
          do i = 1_i4, nrows
            write(io, '(*(I0,1X))') (data(j,i), j=1_i4,ncols)
          end do
        else
          do i = 1_i4, nrows
            write(io, '(*(I0,1X))') (data(i,j), j=1_i4,ncols)
          end do
        end if
      end if
    end if

    close(io)
  end subroutine write_ascii_grid_i4

  pure subroutine coarse_ij(factor, fine_i, fine_j, fine_y_dir, fine_ny, coarse_y_dir, coarse_ny, coarse_i, coarse_j)
    integer(i4), intent(in) :: factor
    integer(i4), intent(in) :: fine_i
    integer(i4), intent(in) :: fine_j
    integer(i4), intent(in) :: fine_y_dir
    integer(i4), intent(in) :: fine_ny
    integer(i4), intent(in) :: coarse_y_dir
    integer(i4), intent(in) :: coarse_ny
    integer(i4), intent(out) :: coarse_i
    integer(i4), intent(out) :: coarse_j
    integer(i4) :: j

    j = fine_j
    if (fine_y_dir == helper_top_down) j = fine_ny - j + 1_i4
    coarse_j = (j - 1_i4) / factor + 1_i4
    coarse_i = (fine_i - 1_i4) / factor + 1_i4
    if (coarse_y_dir == helper_top_down) coarse_j = coarse_ny - coarse_j + 1_i4
  end subroutine coarse_ij

  pure subroutine id_bounds(factor, coarse_i, coarse_j, coarse_y_dir, coarse_ny, fine_y_dir, fine_nx, fine_ny, i_lb, i_ub, j_lb, j_ub)
    integer(i4), intent(in) :: factor
    integer(i4), intent(in) :: coarse_i
    integer(i4), intent(in) :: coarse_j
    integer(i4), intent(in) :: coarse_y_dir
    integer(i4), intent(in) :: coarse_ny
    integer(i4), intent(in) :: fine_y_dir
    integer(i4), intent(in) :: fine_nx
    integer(i4), intent(in) :: fine_ny
    integer(i4), intent(out) :: i_lb
    integer(i4), intent(out) :: i_ub
    integer(i4), intent(out) :: j_lb
    integer(i4), intent(out) :: j_ub
    integer(i4) :: temp, ic, jc

    ic = coarse_i
    i_lb = (ic - 1) * factor + 1
    i_ub = min(ic * factor, fine_nx)

    jc = coarse_j
    if (coarse_y_dir == helper_top_down) jc = coarse_ny - coarse_j + 1
    j_lb = (jc - 1) * factor + 1
    j_ub = min(jc * factor, fine_ny)

    if (fine_y_dir == helper_top_down) then
      temp = j_lb
      j_lb = fine_ny - j_ub + 1
      j_ub = fine_ny - temp + 1
    end if
  end subroutine id_bounds

  pure real(dp) function dist_latlon(lat1, lon1, lat2, lon2)
    real(dp), intent(in) :: lat1
    real(dp), intent(in) :: lon1
    real(dp), intent(in) :: lat2
    real(dp), intent(in) :: lon2
    real(dp) :: theta1, phi1, theta2, phi2
    real(dp) :: term1, term2, term3, temp

    theta1 = deg2rad_dp * lon1
    phi1 = deg2rad_dp * lat1
    theta2 = deg2rad_dp * lon2
    phi2 = deg2rad_dp * lat2

    term1 = cos(phi1) * cos(theta1) * cos(phi2) * cos(theta2)
    term2 = cos(phi1) * sin(theta1) * cos(phi2) * sin(theta2)
    term3 = sin(phi1) * sin(phi2)
    temp = min(term1 + term2 + term3, 1.0_dp)
    dist_latlon = RadiusEarth_dp * acos(temp)
  end function dist_latlon

end module mo_grid_helper
