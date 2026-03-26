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
  use mo_grid_constants, only: top_down, bottom_up
  use mo_constants, only : nodata_dp, RadiusEarth_dp, deg2rad_dp
  use mo_utils, only: flip, optval, is_close
  use mo_message, only : error_message, warn_message
  use mo_string_utils, only : num2str

  implicit none

  private

  public :: value_in_closed_interval
  public :: shift_longitude_near_query
  public :: orient2d
  public :: point_in_triangle
  public :: quad_contains_point
  public :: arrays_match_2d
  public :: append_candidate_id
  public :: update_best_metric
  public :: intersection
  public :: calculate_coarse_extent
  public :: coarse_ij
  public :: id_bounds
  public :: dist_latlon
  public :: check_factor
  public :: read_ascii_grid
  public :: write_ascii_grid
  public :: read_ascii_header
  public :: data_t
#ifdef FORCES_WITH_NETCDF
  public :: check_uniform_axis
  public :: is_x_axis
  public :: is_y_axis
  public :: is_z_axis
  public :: is_t_axis
  public :: is_lon_coord
  public :: is_lat_coord
  public :: mask_from_var
  public :: data_from_var
#endif

  !> \brief Read ESRI ASCII grid into an xy-ordered array.
  interface read_ascii_grid
    module procedure read_ascii_grid_i4, read_ascii_grid_dp
  end interface read_ascii_grid

  !> \brief Write ESRI ASCII grid from an xy- or yx-ordered array.
  interface write_ascii_grid
    module procedure write_ascii_grid_i4, write_ascii_grid_dp
  end interface write_ascii_grid

  !> \brief Check whether two 2D arrays match in shape and element-wise values.
  interface arrays_match_2d
    module procedure arrays_match_2d_dp, arrays_match_2d_lgt
  end interface arrays_match_2d

  !> \class   data_t
  !> \brief   2D data container for different data types.
  type :: data_t
    character(:), allocatable :: dtype       !< selector for data type ('f32', 'f64', 'i8', 'i16', 'i32', 'i64')
    real(sp), allocatable :: data_sp(:,:)    !< data in single precision
    real(dp), allocatable :: data_dp(:,:)    !< data in double precision
    integer(i1), allocatable :: data_i1(:,:) !< data in 1-byte integers
    integer(i2), allocatable :: data_i2(:,:) !< data in 2-byte integers
    integer(i4), allocatable :: data_i4(:,:) !< data in 4-byte integers
    integer(i8), allocatable :: data_i8(:,:) !< data in 8-byte integers
  contains
    procedure, public :: deallocate => data_deallocate
    generic, public :: move => data_move_sp, data_move_dp, data_move_i1, data_move_i2, data_move_i4, data_move_i8
    procedure, private :: data_move_sp, data_move_dp, data_move_i1, data_move_i2, data_move_i4, data_move_i8
  end type data_t

contains

  !> \brief Deallocate all arrays stored in a generic data container.
  subroutine data_deallocate(this)
    class(data_t), intent(inout) :: this !< Data container to clear.

    if (allocated(this%data_sp)) deallocate(this%data_sp)
    if (allocated(this%data_dp)) deallocate(this%data_dp)
    if (allocated(this%data_i1)) deallocate(this%data_i1)
    if (allocated(this%data_i2)) deallocate(this%data_i2)
    if (allocated(this%data_i4)) deallocate(this%data_i4)
    if (allocated(this%data_i8)) deallocate(this%data_i8)
  end subroutine data_deallocate

  !> \brief Move single-precision data out of a generic data container.
  subroutine data_move_sp(this, data)
    class(data_t), intent(inout) :: this
    real(sp), allocatable, dimension(:,:), intent(out) :: data  !< Moved single-precision data.

    if (.not. allocated(this%data_sp)) call error_message("data % get: data not allocated for dtype 'f32'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_sp, data)
  end subroutine data_move_sp

  !> \brief Move double-precision data out of a generic data container.
  subroutine data_move_dp(this, data)
    class(data_t), intent(inout) :: this
    real(dp), allocatable, dimension(:,:), intent(out) :: data  !< Moved double-precision data.

    if (.not. allocated(this%data_dp)) call error_message("data % get: data not allocated for dtype 'f64'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_dp, data)
  end subroutine data_move_dp

  !> \brief Move 1-byte integer data out of a generic data container.
  subroutine data_move_i1(this, data)
    class(data_t), intent(inout) :: this
    integer(i1), allocatable, dimension(:,:), intent(out) :: data  !< Moved 1-byte integer data.

    if (.not. allocated(this%data_i1)) call error_message("data % get: data not allocated for dtype 'i8'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_i1, data)
  end subroutine data_move_i1

  !> \brief Move 2-byte integer data out of a generic data container.
  subroutine data_move_i2(this, data)
    class(data_t), intent(inout) :: this
    integer(i2), allocatable, dimension(:,:), intent(out) :: data  !< Moved 2-byte integer data.

    if (.not. allocated(this%data_i2)) call error_message("data % get: data not allocated for dtype 'i16'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_i2, data)
  end subroutine data_move_i2

  !> \brief Move 4-byte integer data out of a generic data container.
  subroutine data_move_i4(this, data)
    class(data_t), intent(inout) :: this
    integer(i4), allocatable, dimension(:,:), intent(out) :: data  !< Moved 4-byte integer data.

    if (.not. allocated(this%data_i4)) call error_message("data % get: data not allocated for dtype 'i32'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_i4, data)
  end subroutine data_move_i4

  !> \brief Move 8-byte integer data out of a generic data container.
  subroutine data_move_i8(this, data)
    class(data_t), intent(inout) :: this
    integer(i8), allocatable, dimension(:,:), intent(out) :: data  !< Moved 8-byte integer data.

    if (.not. allocated(this%data_i8)) call error_message("data % get: data not allocated for dtype 'i64'") ! LCOV_EXCL_LINE
    call move_alloc(this%data_i8, data)
  end subroutine data_move_i8

  !> \brief Check whether two 2D double-precision arrays match within a tolerance.
  logical function arrays_match_2d_dp(a, b, tol) result(arrays_match)
    implicit none
    real(dp), intent(in) :: a(:, :) !< First array to compare.
    real(dp), intent(in) :: b(:, :) !< Second array to compare.
    real(dp), optional, intent(in) :: tol !< Tolerance for element-wise comparison (default: 1.e-7).

    real(dp) :: tol_
    integer(i4) :: j

    arrays_match = .false.
    if (any(shape(a, kind=i4) /= shape(b, kind=i4))) return

    tol_ = optval(tol, 1.0e-7_dp)
    arrays_match = .true.
    !$omp parallel do default(shared) schedule(static)
    do j = 1_i4, size(a, dim=2, kind=i4)
      !$omp flush(arrays_match)
      if (.not. arrays_match) cycle ! no work for rest of the loop (exit not safe with omp)
      if (.not. all(is_close(a(:, j), b(:, j), tol_, tol_))) then
        !$omp atomic write
        arrays_match = .false.
      end if
    end do
    !$omp end parallel do
  end function arrays_match_2d_dp

  !> \brief Check whether two 2D logical arrays match.
  logical function arrays_match_2d_lgt(a, b) result(arrays_match)
    implicit none
    logical, intent(in) :: a(:, :) !< First array to compare.
    logical, intent(in) :: b(:, :) !< Second array to compare.

    integer(i4) :: j

    arrays_match = .false.
    if (any(shape(a, kind=i4) /= shape(b, kind=i4))) return

    arrays_match = .true.
    !$omp parallel do default(shared) schedule(static)
    do j = 1_i4, size(a, dim=2, kind=i4)
      !$omp flush(arrays_match)
      if (.not. arrays_match) cycle ! no work for rest of the loop (exit not safe with omp)
      if (any(a(:, j) .neqv. b(:, j))) then
        !$omp atomic write
        arrays_match = .false.
      end if
    end do
    !$omp end parallel do
  end function arrays_match_2d_lgt

  !> \brief Check whether a value lies inside a closed interval.
  pure logical function value_in_closed_interval(value, lower, upper) result(in_interval)
    implicit none
    real(dp), intent(in) :: value !< Value to test.
    real(dp), intent(in) :: lower !< Inclusive lower bound.
    real(dp), intent(in) :: upper !< Inclusive upper bound.

    in_interval = (value > lower .or. is_close(value, lower)) .and. &
                  (value < upper .or. is_close(value, upper))
  end function value_in_closed_interval

  !> \brief Shift a longitude by 360-degree multiples to stay near a query longitude.
  pure elemental real(dp) function shift_longitude_near_query(lon, query_lon) result(lon_shifted)
    implicit none
    real(dp), intent(in) :: lon        !< Longitude to shift.
    real(dp), intent(in) :: query_lon  !< Reference longitude for the shift.

    lon_shifted = modulo(lon - query_lon + 180.0_dp, 360.0_dp) - 180.0_dp + query_lon
  end function shift_longitude_near_query

  !> \brief Compute the signed 2D orientation of three points.
  pure real(dp) function orient2d(ax, ay, bx, by, cx, cy) result(cross)
    implicit none
    real(dp), intent(in) :: ax !< X coordinate of the first point.
    real(dp), intent(in) :: ay !< Y coordinate of the first point.
    real(dp), intent(in) :: bx !< X coordinate of the second point.
    real(dp), intent(in) :: by !< Y coordinate of the second point.
    real(dp), intent(in) :: cx !< X coordinate of the third point.
    real(dp), intent(in) :: cy !< Y coordinate of the third point.

    cross = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
  end function orient2d

  !> \brief Check whether a point lies inside or on the boundary of a triangle.
  pure logical function point_in_triangle(px, py, x1, y1, x2, y2, x3, y3) result(is_inside)
    implicit none
    real(dp), intent(in) :: px !< X coordinate of the query point.
    real(dp), intent(in) :: py !< Y coordinate of the query point.
    real(dp), intent(in) :: x1 !< X coordinate of triangle vertex 1.
    real(dp), intent(in) :: y1 !< Y coordinate of triangle vertex 1.
    real(dp), intent(in) :: x2 !< X coordinate of triangle vertex 2.
    real(dp), intent(in) :: y2 !< Y coordinate of triangle vertex 2.
    real(dp), intent(in) :: x3 !< X coordinate of triangle vertex 3.
    real(dp), intent(in) :: y3 !< Y coordinate of triangle vertex 3.

    real(dp) :: c1, c2, c3

    c1 = orient2d(x1, y1, x2, y2, px, py)
    c2 = orient2d(x2, y2, x3, y3, px, py)
    c3 = orient2d(x3, y3, x1, y1, px, py)

    is_inside = ((c1 > 0.0_dp .or. is_close(c1, 0.0_dp)) .and. &
                 (c2 > 0.0_dp .or. is_close(c2, 0.0_dp)) .and. &
                 (c3 > 0.0_dp .or. is_close(c3, 0.0_dp))) .or. &
                ((c1 < 0.0_dp .or. is_close(c1, 0.0_dp)) .and. &
                 (c2 < 0.0_dp .or. is_close(c2, 0.0_dp)) .and. &
                 (c3 < 0.0_dp .or. is_close(c3, 0.0_dp)))
  end function point_in_triangle

  !> \brief Check whether a point lies inside or on the boundary of a quadrilateral.
  pure logical function quad_contains_point(x_vertices, y_vertices, x, y) result(is_inside)
    implicit none
    real(dp), intent(in) :: x_vertices(4) !< X coordinates of the quadrilateral vertices.
    real(dp), intent(in) :: y_vertices(4) !< Y coordinates of the quadrilateral vertices.
    real(dp), intent(in) :: x             !< X coordinate of the query point.
    real(dp), intent(in) :: y             !< Y coordinate of the query point.

    is_inside = point_in_triangle(x, y, x_vertices(1), y_vertices(1), x_vertices(2), y_vertices(2), &
                                  x_vertices(3), y_vertices(3)) .or. &
                point_in_triangle(x, y, x_vertices(1), y_vertices(1), x_vertices(3), y_vertices(3), &
                                  x_vertices(4), y_vertices(4))
  end function quad_contains_point

  !> \brief Append a candidate id if it lies inside bounds and is not yet present.
  subroutine append_candidate_id(cand_ids, ncand, cand_k, k_lb, k_ub)
    implicit none
    integer(i8), intent(inout) :: cand_ids(4) !< Candidate id buffer.
    integer(i4), intent(inout) :: ncand       !< Number of valid entries currently stored.
    integer(i8), intent(in) :: cand_k         !< Candidate id to append.
    integer(i8), intent(in) :: k_lb           !< Inclusive lower id bound.
    integer(i8), intent(in) :: k_ub           !< Inclusive upper id bound.

    integer(i4) :: i

    if (cand_k < k_lb .or. cand_k > k_ub) return
    do i = 1_i4, ncand
      if (cand_ids(i) == cand_k) return
    end do

    ncand = ncand + 1_i4
    cand_ids(ncand) = cand_k
  end subroutine append_candidate_id

  !> \brief Update the current best id and metric with deterministic tie handling.
  subroutine update_best_metric(best_k, best_metric, cand_k, cand_metric)
    implicit none
    integer(i8), intent(inout) :: best_k        !< Current best id.
    real(dp), intent(inout) :: best_metric      !< Current best metric value.
    integer(i8), intent(in) :: cand_k           !< Candidate id to compare.
    real(dp), intent(in) :: cand_metric         !< Candidate metric to compare.

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
  end subroutine update_best_metric

  !> \brief Calculate the intersection point of two infinite lines.
  subroutine intersection(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, x, y)
    real(dp), intent(in) :: p1x  !< X coordinate of line P endpoint 1.
    real(dp), intent(in) :: p1y  !< Y coordinate of line P endpoint 1.
    real(dp), intent(in) :: p2x  !< X coordinate of line P endpoint 2.
    real(dp), intent(in) :: p2y  !< Y coordinate of line P endpoint 2.
    real(dp), intent(in) :: q1x  !< X coordinate of line Q endpoint 1.
    real(dp), intent(in) :: q1y  !< Y coordinate of line Q endpoint 1.
    real(dp), intent(in) :: q2x  !< X coordinate of line Q endpoint 2.
    real(dp), intent(in) :: q2y  !< Y coordinate of line Q endpoint 2.
    real(dp), intent(out) :: x   !< X coordinate of the intersection point.
    real(dp), intent(out) :: y   !< Y coordinate of the intersection point.
    real(dp) :: denom, det1, det2

    denom = (p1x-p2x)*(q1y-q2y) - (p1y-p2y)*(q1x-q2x)
    det1 = p1x*p2y-p1y*p2x
    det2 = q1x*q2y-q1y*q2x
    x = (det1*(q1x-q2x) - det2*(p1x-p2x)) / denom
    y = (det1*(q1y-q2y) - det2*(p1y-p2y)) / denom
  end subroutine intersection

  !> \brief Derive the outer extent of a coarsened grid from a finer grid.
  subroutine calculate_coarse_extent(nx_in,  ny_in,  xllcorner_in,  yllcorner_in,  cellsize_in,  target_resolution, &
                                     nx_out, ny_out, xllcorner_out, yllcorner_out, cellsize_out, tol)
    implicit none

    integer(i4), intent(in) :: nx_in                  !< Number of cells along x on the input grid.
    integer(i4), intent(in) :: ny_in                  !< Number of cells along y on the input grid.
    real(dp), intent(in) :: xllcorner_in              !< Lower-left x coordinate of the input grid.
    real(dp), intent(in) :: yllcorner_in              !< Lower-left y coordinate of the input grid.
    real(dp), intent(in) :: cellsize_in               !< Cell size of the input grid.
    real(dp), intent(in) :: target_resolution         !< Requested coarse-grid cell size.
    integer(i4), intent(out) :: nx_out                !< Number of cells along x on the coarse grid.
    integer(i4), intent(out) :: ny_out                !< Number of cells along y on the coarse grid.
    real(dp), intent(out) :: xllcorner_out            !< Lower-left x coordinate of the coarse grid.
    real(dp), intent(out) :: yllcorner_out            !< Lower-left y coordinate of the coarse grid.
    real(dp), intent(out) :: cellsize_out             !< Cell size of the coarse grid.
    real(dp), intent(in), optional :: tol             !< Tolerance for the resolution ratio check.

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

  !> \brief Validate that a NetCDF axis variable is uniformly spaced.
  subroutine check_uniform_axis(var, cellsize, origin, increasing, tol)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var            !< NetCDF axis variable to validate.
    real(dp), optional, intent(out) :: cellsize    !< Absolute spacing of the axis points.
    real(dp), optional, intent(out) :: origin      !< Lower bound origin implied by the axis.
    logical, optional, intent(out) :: increasing   !< Whether the axis increases with index.
    real(dp), intent(in), optional :: tol          !< Absolute tolerance for uniformity checks.
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

  !> \brief Check whether a NetCDF variable represents a projection x axis.
  logical function is_x_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
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

  !> \brief Check whether a NetCDF variable represents a projection y axis.
  logical function is_y_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
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

  !> \brief Check whether a NetCDF variable represents a vertical axis.
  logical function is_z_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
    character(len=256) :: tmp_str

    is_z_axis = .false.
    if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "Z") is_z_axis = .true.
    else if (var%hasAttribute("positive")) then
      is_z_axis = .true.
    end if
  end function is_z_axis

  !> \brief Check whether a NetCDF variable represents a time axis.
  logical function is_t_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
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

  !> \brief Check whether a NetCDF variable represents longitude coordinates.
  logical function is_lon_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
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

  !> \brief Check whether a NetCDF variable represents latitude coordinates.
  logical function is_lat_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to inspect.
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

  !> \brief Create a mask from a NetCDF variable.
  !> \details Non-missing values become `.true.` and missing values become `.false.`.
  !! Supports 2D data and reads only the first layer of higher dimensions.
  subroutine mask_from_var(var, mask, data, flip_y)
    use mo_netcdf, only : NcVariable
    use mo_utils, only : ne
    use ieee_arithmetic, only : ieee_is_nan
    implicit none
    type(NcVariable), intent(in) :: var                               !< NetCDF variable to read.
    logical, dimension(:, :), allocatable, intent(out) :: mask        !< Derived mask.
    type(data_t), intent(inout), optional :: data                     !< Optional container receiving the raw variable data.
    logical, intent(in), optional :: flip_y                           !< Whether to flip the y dimension after reading.
    integer(i1), dimension(:, :), allocatable :: data_i1
    integer(i1) :: fv_i1
    integer(i2), dimension(:, :), allocatable :: data_i2
    integer(i2) :: fv_i2
    integer(i4), dimension(:, :), allocatable :: data_i4
    integer(i4) :: fv_i4
    integer(i8), dimension(:, :), allocatable :: data_i8
    integer(i8) :: fv_i8
    real(sp), dimension(:, :), allocatable :: data_sp
    real(sp) :: fv_sp
    real(dp), dimension(:, :), allocatable :: data_dp
    real(dp) :: fv_dp
    integer(i4), dimension(:), allocatable :: shp, start, cnt
    character(:), allocatable :: dtype, name
    integer(i4) :: i, nx, ny
    logical :: flip_y_

    flip_y_ = optval(flip_y, default=.false.)

    shp = var%getShape()
    allocate(start(size(shp)), source=1_i4)
    allocate(cnt(size(shp)), source=1_i4)
    cnt(:2) = shp(:2)
    dtype = trim(var%getDtype())

    if (present(data)) then
      if (allocated(data%dtype)) then
        dtype = trim(data%dtype)
      else
        data%dtype = dtype
      end if
      call data%deallocate()
    end if

    select case (dtype)
      case ("i8")
        call var%getData(data_i1, start=start, cnt=cnt)
        if (flip_y_) call flip(data_i1, iDim=2)
        call var%getFillValue(fv_i1)
        nx = size(data_i1, 1)
        ny = size(data_i1, 2)
        allocate(mask(nx, ny))
        !$omp parallel do default(shared) schedule(static)
        do i = 1_i4, ny
          mask(:,i) = data_i1(:,i) /= fv_i1
        end do
        !$omp end parallel do
        if (present(data)) call move_alloc(data_i1, data%data_i1)
      case ("i16")
        call var%getData(data_i2, start=start, cnt=cnt)
        if (flip_y_) call flip(data_i2, iDim=2)
        call var%getFillValue(fv_i2)
        nx = size(data_i2, 1)
        ny = size(data_i2, 2)
        allocate(mask(nx, ny))
        !$omp parallel do default(shared) schedule(static)
        do i = 1_i4, ny
          mask(:,i) = data_i2(:,i) /= fv_i2
        end do
        !$omp end parallel do
        if (present(data)) call move_alloc(data_i2, data%data_i2)
      case ("i32")
        call var%getData(data_i4, start=start, cnt=cnt)
        if (flip_y_) call flip(data_i4, iDim=2)
        call var%getFillValue(fv_i4)
        nx = size(data_i4, 1)
        ny = size(data_i4, 2)
        allocate(mask(nx, ny))
        !$omp parallel do default(shared) schedule(static)
        do i = 1_i4, ny
          mask(:,i) = data_i4(:,i) /= fv_i4
        end do
        !$omp end parallel do
        if (present(data)) call move_alloc(data_i4, data%data_i4)
      case ("i64")
        call var%getData(data_i8, start=start, cnt=cnt)
        if (flip_y_) call flip(data_i8, iDim=2)
        call var%getFillValue(fv_i8)
        nx = size(data_i8, 1)
        ny = size(data_i8, 2)
        allocate(mask(nx, ny))
        !$omp parallel do default(shared) schedule(static)
        do i = 1_i4, ny
          mask(:,i) = data_i8(:,i) /= fv_i8
        end do
        !$omp end parallel do
        if (present(data)) call move_alloc(data_i8, data%data_i8)
      case ("f32")
        call var%getData(data_sp, start=start, cnt=cnt)
        if (flip_y_) call flip(data_sp, iDim=2)
        call var%getFillValue(fv_sp)
        nx = size(data_sp, 1)
        ny = size(data_sp, 2)
        allocate(mask(nx, ny))
        if (ieee_is_nan(fv_sp)) then
          !$omp parallel do default(shared) schedule(static)
          do i = 1_i4, ny
            mask(:,i) = .not. ieee_is_nan(data_sp(:,i))
          end do
          !$omp end parallel do
        else
          !$omp parallel do default(shared) schedule(static)
          do i = 1_i4, ny
            mask(:,i) = ne(data_sp(:,i), fv_sp)
          end do
          !$omp end parallel do
        end if
        if (present(data)) call move_alloc(data_sp, data%data_sp)
      case ("f64")
        call var%getData(data_dp, start=start, cnt=cnt)
        if (flip_y_) call flip(data_dp, iDim=2)
        call var%getFillValue(fv_dp)
        nx = size(data_dp, 1)
        ny = size(data_dp, 2)
        allocate(mask(nx, ny))
        if (ieee_is_nan(fv_dp)) then
          !$omp parallel do default(shared) schedule(static)
          do i = 1_i4, ny
            mask(:,i) = .not. ieee_is_nan(data_dp(:,i))
          end do
          !$omp end parallel do
        else
          !$omp parallel do default(shared) schedule(static)
          do i = 1_i4, ny
            mask(:,i) = ne(data_dp(:,i), fv_dp)
          end do
          !$omp end parallel do
        end if
        if (present(data)) call move_alloc(data_dp, data%data_dp)
      case default
        name = trim(var%getName())
        call error_message("mask_from_var: Unsupported variable type: ", name, " - dtype: ", dtype) ! LCOV_EXCL_LINE
    end select
  end subroutine mask_from_var

  !> \brief Read data from a NetCDF variable into a generic data container.
  subroutine data_from_var(var, data, flip_y)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var    !< NetCDF variable to read.
    type(data_t), intent(inout) :: data    !< Target generic data container.
    logical, intent(in), optional :: flip_y !< Whether to flip the y dimension after reading.
    integer(i4), dimension(:), allocatable :: shp, start, cnt
    character(:), allocatable :: name
    logical :: flip_y_

    flip_y_ = optval(flip_y, default=.false.)
    shp = var%getShape()
    allocate(start(size(shp)), source=1_i4)
    allocate(cnt(size(shp)), source=1_i4)
    cnt(:2) = shp(:2)
    if (.not. allocated(data%dtype)) data%dtype = trim(var%getDtype())
    call data%deallocate()

    select case (data%dtype)
      case ("i8")
        call var%getData(data%data_i1, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_i1, iDim=2)
      case ("i16")
        call var%getData(data%data_i2, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_i2, iDim=2)
      case ("i32")
        call var%getData(data%data_i4, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_i4, iDim=2)
      case ("i64")
        call var%getData(data%data_i8, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_i8, iDim=2)
      case ("f32")
        call var%getData(data%data_sp, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_sp, iDim=2)
      case ("f64")
        call var%getData(data%data_dp, start=start, cnt=cnt)
        if (flip_y_) call flip(data%data_dp, iDim=2)
      case default
        name = trim(var%getName())
        call error_message("data_from_var: Unsupported variable type: ", name, " - dtype: ", data%dtype) ! LCOV_EXCL_LINE
    end select
  end subroutine data_from_var

#endif

  !> \brief Check that two cell sizes differ by an integer scaling factor.
  subroutine check_factor(fine_cellsize, coarse_cellsize, cellfactor, rounded, factor, tol)
    real(dp), intent(in) :: fine_cellsize               !< Cell size of the fine grid.
    real(dp), intent(in) :: coarse_cellsize             !< Cell size of the coarse grid.
    real(dp), optional, intent(out) :: cellfactor       !< Exact floating-point cell-size ratio.
    real(dp), optional, intent(out) :: rounded          !< Rounded floating-point ratio.
    integer(i4), optional, intent(out) :: factor        !< Integer scaling factor.
    real(dp), intent(in), optional :: tol               !< Allowed deviation from an integer ratio.
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

  !> \brief Read a double-precision ESRI ASCII grid into an xy-ordered array.
  subroutine read_ascii_grid_dp(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    use mo_utils, only : eq
    implicit none

    character(len = *), intent(in) :: path                                   !< Path to the ASCII grid file.
    real(dp), dimension(:, :), allocatable, intent(out) :: data              !< Read data array in xy order.
    logical, dimension(:, :), allocatable, intent(out), optional :: mask     !< Optional nodata mask derived from the file.
    integer(i4), intent(in), optional :: ref_nrows                           !< Optional reference number of rows.
    integer(i4), intent(in), optional :: ref_ncols                           !< Optional reference number of columns.
    real(dp), intent(in), optional :: ref_xllcorner                          !< Optional reference lower-left x coordinate.
    real(dp), intent(in), optional :: ref_yllcorner                          !< Optional reference lower-left y coordinate.
    real(dp), intent(in), optional :: ref_cellsize                           !< Optional reference cell size.
    integer(i4), intent(in), optional :: y_direction                         !< Desired y-axis direction for the output arrays.

    integer(i4) :: file_nrows, file_ncols
    real(dp) :: file_xllcorner, file_yllcorner, file_cellsize, file_nodata
    integer(i4) :: i, j, fileunit, hlines
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == bottom_up

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

  !> \brief Read a 4-byte integer ESRI ASCII grid into an xy-ordered array.
  subroutine read_ascii_grid_i4(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    implicit none

    character(len = *), intent(in) :: path                                   !< Path to the ASCII grid file.
    integer(i4), dimension(:, :), allocatable, intent(out) :: data           !< Read data array in xy order.
    logical, dimension(:, :), allocatable, intent(out), optional :: mask     !< Optional nodata mask derived from the file.
    integer(i4), intent(in), optional :: ref_nrows                           !< Optional reference number of rows.
    integer(i4), intent(in), optional :: ref_ncols                           !< Optional reference number of columns.
    real(dp), intent(in), optional :: ref_xllcorner                          !< Optional reference lower-left x coordinate.
    real(dp), intent(in), optional :: ref_yllcorner                          !< Optional reference lower-left y coordinate.
    real(dp), intent(in), optional :: ref_cellsize                           !< Optional reference cell size.
    integer(i4), intent(in), optional :: y_direction                         !< Desired y-axis direction for the output arrays.

    integer(i4) :: file_nrows, file_ncols
    real(dp) :: file_xllcorner, file_yllcorner, file_cellsize, file_nodata
    integer(i4) :: i, j, fileunit, header_size
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == bottom_up

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

  !> \brief Read and optionally validate the header of an ESRI ASCII grid file.
  subroutine read_ascii_header( &
    path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, &
    ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, header_size)

    use mo_os, only : check_path_isfile
    use mo_string_utils, only : tolower
    implicit none

    character(len = *), intent(in) :: path                !< Path to the ASCII grid file.
    integer(i4), intent(out) :: nrows                     !< Number of rows read from the header.
    integer(i4), intent(out) :: ncols                     !< Number of columns read from the header.
    real(dp), intent(out) :: xllcorner                    !< Lower-left x coordinate from the header.
    real(dp), intent(out) :: yllcorner                    !< Lower-left y coordinate from the header.
    real(dp), intent(out) :: cellsize                     !< Cell size read from the header.
    real(dp), intent(out), optional :: nodata             !< Optional nodata marker from the header.
    integer(i4), intent(in), optional :: ref_nrows        !< Optional reference number of rows.
    integer(i4), intent(in), optional :: ref_ncols        !< Optional reference number of columns.
    real(dp), intent(in), optional :: ref_xllcorner       !< Optional reference lower-left x coordinate.
    real(dp), intent(in), optional :: ref_yllcorner       !< Optional reference lower-left y coordinate.
    real(dp), intent(in), optional :: ref_cellsize        !< Optional reference cell size.
    integer(i4), optional, intent(out) :: header_size     !< Number of header lines consumed.

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

  !> \brief Write a double-precision ESRI ASCII grid from an xy- or yx-ordered array.
  subroutine write_ascii_grid_dp(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    character(len=*), intent(in) :: path             !< Output path for the ASCII grid file.
    integer(i4), intent(in) :: ncols                 !< Number of columns to write.
    integer(i4), intent(in) :: nrows                 !< Number of rows to write.
    real(dp), intent(in) :: xllcorner                !< Lower-left x coordinate of the grid.
    real(dp), intent(in) :: yllcorner                !< Lower-left y coordinate of the grid.
    real(dp), intent(in) :: cellsize                 !< Cell size of the grid.
    real(dp), intent(in) :: nodata                   !< Nodata marker to write.
    real(dp), intent(in), optional :: data(:,:)      !< Optional data array to write.
    integer(i4), intent(in), optional :: y_direction !< Direction of the y axis in the supplied data.
    logical, intent(in), optional :: is_xy           !< Whether the supplied data uses xy ordering.

    integer(i4) :: i, j, io, ierr
    logical :: is_bottom_up, is_xy_

    is_bottom_up = .true.
    if (present(y_direction)) is_bottom_up = y_direction == bottom_up
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

  !> \brief Write a 4-byte integer ESRI ASCII grid from an xy- or yx-ordered array.
  subroutine write_ascii_grid_i4(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    character(len=*), intent(in) :: path               !< Output path for the ASCII grid file.
    integer(i4), intent(in) :: ncols                   !< Number of columns to write.
    integer(i4), intent(in) :: nrows                   !< Number of rows to write.
    real(dp), intent(in) :: xllcorner                  !< Lower-left x coordinate of the grid.
    real(dp), intent(in) :: yllcorner                  !< Lower-left y coordinate of the grid.
    real(dp), intent(in) :: cellsize                   !< Cell size of the grid.
    integer(i4), intent(in) :: nodata                  !< Nodata marker to write.
    integer(i4), intent(in), optional :: data(:,:)     !< Optional data array to write.
    integer(i4), intent(in), optional :: y_direction   !< Direction of the y axis in the supplied data.
    logical, intent(in), optional :: is_xy             !< Whether the supplied data uses xy ordering.

    integer(i4) :: i, j, io, ierr
    logical :: is_bottom_up, is_xy_

    is_bottom_up = .false.
    if (present(y_direction)) is_bottom_up = y_direction == bottom_up
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

  !> \brief Map one fine-grid cell index pair to its containing coarse-grid cell.
  pure subroutine coarse_ij(factor, fine_i, fine_j, fine_y_dir, fine_ny, coarse_y_dir, coarse_ny, coarse_i, coarse_j)
    integer(i4), intent(in) :: factor        !< Integer coarsening factor.
    integer(i4), intent(in) :: fine_i        !< Fine-grid x index.
    integer(i4), intent(in) :: fine_j        !< Fine-grid y index.
    integer(i4), intent(in) :: fine_y_dir    !< Fine-grid y-axis direction selector.
    integer(i4), intent(in) :: fine_ny       !< Number of rows on the fine grid.
    integer(i4), intent(in) :: coarse_y_dir  !< Coarse-grid y-axis direction selector.
    integer(i4), intent(in) :: coarse_ny     !< Number of rows on the coarse grid.
    integer(i4), intent(out) :: coarse_i     !< Containing coarse-grid x index.
    integer(i4), intent(out) :: coarse_j     !< Containing coarse-grid y index.
    integer(i4) :: j

    j = fine_j
    if (fine_y_dir == top_down) j = fine_ny - j + 1_i4
    coarse_j = (j - 1_i4) / factor + 1_i4
    coarse_i = (fine_i - 1_i4) / factor + 1_i4
    if (coarse_y_dir == top_down) coarse_j = coarse_ny - coarse_j + 1_i4
  end subroutine coarse_ij

  !> \brief Compute fine-grid index bounds covered by one coarse-grid cell.
  pure subroutine id_bounds(factor, coarse_i, coarse_j, coarse_y_dir, coarse_ny, fine_y_dir, fine_nx, fine_ny, i_lb, i_ub, j_lb, j_ub)
    integer(i4), intent(in) :: factor         !< Integer coarsening factor.
    integer(i4), intent(in) :: coarse_i       !< Coarse-grid x index.
    integer(i4), intent(in) :: coarse_j       !< Coarse-grid y index.
    integer(i4), intent(in) :: coarse_y_dir   !< Coarse-grid y-axis direction selector.
    integer(i4), intent(in) :: coarse_ny      !< Number of rows on the coarse grid.
    integer(i4), intent(in) :: fine_y_dir     !< Fine-grid y-axis direction selector.
    integer(i4), intent(in) :: fine_nx        !< Number of columns on the fine grid.
    integer(i4), intent(in) :: fine_ny        !< Number of rows on the fine grid.
    integer(i4), intent(out) :: i_lb          !< Inclusive lower fine-grid x bound.
    integer(i4), intent(out) :: i_ub          !< Inclusive upper fine-grid x bound.
    integer(i4), intent(out) :: j_lb          !< Inclusive lower fine-grid y bound.
    integer(i4), intent(out) :: j_ub          !< Inclusive upper fine-grid y bound.
    integer(i4) :: temp, ic, jc

    ic = coarse_i
    i_lb = (ic - 1) * factor + 1
    i_ub = min(ic * factor, fine_nx)

    jc = coarse_j
    if (coarse_y_dir == top_down) jc = coarse_ny - coarse_j + 1
    j_lb = (jc - 1) * factor + 1
    j_ub = min(jc * factor, fine_ny)

    if (fine_y_dir == top_down) then
      temp = j_lb
      j_lb = fine_ny - j_ub + 1
      j_ub = fine_ny - temp + 1
    end if
  end subroutine id_bounds

  !> \brief Compute the spherical great-circle distance between two lon/lat points.
  pure real(dp) function dist_latlon(lat1, lon1, lat2, lon2)
    real(dp), intent(in) :: lat1 !< Latitude of the first point in degrees.
    real(dp), intent(in) :: lon1 !< Longitude of the first point in degrees.
    real(dp), intent(in) :: lat2 !< Latitude of the second point in degrees.
    real(dp), intent(in) :: lon2 !< Longitude of the second point in degrees.
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
