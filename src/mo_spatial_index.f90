!> \file mo_spatial_index.f90
!> \copydoc mo_spatial_index

!> \brief Exact KD-tree spatial index for Cartesian and spherical point sets.
!> \details This module provides a transient nearest-neighbor index that can be
!! initialized from Euclidean coordinates of arbitrary runtime dimension or from
!! longitude/latitude coordinates. Spherical coordinates are converted to 3D
!! unit vectors so the KD-tree can operate on chord distance while preserving
!! the exact nearest-neighbor result for great-circle distance. Equal-distance
!! ties are resolved by the smaller point id.
!> \authors Sebastian Mueller
!> \date    Mar 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_spatial_index

  use mo_kind, only: i4, i8, dp
  use mo_grid_constants, only: cartesian, spherical
  use mo_constants, only: deg2rad_dp
  use mo_utils, only: is_close
  use mo_message, only: error_message

  implicit none

  private

  integer(i4), parameter :: lonlat_ndim = 2_i4
  integer(i4), parameter :: spherical_storage_ndim = 3_i4
  integer(i8), parameter :: loop_parallel_min_n = 2048_i8
  integer(i8), parameter :: build_task_min_n = 16384_i8
  integer(i4), parameter :: build_task_max_depth = 4_i4

  !> \class spatial_index_t
  !> \brief Exact transient KD-tree index for nearest-neighbor queries.
  !> \details The index stores coordinates column-wise in the implicit KD-tree
  !! layout used during search. Cartesian indices keep the original point
  !! dimension, while spherical indices accept lon/lat input and store 3D unit
  !! vectors internally.
  type, public :: spatial_index_t
    integer(i4) :: ndim = 0_i4                    !< External point dimension expected by the public query API.
    integer(i4) :: storage_ndim = 0_i4            !< Internal point dimension stored in \ref pts.
    integer(i4) :: coordsys = cartesian           !< Coordinate mode, either Cartesian or spherical lon/lat.
    real(dp), allocatable :: pts(:, :)            !< Point coordinates stored column-wise in KD-tree order.
    integer(i8), allocatable :: point_ids(:)      !< User point ids stored alongside \ref pts.
    integer(i4), allocatable :: split_axis(:)     !< Splitting axis used by each implicit KD-tree node.
  contains
    procedure, public :: clear => spatial_index_clear
    procedure, public :: init => spatial_index_init
    procedure, public :: init_lonlat => spatial_index_init_lonlat
    procedure, public :: nearest_id => spatial_index_nearest_id
    procedure, public :: nearest_ids => spatial_index_nearest_ids
    procedure, public :: nearest_id_lonlat => spatial_index_nearest_id_lonlat
    procedure, public :: nearest_ids_lonlat => spatial_index_nearest_ids_lonlat
  end type spatial_index_t

contains

  !> \brief Release all allocated storage and reset the index to Cartesian empty state.
  subroutine spatial_index_clear(this)
    class(spatial_index_t), intent(inout) :: this

    if (allocated(this%pts)) deallocate(this%pts)
    if (allocated(this%point_ids)) deallocate(this%point_ids)
    if (allocated(this%split_axis)) deallocate(this%split_axis)
    this%ndim = 0_i4
    this%storage_ndim = 0_i4
    this%coordsys = cartesian
  end subroutine spatial_index_clear

  !> \brief Build a Cartesian KD-tree from Euclidean point coordinates.
  !> \details `points` must have shape `(npts, ndim)` and `point_ids` must have
  !! length `npts`. The point dimension is taken from the second array extent at
  !! runtime.
  subroutine spatial_index_init(this, points, point_ids)
    class(spatial_index_t), intent(inout) :: this
    real(dp), intent(in) :: points(:, :)
    integer(i8), intent(in) :: point_ids(:)

    integer(i8) :: ipt, npts
    integer(i4) :: idim, ndim

    npts = size(points, 1, kind=i8)
    ndim = size(points, 2, kind=i4)
    if (npts /= size(point_ids, kind=i8)) &
      call error_message("spatial_index % init: point count and point_ids size do not match.") ! LCOV_EXCL_LINE
    if (ndim < 1_i4) call error_message("spatial_index % init: need at least one point dimension.") ! LCOV_EXCL_LINE

    call this%clear()
    this%ndim = ndim
    this%storage_ndim = ndim
    this%coordsys = cartesian

    allocate(this%pts(this%storage_ndim, npts))
    allocate(this%point_ids(npts))
    allocate(this%split_axis(npts))

    !$omp parallel do default(shared) private(ipt,idim) schedule(static) if(npts >= loop_parallel_min_n)
    do ipt = 1_i8, npts
      do idim = 1_i4, this%storage_ndim
        this%pts(idim, ipt) = points(ipt, idim)
      end do
      this%point_ids(ipt) = point_ids(ipt)
    end do
    !$omp end parallel do

    if (npts > 0_i8) call spatial_index_build_root(this, npts)
  end subroutine spatial_index_init

  !> \brief Build a spherical KD-tree from longitude/latitude coordinates.
  !> \details `coords` must have shape `(npts,2)` with longitude in column 1
  !! and latitude in column 2, both in degrees. Coordinates are converted to 3D
  !! unit vectors before the KD-tree is built.
  subroutine spatial_index_init_lonlat(this, coords, point_ids)
    class(spatial_index_t), intent(inout) :: this
    real(dp), intent(in) :: coords(:, :)
    integer(i8), intent(in) :: point_ids(:)

    integer(i8) :: ipt, npts

    npts = size(coords, 1, kind=i8)
    if (size(coords, 2) /= lonlat_ndim) &
      call error_message("spatial_index % init_lonlat: coordinates need shape (:,2).") ! LCOV_EXCL_LINE
    if (npts /= size(point_ids, kind=i8)) &
      call error_message("spatial_index % init_lonlat: point count and point_ids size do not match.") ! LCOV_EXCL_LINE

    call this%clear()
    this%ndim = lonlat_ndim
    this%storage_ndim = spherical_storage_ndim
    this%coordsys = spherical

    allocate(this%pts(this%storage_ndim, npts))
    allocate(this%point_ids(npts))
    allocate(this%split_axis(npts))

    !$omp parallel do default(shared) private(ipt) schedule(static) if(npts >= loop_parallel_min_n)
    do ipt = 1_i8, npts
      call lonlat_to_unitvec(coords(ipt, 1), coords(ipt, 2), this%pts(:, ipt))
      this%point_ids(ipt) = point_ids(ipt)
    end do
    !$omp end parallel do

    if (npts > 0_i8) call spatial_index_build_root(this, npts)
  end subroutine spatial_index_init_lonlat

  !> \brief Return the id of the nearest Cartesian point to a single query.
  integer(i8) function spatial_index_nearest_id(this, query) result(point_id)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: query(:)

    real(dp) :: target(size(query))

    if (this%coordsys == spherical) &
      call error_message("spatial_index % nearest_id: spherical indices need nearest_id_lonlat.") ! LCOV_EXCL_LINE
    if (size(query, kind=i4) /= this%storage_ndim) &
      call error_message("spatial_index % nearest_id: query dimension does not match index.") ! LCOV_EXCL_LINE

    target = query
    point_id = spatial_index_nearest_target(this, target)
  end function spatial_index_nearest_id

  !> \brief Return the ids of the nearest Cartesian points for a batch of queries.
  !> \details `queries` must have shape `(nquery, ndim)` matching the dimension
  !! used during \ref init.
  function spatial_index_nearest_ids(this, queries) result(point_ids)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: queries(:, :)
    integer(i8) :: point_ids(size(queries, 1))

    integer(i8) :: iquery
    integer(i4) :: idim
    real(dp) :: target(size(queries, 2))

    if (this%coordsys == spherical) &
      call error_message("spatial_index % nearest_ids: spherical indices need nearest_ids_lonlat.") ! LCOV_EXCL_LINE
    if (size(queries, 2) /= this%storage_ndim) &
      call error_message("spatial_index % nearest_ids: query dimension does not match index.") ! LCOV_EXCL_LINE

    !$omp parallel do default(shared) private(iquery,idim,target) schedule(static) &
    !$omp& if(size(queries, 1, kind=i8) >= loop_parallel_min_n)
    do iquery = 1_i8, size(queries, 1, kind=i8)
      do idim = 1_i4, size(queries, 2)
        target(idim) = queries(iquery, idim)
      end do
      point_ids(iquery) = spatial_index_nearest_target(this, target)
    end do
    !$omp end parallel do
  end function spatial_index_nearest_ids

  !> \brief Return the id of the nearest spherical point to one lon/lat query.
  integer(i8) function spatial_index_nearest_id_lonlat(this, lon, lat) result(point_id)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: lon
    real(dp), intent(in) :: lat

    real(dp) :: target(spherical_storage_ndim)

    if (this%coordsys /= spherical) &
      call error_message("spatial_index % nearest_id_lonlat: index is not spherical.") ! LCOV_EXCL_LINE

    call lonlat_to_unitvec(lon, lat, target)
    point_id = spatial_index_nearest_target(this, target)
  end function spatial_index_nearest_id_lonlat

  !> \brief Return the ids of the nearest spherical points for a batch of lon/lat queries.
  !> \details `coords` must have shape `(nquery,2)` with longitude in column 1
  !! and latitude in column 2, both in degrees.
  function spatial_index_nearest_ids_lonlat(this, coords) result(point_ids)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: coords(:, :)
    integer(i8) :: point_ids(size(coords, 1))

    integer(i8) :: iquery
    real(dp) :: target(spherical_storage_ndim)

    if (this%coordsys /= spherical) &
      call error_message("spatial_index % nearest_ids_lonlat: index is not spherical.") ! LCOV_EXCL_LINE
    if (size(coords, 2) /= lonlat_ndim) &
      call error_message("spatial_index % nearest_ids_lonlat: coordinates need shape (:,2).") ! LCOV_EXCL_LINE

    !$omp parallel do default(shared) private(iquery,target) schedule(static) if(size(coords, 1, kind=i8) >= loop_parallel_min_n)
    do iquery = 1_i8, size(coords, 1, kind=i8)
      call lonlat_to_unitvec(coords(iquery, 1), coords(iquery, 2), target)
      point_ids(iquery) = spatial_index_nearest_target(this, target)
    end do
    !$omp end parallel do
  end function spatial_index_nearest_ids_lonlat

  !> \brief Query the KD-tree with a target already expressed in storage space.
  integer(i8) function spatial_index_nearest_target(this, target) result(point_id)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: target(:)

    real(dp) :: best_dist2

    point_id = 0_i8
    if (.not. allocated(this%point_ids)) return
    if (size(this%point_ids, kind=i8) < 1_i8) return
    if (size(target, kind=i4) /= this%storage_ndim) &
      call error_message("spatial_index % nearest_target: target dimension does not match index.") ! LCOV_EXCL_LINE

    best_dist2 = huge(1.0_dp)
    call spatial_index_search(this, 1_i8, size(this%point_ids, kind=i8), target, point_id, best_dist2)
  end function spatial_index_nearest_target

  !> \brief Enter one OpenMP region to build the KD-tree and spawn top-level subtree tasks.
  subroutine spatial_index_build_root(this, npts)
    class(spatial_index_t), intent(inout) :: this
    integer(i8), intent(in) :: npts

    if (npts < 1_i8) return

    !$omp parallel default(shared) if(npts >= build_task_min_n)
    !$omp single
    call spatial_index_build(this, 1_i8, npts, 0_i4)
    !$omp end single
    !$omp end parallel
  end subroutine spatial_index_build_root

  !> \brief Build the implicit KD-tree layout in place over the stored points.
  recursive subroutine spatial_index_build(this, lo, hi, depth)
    class(spatial_index_t), intent(inout) :: this
    integer(i8), intent(in) :: lo
    integer(i8), intent(in) :: hi
    integer(i4), intent(in) :: depth

    integer(i8) :: mid, left_size, right_size
    logical :: spawn_left, spawn_right

    if (lo > hi) return

    mid = lo + (hi - lo) / 2_i8
    this%split_axis(mid) = 1_i4 + mod(depth, this%storage_ndim)
    call spatial_index_quickselect(this%pts, this%point_ids, lo, hi, mid, this%split_axis(mid))

    left_size = mid - lo
    right_size = hi - mid
    spawn_left = left_size >= build_task_min_n .and. depth < build_task_max_depth
    spawn_right = right_size >= build_task_min_n .and. depth < build_task_max_depth

    if (lo < mid) then
      if (spawn_left) then
        !$omp task default(shared) firstprivate(lo,mid,depth)
        call spatial_index_build(this, lo, mid - 1_i8, depth + 1_i4)
        !$omp end task
      else
        call spatial_index_build(this, lo, mid - 1_i8, depth + 1_i4)
      end if
    end if
    if (mid < hi) then
      if (spawn_right) then
        !$omp task default(shared) firstprivate(mid,hi,depth)
        call spatial_index_build(this, mid + 1_i8, hi, depth + 1_i4)
        !$omp end task
      else
        call spatial_index_build(this, mid + 1_i8, hi, depth + 1_i4)
      end if
    end if
    if (spawn_left .or. spawn_right) then
      !$omp taskwait
    end if
  end subroutine spatial_index_build

  !> \brief Recursively search the implicit KD-tree for the nearest stored point.
  recursive subroutine spatial_index_search(this, lo, hi, target, best_id, best_dist2)
    class(spatial_index_t), intent(in) :: this
    integer(i8), intent(in) :: lo
    integer(i8), intent(in) :: hi
    real(dp), intent(in) :: target(:)
    integer(i8), intent(inout) :: best_id
    real(dp), intent(inout) :: best_dist2

    integer(i8) :: mid
    integer(i4) :: axis
    real(dp) :: delta, plane_dist2, cand_dist2

    if (lo > hi) return

    mid = lo + (hi - lo) / 2_i8
    axis = this%split_axis(mid)
    cand_dist2 = spatial_index_point_dist2(this, mid, target)
    call spatial_index_update_best(best_id, best_dist2, this%point_ids(mid), cand_dist2)

    delta = target(axis) - this%pts(axis, mid)
    plane_dist2 = delta * delta

    if (delta <= 0.0_dp) then
      call spatial_index_search(this, lo, mid - 1_i8, target, best_id, best_dist2)
      if (plane_dist2 < best_dist2 .or. is_close(plane_dist2, best_dist2)) then
        call spatial_index_search(this, mid + 1_i8, hi, target, best_id, best_dist2)
      end if
    else
      call spatial_index_search(this, mid + 1_i8, hi, target, best_id, best_dist2)
      if (plane_dist2 < best_dist2 .or. is_close(plane_dist2, best_dist2)) then
        call spatial_index_search(this, lo, mid - 1_i8, target, best_id, best_dist2)
      end if
    end if
  end subroutine spatial_index_search

  !> \brief Partition stored points in place so `kth` holds the axis median.
  recursive subroutine spatial_index_quickselect(pts, point_ids, lo, hi, kth, axis)
    real(dp), intent(inout) :: pts(:, :)
    integer(i8), intent(inout) :: point_ids(:)
    integer(i8), intent(in) :: lo
    integer(i8), intent(in) :: hi
    integer(i8), intent(in) :: kth
    integer(i4), intent(in) :: axis

    integer(i8) :: i, j, pivot_idx
    real(dp) :: pivot

    if (lo >= hi) return

    pivot_idx = lo + (hi - lo) / 2_i8
    pivot = pts(axis, pivot_idx)
    i = lo
    j = hi

    do
      do while (i <= hi)
        if (pts(axis, i) >= pivot) exit
        i = i + 1_i8
      end do
      do while (j >= lo)
        if (pts(axis, j) <= pivot) exit
        j = j - 1_i8
      end do
      if (i > j) exit
      call spatial_index_swap_points(pts, point_ids, i, j)
      i = i + 1_i8
      j = j - 1_i8
    end do

    if (kth <= j) call spatial_index_quickselect(pts, point_ids, lo, j, kth, axis)
    if (kth >= i) call spatial_index_quickselect(pts, point_ids, i, hi, kth, axis)
  end subroutine spatial_index_quickselect

  !> \brief Swap two stored points and their associated ids.
  subroutine spatial_index_swap_points(pts, point_ids, lhs, rhs)
    real(dp), intent(inout) :: pts(:, :)
    integer(i8), intent(inout) :: point_ids(:)
    integer(i8), intent(in) :: lhs
    integer(i8), intent(in) :: rhs

    real(dp) :: tmp_pt(size(pts, 1))
    integer(i8) :: tmp_id

    if (lhs == rhs) return

    tmp_pt = pts(:, lhs)
    pts(:, lhs) = pts(:, rhs)
    pts(:, rhs) = tmp_pt

    tmp_id = point_ids(lhs)
    point_ids(lhs) = point_ids(rhs)
    point_ids(rhs) = tmp_id
  end subroutine spatial_index_swap_points

  !> \brief Update the current nearest-neighbor candidate with distance/id tie-breaking.
  subroutine spatial_index_update_best(best_id, best_dist2, cand_id, cand_dist2)
    integer(i8), intent(inout) :: best_id
    real(dp), intent(inout) :: best_dist2
    integer(i8), intent(in) :: cand_id
    real(dp), intent(in) :: cand_dist2

    if (best_id < 1_i8) then
      best_id = cand_id
      best_dist2 = cand_dist2
      return
    end if

    if (cand_dist2 < best_dist2) then
      best_id = cand_id
      best_dist2 = cand_dist2
      return
    end if

    if (is_close(cand_dist2, best_dist2) .and. cand_id < best_id) then
      best_id = cand_id
      best_dist2 = cand_dist2
    end if
  end subroutine spatial_index_update_best

  pure real(dp) function spatial_index_point_dist2(this, idx, target) result(dist2)
    class(spatial_index_t), intent(in) :: this
    integer(i8), intent(in) :: idx
    real(dp), intent(in) :: target(:)

    select case (this%storage_ndim)
      case (2_i4)
        dist2 = spatial_index_point_dist2_2d(this%pts, idx, target)
      case (3_i4)
        dist2 = spatial_index_point_dist2_3d(this%pts, idx, target)
      case default
        dist2 = spatial_index_point_dist2_generic(this%pts, idx, target)
    end select
  end function spatial_index_point_dist2

  pure real(dp) function spatial_index_point_dist2_2d(pts, idx, target) result(dist2)
    real(dp), intent(in) :: pts(:, :)
    integer(i8), intent(in) :: idx
    real(dp), intent(in) :: target(:)

    real(dp) :: dx, dy

    dx = pts(1, idx) - target(1)
    dy = pts(2, idx) - target(2)
    dist2 = dx * dx + dy * dy
  end function spatial_index_point_dist2_2d

  pure real(dp) function spatial_index_point_dist2_3d(pts, idx, target) result(dist2)
    real(dp), intent(in) :: pts(:, :)
    integer(i8), intent(in) :: idx
    real(dp), intent(in) :: target(:)

    real(dp) :: dx, dy, dz

    dx = pts(1, idx) - target(1)
    dy = pts(2, idx) - target(2)
    dz = pts(3, idx) - target(3)
    dist2 = dx * dx + dy * dy + dz * dz
  end function spatial_index_point_dist2_3d

  pure real(dp) function spatial_index_point_dist2_generic(pts, idx, target) result(dist2)
    real(dp), intent(in) :: pts(:, :)
    integer(i8), intent(in) :: idx
    real(dp), intent(in) :: target(:)

    integer(i4) :: idim
    real(dp) :: delta

    dist2 = 0.0_dp
    do idim = 1_i4, size(target, kind=i4)
      delta = pts(idim, idx) - target(idim)
      dist2 = dist2 + delta * delta
    end do
  end function spatial_index_point_dist2_generic

  !> \brief Convert longitude and latitude in degrees to a 3D unit vector.
  subroutine lonlat_to_unitvec(lon, lat, vec)
    real(dp), intent(in) :: lon
    real(dp), intent(in) :: lat
    real(dp), intent(out) :: vec(spherical_storage_ndim)

    real(dp) :: theta, phi, cos_phi

    theta = deg2rad_dp * lon
    phi = deg2rad_dp * lat
    cos_phi = cos(phi)

    vec(1) = cos_phi * cos(theta)
    vec(2) = cos_phi * sin(theta)
    vec(3) = sin(phi)
  end subroutine lonlat_to_unitvec

end module mo_spatial_index
