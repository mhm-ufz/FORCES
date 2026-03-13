module mo_spatial_index

  use mo_kind, only: i4, i8, dp
  use mo_constants, only: deg2rad_dp
  use mo_utils, only: is_close

  implicit none

  private

  integer(i4), parameter :: spatial_ndim = 3_i4

  public :: spatial_index_t

  type :: spatial_index_t
    real(dp), allocatable :: pts(:, :)
    integer(i8), allocatable :: point_ids(:)
    integer(i4), allocatable :: split_axis(:)
  contains
    procedure :: build_from_lonlat_grid => spatial_index_build_from_lonlat_grid
    procedure :: nearest_id_lonlat => spatial_index_nearest_id_lonlat
  end type spatial_index_t

contains

  subroutine spatial_index_build_from_lonlat_grid(this, lon, lat, cell_ij)
    class(spatial_index_t), intent(inout) :: this
    real(dp), intent(in) :: lon(:, :)
    real(dp), intent(in) :: lat(:, :)
    integer(i4), intent(in) :: cell_ij(:, :)

    integer(i8) :: ipt, npts
    integer(i4) :: i, j

    npts = size(cell_ij, 1, kind=i8)

    if (allocated(this%pts)) deallocate(this%pts)
    if (allocated(this%point_ids)) deallocate(this%point_ids)
    if (allocated(this%split_axis)) deallocate(this%split_axis)

    allocate(this%pts(spatial_ndim, npts))
    allocate(this%point_ids(npts))
    allocate(this%split_axis(npts))

    do ipt = 1_i8, npts
      i = cell_ij(ipt, 1)
      j = cell_ij(ipt, 2)
      call lonlat_to_unitvec(lon(i, j), lat(i, j), this%pts(:, ipt))
      this%point_ids(ipt) = ipt
    end do

    if (npts > 0_i8) call spatial_index_build(this, 1_i8, npts, 0_i4)
  end subroutine spatial_index_build_from_lonlat_grid

  integer(i8) function spatial_index_nearest_id_lonlat(this, lon, lat) result(point_id)
    class(spatial_index_t), intent(in) :: this
    real(dp), intent(in) :: lon
    real(dp), intent(in) :: lat

    real(dp) :: target(spatial_ndim), best_dist2

    point_id = 0_i8
    if (.not. allocated(this%point_ids)) return
    if (size(this%point_ids, kind=i8) < 1_i8) return

    call lonlat_to_unitvec(lon, lat, target)
    best_dist2 = huge(1.0_dp)
    call spatial_index_search(this, 1_i8, size(this%point_ids, kind=i8), target, point_id, best_dist2)
  end function spatial_index_nearest_id_lonlat

  recursive subroutine spatial_index_build(this, lo, hi, depth)
    class(spatial_index_t), intent(inout) :: this
    integer(i8), intent(in) :: lo
    integer(i8), intent(in) :: hi
    integer(i4), intent(in) :: depth

    integer(i8) :: mid

    if (lo > hi) return

    mid = lo + (hi - lo) / 2_i8
    this%split_axis(mid) = 1_i4 + mod(depth, spatial_ndim)
    call spatial_index_quickselect(this%pts, this%point_ids, lo, hi, mid, this%split_axis(mid))

    if (lo < mid) call spatial_index_build(this, lo, mid - 1_i8, depth + 1_i4)
    if (mid < hi) call spatial_index_build(this, mid + 1_i8, hi, depth + 1_i4)
  end subroutine spatial_index_build

  recursive subroutine spatial_index_search(this, lo, hi, target, best_id, best_dist2)
    class(spatial_index_t), intent(in) :: this
    integer(i8), intent(in) :: lo
    integer(i8), intent(in) :: hi
    real(dp), intent(in) :: target(spatial_ndim)
    integer(i8), intent(inout) :: best_id
    real(dp), intent(inout) :: best_dist2

    integer(i8) :: mid
    integer(i4) :: axis
    real(dp) :: delta, plane_dist2, cand_dist2

    if (lo > hi) return

    mid = lo + (hi - lo) / 2_i8
    axis = this%split_axis(mid)
    cand_dist2 = sum((this%pts(:, mid) - target)**2)
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
      do while (pts(axis, i) < pivot)
        i = i + 1_i8
      end do
      do while (pts(axis, j) > pivot)
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

  subroutine spatial_index_swap_points(pts, point_ids, lhs, rhs)
    real(dp), intent(inout) :: pts(:, :)
    integer(i8), intent(inout) :: point_ids(:)
    integer(i8), intent(in) :: lhs
    integer(i8), intent(in) :: rhs

    real(dp) :: tmp_pt(spatial_ndim)
    integer(i8) :: tmp_id

    if (lhs == rhs) return

    tmp_pt = pts(:, lhs)
    pts(:, lhs) = pts(:, rhs)
    pts(:, rhs) = tmp_pt

    tmp_id = point_ids(lhs)
    point_ids(lhs) = point_ids(rhs)
    point_ids(rhs) = tmp_id
  end subroutine spatial_index_swap_points

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

  subroutine lonlat_to_unitvec(lon, lat, vec)
    real(dp), intent(in) :: lon
    real(dp), intent(in) :: lat
    real(dp), intent(out) :: vec(spatial_ndim)

    real(dp) :: theta, phi, cos_phi

    theta = deg2rad_dp * lon
    phi = deg2rad_dp * lat
    cos_phi = cos(phi)

    vec(1) = cos_phi * cos(theta)
    vec(2) = cos_phi * sin(theta)
    vec(3) = sin(phi)
  end subroutine lonlat_to_unitvec

end module mo_spatial_index
