!> \file    mo_points_regridder.f90
!> \copydoc mo_points_regridder

!> \brief   Nearest-neighbor mapping for point sets.
!> \details Provides nearest-neighbor mappings between structured grids and
!!          unstructured point sets. These regridders store an index map at
!!          initialization and apply it to data vectors during execution.
!> \authors Sebastian Müller
!> \date Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_points_regridder

  use mo_grid, only: grid_t, cartesian, spherical
  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_points, only: points_t
  use mo_spatial_index, only: spatial_index_t
  use mo_utils, only: optval

  implicit none

  private

  public :: nearest_grid_to_points_t
  public :: nearest_points_to_grid_t
  public :: nearest_points_to_points_t

  !> \class nearest_grid_to_points_t
  !> \brief Nearest-neighbor mapper from grid cells to target points.
  type nearest_grid_to_points_t
    type(grid_t), pointer :: source_grid => null()      !< source grid
    type(points_t), pointer :: target_points => null() !< target point set
    integer(i8), allocatable :: id_map(:)               !< source cell id for each target point
    logical :: source_use_aux = .false.                 !< use auxiliary lon/lat grid coordinates
  contains
    procedure, public :: init => grid_to_points_init
    procedure, private :: grid_to_points_exe_dp
    procedure, private :: grid_to_points_exe_i4
    generic, public :: execute => grid_to_points_exe_dp, grid_to_points_exe_i4
  end type nearest_grid_to_points_t

  !> \class nearest_points_to_grid_t
  !> \brief Nearest-neighbor mapper from source points to grid cells.
  type nearest_points_to_grid_t
    type(points_t), pointer :: source_points => null() !< source point set
    type(grid_t), pointer :: target_grid => null()       !< target grid
    integer(i8), allocatable :: id_map(:)                !< source point id for each target cell
    logical :: target_use_aux = .false.                  !< use auxiliary lon/lat grid coordinates
  contains
    procedure, public :: init => points_to_grid_init
    procedure, private :: points_to_grid_exe_dp
    procedure, private :: points_to_grid_exe_i4
    generic, public :: execute => points_to_grid_exe_dp, points_to_grid_exe_i4
  end type nearest_points_to_grid_t

  !> \class nearest_points_to_points_t
  !> \brief Nearest-neighbor mapper between two point sets.
  type nearest_points_to_points_t
    type(points_t), pointer :: source_points => null() !< source point set
    type(points_t), pointer :: target_points => null() !< target point set
    integer(i8), allocatable :: id_map(:)                !< source point id for each target point
  contains
    procedure, public :: init => points_to_points_init
    procedure, private :: points_to_points_exe_dp
    procedure, private :: points_to_points_exe_i4
    generic, public :: execute => points_to_points_exe_dp, points_to_points_exe_i4
  end type nearest_points_to_points_t

contains

  !> \brief Build the nearest-neighbor map from source grid cells to target points.
  subroutine grid_to_points_init(this, source_grid, target_points, use_aux)
    class(nearest_grid_to_points_t), intent(inout) :: this
    type(grid_t), pointer, intent(in) :: source_grid !< source grid
    type(points_t), pointer, intent(in) :: target_points !< target point set
    logical, optional, intent(in) :: use_aux !< force auxiliary lon/lat coordinates on the source grid
    type(spatial_index_t) :: index
    logical :: use_aux_

    this%source_grid => source_grid
    this%target_points => target_points
    use_aux_ = optval(use_aux, .false.)
    this%source_use_aux = use_aux_
    if (target_points%coordsys == spherical .and. source_grid%coordsys == cartesian) this%source_use_aux = .true.
    if (this%source_use_aux .and. .not.source_grid%has_aux_coords()) &
      call error_message("nearest_grid_to_points % init: source grid has no auxiliary lon/lat coordinates")
    call source_grid%build_spatial_index(index, use_aux=this%source_use_aux)
    if (target_points%coordsys == spherical) then
      this%id_map = index%nearest_ids_lonlat(target_points%coords())
    else
      this%id_map = index%nearest_ids(target_points%coords())
    end if
  end subroutine grid_to_points_init

  !> \brief Apply a grid-to-points map to double precision data.
  subroutine grid_to_points_exe_dp(this, in_data, out_data)
    class(nearest_grid_to_points_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:) !< source grid data
    real(dp), intent(out) :: out_data(:) !< mapped target point data
    integer(i8) :: i
    call check_grid_to_points(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_points%npoints
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine grid_to_points_exe_dp

  !> \brief Apply a grid-to-points map to integer data.
  subroutine grid_to_points_exe_i4(this, in_data, out_data)
    class(nearest_grid_to_points_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:) !< source grid data
    integer(i4), intent(out) :: out_data(:) !< mapped target point data
    integer(i8) :: i
    call check_grid_to_points(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_points%npoints
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine grid_to_points_exe_i4

  !> \brief Build the nearest-neighbor map from source points to target grid cells.
  subroutine points_to_grid_init(this, source_points, target_grid, use_aux)
    class(nearest_points_to_grid_t), intent(inout) :: this
    type(points_t), pointer, intent(in) :: source_points !< source point set
    type(grid_t), pointer, intent(in) :: target_grid !< target grid
    logical, optional, intent(in) :: use_aux !< force auxiliary lon/lat coordinates on the target grid
    type(spatial_index_t) :: index
    real(dp), allocatable :: target_coords(:, :)
    logical :: use_aux_

    this%source_points => source_points
    this%target_grid => target_grid
    use_aux_ = optval(use_aux, .false.)
    this%target_use_aux = use_aux_
    if (source_points%coordsys == spherical .and. target_grid%coordsys == cartesian) this%target_use_aux = .true.
    if (this%target_use_aux .and. .not.target_grid%has_aux_coords()) &
      call error_message("nearest_points_to_grid % init: target grid has no auxiliary lon/lat coordinates")
    call source_points%build_spatial_index(index)
    call collect_grid_points(target_grid, this%target_use_aux, target_coords)
    if (source_points%coordsys == spherical) then
      this%id_map = index%nearest_ids_lonlat(target_coords)
    else
      this%id_map = index%nearest_ids(target_coords)
    end if
  end subroutine points_to_grid_init

  !> \brief Apply a points-to-grid map to double precision data.
  subroutine points_to_grid_exe_dp(this, in_data, out_data)
    class(nearest_points_to_grid_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:) !< source point data
    real(dp), intent(out) :: out_data(:) !< mapped target grid data
    integer(i8) :: i
    call check_points_to_grid(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_grid%ncells
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine points_to_grid_exe_dp

  !> \brief Apply a points-to-grid map to integer data.
  subroutine points_to_grid_exe_i4(this, in_data, out_data)
    class(nearest_points_to_grid_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:) !< source point data
    integer(i4), intent(out) :: out_data(:) !< mapped target grid data
    integer(i8) :: i
    call check_points_to_grid(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_grid%ncells
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine points_to_grid_exe_i4

  !> \brief Build the nearest-neighbor map between two point sets.
  subroutine points_to_points_init(this, source_points, target_points)
    class(nearest_points_to_points_t), intent(inout) :: this
    type(points_t), pointer, intent(in) :: source_points !< source point set
    type(points_t), pointer, intent(in) :: target_points !< target point set
    type(spatial_index_t) :: index

    if (source_points%coordsys /= target_points%coordsys) &
      call error_message("nearest_points_to_points % init: coordinate systems differ")
    this%source_points => source_points
    this%target_points => target_points
    call source_points%build_spatial_index(index)
    if (source_points%coordsys == spherical) then
      this%id_map = index%nearest_ids_lonlat(target_points%coords())
    else
      this%id_map = index%nearest_ids(target_points%coords())
    end if
  end subroutine points_to_points_init

  !> \brief Apply a points-to-points map to double precision data.
  subroutine points_to_points_exe_dp(this, in_data, out_data)
    class(nearest_points_to_points_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:) !< source point data
    real(dp), intent(out) :: out_data(:) !< mapped target point data
    integer(i8) :: i
    call check_points_to_points(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_points%npoints
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine points_to_points_exe_dp

  !> \brief Apply a points-to-points map to integer data.
  subroutine points_to_points_exe_i4(this, in_data, out_data)
    class(nearest_points_to_points_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:) !< source point data
    integer(i4), intent(out) :: out_data(:) !< mapped target point data
    integer(i8) :: i
    call check_points_to_points(this, size(in_data, kind=i8), size(out_data, kind=i8))
    do i = 1_i8, this%target_points%npoints
      out_data(i) = in_data(this%id_map(i))
    end do
  end subroutine points_to_points_exe_i4

  !> \brief Collect grid-cell center coordinates for nearest-neighbor lookup.
  subroutine collect_grid_points(grid, use_aux, coords)
    type(grid_t), intent(in) :: grid !< grid to sample
    logical, intent(in) :: use_aux !< use auxiliary lon/lat coordinates
    real(dp), allocatable, intent(out) :: coords(:, :) !< coordinate matrix with shape (ncells,2)
    integer(i8) :: k
    integer(i4) :: i, j

    allocate(coords(grid%ncells, 2))
    do k = 1_i8, grid%ncells
      i = grid%cell_ij(k, 1)
      j = grid%cell_ij(k, 2)
      if (use_aux) then
        coords(k, 1) = grid%lon(i, j)
        coords(k, 2) = grid%lat(i, j)
      else
        coords(k, 1) = grid%x_center(i)
        coords(k, 2) = grid%y_center(j)
      end if
    end do
  end subroutine collect_grid_points

  !> \brief Validate data sizes for grid-to-points execution.
  subroutine check_grid_to_points(this, nsource, ntarget)
    class(nearest_grid_to_points_t), intent(in) :: this
    integer(i8), intent(in) :: nsource, ntarget !< source and target data sizes
    if (.not.allocated(this%id_map)) call error_message("nearest_grid_to_points % execute: regridder not initialized")
    if (nsource /= this%source_grid%ncells) call error_message("nearest_grid_to_points % execute: source size mismatch")
    if (ntarget /= this%target_points%npoints) call error_message("nearest_grid_to_points % execute: target size mismatch")
  end subroutine check_grid_to_points

  !> \brief Validate data sizes for points-to-grid execution.
  subroutine check_points_to_grid(this, nsource, ntarget)
    class(nearest_points_to_grid_t), intent(in) :: this
    integer(i8), intent(in) :: nsource, ntarget !< source and target data sizes
    if (.not.allocated(this%id_map)) call error_message("nearest_points_to_grid % execute: regridder not initialized")
    if (nsource /= this%source_points%npoints) call error_message("nearest_points_to_grid % execute: source size mismatch")
    if (ntarget /= this%target_grid%ncells) call error_message("nearest_points_to_grid % execute: target size mismatch")
  end subroutine check_points_to_grid

  !> \brief Validate data sizes for points-to-points execution.
  subroutine check_points_to_points(this, nsource, ntarget)
    class(nearest_points_to_points_t), intent(in) :: this
    integer(i8), intent(in) :: nsource, ntarget !< source and target data sizes
    if (.not.allocated(this%id_map)) call error_message("nearest_points_to_points % execute: regridder not initialized")
    if (nsource /= this%source_points%npoints) call error_message("nearest_points_to_points % execute: source size mismatch")
    if (ntarget /= this%target_points%npoints) call error_message("nearest_points_to_points % execute: target size mismatch")
  end subroutine check_points_to_points

end module mo_points_regridder
