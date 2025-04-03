!> \file    mo_regridder.f90
!> \copydoc mo_regridder

!> \brief   Regridding utils.
!> \details This module provides a regridder for rectangular grids with cellsizes that have an integer ratio.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_regridder

  use mo_kind, only: i4, dp
  use mo_grid, only: grid, id_bounds
  use mo_utils, only: is_close

  implicit none

  private
  ! scaling selector
  integer(i4), public, parameter :: up_scaling = 0_i4 !< from fine to coarse grid
  integer(i4), public, parameter :: down_sclaing = 1_i4 !< from coarse to fine grid

  !> \class   regridder
  !> \brief   Regridder type to remap data on regular grids with an integer cellsize ratio.
  type, public :: regridder
    integer(i4) :: scaling_mode        !< up_scaling or down_scaling
    type(grid), pointer :: source_grid !< source grid
    type(grid), pointer :: target_grid !< target grid
    type(grid), pointer :: fine_grid   !< high resolution grid (source when upscaling, target when downscaling)
    type(grid), pointer :: coarse_grid !< low resolution grid (source when downscaling, target when upscaling)
    integer(i4) :: factor              !< coarse_grid % cellsize / fine_grid % cellsize
    integer(i4), dimension(:), allocatable :: y_lb              !< lower bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: y_ub              !< upper bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_lb              !< lower bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_ub              !< upper bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: n_subcells        !< valid fine grid cells in coarse cell (coarse%n_cells)
    integer(i4), dimension(:, :), allocatable :: coarse_id_map  !< 2d index array of coarse ids (fine%nx, fine%ny)
  contains
    procedure, public :: init => regridder_init
    procedure, private :: regridder_execute_dp, regridder_execute_i4, regridder_execute_i4_dp
    generic, public :: execute => regridder_execute_dp, regridder_execute_i4, regridder_execute_i4_dp
  end type regridder

contains

  !> \brief Setup regridder from given source and target grids
  subroutine regridder_init(this, source_grid, target_grid, tol)

    use mo_constants, only : nodata_dp, nodata_i4

    implicit none

    class(regridder), intent(inout) :: this
    type(grid), target, intent(inout) :: source_grid !< given high resolution grid
    type(grid), target, intent(inout) :: target_grid !< resulting low resolution grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    real(dp), dimension(:, :), allocatable :: areaCell0_2D
    integer(i4) :: i_ub, i_lb, j_lb, j_ub, k, ic, jc

    this%source_grid => source_grid
    this%target_grid => target_grid

    if (source_grid%cellsize < target_grid%cellsize) then
      this%scaling_mode = up_scaling
      call target_grid%check_is_filled_by(source_grid, tol=tol)
      this%fine_grid => source_grid
      this%coarse_grid => target_grid
    else
      this%scaling_mode = down_sclaing
      call target_grid%check_is_covered_by(source_grid, tol=tol)
      this%fine_grid => target_grid
      this%coarse_grid => source_grid
    end if

    this%factor = nint(this%coarse_grid%cellsize / this%fine_grid%cellsize, i4)

    allocate(this%y_lb(this%coarse_grid%n_cells))
    allocate(this%y_ub(this%coarse_grid%n_cells))
    allocate(this%x_lb(this%coarse_grid%n_cells))
    allocate(this%x_ub(this%coarse_grid%n_cells))
    allocate(this%n_subcells(this%coarse_grid%n_cells))
    allocate(this%coarse_id_map(this%fine_grid%nx, this%fine_grid%ny), source=nodata_i4)

    k = 0
    do jc = 1, this%coarse_grid%ny
      do ic = 1, this%coarse_grid%nx
        if (this%coarse_grid%has_mask()) then
          if (.NOT. this%coarse_grid%mask(ic, jc)) cycle
        end if
        k = k + 1
        call id_bounds(this%factor, ic, jc, &
          this%coarse_grid%y_direction, this%coarse_grid%ny, &
          this%fine_grid%y_direction, this%fine_grid%nx, this%fine_grid%ny, &
          i_lb, i_ub, j_lb, j_ub)

        this%x_lb(k) = i_lb
        this%x_ub(k) = i_ub
        this%y_lb(k) = j_lb
        this%y_ub(k) = j_ub

        if (this%fine_grid%has_mask()) then
          this%n_subcells(k) = count(this%fine_grid%mask(i_lb : i_ub, j_lb : j_ub))
        else
          this%n_subcells(k) = (i_ub-i_lb+1)*(j_ub-j_lb+1)
        end if
        ! Delimitation of level-11 cells on level-0
        this%coarse_id_map(i_lb : i_ub, j_lb : j_ub) = k
      end do
    end do

  end subroutine regridder_init

  subroutine regridder_execute_dp(this,  in_data, out_data, p)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data
    real(dp), intent(in), optional ::  p

    real(dp) :: norm

    norm = 1.0_dp
    if (present(p)) norm = p

  end subroutine regridder_execute_dp

  subroutine regridder_execute_i4(this,  in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    integer(i4), dimension(:), intent(out) ::  out_data
  end subroutine regridder_execute_i4

  subroutine regridder_execute_i4_dp(this,  in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data
  end subroutine regridder_execute_i4_dp

end module mo_regridder
