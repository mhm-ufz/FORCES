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

  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan, ieee_is_negative
  use mo_kind, only: i4, dp
  use mo_grid, only: grid, id_bounds
  use mo_utils, only: is_close, eq
  use mo_string_utils, only: num2str
  use mo_message, only: error_message
  use mo_constants, only: nodata_i4, nodata_dp

  implicit none

  private
  ! scaling selector
  integer(i4), public, parameter :: up_scaling = 0_i4 !< from fine to coarse grid
  integer(i4), public, parameter :: down_scaling = 1_i4 !< from coarse to fine grid
  ! upscaling operator
  integer(i4), public, parameter :: up_p_mean = 0_i4 !< power mean upscaling operator
  integer(i4), public, parameter :: up_a_mean = 1_i4 !< arithmetic mean upscaling operator (p-mean with p=1)
  integer(i4), public, parameter :: up_g_mean = 2_i4 !< geometric mean upscaling operator (p-mean with p=0)
  integer(i4), public, parameter :: up_h_mean = 3_i4 !< harmonic mean upscaling operator (p-mean with p=-1)
  integer(i4), public, parameter :: up_min = 4_i4 !< minimum value upscaling operator (p-mean for p -> -inf)
  integer(i4), public, parameter :: up_max = 5_i4 !< maximum value upscaling operator (p-mean for p -> +inf)
  integer(i4), public, parameter :: up_sum = 6_i4 !< summation upscaling operator
  integer(i4), public, parameter :: up_var = 7_i4 !< variance upscaling operator
  integer(i4), public, parameter :: up_std = 8_i4 !< standard deviation upscaling operator
  integer(i4), public, parameter :: up_laf = 9_i4 !< largest area fraction upscaling operator
  integer(i4), public, parameter :: up_fraction = 10_i4 !< area fraction of given class upscaling operator
  ! downscaling operator
  integer(i4), public, parameter :: down_nearest = 0_i4 !< nearest neighbor downscaling operator
  integer(i4), public, parameter :: down_split = 1_i4 !< inverse sum downscaling operator with equal summands

  !> \class   regridder
  !> \brief   Regridder type to remap data on regular grids with an integer cellsize ratio and matching lower left corner.
  type, public :: regridder
    integer(i4) :: scaling_mode        !< up_scaling (0) or down_scaling (1)
    type(grid), pointer :: source_grid !< source grid
    type(grid), pointer :: target_grid !< target grid
    type(grid), pointer :: fine_grid   !< high resolution grid (source when upscaling, target when downscaling)
    type(grid), pointer :: coarse_grid !< low resolution grid (source when downscaling, target when upscaling)
    integer(i4) :: factor              !< coarse_grid % cellsize / fine_grid % cellsize
    integer(i4), dimension(:), allocatable :: y_lb                 !< lower bound for y-id on fine grid (coarse%ncells)
    integer(i4), dimension(:), allocatable :: y_ub                 !< upper bound for y-id on fine grid (coarse%ncells)
    integer(i4), dimension(:), allocatable :: x_lb                 !< lower bound for x-id on fine grid (coarse%ncells)
    integer(i4), dimension(:), allocatable :: x_ub                 !< upper bound for x-id on fine grid (coarse%ncells)
    integer(i4), dimension(:), allocatable :: n_subcells           !< valid fine grid cells in coarse cell (coarse%ncells)
    real(dp), dimension(:, :), allocatable :: weights              !< cell area ratios (fine%nx,fine%ny)
    integer(i4), dimension(:), allocatable :: id_map               !< flat index array of coarse ids (fine%ncells)
  contains
    procedure, public :: init => regridder_init
    ! separate routines for all packed(1d)/unpacked(2d) combinations of IO-data
    procedure, private :: regridder_exe_dp_1d_1d, regridder_exe_dp_1d_2d, regridder_exe_dp_2d_1d, regridder_exe_dp_2d_2d
    procedure, private :: regridder_exe_i4_1d_1d, regridder_exe_i4_1d_2d, regridder_exe_i4_2d_1d, regridder_exe_i4_2d_2d
    procedure, private :: regridder_exe_i4_dp_1d_1d, regridder_exe_i4_dp_1d_2d, regridder_exe_i4_dp_2d_1d, regridder_exe_i4_dp_2d_2d
    generic, public :: execute => regridder_exe_dp_1d_1d, regridder_exe_dp_1d_2d, regridder_exe_dp_2d_1d, regridder_exe_dp_2d_2d
    generic, public :: execute => regridder_exe_i4_1d_1d, regridder_exe_i4_1d_2d, regridder_exe_i4_2d_1d, regridder_exe_i4_2d_2d
    generic, public :: execute => regridder_exe_i4_dp_1d_1d, regridder_exe_i4_dp_1d_2d
    generic, public :: execute => regridder_exe_i4_dp_2d_1d, regridder_exe_i4_dp_2d_2d
    procedure, private :: upscale_p_mean => regridder_upscale_p_mean
    procedure, private :: upscale_a_mean => regridder_upscale_a_mean
    procedure, private :: upscale_g_mean => regridder_upscale_g_mean
    procedure, private :: upscale_h_mean => regridder_upscale_h_mean
    procedure, private :: regridder_upscale_min_dp, regridder_upscale_min_i4
    generic, private :: upscale_min => regridder_upscale_min_dp, regridder_upscale_min_i4
    procedure, private :: regridder_upscale_max_dp, regridder_upscale_max_i4
    generic, private :: upscale_max => regridder_upscale_max_dp, regridder_upscale_max_i4
    procedure, private :: regridder_upscale_sum_dp, regridder_upscale_sum_i4
    generic, private :: upscale_sum => regridder_upscale_sum_dp, regridder_upscale_sum_i4
    procedure, private :: upscale_var => regridder_upscale_var
    procedure, private :: upscale_std => regridder_upscale_std
    procedure, private :: upscale_laf => regridder_upscale_laf
    procedure, private :: upscale_fraction => regridder_upscale_fraction
    procedure, private :: regridder_downscale_nearest_dp_1d, regridder_downscale_nearest_i4_1d
    generic, private :: downscale_nearest => regridder_downscale_nearest_dp_1d, regridder_downscale_nearest_i4_1d
    procedure, private :: regridder_downscale_nearest_dp_2d, regridder_downscale_nearest_i4_2d
    generic, private :: downscale_nearest => regridder_downscale_nearest_dp_2d, regridder_downscale_nearest_i4_2d
    procedure, private :: regridder_downscale_split_1d, regridder_downscale_split_2d
    generic, private :: downscale_split => regridder_downscale_split_1d, regridder_downscale_split_2d
  end type regridder

contains

  !> \brief Setup regridder from given source and target grids
  subroutine regridder_init(this, source_grid, target_grid, tol)
    use mo_constants, only : nodata_i4
    implicit none
    class(regridder), intent(inout) :: this
    type(grid), target, intent(inout) :: source_grid !< given high resolution grid
    type(grid), target, intent(inout) :: target_grid !< resulting low resolution grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    integer(i4) :: i_ub, i_lb, j_lb, j_ub, k, ic, jc
    integer(i4), dimension(:, :), allocatable :: coarse_id_matrix
    real(dp), dimension(:), allocatable :: weights

    this%source_grid => source_grid
    this%target_grid => target_grid

    if (source_grid%cellsize < target_grid%cellsize) then
      this%scaling_mode = up_scaling
      call target_grid%check_is_filled_by(source_grid, tol=tol)
      this%fine_grid => source_grid
      this%coarse_grid => target_grid
    else
      this%scaling_mode = down_scaling
      call target_grid%check_is_covered_by(source_grid, tol=tol)
      this%fine_grid => target_grid
      this%coarse_grid => source_grid
    end if

    this%factor = nint(this%coarse_grid%cellsize / this%fine_grid%cellsize, i4)

    allocate(this%y_lb(this%coarse_grid%ncells))
    allocate(this%y_ub(this%coarse_grid%ncells))
    allocate(this%x_lb(this%coarse_grid%ncells))
    allocate(this%x_ub(this%coarse_grid%ncells))
    allocate(this%n_subcells(this%coarse_grid%ncells))
    allocate(this%id_map(this%fine_grid%ncells))
    allocate(this%weights(this%fine_grid%nx, this%fine_grid%ny))

    allocate(coarse_id_matrix(this%fine_grid%nx, this%fine_grid%ny), source=nodata_i4)
    allocate(weights(this%fine_grid%ncells))

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
        ! coarse cell id on fine sub-cells
        coarse_id_matrix(i_lb : i_ub, j_lb : j_ub) = k
      end do
    end do
    this%id_map = this%fine_grid%pack_data(coarse_id_matrix)
    deallocate(coarse_id_matrix)

    ! generate weights from area fractions
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1, this%fine_grid%ncells
      weights(k) = this%fine_grid%cell_area(k) / this%coarse_grid%cell_area(this%id_map(k))
    end do
    !$omp end parallel do
    this%weights = this%fine_grid%unpack_data(weights)
    deallocate(weights)

  end subroutine regridder_init

  subroutine arg_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    integer(i4), intent(out) ::  up_operator !< upscaling operator
    integer(i4), intent(out) ::  down_operator !< downscaling operator
    integer(i4), intent(in), optional ::  upscaling_operator !< provided upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< provided downscaling operator (down_nearest by default)
    up_operator = up_a_mean
    if (present(upscaling_operator)) up_operator = upscaling_operator
    down_operator = down_nearest
    if (present(downscaling_operator)) down_operator = downscaling_operator
  end subroutine arg_init

  subroutine regridder_exe_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    call this%regridder_exe_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_dp), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine regridder_exe_dp_1d_1d

  subroutine regridder_exe_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_dp), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine regridder_exe_dp_1d_2d

  subroutine regridder_exe_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine regridder_exe_dp_2d_2d

  subroutine regridder_exe_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call arg_init(up_operator, down_operator, upscaling_operator, downscaling_operator)

    if (this%scaling_mode == up_scaling) then
      select case (up_operator)
        case (up_p_mean)
          call this%upscale_p_mean(in_data, out_data, p)
        case (up_a_mean)
          call this%upscale_a_mean(in_data, out_data)
        case (up_g_mean)
          call this%upscale_g_mean(in_data, out_data)
        case (up_h_mean)
          call this%upscale_h_mean(in_data, out_data)
        case (up_min)
          call this%upscale_min(in_data, out_data)
        case (up_max)
          call this%upscale_max(in_data, out_data)
        case (up_sum)
          call this%upscale_sum(in_data, out_data)
        case (up_var)
          call this%upscale_var(in_data, out_data)
        case (up_std)
          call this%upscale_std(in_data, out_data)
        case (up_laf)
          call error_message("regridder: largest area fraction upscaling operator not supported for real data.")
        case (up_fraction)
          call error_message("regridder: class fraction upscaling operator not supported for real input data.")
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call this%downscale_split(in_data, out_data)
        case default
          call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if

  end subroutine regridder_exe_dp_2d_1d

  subroutine regridder_exe_i4_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    call this%regridder_exe_i4_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine regridder_exe_i4_1d_1d

  subroutine regridder_exe_i4_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    integer(i4), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_i4_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_i4)
  end subroutine regridder_exe_i4_1d_2d

  subroutine regridder_exe_i4_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    integer(i4), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_i4_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_i4)
  end subroutine regridder_exe_i4_2d_2d

  subroutine regridder_exe_i4_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call arg_init(up_operator, down_operator, upscaling_operator, downscaling_operator)

    if (this%scaling_mode == up_scaling) then
      select case (up_operator)
        case (up_p_mean)
          call error_message("regridder: p-mean upscaling operator not supported for integer data.")
        case (up_a_mean)
          call error_message("regridder: arithmetic mean upscaling operator not supported for integer data.")
        case (up_g_mean)
          call error_message("regridder: geometric mean upscaling operator not supported for integer data.")
        case (up_h_mean)
          call error_message("regridder: harmonic mean upscaling operator not supported for integer data.")
        case (up_min)
          call this%upscale_min(in_data, out_data)
        case (up_max)
          call this%upscale_max(in_data, out_data)
        case (up_sum)
          call this%upscale_sum(in_data, out_data)
        case (up_var)
          call error_message("regridder: variance upscaling operator not supported for integer data.")
        case (up_std)
          call error_message("regridder: standard deviation upscaling not supported for integer data.")
        case (up_laf)
          call this%upscale_laf(in_data, out_data, vmin, vmax)
        case (up_fraction)
          call error_message("regridder: class fraction upscaling operator not supported for integer output data.")
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call error_message("regridder: equal summand downscaling operator not supported for integer output data.")
        case default
          call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if
  end subroutine regridder_exe_i4_2d_1d

  subroutine regridder_exe_i4_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    call this%regridder_exe_i4_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine regridder_exe_i4_dp_1d_1d

  subroutine regridder_exe_i4_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_i4_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine regridder_exe_i4_dp_1d_2d

  subroutine regridder_exe_i4_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) ::  temp
    call this%regridder_exe_i4_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine regridder_exe_i4_dp_2d_2d

  subroutine regridder_exe_i4_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data !< output data on target grid
    integer(i4), intent(in), optional ::  upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional ::  downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional ::  p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional ::  class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up up_laf operator (huge() by default - tbd)
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up up_laf operator (-huge() by default - tbd)

    integer(i4), dimension(:), allocatable :: temp

    integer(i4) :: up_operator, down_operator
    call arg_init(up_operator, down_operator, upscaling_operator, downscaling_operator)

    if (this%scaling_mode == up_scaling) then
      select case (up_operator)
        case (up_p_mean)
          call this%upscale_p_mean(real(in_data, dp), out_data, p)
        case (up_a_mean)
          call this%upscale_a_mean(real(in_data, dp), out_data)
        case (up_g_mean)
          call this%upscale_g_mean(real(in_data, dp), out_data)
        case (up_h_mean)
          call this%upscale_h_mean(real(in_data, dp), out_data)
        case (up_min)
          allocate(temp(this%coarse_grid%ncells))
          call this%upscale_min(in_data, temp)
          out_data = real(temp, dp)
          deallocate(temp)
        case (up_max)
          allocate(temp(this%coarse_grid%ncells))
          call this%upscale_max(in_data, temp)
          out_data = real(temp, dp)
          deallocate(temp)
        case (up_sum)
          allocate(temp(this%coarse_grid%ncells))
          call this%upscale_sum(in_data, temp)
          out_data = real(temp, dp)
          deallocate(temp)
        case (up_var)
          call this%upscale_var(real(in_data, dp), out_data)
        case (up_std)
          call this%upscale_std(real(in_data, dp), out_data)
        case (up_laf)
          allocate(temp(this%coarse_grid%ncells))
          call this%upscale_laf(in_data, temp, vmin, vmax)
          out_data = real(temp, dp)
          deallocate(temp)
        case (up_fraction)
          call this%upscale_fraction(in_data, out_data, class_id)
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(real(in_data, dp), out_data)
        case (down_split)
          call this%downscale_split(real(in_data, dp), out_data)
        case default
          call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if
  end subroutine regridder_exe_i4_dp_2d_1d

  subroutine regridder_upscale_p_mean(this, in_data, out_data, p)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    real(dp), intent(in), optional :: p !< exponent for the p-norm (1.0 for arithmetic mean by default)
    integer(i4) :: k
    real(dp) :: p_
    p_ = 1.0_dp
    if (present(p)) p_ = p
    if (eq(p_, 0.0_dp)) then
      call this%upscale_g_mean(in_data, out_data)
    else if (eq(p_, 1.0_dp)) then
      call this%upscale_a_mean(in_data, out_data)
    else if (eq(p_, -1.0_dp)) then
      call this%upscale_h_mean(in_data, out_data)
    else if (.not.ieee_is_finite(p_)) then
      if (ieee_is_nan(p_)) call error_message("upscale_p_mean: p is not a number.")
      if (ieee_is_negative(p_)) then
        call this%upscale_min(in_data, out_data)
      else
        call this%upscale_max(in_data, out_data)
      end if
    else
      call check_upscaling(this%scaling_mode)
      !$omp parallel do default(shared) private(k) schedule(static)
      do k = 1_i4, this%coarse_grid%ncells
        out_data(k) = sum( &
            pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) ** p_ &
          * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        ) ** (1.0_dp / p_)
      end do
      !$omp end parallel do
    end if
  end subroutine regridder_upscale_p_mean

  subroutine regridder_upscale_a_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = sum( &
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_a_mean

  subroutine regridder_upscale_g_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = exp(sum(log( & ! prod(xi^wi) = exp(sum(wi*log(xi)))
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_g_mean

  subroutine regridder_upscale_h_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = 1.0_dp / sum( 1.0_dp / &
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_h_mean

  subroutine regridder_upscale_min_dp(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = minval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_min_dp

  subroutine regridder_upscale_min_i4(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = minval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_min_i4

  subroutine regridder_upscale_max_dp(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = maxval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_max_dp

  subroutine regridder_upscale_max_i4(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = maxval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_max_i4

  subroutine regridder_upscale_sum_dp(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = sum( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_sum_dp

  subroutine regridder_upscale_sum_i4(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = sum( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_sum_i4

  subroutine regridder_upscale_var(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    real(dp) ::  mean
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k, mean) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      mean = sum( &
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
              this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
              this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
      out_data(k) = sum( &
        ( pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        - mean ) ** 2_i4 &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_var

  subroutine regridder_upscale_std(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    call this%upscale_var(in_data, out_data)
    out_data = sqrt(out_data)
  end subroutine regridder_upscale_std

  subroutine regridder_upscale_laf(this, in_data, out_data, vmin, vmax)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4), intent(in), optional ::  vmin !< minimum of values to speed up operator
    integer(i4), intent(in), optional ::  vmax !< maximum of values to speed up operator
    integer(i4) :: k, i, laf_v, cnt_v, cnt_i, min_v, max_v

    call check_upscaling(this%scaling_mode)

    if (present(vmin)) then
      min_v = vmin
    else
      min_v = minval(in_data)
    endif
    if (present(vmax)) then
      max_v = vmax
    else
      max_v = maxval(in_data)
    endif
    if (min_v > max_v) call error_message("upscale_laf: vmin is bigger than vmax.")

    !$omp parallel do default(shared) private(k, i, laf_v, cnt_v, cnt_i) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      laf_v = min_v
      cnt_v = 0
      do i = min_v, max_v
        ! nodata value should be out of range (-9999) so we don't need to pack data
        cnt_i = count(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)) == i)
        if (cnt_i > cnt_v) then
          laf_v = i
          cnt_v = cnt_i
        end if
      end do
      out_data(k) = laf_v
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_laf

  subroutine regridder_upscale_fraction(this, in_data, out_data, class_id)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4), intent(in), optional ::  class_id !< class id to determine area fraction of
    integer(i4) :: k, cls_id

    call check_upscaling(this%scaling_mode)

    cls_id = nodata_i4
    if (present(class_id)) cls_id = class_id

    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = sum( &
        this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)) == cls_id) &
              .and. this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine regridder_upscale_fraction

  subroutine regridder_downscale_nearest_dp_1d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_nearest_dp_1d

  subroutine regridder_downscale_nearest_i4_1d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_nearest_i4_1d

  subroutine regridder_downscale_split_1d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k)) / real(this%n_subcells(this%id_map(k)), dp)
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_split_1d

  subroutine regridder_downscale_nearest_dp_2d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k,i,j
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      i = this%fine_grid%cell_ij(this%id_map(k), 1)
      j = this%fine_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_nearest_dp_2d

  subroutine regridder_downscale_nearest_i4_2d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k,i,j
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      i = this%fine_grid%cell_ij(this%id_map(k), 1)
      j = this%fine_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_nearest_i4_2d

  subroutine regridder_downscale_split_2d(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) ::  in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) ::  out_data
    integer(i4) :: k,i,j
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i4, this%fine_grid%ncells
      i = this%fine_grid%cell_ij(this%id_map(k), 1)
      j = this%fine_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j) / real(this%n_subcells(this%id_map(k)), dp)
    end do
    !$omp end parallel do
  end subroutine regridder_downscale_split_2d

  subroutine check_upscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= up_scaling) call error_message("regridder: not setup for upscaling.")
  end subroutine check_upscaling

  subroutine check_downscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= down_scaling) call error_message("regridder: not setup for downscaling.")
  end subroutine check_downscaling

end module mo_regridder
