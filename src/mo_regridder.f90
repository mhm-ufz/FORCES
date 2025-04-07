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
    ! TODO: do we need the fully id map array?
    integer(i4), dimension(:, :), allocatable :: coarse_id_matrix  !< 2d index array of coarse ids (fine%nx, fine%ny)
    integer(i4), dimension(:), allocatable :: coarse_id_map        !< flat index array of coarse ids (fine%ncells)
  contains
    procedure, public :: init => regridder_init
    procedure, private :: regridder_execute_dp, regridder_execute_i4, regridder_execute_i4_dp
    generic, public :: execute => regridder_execute_dp, regridder_execute_i4, regridder_execute_i4_dp
    procedure, public :: upscale_p_mean => regridder_upscale_p_mean
    procedure, public :: upscale_a_mean => regridder_upscale_a_mean
    procedure, public :: upscale_g_mean => regridder_upscale_g_mean
    procedure, public :: upscale_h_mean => regridder_upscale_h_mean
    procedure, private :: regridder_upscale_min_dp, regridder_upscale_min_i4
    generic, public :: upscale_min => regridder_upscale_min_dp, regridder_upscale_min_i4
    procedure, private :: regridder_upscale_max_dp, regridder_upscale_max_i4
    generic, public :: upscale_max => regridder_upscale_max_dp, regridder_upscale_max_i4
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
    allocate(this%coarse_id_matrix(this%fine_grid%nx, this%fine_grid%ny), source=nodata_i4)
    allocate(this%coarse_id_map(this%fine_grid%ncells))

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
        this%coarse_id_matrix(i_lb : i_ub, j_lb : j_ub) = k
      end do
    end do
    this%coarse_id_map = this%fine_grid%pack_data(this%coarse_id_matrix)

  end subroutine regridder_init

  subroutine arg_init(up_op, down_op, norm, cls_id, upscaling_operator, downscaling_operator, p, class_id)
    integer(i4), intent(out) ::  up_op
    integer(i4), intent(out) ::  down_op
    real(dp), intent(out) ::  norm
    integer(i4), intent(out) ::  cls_id
    integer(i4), intent(in), optional ::  upscaling_operator
    integer(i4), intent(in), optional ::  downscaling_operator
    real(dp), intent(in), optional ::  p
    integer(i4), intent(in), optional ::  class_id
    up_op = up_a_mean
    if (present(upscaling_operator)) up_op = upscaling_operator
    down_op = down_nearest
    if (present(downscaling_operator)) down_op = downscaling_operator
    norm = 1.0_dp
    if (present(p)) norm = p
    cls_id = 0_i4
    if (present(class_id)) cls_id = class_id
  end subroutine arg_init

  subroutine regridder_execute_dp(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data
    integer(i4), intent(in), optional ::  upscaling_operator
    integer(i4), intent(in), optional ::  downscaling_operator
    real(dp), intent(in), optional ::  p
    integer(i4), intent(in), optional ::  class_id

    real(dp) :: norm
    integer(i4) :: up_op, down_op, cls_id

    call arg_init(up_op, down_op, norm, cls_id, upscaling_operator, downscaling_operator, p, class_id)
    if (this%scaling_mode == up_scaling) then
      select case (up_op)
        case (up_p_mean)
          call this%upscale_p_mean(in_data, out_data, norm)
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
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_op)))
      end select
    else ! down-scaling
      select case (down_op)
      case (down_nearest)
        call error_message("downscale_nearest not implemented")
        ! call this%downscale_nearest(in_data, out_data)
      case (down_split)
        call error_message("downscale_fraction not implemented")
        ! call this%downscale_fraction(in_data, out_data)
      case default
        call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_op)))
    end select
  end if

  end subroutine regridder_execute_dp

  subroutine regridder_execute_i4(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    integer(i4), dimension(:), intent(out) ::  out_data
    integer(i4), intent(in), optional ::  upscaling_operator
    integer(i4), intent(in), optional ::  downscaling_operator
    real(dp), intent(in), optional ::  p
    integer(i4), intent(in), optional ::  class_id

    real(dp) :: norm
    integer(i4) :: up_op, down_op, cls_id
    call arg_init(up_op, down_op, norm, cls_id, upscaling_operator, downscaling_operator, p, class_id)
    if (this%scaling_mode == up_scaling) then
      select case (up_op)
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
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_op)))
      end select
    else ! down-scaling
      select case (down_op)
        case (down_nearest)
          call error_message("downscale_nearest not implemented")
          ! call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call error_message("downscale_fraction not implemented")
          ! call this%downscale_fraction(in_data, out_data)
        case default
          call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_op)))
      end select
    end if
  end subroutine regridder_execute_i4

  subroutine regridder_execute_i4_dp(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data
    integer(i4), intent(in), optional ::  upscaling_operator
    integer(i4), intent(in), optional ::  downscaling_operator
    real(dp), intent(in), optional ::  p
    integer(i4), intent(in), optional ::  class_id

    real(dp) :: norm
    integer(i4) :: up_op, down_op, cls_id
    call arg_init(up_op, down_op, norm, cls_id, upscaling_operator, downscaling_operator, p, class_id)
    if (this%scaling_mode == up_scaling) then
      select case (up_op)
        case (up_p_mean)
          call this%upscale_p_mean(real(in_data, dp), out_data, norm)
        case (up_a_mean)
          call this%upscale_a_mean(real(in_data, dp), out_data)
        case (up_g_mean)
          call this%upscale_g_mean(real(in_data, dp), out_data)
        case (up_h_mean)
          call this%upscale_h_mean(real(in_data, dp), out_data)
        case (up_min)
          call this%upscale_min(real(in_data, dp), out_data)
        case (up_max)
          call this%upscale_max(real(in_data, dp), out_data)
        case default
          call error_message("regridder: unknown upscaling operator: ", trim(num2str(up_op)))
      end select
    else ! down-scaling
      select case (down_op)
        case (down_nearest)
          call error_message("downscale_nearest not implemented")
          ! call this%downscale_nearest(real(in_data, dp), out_data)
        case (down_split)
          call error_message("downscale_fraction not implemented")
          ! call this%downscale_fraction(real(in_data, dp), out_data)
        case default
          call error_message("regridder: unknown downscaling operator: ", trim(num2str(down_op)))
      end select
    end if
  end subroutine regridder_execute_i4_dp

  subroutine regridder_upscale_p_mean(this, in_data, out_data, p)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data
    real(dp), intent(in) ::  p
    real(dp), dimension(:,:), allocatable ::  source_matrix, weight_matrix
    integer(i4) :: k
    if (eq(p, 0.0_dp)) then
      call this%upscale_g_mean(in_data, out_data)
    else if (eq(p, 1.0_dp)) then
      call this%upscale_a_mean(in_data, out_data)
    else if (eq(p, -1.0_dp)) then
      call this%upscale_h_mean(in_data, out_data)
    else if (.not.ieee_is_finite(p)) then
      if (ieee_is_nan(p)) call error_message("upscale_p_mean: p is not a number.")
      if (ieee_is_negative(p)) then
        call this%upscale_min(in_data, out_data)
      else
        call this%upscale_max(in_data, out_data)
      end if
    else
      call check_upscaling(this%scaling_mode)
      allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
      allocate(weight_matrix(this%fine_grid%nx, this%fine_grid%ny))
      source_matrix = this%fine_grid%unpack_data(in_data ** p)
      weight_matrix = this%fine_grid%unpack_data(this%fine_grid%cell_area)
      ! TODO: openmp
      do k = 1_i4, this%coarse_grid%ncells
        out_data(k) = (sum( &
            pack(source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
          * pack(weight_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        ) / this%coarse_grid%cell_area(k)) ** (1.0_dp / p)
      end do
      deallocate(source_matrix)
      deallocate(weight_matrix)
    end if
  end subroutine regridder_upscale_p_mean

  subroutine regridder_upscale_a_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data

    real(dp), dimension(:,:), allocatable ::  source_matrix, weight_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    allocate(weight_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(in_data)
    weight_matrix = this%fine_grid%unpack_data(this%fine_grid%cell_area)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = sum( &
          pack(source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(weight_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
      ) / this%coarse_grid%cell_area(k)
    end do
    deallocate(source_matrix)
    deallocate(weight_matrix)
  end subroutine regridder_upscale_a_mean

  subroutine regridder_upscale_g_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data

    real(dp), dimension(:,:), allocatable ::  source_matrix, weight_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    allocate(weight_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(log(in_data))  ! prod(xi^wi) = exp(sum(wi*log(xi)))
    weight_matrix = this%fine_grid%unpack_data(this%fine_grid%cell_area)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = exp(sum( &
          pack(source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(weight_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
      ) / this%coarse_grid%cell_area(k))
    end do
    deallocate(source_matrix)
    deallocate(weight_matrix)
  end subroutine regridder_upscale_g_mean

  subroutine regridder_upscale_h_mean(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data

    real(dp), dimension(:,:), allocatable ::  source_matrix, weight_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    allocate(weight_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(1.0_dp / in_data)
    weight_matrix = this%fine_grid%unpack_data(this%fine_grid%cell_area)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = 1.0_dp / (sum( &
          pack(source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(weight_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
      ) / this%coarse_grid%cell_area(k))
    end do
    deallocate(source_matrix)
    deallocate(weight_matrix)
  end subroutine regridder_upscale_h_mean

  subroutine regridder_upscale_min_dp(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data

    real(dp), dimension(:,:), allocatable ::  source_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(in_data)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = minval( &
        source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    deallocate(source_matrix)
  end subroutine regridder_upscale_min_dp

  subroutine regridder_upscale_min_i4(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    integer(i4), dimension(:), intent(out) ::  out_data

    integer(i4), dimension(:,:), allocatable ::  source_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(in_data)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = minval( &
        source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    deallocate(source_matrix)
  end subroutine regridder_upscale_min_i4

  subroutine regridder_upscale_max_dp(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    real(dp), dimension(:), intent(in) ::  in_data
    real(dp), dimension(:), intent(out) ::  out_data

    real(dp), dimension(:,:), allocatable ::  source_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(in_data)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = maxval( &
        source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    deallocate(source_matrix)
  end subroutine regridder_upscale_max_dp

  subroutine regridder_upscale_max_i4(this, in_data, out_data)
    class(regridder), intent(inout) :: this
    integer(i4), dimension(:), intent(in) ::  in_data
    integer(i4), dimension(:), intent(out) ::  out_data

    integer(i4), dimension(:,:), allocatable ::  source_matrix
    integer(i4) :: k

    call check_upscaling(this%scaling_mode)
    allocate(source_matrix(this%fine_grid%nx, this%fine_grid%ny))
    source_matrix = this%fine_grid%unpack_data(in_data)
    ! TODO: openmp
    do k = 1_i4, this%coarse_grid%ncells
      out_data(k) = maxval( &
        source_matrix(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    deallocate(source_matrix)
  end subroutine regridder_upscale_max_i4

  subroutine check_upscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= up_scaling) call error_message("regridder: not setup for upscaling.")
  end subroutine check_upscaling

  subroutine check_downscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= down_scaling) call error_message("regridder: not setup for downscaling.")
  end subroutine check_downscaling

end module mo_regridder
