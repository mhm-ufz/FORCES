!> \file    mo_grid_scaler.f90
!> \copydoc mo_grid_scaler

!> \brief   Regridding utils.
!> \details This module provides a scaler for rectangular grids with cellsizes that have an integer ratio.
!! \par Examples
!! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
!! - \ref 03_nc_regridder.f90 : \copybrief 03_nc_regridder.f90
!!   \include 03_nc_regridder.f90
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid_scaler

  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan, ieee_is_negative
  use mo_kind, only: i4, i8, dp
  use mo_grid, only: grid_t, id_bounds
  use mo_utils, only: is_close, eq, flipped
  use mo_string_utils, only: num2str
  use mo_message, only: error_message
  use mo_constants, only: nodata_i4, nodata_dp

  implicit none

  private
  private :: scaler_init
  !> \name Scaling Indicators
  !> \brief Constants to indicate the scaling mode of the \ref scaler_t.
  !!@{
  integer(i4), public, parameter :: no_scaling = -1_i4 !< grids have same resolution
  integer(i4), public, parameter :: up_scaling = 0_i4 !< from fine to coarse grid
  integer(i4), public, parameter :: down_scaling = 1_i4 !< from coarse to fine grid
  !!@}
  !> \name Upscaling Operators
  !> \brief Constants to specify the upscaling operator in the \ref scaler_t::execute method of the \ref scaler_t.
  !!@{
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
  !!@}
  !> \name Downscaling Operators
  !> \brief Constants to specify the downscaling operator in the \ref scaler_t::execute method of the \ref scaler_t.
  !!@{
  integer(i4), public, parameter :: down_nearest = 0_i4 !< nearest neighbor downscaling operator
  integer(i4), public, parameter :: down_split = 1_i4 !< inverse sum downscaling operator with equal summands
  !!@}

  !> \class   scaler_t
  !> \brief   Scaler type to remap data on regular grids with an integer cellsize ratio and matching lower left corner.
  !> \details Scaler to up or down scale values on given grid. Provides an \ref execute method to regrid packed/unpacked data.
  !!
  !! Upscaling operator (`upscaling_operator`) can be the following:
  !! - \ref up_p_mean (0): power mean upscaling operator
  !! - \ref up_a_mean (1): arithmetic mean upscaling operator (p-mean with p=1)
  !! - \ref up_g_mean (2): geometric mean upscaling operator (p-mean with p=0)
  !! - \ref up_h_mean (3): harmonic mean upscaling operator (p-mean with p=-1)
  !! - \ref up_min (4): minimum value upscaling operator (p-mean for p -> -inf)
  !! - \ref up_max (5): maximum value upscaling operator (p-mean for p -> inf)
  !! - \ref up_sum (6): summation upscaling operator
  !! - \ref up_var (7): variance upscaling operator
  !! - \ref up_std (8): standard deviation upscaling operator
  !! - \ref up_laf (9): largest area fraction upscaling operator
  !! - \ref up_fraction (10): area fraction of given class upscaling operator
  !!
  !! Downscaling operator (`downscaling_operator`) can be the following:
  !! - \ref down_nearest (0): nearest neighbor downscaling operator
  !! - \ref down_split (1): inverse sum downscaling operator with equal summands
  !!
  !! \par Examples
  !! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
  !! - \ref 03_nc_regridder.f90 : \copybrief 03_nc_regridder.f90
  type, public :: scaler_t
    integer(i4) :: scaling_mode                           !< \ref up_scaling (0) or \ref down_scaling (1)
    type(grid_t), pointer :: source_grid => null()        !< source grid
    type(grid_t), pointer :: target_grid => null()        !< target grid
    type(grid_t), pointer :: fine_grid => null()          !< high resolution grid (source when upscaling, target when downscaling)
    type(grid_t), pointer :: coarse_grid => null()        !< low resolution grid (source when downscaling, target when upscaling)
    integer(i4) :: upscaling_operator = up_a_mean         !< default upscaling operator when executing (default: arithmetic mean)
    integer(i4) :: downscaling_operator = down_nearest    !< default downscaling operator when executing (default: nearest neighbor)
    integer(i4) :: factor                                 !< coarse_grid % cellsize / fine_grid % cellsize
    logical :: y_dir_match                                !< coarse_grid % y_direction == fine_grid % y_direction
    integer(i4), dimension(:), allocatable :: y_lb        !< lower bound for y-id on fine grid (coarse\%ncells)
    integer(i4), dimension(:), allocatable :: y_ub        !< upper bound for y-id on fine grid (coarse\%ncells)
    integer(i4), dimension(:), allocatable :: x_lb        !< lower bound for x-id on fine grid (coarse\%ncells)
    integer(i4), dimension(:), allocatable :: x_ub        !< upper bound for x-id on fine grid (coarse\%ncells)
    integer(i4), dimension(:), allocatable :: n_subcells  !< valid fine grid cells in coarse cell (coarse\%ncells)
    real(dp), dimension(:, :), allocatable :: weights     !< cell area ratios (fine\%nx,fine\%ny)
    integer(i8), dimension(:), allocatable :: id_map      !< flat index array of coarse ids (fine\%ncells)
  contains
    procedure, public :: init => scaler_init
    procedure, public :: operator_init
    ! separate routines for all packed(1d)/unpacked(2d) combinations of IO-data
    procedure, private :: scaler_exe_dp_1d_1d, scaler_exe_dp_1d_2d, scaler_exe_dp_2d_1d, scaler_exe_dp_2d_2d
    procedure, private :: scaler_exe_i4_1d_1d, scaler_exe_i4_1d_2d, scaler_exe_i4_2d_1d, scaler_exe_i4_2d_2d
    procedure, private :: scaler_exe_i4_dp_1d_1d, scaler_exe_i4_dp_1d_2d, scaler_exe_i4_dp_2d_1d, scaler_exe_i4_dp_2d_2d
    !> \brief Execute scaler.
    !> \details Execute scaler for various IO data formats and types with a specified operator.
    !! Used operator depends on the scaling mode of the scaler (fine to coarse or coarse to fine).
    !> \param[in] "integer(i4)/real(dp) :: in_data(..)" packed or unpacked input data on source grid
    !> \param[out] "integer(i4)/real(dp) :: out_data(..)" packed or unpacked output data on target grid
    !> \param[in] "integer(i4), optional :: upscaling_operator" upscaling operator (\ref up_a_mean by default)
    !> \param[in] "integer(i4), optional :: downscaling_operator" downscaling operator (\ref down_nearest by default)
    !> \param[in] "real(dp), optional :: p" exponent for power norm \ref up_p_mean
    !> \param[in] "integer(i4), optional :: class_id" class id for \ref up_fraction operator
    !> \param[in] "integer(i4), optional :: vmin" minimum of values to speed up \ref up_laf operator
    !> \param[in] "integer(i4), optional :: vmax" maximum of values to speed up \ref up_laf operator
    !!
    !! Upscaling operator (`upscaling_operator`) can be the following:
    !! - \ref up_p_mean (0): power mean upscaling operator
    !! - \ref up_a_mean (1): arithmetic mean upscaling operator (p-mean with p=1)
    !! - \ref up_g_mean (2): geometric mean upscaling operator (p-mean with p=0)
    !! - \ref up_h_mean (3): harmonic mean upscaling operator (p-mean with p=-1)
    !! - \ref up_min (4): minimum value upscaling operator (p-mean for p -> -inf)
    !! - \ref up_max (5): maximum value upscaling operator (p-mean for p -> inf)
    !! - \ref up_sum (6): summation upscaling operator
    !! - \ref up_var (7): variance upscaling operator
    !! - \ref up_std (8): standard deviation upscaling operator
    !! - \ref up_laf (9): largest area fraction upscaling operator
    !! - \ref up_fraction (10): area fraction of given class upscaling operator
    !!
    !! Downscaling operator (`downscaling_operator`) can be the following:
    !! - \ref down_nearest (0): nearest neighbor downscaling operator
    !! - \ref down_split (1): inverse sum downscaling operator with equal summands
    generic, public :: execute => scaler_exe_dp_1d_1d, scaler_exe_dp_1d_2d, scaler_exe_dp_2d_1d, scaler_exe_dp_2d_2d, &
                                  scaler_exe_i4_1d_1d, scaler_exe_i4_1d_2d, scaler_exe_i4_2d_1d, scaler_exe_i4_2d_2d, &
                                  scaler_exe_i4_dp_1d_1d, scaler_exe_i4_dp_1d_2d, &
                                  scaler_exe_i4_dp_2d_1d, scaler_exe_i4_dp_2d_2d
    procedure, private :: upscale_p_mean => scaler_upscale_p_mean
    procedure, private :: upscale_a_mean => scaler_upscale_a_mean
    procedure, private :: upscale_g_mean => scaler_upscale_g_mean
    procedure, private :: upscale_h_mean => scaler_upscale_h_mean
    procedure, private :: scaler_upscale_min_dp, scaler_upscale_min_i4
    generic, private :: upscale_min => scaler_upscale_min_dp, scaler_upscale_min_i4
    procedure, private :: scaler_upscale_max_dp, scaler_upscale_max_i4
    generic, private :: upscale_max => scaler_upscale_max_dp, scaler_upscale_max_i4
    procedure, private :: scaler_upscale_sum_dp, scaler_upscale_sum_i4
    generic, private :: upscale_sum => scaler_upscale_sum_dp, scaler_upscale_sum_i4
    procedure, private :: upscale_var => scaler_upscale_var
    procedure, private :: upscale_std => scaler_upscale_std
    procedure, private :: upscale_laf => scaler_upscale_laf
    procedure, private :: upscale_fraction => scaler_upscale_fraction
    procedure, private :: scaler_downscale_nearest_dp_1d, scaler_downscale_nearest_i4_1d
    generic, public :: downscale_nearest => scaler_downscale_nearest_dp_1d, scaler_downscale_nearest_i4_1d
    procedure, private :: scaler_downscale_nearest_dp_2d, scaler_downscale_nearest_i4_2d
    generic, public :: downscale_nearest => scaler_downscale_nearest_dp_2d, scaler_downscale_nearest_i4_2d
    procedure, private :: scaler_downscale_split_1d, scaler_downscale_split_2d
    generic, public :: downscale_split => scaler_downscale_split_1d, scaler_downscale_split_2d
  end type scaler_t

contains

  !> \brief Setup scaler from given source and target grids
  subroutine scaler_init(this, source_grid, target_grid, upscaling_operator, downscaling_operator, tol)
    use mo_constants, only : nodata_i8
    implicit none
    class(scaler_t), intent(inout) :: this
    type(grid_t), pointer, intent(in) :: source_grid !< given source grid (can be a target)
    type(grid_t), pointer, intent(in) :: target_grid !< resulting target grid (can be a target)
    integer(i4), intent(in), optional :: upscaling_operator !< default upscaling operator to use (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< default downscaling operator to use (down_nearest by default)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparison (default: 1.e-7)

    integer(i4) :: i_ub, i_lb, j_lb, j_ub, ic, jc
    integer(i8) :: k
    integer(i8), dimension(:, :), allocatable :: coarse_id_matrix
    real(dp), dimension(:), allocatable :: weights

    this%source_grid => source_grid
    this%target_grid => target_grid

    if (source_grid%cellsize < target_grid%cellsize) then
      this%scaling_mode = up_scaling
      call target_grid%check_is_filled_by(source_grid, tol=tol)
      this%fine_grid => source_grid
      this%coarse_grid => target_grid
    else if (source_grid%cellsize > target_grid%cellsize) then
      this%scaling_mode = down_scaling
      call target_grid%check_is_covered_by(source_grid, tol=tol)
      this%fine_grid => target_grid
      this%coarse_grid => source_grid
    else ! equal resolutions (only type conversion, masking and flipping)
      ! TODO: should we skip allocating the grid mapper arrays below to safe memory?
      this%scaling_mode = no_scaling
      call target_grid%check_is_filled_by(source_grid, tol=tol)
      this%fine_grid => source_grid
      this%coarse_grid => target_grid
    end if

    if (present(upscaling_operator)) this%upscaling_operator = upscaling_operator
    if (present(downscaling_operator)) this%downscaling_operator = downscaling_operator

    this%factor = nint(this%coarse_grid%cellsize / this%fine_grid%cellsize, i4)
    this%y_dir_match = this%source_grid%y_direction == this%target_grid%y_direction

    allocate(this%y_lb(this%coarse_grid%ncells))
    allocate(this%y_ub(this%coarse_grid%ncells))
    allocate(this%x_lb(this%coarse_grid%ncells))
    allocate(this%x_ub(this%coarse_grid%ncells))
    allocate(this%n_subcells(this%coarse_grid%ncells))
    allocate(this%id_map(this%fine_grid%ncells))

    allocate(coarse_id_matrix(this%fine_grid%nx, this%fine_grid%ny), source=nodata_i8)

    k = 0_i8
    do jc = 1, this%coarse_grid%ny
      do ic = 1, this%coarse_grid%nx
        if (this%coarse_grid%has_mask()) then
          if (.NOT. this%coarse_grid%mask(ic, jc)) cycle
        end if
        k = k + 1_i8
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
    this%id_map = this%fine_grid%pack(coarse_id_matrix)
    deallocate(coarse_id_matrix)

    ! determine weights
    allocate(this%weights(this%fine_grid%nx, this%fine_grid%ny))
    allocate(weights(this%fine_grid%ncells), source=1.0_dp) ! 1.0 as default (also when uncovered) and when resolutions are equal

    ! generate weights from area fractions
    if (this%scaling_mode /= no_scaling) then
      !$omp parallel do default(shared) private(k) schedule(static)
      do k = 1_i8, this%fine_grid%ncells
        if (this%id_map(k) == nodata_i8) cycle ! skip uncovered coarse cells in up-scaling
        weights(k) = this%fine_grid%cell_area(k) / this%coarse_grid%cell_area(this%id_map(k))
      end do
      !$omp end parallel do
    end if
    this%weights = this%fine_grid%unpack(weights)
    deallocate(weights)
  end subroutine scaler_init

  !> \brief Default arguments to execute scaler (arithmetic mean as upscaling, nearest neighbor as downscaling)
  subroutine operator_init(this, up_operator, down_operator, upscaling_operator, downscaling_operator)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(out) :: up_operator !< upscaling operator
    integer(i4), intent(out) :: down_operator !< downscaling operator
    integer(i4), intent(in), optional :: upscaling_operator !< provided upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< provided downscaling operator (down_nearest by default)
    up_operator = this%upscaling_operator
    if (present(upscaling_operator)) up_operator = upscaling_operator
    down_operator = this%downscaling_operator
    if (present(downscaling_operator)) down_operator = downscaling_operator
  end subroutine operator_init

  !> \brief Execute scaler for packed real input and output.
  subroutine scaler_exe_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    call this%scaler_exe_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_dp), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_dp_1d_1d

  !> \brief Execute scaler for packed real input and unpacked real output.
  subroutine scaler_exe_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_dp), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine scaler_exe_dp_1d_2d

  !> \brief Execute scaler for unpacked real input and output.
  subroutine scaler_exe_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine scaler_exe_dp_2d_2d

  !> \brief Execute scaler for unpacked real input and packed real output.
  subroutine scaler_exe_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        out_data = pack(in_data, this%target_grid%mask)
      else
        out_data = pack(flipped(in_data, idim=2), this%target_grid%mask)
      end if
    else if (this%scaling_mode == up_scaling) then
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
          call error_message("scaler: largest area fraction upscaling operator not supported for real data.")
        case (up_fraction)
          call error_message("scaler: class fraction upscaling operator not supported for real input data.")
        case default
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call this%downscale_split(in_data, out_data)
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if

  end subroutine scaler_exe_dp_2d_1d

  !> \brief Execute scaler for packed integer input and output.
  subroutine scaler_exe_i4_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    call this%scaler_exe_i4_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_i4_1d_1d

  !> \brief Execute scaler for packed integer input and unpacked integer output.
  subroutine scaler_exe_i4_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_i4_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_i4)
  end subroutine scaler_exe_i4_1d_2d

  !> \brief Execute scaler for unpacked integer input output.
  subroutine scaler_exe_i4_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_i4_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_i4)
  end subroutine scaler_exe_i4_2d_2d

  !> \brief Execute scaler for unpacked integer input and packed integer output.
  subroutine scaler_exe_i4_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        out_data = pack(in_data, this%target_grid%mask)
      else
        out_data = pack(flipped(in_data, idim=2), this%target_grid%mask)
      end if
    else if (this%scaling_mode == up_scaling) then
      select case (up_operator)
        case (up_p_mean)
          call error_message("scaler: p-mean upscaling operator not supported for integer data.")
        case (up_a_mean)
          call error_message("scaler: arithmetic mean upscaling operator not supported for integer data.")
        case (up_g_mean)
          call error_message("scaler: geometric mean upscaling operator not supported for integer data.")
        case (up_h_mean)
          call error_message("scaler: harmonic mean upscaling operator not supported for integer data.")
        case (up_min)
          call this%upscale_min(in_data, out_data)
        case (up_max)
          call this%upscale_max(in_data, out_data)
        case (up_sum)
          call this%upscale_sum(in_data, out_data)
        case (up_var)
          call error_message("scaler: variance upscaling operator not supported for integer data.")
        case (up_std)
          call error_message("scaler: standard deviation upscaling not supported for integer data.")
        case (up_laf)
          call this%upscale_laf(in_data, out_data, vmin, vmax)
        case (up_fraction)
          call error_message("scaler: class fraction upscaling operator not supported for integer output data.")
        case default
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call error_message("scaler: equal summand downscaling operator not supported for integer output data.")
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if
  end subroutine scaler_exe_i4_2d_1d

  !> \brief Execute scaler for packed integer input and packed real output.
  subroutine scaler_exe_i4_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    call this%scaler_exe_i4_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_i4_dp_1d_1d

  !> \brief Execute scaler for packed integer input and unpacked real output.
  subroutine scaler_exe_i4_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_i4_dp_2d_1d( &
      unpack(in_data,this%source_grid%mask,nodata_i4), temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine scaler_exe_i4_dp_1d_2d

  !> \brief Execute scaler for unpacked integer input and unpacked real output.
  subroutine scaler_exe_i4_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%nx,this%target_grid%ny), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), dimension(this%target_grid%ncells) :: temp
    call this%scaler_exe_i4_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    out_data = unpack(temp, this%target_grid%mask, nodata_dp)
  end subroutine scaler_exe_i4_dp_2d_2d

  !> \brief Execute scaler for unpacked integer input and packed real output.
  subroutine scaler_exe_i4_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data !< input data on source grid
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator (huge() by default - tbd)
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator (-huge() by default - tbd)

    integer(i4), dimension(:), allocatable :: temp

    integer(i4) :: up_operator, down_operator
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only converting, masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        out_data = pack(real(in_data, dp), this%target_grid%mask)
      else
        out_data = pack(flipped(real(in_data, dp), idim=2), this%target_grid%mask)
      end if
    else if (this%scaling_mode == up_scaling) then
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
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator)))
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(real(in_data, dp), out_data)
        case (down_split)
          call this%downscale_split(real(in_data, dp), out_data)
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator)))
      end select
    end if
  end subroutine scaler_exe_i4_dp_2d_1d

  subroutine scaler_upscale_p_mean(this, in_data, out_data, p)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    real(dp), intent(in), optional :: p !< exponent for the p-norm (1.0 for arithmetic mean by default)
    integer(i8) :: k
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
      do k = 1_i8, this%coarse_grid%ncells
        out_data(k) = sum( &
            pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                 this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) ** p_ &
          * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
                 this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        ) ** (1.0_dp / p_)
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_p_mean

  subroutine scaler_upscale_a_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = sum( &
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_a_mean

  subroutine scaler_upscale_g_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = exp(sum(log( & ! prod(xi^wi) = exp(sum(wi*log(xi)))
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_g_mean

  subroutine scaler_upscale_h_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = 1.0_dp / sum( 1.0_dp / &
          pack(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))) &
        * pack(this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
               this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k))))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_h_mean

  subroutine scaler_upscale_min_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = minval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_min_dp

  subroutine scaler_upscale_min_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = minval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_min_i4

  subroutine scaler_upscale_max_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = maxval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_max_dp

  subroutine scaler_upscale_max_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = maxval( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_max_i4

  subroutine scaler_upscale_sum_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = sum( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_sum_dp

  subroutine scaler_upscale_sum_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = sum( &
        in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_sum_i4

  subroutine scaler_upscale_var(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    real(dp) :: mean
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k, mean) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
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
  end subroutine scaler_upscale_var

  subroutine scaler_upscale_std(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    call this%upscale_var(in_data, out_data)
    out_data = sqrt(out_data)
  end subroutine scaler_upscale_std

  subroutine scaler_upscale_laf(this, in_data, out_data, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up operator
    integer(i4) :: i, laf_v, cnt_v, cnt_i, min_v, max_v
    integer(i8) :: k

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
    do k = 1_i8, this%coarse_grid%ncells
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
  end subroutine scaler_upscale_laf

  subroutine scaler_upscale_fraction(this, in_data, out_data, class_id)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i4), intent(in), optional :: class_id !< class id to determine area fraction of
    integer(i4) :: cls_id
    integer(i8) :: k

    call check_upscaling(this%scaling_mode)

    cls_id = nodata_i4
    if (present(class_id)) cls_id = class_id

    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      out_data(k) = sum( &
        this%weights(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)), &
        mask=(in_data(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)) == cls_id) &
              .and. this%fine_grid%mask(this%x_lb(k):this%x_ub(k), this%y_lb(k):this%y_ub(k)))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_fraction

  subroutine scaler_downscale_nearest_dp_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_dp_1d

  subroutine scaler_downscale_nearest_i4_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%ncells), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_i4_1d

  subroutine scaler_downscale_split_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%ncells), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k)) / real(this%n_subcells(this%id_map(k)), dp)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_split_1d

  subroutine scaler_downscale_nearest_dp_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i4) :: i, j
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_dp_2d

  subroutine scaler_downscale_nearest_i4_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    integer(i4), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i4) :: i, j
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_i4_2d

  subroutine scaler_downscale_split_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), dimension(this%source_grid%nx,this%source_grid%ny), intent(in) :: in_data
    real(dp), dimension(this%target_grid%ncells), intent(out) :: out_data
    integer(i4) :: i, j
    integer(i8) :: k
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(k,i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j) / real(this%n_subcells(this%id_map(k)), dp)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_split_2d

  subroutine check_upscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= up_scaling) call error_message("scaler: not setup for upscaling.")
  end subroutine check_upscaling

  subroutine check_downscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= down_scaling) call error_message("scaler: not setup for downscaling.")
  end subroutine check_downscaling

end module mo_grid_scaler
