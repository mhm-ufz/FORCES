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
  use mo_grid, only: grid_t, cartesian, spherical, top_down, bottom_up
  use mo_grid_helper, only: id_bounds, coarse_ij, check_factor, dist_latlon
  use mo_utils, only: is_close, eq, flipped, optval
  use mo_string_utils, only: num2str
  use mo_message, only: error_message
  use mo_constants, only: nodata_i4
  use mo_orderpack, only: omedian
  use mo_spatial_index, only: spatial_index_t

  implicit none

  private
  integer(i8), parameter :: nearest_regridder_loop_parallel_min_n = 2048_i8
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
  integer(i4), public, parameter :: up_median = 11_i4 !< median upscaling operator
  !!@}
  !> \name Downscaling Operators
  !> \brief Constants to specify the downscaling operator in the \ref scaler_t::execute method of the \ref scaler_t.
  !!@{
  integer(i4), public, parameter :: down_nearest = 0_i4 !< nearest neighbor downscaling operator
  integer(i4), public, parameter :: down_split = 1_i4 !< inverse sum downscaling operator with equal summands
  !!@}
  !> \name Weight Mode Operators
  !> \brief Constants to specify the weight calculation mode in the \ref scaler_t::init method of the \ref scaler_t.
  !!@{
  integer(i4), public, parameter :: weight_area = 0_i4 !< area based weights (default)
  integer(i4), public, parameter :: weight_count = 1_i4 !< sub-cell count based weights
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
  !! - \ref up_median (11): median upscaling operator
  !!
  !! Downscaling operator (`downscaling_operator`) can be the following:
  !! - \ref down_nearest (0): nearest neighbor downscaling operator
  !! - \ref down_split (1): inverse sum downscaling operator with equal summands
  !!
  !! \par Examples
  !! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
  !! - \ref 03_nc_regridder.f90 : \copybrief 03_nc_regridder.f90
  type, public :: scaler_t
    integer(i4) :: scaling_mode                             !< \ref up_scaling (0), \ref down_scaling (1) or \ref no_scaling (-1)
    type(grid_t), pointer :: source_grid => null()          !< source grid
    type(grid_t), pointer :: target_grid => null()          !< target grid
    type(grid_t), pointer :: fine_grid => null()            !< high resolution grid (target when downscaling, source otherwise)
    type(grid_t), pointer :: coarse_grid => null()          !< low resolution grid (source when downscaling, target otherwise)
    integer(i4) :: upscaling_operator = up_a_mean           !< default upscaling operator when executing (default: arithmetic mean)
    integer(i4) :: downscaling_operator = down_nearest      !< default downscaling operator when executing (default: nearest)
    integer(i4) :: weight_mode = weight_area                !< weight mode (default: area based)
    integer(i4) :: factor                                   !< coarse_grid % cellsize / fine_grid % cellsize
    logical :: y_dir_match                                  !< coarse_grid % y_direction == fine_grid % y_direction
    logical :: cache_bounds                                 !< flag to cache bounds for coarse cells
    integer(i4), private, dimension(:), allocatable :: y_lb !< cached lower bound for y-id on fine grid (coarse\%ncells)
    integer(i4), private, dimension(:), allocatable :: y_ub !< cached upper bound for y-id on fine grid (coarse\%ncells)
    integer(i4), private, dimension(:), allocatable :: x_lb !< cached lower bound for x-id on fine grid (coarse\%ncells)
    integer(i4), private, dimension(:), allocatable :: x_ub !< cached upper bound for x-id on fine grid (coarse\%ncells)
    integer(i4), dimension(:), allocatable :: n_subcells    !< valid fine grid cells in coarse cell (coarse\%ncells)
    integer(i8), dimension(:), allocatable :: id_map        !< flat index array of coarse ids (fine\%ncells)
    real(dp), dimension(:, :), allocatable :: weights       !< cell area ratios (fine\%nx,fine\%ny)
    real(dp), dimension(:), allocatable :: coarse_weights   !< weights based on sub-cell count (coarse\%ncells)
  contains
    procedure, public :: init => scaler_init
    procedure, private :: scaler_coarse_ij, scaler_coarse_ij_cell
    generic, public :: coarse_ij => scaler_coarse_ij, scaler_coarse_ij_cell
    procedure, private :: scaler_coarse_bounds, scaler_coarse_bounds_cell
    generic, public :: coarse_bounds => scaler_coarse_bounds, scaler_coarse_bounds_cell
    procedure, public :: operator_init
    procedure, private :: check_packed_source => scaler_check_packed_source
    procedure, private :: check_packed_target => scaler_check_packed_target
    procedure, private :: check_unpacked_source => scaler_check_unpacked_source
    procedure, private :: check_unpacked_target => scaler_check_unpacked_target
    procedure, private :: check_packed_fine => scaler_check_packed_fine
    procedure, private :: check_packed_coarse => scaler_check_packed_coarse
    procedure, private :: check_unpacked_fine => scaler_check_unpacked_fine
    procedure, private :: check_unpacked_coarse => scaler_check_unpacked_coarse
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
    !! - \ref up_median (11): median upscaling operator
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
    procedure, private :: upscale_median => scaler_upscale_median
    procedure, private :: upscale_laf => scaler_upscale_laf
    procedure, private :: upscale_fraction => scaler_upscale_fraction
    procedure, private :: scaler_downscale_nearest_dp_1d, scaler_downscale_nearest_i4_1d
    procedure, private :: scaler_downscale_nearest_dp_2d, scaler_downscale_nearest_i4_2d
    !> \brief Nearest neighbor downscaling.
    generic, public :: downscale_nearest => scaler_downscale_nearest_dp_2d, scaler_downscale_nearest_i4_2d, &
                                            scaler_downscale_nearest_dp_1d, scaler_downscale_nearest_i4_1d
    procedure, private :: scaler_downscale_split_1d, scaler_downscale_split_2d
    !> \brief Inverse sum downscaling with equal summands.
    generic, public :: downscale_split => scaler_downscale_split_1d, scaler_downscale_split_2d
    procedure, private :: scaler_maxloc_dp_1d, scaler_maxloc_dp_2d, scaler_maxloc_i4_1d, scaler_maxloc_i4_2d
    !> \brief Find location of maximum value in coarse cell on fine grid by cell id.
    generic, private :: maxloc => scaler_maxloc_dp_1d, scaler_maxloc_dp_2d, scaler_maxloc_i4_1d, scaler_maxloc_i4_2d
    procedure, private :: scaler_minloc_dp_1d, scaler_minloc_dp_2d, scaler_minloc_i4_1d, scaler_minloc_i4_2d
    !> \brief Find location of minimum value in coarse cell on fine grid by cell id.
    generic, private :: minloc => scaler_minloc_dp_1d, scaler_minloc_dp_2d, scaler_minloc_i4_1d, scaler_minloc_i4_2d
  end type scaler_t

  !> \class nearest_regridder_t
  !> \brief Nearest-neighbor regridder for arbitrary source and target grids.
  !> \details Builds a target-to-source cell map once and reuses it for packed
  !! and unpacked execute calls. Mapping is done over active source cells only
  !! and returns values on active target cells only.
  type, public :: nearest_regridder_t
    type(grid_t), pointer :: source_grid => null()     !< Source grid used to build the mapping.
    type(grid_t), pointer :: target_grid => null()     !< Target grid receiving mapped values.
    integer(i8), allocatable :: id_map(:)              !< Nearest source cell id for each active target cell.
    integer(i4), private :: mapping_coordsys = cartesian !< Coordinate space used for the mapping.
    logical, private :: source_use_aux = .false.       !< Use auxiliary lon/lat on the source grid.
    logical, private :: target_use_aux = .false.       !< Use auxiliary lon/lat on the target grid.
  contains
    procedure, public :: init => nearest_regridder_init
    procedure, private :: nearest_regridder_exe_dp_1d_1d, nearest_regridder_exe_dp_1d_2d
    procedure, private :: nearest_regridder_exe_dp_2d_1d, nearest_regridder_exe_dp_2d_2d
    procedure, private :: nearest_regridder_exe_i4_1d_1d, nearest_regridder_exe_i4_1d_2d
    procedure, private :: nearest_regridder_exe_i4_2d_1d, nearest_regridder_exe_i4_2d_2d
    procedure, private :: select_space => nearest_regridder_select_space
    procedure, private :: check_ready => nearest_regridder_check_ready
    procedure, private :: check_packed_source => nearest_regridder_check_packed_source
    procedure, private :: check_packed_target => nearest_regridder_check_packed_target
    procedure, private :: check_unpacked_source => nearest_regridder_check_unpacked_source
    procedure, private :: check_unpacked_target => nearest_regridder_check_unpacked_target
    procedure, private :: prepare_source_aux_vertices => nearest_regridder_prepare_source_aux_vertices
    procedure, private :: collect_target_points => nearest_regridder_collect_target_points
    procedure, private :: query_source_ids => nearest_regridder_query_source_ids
    procedure, private :: derive_target_mask => nearest_regridder_derive_target_mask
    procedure, private :: find_containing_source_cell => nearest_regridder_find_containing_source_cell
    procedure, private :: candidate_metric => nearest_regridder_candidate_metric
    procedure, private :: apply_target_mask => nearest_regridder_apply_target_mask
    procedure, private :: build_id_map => nearest_regridder_build_id_map
    procedure, private :: repack_id_map => nearest_regridder_repack_id_map
    procedure, private :: map_dp => nearest_regridder_map_dp
    procedure, private :: map_i4 => nearest_regridder_map_i4
    !> \brief Execute nearest-neighbor remapping.
    !> \details Supports packed and unpacked integer and real input/output in the
    !! same shape combinations as \ref scaler_t::execute when input and output
    !! use the same data type.
    generic, public :: execute => nearest_regridder_exe_dp_1d_1d, nearest_regridder_exe_dp_1d_2d, &
                                  nearest_regridder_exe_dp_2d_1d, nearest_regridder_exe_dp_2d_2d, &
                                  nearest_regridder_exe_i4_1d_1d, nearest_regridder_exe_i4_1d_2d, &
                                  nearest_regridder_exe_i4_2d_1d, nearest_regridder_exe_i4_2d_2d
  end type nearest_regridder_t

contains

  !> \brief Setup scaler from given source and target grids
  subroutine scaler_init(this, source_grid, target_grid, upscaling_operator, downscaling_operator, weight_mode, cache_bounds, tol)
    use mo_constants, only : nodata_i8
    implicit none
    class(scaler_t), intent(inout) :: this
    type(grid_t), pointer, intent(in) :: source_grid !< given source grid (can be a target)
    type(grid_t), pointer, intent(in) :: target_grid !< resulting target grid (can be a target)
    integer(i4), intent(in), optional :: upscaling_operator !< default upscaling operator to use (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< default downscaling operator to use (down_nearest by default)
    integer(i4), intent(in), optional :: weight_mode !< weight mode (weight_area by default)
    logical, intent(in), optional :: cache_bounds !< flag to cache coarse cell bounds (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparison (default: 1.e-7)

    integer(i4) :: i_ub, i_lb, j_lb, j_ub, ic, jc
    integer(i8) :: k
    integer(i8), dimension(:, :), allocatable :: coarse_id_matrix
    real(dp), dimension(:), allocatable :: weights
    logical :: is_upscaling

    this%upscaling_operator = optval(upscaling_operator, default=up_a_mean)
    this%downscaling_operator = optval(downscaling_operator, default=down_nearest)
    this%weight_mode = optval(weight_mode, default=weight_area)
    this%cache_bounds = optval(cache_bounds, default=.true.)

    this%source_grid => source_grid
    this%target_grid => target_grid
    this%y_dir_match = this%source_grid%y_direction == this%target_grid%y_direction

    ! first check scaling factor
    is_upscaling = source_grid%cellsize < target_grid%cellsize
    if (is_upscaling) then
      call check_factor(source_grid%cellsize, target_grid%cellsize, factor=this%factor, tol=tol)
    else
      call check_factor(target_grid%cellsize, source_grid%cellsize, factor=this%factor, tol=tol)
    end if

    if (this%factor == 1_i4) then
      ! equal resolutions (only type conversion, masking and flipping)
      this%scaling_mode = no_scaling
      call target_grid%check_is_filled_by(source_grid, tol=tol)
      this%fine_grid => source_grid
      this%coarse_grid => target_grid
      this%cache_bounds = .false.
      this%weight_mode = weight_count
    else if (is_upscaling) then
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

    if (this%cache_bounds) then
      allocate(this%y_lb(this%coarse_grid%ncells))
      allocate(this%y_ub(this%coarse_grid%ncells))
      allocate(this%x_lb(this%coarse_grid%ncells))
      allocate(this%x_ub(this%coarse_grid%ncells))
    end if

    allocate(this%id_map(this%fine_grid%ncells))
    allocate(coarse_id_matrix(this%coarse_grid%nx, this%coarse_grid%ny))
    call this%coarse_grid%gen_id_matrix(coarse_id_matrix)

    !$omp parallel do default(shared) private(ic,jc) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      call this%coarse_ij(k, ic, jc)
      this%id_map(k) = coarse_id_matrix(ic, jc) ! this also picks nodata values
    end do
    !$omp end parallel do
    deallocate(coarse_id_matrix)

    ! skip further initialization for no scaling
    if (this%scaling_mode == no_scaling) then
      ! TODO: this actually doesn't respect smaller target grid masks (should be 0 if source cell has no target coverage)
      allocate(this%n_subcells(this%coarse_grid%ncells), source=1_i4)
      allocate(this%coarse_weights(this%coarse_grid%ncells), source=1.0_dp)
      return
    end if

    allocate(this%n_subcells(this%coarse_grid%ncells))
    allocate(this%coarse_weights(this%coarse_grid%ncells))
    !$omp parallel do default(shared) private(i_lb,i_ub,j_lb,j_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      ! coarse_bounds only uses cache when called with cell ID, so we are safe here calling with (i,j)
      call this%coarse_bounds(this%coarse_grid%cell_ij(k,1), this%coarse_grid%cell_ij(k,2), i_lb, i_ub, j_lb, j_ub)
      ! store bounds
      if (this%cache_bounds) then
        this%x_lb(k) = i_lb
        this%x_ub(k) = i_ub
        this%y_lb(k) = j_lb
        this%y_ub(k) = j_ub
      end if
      ! count valid fine cells in coarse cell (mask should be always present here)
      this%n_subcells(k) = count(this%fine_grid%mask(i_lb : i_ub, j_lb : j_ub))
      ! compute coarse weights for sub-cell count based weighting either way
      this%coarse_weights(k) = 1.0_dp / real(max(1_i4, this%n_subcells(k)), dp)
    end do
    !$omp end parallel do

    ! determine area based weights
    if (this%weight_mode == weight_area) then
      allocate(this%weights(this%fine_grid%nx, this%fine_grid%ny))
      allocate(weights(this%fine_grid%ncells))
      ! generate weights from area fractions
      !$omp parallel do default(shared) private(k) schedule(static)
      do k = 1_i8, this%fine_grid%ncells
        if (this%id_map(k) == nodata_i8) then
          weights(k) = 1.0_dp ! skip uncovered coarse cells in up-scaling
        else
          weights(k) = this%fine_grid%cell_area(k) / this%coarse_grid%cell_area(this%id_map(k))
        end if
      end do
      !$omp end parallel do
      call this%fine_grid%unpack_into(weights, this%weights)
      deallocate(weights)
    end if

  end subroutine scaler_init

  !> \brief Setup nearest-neighbor regridder from source and target grids.
  !> \details Builds a transient spatial index over the active source cells and
  !! stores the nearest source cell id for every active target cell. When
  !! `derive_target_mask=.true.`, the target grid mask is first reduced by
  !! local source-cell containment before the final active-cell mapping is built.
  subroutine nearest_regridder_init(this, source_grid, target_grid, use_aux, derive_target_mask)
    class(nearest_regridder_t), intent(inout) :: this
    type(grid_t), pointer, intent(in) :: source_grid
    type(grid_t), pointer, intent(in) :: target_grid
    logical, intent(in), optional :: use_aux
    logical, intent(in), optional :: derive_target_mask

    type(spatial_index_t) :: index
    integer(i4), allocatable :: old_target_cell_ij(:, :)
    real(dp), allocatable :: target_points(:, :)
    integer(i8), allocatable :: nearest_ids(:)
    logical, allocatable :: target_mask(:, :)
    logical :: use_aux_, derive_target_mask_

    use_aux_ = optval(use_aux, .false.)
    derive_target_mask_ = optval(derive_target_mask, .false.)

    this%source_grid => source_grid
    this%target_grid => target_grid
    if (allocated(this%id_map)) deallocate(this%id_map)

    call this%select_space(use_aux_)
    if (derive_target_mask_) then
      call this%prepare_source_aux_vertices()
      allocate(target_mask(this%target_grid%nx, this%target_grid%ny), source=this%target_grid%mask)
      if (this%source_grid%ncells < 1_i8) then
        target_mask = .false.
        if (any(target_mask .neqv. this%target_grid%mask)) call this%apply_target_mask(target_mask)
        allocate(this%id_map(this%target_grid%ncells))
        return
      end if

      allocate(old_target_cell_ij(this%target_grid%ncells, 2), source=this%target_grid%cell_ij)
      call this%collect_target_points(target_points)
      call this%source_grid%build_spatial_index(index, use_aux=this%source_use_aux)
      call this%query_source_ids(index, target_points, nearest_ids)
      call this%derive_target_mask(target_points, nearest_ids, target_mask)

      if (any(target_mask .neqv. this%target_grid%mask)) then
        call this%apply_target_mask(target_mask)
      end if
      call this%repack_id_map(old_target_cell_ij, nearest_ids)
      return
    end if
    call this%build_id_map()
  end subroutine nearest_regridder_init

  subroutine nearest_regridder_exe_dp_1d_1d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:)
    real(dp), intent(out) :: out_data(:)

    call this%check_ready()
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%map_dp(in_data, out_data)
  end subroutine nearest_regridder_exe_dp_1d_1d

  subroutine nearest_regridder_exe_dp_1d_2d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:)
    real(dp), intent(out) :: out_data(:, :)

    real(dp), allocatable :: temp_out(:)

    call this%check_ready()
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_out(this%target_grid%ncells))
    call this%map_dp(in_data, temp_out)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine nearest_regridder_exe_dp_1d_2d

  subroutine nearest_regridder_exe_dp_2d_1d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)

    real(dp), allocatable :: temp_in(:)

    call this%check_ready()
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    allocate(temp_in(this%source_grid%ncells))
    call this%source_grid%pack_into(in_data, temp_in)
    call this%map_dp(temp_in, out_data)
  end subroutine nearest_regridder_exe_dp_2d_1d

  subroutine nearest_regridder_exe_dp_2d_2d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:, :)

    real(dp), allocatable :: temp_in(:), temp_out(:)

    call this%check_ready()
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_in(this%source_grid%ncells))
    allocate(temp_out(this%target_grid%ncells))
    call this%source_grid%pack_into(in_data, temp_in)
    call this%map_dp(temp_in, temp_out)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine nearest_regridder_exe_dp_2d_2d

  subroutine nearest_regridder_exe_i4_1d_1d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:)
    integer(i4), intent(out) :: out_data(:)

    call this%check_ready()
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%map_i4(in_data, out_data)
  end subroutine nearest_regridder_exe_i4_1d_1d

  subroutine nearest_regridder_exe_i4_1d_2d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:)
    integer(i4), intent(out) :: out_data(:, :)

    integer(i4), allocatable :: temp_out(:)

    call this%check_ready()
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_out(this%target_grid%ncells))
    call this%map_i4(in_data, temp_out)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine nearest_regridder_exe_i4_1d_2d

  subroutine nearest_regridder_exe_i4_2d_1d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)

    integer(i4), allocatable :: temp_in(:)

    call this%check_ready()
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    allocate(temp_in(this%source_grid%ncells))
    call this%source_grid%pack_into(in_data, temp_in)
    call this%map_i4(temp_in, out_data)
  end subroutine nearest_regridder_exe_i4_2d_1d

  subroutine nearest_regridder_exe_i4_2d_2d(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:, :)

    integer(i4), allocatable :: temp_in(:), temp_out(:)

    call this%check_ready()
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_in(this%source_grid%ncells))
    allocate(temp_out(this%target_grid%ncells))
    call this%source_grid%pack_into(in_data, temp_in)
    call this%map_i4(temp_in, temp_out)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine nearest_regridder_exe_i4_2d_2d

  subroutine nearest_regridder_select_space(this, use_aux)
    class(nearest_regridder_t), intent(inout) :: this
    logical, intent(in) :: use_aux

    this%mapping_coordsys = cartesian
    this%source_use_aux = .false.
    this%target_use_aux = .false.

    if (this%source_grid%coordsys == cartesian .and. this%target_grid%coordsys == cartesian) then
      if (use_aux) then
        if (.not. this%source_grid%has_aux_coords()) then
          call error_message("nearest_regridder % init: source grid has no auxiliary lon/lat coordinates.") ! LCOV_EXCL_LINE
        end if
        if (.not. this%target_grid%has_aux_coords()) then
          call error_message("nearest_regridder % init: target grid has no auxiliary lon/lat coordinates.") ! LCOV_EXCL_LINE
        end if
        this%mapping_coordsys = spherical
        this%source_use_aux = .true.
        this%target_use_aux = .true.
      end if
      return
    end if

    this%mapping_coordsys = spherical
    if (this%source_grid%coordsys == cartesian) then
      if (.not. this%source_grid%has_aux_coords()) then
        call error_message("nearest_regridder % init: Cartesian source grid needs auxiliary lon/lat coordinates", &
                           " for spherical mapping.") ! LCOV_EXCL_LINE
      end if
      this%source_use_aux = .true.
    end if
    if (this%target_grid%coordsys == cartesian) then
      if (.not. this%target_grid%has_aux_coords()) then
        call error_message("nearest_regridder % init: Cartesian target grid needs auxiliary lon/lat coordinates", &
                           " for spherical mapping.") ! LCOV_EXCL_LINE
      end if
      this%target_use_aux = .true.
    end if
  end subroutine nearest_regridder_select_space

  subroutine nearest_regridder_prepare_source_aux_vertices(this)
    class(nearest_regridder_t), intent(inout) :: this

    if (.not. this%source_use_aux) return
    if (this%source_grid%has_aux_vertices()) return
    if (this%source_grid%nx <= 1_i4 .or. this%source_grid%ny <= 1_i4) then
      call error_message("nearest_regridder % init: source grid needs auxiliary vertices for derived masking", &
                         " in aux mode and cannot estimate them for singleton dimensions.") ! LCOV_EXCL_LINE
    end if
    call this%source_grid%estimate_aux_vertices()
  end subroutine nearest_regridder_prepare_source_aux_vertices

  subroutine nearest_regridder_collect_target_points(this, target_points)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), allocatable, intent(out) :: target_points(:, :)

    allocate(target_points(this%target_grid%ncells, 2))
    if (this%mapping_coordsys == cartesian) then
      call nearest_regridder_collect_cartesian_points(this%target_grid, target_points)
    else
      call nearest_regridder_collect_lonlat_points(this%target_grid, this%target_use_aux, target_points)
    end if
  end subroutine nearest_regridder_collect_target_points

  subroutine nearest_regridder_query_source_ids(this, index, target_points, nearest_ids)
    class(nearest_regridder_t), intent(in) :: this
    type(spatial_index_t), intent(in) :: index
    real(dp), intent(in) :: target_points(:, :)
    integer(i8), allocatable, intent(out) :: nearest_ids(:)

    if (this%mapping_coordsys == cartesian) then
      nearest_ids = index%nearest_ids(target_points)
    else
      nearest_ids = index%nearest_ids_lonlat(target_points)
    end if
  end subroutine nearest_regridder_query_source_ids

  subroutine nearest_regridder_derive_target_mask(this, target_points, nearest_ids, target_mask)
    class(nearest_regridder_t), intent(inout) :: this
    real(dp), intent(in) :: target_points(:, :)
    integer(i8), intent(in) :: nearest_ids(:)
    logical, intent(inout) :: target_mask(:, :)

    integer(i8) :: k
    integer(i4) :: src_i, src_j, src_i_found, src_j_found, tgt_i, tgt_j
    logical :: found

    if (this%target_grid%ncells < 1_i8) return

    if (size(target_points, 1, kind=i8) /= this%target_grid%ncells .or. size(target_points, 2) /= 2_i4) then
      call error_message("nearest_regridder % init: target point buffer has wrong shape.") ! LCOV_EXCL_LINE
    end if
    if (size(nearest_ids, kind=i8) /= this%target_grid%ncells) then
      call error_message("nearest_regridder % init: nearest source ids have wrong size.") ! LCOV_EXCL_LINE
    end if
    if (size(target_mask, 1) /= this%target_grid%nx .or. size(target_mask, 2) /= this%target_grid%ny) then
      call error_message("nearest_regridder % init: target mask buffer has wrong shape.") ! LCOV_EXCL_LINE
    end if

    !$omp parallel do default(shared) private(k,tgt_i,tgt_j,src_i,src_j,found,src_i_found,src_j_found) schedule(static) &
    !$omp if(this%target_grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, this%target_grid%ncells
      tgt_i = this%target_grid%cell_ij(k, 1)
      tgt_j = this%target_grid%cell_ij(k, 2)
      src_i = this%source_grid%cell_ij(nearest_ids(k), 1)
      src_j = this%source_grid%cell_ij(nearest_ids(k), 2)
      call this%find_containing_source_cell(src_i, src_j, target_points(k, 1), target_points(k, 2), &
                                            found, src_i_found, src_j_found)
      if (.not. found) then
        target_mask(tgt_i, tgt_j) = .false.
      else if (.not. this%source_grid%mask(src_i_found, src_j_found)) then
        target_mask(tgt_i, tgt_j) = .false.
      end if
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_derive_target_mask

  subroutine nearest_regridder_find_containing_source_cell(this, src_i0, src_j0, x, y, found, src_i, src_j)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: src_i0
    integer(i4), intent(in) :: src_j0
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y
    logical, intent(out) :: found
    integer(i4), intent(out) :: src_i
    integer(i4), intent(out) :: src_j

    integer(i4) :: di, dj, icand, insert_pos, ncand
    integer(i4) :: cand_i(8), cand_j(8)
    integer(i8) :: cand_full_id(8), full_id
    real(dp) :: cand_metric(8), metric

    src_i = src_i0
    src_j = src_j0
    found = this%source_grid%in_cell(src_i0, src_j0, x, y, aux=this%source_use_aux)
    if (found) return

    ncand = 0_i4
    do dj = -1_i4, 1_i4
      do di = -1_i4, 1_i4
        src_i = src_i0 + di
        src_j = src_j0 + dj
        if (di == 0_i4 .and. dj == 0_i4) cycle
        if (src_i < 1_i4 .or. src_i > this%source_grid%nx) cycle
        if (src_j < 1_i4 .or. src_j > this%source_grid%ny) cycle

        metric = this%candidate_metric(src_i, src_j, x, y)
        full_id = int(src_j - 1_i4, i8) * int(this%source_grid%nx, i8) + int(src_i, i8)
        insert_pos = ncand + 1_i4
        do while (insert_pos > 1_i4)
          if ((metric < cand_metric(insert_pos - 1_i4) .and. .not. is_close(metric, cand_metric(insert_pos - 1_i4))) .or. &
              (is_close(metric, cand_metric(insert_pos - 1_i4)) .and. full_id < cand_full_id(insert_pos - 1_i4))) then
            insert_pos = insert_pos - 1_i4
          else
            exit
          end if
        end do
        do icand = ncand, insert_pos, -1_i4
          cand_i(icand + 1_i4) = cand_i(icand)
          cand_j(icand + 1_i4) = cand_j(icand)
          cand_metric(icand + 1_i4) = cand_metric(icand)
          cand_full_id(icand + 1_i4) = cand_full_id(icand)
        end do
        cand_i(insert_pos) = src_i
        cand_j(insert_pos) = src_j
        cand_metric(insert_pos) = metric
        cand_full_id(insert_pos) = full_id
        ncand = ncand + 1_i4
      end do
    end do

    do icand = 1_i4, ncand
      src_i = cand_i(icand)
      src_j = cand_j(icand)
      found = this%source_grid%in_cell(src_i, src_j, x, y, aux=this%source_use_aux)
      if (found) return
    end do

    src_i = src_i0
    src_j = src_j0
    found = .false.
  end subroutine nearest_regridder_find_containing_source_cell

  pure real(dp) function nearest_regridder_candidate_metric(this, src_i, src_j, x, y) result(metric)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: src_i
    integer(i4), intent(in) :: src_j
    real(dp), intent(in) :: x
    real(dp), intent(in) :: y

    real(dp) :: dx, dy

    if (this%mapping_coordsys == cartesian) then
      dx = this%source_grid%x_center(src_i) - x
      dy = this%source_grid%y_center(src_j) - y
      metric = dx * dx + dy * dy
    else if (this%source_use_aux) then
      metric = dist_latlon(this%source_grid%lat(src_i, src_j), this%source_grid%lon(src_i, src_j), y, x)
    else
      metric = dist_latlon(this%source_grid%y_center(src_j), this%source_grid%x_center(src_i), y, x)
    end if
  end function nearest_regridder_candidate_metric

  subroutine nearest_regridder_apply_target_mask(this, target_mask)
    class(nearest_regridder_t), intent(inout) :: this
    logical, intent(in) :: target_mask(:, :)

    real(dp), allocatable :: area_matrix(:, :)
    real(dp), allocatable :: packed_area(:)
    logical :: has_cell_area

    if (size(target_mask, 1) /= this%target_grid%nx .or. size(target_mask, 2) /= this%target_grid%ny) then
      call error_message("nearest_regridder % init: derived target mask has wrong shape.") ! LCOV_EXCL_LINE
    end if

    has_cell_area = allocated(this%target_grid%cell_area)
    if (has_cell_area) then
      allocate(area_matrix(this%target_grid%nx, this%target_grid%ny))
      call this%target_grid%unpack_into(this%target_grid%cell_area, area_matrix)
    end if

    this%target_grid%mask = target_mask
    call this%target_grid%calculate_cell_ids()

    if (has_cell_area) then
      allocate(packed_area(this%target_grid%ncells))
      call this%target_grid%pack_into(area_matrix, packed_area)
      deallocate(this%target_grid%cell_area)
      call move_alloc(packed_area, this%target_grid%cell_area)
    end if
  end subroutine nearest_regridder_apply_target_mask

  subroutine nearest_regridder_build_id_map(this)
    class(nearest_regridder_t), intent(inout) :: this

    type(spatial_index_t) :: index
    real(dp), allocatable :: target_points(:, :)

    if (allocated(this%id_map)) deallocate(this%id_map)
    if (this%target_grid%ncells < 1_i8) then
      allocate(this%id_map(0))
      return
    end if
    if (this%source_grid%ncells < 1_i8) then
      call error_message("nearest_regridder % init: source grid has no active cells.") ! LCOV_EXCL_LINE
    end if

    call this%source_grid%build_spatial_index(index, use_aux=this%source_use_aux)
    call this%collect_target_points(target_points)
    call this%query_source_ids(index, target_points, this%id_map)
  end subroutine nearest_regridder_build_id_map

  subroutine nearest_regridder_repack_id_map(this, old_target_cell_ij, nearest_ids)
    class(nearest_regridder_t), intent(inout) :: this
    integer(i4), intent(in) :: old_target_cell_ij(:, :)
    integer(i8), intent(in) :: nearest_ids(:)

    integer(i8) :: k
    integer(i4) :: i, j
    integer(i8), allocatable :: nearest_id_matrix(:, :)

    if (size(old_target_cell_ij, 1, kind=i8) /= size(nearest_ids, kind=i8) .or. size(old_target_cell_ij, 2) /= 2_i4) then
      call error_message("nearest_regridder % init: old target cell ids and nearest ids do not match.") ! LCOV_EXCL_LINE
    end if

    allocate(nearest_id_matrix(this%target_grid%nx, this%target_grid%ny), source=0_i8)
    !$omp parallel do default(shared) private(k,i,j) schedule(static) &
    !$omp& if(size(nearest_ids, kind=i8) >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, size(nearest_ids, kind=i8)
      i = old_target_cell_ij(k, 1)
      j = old_target_cell_ij(k, 2)
      nearest_id_matrix(i, j) = nearest_ids(k)
    end do
    !$omp end parallel do

    if (allocated(this%id_map)) deallocate(this%id_map)
    allocate(this%id_map(this%target_grid%ncells))
    !$omp parallel do default(shared) private(k,i,j) schedule(static) &
    !$omp& if(this%target_grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, this%target_grid%ncells
      i = this%target_grid%cell_ij(k, 1)
      j = this%target_grid%cell_ij(k, 2)
      this%id_map(k) = nearest_id_matrix(i, j)
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_repack_id_map

  subroutine nearest_regridder_collect_cartesian_points(grid, points)
    type(grid_t), intent(in) :: grid
    real(dp), intent(out) :: points(:, :)

    integer(i8) :: k
    integer(i4) :: i, j

    if (size(points, 1, kind=i8) /= grid%ncells .or. size(points, 2) /= 2_i4) then
      call error_message("nearest_regridder: Cartesian point buffer has wrong shape.") ! LCOV_EXCL_LINE
    end if

    !$omp parallel do default(shared) private(k,i,j) schedule(static) if(grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, grid%ncells
      i = grid%cell_ij(k, 1)
      j = grid%cell_ij(k, 2)
      points(k, 1) = grid%x_center(i)
      points(k, 2) = grid%y_center(j)
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_collect_cartesian_points

  subroutine nearest_regridder_collect_lonlat_points(grid, use_aux, points)
    type(grid_t), intent(in) :: grid
    logical, intent(in) :: use_aux
    real(dp), intent(out) :: points(:, :)

    integer(i8) :: k
    integer(i4) :: i, j

    if (size(points, 1, kind=i8) /= grid%ncells .or. size(points, 2) /= 2_i4) then
      call error_message("nearest_regridder: lon/lat point buffer has wrong shape.") ! LCOV_EXCL_LINE
    end if

    if (use_aux .and. .not. grid%has_aux_coords()) then
      call error_message("nearest_regridder: grid has no auxiliary lon/lat coordinates.") ! LCOV_EXCL_LINE
    end if
    if (.not. use_aux .and. grid%coordsys /= spherical) then
      call error_message("nearest_regridder: regular lon/lat points need a spherical grid.") ! LCOV_EXCL_LINE
    end if

    !$omp parallel do default(shared) private(k,i,j) schedule(static) if(grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, grid%ncells
      i = grid%cell_ij(k, 1)
      j = grid%cell_ij(k, 2)
      if (use_aux) then
        points(k, 1) = grid%lon(i, j)
        points(k, 2) = grid%lat(i, j)
      else
        points(k, 1) = grid%x_center(i)
        points(k, 2) = grid%y_center(j)
      end if
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_collect_lonlat_points

  subroutine nearest_regridder_check_ready(this)
    class(nearest_regridder_t), intent(in) :: this

    if (.not. associated(this%source_grid) .or. .not. associated(this%target_grid)) then
      call error_message("nearest_regridder % execute: regridder is not initialized.") ! LCOV_EXCL_LINE
    end if
    if (.not. allocated(this%id_map)) then
      call error_message("nearest_regridder % execute: regridder is not initialized.") ! LCOV_EXCL_LINE
    end if
    if (size(this%id_map, kind=i8) /= this%target_grid%ncells) then
      call error_message("nearest_regridder % execute: target mapping size does not match target grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine nearest_regridder_check_ready

  subroutine nearest_regridder_check_packed_source(this, nvals)
    class(nearest_regridder_t), intent(in) :: this
    integer(i8), intent(in) :: nvals

    if (nvals /= this%source_grid%ncells) then
      call error_message("nearest_regridder % execute: packed source size does not match source grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine nearest_regridder_check_packed_source

  subroutine nearest_regridder_check_packed_target(this, nvals)
    class(nearest_regridder_t), intent(in) :: this
    integer(i8), intent(in) :: nvals

    if (nvals /= this%target_grid%ncells) then
      call error_message("nearest_regridder % execute: packed target size does not match target grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine nearest_regridder_check_packed_target

  subroutine nearest_regridder_check_unpacked_source(this, nx, ny)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny

    if (nx /= this%source_grid%nx .or. ny /= this%source_grid%ny) then
      call error_message("nearest_regridder % execute: source matrix shape does not match source grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine nearest_regridder_check_unpacked_source

  subroutine nearest_regridder_check_unpacked_target(this, nx, ny)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny

    if (nx /= this%target_grid%nx .or. ny /= this%target_grid%ny) then
      call error_message("nearest_regridder % execute: target matrix shape does not match target grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine nearest_regridder_check_unpacked_target

  subroutine nearest_regridder_map_dp(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    real(dp), intent(in) :: in_data(:)
    real(dp), intent(out) :: out_data(:)

    integer(i8) :: k

    !$omp parallel do default(shared) private(k) schedule(static) &
    !$omp& if(this%target_grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, this%target_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_map_dp

  subroutine nearest_regridder_map_i4(this, in_data, out_data)
    class(nearest_regridder_t), intent(in) :: this
    integer(i4), intent(in) :: in_data(:)
    integer(i4), intent(out) :: out_data(:)

    integer(i8) :: k

    !$omp parallel do default(shared) private(k) schedule(static) &
    !$omp& if(this%target_grid%ncells >= nearest_regridder_loop_parallel_min_n)
    do k = 1_i8, this%target_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine nearest_regridder_map_i4

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

  subroutine scaler_check_packed_source(this, nvals)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: nvals
    if (nvals /= this%source_grid%ncells) then
      call error_message("scaler % execute: packed source size does not match source grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_packed_source

  subroutine scaler_check_packed_target(this, nvals)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: nvals
    if (nvals /= this%target_grid%ncells) then
      call error_message("scaler % execute: packed target size does not match target grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_packed_target

  subroutine scaler_check_unpacked_source(this, nx, ny)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny
    if (nx /= this%source_grid%nx .or. ny /= this%source_grid%ny) then
      call error_message("scaler % execute: source matrix shape does not match source grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_unpacked_source

  subroutine scaler_check_unpacked_target(this, nx, ny)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny
    if (nx /= this%target_grid%nx .or. ny /= this%target_grid%ny) then
      call error_message("scaler % execute: target matrix shape does not match target grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_unpacked_target

  subroutine scaler_check_packed_fine(this, nvals)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: nvals
    if (nvals /= this%fine_grid%ncells) then
      call error_message("scaler: packed fine-grid size does not match fine grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_packed_fine

  subroutine scaler_check_packed_coarse(this, nvals)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: nvals
    if (nvals /= this%coarse_grid%ncells) then
      call error_message("scaler: packed coarse-grid size does not match coarse grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_packed_coarse

  subroutine scaler_check_unpacked_fine(this, nx, ny)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny
    if (nx /= this%fine_grid%nx .or. ny /= this%fine_grid%ny) then
      call error_message("scaler: fine-grid matrix shape does not match fine grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_unpacked_fine

  subroutine scaler_check_unpacked_coarse(this, nx, ny)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: nx
    integer(i4), intent(in) :: ny
    if (nx /= this%coarse_grid%nx .or. ny /= this%coarse_grid%ny) then
      call error_message("scaler: coarse-grid matrix shape does not match coarse grid.") ! LCOV_EXCL_LINE
    end if
  end subroutine scaler_check_unpacked_coarse

  !> \brief Execute scaler for packed real input and output.
  subroutine scaler_exe_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:) !< input data on source grid
    real(dp), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), allocatable :: temp_in(:, :)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_packed_target(size(out_data, kind=i8))
    allocate(temp_in(this%source_grid%nx, this%source_grid%ny))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_dp_2d_1d(temp_in, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_dp_1d_1d

  !> \brief Execute scaler for packed real input and unpacked real output.
  subroutine scaler_exe_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:) !< input data on source grid
    real(dp), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), allocatable :: temp_out(:), temp_in(:, :)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_out(this%target_grid%ncells), temp_in(this%source_grid%nx, this%source_grid%ny))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_dp_2d_1d(temp_in, temp_out, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine scaler_exe_dp_1d_2d

  !> \brief Execute scaler for unpacked real input and output.
  subroutine scaler_exe_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :) !< input data on source grid
    real(dp), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), allocatable :: temp(:)
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp(this%target_grid%ncells))
    call this%scaler_exe_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp, out_data)
  end subroutine scaler_exe_dp_2d_2d

  !> \brief Execute scaler for unpacked real input and packed real output.
  subroutine scaler_exe_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :) !< input data on source grid
    real(dp), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        call this%target_grid%pack_into(in_data, out_data)
      else
        call this%target_grid%pack_into(flipped(in_data, idim=2), out_data)
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
        case (up_median)
          call this%upscale_median(in_data, out_data)
        case (up_laf)
          call error_message("scaler: largest area fraction upscaling operator not supported for real data.") ! LCOV_EXCL_LINE
        case (up_fraction)
          call error_message("scaler: class fraction upscaling operator not supported for real input data.") ! LCOV_EXCL_LINE
        case default
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator))) ! LCOV_EXCL_LINE
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call this%downscale_split(in_data, out_data)
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator))) ! LCOV_EXCL_LINE
      end select
    end if

  end subroutine scaler_exe_dp_2d_1d

  !> \brief Execute scaler for packed integer input and output.
  subroutine scaler_exe_i4_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< input data on source grid
    integer(i4), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), allocatable :: temp_in(:, :)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_packed_target(size(out_data, kind=i8))
    allocate(temp_in(this%source_grid%nx, this%source_grid%ny))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_i4_2d_1d(temp_in, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_i4_1d_1d

  !> \brief Execute scaler for packed integer input and unpacked integer output.
  subroutine scaler_exe_i4_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< input data on source grid
    integer(i4), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), allocatable :: temp_out(:), temp_in(:, :)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_out(this%target_grid%ncells), temp_in(this%source_grid%nx, this%source_grid%ny))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_i4_2d_1d(temp_in, temp_out, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine scaler_exe_i4_1d_2d

  !> \brief Execute scaler for unpacked integer input output.
  subroutine scaler_exe_i4_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< input data on source grid
    integer(i4), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), allocatable :: temp(:)
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp(this%target_grid%ncells))
    call this%scaler_exe_i4_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp, out_data)
  end subroutine scaler_exe_i4_2d_2d

  !> \brief Execute scaler for unpacked integer input and packed integer output.
  subroutine scaler_exe_i4_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< input data on source grid
    integer(i4), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator

    integer(i4) :: up_operator, down_operator
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        call this%target_grid%pack_into(in_data, out_data)
      else
        call this%target_grid%pack_into(flipped(in_data, idim=2), out_data)
      end if
    else if (this%scaling_mode == up_scaling) then
      select case (up_operator)
        case (up_p_mean)
          call error_message("scaler: p-mean upscaling operator not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_a_mean)
          call error_message("scaler: arithmetic mean upscaling operator not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_g_mean)
          call error_message("scaler: geometric mean upscaling operator not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_h_mean)
          call error_message("scaler: harmonic mean upscaling operator not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_min)
          call this%upscale_min(in_data, out_data)
        case (up_max)
          call this%upscale_max(in_data, out_data)
        case (up_sum)
          call this%upscale_sum(in_data, out_data)
        case (up_var)
          call error_message("scaler: variance upscaling operator not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_std)
          call error_message("scaler: standard deviation upscaling not supported for integer data.") ! LCOV_EXCL_LINE
        case (up_median)
          call error_message("scaler: median upscaling operator not supported for integer output data.") ! LCOV_EXCL_LINE
        case (up_laf)
          call this%upscale_laf(in_data, out_data, vmin, vmax)
        case (up_fraction)
          call error_message("scaler: class fraction upscaling operator not supported for integer output data.") ! LCOV_EXCL_LINE
        case default
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator))) ! LCOV_EXCL_LINE
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(in_data, out_data)
        case (down_split)
          call error_message("scaler: equal summand downscaling operator not supported for integer output data.") ! LCOV_EXCL_LINE
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator))) ! LCOV_EXCL_LINE
      end select
    end if
  end subroutine scaler_exe_i4_2d_1d

  !> \brief Execute scaler for packed integer input and packed real output.
  subroutine scaler_exe_i4_dp_1d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< input data on source grid
    real(dp), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), allocatable :: temp_in(:, :)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_packed_target(size(out_data, kind=i8))
    allocate(temp_in(this%source_grid%nx, this%source_grid%ny))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_i4_dp_2d_1d(temp_in, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
  end subroutine scaler_exe_i4_dp_1d_1d

  !> \brief Execute scaler for packed integer input and unpacked real output.
  subroutine scaler_exe_i4_dp_1d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< input data on source grid
    real(dp), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    integer(i4), allocatable :: temp_in(:, :)
    real(dp), allocatable :: temp_out(:)
    call this%check_packed_source(size(in_data, kind=i8))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp_in(this%source_grid%nx, this%source_grid%ny), temp_out(this%target_grid%ncells))
    call this%source_grid%unpack_into(in_data, temp_in)
    call this%scaler_exe_i4_dp_2d_1d(temp_in, temp_out, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp_out, out_data)
  end subroutine scaler_exe_i4_dp_1d_2d

  !> \brief Execute scaler for unpacked integer input and unpacked real output.
  subroutine scaler_exe_i4_dp_2d_2d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< input data on source grid
    real(dp), intent(out) :: out_data(:, :) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator
    real(dp), allocatable :: temp(:)
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_unpacked_target(size(out_data, 1), size(out_data, 2))
    allocate(temp(this%target_grid%ncells))
    call this%scaler_exe_i4_dp_2d_1d(in_data, temp, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    call this%target_grid%unpack_into(temp, out_data)
  end subroutine scaler_exe_i4_dp_2d_2d

  !> \brief Execute scaler for unpacked integer input and packed real output.
  subroutine scaler_exe_i4_dp_2d_1d(this, in_data, out_data, upscaling_operator, downscaling_operator, p, class_id, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< input data on source grid
    real(dp), intent(out) :: out_data(:) !< output data on target grid
    integer(i4), intent(in), optional :: upscaling_operator !< upscaling operator (up_a_mean by default)
    integer(i4), intent(in), optional :: downscaling_operator !< downscaling operator (down_nearest by default)
    real(dp), intent(in), optional :: p !< exponent for the up_p_mean operator
    integer(i4), intent(in), optional :: class_id !< class id for up_fraction operator
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up up_laf operator (huge() by default - tbd)
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up up_laf operator (-huge() by default - tbd)

    integer(i4), dimension(:), allocatable :: temp

    integer(i4) :: up_operator, down_operator
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%operator_init(up_operator, down_operator, upscaling_operator, downscaling_operator)
    ! shortcut if resolution is equal (only converting, masking and flipping)
    if (this%scaling_mode == no_scaling) then
      if (this%y_dir_match) then
        call this%target_grid%pack_into(real(in_data, dp), out_data)
      else
        call this%target_grid%pack_into(flipped(real(in_data, dp), idim=2), out_data)
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
        case (up_median)
          call this%upscale_median(real(in_data, dp), out_data)
        case (up_laf)
          allocate(temp(this%coarse_grid%ncells))
          call this%upscale_laf(in_data, temp, vmin, vmax)
          out_data = real(temp, dp)
          deallocate(temp)
        case (up_fraction)
          call this%upscale_fraction(in_data, out_data, class_id)
        case default
          call error_message("scaler: unknown upscaling operator: ", trim(num2str(up_operator))) ! LCOV_EXCL_LINE
      end select
    else ! down-scaling
      select case (down_operator)
        case (down_nearest)
          call this%downscale_nearest(real(in_data, dp), out_data)
        case (down_split)
          call this%downscale_split(real(in_data, dp), out_data)
        case default
          call error_message("scaler: unknown downscaling operator: ", trim(num2str(down_operator))) ! LCOV_EXCL_LINE
      end select
    end if
  end subroutine scaler_exe_i4_dp_2d_1d

  subroutine scaler_upscale_p_mean(this, in_data, out_data, p)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    real(dp), intent(in), optional :: p !< exponent for the p-norm (1.0 for arithmetic mean by default)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    real(dp) :: p_
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    p_ = 1.0_dp
    if (present(p)) p_ = p
    if (eq(p_, 0.0_dp)) then
      call this%upscale_g_mean(in_data, out_data)
    else if (eq(p_, 1.0_dp)) then
      call this%upscale_a_mean(in_data, out_data)
    else if (eq(p_, -1.0_dp)) then
      call this%upscale_h_mean(in_data, out_data)
    else if (.not.ieee_is_finite(p_)) then
      if (ieee_is_nan(p_)) call error_message("upscale_p_mean: p is not a number.") ! LCOV_EXCL_LINE
      if (ieee_is_negative(p_)) then
        call this%upscale_min(in_data, out_data)
      else
        call this%upscale_max(in_data, out_data)
      end if
    else
      call check_upscaling(this%scaling_mode)
      if (this%weight_mode == weight_area) then
        !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
        do k = 1_i8, this%coarse_grid%ncells
          call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
          out_data(k) = sum( &
              pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) ** p_ &
            * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) &
          ) ** (1.0_dp / p_)
        end do
        !$omp end parallel do
      else
        !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
        do k = 1_i8, this%coarse_grid%ncells
          call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
          out_data(k) = sum( &
            pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) ** p_ * this%coarse_weights(k) &
          ) ** (1.0_dp / p_)
        end do
        !$omp end parallel do
      end if
    end if
  end subroutine scaler_upscale_p_mean

  subroutine scaler_upscale_a_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    if (this%weight_mode == weight_area) then
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = sum( &
            pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) &
          * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)))
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = sum(pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) * this%coarse_weights(k))
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_a_mean

  subroutine scaler_upscale_g_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    if (this%weight_mode == weight_area) then
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = exp(sum(log( & ! prod(xi^wi) = exp(sum(wi*log(xi)))
            pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))) &
          * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))))
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = exp(sum(log( & ! prod(xi^wi) = exp(sum(wi*log(xi)))
            pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))) * this%coarse_weights(k)))
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_g_mean

  subroutine scaler_upscale_h_mean(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    if (this%weight_mode == weight_area) then
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = 1.0_dp / sum( &
            1.0_dp / pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) &
          * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)))
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = 1.0_dp / sum( &
          1.0_dp / pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) * this%coarse_weights(k))
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_h_mean

  subroutine scaler_upscale_min_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = minval(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_min_dp

  subroutine scaler_upscale_min_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = minval(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_min_i4

  subroutine scaler_upscale_max_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = maxval(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_max_dp

  subroutine scaler_upscale_max_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = maxval(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_max_i4

  subroutine scaler_upscale_sum_dp(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = sum(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_sum_dp

  subroutine scaler_upscale_sum_i4(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      out_data(k) = sum(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_sum_i4

  subroutine scaler_upscale_var(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    real(dp) :: mean
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    if (this%weight_mode == weight_area) then
      !$omp parallel do default(shared) private(mean,x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        mean = sum(  pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) &
                  * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)))
        out_data(k) = sum( &
          ( pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) - mean ) ** 2_i4 &
          * pack(this%weights(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)))
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(mean,x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        mean = sum(pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) * this%coarse_weights(k))
        out_data(k) = sum( &
          ( pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) - mean ) ** 2_i4 * this%coarse_weights(k))
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_var

  subroutine scaler_upscale_std(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call this%upscale_var(in_data, out_data)
    out_data = sqrt(out_data)
  end subroutine scaler_upscale_std

  subroutine scaler_upscale_median(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub, n
    real(dp) :: vals(this%factor * this%factor)
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub,vals,n) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      n = this%n_subcells(k)
      ! need vals temporary array to prevent intel bug with openmp and pack+omedian (segmentation fault or wrong results)
      vals(1_i4:n) = pack(in_data(x_lb:x_ub,y_lb:y_ub), this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
      out_data(k) = omedian(vals(1_i4:n))
    end do
    !$omp end parallel do
  end subroutine scaler_upscale_median


  subroutine scaler_upscale_laf(this, in_data, out_data, vmin, vmax)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)
    integer(i4), intent(in), optional :: vmin !< minimum of values to speed up operator
    integer(i4), intent(in), optional :: vmax !< maximum of values to speed up operator
    integer(i4) :: i, laf_v, cnt_v, cnt_i, min_v, max_v
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
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
    if (min_v > max_v) call error_message("upscale_laf: vmin is bigger than vmax.") ! LCOV_EXCL_LINE

    !$omp parallel do default(shared) private(i,laf_v,cnt_v,cnt_i,x_lb,x_ub,y_lb,y_ub) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      laf_v = min_v
      cnt_v = 0
      do i = min_v, max_v
        ! nodata value should be out of range (-9999) so we don't need to pack data
        cnt_i = count((in_data(x_lb:x_ub,y_lb:y_ub) == i) .and. this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
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
    integer(i4), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i4), intent(in), optional :: class_id !< class id to determine area fraction of
    integer(i4) :: cls_id
    integer(i8) :: k
    integer(i4) :: x_lb, x_ub, y_lb, y_ub
    call this%check_unpacked_source(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(out_data, kind=i8))
    call check_upscaling(this%scaling_mode)
    cls_id = optval(class_id, default=nodata_i4)
    if (this%weight_mode == weight_area) then
      !$omp parallel do default(shared) private(k,x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = sum(this%weights(x_lb:x_ub,y_lb:y_ub), &
                          mask=(in_data(x_lb:x_ub,y_lb:y_ub)==cls_id).and.this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub))
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(shared) private(k,x_lb,x_ub,y_lb,y_ub) schedule(static)
      do k = 1_i8, this%coarse_grid%ncells
        call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
        out_data(k) = count((in_data(x_lb:x_ub,y_lb:y_ub)==cls_id).and.this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub)) &
                      * this%coarse_weights(k)
      end do
      !$omp end parallel do
    end if
  end subroutine scaler_upscale_fraction

  subroutine scaler_downscale_nearest_dp_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    call this%check_packed_coarse(size(in_data, kind=i8))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_dp_1d

  subroutine scaler_downscale_nearest_i4_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:)
    integer(i4), intent(out) :: out_data(:)
    integer(i8) :: k
    call this%check_packed_coarse(size(in_data, kind=i8))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_i4_1d

  subroutine scaler_downscale_split_1d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:)
    real(dp), intent(out) :: out_data(:)
    integer(i8) :: k
    call this%check_packed_coarse(size(in_data, kind=i8))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      out_data(k) = in_data(this%id_map(k)) * this%coarse_weights(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_split_1d

  subroutine scaler_downscale_nearest_dp_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i4) :: i, j
    integer(i8) :: k
    call this%check_unpacked_coarse(size(in_data, 1), size(in_data, 2))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_dp_2d

  subroutine scaler_downscale_nearest_i4_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :)
    integer(i4), intent(out) :: out_data(:)
    integer(i4) :: i, j
    integer(i8) :: k
    call this%check_unpacked_coarse(size(in_data, 1), size(in_data, 2))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j)
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_nearest_i4_2d

  subroutine scaler_downscale_split_2d(this, in_data, out_data)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :)
    real(dp), intent(out) :: out_data(:)
    integer(i4) :: i, j
    integer(i8) :: k
    call this%check_unpacked_coarse(size(in_data, 1), size(in_data, 2))
    call this%check_packed_fine(size(out_data, kind=i8))
    call check_downscaling(this%scaling_mode)
    !$omp parallel do default(shared) private(i,j) schedule(static)
    do k = 1_i8, this%fine_grid%ncells
      i = this%coarse_grid%cell_ij(this%id_map(k), 1)
      j = this%coarse_grid%cell_ij(this%id_map(k), 2)
      out_data(k) = in_data(i,j) * this%coarse_weights(this%id_map(k))
    end do
    !$omp end parallel do
  end subroutine scaler_downscale_split_2d

  !> \brief Find location of maximum value in coarse cell on fine grid by cell id.
  subroutine scaler_maxloc_dp_2d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of max values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4) :: x_lb, x_ub, y_lb, y_ub, loc(2), ix, iy
    integer(i8) :: k
    integer(i8), allocatable :: cells(:,:)
    logical :: back_
    call this%check_unpacked_fine(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(ids, kind=i8))
    back_ = optval(back, default=.false.)
    allocate(cells(this%fine_grid%nx,this%fine_grid%ny))
    call this%fine_grid%gen_id_matrix(cells)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub,loc,ix,iy) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      loc = maxloc(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub), back=back_)
      ix = x_lb + loc(1) - 1_i4
      iy = y_lb + loc(2) - 1_i4
      ids(k) = cells(ix,iy)
    end do
    !$omp end parallel do
    deallocate(cells)
  end subroutine scaler_maxloc_dp_2d

  !> \brief Find location of maximum value in coarse cell on fine grid by cell id.
  subroutine scaler_maxloc_dp_1d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of max values
    logical, intent(in), optional :: back !< start search from back (default: false)
    real(dp), allocatable :: temp_in(:, :)
    call this%check_packed_fine(size(in_data, kind=i8))
    call this%check_packed_target(size(ids, kind=i8))
    allocate(temp_in(this%fine_grid%nx, this%fine_grid%ny))
    call this%fine_grid%unpack_into(in_data, temp_in)
    call this%scaler_maxloc_dp_2d(temp_in, ids, back)
  end subroutine scaler_maxloc_dp_1d

  !> \brief Find location of maximum value in coarse cell on fine grid by cell id.
  subroutine scaler_maxloc_i4_2d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of max values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4) :: x_lb, x_ub, y_lb, y_ub, loc(2), ix, iy
    integer(i8) :: k
    integer(i8), allocatable :: cells(:,:)
    logical :: back_
    call this%check_unpacked_fine(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(ids, kind=i8))
    back_ = optval(back, default=.false.)
    allocate(cells(this%fine_grid%nx,this%fine_grid%ny))
    call this%fine_grid%gen_id_matrix(cells)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub,loc,ix,iy) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      loc = maxloc(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub), back=back_)
      ix = x_lb + loc(1) - 1_i4
      iy = y_lb + loc(2) - 1_i4
      ids(k) = cells(ix,iy)
    end do
    !$omp end parallel do
    deallocate(cells)
  end subroutine scaler_maxloc_i4_2d

  !> \brief Find location of maximum value in coarse cell on fine grid by cell id.
  subroutine scaler_maxloc_i4_1d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of max values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4), allocatable :: temp_in(:, :)
    call this%check_packed_fine(size(in_data, kind=i8))
    call this%check_packed_target(size(ids, kind=i8))
    allocate(temp_in(this%fine_grid%nx, this%fine_grid%ny))
    call this%fine_grid%unpack_into(in_data, temp_in)
    call this%scaler_maxloc_i4_2d(temp_in, ids, back)
  end subroutine scaler_maxloc_i4_1d

  !> \brief Find location of minimum value in coarse cell on fine grid by cell id.
  subroutine scaler_minloc_dp_2d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:, :) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of min values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4) :: x_lb, x_ub, y_lb, y_ub, loc(2), ix, iy
    integer(i8) :: k
    integer(i8), allocatable :: cells(:,:)
    logical :: back_
    call this%check_unpacked_fine(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(ids, kind=i8))
    back_ = optval(back, default=.false.)
    allocate(cells(this%fine_grid%nx,this%fine_grid%ny))
    call this%fine_grid%gen_id_matrix(cells)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub,loc,ix,iy) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      loc = minloc(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub), back=back_)
      ix = x_lb + loc(1) - 1_i4
      iy = y_lb + loc(2) - 1_i4
      ids(k) = cells(ix,iy)
    end do
    !$omp end parallel do
    deallocate(cells)
  end subroutine scaler_minloc_dp_2d

  !> \brief Find location of minimum value in coarse cell on fine grid by cell id.
  subroutine scaler_minloc_dp_1d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    real(dp), intent(in) :: in_data(:) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of min values
    logical, intent(in), optional :: back !< start search from back (default: false)
    real(dp), allocatable :: temp_in(:, :)
    call this%check_packed_fine(size(in_data, kind=i8))
    call this%check_packed_target(size(ids, kind=i8))
    allocate(temp_in(this%fine_grid%nx, this%fine_grid%ny))
    call this%fine_grid%unpack_into(in_data, temp_in)
    call this%scaler_minloc_dp_2d(temp_in, ids, back)
  end subroutine scaler_minloc_dp_1d

  !> \brief Find location of minimum value in coarse cell on fine grid by cell id.
  subroutine scaler_minloc_i4_2d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:, :) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of min values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4) :: x_lb, x_ub, y_lb, y_ub, loc(2), ix, iy
    integer(i8) :: k
    integer(i8), allocatable :: cells(:,:)
    logical :: back_
    call this%check_unpacked_fine(size(in_data, 1), size(in_data, 2))
    call this%check_packed_target(size(ids, kind=i8))
    back_ = optval(back, default=.false.)
    allocate(cells(this%fine_grid%nx,this%fine_grid%ny))
    call this%fine_grid%gen_id_matrix(cells)
    !$omp parallel do default(shared) private(x_lb,x_ub,y_lb,y_ub,loc,ix,iy) schedule(static)
    do k = 1_i8, this%coarse_grid%ncells
      call this%coarse_bounds(k, x_lb, x_ub, y_lb, y_ub)
      loc = minloc(in_data(x_lb:x_ub,y_lb:y_ub), mask=this%fine_grid%mask(x_lb:x_ub,y_lb:y_ub), back=back_)
      ix = x_lb + loc(1) - 1_i4
      iy = y_lb + loc(2) - 1_i4
      ids(k) = cells(ix,iy)
    end do
    !$omp end parallel do
    deallocate(cells)
  end subroutine scaler_minloc_i4_2d

  !> \brief Find location of minimum value in coarse cell on fine grid by cell id.
  subroutine scaler_minloc_i4_1d(this, in_data, ids, back)
    class(scaler_t), intent(inout) :: this
    integer(i4), intent(in) :: in_data(:) !< fine grid input data
    integer(i8), intent(out) :: ids(:) !< fine grid cell ids of min values
    logical, intent(in), optional :: back !< start search from back (default: false)
    integer(i4), allocatable :: temp_in(:, :)
    call this%check_packed_fine(size(in_data, kind=i8))
    call this%check_packed_target(size(ids, kind=i8))
    allocate(temp_in(this%fine_grid%nx, this%fine_grid%ny))
    call this%fine_grid%unpack_into(in_data, temp_in)
    call this%scaler_minloc_i4_2d(temp_in, ids, back)
  end subroutine scaler_minloc_i4_1d

  !> \brief Get coarse grid indices from fine grid indices.
  pure subroutine scaler_coarse_ij(this, fine_i, fine_j, coarse_i, coarse_j)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: fine_i !< i index on fine grid
    integer(i4), intent(in) :: fine_j !< j index on fine grid
    integer(i4), intent(out) :: coarse_i !< i index on coarse grid
    integer(i4), intent(out) :: coarse_j !< j index on coarse grid
    call coarse_ij(this%factor, fine_i, fine_j, &
      this%fine_grid%y_direction, this%fine_grid%ny, &
      this%coarse_grid%y_direction, this%coarse_grid%ny, &
      coarse_i, coarse_j)
  end subroutine scaler_coarse_ij

  !> \brief Get coarse grid indices from fine grid cell index.
  pure subroutine scaler_coarse_ij_cell(this, fine_cell, coarse_i, coarse_j)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: fine_cell !< cell index on fine grid
    integer(i4), intent(out) :: coarse_i !< i index on coarse grid
    integer(i4), intent(out) :: coarse_j !< j index on coarse grid
    call coarse_ij(this%factor, this%fine_grid%cell_ij(fine_cell, 1), this%fine_grid%cell_ij(fine_cell, 2), &
      this%fine_grid%y_direction, this%fine_grid%ny, &
      this%coarse_grid%y_direction, this%coarse_grid%ny, &
      coarse_i, coarse_j)
  end subroutine scaler_coarse_ij_cell

  !> \brief Get fine cell index bounds from coarse grid indices.
  pure subroutine scaler_coarse_bounds(this, coarse_i, coarse_j, x_lb, x_ub, y_lb, y_ub)
    class(scaler_t), intent(in) :: this
    integer(i4), intent(in) :: coarse_i !< i index on coarse grid
    integer(i4), intent(in) :: coarse_j !< j index on coarse grid
    integer(i4), intent(out) :: x_lb !< lower bound in x on fine grid
    integer(i4), intent(out) :: x_ub !< upper bound in x on fine grid
    integer(i4), intent(out) :: y_lb !< lower bound in y on fine grid
    integer(i4), intent(out) :: y_ub !< upper bound in y on fine grid
    call id_bounds(this%factor, coarse_i, coarse_j, &
      this%coarse_grid%y_direction, this%coarse_grid%ny, &
      this%fine_grid%y_direction, this%fine_grid%nx, this%fine_grid%ny, &
      x_lb, x_ub, y_lb, y_ub)
  end subroutine scaler_coarse_bounds

  !> \brief Get fine cell index bounds from coarse grid cell index.
  pure subroutine scaler_coarse_bounds_cell(this, coarse_cell, x_lb, x_ub, y_lb, y_ub)
    class(scaler_t), intent(in) :: this
    integer(i8), intent(in) :: coarse_cell !< cell index on coarse grid
    integer(i4), intent(out) :: x_lb !< lower bound in x on fine grid
    integer(i4), intent(out) :: x_ub !< upper bound in x on fine grid
    integer(i4), intent(out) :: y_lb !< lower bound in y on fine grid
    integer(i4), intent(out) :: y_ub !< upper bound in y on fine grid
    if (this%cache_bounds) then
      x_lb = this%x_lb(coarse_cell)
      x_ub = this%x_ub(coarse_cell)
      y_lb = this%y_lb(coarse_cell)
      y_ub = this%y_ub(coarse_cell)
      return
    end if
    call id_bounds(this%factor, this%coarse_grid%cell_ij(coarse_cell,1), this%coarse_grid%cell_ij(coarse_cell,2), &
      this%coarse_grid%y_direction, this%coarse_grid%ny, &
      this%fine_grid%y_direction, this%fine_grid%nx, this%fine_grid%ny, &
      x_lb, x_ub, y_lb, y_ub)
  end subroutine scaler_coarse_bounds_cell

  subroutine check_upscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= up_scaling) call error_message("scaler: not setup for upscaling.") ! LCOV_EXCL_LINE
  end subroutine check_upscaling

  subroutine check_downscaling(scaling_mode)
    integer(i4), intent(in) :: scaling_mode        !< up_scaling or down_scaling
    if (scaling_mode /= down_scaling) call error_message("scaler: not setup for downscaling.") ! LCOV_EXCL_LINE
  end subroutine check_downscaling

end module mo_grid_scaler
