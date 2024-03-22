!> \file    mo_grid.f90
!> \brief   \copybrief mo_grid
!> \details \copydetails mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_kind, only: i4, dp, sp
  use mo_utils, only: flip
  use mo_message, only : error_message
  use mo_string_utils, only : num2str

  implicit none
  private
  ! coordsys selector
  integer(i4), public, parameter :: coordsys_cart = 0_i4 !< Cartesian coordinate system.
  integer(i4), public, parameter :: coordsys_sph_deg = 1_i4 !< Spherical coordinates in degrees.
  ! integer(i4), public, parameter :: coordsys_sph_rad = 2_i4
  !> \class   coordsys_t
  !> \brief   Supported coordniate systems (cart, sph_deg).
  type, private :: coordsys_t
    integer(i4) :: cart = 0_i4 !< Cartesian coordinate system.
    integer(i4) :: sph_deg = 1_i4 !< Spherical coordinates in degrees.
    ! integer(i4), public :: sph_rad = 2_i4
  end type coordsys_t
  type(coordsys_t), public, parameter :: coordsys = coordsys_t(0_i4, 1_i4) !< Supported coordniate systems (cart, sph_deg).
  ! align selector
  integer(i4), public, parameter :: align_ll = 0_i4 !< align in lower left corner
  integer(i4), public, parameter :: align_lr = 1_i4 !< align in lower right corner
  integer(i4), public, parameter :: align_tl = 2_i4 !< align in top left corner
  integer(i4), public, parameter :: align_tr = 3_i4 !< align in top right corner
  !> \class   align_t
  !> \brief   Supported aligning corners (ll, lr, tl, tr).
  type, private :: align_t
    integer(i4) :: ll = 0_i4 !< align in lower left corner
    integer(i4) :: lr = 1_i4 !< align in lower right corner
    integer(i4) :: tl = 2_i4 !< align in top left corner
    integer(i4) :: tr = 3_i4 !< align in top right corner
  end type align_t
  type(align_t), public, parameter :: align = align_t(0_i4, 1_i4, 2_i4, 3_i4) !< Supported aligning corners (ll, lr, tl, tr).

  ! -------------------------------------------------------------------
  ! GRID description
  ! -------------------------------------------------------------------
  !> \class   grid_t
  !> \brief   2D grid description with data in xy order with strictly increasing axis.
  !!
  !!          ASCII grid files have the exact oposite behavior: yx order, with decreasing y-axis.
  !!          NetCDF files nativly have yx order, but since Fortran arrays are column-major order,
  !!          the data read from .nc files is in xy order. If the y axis is decreasing, data arrays
  !!          should be flipped.
  type, public :: grid_t
    integer(i4) :: coordsys = coordsys%cart !< Coordinate system for x and y. 0 -> Cartesian (default), 1 -> Spherical
    ! general domain information
    integer(i4) :: nx        !< size of x-axis (number of cols in ascii grid file)
    integer(i4) :: ny        !< size of y-axis (number of rows in ascii grid file)
    integer(i4) :: n_cells   !< number of cells in mask
    real(dp) :: xllcorner    !< x coordinate of the lowerleft corner
    real(dp) :: yllcorner    !< y coordinate of the lowerleft corner
    real(dp) :: cellsize     !< cellsize x = cellsize y
    real(dp) :: nodata_value !< code to define the mask
    integer(i4), dimension(:), allocatable :: id     !< IDs of cells in mask (1..n_cells)
    real(dp), dimension(:), allocatable :: cell_area !< area of the cell in sqare m, size (n_cells)
    logical, dimension(:, :), allocatable :: mask    !< the mask for valid cells in the original grid, size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat    !< 2d longitude array (auxiliary coordinate for X axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lon    !< 2d latitude  array (auxiliary coordinate for Y axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat_vertices  !< latitude coordinates or the grid nodes, size (nx+1, ny+1)
    real(dp), dimension(:, :), allocatable :: lon_vertices  !< longitude coordinates or the grid nodes, size (nx+1, ny+1)
    integer(i4), dimension(:, :), allocatable :: cell_ij    !< matrix IDs (i, j) per cell in mask, size (n_cells, 2)
  contains
  !   !> \copydoc mo_grid::from_header_info
  !   procedure, public :: from_header_info !< \see mo_grid::from_header_info
  !   !> \copydoc mo_grid::from_file
  !   procedure, public :: from_file !< \see mo_grid::from_file
  !   !> \copydoc mo_grid::extend
  !   procedure, public :: extend !< \see mo_grid::extend
  !   !> \copydoc mo_grid::x_axis
  !   procedure, public :: x_axis !< \see mo_grid::x_axis
  !   !> \copydoc mo_grid::y_axis
  !   procedure, public :: y_axis !< \see mo_grid::y_axis
  !   !> \copydoc mo_grid::x_vertices
  !   procedure, public :: x_vertices !< \see mo_grid::x_vertices
  !   !> \copydoc mo_grid::y_vertices
  !   procedure, public :: y_vertices !< \see mo_grid::y_vertices
  !   !> \copydoc mo_grid::x_bounds
  !   procedure, public :: x_bounds !< \see mo_grid::x_bounds
  !   !> \copydoc mo_grid::y_bounds
  !   procedure, public :: y_bounds !< \see mo_grid::y_bounds
  !   !> \copydoc mo_grid::estimate_aux_coords
  !   procedure, public :: estimate_aux_coords !< \see mo_grid::estimate_aux_coords
  !   !> \copydoc mo_grid::estimate_aux_vertices
  !   procedure, public :: estimate_aux_vertices !< \see mo_grid::estimate_aux_vertices
  !   !> \copydoc mo_grid::derive_level
  !   procedure, public :: derive_level !< \see mo_grid::derive_level
  !   !> \copydoc mo_grid::is_covered_by
  !   procedure, public :: is_covered_by !< \see mo_grid::is_covered_by
    !> \copydoc mo_grid::calculate_cell_ids
    procedure, public :: calculate_cell_ids !< \see mo_grid::calculate_cell_ids
    !> \copydoc mo_grid::estimate_cell_area
    procedure, public :: estimate_cell_area !< \see mo_grid::estimate_cell_area
  !   !> \copydoc mo_grid::read_data
  !   procedure, public :: read_data !< \see mo_grid::read_data
  !   !> \copydoc mo_grid::pack_data
  !   procedure, public :: pack_data !< \see mo_grid::pack_data
  !   !> \copydoc mo_grid::unpack_data
  !   procedure, public :: unpack_data !< \see mo_grid::unpack_data
  end type grid_t

  !> \class   upscaler_t
  !> \brief   Upscaler type to remap data on regular grids with an integer cellsize ratio.
  type, public :: upscaler_t
    type(grid_t), pointer :: fine_grid !< high resolution grid
    type(grid_t), pointer :: coarse_grid !< low resolution grid
    integer(i4), dimension(:), allocatable :: y_lb              !< lower bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: y_ub              !< upper bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_lb              !< lower bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_ub              !< upper bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: n_subcells        !< valid fine grid cells in coarse cell (coarse%n_cells)
    integer(i4), dimension(:, :), allocatable :: coarse_id_map  !< 2d index array of coarse ids (fine%nx, fine%ny)
  contains
    !> \copydoc mo_grid::from_target_resolution
    procedure, public :: from_target_resolution !< \see mo_grid::from_target_resolution
  !   !> \copydoc mo_grid::from_grids
  !   procedure, public :: from_grids !< \see mo_grid::from_grids
  end type upscaler_t

  !>       \brief Reads spatial data files of ASCII format.
  !>       \details Reads spatial input data, e.g. dem, aspect, flow direction.
  !>       \authors Juliane Mai
  !>       \date Jan 2013
  !>       \changelog
  !!       - Matthias Zink, Feb 2013
  !!         - added interface and routine for datatype i4
  !!       - David Schaefer, Mar 2015
  !!         - removed double allocation of temporary data
  !!       - Robert Schweppe, Jun 2018
  !!         - refactoring and reformatting
  !!       - Sebastian Müller, Mar 2024
  !!         - moving to FORCES
  !!         - remove fileunit input (use newunit)
  !!         - make mask optional output
  !!         - add flip_y argument
  interface  read_spatial_data_ascii
    module procedure read_spatial_data_ascii_i4, read_spatial_data_ascii_dp
  end interface read_spatial_data_ascii

contains

  !>       \brief Setup upscaler and coarse grid from given target resolution
  !>       \details following attributes are calculated for the coarse grid:
  !!       -  cell id & numbering
  !!       -  mask creation
  !!       -  storage of cell cordinates (row and coloum indizes)
  !!       -  sorage of four sub-cell corner cordinates
  !!
  !!       fine_grid and coarse_grid need to be targets, since the upscaler will only
  !!       hold pointers to the associated grids.
  !>       \authors Rohini Kumar
  !>       \date Jan 2013
  !>       \changelog
  !!       - Sebastian Müller, Mar 2024
  !!         - moving to FORCES
  !!         - is now a method of the upscaler type
  subroutine from_target_resolution(this, target_resolution, fine_grid, coarse_grid, estimate_aux)

    use mo_constants, only : nodata_dp, nodata_i4

    implicit none

    class(upscaler_t), intent(inout) :: this !< remapper type for given grids
    real(dp), intent(in) :: target_resolution !< desired target resolution
    type(grid_t), target, intent(in) :: fine_grid !< given high resolution grid
    type(grid_t), target, intent(inout) :: coarse_grid !< resulting low resolution grid
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)

    real(dp), dimension(:, :), allocatable :: areaCell0_2D
    real(dp) :: cellFactor
    integer(i4) :: cellFactor_i
    integer(i4) :: i_ub, i_lb, j_lb, j_ub
    integer(i4) :: i, j, k, ic, jc
    logical :: estimate_aux_

    estimate_aux_ = .true.
    if (present(estimate_aux)) estimate_aux_ = estimate_aux

    !--------------------------------------------------------
    ! 1) Estimate each variable locally for a given domain
    ! 2) Pad each variable to its corresponding global one
    !--------------------------------------------------------
    ! grid properties
    call calculate_coarse_extend( &
      nx_in=fine_grid%nx, &
      ny_in=fine_grid%ny, &
      xllcorner_in=fine_grid%xllcorner, &
      yllcorner_in=fine_grid%yllcorner, &
      cellsize_in=fine_grid%cellsize, &
      target_resolution=target_resolution, &
      nx_out=coarse_grid%nx, &
      ny_out=coarse_grid%ny, &
      xllcorner_out=coarse_grid%xllcorner, &
      yllcorner_out=coarse_grid%yllcorner, &
      cellsize_out=coarse_grid%cellsize &
    )

    cellFactor = anint(coarse_grid%cellsize / fine_grid%cellsize, dp)
    cellFactor_i = nint(cellFactor, i4)

    ! only re-calculate mask if not given externally
    if (.not. allocated(coarse_grid%mask)) then
      ! allocation and initalization of mask at coarse grid
      allocate(coarse_grid%mask(coarse_grid%nx, coarse_grid%ny))
      coarse_grid%mask(:, :) = .false.

      ! create mask at coarse grid
      do j = 1_i4, fine_grid%ny
        ! everything would be better with 0-based ids
        jc = (j-1_i4) / cellfactor_i + 1_i4
        do i = 1, fine_grid%nx
          if (.not. fine_grid%mask(i, j)) cycle
          ic = (i-1_i4) / cellfactor_i + 1_i4
          coarse_grid%mask(ic, jc) = .true.
        end do
      end do

      ! estimate n_cells and initalize related variables
      coarse_grid%n_cells = count(coarse_grid%mask)
      ! allocate and initalize cell1 related variables
      allocate(coarse_grid%id(coarse_grid%n_cells))
      coarse_grid%id = [ (k, k = 1, coarse_grid%n_cells) ]
    end if

    ! lowres additional properties
    allocate(areaCell0_2D(fine_grid%nx, fine_grid%ny))
    areaCell0_2D(:, :) = unpack(fine_grid%cell_area, fine_grid%mask, nodata_dp)

    if (.not. allocated(coarse_grid%cell_ij)) then
      allocate(coarse_grid%cell_ij  (coarse_grid%n_cells, 2))
      allocate(coarse_grid%cell_area  (coarse_grid%n_cells))
    end if

    allocate(this%y_lb(coarse_grid%n_cells))
    allocate(this%y_ub(coarse_grid%n_cells))
    allocate(this%x_lb(coarse_grid%n_cells))
    allocate(this%x_ub(coarse_grid%n_cells))
    allocate(this%n_subcells(coarse_grid%n_cells))
    allocate(this%coarse_id_map(fine_grid%nx, fine_grid%ny))
    this%coarse_id_map = nodata_i4

    this%fine_grid => fine_grid
    this%coarse_grid => coarse_grid

    k = 0
    do jc = 1, coarse_grid%ny
      do ic = 1, coarse_grid%nx
        if (.NOT. coarse_grid%mask(ic, jc)) cycle
        k = k + 1

        coarse_grid%cell_ij(k, 1) = ic
        coarse_grid%cell_ij(k, 2) = jc

        ! coord. of all corners -> of finer scale
        i_lb = (ic - 1) * cellFactor_i + 1
        ! constrain the range to fine grid extend
        i_ub = min(ic * cellFactor_i, fine_grid%nx)

        j_lb = (jc - 1) * cellFactor_i + 1
        ! constrain the range to fine grid extend
        j_ub = min(jc * cellFactor_i, fine_grid%ny)

        this%x_lb(k) = i_lb
        this%x_ub(k) = i_ub

        this%y_lb(k) = j_lb
        this%y_ub(k) = j_ub

        ! effective area [km2] & total no. of fine grid cells within a given coarse grid cell
        coarse_grid%cell_area(k) = sum(areacell0_2D(i_lb : i_ub, j_lb : j_ub), fine_grid%mask(i_lb : i_ub, j_lb : j_ub))
        this%n_subcells(k) = count(fine_grid%mask(i_lb : i_ub, j_lb : j_ub))
        ! Delimitation of level-11 cells on level-0
        this%coarse_id_map(i_lb : i_ub, j_lb : j_ub) = k
      end do
    end do

    ! free space
    deallocate(areaCell0_2D)

    ! only estimate aux coords if we are on a projected grid
    if ( estimate_aux_ .and. fine_grid%coordsys == coordsys%cart .and. allocated(fine_grid%lat) .and. allocated(fine_grid%lon)) then
      allocate(coarse_grid%lat(coarse_grid%nx, coarse_grid%ny))
      allocate(coarse_grid%lon(coarse_grid%nx, coarse_grid%ny))
      do jc = 1, coarse_grid%ny
        do ic = 1, coarse_grid%nx
          ! coord. of all corners -> of finer scale
          i_lb = (ic - 1) * cellFactor_i + 1
          ! constrain the range to fine grid extend
          i_ub = min(ic * cellFactor_i, fine_grid%nx)
          j_lb = (jc - 1) * cellFactor_i + 1
          ! constrain the range to fine grid extend
          j_ub = min(jc * cellFactor_i, fine_grid%ny)
          ! estimate lat-lon coords by averaging sub-cell coordinates
          coarse_grid%lat(ic, jc) = sum(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)) / real(size(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)), dp)
          coarse_grid%lon(ic, jc) = sum(fine_grid%lon(i_lb:i_ub, j_lb:j_ub)) / real(size(fine_grid%lon(i_lb:i_ub, j_lb:j_ub)), dp)
        end do
      end do
    end if

  end subroutine from_target_resolution

  ! ------------------------------------------------------------------

  !>       \brief calculate grid properties regarding cell ids
  !>       \details following tasks are performed:
  !!       -  cell ids & numbering
  !!       -  storage of cell cordinates (row and coloum id)
  !>       \authors Rohini Kumar
  !>       \date Jan 2013
  !>       \changelog
  !!       - Rohini Kumar & Matthias Cuntz, May 2014
  !!         - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !!       - Matthias Cuntz, May 2014
  !!         - changed empirical distribution function so that doubles get the same value
  !!       - Matthias Zink & Matthias Cuntz, Feb 2016
  !!         - code speed up due to reformulation of CDF calculation
  !!       - Rohini Kumar, Mar 2016
  !!         - changes for handling multiple soil database options
  !!       - Robert Schweppe, Jun 2018
  !!         - refactoring and reformatting
  !!       - Sebastian Müller, Mar 2024
  !!         - moved to FORCES
  subroutine calculate_cell_ids(this)
    implicit none

    class(grid_t), intent(inout) :: this

    integer(i4) :: i, j, k

    !--------------------------------------------------------
    ! 1) Estimate each variable locally for a given domain
    ! 2) Pad each variable to its corresponding global one
    !--------------------------------------------------------
    this%n_cells = count(this%mask)

    allocate(this%cell_ij(this%n_cells, 2))
    allocate(this%id(this%n_cells))
    this%id = [ (k, k = 1, this%n_cells) ]

    !------------------------------------------------
    ! start looping for cell cordinates and ids
    !------------------------------------------------
    k = 0
    do j = 1, this%ny
      do i = 1, this%nx
        if (.NOT. this%mask(i, j)) cycle
        k = k + 1
        this%cell_ij(k, 1) = i
        this%cell_ij(k, 2) = j
      end do
    end do
  end subroutine calculate_cell_ids

  ! ------------------------------------------------------------------

  !>       \brief estimate cell area
  !>       \details following tasks are performed:
  !!       - estimate cell area based on coordinate system
  !>       \authors Rohini Kumar
  !>       \date Jan 2013
  !>       \changelog
  !!       - Rohini Kumar & Matthias Cuntz, May 2014
  !!         - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !!       - Matthias Cuntz, May 2014
  !!         - changed empirical distribution function so that doubles get the same value
  !!       - Matthias Zink & Matthias Cuntz, Feb 2016
  !!         - code speed up due to reformulation of CDF calculation
  !!       - Rohini Kumar, Mar 2016
  !!         - changes for handling multiple soil database options
  !!       - Robert Schweppe, Jun 2018
  !!         - refactoring and reformatting
  !!       - Sebastian Müller, Mar 2024
  !!         - moved to FORCES
  subroutine estimate_cell_area(this)

    use mo_constants, only : RadiusEarth_dp, TWOPI_dp
    implicit none

    class(grid_t), intent(inout) :: this

    real(dp), dimension(:, :), allocatable :: areaCell_2D
    integer(i4) :: j
    real(dp) :: rdum, degree_to_radian, degree_to_metre

    if (.not. allocated(this%cell_area)) allocate(this%cell_area(this%n_cells))

    ! regular X-Y coordinate system
    if(this%coordsys .eq. coordsys%cart) then
      this%cell_area(:) = this%cellsize * this%cellsize

    ! regular lat-lon coordinate system
    else if(this%coordsys .eq. coordsys%sph_deg) then
      allocate(areaCell_2D(this%nx, this%ny))

      degree_to_radian = TWOPI_dp / 360.0_dp
      degree_to_metre = RadiusEarth_dp * degree_to_radian
      do j = 1, this%ny
        ! get latitude in degrees (y-axis is increasing!)
        rdum = this%yllcorner + (real(j, dp) - 0.5_dp) * this%cellsize
        ! convert to radians
        rdum = rdum * degree_to_radian
        !    AREA [m2]
        areaCell_2D(:, j) = (this%cellsize * cos(rdum) * degree_to_metre) * (this%cellsize * degree_to_metre)
      end do
      this%cell_area(:) = pack(areaCell_2D(:, :), this%mask)

      ! free space
      deallocate(areaCell_2D)
    else
      call error_message("estimate_cell_area: unknown coordsys value: ", num2str(this%coordsys))
    end if

  end subroutine estimate_cell_area

  ! ------------------------------------------------------------------

  !>       \brief calculate coarse grid extend
  !>       \details Calculates basic grid properties at a required coarser level using
  !!       information of a given finer level.
  !!       Calculates basic grid properties at a required coarser level (e.g., L11) using
  !!       information of a given finer level (e.g., L0). Basic grid properties such as
  !!       nx, ny, xllcorner, yllcorner cellsize are estimated in this
  !!       routine.
  !>       \authors Matthias Zink & Rohini Kumar
  !>       \date Feb 2013
  subroutine calculate_coarse_extend(nx_in,  ny_in,  xllcorner_in,  yllcorner_in,  cellsize_in,  target_resolution, &
                                     nx_out, ny_out, xllcorner_out, yllcorner_out, cellsize_out, tol, aligning)

    implicit none

    integer(i4), intent(in) :: nx_in !< no. of cells in x direction at an input level
    integer(i4), intent(in) :: ny_in !< no. of cells in y direction at an input level
    real(dp), intent(in) :: xllcorner_in !< xllcorner at an input level
    real(dp), intent(in) :: yllcorner_in !< yllcorner at an input level
    real(dp), intent(in) :: cellsize_in !< cell size at an input level
    real(dp), intent(in) :: target_resolution !< resolution of an output level
    integer(i4), intent(out) :: nx_out !< no. of cells in x direction at an output level
    integer(i4), intent(out) :: ny_out !< no. of cells in y direction at an output level
    real(dp), intent(out) :: xllcorner_out !< xllcorner at an output level
    real(dp), intent(out) :: yllcorner_out !< yllcorner at an output level
    real(dp), intent(out) :: cellsize_out !< cell size at an output level
    real(dp), intent(in), optional :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: aligning !< aligning selector (corner: ll -> 0 (default), lr -> 1, tl -> 2, tr -> 3)

    real(dp) :: cellFactor, rounded, tol_
    integer(i4) :: rounded_int, align_

    tol_ = 1.e-7_dp
    if ( present(tol) ) tol_ = tol

    align_ = align%ll
    if ( present(aligning) ) align_ = aligning

    cellFactor = target_resolution / cellsize_in
    rounded = anint(cellFactor)
    rounded_int = nint(cellFactor)

    if (abs(rounded - cellFactor) > tol_) then
      call error_message( &
        '***ERROR: Two resolutions size do not confirm: ', &
        trim(adjustl(num2str(nint(target_resolution)))), &
        trim(adjustl(num2str(nint(cellsize_in)))))
    end if

    if (rounded_int < 1_i4) call error_message("***ERROR: cell factor needs to be >= 1 to setup an upscaler.")

    cellsize_out = target_resolution
    ny_out = nint(real(ny_in, dp) / cellFactor)
    nx_out = nint(real(nx_in, dp) / cellFactor)

    ! if we rounded down, but now we would miss cells, add rows and/or cols
    if ( ny_out * rounded_int < ny_in ) ny_out = ny_out + 1_i4
    if ( nx_out * rounded_int < nx_in ) nx_out = nx_out + 1_i4

    ! align grids based on the selected aligning corner
    ! keep yll if aligning in (lower)-left or (lower)-right
    if (align_ == align%ll .or. align_ == align%lr) then
      yllcorner_out = yllcorner_in
    else
      yllcorner_out = yllcorner_in + real(ny_in, dp) * target_resolution / rounded - real(ny_out, dp) * cellsize_out
    endif
    ! keep xll if aligning in lower-(left) or top-(left)
    if (align_ == align%ll .or. align_ == align%tl) then
      xllcorner_out = xllcorner_in
    else
      xllcorner_out = xllcorner_in + real(nx_in, dp) * target_resolution / rounded - real(nx_out, dp) * cellsize_out
    endif

  end subroutine calculate_coarse_extend

  ! ------------------------------------------------------------------

  !>       \brief Read spatial data.
  !>       \details Read spatial data from ascii file. Data will be transposed to be in xy order.
  !>       \authors Robert Schweppe
  !>       \date Jun 2018
  subroutine read_spatial_data_ascii_dp(path, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, data, mask, flip_y)
    implicit none

    character(len = *), intent(in) :: path !< path with location
    integer(i4), intent(in) :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in) :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in) :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_cellsize !< reference cellsize
    real(dp), dimension(:, :), allocatable, intent(out) :: data !< data, size (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask, size (nx, ny)
    logical, intent(in), optional :: flip_y !< whether to flip data along dim 2 to have an increasing y-axis (default: .true.)

    integer(i4) :: file_nrows ! number of rows of data fields (ny)
    integer(i4) :: file_ncols ! number of columns of data fields (nx)
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    real(dp), dimension(:, :), allocatable :: tmp_data ! data to be transposed, size (ny, nx)
    logical, dimension(:, :), allocatable :: tmp_mask ! mask to be transposed, size (ny, nx)
    integer(i4) :: i, j, fileunit
    logical :: flip_y_

    flip_y_ = .true.
    if (present(flip_y)) flip_y_ = flip_y

    ! compare headers always with reference header (intent in)
    call read_header_ascii(path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata)
    if ((file_ncols .ne. ref_ncols)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: ncols')
    if ((file_nrows .ne. ref_nrows)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: nrows')
    if ((abs(file_xllcorner - ref_xllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: xllcorner')
    if ((abs(file_yllcorner - ref_yllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: yllcorner')
    if ((abs(file_cellsize - ref_cellsize)   .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: cellsize')

    ! allocation and initialization of matrices
    allocate(tmp_data(file_nrows, file_ncols))
    tmp_data = file_nodata

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, 6
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (tmp_data(i, j), j = 1, file_ncols)
    end do
    close(fileunit)

    ! transpose of data due to longitude-latitude ordering
    allocate(data(file_ncols, file_nrows))
    data = transpose(tmp_data)
    if (flip_y_) call flip(data, idim=2)

    if ( present(mask) ) then
      allocate(tmp_mask(file_nrows, file_ncols))
      tmp_mask = .true.
      where (abs(tmp_data - file_nodata) .lt. tiny(1.0_dp))
        tmp_mask = .false.
      end where
      ! set mask .false. if nodata value appeared
      allocate(mask(file_ncols, file_nrows))
      mask = transpose(tmp_mask)
      if (flip_y_) call flip(mask, idim=2)
      deallocate(tmp_mask)
    end if

    deallocate(tmp_data)

  end subroutine read_spatial_data_ascii_dp

  !>       \brief Read spatial data.
  !>       \details Read spatial data from ascii file. Data will be transposed to be in xy order.
  !>       \authors Robert Schweppe
  !>       \date Jun 2018
  subroutine read_spatial_data_ascii_i4(path, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, data, mask, flip_y)
    implicit none

    character(len = *), intent(in) :: path !< path with location
    integer(i4), intent(in) :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in) :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in) :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_cellsize !< reference cellsize
    integer(i4), dimension(:, :), allocatable, intent(out) :: data !< data (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask (nx, ny)
    logical, intent(in), optional :: flip_y !< whether to flip data along dim 2 to have an increasing y-axis (default: .true.)

    integer(i4) :: file_nrows ! number of rows of data fields
    integer(i4) :: file_ncols ! number of columns of data fields
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    integer(i4), dimension(:, :), allocatable :: tmp_data ! data
    logical, dimension(:, :), allocatable :: tmp_mask ! mask
    integer(i4) :: i, j, fileunit
    logical :: flip_y_

    flip_y_ = .true.
    if (present(flip_y)) flip_y_ = flip_y

    ! compare headers always with reference header (intent in)
    call read_header_ascii(path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata)
    if ((file_ncols .ne. ref_ncols)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: ncols')
    if ((file_nrows .ne. ref_nrows)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: nrows')
    if ((abs(file_xllcorner - ref_xllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: xllcorner')
    if ((abs(file_yllcorner - ref_yllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: yllcorner')
    if ((abs(file_cellsize - ref_cellsize)   .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: cellsize')

    ! allocation and initialization of matrices
    allocate(tmp_data(file_nrows, file_ncols))
    tmp_data = int(file_nodata, i4)

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, 6
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (tmp_data(i, j), j = 1, file_ncols)
    end do
    close(fileunit)

    ! transpose of data due to longitude-latitude ordering
    allocate(data(file_ncols, file_nrows))
    data = transpose(tmp_data)
    if (flip_y_) call flip(data, idim=2)

    if ( present(mask) ) then
      allocate(tmp_mask(file_nrows, file_ncols))
      tmp_mask = .true.
      where (tmp_data .EQ. int(file_nodata, i4))
        tmp_mask = .false.
      end where
      ! set mask .false. if nodata value appeared
      allocate(mask(file_ncols, file_nrows))
      mask = transpose(tmp_mask)
      if (flip_y_) call flip(mask, idim=2)
      deallocate(tmp_mask)
    end if

    deallocate(tmp_data)

  end subroutine read_spatial_data_ascii_i4

  ! ------------------------------------------------------------------

  !>       \brief Reads header lines of ASCII files.
  !>       \details Reads header lines of ASCII files, e.g. dem, aspect, flow direction.
  !>       \authors Juliane Mai
  !>       \date Jan 2013
  subroutine read_header_ascii(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata)

    use mo_os, only : check_path_isfile
    use mo_constants, only : nodata_dp
    implicit none

    character(len = *), intent(in) :: path !< Name of file and its location
    integer(i4), intent(out) :: nrows !< number of rows (ny)
    integer(i4), intent(out) :: ncols !< number of columns (nx)
    real(dp), intent(out) :: xllcorner !< lower left corner (x)
    real(dp), intent(out) :: yllcorner !< lower left corner (y)
    real(dp), intent(out) :: cellsize !< cell size [m]
    real(dp), intent(out) :: nodata !< nodata value (default -9999.0)

    character(5) :: dummy
    integer(i4) :: io, fileunit

    !checking whether the file exists
    call check_path_isfile(path=path, raise=.true.)
    ! reading header from a file
    open (newunit = fileunit, file = path, status = 'old')
    read (fileunit, *) dummy, ncols
    read (fileunit, *) dummy, nrows
    read (fileunit, *) dummy, xllcorner
    read (fileunit, *) dummy, yllcorner
    read (fileunit, *) dummy, cellsize
    read (fileunit, *, iostat=io) dummy, nodata
    ! EOF reached (nodata not present, use default value)
    if (io < 0) nodata = nodata_dp
    close(fileunit)
  end subroutine read_header_ascii

end module mo_grid
