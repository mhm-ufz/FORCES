!> \file    mo_grid.f90
!> \brief   \copybrief mo_grid
!> \details \copydetails mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!!          In contrast to ascii grids, data will be assumed to follow xy axis order and increasing axis.
!!          Ascii grids actually represent data with a decreasing y-axis and in yx order.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_kind, only: i4, dp, sp
  use mo_utils, only: flip
  use mo_message, only : error_message, warn_message
  use mo_string_utils, only : num2str

  implicit none

  public :: read_spatial_data_ascii

  private
  ! coordsys selector
  integer(i4), public, parameter :: coordsys_cart = 0_i4 !< Cartesian coordinate system.
  integer(i4), public, parameter :: coordsys_sph_deg = 1_i4 !< Spherical coordinates in degrees.
  ! integer(i4), public, parameter :: coordsys_sph_rad = 2_i4
  ! align selector
  integer(i4), public, parameter :: align_ll = 0_i4 !< align in lower left corner
  integer(i4), public, parameter :: align_lr = 1_i4 !< align in lower right corner
  integer(i4), public, parameter :: align_ul = 2_i4 !< align in upper left corner
  integer(i4), public, parameter :: align_ur = 3_i4 !< align in upper right corner

  ! -------------------------------------------------------------------
  ! GRID description
  ! -------------------------------------------------------------------
  !> \class   grid_t
  !> \brief   2D grid description with data in xy order with strictly increasing axis.
  !> \details This type represents uniform grids with data in xy order with strictly increasing axis.
  !!          ASCII grid files have the exact oposite behavior: yx order, with decreasing y-axis.
  !!          NetCDF files nativly have yx order, but since Fortran arrays are column-major order,
  !!          the data read from .nc files is in xy order. If the y axis is decreasing, data arrays
  !!          should be flipped.
  type, public :: grid_t
    integer(i4) :: coordsys = coordsys_cart !< Coordinate system for x and y. 0 -> Cartesian (default), 1 -> Spherical
    ! general domain information
    integer(i4) :: nx        !< size of x-axis (number of cols in ascii grid file)
    integer(i4) :: ny        !< size of y-axis (number of rows in ascii grid file)
    integer(i4) :: n_cells   !< number of cells in mask
    real(dp) :: xllcorner    !< x coordinate of the lowerleft corner
    real(dp) :: yllcorner    !< y coordinate of the lowerleft corner
    real(dp) :: cellsize     !< cellsize x = cellsize y
    integer(i4), dimension(:), allocatable :: id     !< IDs of cells in mask (1..n_cells)
    real(dp), dimension(:), allocatable :: cell_area !< area of the cell in sqare m, size (n_cells)
    logical, dimension(:, :), allocatable :: mask    !< the mask for valid cells in the original grid, size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat    !< 2d longitude array (auxiliary coordinate for X axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lon    !< 2d latitude  array (auxiliary coordinate for Y axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat_vertices  !< latitude coordinates or the grid nodes, size (nx+1, ny+1)
    real(dp), dimension(:, :), allocatable :: lon_vertices  !< longitude coordinates or the grid nodes, size (nx+1, ny+1)
    integer(i4), dimension(:, :), allocatable :: cell_ij    !< matrix IDs (i, j) per cell in mask, size (n_cells, 2)
  contains
    !> \copydoc mo_grid::from_header_info
    procedure, public :: init => grid_init !< \see mo_grid::from_header_info
    !> \copydoc mo_grid::from_ascii_file
    procedure, public :: from_ascii_file !< \see mo_grid::from_ascii_file
    procedure, private :: from_nc_dataset, from_nc_file
    !> \brief initialize grid from a netcdf file/dataset with a reference variable.
    generic, public :: from_netcdf => from_nc_dataset, from_nc_file !< \see mo_grid::from_nc_file
    procedure, private :: aux_from_nc_dataset, aux_from_nc_file
    !> \brief read auxilliar coordinates from netcdf file/dataset.
    generic, public :: aux_from_netcdf => aux_from_nc_dataset, aux_from_nc_file !< \see mo_grid::aux_from_nc_file
    !> \copydoc mo_grid::extend
    procedure, public :: extend !< \see mo_grid::extend
    !> \copydoc mo_grid::x_axis
    procedure, public :: x_axis !< \see mo_grid::x_axis
    !> \copydoc mo_grid::y_axis
    procedure, public :: y_axis !< \see mo_grid::y_axis
    !> \copydoc mo_grid::x_vertices
    procedure, public :: x_vertices !< \see mo_grid::x_vertices
    !> \copydoc mo_grid::y_vertices
    procedure, public :: y_vertices !< \see mo_grid::y_vertices
    !> \copydoc mo_grid::x_bounds
    procedure, public :: x_bounds !< \see mo_grid::x_bounds
    !> \copydoc mo_grid::y_bounds
    procedure, public :: y_bounds !< \see mo_grid::y_bounds
    !> \copydoc mo_grid::estimate_aux_coords
    procedure, public :: estimate_aux_coords !< \see mo_grid::estimate_aux_coords
    !> \copydoc mo_grid::estimate_aux_vertices
    procedure, public :: estimate_aux_vertices !< \see mo_grid::estimate_aux_vertices
    !> \copydoc mo_grid::lat_bounds
    procedure, public :: lat_bounds !< \see mo_grid::lat_bounds
    !> \copydoc mo_grid::lon_bounds
    procedure, public :: lon_bounds !< \see mo_grid::lon_bounds
    ! !> \copydoc mo_grid::derive_level
    ! procedure, public :: derive_level !< \see mo_grid::derive_level
    !> \copydoc mo_grid::is_masked
    procedure, public :: is_masked !< \see mo_grid::is_masked
    !> \copydoc mo_grid::check_is_covered_by
    procedure, public :: check_is_covered_by !< \see mo_grid::check_is_covered_by
    !> \copydoc mo_grid::check_is_covering
    procedure, public :: check_is_covering !< \see mo_grid::check_is_covering
    ! !> \copydoc mo_grid::read_aux_coords
    ! procedure, public :: read_aux_coords !< \see mo_grid::read_aux_coords
    !> \copydoc mo_grid::has_aux_coords
    procedure, public :: has_aux_coords !< \see mo_grid::has_aux_coords
    !> \copydoc mo_grid::has_aux_vertices
    procedure, public :: has_aux_vertices !< \see mo_grid::has_aux_vertices
    !> \copydoc mo_grid::calculate_cell_ids
    procedure, public :: calculate_cell_ids !< \see mo_grid::calculate_cell_ids
    !> \copydoc mo_grid::estimate_cell_area
    procedure, public :: estimate_cell_area !< \see mo_grid::estimate_cell_area
    ! !> \copydoc mo_grid::read_data
    ! procedure, public :: read_data !< \see mo_grid::read_data
    ! !> \copydoc mo_grid::pack_data
    ! procedure, public :: pack_data !< \see mo_grid::pack_data
    ! !> \copydoc mo_grid::unpack_data
    ! procedure, public :: unpack_data !< \see mo_grid::unpack_data
    ! !> \copydoc mo_grid::flip_packed_data
    ! procedure, public :: flip_packed_data !< \see mo_grid::flip_packed_data
  end type grid_t

  !> \class   upscaler_t
  !> \brief   Upscaler type to remap data on regular grids with an integer cellsize ratio.
  type, public :: upscaler_t
    type(grid_t), pointer :: fine_grid   !< high resolution grid
    type(grid_t), pointer :: coarse_grid !< low resolution grid
    integer(i4) :: factor                !< coarse_grid % cellsize / fine_grid % cellsize
    integer(i4), dimension(:), allocatable :: y_lb              !< lower bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: y_ub              !< upper bound for y-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_lb              !< lower bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: x_ub              !< upper bound for x-id on fine grid (coarse%n_cells)
    integer(i4), dimension(:), allocatable :: n_subcells        !< valid fine grid cells in coarse cell (coarse%n_cells)
    integer(i4), dimension(:, :), allocatable :: coarse_id_map  !< 2d index array of coarse ids (fine%nx, fine%ny)
  contains
    !> \copydoc mo_grid::from_target_resolution
    procedure, public :: from_target_resolution !< \see mo_grid::from_target_resolution
    ! !> \copydoc mo_grid::from_grids
    ! procedure, public :: from_grids !< \see mo_grid::from_grids
  end type upscaler_t

  !> \brief Reads spatial data files of ASCII format.
  !> \details Reads spatial input data, e.g. dem, aspect, flow direction.
  !> \authors Juliane Mai
  !> \date Jan 2013
  !> \changelog
  !! - Matthias Zink, Feb 2013
  !!   - added interface and routine for datatype i4
  !! - David Schaefer, Mar 2015
  !!   - removed double allocation of temporary data
  !! - Robert Schweppe, Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Müller, Mar 2024
  !!   - moving to FORCES
  !!   - remove fileunit input (use newunit)
  !!   - make mask optional output
  !!   - add flip_y argument
  interface read_spatial_data_ascii
    module procedure read_spatial_data_ascii_i4, read_spatial_data_ascii_dp
  end interface read_spatial_data_ascii

contains

  !> \brief Setup upscaler and coarse grid from given target resolution
  !> \details following attributes are calculated for the coarse grid:
  !!          -  cell id & numbering
  !!          -  mask creation
  !!          -  storage of cell cordinates (row and coloum indizes)
  !!          -  sorage of four sub-cell corner cordinates
  !!
  !!          fine_grid and coarse_grid need to be targets, since the upscaler will only
  !!          hold pointers to the associated grids.
  !> \authors Rohini Kumar
  !> \date    Jan 2013
  !> \changelog
  !! - Sebastian Müller, Mar 2024
  !!   - moving to FORCES
  !!   - is now a method of the upscaler type
  subroutine from_target_resolution(this, target_resolution, fine_grid, coarse_grid, estimate_aux, tol)

    use mo_constants, only : nodata_dp, nodata_i4

    implicit none

    class(upscaler_t), intent(inout) :: this !< remapper type for given grids
    real(dp), intent(in) :: target_resolution !< desired target resolution
    type(grid_t), target, intent(in) :: fine_grid !< given high resolution grid
    type(grid_t), target, intent(inout) :: coarse_grid !< resulting low resolution grid
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    real(dp), dimension(:, :), allocatable :: areaCell0_2D
    real(dp) :: cellFactor
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
      cellsize_out=coarse_grid%cellsize, &
      tol=tol &
    )

    coarse_grid%coordsys = fine_grid%coordsys

    cellFactor = anint(coarse_grid%cellsize / fine_grid%cellsize, dp)
    this%factor = nint(cellFactor, i4)

    ! allocation and initalization of mask at coarse grid
    allocate(coarse_grid%mask(coarse_grid%nx, coarse_grid%ny))
    coarse_grid%mask(:, :) = .false.

    ! create mask at coarse grid
    do j = 1_i4, fine_grid%ny
      ! everything would be better with 0-based ids
      jc = (j-1_i4) / this%factor + 1_i4
      do i = 1, fine_grid%nx
        if (.not. fine_grid%mask(i, j)) cycle
        ic = (i-1_i4) / this%factor + 1_i4
        coarse_grid%mask(ic, jc) = .true.
      end do
    end do

    call coarse_grid%calculate_cell_ids()

    ! lowres additional properties
    allocate(areaCell0_2D(fine_grid%nx, fine_grid%ny))
    areaCell0_2D(:, :) = unpack(fine_grid%cell_area, fine_grid%mask, nodata_dp)

    allocate(coarse_grid%cell_area(coarse_grid%n_cells))
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
        ! coord. of all corners -> of finer scale
        i_lb = (ic - 1) * this%factor + 1
        ! constrain the range to fine grid extend
        i_ub = min(ic * this%factor, fine_grid%nx)

        j_lb = (jc - 1) * this%factor + 1
        ! constrain the range to fine grid extend
        j_ub = min(jc * this%factor, fine_grid%ny)

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
    if ( estimate_aux_ .and. fine_grid%coordsys == coordsys_cart .and. fine_grid%has_aux_coords()) then
      call coarse_grid%estimate_aux_coords(fine_grid, tol=tol)
    end if

  end subroutine from_target_resolution

  ! ------------------------------------------------------------------

  !> \brief initialize grid from ascii header content
  !> \details initialize grid from standard ascii header content (nx (cols), ny (rows), cellsize, lower-left corner)
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_init(this, nx, ny, xllcorner, yllcorner, cellsize, coordsys, mask)
    implicit none
    class(grid_t), intent(inout) :: this
    integer(i4), intent(in) :: nx !< Number of x-axis subdivisions
    integer(i4), intent(in) :: ny !< Number of y-axis subdivisions
    real(dp), optional, intent(in) :: xllcorner !< lower left corner (x) (default 0.0)
    real(dp), optional, intent(in) :: yllcorner !< lower left corner (y) (default 0.0)
    real(dp), optional, intent(in) :: cellsize !< cell size [m] or [deg] (default 1.0)
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (default 0 for cartesian)
    logical, dimension(:,:), optional, intent(in) :: mask !< desired mask for the grid (default: all .true.)

    this%nx = nx
    this%ny = ny
    this%xllcorner = 0.0_dp
    if ( present(xllcorner) ) this%xllcorner = xllcorner
    this%yllcorner = 0.0_dp
    if ( present(yllcorner) ) this%yllcorner = yllcorner
    this%cellsize = 1.0_dp
    if ( present(cellsize) ) this%cellsize = cellsize
    ! check if coordsys is supported
    this%coordsys = coordsys_cart
    if ( present(coordsys) ) then
      if (coordsys /= coordsys_cart .and. coordsys /= coordsys_sph_deg) &
        call error_message("grid % init: unknown coordsys value: ", num2str(coordsys))
      this%coordsys = coordsys
    end if
    if ( present(mask) ) then
      if (size(mask, dim=1) /= nx .or. size(mask, dim=2) /= ny) &
        call error_message("grid % init: mask has wrong shape: mask(", &
                           trim(adjustl(num2str(size(mask, dim=1)))), ",", &
                           trim(adjustl(num2str(size(mask, dim=2)))), ") =/= grid(", &
                           trim(adjustl(num2str(nx))), ",", &
                           trim(adjustl(num2str(ny))), ")")
      allocate(this%mask(this%nx, this%ny))
      this%mask = mask
    end if
    ! if no mask given, this will initialize the default mask
    call this%calculate_cell_ids()
    call this%estimate_cell_area()

  end subroutine grid_init

  !> \brief initialize grid from ascii grid file
  !> \details initialize grid from a given ascii grid file.
  !!          If mask should be read, it will be in xy order with increasing y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_ascii_file(this, path, coordsys, read_mask)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (default 0 for cartesian)
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given file (default: .true.)

    integer(i4) :: nx, ny
    real(dp) :: xll, yll, cellsize
    real(dp), allocatable, dimension(:,:) :: dummy
    logical, allocatable, dimension(:,:) :: mask
    logical :: read_mask_

    read_mask_ = .true.
    if ( present(read_mask) ) read_mask_ = read_mask

    call check_path_isfile(path=path, raise=.true.)
    call read_header_ascii(path, nx ,ny, xll, yll, cellsize)

    if (read_mask_) then
      call read_spatial_data_ascii_dp(path, nx, ny, xll, yll, cellsize, dummy, mask, flip_y=.true.)
      deallocate(dummy)
    else
      allocate(mask(nx, ny))
      mask(:,:) = .true.
    end if

    call this%init(nx, ny, xll, yll, cellsize, coordsys, mask)
    deallocate(mask)

  end subroutine from_ascii_file

  !> \brief initialize grid from a netcdf file
  !> \details initialize grid from a netcdf file and a reference variable.
  !!          If mask should be read, it will be in xy order with increasing y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_nc_file(this, path, var, read_mask, read_aux, tol)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%from_nc_dataset(nc, var, read_mask, read_aux, tol)
    call nc%close()
  end subroutine from_nc_file

  !> \brief initialize grid from a netcdf dataset
  !> \details initialize grid from a netcdf dataset and a reference variable.
  !!          If mask should be read, it will be in xy order with increasing y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_nc_dataset(this, nc, var, read_mask, read_aux, tol)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)

    type(NcVariable) :: ncvar, xvar, yvar, latvar, lonvar
    type(NcDimension), dimension(:), allocatable :: dims

    integer(i4), dimension(:), allocatable :: shp, start, cnt
    integer(i4) :: nx, ny, rnk, coordsys
    real(dp) :: xll, yll, cellsize, cs_x, cs_y, tol_
    real(dp), allocatable, dimension(:,:) :: dummy
    character(len=256) :: tmp_str
    character(len=256), allocatable, dimension(:) :: coords_str
    logical, allocatable, dimension(:,:) :: mask
    logical :: y_inc, read_mask_, read_aux_, x_sph, y_sph, x_cart, y_cart

    tol_ = 1.e-7_dp
    read_mask_ = .true.
    read_aux_ = .true.
    if ( present(tol) ) tol_ = tol
    if ( present(read_mask) ) read_mask_ = read_mask
    if ( present(read_aux) ) read_aux_ = read_aux

    ncvar = nc%getVariable(var)
    rnk = ncvar%getRank()
    if (rnk < 2) call error_message("grid % from_netcdf: given variable has too few dimensions: ", trim(nc%fname), ":", var)

    dims = ncvar%getDimensions()
    nx = dims(1)%getLength()
    ny = dims(2)%getLength()
    xvar = nc%getVariable(trim(dims(1)%getName()))
    yvar = nc%getVariable(trim(dims(2)%getName()))

    ! check if x/y axis are x/y/lon/lat by standard_name, units, axistype or long_name
    if (is_x_axis(yvar).or.is_lon_coord(yvar).or.is_y_axis(xvar).or.is_lat_coord(xvar)) &
      call error_message("grid % from_netcdf: variable seems to have wrong axis order (not y-x): ", trim(nc%fname), ":", var)

    x_cart = is_x_axis(xvar)
    y_cart = is_y_axis(yvar)
    x_sph = is_lon_coord(xvar)
    y_sph = is_lat_coord(yvar)

    if (.not.(x_cart.or.x_sph)) &
      call error_message("grid % from_netcdf: can't determine coordinate system from x-axis: ", trim(nc%fname), ":", var)
    if (.not.(y_cart.or.y_sph)) &
      call error_message("grid % from_netcdf: can't determine coordinate system from y-axis: ", trim(nc%fname), ":", var)
    if (.not.(x_sph.eqv.y_sph)) &
      call error_message("grid % from_netcdf: x and y axis seem to have different coordinate systems: ", trim(nc%fname), ":", var)

    coordsys = coordsys_cart
    if (x_sph) coordsys = coordsys_sph_deg

    ! check axis uniformity and monotonicity
    call check_uniform_axis(xvar, cellsize=cs_x, origin=xll, tol=tol)
    call check_uniform_axis(yvar, cellsize=cs_y, origin=yll, increasing=y_inc, tol=tol)
    if (.not.y_inc) call warn_message("grid % from_netcdf: y axis is decreasing which results in inefficient data flipping. ", &
                                      "You could flip the file with: 'cdo invertlat <ifile> <ofile>'. ", trim(nc%fname), ":", var)
    ! check cellsize in x and y direction
    if (.not.is_close(cs_x, cs_y, rtol=0.0_dp, atol=tol_)) &
      call error_message("grid % from_netcdf: x and y axis have different cell sizes: ", trim(nc%fname), ":", var)
    cellsize = cs_x

    ! get mask from variable mask (assumed to be constant over time)
    if (read_mask_) then
      shp = ncvar%getShape()
      allocate(start(rnk), source=1_i4)
      allocate(cnt(rnk), source=1_i4)
      ! only use first 2 dims and use first layer of potential other dims (z, time, soil-layer etc.)
      cnt(:2) = shp(:2)
      call ncvar%getData(dummy, start=start, cnt=cnt, mask=mask)
      ! flip mask if y-axis is decreasing in nc-file
      if (.not.y_inc) call flip(mask, iDim=2)
      deallocate(dummy)
    else
      allocate(mask(nx, ny))
      mask(:,:) = .true.
    end if

    call this%init(nx, ny, xll, yll, cellsize, coordsys, mask)
    deallocate(mask)

    if (read_aux_ .and. coordsys == coordsys_cart .and. ncvar%hasAttribute("coordinates")) then
      call ncvar%getAttribute("coordinates", tmp_str)
      coords_str = splitString(trim(tmp_str), " ")
      if (size(coords_str) < 2) &
        call error_message("grid % from_netcdf: to few auxilliar coordinates: ", trim(nc%fname), ":", var, " - ", trim(tmp_str))
      call this%aux_from_netcdf(nc, lat=trim(coords_str(size(coords_str)-1)), lon=trim(coords_str(size(coords_str))))
    end if

  end subroutine from_nc_dataset

  !> \brief read auxilliar coordinates from a netcdf file
  !> \details read auxilliar coordinates (lat, lon) from a netcdf file.
  !!          If mask should be read, it will be in xy order with increasing y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine aux_from_nc_file(this, path, lat, lon)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: lat !< nc variable name for latitude
    character(*), intent(in) :: lon !< nc variable name for longitude
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%aux_from_nc_dataset(nc, lat, lon)
    call nc%close()
  end subroutine aux_from_nc_file

  !> \brief read auxilliar coordinates from a netcdf file
  !> \details read auxilliar coordinates (lat, lon) from a netcdf file.
  !!          lat and lon will be read in xy order with increasing y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine aux_from_nc_dataset(this, nc, lat, lon)
    use mo_netcdf, only : NcDataset, NcVariable
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: lat !< nc variable name for latitude
    character(*), intent(in) :: lon !< nc variable name for longitude

    type(NcVariable) :: latvar, lonvar
    integer(i4) :: rnk
    real(dp), allocatable, dimension(:,:) :: dummy
    logical :: y_inc
    character(:), allocatable :: lon_, lat_

    if (this%coordsys /= coordsys_cart) &
      call error_message("grid % aux_from_netcdf: need projected axis to have auxilliar coordinates.")

    lon_ = lon
    lat_ = lat

    latvar = nc%getVariable(lat_)
    if (is_lon_coord(latvar)) then
      ! be forgiving when coordinate order is wrong
      call warn_message("grid % aux_from_netcdf: auxilliar coordinates seem to be given in wrong order: ", &
                        trim(nc%fname), ":", lat, "/", lon)
      lat_ = lon
      lon_ = lat
      latvar = nc%getVariable(lat_)
    end if

    if (.not.is_lat_coord(latvar)) &
      call error_message("grid % aux_from_netcdf: auxilliar latitude coordinate is not valid: ", trim(nc%fname), ":", lat_)
    rnk = latvar%getRank()
    if (rnk /= 2) call &
      error_message("grid % from_netcdf: auxilliar latitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lat_)

    call latvar%getData(dummy)
    y_inc = .true.
    if (size(dummy, dim=2)>1) y_inc = dummy(1,1) < dummy(1,2)
    allocate(this%lat(this%nx, this%ny))
    if (.not.y_inc) call flip(dummy, iDim=2)
    this%lat = dummy
    deallocate(dummy)

    lonvar = nc%getVariable(lon_)
    if (.not.is_lon_coord(lonvar)) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not valid: ", trim(nc%fname), ":", lon_)
    rnk = lonvar%getRank()
    if (rnk /= 2) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lon_)

    call lonvar%getData(dummy)
    allocate(this%lon(this%nx, this%ny))
    if (.not.y_inc) call flip(dummy, iDim=2)
    this%lon = dummy
    deallocate(dummy)

  end subroutine aux_from_nc_dataset

  !> \brief get grid extend
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine extend(this, x_min, x_max, y_min, y_max)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), optional, intent(out) :: x_min !< left bound (x)
    real(dp), optional, intent(out) :: x_max !< right bound (x)
    real(dp), optional, intent(out) :: y_min !< lower bound (y)
    real(dp), optional, intent(out) :: y_max !< upper bound (y)

    if ( present(x_min) ) x_min = this%xllcorner
    if ( present(x_max) ) x_max = this%xllcorner + this%nx * this%cellsize
    if ( present(y_min) ) y_min = this%yllcorner
    if ( present(y_max) ) y_max = this%yllcorner + this%ny * this%cellsize

  end subroutine extend

  !> \brief x-axis of the grid cell centers
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function x_axis(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: x_axis

    integer(i4) :: i

    x_axis = [ ((i-0.5_dp) * this%cellsize + this%xllcorner, i=1_i4, this%nx) ]

  end function x_axis

  !> \brief y-axis of the grid cell centers
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_axis(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: y_axis

    integer(i4) :: i

    y_axis = [ ((i-0.5_dp) * this%cellsize + this%yllcorner, i=1_i4, this%ny) ]

  end function y_axis

  !> \brief x-vertices of the grid cell edges
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function x_vertices(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: x_vertices

    integer(i4) :: i

    x_vertices = [ (i * this%cellsize + this%xllcorner, i=0_i4, this%nx) ]

  end function x_vertices

  !> \brief y-vertices of the grid cell edges
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_vertices(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:) :: y_vertices

    integer(i4) :: i

    y_vertices = [ (i * this%cellsize + this%yllcorner, i=0_i4, this%ny) ]

  end function y_vertices

  !> \brief x-bounds of the grid cell following cf-conventions (2, nx).
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function x_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:) :: x_bounds
    real(dp), allocatable, dimension(:) :: x_ax
    x_ax = this%x_vertices()
    allocate(x_bounds(2, this%nx))
    x_bounds(1,:) = x_ax(1:this%nx)
    x_bounds(2,:) = x_ax(2:this%nx+1)
  end function x_bounds

  !> \brief y-bounds of the grid cells following cf-conventions (2, ny).
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:) :: y_bounds
    real(dp), allocatable, dimension(:) :: y_ax
    y_ax = this%y_vertices()
    allocate(y_bounds(2, this%ny))
    y_bounds(1,:) = y_ax(1:this%ny)
    y_bounds(2,:) = y_ax(2:this%ny+1)
  end function y_bounds

  !> \brief estimate auxilliar coordinates (lat, lon) from finer grid
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine estimate_aux_coords(this, fine_grid, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(in) :: fine_grid !< finer grid to estimate the auxilliar coordinates from
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: n_subcells
    integer(i4) :: i_ub, i_lb, j_lb, j_ub, i, j, factor

    call this%check_is_covering(fine_grid, tol=tol, check_mask=.false.)
    call check_factor(fine_grid%cellsize, this%cellsize, factor=factor, tol=tol)

    if (this % coordsys /= coordsys_cart) &
      call error_message("grid % estimate_aux_coords: grids allready use spherical coordinate system for axis.")

    if (.not. fine_grid%has_aux_coords()) &
      call error_message("grid % estimate_aux_coords: fine grid has no auxilliar coordinates defined.")

    if (this%has_aux_coords()) &
      call error_message("grid % estimate_aux_coords: grid already has auxilliar coordinates defined.")

    allocate(this%lat(this%nx, this%ny))
    allocate(this%lon(this%nx, this%ny))
    do j = 1, this%ny
      do i = 1, this%nx
        ! coord. of all corners -> of finer scale
        i_lb = (i - 1) * factor + 1
        i_ub = min(i * factor, fine_grid%nx)
        j_lb = (j - 1) * factor + 1
        j_ub = min(j * factor, fine_grid%ny)
        n_subcells = real(size(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)), dp)
        ! estimate lat-lon coords by averaging sub-cell coordinates (even if fine grid is "a bit" smaller)
        this%lat(i, j) = sum(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)) / n_subcells
        this%lon(i, j) = sum(fine_grid%lon(i_lb:i_ub, j_lb:j_ub)) / n_subcells
      end do
    end do

  end subroutine estimate_aux_coords

  !> \brief estimate vertices of auxilliar coordinate cells
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine estimate_aux_vertices(this)
    implicit none
    class(grid_t), intent(inout) :: this
    integer(i4) :: i, j

    if (.not. this%has_aux_coords()) &
      call error_message("grid % estimate_aux_vertices: grid has no auxilliar coordinates defined.")

    if (this%has_aux_vertices()) &
      call error_message("grid % estimate_aux_vertices: grid already has auxilliar vertices defined.")

    if (this%nx == 1_i4 .or. this%ny == 1_i4) &
      call error_message("grid % estimate_aux_vertices: grid needs at least 2 cells in both directions.")

    allocate(this%lat_vertices(this%nx+1_i4, this%ny+1_i4))
    allocate(this%lon_vertices(this%nx+1_i4, this%ny+1_i4))
    ! first calculate the inner vertices as mean of surrounding cell centers
    do j = 2, this%ny
      do i = 2, this%nx
        this%lat_vertices(i, j) = sum(this%lat(i-1:i, j-1:j)) / 4.0_dp
        this%lon_vertices(i, j) = sum(this%lon(i-1:i, j-1:j)) / 4.0_dp
      end do
    end do
    ! these formulas seem off, but are correct: extrapolate from next inner point on line with mean of two connected cell centers.
    do j = 2, this%ny
      ! left side
      this%lat_vertices(1, j) = sum(this%lat(1, j-1:j)) - this%lat_vertices(2, j)
      this%lon_vertices(1, j) = sum(this%lon(1, j-1:j)) - this%lon_vertices(2, j)
      ! right side
      this%lat_vertices(this%nx+1, j) = sum(this%lat(this%nx, j-1:j)) - this%lat_vertices(this%nx, j)
      this%lon_vertices(this%nx+1, j) = sum(this%lon(this%nx, j-1:j)) - this%lon_vertices(this%nx, j)
    end do
    do i = 2, this%nx
      ! upper side
      this%lat_vertices(i, 1) = sum(this%lat(i-1:i, 1)) - this%lat_vertices(i, 2)
      this%lon_vertices(i, 1) = sum(this%lon(i-1:i, 1)) - this%lon_vertices(i, 2)
      ! lower side
      this%lat_vertices(i, this%ny+1) = sum(this%lat(i-1:i, this%ny)) - this%lat_vertices(i, this%ny)
      this%lon_vertices(i, this%ny+1) = sum(this%lon(i-1:i, this%ny)) - this%lon_vertices(i, this%ny)
    end do
    ! lower left corner
    this%lat_vertices(1, 1) = this%lat_vertices(2, 1) ! from right neighbor
    this%lon_vertices(1, 1) = this%lon_vertices(1, 2) ! from upper neighbor
    ! upper left corner
    this%lat_vertices(1, this%ny+1) = this%lat_vertices(2, this%ny+1) ! from right neighbor
    this%lon_vertices(1, this%ny+1) = this%lon_vertices(1, this%ny) ! from lower neighbor
    ! lower right corner
    this%lat_vertices(this%nx+1, 1) = this%lat_vertices(this%nx, 1) ! from left neighbor
    this%lon_vertices(this%nx+1, 1) = this%lon_vertices(this%nx+1, 2) ! from upper neighbor
    ! upper right corner
    this%lat_vertices(this%nx+1, this%ny+1) = this%lat_vertices(this%nx, this%ny+1) ! from left neighbor
    this%lon_vertices(this%nx+1, this%ny+1) = this%lon_vertices(this%nx+1, this%ny) ! from lower neighbor

  end subroutine estimate_aux_vertices

  !> \brief lat-bounds of the grid cell following cf-conventions (4, nx, ny).
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function lat_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lat_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lat_bounds: grid has no auxilliar vertices defined.")
    allocate(lat_bounds(4, this%nx, this%ny))
    ! lower-left corner of the cells
    lat_bounds(1,:,:) = this%lat_vertices(1:this%nx, 1:this%ny)
    ! lower-right corner of the cells
    lat_bounds(2,:,:) = this%lat_vertices(2:this%nx+1, 1:this%ny)
    ! upper-right corner of the cells
    lat_bounds(3,:,:) = this%lat_vertices(2:this%nx+1, 2:this%ny+1)
    ! upper-left corner of the cells
    lat_bounds(4,:,:) = this%lat_vertices(1:this%nx, 2:this%ny+1)
  end function lat_bounds

  !> \brief lon-bounds of the grid cell following cf-conventions (4, nx, ny).
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function lon_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lon_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lon_bounds: grid has no auxilliar vertices defined.")
    allocate(lon_bounds(4, this%nx, this%ny))
    ! lower-left corner of the cells
    lon_bounds(1,:,:) = this%lon_vertices(1:this%nx, 1:this%ny)
    ! lower-right corner of the cells
    lon_bounds(2,:,:) = this%lon_vertices(2:this%nx+1, 1:this%ny)
    ! upper-right corner of the cells
    lon_bounds(3,:,:) = this%lon_vertices(2:this%nx+1, 2:this%ny+1)
    ! upper-left corner of the cells
    lon_bounds(4,:,:) = this%lon_vertices(1:this%nx, 2:this%ny+1)
  end function lon_bounds

  !> \brief check if given grid is masked (mask allocated and any value .false.)
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_masked(this)
    implicit none
    class(grid_t), intent(in) :: this
    if (.not. allocated(this%mask)) then
      is_masked = .false.
      return
    end if
    is_masked = .not. all(this%mask)
  end function is_masked

  !> \brief check if given grid is covered by coarser grid
  !> \details check if given grid is compatible and covered by coarser grid and raise an error if this is not the case.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_is_covered_by(this, coarse_grid, tol, check_mask)
    use mo_utils, only: eq
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: coarse_grid !< coarse grid that should cover this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask

    integer(i4) :: factor, i, j, i_lb, i_ub, j_lb, j_ub
    logical :: check_mask_

    check_mask_ = .true.
    if ( present(check_mask) ) check_mask_ = check_mask

    if (this%coordsys /= coarse_grid%coordsys) &
      call error_message("grid % check_is_covered_by: grids don't use the same coordinate system.")

    call check_factor(this%cellsize, coarse_grid%cellsize, factor=factor, tol=tol)

    ! check ll corner
    if ( .not. (eq(this%xllcorner, coarse_grid%xllcorner) .and. eq(this%yllcorner, coarse_grid%yllcorner)) ) then
      call error_message("grid % check_is_covered_by: coarse grid lower-left corner is not matching.")
    end if
    ! check extend
    if (.not. ((coarse_grid%nx - 1) * factor <= this%nx .and. this%nx <= coarse_grid%nx * factor .and. &
               (coarse_grid%ny - 1) * factor <= this%ny .and. this%ny <= coarse_grid%ny * factor)) then
      call error_message("grid % check_is_covered_by: coarse grid extend is not matching.")
    end if

    if ( check_mask_ .and. coarse_grid%is_masked()) then
      if (.not. this%is_masked()) call error_message("grid % check_is_covered_by: coarse grid is masked, this grid not.")
      do j = 1, coarse_grid%ny
        do i = 1, coarse_grid%nx
          if ( coarse_grid%mask(i, j)) cycle
          ! coord. of all corners -> of finer scale
          i_lb = (i - 1) * factor + 1
          i_ub = min(i * factor, coarse_grid%nx)
          j_lb = (j - 1) * factor + 1
          j_ub = min(j * factor, coarse_grid%ny)
          if (any(this%mask(i_lb:i_ub, j_lb:j_ub))) &
            call error_message("grid % check_is_covered_by: fine cells outside of coarse mask.")
        end do
      end do
    end if
  end subroutine check_is_covered_by

  !> \brief check if given grid is covering finer grid
  !> \details check if given grid is compatible with and covering finer grid and raise an error if this is not the case.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_is_covering(this, fine_grid, tol, check_mask)
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: fine_grid !< finer grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask
    call fine_grid%check_is_covered_by(coarse_grid=this, tol=tol, check_mask=check_mask)
  end subroutine check_is_covering

  !> \brief check if given grid has auxilliar coordinates allocated.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function has_aux_coords(this)
    implicit none
    class(grid_t), intent(in) :: this
    has_aux_coords = allocated(this%lat) .and. allocated(this%lon)
  end function has_aux_coords

  !> \brief check if given grid has auxilliar vertices allocated.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function has_aux_vertices(this)
    implicit none
    class(grid_t), intent(in) :: this
    has_aux_vertices = allocated(this%lat_vertices) .and. allocated(this%lon_vertices)
  end function has_aux_vertices

  ! ------------------------------------------------------------------

  !> \brief   calculate grid properties regarding cell ids
  !> \details following tasks are performed:
  !!          -  cell ids & numbering
  !!          -  storage of cell cordinates (row and coloum id)
  !> \authors Rohini Kumar
  !> \date Jan 2013
  !> \changelog
  !! - Rohini Kumar & Matthias Cuntz, May 2014
  !!   - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !! - Matthias Cuntz, May 2014
  !!   - changed empirical distribution function so that doubles get the same value
  !! - Matthias Zink & Matthias Cuntz, Feb 2016
  !!   - code speed up due to reformulation of CDF calculation
  !! - Rohini Kumar, Mar 2016
  !!   - changes for handling multiple soil database options
  !! - Robert Schweppe, Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Müller, Mar 2024
  !!   - moved to FORCES
  subroutine calculate_cell_ids(this)
    implicit none

    class(grid_t), intent(inout) :: this

    integer(i4) :: i, j, k

    ! if mask not allocated create one with only .true. values
    if (.not. allocated(this%mask)) then
      allocate(this%mask(this%nx, this%ny))
      this%mask = .true.
    end if

    this%n_cells = count(this%mask)
    allocate(this%cell_ij(this%n_cells, 2))
    allocate(this%id(this%n_cells))
    this%id = [ (k, k = 1, this%n_cells) ]

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

  !> \brief estimate cell area
  !> \details following tasks are performed:
  !!          - estimate cell area based on coordinate system
  !> \authors Rohini Kumar
  !> \date Jan 2013
  !> \changelog
  !! - Rohini Kumar & Matthias Cuntz, May 2014
  !!   - cell area calulation based on a regular lat-lon grid or on a regular X-Y coordinate system
  !! - Matthias Cuntz, May 2014
  !!   - changed empirical distribution function so that doubles get the same value
  !! - Matthias Zink & Matthias Cuntz, Feb 2016
  !!   - code speed up due to reformulation of CDF calculation
  !! - Rohini Kumar, Mar 2016
  !!   - changes for handling multiple soil database options
  !! - Robert Schweppe, Jun 2018
  !!   - refactoring and reformatting
  !! - Sebastian Müller, Mar 2024
  !!   - moved to FORCES
  subroutine estimate_cell_area(this)

    use mo_constants, only : RadiusEarth_dp, TWOPI_dp
    implicit none

    class(grid_t), intent(inout) :: this

    real(dp), dimension(:, :), allocatable :: areaCell_2D
    integer(i4) :: j
    real(dp) :: rdum, degree_to_radian, degree_to_metre

    if (.not. allocated(this%cell_area)) allocate(this%cell_area(this%n_cells))

    ! regular X-Y coordinate system
    if(this%coordsys .eq. coordsys_cart) then
      this%cell_area(:) = this%cellsize * this%cellsize

    ! regular lat-lon coordinate system
    else if(this%coordsys .eq. coordsys_sph_deg) then
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

  !> \brief calculate coarse grid extend
  !> \details Calculates basic grid properties at a required coarser level using
  !!          information of a given finer level.
  !!          Calculates basic grid properties at a required coarser level (e.g., L11) using
  !!          information of a given finer level (e.g., L0). Basic grid properties such as
  !!          nx, ny, xllcorner, yllcorner cellsize are estimated in this routine.
  !> \authors Matthias Zink & Rohini Kumar
  !> \date Feb 2013
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

    real(dp) :: cellFactor, rounded
    integer(i4) :: factor, align_

    align_ = align_ll
    if ( present(aligning) ) align_ = aligning

    call check_factor(cellsize_in, target_resolution, cellFactor, rounded, factor, tol)

    cellsize_out = target_resolution
    ny_out = nint(real(ny_in, dp) / cellFactor)
    nx_out = nint(real(nx_in, dp) / cellFactor)

    ! if we rounded down, but now we would miss cells, add rows and/or cols
    if ( ny_out * factor < ny_in ) ny_out = ny_out + 1_i4
    if ( nx_out * factor < nx_in ) nx_out = nx_out + 1_i4

    ! align grids based on the selected aligning corner
    ! keep yll if aligning in (lower)-left or (lower)-right
    if (align_ == align_ll .or. align_ == align_lr) then
      yllcorner_out = yllcorner_in
    else
      yllcorner_out = yllcorner_in + real(ny_in, dp) * target_resolution / rounded - real(ny_out, dp) * cellsize_out
    endif
    ! keep xll if aligning in lower-(left) or top-(left)
    if (align_ == align_ll .or. align_ == align_ul) then
      xllcorner_out = xllcorner_in
    else
      xllcorner_out = xllcorner_in + real(nx_in, dp) * target_resolution / rounded - real(nx_out, dp) * cellsize_out
    endif

  end subroutine calculate_coarse_extend

  ! ------------------------------------------------------------------

  !> \brief check if given axis is a uniform axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_uniform_axis(var, cellsize, origin, increasing, tol)
    use mo_netcdf, only : NcVariable
    use mo_utils, only: is_close
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable for corresponding axis
    real(dp), optional, intent(out) :: cellsize !< cellsize of the uniform axis
    real(dp), optional, intent(out) :: origin !< origin of the axis vertices
    logical, optional, intent(out) :: increasing !< whether the axis has increasing values
    real(dp), intent(in), optional :: tol !< tolerance for cell size comparisson (default: 1.e-7)
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
    ! store var name for error messages
    name = var%getName()

    tol_ = 1.e-7_dp
    if ( present(tol) ) tol_ = tol

    if (size(axis) == 0_i4) &
      call error_message("check_uniform_axis: axis is empty: ", name)

    if (size(axis) > 1_i4) then
      diff = (axis(size(axis)) - axis(1)) / real(size(axis) - 1_i4, dp)
      if (.not.all(is_close(axis(2:size(axis))-axis(1:size(axis)-1), diff, rtol=0.0_dp, atol=tol_))) &
        call error_message("check_uniform_axis: given axis is not uniform: ", name)
    else
      if (.not. has_bnds) &
        call error_message("check_uniform_axis: can't check axis of size 1 when no bounds are given: ", name)
      diff = bounds(2,1) - bounds(1,1)
    end if

    if (has_bnds) then
      ! be forgiving if the bounds don't have the same monotonicity as the axis (cf-convetion is hard)
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
        call error_message("check_uniform_axis: given bounds are not uniform: ", name)
      if (.not.all(is_close(axis(:)-bounds(i_lb,:), 0.5_dp*diff, rtol=0.0_dp, atol=tol_))) &
        call error_message("check_uniform_axis: given bounds are not centered around axis points: ", name)
    end if

    if ( present(cellsize) ) cellsize = abs(diff)
    if ( present(origin) ) origin = minval(axis) - 0.5_dp * abs(diff)
    if ( present(increasing) ) increasing = diff > 0.0_dp

  end subroutine check_uniform_axis

  !> \brief check if given variable is a x-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_x_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
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

  !> \brief check if given variable is a y-axis.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_y_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
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

  !> \brief check if given variable is a lon coordinate.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_lon_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
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

  !> \brief check if given variable is a lat coordinate.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_lat_coord(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
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

  !> \brief calculate and check cell-size factor for validity.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_factor(fine_cellsize, coarse_cellsize, cellfactor, rounded, factor, tol)
    real(dp), intent(in) :: fine_cellsize !< cellsize of fine grid
    real(dp), intent(in) :: coarse_cellsize !< cellsize of coarse grid
    real(dp), optional, intent(out) :: cellfactor !< raw real cellfactor
    real(dp), optional, intent(out) :: rounded !< rounded real cellfactor
    integer(i4), optional, intent(out) :: factor !< integer cellfactor
    real(dp), intent(in), optional :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: cellfactor_, rounded_, tol_
    integer(i4) :: factor_

    tol_ = 1.e-7_dp
    if ( present(tol) ) tol_ = tol

    cellfactor_ = coarse_cellsize / fine_cellsize
    rounded_ = anint(cellfactor_)
    factor_ = nint(cellfactor_)

    if (abs(rounded_ - cellfactor_) > tol_) then
      call error_message( &
        'check_factor: Two resolutions size do not confirm: ', &
        trim(adjustl(num2str(nint(coarse_cellsize)))), &
        trim(adjustl(num2str(nint(fine_cellsize)))))
    end if
    if (factor_ < 1_i4) call error_message("check_factor: cell factor needs to be >= 1 to setup an upscaler.")
    if ( present(cellfactor) ) cellfactor = cellfactor_
    if ( present(rounded) ) rounded = rounded_
    if ( present(factor) ) factor = factor_
  end subroutine check_factor

  ! ------------------------------------------------------------------

  !> \brief Read spatial data.
  !> \details Read spatial data from ascii file. Data will be transposed to be in xy order.
  !> \authors Robert Schweppe
  !> \date Jun 2018
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

  !> \brief Read spatial data.
  !> \details Read spatial data from ascii file. Data will be transposed to be in xy order.
  !> \authors Robert Schweppe
  !> \date Jun 2018
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

  !> \brief Reads header lines of ASCII files.
  !> \details Reads header lines of ASCII files, e.g. dem, aspect, flow direction.
  !> \authors Juliane Mai
  !> \date Jan 2013
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
    real(dp), intent(out), optional :: nodata !< nodata value (default -9999.0)

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
    if (present(nodata)) then
      read (fileunit, *, iostat=io) dummy, nodata
      ! EOF reached (nodata not present, use default value)
      if (io < 0) nodata = nodata_dp
    end if
    close(fileunit)
  end subroutine read_header_ascii

end module mo_grid
