!> \file    mo_grid.f90
!> \copydoc mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!!          In contrast to ascii grids, data will be assumed to follow xy axis order and increasing axis.
!!          Ascii grids actually represent data with a decreasing y-axis and in yx order.
!! \par Examples
!! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
!!   \include 01_regridding.f90
!! - \ref 02_nc_output.f90 : \copybrief 02_nc_output.f90
!! - \ref 03_nc_regridder.f90 : \copybrief 03_nc_regridder.f90
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_kind, only: i4, i8, dp, sp
  use mo_utils, only: flip
  use mo_message, only : error_message, warn_message
  use mo_string_utils, only : num2str

  implicit none

  public :: write_ascii_grid
  public :: read_ascii_grid
  public :: read_ascii_header
  public :: id_bounds
  public :: dist_latlon
#ifdef FORCES_WITH_NETCDF
  public :: is_x_axis
  public :: is_y_axis
  public :: is_z_axis
  public :: is_t_axis
  public :: is_lon_coord
  public :: is_lat_coord
  public :: check_uniform_axis
#endif

  private

  !> \name Coordinate System Selectors
  !> \brief Constants to specify the coordinate system in the \ref grid_t.
  !!@{
  integer(i4), public, parameter :: cartesian = 0_i4 !<    Cartesian coordinate system.
  integer(i4), public, parameter :: spherical = 1_i4 !< Spherical coordinates in degrees.
  !!@}

  !> \name Y-Axis Direction Selectors
  !> \brief Constants to specify the y-axis direction in the \ref grid_t.
  !!@{
  integer(i4), public, parameter :: keep_y = -1_i4 !< keep y-axis direction.
  integer(i4), public, parameter :: top_down = 0_i4 !< y-axis with decreasing values.
  integer(i4), public, parameter :: bottom_up = 1_i4 !< y-axis with increasing values.
  !!@}

  !> \name Lower-Left Corner Alignment Selectors
  !> \brief Constants to the alignment of the lower-left corner of grids.
  !!@{
  integer(i4), public, parameter :: lower_left = 0_i4 !< align in lower left corner
  integer(i4), public, parameter :: lower_right = 1_i4 !< align in lower right corner
  integer(i4), public, parameter :: upper_left = 2_i4 !< align in upper left corner
  integer(i4), public, parameter :: upper_right = 3_i4 !< align in upper right corner
  !!@}

  !> \class   grid_t
  !> \brief   2D grid description with data in xy order..
  !> \details This type represents uniform grids with data in xy order with strictly increasing or decreasing y-axis.
  !!          ASCII grid files have the opposite behavior: yx order, with decreasing y-axis.
  !!          NetCDF files natively have yx order, but since Fortran arrays are column-major order,
  !!          the data read from .nc files is in xy order. The x-axis will always be increasing.
  !! \par Examples
  !! - \ref 01_regridding.f90 : \copybrief 01_regridding.f90
  type, public :: grid_t
    integer(i4) :: coordsys = cartesian !< Coordinate system for x and y. 0 -> Cartesian (default), 1 -> Spherical
    integer(i4) :: y_direction = top_down !< y-axis direction (either top_down (0, default) or bottom_up (1))
    ! general domain information
    integer(i4) :: nx        !< size of x-axis (number of cols in ascii grid file)
    integer(i4) :: ny        !< size of y-axis (number of rows in ascii grid file)
    integer(i8) :: ncells   !< number of cells in mask
    real(dp) :: xllcorner    !< x coordinate of the lowerleft corner
    real(dp) :: yllcorner    !< y coordinate of the lowerleft corner
    real(dp) :: cellsize     !< cellsize x = cellsize y
    real(dp), dimension(:), allocatable :: cell_area !< area of the cell in sqare m, size (ncells)
    logical, dimension(:, :), allocatable :: mask    !< the mask for valid cells in the original grid, size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat    !< 2d longitude array (auxiliary coordinate for X axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lon    !< 2d latitude  array (auxiliary coordinate for Y axis), size (nx, ny)
    real(dp), dimension(:, :), allocatable :: lat_vertices  !< latitude coordinates or the grid nodes, size (nx+1, ny+1)
    real(dp), dimension(:, :), allocatable :: lon_vertices  !< longitude coordinates or the grid nodes, size (nx+1, ny+1)
    integer(i4), dimension(:, :), allocatable :: cell_ij    !< matrix IDs (i, j) per cell in mask, size (ncells, 2)
  contains
    procedure, public :: init => grid_init
    procedure, public :: from_ascii_file
    procedure, public :: to_ascii_file
#ifdef FORCES_WITH_NETCDF
    procedure, private :: from_nc_dataset, from_nc_file
    generic, public :: from_netcdf => from_nc_dataset, from_nc_file
    procedure, private :: aux_from_nc_dataset, aux_from_nc_file
    generic, public :: aux_from_netcdf => aux_from_nc_dataset, aux_from_nc_file
    procedure, private :: to_nc_dataset, to_nc_file
    generic, public :: to_netcdf => to_nc_dataset, to_nc_file
#endif
    procedure, public :: extent
    procedure, public :: total_area
    procedure, public :: id_matrix
    procedure, public :: cell_id
    procedure, public :: closest_cell_id
    procedure, public :: x_axis
    procedure, public :: y_axis
    procedure, public :: x_vertices
    procedure, public :: y_vertices
    procedure, public :: x_bounds
    procedure, public :: y_bounds
    procedure, public :: upscale_aux_coords
    procedure, public :: downscale_aux_coords
    procedure, public :: estimate_aux_vertices
    procedure, public :: lat_bounds
    procedure, public :: lon_bounds
    procedure, public :: has_mask
    procedure, public :: any_missing
    procedure, public :: check_is_covered_by
    procedure, public :: check_is_covering
    procedure, public :: check_is_filled_by
    procedure, public :: check_is_filling
    procedure, public :: has_aux_coords
    procedure, public :: has_aux_vertices
    procedure, public :: calculate_cell_ids
    procedure, public :: calculate_cell_area
    procedure, public :: is_periodic
    procedure, public :: derive_coarse_grid
    procedure, public :: derive_fine_grid
    procedure, public :: derive_grid
    procedure, private :: read_data_dp, read_data_i4
    generic, public :: read_data => read_data_dp, read_data_i4
    procedure, private :: check_shape => grid_check_shape
    procedure, private :: check_shape_packed => grid_check_shape_packed
    procedure, private :: pack_data_sp, pack_data_dp, pack_data_i4, pack_data_i8, pack_data_lgt
    generic, public :: pack => pack_data_sp, pack_data_dp, pack_data_i4, pack_data_i8, pack_data_lgt
    procedure, private :: unpack_data_sp, unpack_data_dp, unpack_data_i4, unpack_data_i8, unpack_data_lgt
    generic, public :: unpack => unpack_data_sp, unpack_data_dp, unpack_data_i4, unpack_data_i8, unpack_data_lgt
  end type grid_t

  !> \class   layered_grid_t
  !> \brief   3D layered grid description with layer data in xy order.
  !> \details This type represents uniform layered grids with layer data in xy order with increasing x-axis and monotonic y-axis.
  !!          The z-axis is described by a monotonic layers array.
  !!          NetCDF files nativly have zyx order, but since Fortran arrays are column-major order,
  !!          the data read from .nc files is in xyz order.
  type, public :: layered_grid_t
    type(grid_t) :: grid !< 2D grid used for each layer
    logical :: positive_up = .false. !< indicated "upwards" as direction of positive z values
    real(dp), dimension(:), allocatable :: layer  !< layer given by reference point in bounds (see vertices)
    real(dp), dimension(:), allocatable :: layer_vertices  !< layer bounds
  contains
    procedure, public :: init => layer_init
#ifdef FORCES_WITH_NETCDF
    procedure, private :: layer_from_nc_dataset, layer_from_nc_file
    generic, public :: from_netcdf => layer_from_nc_dataset, layer_from_nc_file
    procedure, private :: layer_to_nc_dataset, layer_to_nc_file
    generic, public :: to_netcdf => layer_to_nc_dataset, layer_to_nc_file
#endif
    procedure, public :: z_bounds => layer_z_bounds
    procedure, public :: check_is_covered_by => layer_check_is_covered_by
    procedure, public :: check_is_covering => layer_check_is_covering
    procedure, public :: check_is_filled_by => layer_check_is_filled_by
    procedure, public :: check_is_filling => layer_check_is_filling
    procedure, private :: check_shape => layer_check_shape
    procedure, private :: check_shape_packed => layer_check_shape_packed
    procedure, private :: layer_pack_sp, layer_pack_dp, layer_pack_i4, layer_pack_i8, layer_pack_lgt
    generic, public :: pack => layer_pack_sp, layer_pack_dp, layer_pack_i4, layer_pack_i8, layer_pack_lgt
    procedure, private :: layer_unpack_sp, layer_unpack_dp, layer_unpack_i4, layer_unpack_i8, layer_unpack_lgt
    generic, public :: unpack => layer_unpack_sp, layer_unpack_dp, layer_unpack_i4, layer_unpack_i8, layer_unpack_lgt
  end type layered_grid_t

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
  interface read_ascii_grid
    module procedure read_ascii_grid_i4, read_ascii_grid_dp
  end interface read_ascii_grid

  !> \brief Write spatial data.
  !> \details Write spatial data to ascii file. Data will be transposed to be in xy order.
  !> \authors Sebastian Müller
  !> \date Mar 2025
  interface write_ascii_grid
    module procedure write_ascii_grid_i4, write_ascii_grid_dp
  end interface write_ascii_grid

contains

  ! ------------------------------------------------------------------

  !> \brief initialize layered grid
  !> \details initialize grid from standard ascii header content (nx (cols), ny (rows), cellsize, lower-left corner)
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_init(this, grid, layer, layer_vertices, positive_up)
    implicit none
    class(layered_grid_t), intent(inout) :: this
    type(grid_t), intent(in) :: grid !< 2D grid used for each layer
    real(dp), dimension(:), intent(in) :: layer  !< layer given by reference point in bounds (see vertices)
    real(dp), dimension(:), intent(in) :: layer_vertices  !< layer bounds
    logical, optional, intent(in) :: positive_up !< indicated "upwards" as direction of positive z values (.false. by default)
    integer(i4) :: i
    real(dp) :: minl, maxl
    real(dp), dimension(:), allocatable :: diffs
    if (size(layer) == 0) call error_message("layered_grid % init: need at least one layer.")
    if (size(layer) + 1 /= size(layer_vertices)) call error_message("layered_grid % init: size of layer and vertices not matching.")
    do i = 1, size(layer)
      minl = min(layer_vertices(i), layer_vertices(i+1))
      maxl = max(layer_vertices(i), layer_vertices(i+1))
      if (layer(i)<minl .or. layer(i)>maxl) call error_message("layered_grid % init: layers not within bounds form vertices.")
    end do
    diffs = layer_vertices(2:) - layer_vertices(:size(layer))
    if (.not.(all(diffs > 0.0_dp).or.all(diffs < 0.0_dp))) call error_message("layered_grid % init: layers not monotonous.")
    this%grid = grid
    this%layer = layer
    this%layer_vertices = layer_vertices
    if (present(positive_up)) this%positive_up = positive_up
  end subroutine layer_init

#ifdef FORCES_WITH_NETCDF

  !> \brief initialize grid from a netcdf file
  !> \details initialize grid from a netcdf file and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_from_nc_file(this, path, var, read_mask, read_aux, tol, y_direction)
    use mo_netcdf, only : NcDataset
    implicit none
    class(layered_grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%layer_from_nc_dataset(nc, var, read_mask, read_aux, tol, y_direction)
    call nc%close()
  end subroutine layer_from_nc_file

  !> \brief initialize grid from a netcdf dataset
  !> \details initialize grid from a netcdf dataset and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_from_nc_dataset(this, nc, var, read_mask, read_aux, tol, y_direction)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(layered_grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)

    type(NcVariable) :: ncvar, zvar, bnds
    type(NcDimension), dimension(:), allocatable :: dims
    real(dp), dimension(:,:), allocatable :: bounds
    character(len=256) :: name
    integer(i4) :: nlayer, rnk

    ncvar = nc%getVariable(var)
    rnk = ncvar%getRank()
    if (rnk < 3) call error_message("grid % from_netcdf: given variable has too few dimensions: ", trim(nc%fname), ":", var)
    dims = ncvar%getDimensions()
    zvar = nc%getVariable(trim(dims(3)%getName()))

    if (.not.(is_z_axis(zvar))) &
      call error_message("grid % from_netcdf: can't interpret z-axis: ", trim(nc%fname), ":", var)

    call zvar%getData(this%layer)
    nlayer = size(this%layer)
    allocate(this%layer_vertices(nlayer+1))

    if (zvar%hasAttribute("bounds")) then
      call zvar%getAttribute("bounds", name)
      bnds = nc%getVariable(trim(name))
      call bnds%getData(bounds)
      this%layer_vertices(:nlayer) = bounds(1,:)
      this%layer_vertices(nlayer+1) = bounds(2,nlayer)
      deallocate(bounds)
    else
      ! by default, bounds with reference point first
      this%layer_vertices(:nlayer) = this%layer
      if (nlayer == 1) then
        ! default layer thickness of 1.0
        this%layer_vertices(2) = this%layer(1) + 1.0_dp
      else
        ! last layer thickness from previous layer thickness
        this%layer_vertices(nlayer + 1) = 2 * this%layer(nlayer) - this%layer(nlayer - 1)
      end if
    end if

    ! set positive direction (down by default for horizons)
    if (zvar%hasAttribute("positive")) then
      call zvar%getAttribute("positive", name)
      if (trim(name) == "up") then
        this%positive_up = .true.
      else
        this%positive_up = .false.
      end if
    end if

    ! setup horizontal grid
    call this%grid%from_netcdf(nc, var, read_mask, read_aux, tol, y_direction)

  end subroutine layer_from_nc_dataset

  !> \brief write grid to a netcdf file
  !> \details write grid to a netcdf file with possible data variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_to_nc_file(this, path, x_name, y_name, z_name, aux_lon_name, aux_lat_name, double_precision, append)
    use mo_netcdf, only : NcDataset
    implicit none
    class(layered_grid_t), intent(in) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension
    character(*), optional, intent(in) :: z_name !< name for z-axis variable and dimension
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    logical, optional, intent(in) :: append !< whether netcdf file should be opened in append mode (default .false.)
    type(NcDataset) :: nc
    character(1) :: fmode
    fmode = "w"
    if ( present(append) ) then
      if (append) fmode = "a"
    end if
    nc = NcDataset(path, fmode)
    call this%layer_to_nc_dataset(nc, x_name, y_name, z_name, aux_lon_name, aux_lat_name, double_precision)
    call nc%close()
  end subroutine layer_to_nc_file

  !> \brief initialize grid from a netcdf dataset
  !> \details initialize grid from a netcdf dataset and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_to_nc_dataset(this, nc, x_name, y_name, z_name, aux_lon_name, aux_lat_name, double_precision)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(layered_grid_t), intent(in) :: this
    type(NcDataset), intent(inout) :: nc !< NetCDF Dataset
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension
    character(*), optional, intent(in) :: z_name !< name for z-axis variable and dimension
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    type(NcDimension) :: z_dim, b_dim
    type(NcVariable) :: z_var, zb_var
    character(:), allocatable :: zname
    logical :: double_precision_
    character(3) :: dtype

    call this%grid%to_netcdf(nc, x_name, y_name, aux_lon_name, aux_lat_name, double_precision)

    double_precision_ = .true.
    if (present(double_precision)) double_precision_ = double_precision
    dtype = "f32"
    if ( double_precision_ ) dtype = "f64"
    zname = "z"
    if (present(z_name)) zname = z_name

    z_dim = nc%setDimension(zname, size(this%layer))
    b_dim = nc%getDimension("bnds")
    z_var = nc%setVariable(zname, dtype, [z_dim])
    zb_var = nc%setVariable(zname // "_bnds", dtype, [b_dim, z_dim])
    call z_var%setAttribute("axis", "Z")
    call z_var%setAttribute("bounds", zname // "_bnds")
    if (this%positive_up) then
      call z_var%setAttribute("standard_name", "height")
      call z_var%setAttribute("positive", "up")
    else
      call z_var%setAttribute("standard_name", "depth")
      call z_var%setAttribute("positive", "down")
    end if
    if (double_precision_) then
      call z_var%setData(this%layer)
      call zb_var%setData(this%z_bounds())
    else
      call z_var%setData(real(this%layer, sp))
      call zb_var%setData(real(this%z_bounds(), sp))
    end if
  end subroutine layer_to_nc_dataset

#endif

  !> \brief z-bounds of the grid cell following cf-conventions (2, nz).
  !> \return `real(dp), allocatable, dimension(:,:) :: z_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function layer_z_bounds(this) result(z_bounds)
    implicit none
    class(layered_grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:) :: z_bounds
    allocate(z_bounds(2, size(this%layer)))
    z_bounds(1,:) = this%layer_vertices(:size(this%layer))
    z_bounds(2,:) = this%layer_vertices(2:)
  end function layer_z_bounds

  !> \brief check if given grid is covered by coarser grid
  !> \details check if given grid is compatible and covered by coarser grid and raise an error if this is not the case.
  !! Layers of coarse grid need to fully cover the layers of given grid.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_check_is_covered_by(this, coarse_grid, tol, check_mask)
    use mo_utils, only: eq
    implicit none
    class(layered_grid_t), intent(in) :: this
    type(layered_grid_t), intent(in) :: coarse_grid !< coarse grid that should cover this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask
    if (minval(this%layer_vertices) < minval(coarse_grid%layer_vertices)) &
      call error_message("layered_grid % check_is_covered_by: coarse layers not covering.")
    if (maxval(this%layer_vertices) > maxval(coarse_grid%layer_vertices)) &
      call error_message("layered_grid % check_is_covered_by: coarse layers not covering.")
    call this%grid%check_is_covered_by(coarse_grid%grid, tol, check_mask)
  end subroutine layer_check_is_covered_by

  !> \brief check if given grid is covering finer grid
  !> \details check if given grid is compatible with and covering finer grid and raise an error if this is not the case.
  !! Layers of coarse grid need to fully cover the layers of given grid.
  !! \note The coarse grid is allowed to have valid cells outside of the fine grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_check_is_covering(this, fine_grid, tol, check_mask)
    implicit none
    class(layered_grid_t), intent(in) :: this
    type(layered_grid_t), intent(in) :: fine_grid !< finer grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if coarse mask covers fine mask
    call fine_grid%check_is_covered_by(coarse_grid=this, tol=tol, check_mask=check_mask)
  end subroutine layer_check_is_covering

  !> \brief check if given grid is filled by fine grid
  !> \details check if given grid is compatible and filled by finer grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_check_is_filled_by(this, fine_grid, tol, check_mask)
    use mo_orderpack, only: sort
    use mo_utils, only: eq
    implicit none
    class(layered_grid_t), intent(in) :: this
    type(layered_grid_t), intent(in) :: fine_grid !< fine grid that should fill this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask
    real(dp), dimension(:), allocatable :: vertices
    vertices = this%layer_vertices
    call sort(vertices)
    ! check on both ends if first and last layer are touched by fine grids layers
    if (vertices(2) < minval(fine_grid%layer_vertices)) &
      call error_message("layered_grid % check_is_filled_by: some coarse layers not filled.")
    if (vertices(size(vertices)-1) > maxval(fine_grid%layer_vertices)) &
      call error_message("layered_grid % check_is_filled_by: some coarse layers not filled.")
    call this%grid%check_is_filled_by(fine_grid%grid, tol, check_mask)
    deallocate(vertices)
  end subroutine layer_check_is_filled_by

  !> \brief check if given grid is filling coarser grid
  !> \details check if given grid is compatible with and filling coarser grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine layer_check_is_filling(this, coarse_grid, tol, check_mask)
    implicit none
    class(layered_grid_t), intent(in) :: this
    type(layered_grid_t), intent(in) :: coarse_grid !< coarser grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask
    call coarse_grid%check_is_filled_by(fine_grid=this, tol=tol, check_mask=check_mask)
  end subroutine layer_check_is_filling

  !> \brief Check 3D data shape for layered grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine layer_check_shape(this, data_shape)
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i8), intent(in) :: data_shape(3) !< (x,y,z)
    if (data_shape(1) /= this%grid%nx .or. data_shape(2) /= this%grid%ny .or. data_shape(3) /= size(this%layer)) then
      call error_message( &
        "layered data: data has wrong shape. Expected: (", &
        num2str(this%grid%nx), ",", num2str(this%grid%ny), ",", num2str(size(this%layer)), "), got: (", &
        num2str(data_shape(1)), ",", num2str(data_shape(2)), ",", num2str(data_shape(3)), ")")
    end if
  end subroutine layer_check_shape

  !> \brief Check 2D packed data shape for layered grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine layer_check_shape_packed(this, data_shape)
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i8), intent(in) :: data_shape(2) !< (n,z)
    if (data_shape(1) /= this%grid%ncells .or. data_shape(2) /= size(this%layer)) then
      call error_message( &
        "layered data: packed data has wrong shape. Expected: (", &
        num2str(this%grid%ncells), ",", num2str(size(this%layer)), "), got: (", &
        num2str(data_shape(1)), ",", num2str(data_shape(2)), ")")
    end if
  end subroutine layer_check_shape_packed

  !> \brief Pack 3D data with grid mask
  !> \return `real(sp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_pack_sp(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:,:,:) !< (x,y,z)
    real(sp), allocatable :: out_data(:,:) !< (n,z)
    integer(i4) :: i
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%grid%ncells, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,i) = pack(data(:,:,i), this%grid%mask)
    end do
  end function layer_pack_sp

  !> \brief Pack 3D data with grid mask
  !> \return `real(dp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_pack_dp(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:,:,:) !< (x,y,z)
    real(dp), allocatable :: out_data(:,:) !< (n,z)
    integer(i4) :: i
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%grid%ncells, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,i) = pack(data(:,:,i), this%grid%mask)
    end do
  end function layer_pack_dp

  !> \brief Pack 3D data with grid mask
  !> \return `integer(i4) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_pack_i4(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:,:,:) !< (x,y,z)
    integer(i4), allocatable :: out_data(:,:) !< (n,z)
    integer(i4) :: i
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%grid%ncells, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,i) = pack(data(:,:,i), this%grid%mask)
    end do
  end function layer_pack_i4

  !> \brief Pack 3D data with grid mask
  !> \return `integer(i8) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_pack_i8(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:,:,:) !< (x,y,z)
    integer(i8), allocatable :: out_data(:,:) !< (n,z)
    integer(i4) :: i
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%grid%ncells, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,i) = pack(data(:,:,i), this%grid%mask)
    end do
  end function layer_pack_i8

  !> \brief Pack 3D data with grid mask
  !> \return `logical :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_pack_lgt(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    logical, intent(in) :: data(:,:,:) !< (x,y,z)
    logical, allocatable :: out_data(:,:) !< (n,z)
    integer(i4) :: i
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%grid%ncells, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,i) = pack(data(:,:,i), this%grid%mask)
    end do
  end function layer_pack_lgt

  !> \brief Unpack 2D data with grid mask
  !> \return `real(sp) :: out_data(:,:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_unpack_sp(this, data) result(out_data)
    use mo_constants, only : nodata_sp
    implicit none
    class(layered_grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:,:)
    real(sp), allocatable :: out_data(:,:,:)
    integer(i4) :: i
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%grid%nx, this%grid%ny, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,:,i) = unpack(data(:,i), this%grid%mask, nodata_sp)
    end do
  end function layer_unpack_sp

  !> \brief Unpack 2D data with grid mask
  !> \return `real(dp) :: out_data(:,:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_unpack_dp(this, data) result(out_data)
    use mo_constants, only : nodata_dp
    implicit none
    class(layered_grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:,:)
    real(dp), allocatable :: out_data(:,:,:)
    integer(i4) :: i
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%grid%nx, this%grid%ny, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,:,i) = unpack(data(:,i), this%grid%mask, nodata_dp)
    end do
  end function layer_unpack_dp

  !> \brief Unpack 2D data with grid mask
  !> \return `integer(i4) :: out_data(:,:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_unpack_i4(this, data) result(out_data)
    use mo_constants, only : nodata_i4
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:,:)
    integer(i4), allocatable :: out_data(:,:,:)
    integer(i4) :: i
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%grid%nx, this%grid%ny, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,:,i) = unpack(data(:,i), this%grid%mask, nodata_i4)
    end do
  end function layer_unpack_i4

  !> \brief Unpack 2D data with grid mask
  !> \return `integer(i8) :: out_data(:,:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_unpack_i8(this, data) result(out_data)
    use mo_constants, only : nodata_i8
    implicit none
    class(layered_grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:,:)
    integer(i8), allocatable :: out_data(:,:,:)
    integer(i4) :: i
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%grid%nx, this%grid%ny, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,:,i) = unpack(data(:,i), this%grid%mask, nodata_i8)
    end do
  end function layer_unpack_i8

  !> \brief Unpack 2D data with grid mask
  !> \return `logical :: out_data(:,:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function layer_unpack_lgt(this, data) result(out_data)
    implicit none
    class(layered_grid_t), intent(in) :: this
    logical, intent(in) :: data(:,:)
    logical, allocatable :: out_data(:,:,:)
    integer(i4) :: i
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%grid%nx, this%grid%ny, size(this%layer)))
    do i = 1, size(this%layer)
      out_data(:,:,i) = unpack(data(:,i), this%grid%mask, .false.)
    end do
  end function layer_unpack_lgt

  ! ------------------------------------------------------------------

  !> \brief initialize grid from ascii header content
  !> \details initialize grid from standard ascii header content (nx (cols), ny (rows), cellsize, lower-left corner)
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine grid_init(this, nx, ny, xllcorner, yllcorner, cellsize, coordsys, mask, y_direction)
    implicit none
    class(grid_t), intent(inout) :: this
    integer(i4), intent(in) :: nx !< Number of x-axis subdivisions
    integer(i4), intent(in) :: ny !< Number of y-axis subdivisions
    real(dp), optional, intent(in) :: xllcorner !< lower left corner (x) (default 0.0)
    real(dp), optional, intent(in) :: yllcorner !< lower left corner (y) (default 0.0)
    real(dp), optional, intent(in) :: cellsize !< cell size [m] or [deg] (default 1.0)
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (0 (default) for cartesian, 1 for spherical)
    logical, dimension(:,:), optional, intent(in) :: mask !< desired mask for the grid (default: all .true.)
    integer(i4), optional, intent(in) :: y_direction !< y-axis direction (0 (default) for top-down, 1 for bottom-up)

    this%nx = nx
    this%ny = ny
    this%xllcorner = 0.0_dp
    if ( present(xllcorner) ) this%xllcorner = xllcorner
    this%yllcorner = 0.0_dp
    if ( present(yllcorner) ) this%yllcorner = yllcorner
    this%cellsize = 1.0_dp
    if ( present(cellsize) ) this%cellsize = cellsize
    ! check if coordsys is supported
    this%coordsys = cartesian
    if ( present(coordsys) ) then
      if (coordsys /= cartesian .and. coordsys /= spherical) &
        call error_message("grid % init: unknown coordsys value: ", num2str(coordsys))
      this%coordsys = coordsys
    end if
    ! check if y-direction is supported
    this%y_direction = top_down
    if ( present(y_direction) ) then
      if (.not.any(y_direction==[keep_y, bottom_up, top_down])) &
        call error_message("grid % init: unknown y_direction value: ", num2str(y_direction))
      if (y_direction /= keep_y) this%y_direction = y_direction
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
    call this%calculate_cell_area()

  end subroutine grid_init

  !> \brief initialize grid from ascii grid file
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_ascii_file(this, path, coordsys, read_mask, y_direction)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    integer(i4), optional, intent(in) :: coordsys !< desired coordinate system (0 (default) for cartesian, 1 for lat-lon)
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given file (default: .true.)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) or 0 for top-down, 1 for bottom-up)

    integer(i4) :: nx, ny
    real(dp) :: xll, yll, cellsize
    real(dp), allocatable, dimension(:,:) :: dummy
    logical, allocatable, dimension(:,:) :: mask
    logical :: read_mask_
    integer(i4) :: y_dir

    read_mask_ = .true.
    if (present(read_mask)) read_mask_ = read_mask
    y_dir = keep_y
    if (present(y_direction)) y_dir = y_direction
    if (y_dir == keep_y) y_dir = top_down
    if (.not.any(y_dir==[bottom_up, top_down])) &
      call error_message("grid % from_ascii_file: y-direction not valid: ", trim(num2str(y_dir)))

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_header(path, nx ,ny, xll, yll, cellsize)

    if (read_mask_) then
      call read_ascii_grid_dp(path, dummy, mask, y_direction=y_dir)
      deallocate(dummy)
    else
      allocate(mask(nx, ny))
      mask(:,:) = .true.
    end if

    call this%init(nx, ny, xll, yll, cellsize, coordsys, mask, y_dir)
    deallocate(mask)

  end subroutine from_ascii_file

  !> \brief write grid to ascii grid file
  !> \details Writes the grid information to an ascii grid. If mask should be written, it will be stored as 1/nodata map.
  !!          If no mask should be written, only the header is stored.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine to_ascii_file(this, path, write_mask)
    use mo_constants, only: nodata_i4
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    logical, optional, intent(in) :: write_mask !< Whether to write the mask to the given file (default: .true.)

    logical :: write_mask_
    integer(i4), allocatable, dimension(:,:) :: dummy

    write_mask_ = .true.
    if ( present(write_mask) ) write_mask_ = write_mask

    if (write_mask_) then
      allocate(dummy(this%nx, this%ny), source=nodata_i4)
      where (this%mask) dummy = 1_i4
    end if
    call write_ascii_grid_i4( &
      path=path, &
      ncols=this%nx, &
      nrows=this%ny, &
      xllcorner=this%xllcorner, &
      yllcorner=this%yllcorner, &
      cellsize=this%cellsize, &
      nodata=nodata_i4, &
      data=dummy, &  ! unallocated dummy will result in present(data) == .false.
      y_direction=this%y_direction)
    if (allocated(dummy)) deallocate(dummy)
  end subroutine to_ascii_file

  !> \brief Read data from ascii file conforming this grid
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine read_data_dp(this, path, data)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    real(dp), intent(out), allocatable, dimension(:,:) :: data

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_grid_dp(path, data, &
      ref_ncols=this%nx, &
      ref_nrows=this%ny, &
      ref_xllcorner=this%xllcorner, &
      ref_yllcorner=this%yllcorner, &
      ref_cellsize=this%cellsize, &
      y_direction=this%y_direction)
  end subroutine read_data_dp

  !> \brief Read data from ascii file conforming this grid
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine read_data_i4(this, path, data)
    use mo_os, only: check_path_isfile
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< path to the ascii grid file
    integer(i4), intent(out), allocatable, dimension(:,:) :: data

    call check_path_isfile(path=path, raise=.true.)
    call read_ascii_grid_i4(path, data, &
      ref_ncols=this%nx, &
      ref_nrows=this%ny, &
      ref_xllcorner=this%xllcorner, &
      ref_yllcorner=this%yllcorner, &
      ref_cellsize=this%cellsize, &
      y_direction=this%y_direction)
  end subroutine read_data_i4

#ifdef FORCES_WITH_NETCDF

  !> \brief initialize grid from a netcdf file
  !> \details initialize grid from a netcdf file and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_nc_file(this, path, var, read_mask, read_aux, tol, y_direction)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), intent(in) :: var !< nc variable name to determine the grid from
    logical, optional, intent(in) :: read_mask !< Whether to read the mask from the given variable (default: .true.)
    logical, optional, intent(in) :: read_aux !< Whether to read auxilliar coordinates if possible (default: .true.)
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)
    type(NcDataset) :: nc
    nc = NcDataset(path, "r")
    call this%from_nc_dataset(nc, var, read_mask, read_aux, tol, y_direction)
    call nc%close()
  end subroutine from_nc_file

  !> \brief initialize grid from a netcdf dataset
  !> \details initialize grid from a netcdf dataset and a reference variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine from_nc_dataset(this, nc, var, read_mask, read_aux, tol, y_direction)
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
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) as present, 0 for top-down, 1 for bottom-up)

    type(NcVariable) :: ncvar, xvar, yvar
    type(NcDimension), dimension(:), allocatable :: dims

    integer(i4), dimension(:), allocatable :: shp, start, cnt
    integer(i4) :: nx, ny, rnk, coordsys, y_dir, i
    real(dp) :: xll, yll, cellsize, cs_x, cs_y, tol_
    real(dp), allocatable, dimension(:,:) :: dummy
    character(len=256) :: tmp_str
    character(len=256), allocatable, dimension(:) :: coords_str
    character(:), allocatable :: lat_name, lon_name
    logical, allocatable, dimension(:,:) :: mask
    logical :: y_inc, read_mask_, read_aux_, x_sph, y_sph, x_cart, y_cart, flip_y, found_lat, found_lon

    y_dir = keep_y
    if (present(y_direction)) y_dir = y_direction

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

    coordsys = cartesian
    if (x_sph) coordsys = spherical

    ! check axis uniformity and monotonicity
    call check_uniform_axis(xvar, cellsize=cs_x, origin=xll, tol=tol)
    call check_uniform_axis(yvar, cellsize=cs_y, origin=yll, increasing=y_inc, tol=tol)
    if (y_dir == keep_y) then
      y_dir = top_down
      if (y_inc) y_dir = bottom_up
    end if
    ! check y_dir
    if (.not.any(y_dir==[bottom_up, top_down])) &
      call error_message("grid % from_netcdf: y-direction not valid: ", trim(num2str(y_dir)))

    ! warn about flipping if present axis is not in desired direction
    flip_y = y_inc.neqv.(y_dir==bottom_up)
    if (flip_y) then
      call warn_message("grid % from_netcdf: y axis direction is oposite to desired one (inefficient flipping). ", &
                        "You could flip the file beforehand with: 'cdo invertlat <ifile> <ofile>'. ", trim(nc%fname), ":", var)
    end if
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
      if (flip_y) call flip(mask, iDim=2)
      deallocate(dummy)
    else
      allocate(mask(nx, ny))
      mask(:,:) = .true.
    end if

    call this%init(nx, ny, xll, yll, cellsize, coordsys, mask, y_dir)
    deallocate(mask)

    if (read_aux_ .and. coordsys == cartesian .and. ncvar%hasAttribute("coordinates")) then
      call ncvar%getAttribute("coordinates", tmp_str)
      coords_str = splitString(trim(tmp_str), " ")
      ! search for lat-lon variables in given coordinates
      found_lat = .false.
      found_lon = .false.
      do i = 1_i4, size(coords_str)
        ncvar = nc%getVariable(trim(coords_str(i)))
        if (.not.found_lat) then
          if (is_lat_coord(ncvar)) then
            found_lat = .true.
            lat_name = trim(coords_str(i))
          end if
        end if
        if (.not.found_lon) then
          if (is_lon_coord(ncvar)) then
            found_lon = .true.
            lon_name = trim(coords_str(i))
          end if
        end if
      end do
      if (.not.(found_lat.and.found_lon)) then
        call error_message( "grid % from_netcdf: could not find lat/lon auxilliar coordinates: ", &
                           trim(nc%fname), ":", var, " - ", trim(tmp_str))
      end if
      call this%aux_from_netcdf(nc, lat=trim(coords_str(size(coords_str)-1)), lon=trim(coords_str(size(coords_str))))
    end if

  end subroutine from_nc_dataset

  !> \brief read auxilliar coordinates from a netcdf file
  !> \details read auxilliar coordinates (lat, lon) from a netcdf file.
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
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine aux_from_nc_dataset(this, nc, lat, lon)
    use mo_netcdf, only : NcDataset, NcVariable
    use mo_utils, only : swap
    implicit none
    class(grid_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< NetCDF Dataset
    character(*), intent(in) :: lat !< nc variable name for latitude
    character(*), intent(in) :: lon !< nc variable name for longitude

    type(NcVariable) :: latvar, lonvar, latbvar, lonbvar
    integer(i4) :: rnk, i_ll, i_lr, i_ur, i_ul
    real(dp), allocatable, dimension(:,:) :: dummy
    real(dp), allocatable, dimension(:,:,:) :: latbnds, lonbnds
    logical :: y_inc, flip_v, flip_h, flip_y
    character(:), allocatable :: lon_, lat_
    character(len=256) :: name

    if (this%coordsys /= cartesian) &
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
    if (rnk /= 2) &
      call error_message("grid % from_netcdf: auxilliar latitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lat_)

    call latvar%getData(dummy)
    y_inc = .true.
    if (size(dummy, dim=2)>1) y_inc = dummy(1,1) < dummy(1,2)
    if (size(dummy, dim=1) /= this%nx .or. size(dummy, dim=2) /= this%ny) &
      call error_message("grid % from_netcdf: auxilliar latitude coordinate has wrong shape: ", trim(nc%fname), ":", lat_)

    flip_y = y_inc.neqv.(this%y_direction==bottom_up)
    allocate(this%lat(this%nx, this%ny))
    if (flip_y) call flip(dummy, iDim=2)
    this%lat = dummy
    deallocate(dummy)

    lonvar = nc%getVariable(lon_)
    if (.not.is_lon_coord(lonvar)) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not valid: ", trim(nc%fname), ":", lon_)
    rnk = lonvar%getRank()
    if (rnk /= 2) &
      call error_message("grid % aux_from_netcdf: auxilliar longitude coordinate is not 2 dimensional: ", trim(nc%fname), ":", lon_)

    call lonvar%getData(dummy)
    if (size(dummy, dim=1) /= this%nx .or. size(dummy, dim=2) /= this%ny) &
      call error_message("grid % from_netcdf: auxilliar longitude coordinate has wrong shape: ", trim(nc%fname), ":", lon_)
    allocate(this%lon(this%nx, this%ny))
    if (flip_y) call flip(dummy, iDim=2)
    this%lon = dummy
    deallocate(dummy)

    ! read aux vertices if present
    if (latvar%hasAttribute("bounds") .and. lonvar%hasAttribute("bounds")) then
      ! indices for lower-left, lower-right, upper-right and upper-left
      i_ll = 1_i4
      i_lr = 2_i4
      i_ur = 3_i4
      i_ul = 4_i4
      ! lat bounds (4, nx, ny)
      call latvar%getAttribute("bounds", name)
      latbvar = latvar%parent%getVariable(trim(name))
      call latbvar%getData(latbnds)
      if (.not.y_inc) call flip(latbnds, iDim=3) ! easier to use increasing bounds for calculations
      ! lon bounds (4, nx, ny)
      call lonvar%getAttribute("bounds", name)
      lonbvar = lonvar%parent%getVariable(trim(name))
      call lonbvar%getData(lonbnds)
      if (.not.y_inc) call flip(lonbnds, iDim=3) ! easier to use increasing bounds for calculations

      ! check if bounds need to be flipped (ACTUALLY UNEEDED IF CF CONFORMING)
      ! should look like this:      if y is decreasing:     if x is decreasing:     if x and y are decreasing:
      ! 4 -- 3   ul--ur             1 -- 2                  3 -- 4                  2 -- 1
      ! |    |   |    |             |    |                  |    |                  |    |
      ! 1 -- 2   ll--lr             4 -- 3                  2 -- 1                  3 -- 4
      flip_h = lonbnds(2,1,1) > lonbnds(1,1,1) ! .true. if x decreasing
      flip_v = latbnds(2,1,1) > latbnds(3,1,1) ! .true. if y decreasing
      if (flip_h) then
        call swap(i_ll, i_lr)
        call swap(i_ul, i_ur)
      end if
      if (flip_v) then
        call swap(i_ll, i_ul)
        call swap(i_lr, i_ur)
      end if
      allocate(this%lat_vertices(this%nx+1_i4, this%ny+1_i4))
      allocate(this%lon_vertices(this%nx+1_i4, this%ny+1_i4))
      ! lower left corner for matrix excluding right and upper side
      this%lat_vertices(1:this%nx, 1:this%ny) = latbnds(i_ll,:,:)
      this%lon_vertices(1:this%nx, 1:this%ny) = lonbnds(i_ll,:,:)
      ! lower right corner for right end of vertices map
      this%lat_vertices(this%nx+1_i4, 1:this%ny) = latbnds(i_lr,this%nx,:)
      this%lon_vertices(this%nx+1_i4, 1:this%ny) = lonbnds(i_lr,this%nx,:)
      ! upper left corner for upper end of vertices map
      this%lat_vertices(1:this%nx, this%ny+1_i4) = latbnds(i_ul,:,this%ny)
      this%lon_vertices(1:this%nx, this%ny+1_i4) = lonbnds(i_ul,:,this%ny)
      ! upper right corner of upper right cell for upper right end of vertices map
      this%lat_vertices(this%nx+1_i4, this%ny+1_i4) = latbnds(i_ur,this%nx,this%ny)
      this%lon_vertices(this%nx+1_i4, this%ny+1_i4) = lonbnds(i_ur,this%nx,this%ny)
      deallocate(lonbnds)
      deallocate(latbnds)
      ! vertices created from increasing axis, flip them if needed
      if (this%y_direction == top_down) then
        call flip(this%lat_vertices, 2)
        call flip(this%lon_vertices, 2)
      end if
    end if

  end subroutine aux_from_nc_dataset

  !> \brief write grid to a netcdf file
  !> \details write grid to a netcdf file with possible data variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine to_nc_file(this, path, x_name, y_name, aux_lon_name, aux_lat_name, double_precision, append)
    use mo_netcdf, only : NcDataset
    implicit none
    class(grid_t), intent(in) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    logical, optional, intent(in) :: append !< whether netcdf file should be opened in append mode (default .false.)
    type(NcDataset) :: nc
    character(1) :: fmode
    fmode = "w"
    if ( present(append) ) then
      if (append) fmode = "a"
    end if
    nc = NcDataset(path, fmode)
    call this%to_nc_dataset(nc, x_name, y_name, aux_lon_name, aux_lat_name, double_precision)
    call nc%close()
  end subroutine to_nc_file

  !> \brief write grid to a netcdf dataset
  !> \details write grid to a netcdf dataset with possible data variable.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine to_nc_dataset(this, nc, x_name, y_name, aux_lon_name, aux_lat_name, double_precision)
    use mo_netcdf, only : NcDataset, NcVariable, NcDimension
    use mo_utils, only : is_close
    use mo_string_utils, only : splitString
    implicit none
    class(grid_t), intent(in) :: this
    type(NcDataset), intent(inout) :: nc !< NetCDF Dataset
    character(*), optional, intent(in) :: x_name !< name for x-axis variable and dimension
    character(*), optional, intent(in) :: y_name !< name for y-axis variable and dimension
    character(*), optional, intent(in) :: aux_lon_name !< name for auxilliar longitude coordinate variable
    character(*), optional, intent(in) :: aux_lat_name !< name for auxilliar latitude coordinate variable
    logical, optional, intent(in) :: double_precision !< whether to use double precision to store axis (default .true.)
    type(NcDimension) :: x_dim, y_dim, b_dim, v_dim
    type(NcVariable) :: x_var, y_var, xb_var, yb_var
    character(:), allocatable :: xname, yname, lonname, latname
    logical :: double_precision_
    character(3) :: dtype

    double_precision_ = .true.
    if (present(double_precision)) double_precision_ = double_precision
    dtype = "f32"
    if ( double_precision_ ) dtype = "f64"

    if (this%coordsys==cartesian) then
      xname = "x"
      yname = "y"
    else
      xname = "lon"
      yname = "lat"
    end if
    lonname = "lon"
    latname = "lat"
    if (present(x_name)) xname = x_name
    if (present(y_name)) yname = y_name
    if (present(aux_lon_name)) lonname = aux_lon_name
    if (present(aux_lat_name)) latname = aux_lat_name

    x_dim = nc%setDimension(xname, this%nx)
    y_dim = nc%setDimension(yname, this%ny)
    if (nc%hasDimension("bnds")) then
      b_dim = nc%getDimension("bnds") ! check size
      if (b_dim%getLength() /= 2_i4) call error_message("grid % to_netcdf: bnds dim already present but with length =/= 2")
    else
      b_dim = nc%setDimension("bnds", 2_i4)
    end if
    if (this%has_aux_vertices()) then
      if (nc%hasDimension("nv")) then
        v_dim = nc%getDimension("nv") ! check size
        if (v_dim%getLength() /= 4_i4) call error_message("grid % to_netcdf: nv dim already present but with length =/= 4")
      else
        v_dim = nc%setDimension("nv", 4_i4)
      end if
    end if
    x_var = nc%setVariable(xname, dtype, [x_dim])
    y_var = nc%setVariable(yname, dtype, [y_dim])
    xb_var = nc%setVariable(xname // "_bnds", dtype, [b_dim, x_dim])
    yb_var = nc%setVariable(yname // "_bnds", dtype, [b_dim, y_dim])

    if (this%coordsys==cartesian) then
      call x_var%setAttribute("long_name", "x coordinate of projection")
      call x_var%setAttribute("standard_name", "projection_x_coordinate")
      call x_var%setAttribute("units", "m") ! TODO: this should be configurable
      call y_var%setAttribute("long_name", "y coordinate of projection")
      call y_var%setAttribute("standard_name", "projection_y_coordinate")
      call y_var%setAttribute("units", "m") ! TODO: this should be configurable
    else
      call x_var%setAttribute("long_name", "longitude")
      call x_var%setAttribute("standard_name", "longitude")
      call x_var%setAttribute("units", "degrees_east")
      call y_var%setAttribute("long_name", "latitude")
      call y_var%setAttribute("standard_name", "latitude")
      call y_var%setAttribute("units", "degrees_north")
    end if
    call x_var%setAttribute("axis", "X")
    call x_var%setAttribute("bounds", xname // "_bnds")
    call y_var%setAttribute("axis", "Y")
    call y_var%setAttribute("bounds", yname // "_bnds")

    if (double_precision_) then
      call x_var%setData(this%x_axis())
      call y_var%setData(this%y_axis())
      call xb_var%setData(this%x_bounds())
      call yb_var%setData(this%y_bounds())
    else
      call x_var%setData(real(this%x_axis(), sp))
      call y_var%setData(real(this%y_axis(), sp))
      call xb_var%setData(real(this%x_bounds(), sp))
      call yb_var%setData(real(this%y_bounds(), sp))
    end if

    if (this%has_aux_coords()) then
      x_var = nc%setVariable(lonname, dtype, [x_dim, y_dim])
      y_var = nc%setVariable(latname, dtype, [x_dim, y_dim])
      call x_var%setAttribute("long_name", "longitude")
      call x_var%setAttribute("standard_name", "longitude")
      call x_var%setAttribute("units", "degrees_east")
      call y_var%setAttribute("long_name", "latitude")
      call y_var%setAttribute("standard_name", "latitude")
      call y_var%setAttribute("units", "degrees_north")

      if (double_precision_) then
        call x_var%setData(this%lon)
        call y_var%setData(this%lat)
      else
        call x_var%setData(real(this%lon, sp))
        call y_var%setData(real(this%lat, sp))
      end if

      if (this%has_aux_vertices()) then
        call x_var%setAttribute("bounds", lonname // "_bnds")
        call y_var%setAttribute("bounds", latname // "_bnds")
        xb_var = nc%setVariable(lonname // "_bnds", dtype, [v_dim, x_dim, y_dim])
        yb_var = nc%setVariable(latname // "_bnds", dtype, [v_dim, x_dim, y_dim])
        if (double_precision_) then
          call xb_var%setData(this%lon_bounds())
          call yb_var%setData(this%lat_bounds())
        else
          call xb_var%setData(real(this%lon_bounds(), sp))
          call yb_var%setData(real(this%lat_bounds(), sp))
        end if
      end if
    end if

  end subroutine to_nc_dataset

#endif

  !> \brief get grid extent
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine extent(this, x_min, x_max, y_min, y_max, x_size, y_size)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), optional, intent(out) :: x_min !< left bound (x)
    real(dp), optional, intent(out) :: x_max !< right bound (x)
    real(dp), optional, intent(out) :: y_min !< lower bound (y)
    real(dp), optional, intent(out) :: y_max !< upper bound (y)
    real(dp), optional, intent(out) :: x_size !< x extent
    real(dp), optional, intent(out) :: y_size !< y extent

    if ( present(x_min) ) x_min = this%xllcorner
    if ( present(x_max) ) x_max = this%xllcorner + this%nx * this%cellsize
    if ( present(y_min) ) y_min = this%yllcorner
    if ( present(y_max) ) y_max = this%yllcorner + this%ny * this%cellsize
    if ( present(x_size) ) x_size = this%nx * this%cellsize
    if ( present(y_size) ) y_size = this%ny * this%cellsize

  end subroutine extent

  !> \brief Total domain area. NaN if cell_area not allocated.
  !> \return `real(dp) :: total_area`
  !> \authors Sebastian Müller
  !> \date Apr 2025
  real(dp) function total_area(this)
    use mo_sentinel, only : set_sentinel
    implicit none
    class(grid_t), intent(in) :: this

    if (allocated(this%cell_area)) then
      total_area = sum(this%cell_area)
    else
      call set_sentinel(total_area)
    end if

  end function total_area

  !> \brief Matrix of cell IDs.
  !> \return `integer(i8) :: id_matrix(nx,ny)`
  !> \authors Sebastian Müller
  !> \date Jun 2025
  function id_matrix(this)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), dimension(this%nx, this%ny) :: id_matrix
    integer(i8) :: i
    id_matrix = this%unpack([(i, i=1_i8, this%ncells)])
  end function id_matrix

  !> \brief Cell ID for given matrix indices.
  !> \return `integer(i8) :: cell_id`
  !> \authors Sebastian Müller
  !> \date Jun 2025
  integer(i8) function cell_id(this, indices)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: indices(2) !< matrix indices (x_i,y_i)
    logical, dimension(this%ncells) :: cell_loc
    cell_loc = (this%cell_ij(:, 1) == indices(1)).and.(this%cell_ij(:, 2) == indices(2))
    cell_id = findloc(cell_loc, .true., dim=1, kind=i8)
    if (cell_id == 0_i8) call error_message("grid%cell_id: given indices not found.")
  end function cell_id

  !> \brief Closest cell ID for given coordinates.
  !> \return `integer(i8) :: closest_cell_id`
  !> \authors Sebastian Müller
  !> \date Jun 2025
  integer(i8) function closest_cell_id(this, coords, use_aux)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: coords(2) !< coordiantes (x,y) or (lon,lat)
    logical, intent(in), optional :: use_aux !< use auxilliar coordinates (lon,lat)
    real(dp), allocatable :: xax(:), yax(:)
    real(dp) :: c(this%ncells, 2), dist(this%ncells)
    integer(i8) :: i
    logical :: aux = .false.
    if (present(use_aux)) aux = use_aux
    if (aux .and. .not.this%has_aux_coords()) call error_message("grid%closest_cell_id: no auxilliar coordniates defined.")
    if (aux) then
      c(:,1) = [(this%lon(this%cell_ij(i,1),this%cell_ij(i,2)), i=1_i8,this%ncells)]
      c(:,2) = [(this%lat(this%cell_ij(i,1),this%cell_ij(i,2)), i=1_i8,this%ncells)]
    else
      xax = this%x_axis()
      yax = this%y_axis()
      c(:,1) = [(xax(this%cell_ij(i,1)), i=1_i8,this%ncells)]
      c(:,2) = [(yax(this%cell_ij(i,2)), i=1_i8,this%ncells)]
    end if
    if (aux.or.this%coordsys==spherical) then
      dist = [(dist_latlon(c(i,2), c(i,1), coords(2), coords(1)), i=1_i8,this%ncells)]
    else
      dist = [(sqrt((c(i,1)-coords(1))**2 + (c(i,2)-coords(2))**2), i=1_i8,this%ncells)]
    end if
    closest_cell_id = minloc(dist, dim=1)
  end function closest_cell_id

  !> \brief x-axis of the grid cell centers
  !> \return `real(dp), allocatable, dimension(:) :: x_axis`
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
  !> \return `real(dp), allocatable, dimension(:) :: y_axis`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_axis(this, y_direction)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:) :: y_axis

    integer(i4) :: i, y_dir
    y_dir = keep_y
    if (present(y_direction)) y_dir = y_direction
    if (y_dir == keep_y) y_dir = this%y_direction

    select case (y_dir)
      case (0)
        y_axis = [ ((i-0.5_dp) * this%cellsize + this%yllcorner, i=this%ny, 1_i4, -1_i4) ]
      case (1)
        y_axis = [ ((i-0.5_dp) * this%cellsize + this%yllcorner, i=1_i4, this%ny) ]
      case default
        call error_message("grid % y_axis: y-direction not valid: ", trim(num2str(y_dir)))
    end select

  end function y_axis

  !> \brief x-vertices of the grid cell edges
  !> \return `real(dp), allocatable, dimension(:) :: x_vertices`
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
  !> \return `real(dp), allocatable, dimension(:) :: y_vertices`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_vertices(this, y_direction)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:) :: y_vertices

    integer(i4) :: i, y_dir
    y_dir = keep_y
    if (present(y_direction)) y_dir = y_direction
    if (y_dir == keep_y) y_dir = this%y_direction

    select case (y_dir)
      case (0)
        y_vertices = [ (i * this%cellsize + this%yllcorner, i=this%ny, 0_i4, -1_i4) ]
      case (1)
        y_vertices = [ (i * this%cellsize + this%yllcorner, i=0_i4, this%ny) ]
      case default
        call error_message("grid % y_vertices: y-direction not valid: ", trim(num2str(y_dir)))
    end select

  end function y_vertices

  !> \brief x-bounds of the grid cell following cf-conventions (2, nx).
  !> \return `real(dp), allocatable, dimension(:,:) :: x_bounds`
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
  !> \return `real(dp), allocatable, dimension(:,:) :: y_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function y_bounds(this, y_direction)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (-1 (default) for current, 0 for top-down, 1 for bottom-up)
    real(dp), allocatable, dimension(:,:) :: y_bounds
    real(dp), allocatable, dimension(:) :: y_ax
    ! bounds follow axis direction
    y_ax = this%y_vertices(y_direction)
    allocate(y_bounds(2, this%ny))
    y_bounds(1,:) = y_ax(1:this%ny)
    y_bounds(2,:) = y_ax(2:this%ny+1)
  end function y_bounds

  !> \brief estimate auxilliar coordinates (lat, lon) from finer grid
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine upscale_aux_coords(this, fine_grid, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(in) :: fine_grid !< finer grid to estimate the auxilliar coordinates from
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: n_subcells
    integer(i4) :: i_ub, i_lb, j_lb, j_ub, i, j, factor
    call this%check_is_covering(fine_grid, tol=tol, check_mask=.false.)
    call check_factor(fine_grid%cellsize, this%cellsize, factor=factor, tol=tol)

    if (this % coordsys /= cartesian) &
      call error_message("grid % upscale_aux_coords: grids already use spherical coordinate system for axis.")

    if (.not. fine_grid%has_aux_coords()) &
      call error_message("grid % upscale_aux_coords: fine grid has no auxilliar coordinates defined.")

    if (this%has_aux_coords()) &
      call error_message("grid % upscale_aux_coords: grid already has auxilliar coordinates defined.")

    allocate(this%lat(this%nx, this%ny))
    allocate(this%lon(this%nx, this%ny))
    do j = 1, this%ny
      do i = 1, this%nx
        ! coord. of all corners -> of finer scale
        call id_bounds(factor, i, j, &
          this%y_direction, this%ny, &
          fine_grid%y_direction, fine_grid%nx, fine_grid%ny, &
          i_lb, i_ub, j_lb, j_ub)
        n_subcells = real(size(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)), dp)
        ! estimate lat-lon coords by averaging sub-cell coordinates (even if fine grid is "a bit" smaller)
        this%lat(i, j) = sum(fine_grid%lat(i_lb:i_ub, j_lb:j_ub)) / n_subcells
        this%lon(i, j) = sum(fine_grid%lon(i_lb:i_ub, j_lb:j_ub)) / n_subcells
      end do
    end do

  end subroutine upscale_aux_coords

  !> \brief estimate auxilliar coordinates (lat, lon) from coarser grid
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine downscale_aux_coords(this, coarse_grid, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    type(grid_t), intent(inout) :: coarse_grid !< finer grid to estimate the auxilliar coordinates from
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    real(dp) :: x, dx, fac
    integer(i4) :: i, j, ii, jj, i_start, i_end, j_start, j_end, k, factor, fac_i, fac_j
    logical :: reset_aux_vertices
    real(dp), allocatable, dimension(:,:) :: latlon_t, latlon_b, latlon_l, latlon_r

    call this%check_is_covered_by(coarse_grid, tol=tol, check_mask=.false.)
    call check_factor(this%cellsize, coarse_grid%cellsize, rounded=fac, factor=factor, tol=tol)

    if (this % coordsys /= cartesian) &
      call error_message("grid % downscale_aux_coords: grids already use spherical coordinate system for axis.")

    if (.not. coarse_grid%has_aux_coords()) &
      call error_message("grid % downscale_aux_coords: fine grid has no auxilliar coordinates defined.")

    if (this%has_aux_coords()) &
      call error_message("grid % downscale_aux_coords: grid already has auxilliar coordinates defined.")

    if (coarse_grid%has_aux_vertices()) then
      reset_aux_vertices = .false.
    else
      reset_aux_vertices = .true.
      ! separate error message for single cell grid here?
      call coarse_grid%estimate_aux_vertices()
    end if

    allocate(latlon_t(factor, 2_i4))
    allocate(latlon_b(factor, 2_i4))
    allocate(latlon_l(factor, 2_i4))
    allocate(latlon_r(factor, 2_i4))

    allocate(this%lat(this%nx, this%ny))
    allocate(this%lon(this%nx, this%ny))
    do j = 1, coarse_grid%ny
      do i = 1, coarse_grid%nx
        ! calculate sub-points along vertice boundaries (top, left, right, bottom)
        ! lon
        x = coarse_grid%lon_vertices(i,j)
        dx = coarse_grid%lon_vertices(i+1,j) - x
        latlon_t(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i,j)
        dx = coarse_grid%lon_vertices(i,j+1) - x
        latlon_l(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i+1,j)
        dx = coarse_grid%lon_vertices(i+1,j+1) - x
        latlon_r(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lon_vertices(i,j+1)
        dx = coarse_grid%lon_vertices(i+1,j+1) - x
        latlon_b(:,1) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        ! lat
        x = coarse_grid%lat_vertices(i,j)
        dx = coarse_grid%lat_vertices(i+1,j) - x
        latlon_t(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i,j)
        dx = coarse_grid%lat_vertices(i,j+1) - x
        latlon_l(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i+1,j)
        dx = coarse_grid%lat_vertices(i+1,j+1) - x
        latlon_r(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        x = coarse_grid%lat_vertices(i,j+1)
        dx = coarse_grid%lat_vertices(i+1,j+1) - x
        latlon_b(:,2) = [(x + dx * (real(k)-0.5_dp) / fac, k=1_i4,factor)]
        ! generate latlon pairs in sub-cells
        call id_bounds(factor, i, j, &
          coarse_grid%y_direction, coarse_grid%ny, &
          this%y_direction, this%nx, this%ny, &
          i_start, i_end, j_start, j_end)
        fac_i = i_end - i_start + 1_i4
        fac_j = j_end - j_start + 1_i4

        do jj = 1, fac_j
          do ii = 1, fac_i
            call intersection( &
              latlon_t(ii,1), latlon_t(ii,2), latlon_b(ii,1), latlon_b(ii,2), &
              latlon_l(jj,1), latlon_l(jj,2), latlon_r(jj,1), latlon_r(jj,2), &
              this%lon(i_start+ii-1,j_start+jj-1), this%lat(i_start+ii-1,j_start+jj-1))
          end do
        end do

      end do
    end do

    if ( reset_aux_vertices ) then
      deallocate(coarse_grid%lat_vertices)
      deallocate(coarse_grid%lon_vertices)
    end if

  end subroutine downscale_aux_coords

  subroutine intersection(p1x, p1y, p2x, p2y, q1x, q1y, q2x, q2y, x, y)
    use mo_utils, only : is_close
    ! start and end point of line p (by x/y components)
    real(dp), intent(in) :: p1x, p1y, p2x, p2y
    ! start and end point of line q (by x/y components)
    real(dp), intent(in) :: q1x, q1y, q2x, q2y
    ! resulting coordinates of the intersection point
    real(dp), intent(out) ::  x, y
    real(dp) ::  denom, det1, det2

    denom = (p1x-p2x)*(q1y-q2y) - (p1y-p2y)*(q1x-q2x)
    ! if (is_close(denom, 0.0_dp)) call error_message("intersection: lines are parallel.")
    det1 = p1x*p2y-p1y*p2x
    det2 = q1x*q2y-q1y*q2x
    x = (det1*(q1x-q2x) - det2*(p1x-p2x)) / denom
    y = (det1*(q1y-q2y) - det2*(p1y-p2y)) / denom

  end subroutine intersection

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
  !> \return `real(dp), allocatable, dimension(:,:,:) :: lat_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function lat_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lat_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lat_bounds: grid has no auxilliar vertices defined.")
    allocate(lat_bounds(4, this%nx, this%ny))
    if (this%y_direction == bottom_up) then
      ! lower-left corner of the cells
      lat_bounds(1,:,:) = this%lat_vertices(1:this%nx, 1:this%ny)
      ! lower-right corner of the cells
      lat_bounds(2,:,:) = this%lat_vertices(2:this%nx+1, 1:this%ny)
      ! upper-right corner of the cells
      lat_bounds(3,:,:) = this%lat_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-left corner of the cells
      lat_bounds(4,:,:) = this%lat_vertices(1:this%nx, 2:this%ny+1)
    else
      ! lower-left corner of the cells
      lat_bounds(1,:,:) = this%lat_vertices(1:this%nx, 2:this%ny+1)
      ! lower-right corner of the cells
      lat_bounds(2,:,:) = this%lat_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-right corner of the cells
      lat_bounds(3,:,:) = this%lat_vertices(2:this%nx+1, 1:this%ny)
      ! upper-left corner of the cells
      lat_bounds(4,:,:) = this%lat_vertices(1:this%nx, 1:this%ny)
    end if
  end function lat_bounds

  !> \brief lon-bounds of the grid cell following cf-conventions (4, nx, ny).
  !> \return `real(dp), allocatable, dimension(:,:,:) :: lon_bounds`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  function lon_bounds(this)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), allocatable, dimension(:,:,:) :: lon_bounds
    if (.not.this%has_aux_vertices()) &
      call error_message("grid % lon_bounds: grid has no auxilliar vertices defined.")
    allocate(lon_bounds(4, this%nx, this%ny))
    if (this%y_direction == bottom_up) then
      ! lower-left corner of the cells
      lon_bounds(1,:,:) = this%lon_vertices(1:this%nx, 1:this%ny)
      ! lower-right corner of the cells
      lon_bounds(2,:,:) = this%lon_vertices(2:this%nx+1, 1:this%ny)
      ! upper-right corner of the cells
      lon_bounds(3,:,:) = this%lon_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-left corner of the cells
      lon_bounds(4,:,:) = this%lon_vertices(1:this%nx, 2:this%ny+1)
    else
      ! lower-left corner of the cells
      lon_bounds(1,:,:) = this%lon_vertices(1:this%nx, 2:this%ny+1)
      ! lower-right corner of the cells
      lon_bounds(2,:,:) = this%lon_vertices(2:this%nx+1, 2:this%ny+1)
      ! upper-right corner of the cells
      lon_bounds(3,:,:) = this%lon_vertices(2:this%nx+1, 1:this%ny)
      ! upper-left corner of the cells
      lon_bounds(4,:,:) = this%lon_vertices(1:this%nx, 1:this%ny)
    end if
  end function lon_bounds

  !> \brief check if given grid has an allocated mask
  !> \return `logical :: has_mask`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function has_mask(this)
    implicit none
    class(grid_t), intent(in) :: this
    has_mask = allocated(this%mask)
  end function has_mask

  !> \brief check if given grid has any missing cells (mask allocated and any value .false.)
  !> \return `logical :: any_missing`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function any_missing(this)
    implicit none
    class(grid_t), intent(in) :: this
    if (.not. this%has_mask()) then
      any_missing = .false.
    else
      any_missing = .not. all(this%mask)
    end if
  end function any_missing

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
    ! check extent
    if (.not. ((coarse_grid%nx - 1) * factor <= this%nx .and. this%nx <= coarse_grid%nx * factor .and. &
               (coarse_grid%ny - 1) * factor <= this%ny .and. this%ny <= coarse_grid%ny * factor)) then
      call error_message("grid % check_is_covered_by: coarse grid extent is not matching.")
    end if

    if ( check_mask_ .and. coarse_grid%any_missing()) then
      if (.not. this%any_missing()) call error_message("grid % check_is_covered_by: coarse grid is masked, this grid not.")
      do j = 1, coarse_grid%ny
        do i = 1, coarse_grid%nx
          if ( coarse_grid%mask(i, j)) cycle
          call id_bounds(factor, i, j, &
            coarse_grid%y_direction, coarse_grid%ny, &
            this%y_direction, this%nx, this%ny, &
            i_lb, i_ub, j_lb, j_ub)
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

  !> \brief check if given grid is filled by fine grid
  !> \details check if given grid is compatible and filled by finer grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_is_filled_by(this, fine_grid, tol, check_mask)
    use mo_utils, only: eq
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: fine_grid !< fine grid that should fill this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask

    integer(i4) :: factor, i, j, i_lb, i_ub, j_lb, j_ub
    logical :: check_mask_

    check_mask_ = .true.
    if ( present(check_mask) ) check_mask_ = check_mask

    if (this%coordsys /= fine_grid%coordsys) then
      call error_message("grid % check_is_filled_by: grids don't use the same coordinate system.")
    end if

    call check_factor(fine_grid%cellsize, this%cellsize, factor=factor, tol=tol)

    ! check ll corner
    if ( .not. (eq(this%xllcorner, fine_grid%xllcorner) .and. eq(this%yllcorner, fine_grid%yllcorner)) ) then
      call error_message("grid % check_is_filled_by: fine grid lower-left corner is not matching.")
    end if
    ! check extent
    if (.not. ((this%nx - 1) * factor <= fine_grid%nx .and. fine_grid%nx <= this%nx * factor .and. &
               (this%ny - 1) * factor <= fine_grid%ny .and. fine_grid%ny <= this%ny * factor)) then
      call error_message("grid % check_is_filled_by: fine grid extent is not matching.")
    end if

    if ( check_mask_ .and. fine_grid%any_missing()) then
      do j = 1, this%ny
        do i = 1, this%nx
          if ( .not.this%mask(i, j)) cycle
          call id_bounds(factor, i, j, &
            this%y_direction, this%ny, &
            fine_grid%y_direction, fine_grid%nx, fine_grid%ny, &
            i_lb, i_ub, j_lb, j_ub)
          if (.not.any(fine_grid%mask(i_lb:i_ub, j_lb:j_ub))) then
            call error_message("grid % check_is_filled_by: coarse cells without any filling fine cells found.")
          end if
        end do
      end do
    end if
  end subroutine check_is_filled_by

  !> \brief check if given grid is filling coarser grid
  !> \details check if given grid is compatible with and filling coarser grid and raise an error if this is not the case.
  !! \note The fine grid is allowed to have valid cells outside of the coarse grids masked region.
  !> \authors Sebastian Müller
  !> \date Mar 2024
  subroutine check_is_filling(this, coarse_grid, tol, check_mask)
    implicit none
    class(grid_t), intent(in) :: this
    type(grid_t), intent(in) :: coarse_grid !< coarser grid that should be covered by this grid
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    logical, optional, intent(in) :: check_mask !< whether to check if fine mask fills coarse mask
    call coarse_grid%check_is_filled_by(fine_grid=this, tol=tol, check_mask=check_mask)
  end subroutine check_is_filling

  !> \brief check if given grid has auxilliar coordinates allocated.
  !> \return `logical :: has_aux_coords`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function has_aux_coords(this)
    implicit none
    class(grid_t), intent(in) :: this
    has_aux_coords = allocated(this%lat) .and. allocated(this%lon)
  end function has_aux_coords

  !> \brief check if given grid has auxilliar vertices allocated.
  !> \return `logical :: has_aux_vertices`
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

    integer(i4) :: i, j
    integer(i8) :: k

    ! if mask not allocated create one with only .true. values
    if (.not. allocated(this%mask)) then
      allocate(this%mask(this%nx, this%ny))
      this%mask = .true.
    end if

    this%ncells = count(this%mask)
    allocate(this%cell_ij(this%ncells, 2))

    k = 0_i8
    do j = 1, this%ny
      do i = 1, this%nx
        if (.NOT. this%mask(i, j)) cycle
        k = k + 1_i8
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
  subroutine calculate_cell_area(this)

    use mo_constants, only : RadiusEarth_dp, deg2rad_dp
    implicit none

    class(grid_t), intent(inout) :: this

    real(dp), dimension(:, :), allocatable :: cell_area
    real(dp) :: factor, cell_size_rad, cell_center_lat_rad
    integer(i4) :: j

    if (.not. allocated(this%cell_area)) allocate(this%cell_area(this%ncells))

    ! regular X-Y coordinate system
    if(this%coordsys .eq. cartesian) then
      this%cell_area(:) = this%cellsize * this%cellsize

    ! regular lat-lon coordinate system
    else if(this%coordsys .eq. spherical) then
      allocate(cell_area(this%nx, this%ny))

      ! A = R ** 2 * dx * (sin(lat1) - sin(lon2))
      !   = R ** 2 * dx * cos([lat1 + lat2] / 2) * sin(dy / 2) * 2
      !
      ! A ~ R ** 2 * dx * cos(cell_center_lat) * dy                (approx. since: sin x = x for small x)

      cell_size_rad = this%cellsize * deg2rad_dp
      factor = (RadiusEarth_dp * cell_size_rad) * (RadiusEarth_dp * sin(cell_size_rad / 2.0_dp) * 2.0_dp)

      do j = 1, this%ny
        ! get latitude of cell-center in radians (y-axis is increasing!)
        cell_center_lat_rad = (this%yllcorner + (real(j, dp) - 0.5_dp) * this%cellsize) * deg2rad_dp
        ! AREA [m2]
        cell_area(:, j) = cos(cell_center_lat_rad) * factor
      end do
      if (this%y_direction == top_down) call flip(cell_area, idim=2)
      this%cell_area(:) = pack(cell_area(:, :), this%mask)

      ! free space
      deallocate(cell_area)
    else
      call error_message("estimate_cell_area: unknown coordsys value: ", num2str(this%coordsys))
    end if

  end subroutine calculate_cell_area

  !> \brief check if given grid is a global lat-lon grid with periodic lon axis
  !> \return `logical :: is_periodic`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_periodic(this)
    use mo_utils, only : is_close
    implicit none
    class(grid_t), intent(in) :: this
    if (this%coordsys == cartesian) then
      is_periodic = .false.
    else
      is_periodic = is_close(360.0_dp, this%nx * this%cellsize - this%xllcorner)
    endif
  end function is_periodic

  !> \brief Generate coarse grid from a fine grid by a given target resolution
  !> \details following attributes are calculated for the coarse grid:
  !!          -  cell id & numbering
  !!          -  mask creation
  !> \return `type(grid_t) :: coarse_grid`
  !> \authors Rohini Kumar
  !> \date    Jan 2013
  !> \changelog
  !! - Sebastian Müller, Mar 2024
  !!   - moving to FORCES
  !!   - is now a method of the grid type
  function derive_coarse_grid(this, target_resolution, estimate_aux, estimate_area, area_method, tol) result(coarse_grid)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from fine grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: coarse_grid !< resulting low resolution grid

    real(dp), dimension(:, :), allocatable :: fine_cell_area
    integer(i4) :: i_ub, i_lb, j_lb, j_ub
    integer(i4) :: i, j
    integer(i8) :: k
    integer(i4) :: factor, area_method_
    logical :: estimate_aux_, estimate_area_

    estimate_aux_ = .true.
    if (present(estimate_aux)) estimate_aux_ = estimate_aux
    estimate_area_ = .true.
    if (present(estimate_area)) estimate_area_ = estimate_area
    area_method_ = 0_i4
    if (present(area_method)) area_method_ = area_method

    !--------------------------------------------------------
    ! 1) Estimate each variable locally for a given domain
    ! 2) Pad each variable to its corresponding global one
    !--------------------------------------------------------
    ! grid properties
    call calculate_coarse_extent( &
      nx_in=this%nx, &
      ny_in=this%ny, &
      xllcorner_in=this%xllcorner, &
      yllcorner_in=this%yllcorner, &
      cellsize_in=this%cellsize, &
      target_resolution=target_resolution, &
      nx_out=coarse_grid%nx, &
      ny_out=coarse_grid%ny, &
      xllcorner_out=coarse_grid%xllcorner, &
      yllcorner_out=coarse_grid%yllcorner, &
      cellsize_out=coarse_grid%cellsize, &
      tol=tol &
    )

    coarse_grid%coordsys = this%coordsys
    coarse_grid%y_direction = this%y_direction

    factor = nint(coarse_grid%cellsize / this%cellsize, i4)

    if (this%is_periodic().and.(.not.coarse_grid%is_periodic())) &
      call error_message("derive_coarse_grid: target resolution is not suitable for a periodic grid.")

    ! create mask at coarse grid
    allocate(coarse_grid%mask(coarse_grid%nx, coarse_grid%ny), source=.false.)
    do j = 1, coarse_grid%ny
      do i = 1, coarse_grid%nx
        call id_bounds(factor, i, j, &
          coarse_grid%y_direction, coarse_grid%ny, &
          this%y_direction, this%nx, this%ny, &
          i_lb, i_ub, j_lb, j_ub)
        coarse_grid%mask(i, j) = any(this%mask(i_lb : i_ub, j_lb : j_ub))
      end do
    end do

    call coarse_grid%calculate_cell_ids()

    if (estimate_area_) then
      select case(area_method_)
        case(0_i4)
          ! lowres additional properties
          allocate(fine_cell_area(this%nx, this%ny))
          fine_cell_area(:, :) = this%unpack(this%cell_area)
          allocate(coarse_grid%cell_area(coarse_grid%ncells))
          k = 0_i8
          do j = 1, coarse_grid%ny
            do i = 1, coarse_grid%nx
              if (.NOT. coarse_grid%mask(i, j)) cycle
              k = k + 1_i8
              call id_bounds(factor, i, j, &
                coarse_grid%y_direction, coarse_grid%ny, &
                this%y_direction, this%nx, this%ny, &
                i_lb, i_ub, j_lb, j_ub)
              ! effective area [km2] & total no. of fine grid cells within a given coarse grid cell
              coarse_grid%cell_area(k) = sum(fine_cell_area(i_lb : i_ub, j_lb : j_ub), this%mask(i_lb : i_ub, j_lb : j_ub))
            end do
          end do
          ! free space
          deallocate(fine_cell_area)
        case(1_i4)
          call coarse_grid%calculate_cell_area()
        case default
          call error_message("derive_coarse_grid: 'area_method' needs to be 0 or 1. Got: ", trim(num2str(area_method_)))
      end select
    end if

    ! only estimate aux coords if we are on a projected grid
    if ( estimate_aux_ .and. this%coordsys == cartesian .and. this%has_aux_coords()) then
      call coarse_grid%upscale_aux_coords(this, tol=tol)
    end if

  end function derive_coarse_grid

  !> \brief Generate fine grid from a coarse grid by a given target resolution
  !> \details following attributes are calculated for the coarse grid:
  !!          -  cell id & numbering
  !!          -  mask creation
  !> \return `type(grid_t) :: fine_grid`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function derive_fine_grid(this, target_resolution, estimate_aux, estimate_area, area_method, tol) result(fine_grid)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in) :: target_resolution !< desired target resolution
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate coarse cell areas respecting fine mask (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from fine grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: fine_grid !< resulting low resolution grid

    integer(i4) :: i, j, ic, jc
    integer(i4) :: factor, area_method_
    logical :: estimate_aux_, estimate_area_

    estimate_aux_ = .true.
    if (present(estimate_aux)) estimate_aux_ = estimate_aux
    estimate_area_ = .true.
    if (present(estimate_area)) estimate_area_ = estimate_area
    area_method_ = 0_i4
    if (present(area_method)) area_method_ = area_method

    call check_factor(target_resolution, this%cellsize, factor=factor, tol=tol)

    fine_grid%nx = this%nx * factor
    fine_grid%ny = this%ny * factor
    fine_grid%cellsize = target_resolution
    fine_grid%xllcorner = this%xllcorner
    fine_grid%yllcorner = this%yllcorner
    fine_grid%coordsys = this%coordsys
    fine_grid%y_direction = this%y_direction

    ! create mask at fine grid
    allocate(fine_grid%mask(fine_grid%nx, fine_grid%ny), source=.false.)
    do j = 1_i4, this%ny
      ! everything would be better with 0-based ids
      jc = (j-1_i4) * factor + 1_i4
      do i = 1, this%nx
        if (.not. this%mask(i, j)) cycle
        ic = (i-1_i4) * factor + 1_i4
        fine_grid%mask(ic : ic + factor - 1_i4, jc :  jc + factor - 1_i4) = .true.
      end do
    end do

    call fine_grid%calculate_cell_ids()

    ! This doesn't preserve the total area if the coarse cells were fractioned.
    if (estimate_area_) call fine_grid%calculate_cell_area()

    ! only estimate aux coords if we are on a projected grid
    if ( estimate_aux_ .and. this%coordsys == cartesian .and. this%has_aux_coords()) then
      call fine_grid%downscale_aux_coords(this, tol=tol)
    end if

  end function derive_fine_grid

  !> \brief Generate a derived grid by a given target resolution
  !> \return `type(grid_t) :: result_grid`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function derive_grid(this, target_resolution, downscaling_factor, upscaling_factor, estimate_aux, estimate_area, area_method, tol)
    implicit none
    class(grid_t), intent(inout) :: this
    real(dp), intent(in), optional :: target_resolution !< desired target resolution
    integer(i4), intent(in), optional :: downscaling_factor !< factor for finer grid
    integer(i4), intent(in), optional :: upscaling_factor !< factor for coarser grid
    logical, intent(in), optional :: estimate_aux !< whether to estimate lat-lon coordinates of coarse grid (default: .true.)
    logical, intent(in), optional :: estimate_area !< whether to estimate cell areas (default: .true.)
    integer(i4), intent(in), optional :: area_method !< method to estimate area: (0, default) from other grid, (1) from cell extent
    real(dp), optional, intent(in) :: tol !< tolerance for cell factor comparisson (default: 1.e-7)
    type(grid_t) :: derive_grid !< resulting grid
    real(dp) :: resolution
    integer(i4) :: input_opt

    input_opt = 0_i4
    if (present(target_resolution)) then
      input_opt = input_opt + 1
      resolution = target_resolution
    end if
    if (present(downscaling_factor)) then
      input_opt = input_opt + 1
      resolution = this%cellsize / real(downscaling_factor, dp)
    end if
    if (present(upscaling_factor)) then
      input_opt = input_opt + 1
      resolution = this%cellsize * real(upscaling_factor, dp)
    end if
    if (input_opt /= 1) then
      call error_message("derive_grid: only one of 'target_resolution', 'factor_up' or 'factor_down' can be given.")
    end if

    if (resolution < this%cellsize) then
      derive_grid = this%derive_fine_grid(resolution, estimate_aux, estimate_area, area_method, tol)
    else
      derive_grid = this%derive_coarse_grid(resolution, estimate_aux, estimate_area, area_method, tol)
    end if

  end function derive_grid

  !> \brief Check 2D data shape for grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine grid_check_shape(this, data_shape)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data_shape(2) !< (x,y)
    if (data_shape(1) /= this%nx .or. data_shape(2) /= this%ny) then
      call error_message( &
        "grid data: data has wrong shape. Expected: (", &
        num2str(this%nx), ",", num2str(this%ny), "), got: (", &
        num2str(data_shape(1)), ",", num2str(data_shape(2)), ")")
    end if
  end subroutine grid_check_shape

  !> \brief Check 1D packed data shape for grid
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  subroutine grid_check_shape_packed(this, data_shape)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data_shape(1) !< (n)
    if (data_shape(1) /= this%ncells) then
      call error_message( &
        "grid data: packed data has wrong shape. Expected: (", &
        num2str(this%ncells), "), got: (", num2str(data_shape(1)), ")")
    end if
  end subroutine grid_check_shape_packed

  !> \brief Pack 2D data with grid mask
  !> \return `real(sp) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function pack_data_sp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:,:)
    real(sp), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function pack_data_sp

  !> \brief Pack 2D data with grid mask
  !> \return `real(dp) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function pack_data_dp(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:,:)
    real(dp), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function pack_data_dp

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i4) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function pack_data_i4(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:,:)
    integer(i4), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function pack_data_i4

  !> \brief Pack 2D data with grid mask
  !> \return `integer(i8) :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function pack_data_i8(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:,:)
    integer(i8), allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function pack_data_i8

  !> \brief Pack 2D data with grid mask
  !> \return `logical :: out_data(:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function pack_data_lgt(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:,:)
    logical, allocatable :: out_data(:)
    call this%check_shape(shape(data, kind=i8))
    allocate(out_data(this%ncells))
    out_data(:) = pack(data, this%mask)
  end function pack_data_lgt

  !> \brief Unpack 1D data with grid mask
  !> \return `real(sp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function unpack_data_sp(this, data) result(out_data)
    use mo_constants, only : nodata_sp
    implicit none
    class(grid_t), intent(in) :: this
    real(sp), intent(in) :: data(:)
    real(sp), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_sp)
  end function unpack_data_sp

  !> \brief Unpack 1D data with grid mask
  !> \return `real(dp) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function unpack_data_dp(this, data) result(out_data)
    use mo_constants, only : nodata_dp
    implicit none
    class(grid_t), intent(in) :: this
    real(dp), intent(in) :: data(:)
    real(dp), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_dp)
  end function unpack_data_dp

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i4) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function unpack_data_i4(this, data) result(out_data)
    use mo_constants, only : nodata_i4
    implicit none
    class(grid_t), intent(in) :: this
    integer(i4), intent(in) :: data(:)
    integer(i4), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i4)
  end function unpack_data_i4

  !> \brief Unpack 1D data with grid mask
  !> \return `integer(i8) :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function unpack_data_i8(this, data) result(out_data)
    use mo_constants, only : nodata_i8
    implicit none
    class(grid_t), intent(in) :: this
    integer(i8), intent(in) :: data(:)
    integer(i8), allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, nodata_i8)
  end function unpack_data_i8

  !> \brief Unpack 1D data with grid mask
  !> \return `logical :: out_data(:,:)`
  !> \authors Sebastian Müller
  !> \date    Mar 2025
  function unpack_data_lgt(this, data) result(out_data)
    implicit none
    class(grid_t), intent(in) :: this
    logical, intent(in) :: data(:)
    logical, allocatable :: out_data(:,:)
    call this%check_shape_packed(shape(data, kind=i8))
    allocate(out_data(this%nx, this%ny))
    out_data(:,:) = unpack(data, this%mask, .false.)
  end function unpack_data_lgt

  ! ------------------------------------------------------------------

  !> \brief calculate coarse grid extent
  !> \details Calculates basic grid properties at a required coarser level using
  !!          information of a given finer level.
  !!          Calculates basic grid properties at a required coarser level (e.g., L11) using
  !!          information of a given finer level (e.g., L0). Basic grid properties such as
  !!          nx, ny, xllcorner, yllcorner cellsize are estimated in this routine.
  !> \authors Matthias Zink & Rohini Kumar
  !> \date Feb 2013
  subroutine calculate_coarse_extent(nx_in,  ny_in,  xllcorner_in,  yllcorner_in,  cellsize_in,  target_resolution, &
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

    align_ = lower_left
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
    if (align_ == lower_left .or. align_ == lower_right) then
      yllcorner_out = yllcorner_in
    else
      yllcorner_out = yllcorner_in + real(ny_in, dp) * target_resolution / rounded - real(ny_out, dp) * cellsize_out
    endif
    ! keep xll if aligning in lower-(left) or top-(left)
    if (align_ == lower_left .or. align_ == upper_left) then
      xllcorner_out = xllcorner_in
    else
      xllcorner_out = xllcorner_in + real(nx_in, dp) * target_resolution / rounded - real(nx_out, dp) * cellsize_out
    endif

  end subroutine calculate_coarse_extent

  ! ------------------------------------------------------------------

#ifdef FORCES_WITH_NETCDF

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
  !> \return `logical :: is_x_axis`
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
  !> \return `logical :: is_y_axis`
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

  !> \brief check if given variable is a z-axis.
  !> \return `logical :: is_z_axis`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_z_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
    character(len=256) :: tmp_str

    is_z_axis = .false.
    if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "Z") is_z_axis = .true.
    else if (var%hasAttribute("positive")) then
      is_z_axis = .true.
    end if
  end function is_z_axis

  !> \brief check if given variable is a time-axis.
  !> \return `logical :: is_t_axis`
  !> \authors Sebastian Müller
  !> \date Mar 2024
  logical function is_t_axis(var)
    use mo_netcdf, only : NcVariable
    implicit none
    type(NcVariable), intent(in) :: var !< NetCDF variable to check
    character(len=256) :: tmp_str

    is_t_axis = .false.
    if (var%hasAttribute("units")) then
      call var%getAttribute("units", tmp_str)
      if (index(tmp_str, "since") > 0) is_t_axis = .true.
    else if (var%hasAttribute("axis")) then
      call var%getAttribute("axis", tmp_str)
      if (trim(tmp_str) == "T") is_t_axis = .true.
    else if (var%hasAttribute("standard_name")) then
      call var%getAttribute("standard_name", tmp_str)
      if (trim(tmp_str) == "time") is_t_axis = .true.
    end if
  end function is_t_axis

  !> \brief check if given variable is a lon coordinate.
  !> \return `logical :: is_lon_coord`
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
  !> \return `logical :: is_lat_coord`
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

#endif

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
        'check_factor: Two resolutions size do not confirm (need to have an integer ratio): ', &
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
  subroutine read_ascii_grid_dp(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    use mo_utils, only : eq
    implicit none

    character(len = *), intent(in) :: path !< path with location
    real(dp), dimension(:, :), allocatable, intent(out) :: data !< data, size (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask, size (nx, ny)
    integer(i4), intent(in), optional :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in), optional :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in), optional :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_cellsize !< reference cellsize
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (0 (default) or -1 for top-down, 1 for bottom-up)

    integer(i4) :: file_nrows ! number of rows of data fields (ny)
    integer(i4) :: file_ncols ! number of columns of data fields (nx)
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    integer(i4) :: i, j, fileunit, hlines
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == bottom_up

    ! compare headers always with reference header (intent in)
    call read_ascii_header( &
      path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata, &
      ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, hlines)

    allocate(data(file_ncols, file_nrows))

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, hlines
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (data(j, i), j = 1, file_ncols)
    end do
    close(fileunit)

    if ( present(mask) ) then
      allocate(mask(file_ncols, file_nrows), source=.true.)
      where (eq(data, file_nodata)) mask = .false.
      if (flip_y) call flip(mask, idim=2)
    end if

    ! transpose of data due to longitude-latitude ordering
    if (flip_y) call flip(data, idim=2)

  end subroutine read_ascii_grid_dp

  !> \brief Read spatial data.
  !> \details Read spatial data from ascii file. Data will be transposed to be in xy order.
  !> \authors Robert Schweppe
  !> \date Jun 2018
  subroutine read_ascii_grid_i4(path, data, mask, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, y_direction)
    implicit none

    character(len = *), intent(in) :: path !< path with location
    integer(i4), dimension(:, :), allocatable, intent(out) :: data !< data (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask (nx, ny)
    integer(i4), intent(in), optional :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in), optional :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in), optional :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_cellsize !< reference cellsize
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (0 (default) or -1 for top-down, 1 for bottom-up)

    integer(i4) :: file_nrows ! number of rows of data fields
    integer(i4) :: file_ncols ! number of columns of data fields
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    integer(i4) :: i, j, fileunit, header_size
    logical :: flip_y

    flip_y = .false.
    if (present(y_direction)) flip_y = y_direction == bottom_up

    ! compare headers always with reference header (intent in)
    call read_ascii_header( &
      path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata, &
      ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, header_size)

    allocate(data(file_ncols, file_nrows))

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, header_size
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (data(j, i), j = 1, file_ncols)
    end do
    close(fileunit)

    if ( present(mask) ) then
      allocate(mask(file_ncols, file_nrows), source=.true.)
      where (data == int(file_nodata, i4)) mask = .false.
      if (flip_y) call flip(mask, idim=2)
    end if
    if (flip_y) call flip(data, idim=2)

  end subroutine read_ascii_grid_i4

  !> \brief Reads header lines of ASCII files.
  !> \details Reads header lines of ASCII files, e.g. dem, aspect, flow direction.
  !> \authors Juliane Mai
  !> \date Jan 2013
  subroutine read_ascii_header( &
    path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, &
    ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, header_size)

    use mo_os, only : check_path_isfile
    use mo_constants, only : nodata_dp
    use mo_string_utils, only : tolower
    implicit none

    character(len = *), intent(in) :: path !< Name of file and its location
    integer(i4), intent(out) :: nrows !< number of rows (ny)
    integer(i4), intent(out) :: ncols !< number of columns (nx)
    real(dp), intent(out) :: xllcorner !< lower left corner (x)
    real(dp), intent(out) :: yllcorner !< lower left corner (y)
    real(dp), intent(out) :: cellsize !< cell size [m]
    real(dp), intent(out), optional :: nodata !< nodata value (default -9999.0)
    integer(i4), intent(in), optional :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in), optional :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in), optional :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in), optional :: ref_cellsize !< reference cellsize
    integer(i4), optional, intent(out) :: header_size !< number of lines of the header

    real(dp) :: file_nodata
    character(12) :: attribute
    integer(i4) :: io, fileunit

    !checking whether the file exists
    call check_path_isfile(path=path, raise=.true.)
    ! reading header from a file
    open (newunit = fileunit, file = path, status = 'old')
    read (fileunit, *) attribute, ncols
    read (fileunit, *) attribute, nrows
    read (fileunit, *) attribute, xllcorner
    read (fileunit, *) attribute, yllcorner
    read (fileunit, *) attribute, cellsize
    if (present(nodata) .or. present(header_size)) then
      read (fileunit, *, iostat=io) attribute, file_nodata
      ! EOF reached (nodata not present, use default value)
      if (io < 0) then
        if (present(nodata)) nodata = nodata_dp
        if (present(header_size)) header_size = 5_i4
      else if (tolower(attribute) == "nodata_value") then
        if (present(nodata)) nodata = file_nodata
        if (present(header_size)) header_size = 6_i4
      else
        if (present(nodata)) nodata = nodata_dp
        if (present(header_size)) header_size = 5_i4
      end if
    end if
    close(fileunit)

    ! compare headers always with reference header (intent in)
    if (present(ref_ncols)) then
      if ((ncols .ne. ref_ncols)) &
        call error_message('read_ascii: header not matching with reference header: ncols')
    end if
    if (present(ref_nrows)) then
      if ((nrows .ne. ref_nrows)) &
        call error_message('read_ascii: header not matching with reference header: nrows')
    end if
    if (present(ref_xllcorner)) then
      if ((abs(xllcorner - ref_xllcorner) .gt. tiny(1.0_dp))) &
        call error_message('read_ascii: header not matching with reference header: xllcorner')
    end if
    if (present(ref_yllcorner)) then
      if ((abs(yllcorner - ref_yllcorner) .gt. tiny(1.0_dp))) &
        call error_message('read_ascii: header not matching with reference header: yllcorner')
    end if
    if (present(ref_cellsize)) then
      if ((abs(cellsize - ref_cellsize)   .gt. tiny(1.0_dp))) &
        call error_message('read_ascii: header not matching with reference header: cellsize')
    end if

  end subroutine read_ascii_header

  !> \brief Write spatial data.
  !> \details Write spatial data to ascii file. Data will be transposed to be in xy order.
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine write_ascii_grid_dp(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    ! Subroutine arguments
    character(len=*), intent(in) :: path             !< File path to write ASCII grid
    integer(i4), intent(in) :: ncols                 !< Number of columns
    integer(i4), intent(in) :: nrows                 !< Number of rows
    real(dp), intent(in) :: xllcorner                !< X-coordinate of lower-left corner
    real(dp), intent(in) :: yllcorner                !< Y-coordinate of lower-left corner
    real(dp), intent(in) :: cellsize                 !< Size of the grid cells
    real(dp), intent(in) :: nodata                   !< Value indicating no data
    real(dp), intent(in), optional :: data(:,:)      !< 2D array of grid data
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (0 (default) or -1 for top-down, 1 for bottom-up)
    logical, intent(in), optional :: is_xy           !< Indicates if data is in (x,y) order (default .true.)

    ! Local variables
    integer(i4) :: i, j
    integer(i4) :: io, ierr
    logical :: is_bottom_up, is_xy_

    ! Set defaults
    is_bottom_up = .true.
    if (present(y_direction)) is_bottom_up = y_direction == bottom_up
    is_xy_ = .true.
    if (present(is_xy)) is_xy_ = is_xy

    ! Check dimensions if data is present
    if (present(data)) then
      if (is_xy_) then
        if (size(data,1) /= ncols .or. size(data,2) /= nrows) then
          call error_message('Error: data array dimensions mismatch (expected: ncols x nrows)')
        end if
      else
        if (size(data,1) /= nrows .or. size(data,2) /= ncols) then
          call error_message('Error: data array dimensions mismatch (expected: nrows x ncols)')
        end if
      end if
    end if

    ! Open file for writing
    open(newunit=io, file=path, status='replace', action='write', form='formatted', iostat=ierr)
    if (ierr /= 0) then
      call error_message('Error opening file: ', path)
    end if

    ! Write header with double precision
    write(io,'(A,I0)') 'ncols         ', ncols
    write(io,'(A,I0)') 'nrows         ', nrows
    write(io,'(A,F0.10)') 'xllcorner     ', xllcorner
    write(io,'(A,F0.10)') 'yllcorner     ', yllcorner
    write(io,'(A,F0.10)') 'cellsize      ', cellsize
    write(io,'(A,F0.10)') 'NODATA_value  ', nodata

    ! Write data array with double precision
    if (present(data)) then
      if (is_bottom_up) then
        if (is_xy_) then
          do i = nrows, 1, -1
            write(io, '(*(F0.10,1X))') (data(j,i), j=1,ncols)
          end do
        else
          do i = nrows, 1, -1
            write(io, '(*(F0.10,1X))') (data(i,j), j=1,ncols)
          end do
        end if
      else
        if (is_xy_) then
          do i = 1, nrows
            write(io, '(*(F0.10,1X))') (data(j,i), j=1,ncols)
          end do
        else
          do i = 1, nrows
            write(io, '(*(F0.10,1X))') (data(i,j), j=1,ncols)
          end do
        end if
      end if
    end if

    ! Close file
    close(io)

  end subroutine write_ascii_grid_dp

  !> \brief Write spatial data.
  !> \details Write spatial data to ascii file. Data will be transposed to be in xy order.
  !> \authors Sebastian Müller
  !> \date Mar 2025
  subroutine write_ascii_grid_i4(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata, data, y_direction, is_xy)
    implicit none

    ! Subroutine arguments
    character(len=*), intent(in) :: path             !< File path to write ASCII grid
    integer(i4), intent(in) :: ncols                 !< Number of columns
    integer(i4), intent(in) :: nrows                 !< Number of rows
    real(dp), intent(in) :: xllcorner                !< X-coordinate of lower-left corner
    real(dp), intent(in) :: yllcorner                !< Y-coordinate of lower-left corner
    real(dp), intent(in) :: cellsize                 !< Size of the grid cells
    integer(i4), intent(in) :: nodata                !< Integer value indicating no data
    integer(i4), intent(in), optional :: data(:,:)   !< 2D integer array of grid data
    integer(i4), intent(in), optional :: y_direction !< y-axis direction (0 (default) or -1 for top-down, 1 for bottom-up)
    logical, intent(in), optional :: is_xy           !< Indicates if data is in (x,y) order (default .true.)

    ! Local variables
    integer(i4) :: i, j
    integer(i4) :: io, ierr
    logical :: is_bottom_up, is_xy_

    ! Set defaults
    is_bottom_up = .false.
    if (present(y_direction)) is_bottom_up = y_direction == bottom_up
    is_xy_ = .true.
    if (present(is_xy)) is_xy_ = is_xy

    ! Check dimensions if data is present
    if (present(data)) then
      if (is_xy_) then
        if (size(data,1) /= ncols .or. size(data,2) /= nrows) then
          call error_message('Error: data array dimensions mismatch (expected: ncols x nrows)')
        end if
      else
        if (size(data,1) /= nrows .or. size(data,2) /= ncols) then
          call error_message('Error: data array dimensions mismatch (expected: nrows x ncols)')
        end if
      end if
    end if

    ! Open file for writing
    open(newunit=io, file=path, status='replace', action='write', form='formatted', iostat=ierr)
    if (ierr /= 0) then
      call error_message('Error opening file: ', path)
    end if

    ! Write header
    write(io,'(A,I0)') 'ncols         ', ncols
    write(io,'(A,I0)') 'nrows         ', nrows
    write(io,'(A,F0.10)') 'xllcorner     ', xllcorner
    write(io,'(A,F0.10)') 'yllcorner     ', yllcorner
    write(io,'(A,F0.10)') 'cellsize      ', cellsize
    write(io,'(A,I0)') 'NODATA_value  ', nodata

    ! Write integer data array
    if (present(data)) then
      if (is_bottom_up) then
        if (is_xy_) then
          do i = nrows, 1, -1
            write(io, '(*(I0,1X))') (data(j,i), j=1,ncols)
          end do
        else
          do i = nrows, 1, -1
            write(io, '(*(I0,1X))') (data(i,j), j=1,ncols)
          end do
        end if
      else
        if (is_xy_) then
          do i = 1, nrows
            write(io, '(*(I0,1X))') (data(j,i), j=1,ncols)
          end do
        else
          do i = 1, nrows
            write(io, '(*(I0,1X))') (data(i,j), j=1,ncols)
          end do
        end if
      end if
    end if

    ! Close file
    close(io)

  end subroutine write_ascii_grid_i4

  !> \brief Derive spatial index bounds.
  !> \details Derive spatial index bounds for fine grid cells covered by a coarse grid cell assuming matching lower-left corner.
  !> \authors Sebastian Müller
  !> \date Apr 2025
  subroutine id_bounds(factor, coarse_i, coarse_j, coarse_y_dir, coarse_ny, fine_y_dir, fine_nx, fine_ny, i_lb, i_ub, j_lb, j_ub)
    integer(i4), intent(in) :: factor
    integer(i4), intent(in) :: coarse_i !< i index on coarse grid (x-axis)
    integer(i4), intent(in) :: coarse_j !< j index on coarse grid (y-axis)
    integer(i4), intent(in) :: coarse_y_dir !< y-axis direction on coarse grid (0 - top-down, 1 - bottom-up)
    integer(i4), intent(in) :: coarse_ny !< maximum for j index on coarse grid (y-axis)
    integer(i4), intent(in) :: fine_y_dir !< y-axis direction on fine grid (0 - top-down, 1 - bottom-up)
    integer(i4), intent(in) :: fine_nx !< maximum for i index on fine grid (x-axis)
    integer(i4), intent(in) :: fine_ny !< maximum for j index on fine grid (y-axis)
    integer(i4), intent(out) :: i_lb !< lower bound for i on fine grid (x-axis)
    integer(i4), intent(out) :: i_ub !< upper bound for i on fine grid (x-axis)
    integer(i4), intent(out) :: j_lb !< lower bound for j on fine grid (y-axis)
    integer(i4), intent(out) :: j_ub !< upper bound for j on fine grid (y-axis)
    integer(i4) :: temp, ic, jc

    ic = coarse_i
    ! x-axis (unaffected by y-direction)
    i_lb = (ic - 1) * factor + 1
    ! constrain the range to fine grid extent
    i_ub = min(ic * factor, fine_nx)

    ! if coarse is top-down, switch assumed index to start at bottom
    jc = coarse_j
    if (coarse_y_dir == top_down) jc = coarse_ny - coarse_j + 1
    j_lb = (jc - 1) * factor + 1
    ! constrain the range to fine grid extent
    j_ub = min(jc * factor, fine_ny)

    ! if fine is top-down, move ids to other side (also switch upper and lower bound)
    if (fine_y_dir == top_down) then
      temp = j_lb
      j_lb = fine_ny - j_ub + 1
      j_ub = fine_ny - temp + 1
    end if

  end subroutine id_bounds

  !> \brief distance between two points on the sphere [m]
  pure real(dp) function dist_latlon(lat1, lon1, lat2, lon2)
    use mo_constants, only : RadiusEarth_dp, deg2rad_dp
    real(dp), intent(in) :: lat1
    real(dp), intent(in) :: lon1
    real(dp), intent(in) :: lat2
    real(dp), intent(in) :: lon2
    real(dp) :: theta1, phi1, theta2, phi2
    real(dp) :: term1, term2, term3, temp

    theta1 = deg2rad_dp * lon1
    phi1 = deg2rad_dp * lat1
    theta2 = deg2rad_dp * lon2
    phi2 = deg2rad_dp * lat2

    term1 = cos(phi1) * cos(theta1) * cos(phi2) * cos(theta2)
    term2 = cos(phi1) * sin(theta1) * cos(phi2) * sin(theta2)
    term3 = sin(phi1) * sin(phi2)
    temp = min(term1 + term2 + term3, 1.0_dp)
    dist_latlon = RadiusEarth_dp * acos(temp);
  end function dist_latlon

end module mo_grid
