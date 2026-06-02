!> \file    mo_points.F90
!> \copydoc mo_points

!> \brief   Unstructured point-set handling.
!> \details Provides \ref points_t for data locations that are not arranged on
!!          a structured grid. A point set stores only coordinates and optional
!!          point identifiers; mesh topology is intentionally not part of this
!!          representation.
!> \authors Sebastian Müller
!> \date Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_points

  use mo_grid_constants, only: cartesian, spherical
  use mo_kind, only: i4, i8, dp, sp
  use mo_message, only: error_message
  use mo_spatial_index, only: spatial_index_t
  use mo_string_utils, only: num2str
  use mo_utils, only: optval
#ifdef FORCES_WITH_NETCDF
  use mo_grid_helper, only: is_x_axis, is_y_axis, is_lon_coord, is_lat_coord
  use mo_netcdf, only: NcDataset, NcDimension, NcVariable
  use mo_string_utils, only: splitString
#endif

  implicit none

  private

  public :: points_t
  public :: cartesian
  public :: spherical

  !> \class points_t
  !> \brief Unstructured set of point coordinates.
  !> \details Coordinates are stored as x/y for cartesian systems and lon/lat for
  !!          spherical systems, selected by \ref coordsys. The optional id array
  !!          can keep stable station or node identifiers separate from the
  !!          one-based array position used internally.
  type, public :: points_t
    integer(i4) :: coordsys = cartesian !< Coordinate system, cartesian or spherical.
    integer(i8) :: npoints = 0_i8       !< Number of points.
    real(dp), allocatable :: x(:)       !< Cartesian x or spherical longitude.
    real(dp), allocatable :: y(:)       !< Cartesian y or spherical latitude.
    integer(i8), allocatable :: id(:)   !< Optional point identifiers.
  contains
    procedure, public :: init => points_init
#ifdef FORCES_WITH_NETCDF
    procedure, private :: from_nc_file => points_from_nc_file
    procedure, private :: from_nc_dataset => points_from_nc_dataset
    generic, public :: from_netcdf => from_nc_file, from_nc_dataset
    procedure, private :: to_nc_file => points_to_nc_file
    procedure, private :: to_nc_dataset => points_to_nc_dataset
    generic, public :: to_netcdf => to_nc_file, to_nc_dataset
#endif
    procedure, public :: has_id => points_has_id
    procedure, public :: coords => points_coords
    procedure, public :: build_spatial_index => points_build_spatial_index
    procedure, private :: closest_point_id_scalar => points_closest_point_id_scalar
    procedure, private :: closest_point_id_batch => points_closest_point_id_batch
    generic, public :: closest_point_id => closest_point_id_scalar, closest_point_id_batch
    procedure, public :: is_matching => points_is_matching
  end type points_t

contains

  !> \brief Initialize a point set from coordinate vectors.
  subroutine points_init(this, x, y, coordsys, id)
    class(points_t), intent(inout) :: this
    real(dp), intent(in) :: x(:) !< x or longitude coordinates
    real(dp), intent(in) :: y(:) !< y or latitude coordinates
    integer(i4), optional, intent(in) :: coordsys !< coordinate system, defaults to cartesian
    integer(i8), optional, intent(in) :: id(:) !< optional stable point identifiers

    if (size(x, kind=i8) /= size(y, kind=i8)) call error_message("points % init: x and y size mismatch")
    this%coordsys = optval(coordsys, cartesian)
    if (.not.any(this%coordsys == [cartesian, spherical])) then
      call error_message("points % init: unknown coordsys value: ", num2str(this%coordsys))
    end if
    this%npoints = size(x, kind=i8)
    if (allocated(this%x)) deallocate(this%x)
    if (allocated(this%y)) deallocate(this%y)
    if (allocated(this%id)) deallocate(this%id)
    allocate(this%x(this%npoints), source=x)
    allocate(this%y(this%npoints), source=y)
    if (present(id)) then
      if (size(id, kind=i8) /= this%npoints) call error_message("points % init: id and coordinate size mismatch")
      allocate(this%id(this%npoints), source=id)
    end if
  end subroutine points_init

  !> \brief Check whether stable point identifiers are available.
  logical function points_has_id(this)
    class(points_t), intent(in) :: this
    points_has_id = allocated(this%id)
  end function points_has_id

  !> \brief Return coordinates as an `(npoints,2)` matrix.
  function points_coords(this) result(coords)
    class(points_t), intent(in) :: this
    real(dp) :: coords(this%npoints, 2)
    coords(:, 1) = this%x
    coords(:, 2) = this%y
  end function points_coords

  !> \brief Build a spatial nearest-neighbor index for the point coordinates.
  subroutine points_build_spatial_index(this, index)
    class(points_t), intent(in) :: this
    type(spatial_index_t), intent(inout) :: index !< initialized spatial index
    integer(i8), allocatable :: point_ids(:)
    integer(i8) :: i

    if (this%npoints < 1_i8) call error_message("points % build_spatial_index: points is empty")
    point_ids = [(i, i=1_i8,this%npoints)]
    if (this%coordsys == spherical) then
      call index%init_lonlat(this%coords(), point_ids)
    else
      call index%init(this%coords(), point_ids)
    end if
  end subroutine points_build_spatial_index

  !> \brief Find the nearest point id for one coordinate pair.
  integer(i8) function points_closest_point_id_scalar(this, coord) result(point_id)
    class(points_t), intent(in) :: this
    real(dp), intent(in) :: coord(2) !< x/y or lon/lat coordinate pair
    type(spatial_index_t) :: index

    call this%build_spatial_index(index)
    if (this%coordsys == spherical) then
      point_id = index%nearest_id_lonlat(coord(1), coord(2))
    else
      point_id = index%nearest_id(coord)
    end if
  end function points_closest_point_id_scalar

  !> \brief Find nearest point ids for a coordinate matrix.
  function points_closest_point_id_batch(this, coords) result(point_ids)
    class(points_t), intent(in) :: this
    real(dp), intent(in) :: coords(:, :) !< coordinate matrix with shape (:,2)
    integer(i8) :: point_ids(size(coords, 1))
    type(spatial_index_t) :: index

    if (size(coords, 2) /= 2_i4) call error_message("points % closest_point_id: coordinates need shape (:,2)")
    call this%build_spatial_index(index)
    if (this%coordsys == spherical) then
      point_ids = index%nearest_ids_lonlat(coords)
    else
      point_ids = index%nearest_ids(coords)
    end if
  end function points_closest_point_id_batch

  !> \brief Check whether two point sets describe the same coordinate locations.
  logical function points_is_matching(this, other)
    class(points_t), intent(in) :: this
    type(points_t), intent(in) :: other !< point set to compare against
    points_is_matching = this%coordsys == other%coordsys .and. this%npoints == other%npoints
    if (.not.points_is_matching) return
    points_is_matching = all(abs(this%x - other%x) <= 0.0_dp) .and. all(abs(this%y - other%y) <= 0.0_dp)
  end function points_is_matching

#ifdef FORCES_WITH_NETCDF
  !> \brief Read point coordinates from a NetCDF file path.
  subroutine points_from_nc_file(this, path, var, x_name, y_name, id_name)
    class(points_t), intent(inout) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: var !< variable whose dimensions/coordinates identify the point dimension
    character(*), optional, intent(in) :: x_name !< explicit x/lon coordinate variable name
    character(*), optional, intent(in) :: y_name !< explicit y/lat coordinate variable name
    character(*), optional, intent(in) :: id_name !< explicit point id variable name
    type(NcDataset) :: nc

    nc = NcDataset(path, "r")
    call points_from_nc_dataset(this, nc, var=var, x_name=x_name, y_name=y_name, id_name=id_name)
    call nc%close()
  end subroutine points_from_nc_file

  !> \brief Read point coordinates from an open NetCDF dataset.
  subroutine points_from_nc_dataset(this, nc, var, x_name, y_name, id_name)
    class(points_t), intent(inout) :: this
    type(NcDataset), intent(in) :: nc !< open NetCDF dataset
    character(*), optional, intent(in) :: var !< variable whose dimensions/coordinates identify the point dimension
    character(*), optional, intent(in) :: x_name !< explicit x/lon coordinate variable name
    character(*), optional, intent(in) :: y_name !< explicit y/lat coordinate variable name
    character(*), optional, intent(in) :: id_name !< explicit point id variable name

    type(NcVariable) :: xvar, yvar, base_var, idvar
    type(NcDimension), allocatable :: dims(:), xdims(:), ydims(:)
    character(len=256), allocatable :: coord_names(:)
    character(len=256) :: tmp, xname, yname, idname
    real(dp), allocatable :: x(:), y(:)
    integer(i8), allocatable :: ids(:)
    integer(i4) :: coordsys_, i
    logical :: var_is_coord
    logical :: have_var, have_coord_attr, have_xname, have_yname, have_idname

    have_var = present(var)
    have_coord_attr = .false.
    have_xname = .false.
    have_yname = .false.
    have_idname = .false.
    if (present(x_name)) then
      xname = trim(x_name)
      have_xname = .true.
    end if
    if (present(y_name)) then
      yname = trim(y_name)
      have_yname = .true.
    end if

    if (have_var) then
      base_var = nc%getVariable(var)
      dims = base_var%getDimensions()
      if (base_var%hasAttribute("coordinates")) then
        call base_var%getAttribute("coordinates", tmp)
        coord_names = splitString(trim(tmp), " ")
        if (size(coord_names) >= 2) then
          xname = trim(coord_names(1))
          yname = trim(coord_names(2))
          have_xname = .true.
          have_yname = .true.
          have_coord_attr = .true.
        end if
      end if
    end if

    if (.not.have_xname) then
      if (nc%hasVariable("x") .and. nc%hasVariable("y")) then
        xname = "x"
        yname = "y"
        have_xname = .true.
        have_yname = .true.
      else if (nc%hasVariable("lon") .and. nc%hasVariable("lat")) then
        xname = "lon"
        yname = "lat"
        have_xname = .true.
        have_yname = .true.
      else
        call error_message("points % from_netcdf: could not infer coordinate variables")
      end if
    end if

    if (.not.have_yname) call error_message("points % from_netcdf: missing y/lat coordinate variable")
    xvar = nc%getVariable(xname)
    yvar = nc%getVariable(yname)
    if (xvar%getRank() /= 1_i4 .or. yvar%getRank() /= 1_i4) &
      call error_message("points % from_netcdf: coordinate variables must be one-dimensional")
    xdims = xvar%getDimensions()
    ydims = yvar%getDimensions()
    if (xdims(1)%getLength64() /= ydims(1)%getLength64()) &
      call error_message("points % from_netcdf: x/y coordinate dimension mismatch")
    if (have_var) then
      if (.not.have_coord_attr) then
        if (.not.any([(trim(dims(i)%getName()) == trim(xdims(1)%getName()), i=1,size(dims))])) &
          call error_message("points % from_netcdf: selected variable does not use point coordinate dimension")
      end if
    end if

    coordsys_ = cartesian
    if (is_lon_coord(xvar) .or. is_lat_coord(yvar)) coordsys_ = spherical
    if (is_x_axis(yvar) .or. is_y_axis(xvar) .or. is_lat_coord(xvar) .or. is_lon_coord(yvar)) &
      call error_message("points % from_netcdf: coordinate variables seem to be in wrong order")
    call xvar%getData(x)
    call yvar%getData(y)

    if (present(id_name)) then
      idname = trim(id_name)
      have_idname = .true.
    else if (have_var) then
      if (base_var%getRank() == 1_i4) then
        var_is_coord = .false.
        if (trim(var) == trim(xname)) var_is_coord = .true.
        if (trim(var) == trim(yname)) var_is_coord = .true.
        if (.not.var_is_coord) then
          idname = trim(var)
          have_idname = .true.
        end if
      end if
    end if

    if (have_idname) then
      idvar = nc%getVariable(idname)
      if (idvar%getRank() /= 1_i4) call error_message("points % from_netcdf: id variable must be one-dimensional")
      call idvar%getData(ids)
      call this%init(x, y, coordsys=coordsys_, id=ids)
    else
      call this%init(x, y, coordsys=coordsys_)
    end if
  end subroutine points_from_nc_dataset

  !> \brief Write point coordinates to a NetCDF file path.
  subroutine points_to_nc_file(this, path, point_dim_name, x_name, y_name, id_name, double_precision, append)
    class(points_t), intent(in) :: this
    character(*), intent(in) :: path !< NetCDF file path
    character(*), optional, intent(in) :: point_dim_name !< point dimension name, defaults to "point"
    character(*), optional, intent(in) :: x_name !< x/lon coordinate variable name
    character(*), optional, intent(in) :: y_name !< y/lat coordinate variable name
    character(*), optional, intent(in) :: id_name !< point id variable name
    logical, optional, intent(in) :: double_precision !< write coordinates as f64 instead of f32
    logical, optional, intent(in) :: append !< append coordinates to an existing file
    type(NcDataset) :: nc
    character(1) :: fmode

    fmode = "w"
    if (optval(append, .false.)) fmode = "a"
    nc = NcDataset(path, fmode)
    call this%to_netcdf(nc, point_dim_name=point_dim_name, x_name=x_name, y_name=y_name, id_name=id_name, &
                        double_precision=double_precision)
    call nc%close()
  end subroutine points_to_nc_file

  !> \brief Write point coordinates to an open NetCDF dataset.
  subroutine points_to_nc_dataset(this, nc, point_dim_name, x_name, y_name, id_name, double_precision)
    class(points_t), intent(in) :: this
    type(NcDataset), intent(inout) :: nc !< open NetCDF dataset
    character(*), optional, intent(in) :: point_dim_name !< point dimension name, defaults to "point"
    character(*), optional, intent(in) :: x_name !< x/lon coordinate variable name
    character(*), optional, intent(in) :: y_name !< y/lat coordinate variable name
    character(*), optional, intent(in) :: id_name !< point id variable name
    logical, optional, intent(in) :: double_precision !< write coordinates as f64 instead of f32
    type(NcDimension) :: point_dim
    type(NcVariable) :: xvar, yvar, idvar
    character(:), allocatable :: dimname, xname, yname, idname, dtype
    logical :: double_precision_

    dimname = optval(point_dim_name, "point")
    if (this%coordsys == spherical) then
      xname = optval(x_name, "lon")
      yname = optval(y_name, "lat")
    else
      xname = optval(x_name, "x")
      yname = optval(y_name, "y")
    end if
    idname = optval(id_name, dimname)
    double_precision_ = optval(double_precision, .true.)
    dtype = "f32"
    if (double_precision_) dtype = "f64"

    point_dim = nc%setDimension(dimname, int(this%npoints, i4))
    xvar = nc%setVariable(xname, dtype, [point_dim])
    yvar = nc%setVariable(yname, dtype, [point_dim])

    if (this%coordsys == spherical) then
      call xvar%setAttribute("long_name", "longitude")
      call xvar%setAttribute("standard_name", "longitude")
      call xvar%setAttribute("units", "degrees_east")
      call yvar%setAttribute("long_name", "latitude")
      call yvar%setAttribute("standard_name", "latitude")
      call yvar%setAttribute("units", "degrees_north")
    else
      call xvar%setAttribute("long_name", "x coordinate of projection")
      call xvar%setAttribute("standard_name", "projection_x_coordinate")
      call xvar%setAttribute("units", "m")
      call yvar%setAttribute("long_name", "y coordinate of projection")
      call yvar%setAttribute("standard_name", "projection_y_coordinate")
      call yvar%setAttribute("units", "m")
    end if

    if (double_precision_) then
      call xvar%setData(this%x)
      call yvar%setData(this%y)
    else
      call xvar%setData(real(this%x, sp))
      call yvar%setData(real(this%y, sp))
    end if

    if (allocated(this%id)) then
      idvar = nc%setVariable(idname, "i64", [point_dim])
      call idvar%setAttribute("long_name", "Point ID")
      call idvar%setAttribute("coordinates", trim(xname) // " " // trim(yname))
      call idvar%setData(this%id)
    end if
  end subroutine points_to_nc_dataset
#endif

end module mo_points
