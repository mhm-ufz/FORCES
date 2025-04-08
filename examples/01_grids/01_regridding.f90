program regrid
  use mo_kind, only: dp
  use mo_constants, only: nodata_dp
  use mo_grid, only: grid
  use mo_regridder, only: regridder
  use mo_netcdf, only: NcDataset, NcVariable, NcDimension
  implicit none
  type(grid), target :: cgrid, fgrid
  type(regridder) :: upscaler
  type(NcDataset) :: nc
  type(NcDimension) :: x_dim, y_dim
  type(NcVariable) :: var
  real(dp), allocatable :: dem(:), dem_mat(:,:), cdem(:), cdem_mat(:,:), area(:,:)

  ! initialize fine grid from DEM ascii file
  call fgrid%from_ascii_file("./src/pf_tests/files/dem.asc")
  call fgrid%read_data("./src/pf_tests/files/dem.asc", dem_mat)
  allocate(dem(fgrid%ncells))
  dem = fgrid%pack_data(dem_mat)

  ! convert ascii DEM to NetCDF file
  nc = NcDataset("dem.nc", "w")
  ! write grid to file
  call fgrid%to_netcdf(nc)
  ! write data
  x_dim = nc%getDimension("x")
  y_dim = nc%getDimension("y")
  var = nc%setVariable("dem", "f64", [x_dim, y_dim])
  call var%setFillValue(nodata_dp)
  call var%setAttribute("missing_value", nodata_dp)
  call var%setData(dem_mat)
  call nc%close()

  ! generate coarser grid by upscaling factor
  cgrid = fgrid%derive_grid(upscaling_factor=16)
  allocate(cdem_mat(cgrid%nx, cgrid%ny))

  ! initialize regridder from source and target grids
  call upscaler%init(fgrid, cgrid)

  ! regrid dem to coarser scale (with mean upscaling by default)
  allocate(cdem(cgrid%ncells))
  call upscaler%execute(dem, cdem)
  cdem_mat = cgrid%unpack_data(cdem)

  ! convert result to NetCDF file
  nc = NcDataset("cdem.nc", "w")
  call cgrid%to_netcdf(nc)
  x_dim = nc%getDimension("x")
  y_dim = nc%getDimension("y")
  var = nc%setVariable("dem_mean", "f64", [x_dim, y_dim])
  call var%setFillValue(nodata_dp)
  call var%setAttribute("missing_value", nodata_dp)
  call var%setData(cdem_mat)

  ! also export cell area
  allocate(area(cgrid%nx,cgrid%ny))
  area = cgrid%unpack_data(cgrid%cell_area)
  var = nc%setVariable("area", "f64", [x_dim, y_dim])
  call var%setFillValue(nodata_dp)
  call var%setAttribute("missing_value", nodata_dp)
  call var%setData(area)
  call nc%close()

end program regrid
