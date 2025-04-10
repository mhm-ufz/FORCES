!> \file    01_regridding.f90
!> \copydoc regrid

!> \brief   Regridding example for a DEM.
!> \details This program demonstrates regridding of a DEM read from an ascii file and saving the result to a NetCDF file.
!> \authors Sebastian Mueller
!> \date    Mar 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program regrid
  use mo_kind, only: dp
  use mo_constants, only: nodata_dp
  use mo_grid, only: grid
  use mo_regridder, only: regridder, up_a_mean
  use mo_netcdf, only: NcDataset, NcVariable, NcDimension
  implicit none
  type(grid), target :: cgrid, fgrid
  type(regridder) :: upscaler
  type(NcDataset) :: nc
  type(NcDimension) :: x_dim, y_dim
  type(NcVariable) :: var
  real(dp), allocatable :: dem(:,:), cdem(:,:), area(:,:)

  ! initialize fine grid from DEM ascii file
  call fgrid%from_ascii_file("./src/pf_tests/files/dem.asc")
  call fgrid%read_data("./src/pf_tests/files/dem.asc", dem)

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
  call var%setData(dem)
  call nc%close()

  ! generate coarser grid by upscaling factor
  cgrid = fgrid%derive_grid(upscaling_factor=16)
  allocate(cdem(cgrid%nx, cgrid%ny))

  ! initialize regridder from source and target grids
  call upscaler%init(fgrid, cgrid)

  ! regrid dem to coarser scale (with mean upscaling by default)
  call upscaler%execute(dem, cdem, upscaling_operator=up_a_mean)

  ! convert result to NetCDF file
  nc = NcDataset("cdem.nc", "w")
  call cgrid%to_netcdf(nc)
  x_dim = nc%getDimension("x")
  y_dim = nc%getDimension("y")
  var = nc%setVariable("dem_mean", "f64", [x_dim, y_dim])
  call var%setFillValue(nodata_dp)
  call var%setAttribute("missing_value", nodata_dp)
  call var%setData(cdem)

  ! also export cell area
  allocate(area(cgrid%nx,cgrid%ny))
  area = cgrid%unpack_data(cgrid%cell_area)
  var = nc%setVariable("area", "f64", [x_dim, y_dim])
  call var%setFillValue(nodata_dp)
  call var%setAttribute("missing_value", nodata_dp)
  call var%setData(area)
  call nc%close()

end program regrid
