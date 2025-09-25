!> \file    01_grids/01_regridding.f90
!> \copydoc regrid

!> \example 01_grids/01_regridding.f90
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
  use mo_grid, only: grid_t
  use mo_grid_scaler, only: scaler_t, up_a_mean
  use mo_grid_io, only: var, output_dataset
  implicit none
  type(grid_t), target :: cgrid, fgrid
  type(scaler_t) :: upscaler
  type(output_dataset) :: ds1, ds2
  real(dp), allocatable :: dem(:,:), cdem(:)
  type(var), allocatable :: vars(:)

  ! initialize fine grid from DEM ascii file
  call fgrid%from_ascii_file("./src/pf_tests/files/dem.asc")
  call fgrid%read_data("./src/pf_tests/files/dem.asc", dem)

  ! extend vars array with variables to add
  allocate(vars(0))
  vars = [vars, var(name="dem", standard_name="height_above_mean_sea_level", units="m", static=.true.)]
  vars = [vars, var(name="area", standard_name="cell_area", units="m2", static=.true.)]

  call ds1%init(path="fdem.nc", grid=fgrid, vars=vars(:1))
  call ds1%update("dem", fgrid%pack(dem))
  call ds1%write()
  call ds1%close()

  ! generate coarser grid by upscaling factor
  cgrid = fgrid%derive_grid(upscaling_factor=16)
  allocate(cdem(cgrid%ncells))

  ! initialize regridder from source and target grids
  call upscaler%init(fgrid, cgrid)

  ! regrid dem to coarser scale (with mean upscaling by default)
  call upscaler%execute(dem, cdem, upscaling_operator=up_a_mean)

  call ds2%init(path="cdem.nc", grid=cgrid, vars=vars)
  call ds2%update("dem", cdem)
  call ds2%update("area", cgrid%cell_area)
  call ds2%write()
  call ds2%close()

end program regrid
