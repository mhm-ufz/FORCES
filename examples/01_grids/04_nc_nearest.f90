!> \file    01_grids/04_nc_nearest.f90
!> \copydoc netcdf_nearest_regridder

!> \example 01_grids/04_nc_nearest.f90
!> \copydoc netcdf_nearest_regridder

!> \brief   NetCDF nearest-neighbor regridding example.
!> \details This program demonstrates nearest-neighbor remapping from a
!!          projected grid with auxiliary lon/lat coordinates onto a regular
!!          lat-lon target grid read from a second NetCDF file.
!!          Call `main -h` for help.
!> \authors Sebastian Mueller
!> \date    Mar 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program netcdf_nearest_regridder
  use mo_kind, only: dp
  use mo_cli, only: cli_parser
  use mo_grid, only: grid_t
  use mo_grid_scaler, only: nearest_regridder_t
  use mo_grid_io, only: var, output_dataset, input_dataset
  implicit none

  type(cli_parser) :: parser
  type(grid_t), target :: grid_i, grid_o
  type(nearest_regridder_t) :: regridder
  type(output_dataset) :: ds_o
  type(input_dataset) :: ds_i
  type(var) :: var_meta
  real(dp), allocatable :: dat_i(:, :), dat_o(:)
  logical :: derive_target_mask
  character(:), allocatable :: name, file_i, file_g, file_o, grid_var

  parser = cli_parser( &
    description='Nearest-neighbor regridder for NetCDF grids.', &
    add_version_option=.true., version='1.0')
  call parser%add_option( &
    'infile', 'i', has_value=.true., default='./src/pf_tests/files/elevation.nc', help='Input netcdf file.')
  call parser%add_option( &
    'gridfile', 'g', has_value=.true., default='./src/pf_tests/files/latlon_grid.nc', &
    help='Target grid netcdf file.')
  call parser%add_option( &
    'outfile', 'o', has_value=.true., default='output_nearest.nc', help='Output netcdf file.')
  call parser%add_option( &
    'var', 'v', has_value=.true., default='dem', help='Variable in the input netcdf file.')
  call parser%add_option( &
    'grid-var', 'm', has_value=.true., default='mask', help='Mask variable used to initialize the target grid.')
  call parser%add_option( &
    'derive-target-mask', 'd', help='Derive the target mask by source-cell containment.')

  call parser%parse()
  name = parser%option_value('var')
  file_i = parser%option_value('infile')
  file_g = parser%option_value('gridfile')
  file_o = parser%option_value('outfile')
  grid_var = parser%option_value('grid-var')
  derive_target_mask = parser%option_was_read('derive-target-mask')

  call ds_i%init(path=file_i, grid=grid_i, vars=[var(name=name, static=.true.)], grid_init_var=name)
  allocate(dat_i(grid_i%nx, grid_i%ny))
  call ds_i%read(name, dat_i)
  var_meta = ds_i%meta(name)
  call ds_i%close()

  call grid_o%from_netcdf(file_g, grid_var)
  call regridder%init(grid_i, grid_o, derive_target_mask=derive_target_mask)

  allocate(dat_o(grid_o%ncells))
  call regridder%execute(dat_i, dat_o)

  call ds_o%init(file_o, grid=grid_o, vars=[var_meta])
  call ds_o%update(name, dat_o)
  call ds_o%write_static()
  call ds_o%close()
end program netcdf_nearest_regridder
