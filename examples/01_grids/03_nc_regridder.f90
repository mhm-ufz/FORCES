!> \file    01_grids/03_nc_regridder.f90
!> \copydoc netcdf_regridder

!> \example 01_grids/03_nc_regridder.f90
!> \copydoc netcdf_regridder

!> \brief   NetCDF regridding example.
!> \details This program demonstrates regridding a static variable from a NetCDF file.
!!          It provides a command line interface to select the files, the variable,
!!          the upscaling operator and the factor.
!!          Call `main -h` for help.
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program netcdf_regridder
  use mo_kind, only: dp, i4
  use mo_cli, only: cli_parser
  use mo_grid, only: grid_t
  use mo_grid_scaler, only: scaler_t, up_a_mean, up_h_mean, up_g_mean
  use mo_grid_io, only: var, output_dataset, input_dataset
  implicit none
  type(cli_parser) :: parser
  type(grid_t), target :: grid_i, grid_o
  type(scaler_t) :: upscaler
  type(output_dataset) :: ds_o
  type(input_dataset) :: ds_i
  type(var) :: var_meta
  real(dp), allocatable :: dat_i(:,:), dat_o(:)
  integer(i4) :: factor, operator
  character(:), allocatable :: name, file_i, file_o, fac
  ! setup the CLI
  parser = cli_parser( &
    description='Simple regridder.', &
    add_version_option=.true., version='1.0')
  call parser%add_option( &
    'infile', 'i', has_value=.true., default="./src/pf_tests/files/dem.nc", help='Input netcdf file.')
  call parser%add_option( &
    'outfile', 'o', has_value=.true., default="output.nc", help='Output netcdf file.')
  call parser%add_option( &
    'var', 'v', has_value=.true., default="dem", help='Variable in the input netcdf file.')
  call parser%add_option( &
    name='factor', s_name='f', has_value=.true., default="2", help='Integer upscaling factor > 1')
  call parser%add_option( &
    name='upscaler', s_name='u', has_value=.true., default="a", &
    help='Upscaling operator to use ("a" - arithmetic mean, "g" - geometric mean, "h" - harmonic mean)')
  ! parse the command
  call parser%parse()
  name = parser%option_value('var')
  file_i = parser%option_value('infile')
  file_o = parser%option_value('outfile')
  fac = parser%option_value('factor')
  read(fac,*) factor ! convert string to integer
  operator = up_a_mean ! default
  select case(parser%option_value('upscaler'))
    case("g")
      operator = up_g_mean
    case("h")
      operator = up_h_mean
  end select
  ! read
  call grid_i%from_netcdf(file_i, name)
  call ds_i%init(path=file_i, grid=grid_i, vars=[var(name=name, static=.true.)])
  allocate(dat_i(grid_i%nx, grid_i%ny))
  call ds_i%read(name, dat_i)
  var_meta = ds_i%meta(name) ! copy all meta data of the variable
  call ds_i%close()
  ! upscale
  grid_o = grid_i%derive_grid(upscaling_factor=factor)
  call upscaler%init(grid_i, grid_o, upscaling_operator=operator)
  allocate(dat_o(grid_o%ncells))
  call upscaler%execute(dat_i, dat_o)
  ! write
  call ds_o%init(file_o, grid=grid_o, vars=[var_meta])
  call ds_o%update(name, dat_o)
  call ds_o%write_static()
  call ds_o%close()
end program netcdf_regridder
