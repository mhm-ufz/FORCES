!> \file    01_grids/05_grid_fill_border.f90
!> \brief   Write DEM border cells and nearest-filled DEM to NetCDF.
program grid_fill_border
  use mo_cli, only: cli_parser
  use mo_grid, only: grid_t
  use mo_grid_io, only: output_dataset, var
  use mo_grid_scaler, only: scaler_t
  use mo_kind, only: dp, i4
  implicit none

  type(cli_parser) :: parser
  type(grid_t), target :: grid, full_grid
  type(scaler_t) :: fill_scaler
  type(output_dataset) :: ds
  character(:), allocatable :: input_path, output_path
  logical, allocatable :: border(:, :), full_mask(:, :)
  integer(i4), allocatable :: border_data(:, :), border_packed(:)
  real(dp), allocatable :: dem(:, :), dem_filled(:)

  parser = cli_parser( &
    description='Write DEM border cells and nearest-filled DEM to NetCDF.', &
    add_version_option=.true., version='1.0')
  call parser%add_option( &
    'infile', 'i', has_value=.true., default='src/pf_tests/files/dem.asc', help='Input DEM ASCII grid file.')
  call parser%add_option( &
    'outfile', 'o', has_value=.true., default='dem_border_filled.nc', help='Output NetCDF file.')
  call parser%parse()
  input_path = parser%option_value('infile')
  output_path = parser%option_value('outfile')

  call grid%from_ascii_file(input_path)
  call grid%read_data(input_path, dem)

  allocate(full_mask(grid%nx, grid%ny), source=.true.)
  call grid%copy_to(full_grid, mask=full_mask)

  call grid%border_mask(border)
  allocate(border_data(grid%nx, grid%ny), source=0_i4)
  where (border) border_data = 1_i4
  border_packed = full_grid%pack(border_data)

  allocate(dem_filled(full_grid%ncells))
  call fill_scaler%init(grid, full_grid, fill_missing=.true., use_index_distance=.true.)
  call fill_scaler%execute(dem, dem_filled)

  call ds%init( &
    path=output_path, &
    grid=full_grid, &
    vars=[ &
      var(name="border", long_name="DEM source mask border", units="1", dtype="i32", kind="i4", static=.true.), &
      var(name="dem_filled", long_name="DEM filled by nearest source border cells", units="m", dtype="f64", kind="dp", static=.true.) &
    ])
  call ds%update("border", border_packed)
  call ds%update("dem_filled", dem_filled)
  call ds%write_static()
  call ds%close()
end program grid_fill_border
