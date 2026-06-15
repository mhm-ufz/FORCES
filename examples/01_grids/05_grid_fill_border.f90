!> \file    01_grids/05_grid_fill_border.f90
!> \brief   Write DEM border cells and nearest-filled DEM to NetCDF.
program grid_fill_border
  use mo_grid, only: grid_t
  use mo_grid_io, only: output_dataset, var
  use mo_kind, only: dp, i4, i8
  implicit none

  type(grid_t), target :: grid, full_grid
  type(output_dataset) :: ds
  character(len=512) :: input_path, output_path
  logical, allocatable :: border(:, :), full_mask(:, :)
  integer(i4), allocatable :: border_data(:, :), border_packed(:)
  integer(i8), allocatable :: fill_id(:)
  real(dp), allocatable :: dem(:, :), dem_packed(:), dem_filled(:)

  input_path = "src/pf_tests/files/dem.asc"
  output_path = "dem_border_filled.nc"
  if (command_argument_count() >= 1_i4) call get_command_argument(1, input_path)
  if (command_argument_count() >= 2_i4) call get_command_argument(2, output_path)

  call grid%from_ascii_file(trim(input_path))
  call grid%read_data(trim(input_path), dem)

  allocate(full_mask(grid%nx, grid%ny), source=.true.)
  call grid%copy_to(full_grid, mask=full_mask)

  call grid%border_mask(border)
  allocate(border_data(grid%nx, grid%ny), source=0_i4)
  where (border) border_data = 1_i4
  border_packed = full_grid%pack(border_data)

  dem_packed = grid%pack(dem)
  call grid%fill_ids(full_grid, fill_id)
  allocate(dem_filled(full_grid%ncells))
  dem_filled = dem_packed(fill_id)

  call ds%init( &
    path=trim(output_path), &
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
