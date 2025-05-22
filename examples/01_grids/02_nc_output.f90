!> \file    02_nc_output.f90
!> \copydoc netcdf_output

!> \brief   Output example.
!> \details This program demonstrates writing a NetCDF file with a time dimension.
!!          It will write daily averages of a hourly quantity (a varying height).
!!          In addition, a static variable is written to the file.
!> \authors Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program netcdf_output
  use mo_kind, only: dp, i4
  use mo_grid, only: grid_t
  use mo_gridded_netcdf, only: var, output_dataset, center_timestamp
  use mo_datetime, only: datetime, timedelta
  implicit none
  type(datetime) :: start_time, end_time, current_time
  type(timedelta) :: step
  type(grid_t), target :: grid
  type(output_dataset) :: ds
  real(dp), allocatable :: dem(:,:), height(:)
  type(var), allocatable :: vars(:)
  real(dp) :: factor

  start_time = datetime("2025-01-01")
  end_time = datetime("2025-07-01")
  step = timedelta(hours=1_i4)

  ! initialize fine grid from DEM ascii file
  call grid%from_ascii_file("./src/pf_tests/files/dem.asc")
  call grid%read_data("./src/pf_tests/files/dem.asc", dem)
  height = grid%pack(dem)

  ! extend vars array with variables to add
  allocate(vars(0))
  vars = [vars, var(name="dem", units="m", static=.true.)]
  vars = [vars, var(name="height", units="m", avg=.true., static=.false.)]

  call ds%init(path="height.nc", grid=grid, vars=vars, start_time=start_time, delta="hours", timestamp=center_timestamp)
  call ds%update("dem", height) ! will be written with first ds%write call

  current_time = start_time
  factor = 0.0_dp
  do while(current_time < end_time)
    factor = factor + 0.01_dp
    current_time = current_time + step
    call ds%update("height", height * cos(factor))
    ! on each new day, write the output for the previous day
    if (current_time%is_new_day()) call ds%write(current_time)
  end do
  call ds%close()

end program netcdf_output
