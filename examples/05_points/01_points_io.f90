!> \file    05_points/01_points_io.f90
!> \copydoc points_io_example

!> \example 05_points/01_points_io.f90
!> \copydoc points_io_example

!> \brief   Points NetCDF I/O example.
!> \details This program reads SCC gauge coordinates from
!!          `src/pf_tests/files/scc_gauges.nc` and writes a temporal synthetic discharge
!!          time series on those gauges to `scc_synthetic_discharge.nc`.
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program points_io_example
  use mo_datetime, only: datetime, timedelta
  use mo_grid_io, only: add_var, var, daily, end_timestamp, time_units_delta
  use mo_kind, only: dp, i4, i8
  use mo_points, only: points_t
  use mo_points_io, only: points_output_dataset
  implicit none

  type(points_t), target :: gauges
  type(points_output_dataset) :: ds
  type(var), allocatable :: vars(:)
  type(datetime) :: start_time, end_time, current_time
  type(timedelta) :: model_step
  real(dp), allocatable :: discharge(:)
  real(dp) :: phase
  integer(i8) :: i

  call gauges%from_netcdf("src/pf_tests/files/scc_gauges.nc", var="station")

  allocate(vars(0))
  call add_var(vars, var(name="discharge", long_name="synthetic discharge", units="m3 s-1", &
                         static=.false., dtype="f64", kind="dp"))

  start_time = datetime("2026-01-01")
  end_time = datetime("2026-01-08")
  model_step = timedelta(hours=1_i4)

  allocate(discharge(gauges%n_points))
  call ds%init(path="scc_synthetic_discharge.nc", points=gauges, vars=vars, start_time=start_time, &
               delta=time_units_delta(daily, end_timestamp), timestamp=end_timestamp, point_dim_name="station")

  current_time = start_time
  phase = 0.0_dp
  do while(current_time < end_time)
    current_time = current_time + model_step
    phase = phase + 0.25_dp

    do i = 1_i8, gauges%n_points
      discharge(i) = 10.0_dp * real(i, dp) + sin(phase + real(i, dp))
    end do
    call ds%update("discharge", discharge)

    if (current_time%is_new_day()) call ds%write(current_time)
  end do
  call ds%close()
end program points_io_example
