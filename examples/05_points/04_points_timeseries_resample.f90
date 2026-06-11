!> \file    05_points/04_points_timeseries_resample.f90
!> \copydoc points_timeseries_resample_example

!> \example 05_points/04_points_timeseries_resample.f90
!> \copydoc points_timeseries_resample_example

!> \brief   Points time-series resampling example.
!> \details This program creates synthetic daily interval-mean discharge data
!!          for SCC gauges, shrinks the time span, resamples it to hourly data,
!!          and writes the result to `resampled_hourly_discharge.nc`.
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program points_timeseries_resample_example
  use mo_datetime, only: datetime
  use mo_grid_io, only: add_var, var
  use mo_kind, only: dp, i4, i8
  use mo_points, only: points_t
  use mo_points_io, only: points_input_dataset, points_series_output_dataset
  use mo_timeseries, only: time_t, resampler_t, daily, hourly, ts_interval, ts_mean
  implicit none

  type(points_t), target :: gauges
  type(points_input_dataset) :: inp
  type(points_series_output_dataset) :: out
  type(time_t) :: daily_axis, hourly_axis
  type(resampler_t) :: resampler
  type(var), allocatable :: input_vars(:), output_vars(:)
  integer(i4), allocatable :: station_ids(:)
  real(dp) :: daily_values(3), hourly_values(48)
  integer(i8) :: i
  integer(i4) :: t

  allocate(input_vars(0))
  call inp%init("src/pf_tests/files/scc_gauges.nc", vars=input_vars, points=gauges, points_init_var="station")
  call inp%get_ids(station_ids)
  call inp%close()

  call daily_axis%init(datetime("2026-01-01"), datetime("2026-01-04"), timestep=daily)
  hourly_axis = daily_axis%resampled(timestep=hourly, timeframe_start=datetime("2026-01-02"))
  call resampler%init(daily_axis, hourly_axis, support=ts_interval, method=ts_mean)

  allocate(output_vars(0))
  call add_var(output_vars, var(name="discharge", long_name="hourly resampled synthetic discharge", units="m3 s-1"))

  call out%init("resampled_hourly_discharge.nc", points=gauges, vars=output_vars, time_axis=hourly_axis, point_dim_name="station")
  call out%set_ids(station_ids, long_name="gauge ID")

  do i = 1_i8, gauges%n_points
    do t = 1_i4, size(daily_values)
      daily_values(t) = 10.0_dp * real(i, dp) + real(t, dp)
    end do
    call resampler%execute(daily_values, hourly_values)
    call out%write_series("discharge", i, hourly_values)
  end do
  call out%close()
end program points_timeseries_resample_example
