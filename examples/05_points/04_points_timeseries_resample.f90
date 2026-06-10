!> \file    05_points/04_points_timeseries_resample.f90
!> \copydoc points_timeseries_resample_example

!> \example 05_points/04_points_timeseries_resample.f90
!> \copydoc points_timeseries_resample_example

!> \brief   Points time-series resampling example.
!> \details This program creates synthetic daily interval-mean discharge data
!!          for SCC gauges, shrinks the time span, resamples it to hourly data,
!!          and writes the result to `scc_resampled_hourly_discharge.nc`.
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
  use mo_timeseries, only: timeseries_axis_t, timeseries_t, daily, hourly, ts_interval, ts_mean
  implicit none

  type(points_t), target :: gauges
  type(points_input_dataset) :: inp
  type(points_series_output_dataset) :: out
  type(timeseries_axis_t) :: daily_axis, hourly_axis
  type(timeseries_t) :: daily_series, hourly_series
  type(var), allocatable :: input_vars(:), output_vars(:)
  integer(i4), allocatable :: station_ids(:), time_values(:), time_bnds(:, :)
  character(:), allocatable :: time_units
  real(dp) :: daily_values(3)
  integer(i8) :: i
  integer(i4) :: t

  allocate(input_vars(0))
  call inp%init("src/pf_tests/files/scc_gauges.nc", vars=input_vars, points=gauges, points_init_var="station")
  call inp%get_ids(station_ids)
  call inp%close()

  call daily_axis%init(datetime("2026-01-01"), datetime("2026-01-04"), timestep=daily)
  call daily_axis%resampled(hourly_axis, timestep=hourly, timeframe_start=datetime("2026-01-02"))
  call hourly_axis%to_cf(time_values, time_bnds, time_units)

  allocate(output_vars(0))
  call add_var(output_vars, var(name="station", long_name="SCC gauge ID", static=.true., dtype="i32", kind="i4"))
  call add_var(output_vars, var(name="discharge", long_name="hourly resampled synthetic discharge", units="m3 s-1", &
                                static=.false., dtype="f64", kind="dp"))

  call out%init("scc_resampled_hourly_discharge.nc", points=gauges, vars=output_vars, &
                time_values=time_values, time_units=time_units, time_bnds=time_bnds, point_dim_name="station")
  call out%write_static("station", station_ids)

  do i = 1_i8, gauges%n_points
    do t = 1_i4, size(daily_values)
      daily_values(t) = 10.0_dp * real(i, dp) + real(t, dp)
    end do
    call daily_series%init(daily_values, daily_axis, support=ts_interval, method=ts_mean)
    call daily_series%resample(hourly_axis, hourly_series)
    call out%write_series("discharge", i, hourly_series%values)
  end do
  call out%close()
end program points_timeseries_resample_example
