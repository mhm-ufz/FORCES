!> \file    05_points/03_points_timeseries_io.f90
!> \copydoc points_timeseries_io_example

!> \example 05_points/03_points_timeseries_io.f90
!> \copydoc points_timeseries_io_example

!> \brief   Points time-series NetCDF I/O example.
!> \details This program reads SCC gauge coordinates and IDs from
!!          `src/pf_tests/files/scc_gauges.nc` and writes static station IDs plus
!!          temporal synthetic discharge series to `scc_synthetic_discharge_timeseries.nc`.
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program points_timeseries_io_example
  use mo_datetime, only: datetime
  use mo_grid_io, only: add_var, var
  use mo_kind, only: dp, i4, i8
  use mo_points, only: points_t
  use mo_points_io, only: points_input_dataset, points_series_output_dataset
  use mo_timeseries, only: time_t, daily
  implicit none

  type(points_t), target :: gauges
  type(points_input_dataset) :: inp
  type(points_series_output_dataset) :: out
  type(time_t) :: time_axis
  type(var), allocatable :: input_vars(:), output_vars(:)
  integer(i4), allocatable :: station_ids(:)
  real(dp), allocatable :: discharge(:)
  integer(i8) :: i
  integer(i4) :: t

  allocate(input_vars(0))
  call inp%init("src/pf_tests/files/scc_gauges.nc", vars=input_vars, points=gauges, points_init_var="station")
  call inp%get_ids(station_ids)
  call inp%close()

  call time_axis%init(datetime("2026-01-01"), datetime("2026-01-08"), timestep=daily)

  allocate(output_vars(0))
  call add_var(output_vars, var(name="discharge", long_name="synthetic discharge", units="m3 s-1"))

  call out%init("scc_synthetic_discharge_timeseries.nc", points=gauges, vars=output_vars, &
                time_axis=time_axis, point_dim_name="station")
  call out%set_ids(station_ids, long_name="SCC gauge ID")

  allocate(discharge(time_axis%n_times()))
  do i = 1_i8, gauges%n_points
    do t = 1_i4, size(discharge)
      discharge(t) = 10.0_dp * real(i, dp) + sin(0.25_dp * real(t, dp) + real(i, dp))
    end do
    call out%write_series("discharge", i, discharge)
  end do
  call out%close()
end program points_timeseries_io_example
