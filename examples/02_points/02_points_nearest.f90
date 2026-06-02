!> \file    02_points/02_points_nearest.f90
!> \copydoc points_nearest_example

!> \example 02_points/02_points_nearest.f90
!> \copydoc points_nearest_example

!> \brief   Points nearest-neighbor regridding example.
!> \details This program reads SCC gauge coordinates from
!!          `info/river/scc_gauges.nc`, builds a small regular cartesian grid
!!          covering those gauges, and writes the nearest gauge ID for every
!!          grid cell to `scc_closest_gauge_grid.nc`.
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program points_nearest_example
  use mo_grid, only: grid_t, cartesian, bottom_up
  use mo_grid_io, only: var, output_dataset
  use mo_kind, only: dp, i4, i8
  use mo_points, only: points_t
  use mo_points_regridder, only: nearest_points_to_grid_t
  implicit none

  type(points_t), target :: gauges
  type(grid_t), target :: grid
  type(nearest_points_to_grid_t) :: regridder
  type(output_dataset) :: ds
  integer(i4), allocatable :: gauge_ids(:), closest_gauge_id(:)
  real(dp) :: xmin, xmax, ymin, ymax, cellsize
  integer(i4) :: i
  integer(i4), parameter :: nx = 5_i4, ny = 5_i4

  call gauges%from_netcdf("info/river/scc_gauges.nc", var="station")

  xmin = minval(gauges%x)
  xmax = maxval(gauges%x)
  ymin = minval(gauges%y)
  ymax = maxval(gauges%y)
  cellsize = max(xmax - xmin, ymax - ymin) / real(max(nx, ny) - 1_i4, dp)

  call grid%init(nx=nx, ny=ny, xllcorner=xmin - 0.5_dp * cellsize, yllcorner=ymin - 0.5_dp * cellsize, &
                 cellsize=cellsize, coordsys=cartesian, y_direction=bottom_up)

  allocate(gauge_ids(gauges%npoints))
  if (gauges%has_id()) then
    gauge_ids = int(gauges%id, i4)
  else
    gauge_ids = [(i, i=1_i4,int(gauges%npoints, i4))]
  end if

  call regridder%init(source_points=gauges, target_grid=grid)
  allocate(closest_gauge_id(grid%ncells))
  call regridder%execute(gauge_ids, closest_gauge_id)

  call ds%init("scc_closest_gauge_grid.nc", grid=grid, &
               vars=[var(name="closest_gauge_id", long_name="nearest SCC gauge ID", static=.true., dtype="i32", kind="i4")])
  call ds%update("closest_gauge_id", closest_gauge_id)
  call ds%write_static()
  call ds%close()
end program points_nearest_example
