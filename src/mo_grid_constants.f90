!> \file    mo_grid_constants.f90
!> \copydoc mo_grid_constants

!> \brief   Shared constants for grid handling.
!> \details This module owns the selector constants shared by `mo_grid`
!!          and helper modules to avoid duplicated definitions and keep
!!          grid-related APIs consistent.
!> \authors Sebastian Mueller
!> \date    Mar 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger:
!!           All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid_constants

  use mo_kind, only: i4

  implicit none

  public

  !> \name Coordinate System Selectors
  !> \brief Constants to specify the coordinate system in a grid.
  !!@{
  integer(i4), parameter :: cartesian = 0_i4 !<    Cartesian coordinate system.
  integer(i4), parameter :: spherical = 1_i4 !< Spherical coordinates in degrees.
  !!@}

  !> \name Y-Axis Direction Selectors
  !> \brief Constants to specify the y-axis direction in a grid.
  !!@{
  integer(i4), parameter :: keep_y = -1_i4 !< keep y-axis direction.
  integer(i4), parameter :: top_down = 0_i4 !< y-axis with decreasing values.
  integer(i4), parameter :: bottom_up = 1_i4 !< y-axis with increasing values.
  !!@}

  !> \name Cell Area Calculation Selectors
  !> \brief Constants to specify the method for calculating cell area.
  !!@{
  integer(i4), parameter :: area_sum = 0_i4 !< Calculate cell area as sum of the area of sub-cells.
  integer(i4), parameter :: area_full = 1_i4 !< Calculate cell area as full cell.
  integer(i4), parameter :: area_count = 2_i4 !< Calculate cell area by count fraction of valid sub-cells.
  !!@}

end module mo_grid_constants
