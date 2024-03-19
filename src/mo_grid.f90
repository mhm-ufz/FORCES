!> \file    mo_grid.f90
!> \brief   \copybrief mo_grid
!> \details \copydetails mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_kind, only: i4, dp, sp

  implicit none
  private
  ! coordsys selector
  integer(i4), public, parameter :: coordsys_cart = 0_i4 !< Cartesian coordinate system.
  integer(i4), public, parameter :: coordsys_sph_deg = 1_i4 !< Spherical coordinates in degrees.
  ! integer(i4), public, parameter :: coordsys_sph_rad = 2_i4
  !> \class   coordsys_t
  !> \brief   Supported coordniate systems (cart, sph_deg).
  type, private :: coordsys_t
    integer(i4) :: cart = 0_i4 !< Cartesian coordinate system.
    integer(i4) :: sph_deg = 1_i4 !< Spherical coordinates in degrees.
    ! integer(i4), public :: sph_rad = 2_i4
  end type coordsys_t
  type(coordsys_t), public, parameter :: coordsys = coordsys_t(0_i4, 1_i4) !< Supported coordniate systems (cart, sph_deg).
  ! align selector
  integer(i4), public, parameter :: align_ll = 0_i4 !< align in lower left corner
  integer(i4), public, parameter :: align_lr = 1_i4 !< align in lower right corner
  integer(i4), public, parameter :: align_tl = 2_i4 !< align in top left corner
  integer(i4), public, parameter :: align_tr = 3_i4 !< align in top right corner
  !> \class   align_t
  !> \brief   Supported aligning corners (ll, lr, tl, tr).
  type, private :: align_t
    integer(i4) :: ll = 0_i4 !< align in lower left corner
    integer(i4) :: lr = 1_i4 !< align in lower right corner
    integer(i4) :: tl = 2_i4 !< align in top left corner
    integer(i4) :: tr = 3_i4 !< align in top right corner
  end type align_t
  type(align_t), public, parameter :: align = align_t(0_i4, 1_i4, 2_i4, 3_i4) !< Supported aligning corners (ll, lr, tl, tr).

end module mo_grid
