!> \file mo_kind.f90
!> \copydoc mo_kind

!> \brief Define number representations
!> \details This module declares the desired ranges and precisions of the number representations,
!! such as single precision or double precision, 32-bit or 64-bit integer, etc.
!> \copyright GNU Lesser General Public License http://www.gnu.org/licenses/
!> \changelog
!> - Sebastian Mueller, Jan 2022
!!   - added option for ISO_FORTRAN ENV
!!   - added quad precision for real and complex
!> \authors Juliane Mai, Matthias Cuntz
!> \date 2011-2014
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_kind

#ifdef FORCES_WITH_ISO_FORTRAN_ENV
  use, intrinsic :: iso_fortran_env, only: &
    int8, int16, int32, int64, real32, real64, real128
#else
  use, intrinsic :: iso_c_binding,   only: &
    int8    => c_int8_t, &
    int16   => c_short, &     ! c_int16_t
    int32   => c_int, &       ! c_int32_t
    int64   => c_long_long, & ! c_int64_t
    real32  => c_float, &
    real64  => c_double, &
    real128 => c_long_double
#endif

  IMPLICIT NONE

  !> 1 Byte Integer Kind
  INTEGER, PARAMETER :: i1  = int8
  !> 2 Byte Integer Kind
  INTEGER, PARAMETER :: i2  = int16
  !> 4 Byte Integer Kind
  INTEGER, PARAMETER :: i4  = int32
  !> 8 Byte Integer Kind
  INTEGER, PARAMETER :: i8  = int64
  !> Single Precision Real Kind
  INTEGER, PARAMETER :: sp  = real32
  !> Double Precision Real Kind
  INTEGER, PARAMETER :: dp  = real64
  !> Quad Precision Real Kind
  INTEGER, PARAMETER :: qp  = real128
  !> Single Precision Complex Kind
  INTEGER, PARAMETER :: spc = real32
  !> Double Precision Complex Kind
  INTEGER, PARAMETER :: dpc = real64
  !> Quad Precision Complex Kind
  INTEGER, PARAMETER :: qpc = real128
  !> Logical Kind
  INTEGER, PARAMETER :: lgt = KIND(.true.)

END MODULE mo_kind
