!> \file mo_kind.f90

!> \brief Define number representations

!> \details This module declares the desired ranges and precisions of the number representations,
!> such as single precision or double precision, 32-bit or 46-bit integer, etc.
!> It confirms mostly with the nrtype module of Numerical Recipes in f90.

!> \authors Juliane Mai, Matthias Cuntz, Nov 2011
!> \date 2011-2014
!> \copyright GNU Lesser General Public License http://www.gnu.org/licenses/

!  Number model from which the SELECTED_REAL_KIND are requested:
!                   4 byte REAL      8 byte REAL
!          IEEE:    precision =>  6   precision =>   15
!                   exponent  => 37   exponent  =>  307
!          CRAY:        -            precision =>   13
!                                    exponent  => 2465

! Written  Juliane Mai, Matthias Cuntz, Nov 2011
! Modified Matthias Cuntz, Nov 2011 - private/public
!                                   - documentation
!                                   - removed tab characters
!          Matthias Cuntz, May 2014 - iso_fortran_env and iso_c_binding
!          S. Mueller, Dec 2019 - remove NR specific types

! License
! -------
! This file is part of the UFZ Fortran library.

! The UFZ Fortran library is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! The UFZ Fortran library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Lesser General Public License for more details.

! You should have received a copy of the GNU Lesser General Public License
! along with the UFZ Fortran library (cf. gpl.txt and lgpl.txt).
! If not, see <http://www.gnu.org/licenses/>.

! Copyright 2011-2014 Matthias Cuntz, Juliane Mai

MODULE mo_kind

  !use, intrinsic :: iso_c_binding, only: sp=>c_float
  !use, intrinsic :: iso_c_binding, only: dp=>c_double
  use, intrinsic :: iso_c_binding, only: spc=>c_float_complex
  use, intrinsic :: iso_c_binding, only: dpc=>c_double_complex
  !use, intrinsic :: iso_c_binding, only: qp=>c_float128
  use, intrinsic :: iso_c_binding, only: i2=>c_short
  use, intrinsic :: iso_c_binding, only: i4=>c_int
  use, intrinsic :: iso_c_binding, only: i8=>c_long_long
  ! this is defined here https://github.com/fortran-lang/stdlib/ like so
  use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
  ! use iso_fortran_env, only: int8, int16, int32, int64
  !use, intrinsic :: iso_c_binding, only: int8=>c_int8_t
  !use, intrinsic :: iso_c_binding, only: int16=>c_int16_t
  !use, intrinsic :: iso_c_binding, only: int32=>c_int32_t
  !use, intrinsic :: iso_c_binding, only: int64=>c_int64_t

implicit none

  private
  !public sp, dp, qp, int8, int16, int32, int64
  public sp, dp, qp, i1, i2, i4, i8, lgt

  integer, parameter :: i1 = selected_int_kind(2)
  integer, parameter :: lgt = kind(.true.)

END MODULE mo_kind
