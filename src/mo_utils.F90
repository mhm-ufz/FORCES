!> \file mo_utils.f90
!> \brief \copybrief mo_utils
!> \details \copydetails mo_utils

!> \brief General utilities for the CHS library
!> \details This module provides general utilities such as comparisons of two reals.
!> \authors Matthias Cuntz, Juliane Mai
!> \date Feb 2014
MODULE mo_utils

  ! Written  Matthias Cuntz, Juliane Mai, Feb 2014
  ! Modified Matthias Cuntz, Juliane Mai, Feb 2014 - equal, notequal
  !          Matthias Cuntz,              May 2014 - swap
  !          Matthias Cuntz,              May 2014 - is_finite, is_nan, is_normal, special_value

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
  ! along with the UFZ Fortran library (LICENSE).
  ! If not, see <http://www.gnu.org/licenses/>.

  ! Copyright 2014 Matthias Cuntz, Juliane Mai

  USE mo_kind, only : sp, dp, i1, i4, i8
  USE mo_string_utils, only : toupper

  IMPLICIT NONE

  PUBLIC :: equal        ! a == b, a .eq. b
  PUBLIC :: greaterequal ! a >= b, a .ge. b
  PUBLIC :: lesserequal  ! a <= b, a .le. b
  PUBLIC :: notequal     ! a /= b, a .ne. b
  PUBLIC :: eq           ! a == b, a .eq. b
  PUBLIC :: ge           ! a >= b, a .ge. b
  PUBLIC :: le           ! a <= b, a .le. b
  PUBLIC :: ne           ! a /= b, a .ne. b
  PUBLIC :: is_finite    ! .true. if not IEEE Inf and not IEEE NaN
  PUBLIC :: is_nan       ! .true. if IEEE NaN
  PUBLIC :: is_normal    ! .true. if not IEEE Inf and not IEEE NaN
  PUBLIC :: locate       ! Find closest values in a monotonic series
  PUBLIC :: swap         ! swaps arrays or elements of an array
  PUBLIC :: special_value ! Special IEEE values
  PUBLIC :: relational_operator_dp, relational_operator_sp ! abstract interface for relational operators

  public :: flip ! flips a dimension of an array
  public :: unpack_chunkwise ! flips a dimension of an array

  !> \brief flip an array at a certain dimension
  interface flip
    procedure flip_1D_dp, flip_2D_dp, flip_3D_dp, flip_4D_dp, flip_1D_i4, flip_2D_i4, flip_3D_i4, flip_4D_i4
  end interface

  !> \brief chunk version of the unpack operation
  interface unpack_chunkwise
    procedure unpack_chunkwise_i1, unpack_chunkwise_dp
  end interface

  ! ------------------------------------------------------------------

  !>        \brief Comparison of real values.

  !>        \details Compares two reals if they are numerically equal or not, i.e.
  !!        equal: \f[ |\frac{a-b}{b}| < \epsilon \f]
  !!
  !!        \b Example
  !!
  !!        Returns ´.false.´
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        isequal = equal(vec1, vec2)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"        First number to compare
  !>        \param[in] "real(sp/dp) :: b"        Second number to compare
  !>        \retval    "real(sp/dp) :: equal" \f$ a == b \f$ logically true or false

  !>        \authors Matthias Cuntz, Juliane Mai
  !>        \date Feb 2014
  !!          - sp, dp
  INTERFACE equal
    MODULE PROCEDURE equal_sp, equal_dp
  END INTERFACE equal

  !> \brief Comparison of real values for inequality.
  !> \see equal
  INTERFACE notequal
    MODULE PROCEDURE notequal_sp, notequal_dp
  END INTERFACE notequal

  !> \brief Comparison of real values: `a >= b`.
  INTERFACE greaterequal
    MODULE PROCEDURE greaterequal_sp, greaterequal_dp
  END INTERFACE greaterequal

  !> \brief Comparison of real values: `a <= b`.
  INTERFACE lesserequal
    MODULE PROCEDURE lesserequal_sp, lesserequal_dp
  END INTERFACE lesserequal

  !> \copydoc equal
  INTERFACE eq
    MODULE PROCEDURE equal_sp, equal_dp
  END INTERFACE eq

  !> \copydoc notequal
  INTERFACE ne
    MODULE PROCEDURE notequal_sp, notequal_dp
  END INTERFACE ne

  !> \copydoc greaterequal
  INTERFACE ge
    MODULE PROCEDURE greaterequal_sp, greaterequal_dp
  END INTERFACE ge

  !> \copydoc lesserequal
  INTERFACE le
    MODULE PROCEDURE lesserequal_sp, lesserequal_dp
  END INTERFACE le


  ! ------------------------------------------------------------------

  !>        \brief .true. if not IEEE Inf.

  !>        \details
  !!        Checks for IEEE Inf, i.e. Infinity.\n
  !!        Wraps to functions of the intrinsic module ieee_arithmetic.
  !!
  !!        \b Example
  !!
  !!        Returns `.false.` in 1st and 4th element.
  !!        \code{.f90}
  !!        vec1 = (/ NaN, 2., 3., Inf, 5., 6. /)
  !!        isfinite = is_finite(vec1)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"   Number to be evaluated.
  !>        \retval "logical :: is_finite"  \f$ a \neq \infty \f$, logically true or false.

  !>        \authors Matthias Cuntz
  !>        \date Mar 2015
  INTERFACE is_finite
    MODULE PROCEDURE is_finite_sp, is_finite_dp
  END INTERFACE is_finite

  !>        \brief .true. if IEEE NaN.

  !>        \details
  !!        Checks for IEEE NaN, i.e. Not-a-Number.\n
  !!        Wraps to functions of the intrinsic module ieee_arithmetic.
  !!
  !!        \b Example
  !!
  !!        Returns `.false.` in all but 1st element
  !!        \code{.f90}
  !!        vec1 = (/ NaN, 2., 3., Inf, 5., 6. /)
  !!        isnan = is_nan(vec1)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"        Number to be evaluated.
  !>        \retval "logical :: is_nan"  \f$ a = NaN \f$, logically true or false.

  INTERFACE is_nan
    MODULE PROCEDURE is_nan_sp, is_nan_dp
  END INTERFACE is_nan

  !>        \brief .true. if nor IEEE Inf nor IEEE NaN.

  !>        \details
  !!        Checks if IEEE Inf and IEEE NaN, i.e. Infinity and Not-a-Number.\n
  !!        Wraps to functions of the intrinsic module ieee_arithmetic.
  !!
  !!        \b Example
  !!
  !!        Returns `.true.` in all but 1st and 4th element.
  !!        \code{.f90}
  !!        vec1 = (/ NaN, 2., 3., Inf, 5., 6. /)
  !!        isnormal = is_normal(vec1)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"        Number to be evaluated.
  !>        \retval "logical :: is_normal" \f$ a \neq \infty \land a = NaN \f$, logically true or false.

  INTERFACE is_normal
    MODULE PROCEDURE is_normal_sp, is_normal_dp
  END INTERFACE is_normal


  ! ------------------------------------------------------------------

  !>         \brief Find closest values in a monotonic series, returns the indexes.

  !>        \details
  !!        Given an array x(1:n), and given a value y,
  !!        returns a value j such that y is between
  !!        x(j) and x(j+1).\n
  !!
  !!        x must be monotonically increasing.\n
  !!        j=0 or j=N is returned to indicate that x is out of range.
  !!
  !!        \b Example
  !!
  !!        Returns `ii = (/1, 5/)`
  !!        \code{.f90}
  !!        x = (/ 1., 2., 3., -999., 5., 6. /)
  !!        y = (/ 1.1, 5.6 /)
  !!        ii = locate(x, y)
  !!        \endcode
  !!
  !!        Returns `ii = 1`
  !!        \code{.f90}
  !!        y = 1.1
  !!        ii = locate(x, y)
  !!        \endcode

  !>        \param[in] "real(dp/sp)    :: x(:)"           Sorted array
  !>        \param[in] "real(dp/sp)    :: y[(:)]"         Value(s) of which the closest match in x(:) is wanted
  !>        \retval    "integer(i4) :: index[(:)]"        Index(es) of x so that y is between x(index) and x(index+1)

  !>        \note x must be monotonically increasing.\n

  !>        \author Matthias Cuntz
  !>        \date May 2014
  INTERFACE locate
    MODULE PROCEDURE locate_0d_dp, locate_0d_sp, locate_1d_dp, locate_1d_sp
  END INTERFACE locate


  ! ------------------------------------------------------------------

  !>        \brief Swap to values or two elements in array.

  !>        \details
  !!        Swaps either two entities, i.e. scalars, vectors, matrices,
  !!        or two elements in a vector.
  !!        The call is either \n
  !!          call swap(x,y) \n
  !!        or \n
  !!          call swap(vec,i,j)
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        \endcode
  !!
  !!        Swaps elements in vec1 and vec2
  !!        \code{.f90}
  !!        call swap(vec1, vec2)
  !!        \endcode
  !!
  !!        Swaps 1st and 3rd element of vec1
  !!        \code{.f90}
  !!        call swap(vec1, 1, 3)
  !!        \endcode


  !>        \param[in] "integer(i4)    :: i"               Index of first element to be swapped with second [case swap(vec,i,j)]
  !>        \param[in] "integer(i4)    :: j"               Index of second element to be swapped with first [case swap(vec,i,j)]
  !>        \param[inout] "real(sp/dp/i4) :: x[(:,...)]"   First scalar or array to swap with second [case swap(x,y)]
  !>        \param[inout] "real(sp/dp/i4) :: y[(:[,:])]"   Second scalar or array to swap with first [case swap(x,y)]
  !>        \param[inout] "real(sp/dp/i4) :: x(:)"         Vector of which to elements are swapped [case swap(vec,i,j)]

  !>        \note No mask or undef.

  !>        \author Matthias Cuntz
  !>        \date May 2014
  INTERFACE swap
    MODULE PROCEDURE &
            swap_xy_dp, swap_xy_sp, swap_xy_i4, &
            swap_vec_dp, swap_vec_sp, swap_vec_i4
  END INTERFACE swap


  ! ------------------------------------------------------------------

  !>        \brief Special IEEE values.

  !>        \details
  !!        Returns special IEEE values such as Infinity or Not-a-Number.\n
  !!        Wraps to function ieee_value of the intrinsic module ieee_arithmetic.\n
  !!        Current special values are:\n
  !!        - IEEE_SIGNALING_NAN
  !!        - IEEE_QUIET_NAN
  !!        - IEEE_NEGATIVE_INF
  !!        - IEEE_POSITIVE_INF
  !!        - IEEE_NEGATIVE_DENORMAL
  !!        - IEEE_POSITIVE_DENORMAL
  !!        - IEEE_NEGATIVE_NORMAL
  !!        - IEEE_POSITIVE_NORMAL
  !!        - IEEE_NEGATIVE_ZERO
  !!        - IEEE_POSITIVE_ZERO
  !!
  !!        \b Example
  !!
  !!        Returns NaN
  !!        \code{.f90}
  !!        NaN = special_value(1.0, 'IEEE_QUIET_NAN')
  !!        nan = special_value(1.0_dp, 'ieee_quiet_nan')
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: x"             dummy for kind of output
  !>        \param[in] "character(le=*) :: ieee"      ieee signal nanme
  !>        \retval    "real(sp/dp) :: special_value" IEEE special value,
  !!                                                  IEEE_SIGNALING_NAN,
  !!                                                  IEEE_QUIET_NAN (==IEEE_SIGNALING_NAN for gfortran),
  !!                                                  IEEE_NEGATIVE_INF,
  !!                                                  IEEE_POSITIVE_INF,
  !!                                                  IEEE_NEGATIVE_DENORMAL (==-0.0 for gfortran),
  !!                                                  IEEE_POSITIVE_DENORMAL (==0.0 for gfortran),
  !!                                                  IEEE_NEGATIVE_NORMAL (==-1.0 for gfortran),
  !!                                                  IEEE_POSITIVE_NORMAL (==1.0 for gfortran),
  !!                                                  IEEE_NEGATIVE_ZERO,
  !!                                                  IEEE_POSITIVE_ZERO,

  !>        \authors Matthias Cuntz
  !>        \date Mar 2015
  INTERFACE special_value
    MODULE PROCEDURE special_value_sp, special_value_dp
  END INTERFACE special_value

  abstract interface
    logical pure function relational_operator_dp(a, b) result(boolean)
      import dp
      real(dp), intent(in) :: a, b
    end function relational_operator_dp
  end interface

  abstract interface
    logical pure function relational_operator_sp(a, b) result(boolean)
      import sp
      real(sp), intent(in) :: a, b
    end function relational_operator_sp
  end interface

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  logical elemental pure function equal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    if ((epsilon(1.0_dp) * abs(b) - abs(a - b)) < 0.0_dp) then
      boolean = .false.
    else
      boolean = .true.
    end if

  end function equal_dp


  logical elemental pure function equal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    if ((epsilon(1.0_sp) * abs(b) - abs(a - b)) < 0.0_sp) then
      boolean = .false.
    else
      boolean = .true.
    end if

  end function equal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function greaterequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = .true.
    ! 1st part is /=, 2nd part is the a<b
    if (((epsilon(1.0_dp) * abs(b) - abs(a - b)) < 0.0_dp) .and. (a < b)) boolean = .false.

  end function greaterequal_dp


  logical elemental pure function greaterequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = .true.
    ! 1st part is /=, 2nd part is the a<b
    if (((epsilon(1.0_sp) * abs(b) - abs(a - b)) < 0.0_sp) .and. (a < b)) boolean = .false.

  end function greaterequal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function lesserequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = .true.
    ! 1st part is /=, 2nd part is the a>b
    if (((epsilon(1.0_dp) * abs(b) - abs(a - b)) < 0.0_dp) .and. (a > b)) boolean = .false.

  end function lesserequal_dp


  logical elemental pure function lesserequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = .true.
    ! 1st part is /=, 2nd part is the a>b
    if (((epsilon(1.0_sp) * abs(b) - abs(a - b)) < 0.0_sp) .and. (a > b)) boolean = .false.

  end function lesserequal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function notequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    if ((epsilon(1.0_dp) * abs(b) - abs(a - b)) < 0.0_dp) then
      boolean = .true.
    else
      boolean = .false.
    end if

  end function notequal_dp


  logical elemental pure function notequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    if ((epsilon(1.0_sp) * abs(b) - abs(a - b)) < 0.0_sp) then
      boolean = .true.
    else
      boolean = .false.
    end if

  end function notequal_sp

  ! ------------------------------------------------------------------

  ELEMENTAL PURE FUNCTION is_finite_dp(a)


  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite

  IMPLICIT NONE

  REAL(dp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_finite_dp !< logical :: is_finite &mdash; \f$ a \neq \infty \f$, logically true or false.

    is_finite_dp = ieee_is_finite(a)

  END FUNCTION is_finite_dp

  ELEMENTAL PURE FUNCTION is_finite_sp(a)

  use, intrinsic :: ieee_arithmetic, only : ieee_is_finite

  IMPLICIT NONE

  REAL(sp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_finite_sp !< logical :: is_finite &mdash; \f$ a \neq \infty \f$, logically true or false.

    is_finite_sp = ieee_is_finite(a)

  END FUNCTION is_finite_sp


  ELEMENTAL PURE FUNCTION is_nan_dp(a)

  use, intrinsic :: ieee_arithmetic, only : isnan => ieee_is_nan

  IMPLICIT NONE

  REAL(dp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_nan_dp !< logical :: is_nan &mdash; \f$ a = NaN \f$, logically true or false.

    is_nan_dp = isnan(a)

  END FUNCTION is_nan_dp

  ELEMENTAL PURE FUNCTION is_nan_sp(a)

  use, intrinsic :: ieee_arithmetic, only : isnan => ieee_is_nan

  IMPLICIT NONE

  REAL(sp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_nan_sp !< logical :: is_nan &mdash; \f$ a = NaN \f$, logically true or false.

    is_nan_sp = isnan(a)

  END FUNCTION is_nan_sp


  ELEMENTAL PURE FUNCTION is_normal_dp(a)

  use, intrinsic :: ieee_arithmetic, only : ieee_is_normal

  IMPLICIT NONE

  REAL(dp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_normal_dp !< logical :: is_normal &mdash; \f$ a \neq \infty \land a = NaN \f$, logically true or false.

    is_normal_dp = ieee_is_normal(a)

  END FUNCTION is_normal_dp

  ELEMENTAL PURE FUNCTION is_normal_sp(a)

  use, intrinsic :: ieee_arithmetic, only : ieee_is_normal

  IMPLICIT NONE

  REAL(sp), INTENT(IN) :: a !< Number to be evaluated.
  LOGICAL :: is_normal_sp !< logical :: is_normal &mdash; \f$ a \neq \infty \land a = NaN \f$, logically true or false.

    is_normal_sp = ieee_is_normal(a)

  END FUNCTION is_normal_sp

  ! ------------------------------------------------------------------

  ! Given an array x(1:N), and given a value y, returns a value j such that y is between
  !  x(j) and x(j+1). x must be monotonically increasing.
  !  j=0 or j=N is returned to indicate that x is out of range.

  FUNCTION locate_0d_dp(x, y)

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), INTENT(IN) :: y
    INTEGER(i4) :: locate_0d_dp

    INTEGER(i4), dimension(1) :: c

    c = minloc(abs(x - y))
    if (le(x(c(1)), y)) then
      locate_0d_dp = c(1)
    else
      locate_0d_dp = c(1) - 1
    end if

  END FUNCTION locate_0d_dp

  FUNCTION locate_0d_sp(x, y)

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), INTENT(IN) :: y
    INTEGER(i4) :: locate_0d_sp

    INTEGER(i4), dimension(1) :: c

    c = minloc(abs(x - y))
    if (le(x(c(1)), y)) then
      locate_0d_sp = c(1)
    else
      locate_0d_sp = c(1) - 1
    end if

  END FUNCTION locate_0d_sp

  FUNCTION locate_1d_dp(x, y)

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), DIMENSION(:), allocatable :: locate_1d_dp

    INTEGER(i4) :: ny, i
    INTEGER(i4), dimension(1) :: c


    ny = size(y)
    if (.not. allocated(locate_1d_dp)) allocate(locate_1d_dp(ny))

    do i = 1, ny
      c = minloc(abs(x - y(i)))
      if (le(x(c(1)), y(i))) then
        locate_1d_dp(i) = c(1)
      else
        locate_1d_dp(i) = c(1) - 1
      end if
    end do

  END FUNCTION locate_1d_dp

  FUNCTION locate_1d_sp(x, y)

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), DIMENSION(:), allocatable :: locate_1d_sp

    INTEGER(i4) :: ny, i
    INTEGER(i4), dimension(1) :: c


    ny = size(y)
    if (.not. allocated(locate_1d_sp)) allocate(locate_1d_sp(ny))

    do i = 1, ny
      c = minloc(abs(x - y(i)))
      if (le(x(c(1)), y(i))) then
        locate_1d_sp(i) = c(1)
      else
        locate_1d_sp(i) = c(1) - 1
      end if
    end do

  END FUNCTION locate_1d_sp

  ! ------------------------------------------------------------------

  elemental pure subroutine swap_xy_dp(x, y)

    real(dp), intent(inout) :: x
    real(dp), intent(inout) :: y

    real(dp) :: z

    z = x
    x = y
    y = z

  end subroutine swap_xy_dp

  elemental pure subroutine swap_xy_sp(x, y)

    real(sp), intent(inout) :: x
    real(sp), intent(inout) :: y

    real(sp) :: z

    z = x
    x = y
    y = z

  end subroutine swap_xy_sp

  elemental pure subroutine swap_xy_i4(x, y)

    integer(i4), intent(inout) :: x
    integer(i4), intent(inout) :: y

    integer(i4) :: z

    z = x
    x = y
    y = z

  end subroutine swap_xy_i4


  subroutine swap_vec_dp(x, i1, i2)

    real(dp), dimension(:), intent(inout) :: x
    integer(i4), intent(in) :: i1
    integer(i4), intent(in) :: i2

    real(dp) :: z

    z = x(i1)
    x(i1) = x(i2)
    x(i2) = z

  end subroutine swap_vec_dp

  subroutine swap_vec_sp(x, i1, i2)

    real(sp), dimension(:), intent(inout) :: x
    integer(i4), intent(in) :: i1
    integer(i4), intent(in) :: i2

    real(sp) :: z

    z = x(i1)
    x(i1) = x(i2)
    x(i2) = z

  end subroutine swap_vec_sp

  subroutine swap_vec_i4(x, i1, i2)

    integer(i4), dimension(:), intent(inout) :: x
    integer(i4), intent(in) :: i1
    integer(i4), intent(in) :: i2

    integer(i4) :: z

    z = x(i1)
    x(i1) = x(i2)
    x(i2) = z

  end subroutine swap_vec_i4

  ! ------------------------------------------------------------------

  function special_value_dp(x, ieee)

    use, intrinsic :: ieee_arithmetic, only : ieee_value, &
          IEEE_SIGNALING_NAN, &
          IEEE_QUIET_NAN, &
          IEEE_NEGATIVE_INF, &
          IEEE_POSITIVE_INF, &
          IEEE_NEGATIVE_DENORMAL, &
          IEEE_POSITIVE_DENORMAL, &
          IEEE_NEGATIVE_NORMAL, &
          IEEE_POSITIVE_NORMAL, &
          IEEE_NEGATIVE_ZERO, &
          IEEE_POSITIVE_ZERO

  implicit none

  real(dp), intent(in) :: x !< dummy for kind of output.
  character(len = *), intent(in) :: ieee !< ieee signal name.
  real(dp) :: special_value_dp !< real(dp) :: special_value &mdash; IEEE special value,
  !!                                                  IEEE_SIGNALING_NAN,
  !!                                                  IEEE_QUIET_NAN,
  !!                                                  IEEE_NEGATIVE_INF,
  !!                                                  IEEE_POSITIVE_INF,
  !!                                                  IEEE_NEGATIVE_DENORMAL,
  !!                                                  IEEE_POSITIVE_DENORMAL,
  !!                                                  IEEE_NEGATIVE_NORMAL,
  !!                                                  IEEE_POSITIVE_NORMAL,
  !!                                                  IEEE_NEGATIVE_ZERO,
  !!                                                  IEEE_POSITIVE_ZERO,

  ! local
  character(len = 21) :: ieee_up
  real(dp) :: tmp

  ieee_up = toupper(ieee)
    select case(trim(ieee_up))
  case('IEEE_SIGNALING_NAN')
    special_value_dp = ieee_value(x, IEEE_SIGNALING_NAN)
  case('IEEE_QUIET_NAN')
    special_value_dp = ieee_value(x, IEEE_QUIET_NAN)
  case('IEEE_NEGATIVE_INF')
    special_value_dp = ieee_value(x, IEEE_NEGATIVE_INF)
  case('IEEE_POSITIVE_INF')
    special_value_dp = ieee_value(x, IEEE_POSITIVE_INF)
  case('IEEE_NEGATIVE_DENORMAL')
    special_value_dp = ieee_value(x, IEEE_NEGATIVE_DENORMAL)
  case('IEEE_POSITIVE_DENORMAL')
    special_value_dp = ieee_value(x, IEEE_POSITIVE_DENORMAL)
  case('IEEE_NEGATIVE_NORMAL')
    special_value_dp = ieee_value(x, IEEE_NEGATIVE_NORMAL)
  case('IEEE_POSITIVE_NORMAL')
    special_value_dp = ieee_value(x, IEEE_POSITIVE_NORMAL)
  case('IEEE_NEGATIVE_ZERO')
    special_value_dp = ieee_value(x, IEEE_NEGATIVE_ZERO)
  case('IEEE_POSITIVE_ZERO')
    special_value_dp = ieee_value(x, IEEE_POSITIVE_ZERO)
  case default
    special_value_dp = 0.0_dp
  end select

  end function special_value_dp

  function special_value_sp(x, ieee)

    use, intrinsic :: ieee_arithmetic, only : ieee_value, &
          IEEE_SIGNALING_NAN, &
          IEEE_QUIET_NAN, &
          IEEE_NEGATIVE_INF, &
          IEEE_POSITIVE_INF, &
          IEEE_NEGATIVE_DENORMAL, &
          IEEE_POSITIVE_DENORMAL, &
          IEEE_NEGATIVE_NORMAL, &
          IEEE_POSITIVE_NORMAL, &
          IEEE_NEGATIVE_ZERO, &
          IEEE_POSITIVE_ZERO

  implicit none

  real(sp), intent(in) :: x !< dummy for kind of output.
  character(len = *), intent(in) :: ieee !< ieee signal name.
  real(sp) :: special_value_sp !< IEEE special value,
  !!                                                              IEEE_SIGNALING_NAN,
  !!                                                              IEEE_QUIET_NAN,
  !!                                                              IEEE_NEGATIVE_INF,
  !!                                                              IEEE_POSITIVE_INF,
  !!                                                              IEEE_NEGATIVE_DENORMAL,
  !!                                                              IEEE_POSITIVE_DENORMAL,
  !!                                                              IEEE_NEGATIVE_NORMAL,
  !!                                                              IEEE_POSITIVE_NORMAL,
  !!                                                              IEEE_NEGATIVE_ZERO,
  !!                                                              IEEE_POSITIVE_ZERO,

  ! local
  character(len = 21) :: ieee_up
    real(sp) :: tmp

  ieee_up = toupper(ieee)
    select case(trim(ieee_up))
  case('IEEE_SIGNALING_NAN')
    special_value_sp = ieee_value(x, IEEE_SIGNALING_NAN)
  case('IEEE_QUIET_NAN')
    special_value_sp = ieee_value(x, IEEE_QUIET_NAN)
  case('IEEE_NEGATIVE_INF')
    special_value_sp = ieee_value(x, IEEE_NEGATIVE_INF)
  case('IEEE_POSITIVE_INF')
    special_value_sp = ieee_value(x, IEEE_POSITIVE_INF)
  case('IEEE_NEGATIVE_DENORMAL')
    special_value_sp = ieee_value(x, IEEE_NEGATIVE_DENORMAL)
  case('IEEE_POSITIVE_DENORMAL')
    special_value_sp = ieee_value(x, IEEE_POSITIVE_DENORMAL)
  case('IEEE_NEGATIVE_NORMAL')
    special_value_sp = ieee_value(x, IEEE_NEGATIVE_NORMAL)
  case('IEEE_POSITIVE_NORMAL')
    special_value_sp = ieee_value(x, IEEE_POSITIVE_NORMAL)
  case('IEEE_NEGATIVE_ZERO')
    special_value_sp = ieee_value(x, IEEE_NEGATIVE_ZERO)
  case('IEEE_POSITIVE_ZERO')
    special_value_sp = ieee_value(x, IEEE_POSITIVE_ZERO)
  case default
    special_value_sp = 0.0_sp
  end select

  end function special_value_sp

  subroutine flip_1D_dp(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    real(dp), dimension(:), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    real(dp), dimension(:), allocatable :: temp_data
    integer(i4) :: iDim1

    if (iDim > 1_i4) then
      call message('Cannot flip 1D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if
    allocate(temp_data(size(data, 1)))

    do iDim1 = 1, size(data, 1)
      temp_data(size(data, 1) - iDim1 + 1) = data(iDim1)
    end do
    call move_alloc(temp_data, data)
  end subroutine flip_1D_dp

  subroutine flip_2D_dp(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message

    real(dp), dimension(:, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    real(dp), dimension(:, :), allocatable :: temp_data
    integer(i4) :: iDim2, iDim1

    if (iDim > 2_i4) then
      call message('Cannot flip 2D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2)))

    if (iDim == 1_i4) then
      do iDim2 = 1, size(data, 2)
        do iDim1 = 1, size(data, 1)
          temp_data(size(data, 1) - iDim1 + 1, iDim2) = data(iDim1, iDim2)
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim2 = 1, size(data, 2)
        temp_data(:, size(data, 2) - iDim2 + 1) = data(:, iDim2)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_2D_dp

  subroutine flip_3D_dp(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    real(dp), dimension(:, :, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    real(dp), dimension(:, :, :), allocatable :: temp_data
    integer(i4) :: iDim3, iDim2, iDim1

    if (iDim > 3_i4) then
      call message('Cannot flip 3D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2), size(data, 3)))

    if (iDim == 1_i4) then
      do iDim3 = 1, size(data, 3)
        do iDim2 = 1, size(data, 2)
          do iDim1 = 1, size(data, 1)
            temp_data(size(data, 1) - iDim1 + 1, iDim2, iDim3) = data(iDim1, iDim2, iDim3)
          end do
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim3 = 1, size(data, 3)
        do iDim2 = 1, size(data, 2)
          temp_data(:, size(data, 2) - iDim2 + 1, iDim3) = data(:, iDim2, iDim3)
        end do
      end do
    else if (iDim == 3_i4) then
      do iDim3 = 1, size(data, 3)
        temp_data(:, :, size(data, 3) - iDim3 + 1) = data(:, :, iDim3)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_3D_dp

  subroutine flip_4D_dp(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    real(dp), dimension(:, :, :, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    real(dp), dimension(:, :, :, :), allocatable :: temp_data
    integer(i4) :: iDim4, iDim3, iDim2, iDim1

    if (iDim > 4_i4) then
      call message('Cannot flip 4D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))

    if (iDim == 1_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          do iDim2 = 1, size(data, 2)
            do iDim1 = 1, size(data, 1)
              temp_data(size(data, 1) - iDim1 + 1, iDim2, iDim3, iDim4) = data(iDim1, iDim2, iDim3, iDim4)
            end do
          end do
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          do iDim2 = 1, size(data, 2)
            temp_data(:, size(data, 2) - iDim2 + 1, iDim3, iDim4) = data(:, iDim2, iDim3, iDim4)
          end do
        end do
      end do
    else if (iDim == 3_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          temp_data(:, :, size(data, 3) - iDim3 + 1, iDim4) = data(:, :, iDim3, iDim4)
        end do
      end do
    else if (iDim == 4_i4) then
      do iDim4 = 1, size(data, 4)
        temp_data(:, :, :, size(data, 4) - iDim4 + 1) = data(:, :, :, iDim4)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_4D_dp

  subroutine flip_1D_i4(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    integer(i4), dimension(:), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    integer(i4), dimension(:), allocatable :: temp_data
    integer(i4) :: iDim1

    if (iDim > 1_i4) then
      call message('Cannot flip 1D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if
    allocate(temp_data(size(data, 1)))

    do iDim1 = 1, size(data, 1)
      temp_data(size(data, 1) - iDim1 + 1) = data(iDim1)
    end do
    call move_alloc(temp_data, data)
  end subroutine flip_1D_i4

  subroutine flip_2D_i4(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message

    integer(i4), dimension(:, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    integer(i4), dimension(:, :), allocatable :: temp_data
    integer(i4) :: iDim2, iDim1

    if (iDim > 2_i4) then
      call message('Cannot flip 2D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2)))

    if (iDim == 1_i4) then
      do iDim2 = 1, size(data, 2)
        do iDim1 = 1, size(data, 1)
          temp_data(size(data, 1) - iDim1 + 1, iDim2) = data(iDim1, iDim2)
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim2 = 1, size(data, 2)
        temp_data(:, size(data, 2) - iDim2 + 1) = data(:, iDim2)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_2D_i4

  subroutine flip_3D_i4(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    integer(i4), dimension(:, :, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    integer(i4), dimension(:, :, :), allocatable :: temp_data
    integer(i4) :: iDim3, iDim2, iDim1

    if (iDim > 3_i4) then
      call message('Cannot flip 3D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2), size(data, 3)))

    if (iDim == 1_i4) then
      do iDim3 = 1, size(data, 3)
        do iDim2 = 1, size(data, 2)
          do iDim1 = 1, size(data, 1)
            temp_data(size(data, 1) - iDim1 + 1, iDim2, iDim3) = data(iDim1, iDim2, iDim3)
          end do
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim3 = 1, size(data, 3)
        do iDim2 = 1, size(data, 2)
          temp_data(:, size(data, 2) - iDim2 + 1, iDim3) = data(:, iDim2, iDim3)
        end do
      end do
    else if (iDim == 3_i4) then
      do iDim3 = 1, size(data, 3)
        temp_data(:, :, size(data, 3) - iDim3 + 1) = data(:, :, iDim3)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_3D_i4

  subroutine flip_4D_i4(data, iDim)
    use mo_string_utils, only: compress, num2str
    use mo_message, only: message
    integer(i4), dimension(:, :, :, :), allocatable, intent(inout) :: data
    integer(i4), intent(in) :: iDim

    integer(i4), dimension(:, :, :, :), allocatable :: temp_data
    integer(i4) :: iDim4, iDim3, iDim2, iDim1

    if (iDim > 4_i4) then
      call message('Cannot flip 4D-array at dimension ', compress(trim(num2str(iDim))))
      stop 1
    end if

    allocate(temp_data(size(data, 1), size(data, 2), size(data, 3), size(data, 4)))

    if (iDim == 1_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          do iDim2 = 1, size(data, 2)
            do iDim1 = 1, size(data, 1)
              temp_data(size(data, 1) - iDim1 + 1, iDim2, iDim3, iDim4) = data(iDim1, iDim2, iDim3, iDim4)
            end do
          end do
        end do
      end do
    else if (iDim == 2_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          do iDim2 = 1, size(data, 2)
            temp_data(:, size(data, 2) - iDim2 + 1, iDim3, iDim4) = data(:, iDim2, iDim3, iDim4)
          end do
        end do
      end do
    else if (iDim == 3_i4) then
      do iDim4 = 1, size(data, 4)
        do iDim3 = 1, size(data, 3)
          temp_data(:, :, size(data, 3) - iDim3 + 1, iDim4) = data(:, :, iDim3, iDim4)
        end do
      end do
    else if (iDim == 4_i4) then
      do iDim4 = 1, size(data, 4)
        temp_data(:, :, :, size(data, 4) - iDim4 + 1) = data(:, :, :, iDim4)
      end do
    end if
    call move_alloc(temp_data, data)
  end subroutine flip_4D_i4

  function unpack_chunkwise_dp(vector, mask, field, chunksizeArg) result(unpacked)
  !< this is a chunkwise application of the intrinsic unpack function
  !! it became necessary as the unpack intrinsic can handle only arrays
  !! with size smaller than huge(default_integer_kind)...
  !! it has the following restrictions:\n
  !!   - vector must be of type dp
  !!   - mask must have rank 1
  !!   - field must be a scalar
    real(dp), dimension(:), intent(in) :: vector
    logical, dimension(:), intent(in) :: mask
    real(dp), intent(in) :: field
    real(dp), dimension(size(mask, kind=i8)) :: unpacked
    integer(i8), intent(in), optional :: chunksizeArg

    integer(i8) :: i, chunksize, indexMin, indexMax, currentCounts, counts

    if (present(chunksizeArg)) then
      chunksize = chunksizeArg
    else
      chunksize = int(huge(0_i4), i8)
    end if
    ! init some values
    i = 1_i8
    indexMax = i * chunksize
    currentCounts = 1_i8
    do while (indexMax < size(mask, kind=i8))
      ! get the indices for the mask
      indexMin = (i-1) * chunksize + 1_i8
      indexMax = minval([i * chunksize, size(mask, kind=i8)])
      ! this is the indexer for the vector
      counts = count(mask(indexMin: indexMax), kind=i8)
      ! unpack slices of maximum size
      if (counts == (indexMax - indexMin + 1_i8)) then
        unpacked(indexMin: indexMax) = vector(currentCounts: currentCounts + counts - 1_i8)
      else if (counts == 0_i8) then
        unpacked(indexMin: indexMax) = field
      else
        unpacked(indexMin: indexMax) = unpack(vector(currentCounts: currentCounts + counts - 1_i8), &
                                              mask(indexMin: indexMax), &
                                              field)
      end if
      ! advance the counters
      currentCounts = currentCounts + counts
      i = i + 1_i8
    end do

  end function unpack_chunkwise_dp

  function unpack_chunkwise_i1(vector, mask, field, chunksizeArg) result(unpacked)
  !< this is a chunkwise application of the intrinsic unpack function
  !! it has the following restrictions:\n
  !!   - vector must be of type i1
  !!   - mask must have rank 1
  !!   - field must be a scalar
    integer(i1), dimension(:), intent(in) :: vector
    logical, dimension(:), intent(in) :: mask
    integer(i1), intent(in) :: field
    integer(i1), dimension(size(mask, kind=i8)) :: unpacked
    integer(i8), intent(in), optional :: chunksizeArg

    integer(i8) :: i, chunksize, indexMin, indexMax, currentCounts, counts

    if (present(chunksizeArg)) then
      chunksize = chunksizeArg
    else
      chunksize = int(huge(0_i4), i8)
    end if
    ! init some values
    i = 1_i8
    indexMax = i * chunksize
    currentCounts = 1_i8
    do while (indexMax < size(mask, kind=i8))
      ! get the indices for the mask
      indexMin = (i-1) * chunksize + 1_i8
      indexMax = minval([i * chunksize, size(mask, kind=i8)])
      ! this is the indexer for the vector
      counts = count(mask(indexMin: indexMax), kind=i8)
      ! unpack slices of maximum size
      unpacked(indexMin: indexMax) = unpack(vector(currentCounts: currentCounts + counts - 1_i8), &
                                            mask(indexMin: indexMax), &
                                            field)
      ! advance the counters
      currentCounts = currentCounts + counts
      i = i + 1_i8
    end do

  end function unpack_chunkwise_i1


END MODULE mo_utils
