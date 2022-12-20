!> \file mo_utils.f90
!> \brief \copybrief mo_utils
!> \details \copydetails mo_utils

!> \brief General utilities for the CHS library
!> \details This module provides general utilities such as comparisons of two reals.
!> \changelog
!! - Matthias Cuntz, Juliane Mai, Feb 2014
!!   - written
!! - Matthias Cuntz, Juliane Mai, Feb 2014
!!   - equal, notequal
!! - Matthias Cuntz,              May 2014
!!   - swap
!! - Matthias Cuntz,              May 2014
!!   - is_finite, is_nan, is_normal, special_value
!> \authors Matthias Cuntz, Juliane Mai
!> \date 2014 - 2016
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_utils

  USE mo_kind, only : sp, dp, i1, i4, i8, spc, dpc
  USE mo_string_utils, only : toupper

  IMPLICIT NONE

  PUBLIC :: arange        ! Natural numbers within interval
  PUBLIC :: cumsum        ! Cumulative sum
  PUBLIC :: imaxloc       ! maxloc(arr)(1)
  PUBLIC :: iminloc       ! maxloc(arr)(1)
  PUBLIC :: linspace      ! numpy like linspace
  PUBLIC :: equal         ! a == b, a .eq. b
  PUBLIC :: greaterequal  ! a >= b, a .ge. b
  PUBLIC :: lesserequal   ! a <= b, a .le. b
  PUBLIC :: notequal      ! a /= b, a .ne. b
  PUBLIC :: eq            ! a == b, a .eq. b
  PUBLIC :: is_close      ! a =~ b, a .eq. b in defined precision
  PUBLIC :: ge            ! a >= b, a .ge. b
  PUBLIC :: le            ! a <= b, a .le. b
  PUBLIC :: ne            ! a /= b, a .ne. b
  PUBLIC :: is_finite     ! .true. if not IEEE Inf and not IEEE NaN
  PUBLIC :: is_nan        ! .true. if IEEE NaN
  PUBLIC :: is_normal     ! .true. if not IEEE Inf and not IEEE NaN
  PUBLIC :: locate        ! Find closest values in a monotonic series
  PUBLIC :: swap          ! swaps arrays or elements of an array
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
  !>        \brief Numbers within a given range.
  !>        \details Gives array with numbers in a given interval, i.e.
  !!        \f[ arange(1) = lower \f]
  !!        \f[ arange(2) = lower+1 \f]
  !!        ...
  !!        \f[ arange(n) = upper \f]
  !!
  !!        Default is lower=1.
  !!        Output array has kind of lower.
  !!
  !!        \b Example
  !!        \code{.f90}
  !!        rr = arange(100._dp)
  !!        \endcode
  !!        -> see also example in test directory

  !>        \param[in] "integer(i4/i8)/real(sp/dp) :: lower"   Start of interval if upper is given,
  !!                                                           Otherwise end of interval and start of interval is 1.
  !>        \param[in] "integer(i4/i8)/real(sp/dp) :: upper    End of interval"
  !>        \return     kind(arr) :: arange(upper-lower+1)     &mdash; 1D array with values within given interval.

  !>        \authors Matthias Cuntz
  !>        \date Jun 2016
  INTERFACE arange
     MODULE PROCEDURE arange_i4, arange_i8, arange_dp, arange_sp
  END INTERFACE arange

  ! ------------------------------------------------------------------
  !>        \brief Cumulative sum.
  !>        \details The cumulative sum of the elements of an array
  !!        \f[ cumsum(i) = \sum_{j=1}^i array(j) \f]
  !!
  !!        \b Example
  !!        \code{.f90}
  !!        vec = (/ 1., 2., 3., 4., 5., 6. /)
  !!        cum = cumsum(vec)
  !!        \endcode
  !!        -> see also example in test directory

  !>        \param[in] "integer(i4/i8)/real(sp/dp)/complex(spc/dpc) :: arr(:)"   1D array
  !>        \return     kind(arr) :: cumsum(size(arr))                           &mdash; Cumulative sum

  !>        \authors Matthias Cuntz
  !>        \date Jun 2016
  INTERFACE cumsum
     MODULE PROCEDURE cumsum_i4, cumsum_i8, cumsum_dp, cumsum_sp, cumsum_dpc, cumsum_spc
  END INTERFACE cumsum

  ! ------------------------------------------------------------------
  !>        \brief First location in array of element with the maximum value.
  !>        \details Fortran intrinsic maxloc return arrays with all subsripts
  !!        corresponding to the maximum value in the array.\n
  !!        This routine returns only the first entry as scalar integer.
  !!
  !!        \b Example
  !!        \code{.f90}
  !!        integer(i4) :: imax
  !!        imax = imaxloc(vec, mask=mask)
  !!        \endcode
  !!        -> see also example in test directory

  !>        \param[in] "integer(i4/i8)/real(sp/dp)/complex(spc/dpc) :: array(:)" Input array
  !>        \param[in] "logical :: mask(:)"   If present, only those locations in array corresponding to
  !!                                          the true values in mask are searched for the maximum value.
  !>        \return     integer(i4) :: imaxloc &mdash; First location of maximum

  !>        \authors Matthias Cuntz, Juliane Mai
  !>        \date Feb 2014
  INTERFACE imaxloc
     MODULE PROCEDURE imaxloc_i4, imaxloc_i8, imaxloc_sp, imaxloc_dp
  END INTERFACE imaxloc

  ! ------------------------------------------------------------------
  !>        \brief First location in array of element with the minimum value.
  !>        \details Fortran intrinsic minloc return arrays with all subsripts
  !!        corresponding to the minimum value in the array.\n
  !!        This routine returns only the first entry as scalar integer.
  !!
  !!        \b Example
  !!        \code{.f90}
  !!        integer(i4) :: imin
  !!        imin = iminloc(vec, mask=mask)
  !!        \endcode
  !!        -> see also example in test directory

  !>        \param[in] "integer(i4/i8)/real(sp/dp)/complex(spc/dpc) :: array(:)" Input array
  !>        \param[in] "logical :: mask(:)"   If present, only those locations in array corresponding to
  !!                                          the true values in mask are searched for the maximum value.
  !>        \return     integer(i4) :: iminloc &mdash; First location of minimum

  !>        \authors Matthias Cuntz, Juliane Mai
  !>        \date Feb 2014
  INTERFACE iminloc
     MODULE PROCEDURE iminloc_i4, iminloc_i8, iminloc_sp, iminloc_dp
  END INTERFACE iminloc

  ! ------------------------------------------------------------------
  !>        \brief Evenly spaced numbers in interval.
  !>        \details Return N evenly spaced numbers over a specified interval [lower,upper].
  !!        \f[ linspace(lower,upper,N) = lower + arange(0,N-1)/(N-1) * (upper-lower) \f]
  !>        Output array has kind of lower.
  !!
  !!        \b Example
  !!        \code{.f90}
  !!         rr = linspace(1.0_dp,11._dp,101)
  !!        \endcode
  !!        -> see also example in test directory

  !>        \param[in] "integer(i4/i8)/real(sp/dp) :: lower"   Start of interval.
  !>        \param[in] "integer(i4/i8)/real(sp/dp) :: upper"   End of interval.
  !>        \param[in] "integer(i4)                :: nstep"   Number of steps.
  !>        \return     kind(lower) :: linspace(N)    &mdash; 1D array with evenly spaced numbers between lower and upper.

  !>        \authors Matthias Cuntz
  !>        \date Jun 2016
  INTERFACE linspace
     MODULE PROCEDURE linspace_i4, linspace_i8, linspace_dp, linspace_sp
  END INTERFACE linspace
  ! ------------------------------------------------------------------

  !>        \brief Comparison of real values.
  !>        \details Compares two reals if they are numerically equal or not, with option for precision, i.e.
  !!        equal: \f[ |a-b| < \mathrm{max} \{ \epsilon_\mathrm{rel} \mathrm{max} \{ |a|,|b| \}, \epsilon_\mathrm{abs} \} \f]
  !!
  !!        \b Example
  !!
  !!        Returns ´.false.´ in 5th element of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        isequal = is_close(vec1, vec2)
  !!        \endcode
  !!        Returns ´.true.´ in all elements of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        isequal = is_close(vec1, vec2, atol = 5.0_dp)
  !!        \endcode
  !!        Returns ´.false.´ in 6th element of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., NaN /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., NaN /)
  !!        isequal = is_close(vec1, vec2, atol = 5.0_dp)
  !!        \endcode
  !!        Returns ´.true.´ in all elements of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., NaN /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., NaN /)
  !!        isequal = is_close(vec1, vec2, equal_nan = .true.)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"                   First number to compare
  !>        \param[in] "real(sp/dp) :: b"                   Second number to compare
  !>        \param[in] "real(sp/dp), optional :: rtol"      Relative tolerance, will scale with a (DEFAULT : 1.0E-5).
  !>        \param[in] "real(sp/dp), optional :: atol"      Absolute tolerance (DEFAULT : 1.0E-8).
  !>        \param[in] "logical, optional    :: equal_nan" If \f$.true.\f$, NaN values will between a and b are considered
  !!                                                        equal.
  !>        \retval    "real(sp/dp) :: is_close"            \f$ a == b \f$ logically true or false

  !>        \authors Arya Prasetya
  !>        \date Jan 2022
  !!          - add precision arguments and is_close
  INTERFACE is_close
    MODULE PROCEDURE is_close_sp, is_close_dp
  END INTERFACE is_close

  !>        \brief Comparison of real values.
  !>        \details Compares two reals if they are exactly numerically equal or not, i.e.
  !!        equal: \f[ |\frac{a-b}{b}| < \epsilon \f]
  !!
  !!        \b Example
  !!
  !!        Returns ´.false.´ in 5th element of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        isequal = equal(vec1, vec2)
  !!        \endcode
  !!        Returns ´.true.´ in all elements of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., 6. /)
  !!        isequal = equal(vec1, vec2)
  !!        \endcode
  !!        Returns ´.false.´ in 6th element of isequal
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2., 3., -999., 5., NaN /)
  !!        vec2 = (/ 1., 1., 3., -999., 10., NaN /)
  !!        isequal = equal(vec1, vec2)
  !!        \endcode

  !>        \param[in] "real(sp/dp) :: a"                   First number to compare
  !>        \param[in] "real(sp/dp) :: b"                   Second number to compare
  !>        \retval    "real(sp/dp) :: equal"            \f$ a == b \f$ logically true or false

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
  !> \see equal
  INTERFACE greaterequal
    MODULE PROCEDURE greaterequal_sp, greaterequal_dp
  END INTERFACE greaterequal

  !> \brief Comparison of real values: `a <= b`.
  !> \see equal
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

  !>        \brief Find closest values in a monotonic series, returns the indexes.
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

  !> \brief abstract interface for a relational operator on double precision arguments
  abstract interface
    logical pure function relational_operator_dp(a, b) result(boolean)
      import dp
      real(dp), intent(in) :: a, b
    end function relational_operator_dp
  end interface

  !> \brief abstract interface for a relational operator on single precision arguments
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

  function arange_i4(lower, upper)

    implicit none

    integer(i4), intent(in)                :: lower
    integer(i4), intent(in), optional      :: upper
    integer(i4), dimension(:), allocatable :: arange_i4

    integer(i4) :: istart, istop
    integer(i4) :: i

    if (present(upper)) then
      istart = lower
      istop  = upper
    else
      istart = 1_i4
      istop  = lower
    endif

    allocate(arange_i4(istop-istart+1_i4))

    forall(i=istart:istop) arange_i4(i-istart+1) = i

  end function arange_i4

  function arange_i8(lower, upper)

    implicit none

    integer(i8), intent(in)                :: lower
    integer(i8), intent(in), optional      :: upper
    integer(i8), dimension(:), allocatable :: arange_i8

    integer(i8) :: istart, istop
    integer(i8) :: i

    if (present(upper)) then
      istart = lower
      istop  = upper
    else
      istart = 1_i8
      istop  = lower
    endif

    allocate(arange_i8(istop-istart+1_i8))

    forall(i=istart:istop) arange_i8(i-istart+1) = i

  end function arange_i8

  function arange_dp(lower, upper)

    implicit none

    real(dp), intent(in)                :: lower
    real(dp), intent(in), optional      :: upper
    real(dp), dimension(:), allocatable :: arange_dp

    integer(i8) :: istart, istop
    integer(i8) :: i

    if (present(upper)) then
      istart = int(lower,i8)
      istop  = int(upper,i8)
    else
      istart = 1_i8
      istop  = int(lower,i8)
    endif

    allocate(arange_dp(istop-istart+1_i8))

    forall(i=istart:istop) arange_dp(i-istart+1) = real(i,dp)

  end function arange_dp

  function arange_sp(lower, upper)

    implicit none

    real(sp), intent(in)                :: lower
    real(sp), intent(in), optional      :: upper
    real(sp), dimension(:), allocatable :: arange_sp

    integer(i8) :: istart, istop
    integer(i8) :: i

    if (present(upper)) then
      istart = int(lower,i8)
      istop  = int(upper,i8)
    else
      istart = 1_i8
      istop  = int(lower,i8)
    endif

    allocate(arange_sp(istop-istart+1_i8))

    forall(i=istart:istop) arange_sp(i-istart+1) = real(i,sp)

  end function arange_sp

  ! ------------------------------------------------------------------

  function cumsum_i4(arr)

    implicit none

    integer(i4), dimension(:), intent(in) :: arr
    integer(i4), dimension(size(arr,1))   :: cumsum_i4

    integer(i4) :: i

    cumsum_i4(1) = arr(1)
    do i=2, size(arr)
       cumsum_i4(i) = cumsum_i4(i-1) + arr(i)
    end do

  end function cumsum_i4

  function cumsum_i8(arr)

    implicit none

    integer(i8), dimension(:), intent(in) :: arr
    integer(i8), dimension(size(arr,1))   :: cumsum_i8

    integer(i4) :: i

    cumsum_i8(1) = arr(1)
    do i=2, size(arr)
       cumsum_i8(i) = cumsum_i8(i-1) + arr(i)
    end do

  end function cumsum_i8

  function cumsum_dp(arr)

    implicit none

    real(dp), dimension(:), intent(in) :: arr
    real(dp), dimension(size(arr,1))   :: cumsum_dp

    integer(i4) :: i

    cumsum_dp(1) = arr(1)
    do i=2, size(arr)
       cumsum_dp(i) = cumsum_dp(i-1) + arr(i)
    end do

  end function cumsum_dp

  function cumsum_dpc(arr)

    implicit none

    complex(dpc), dimension(:), intent(in) :: arr
    complex(dpc), dimension(size(arr,1))   :: cumsum_dpc

    integer(i4) :: i

    cumsum_dpc(1) = arr(1)
    do i=2, size(arr)
       cumsum_dpc(i) = cumsum_dpc(i-1) + arr(i)
    end do

  end function cumsum_dpc

  function cumsum_sp(arr)

    implicit none

    real(sp), dimension(:), intent(in) :: arr
    real(sp), dimension(size(arr,1))   :: cumsum_sp

    integer(i4) :: i

    cumsum_sp(1) = arr(1)
    do i=2, size(arr)
       cumsum_sp(i) = cumsum_sp(i-1) + arr(i)
    end do

  end function cumsum_sp

  function cumsum_spc(arr)

    implicit none

    complex(spc), dimension(:), intent(in) :: arr
    complex(spc), dimension(size(arr,1))   :: cumsum_spc

    integer(i4) :: i

    cumsum_spc(1) = arr(1)
    do i=2, size(arr)
       cumsum_spc(i) = cumsum_spc(i-1) + arr(i)
    end do

  end function cumsum_spc

  ! ------------------------------------------------------------------

  function imaxloc_i4(arr, mask)

    implicit none

    integer(i4), dimension(:), intent(in)           :: arr
    logical,     dimension(:), intent(in), optional :: mask
    integer(i4)                                     :: imaxloc_i4

    integer(i4), dimension(1) :: imax

    if (present(mask)) then
       imax = maxloc(arr, 1, mask)
    else
       imax = maxloc(arr, 1)
    endif
    imaxloc_i4 = imax(1)

  end function imaxloc_i4

  function imaxloc_i8(arr, mask)

    implicit none

    integer(i8), dimension(:), intent(in)           :: arr
    logical,     dimension(:), intent(in), optional :: mask
    integer(i4)                                     :: imaxloc_i8

    integer(i4), dimension(1) :: imax

    if (present(mask)) then
       imax = maxloc(arr, 1, mask)
    else
       imax = maxloc(arr, 1)
    endif
    imaxloc_i8 = imax(1)

  end function imaxloc_i8

  function imaxloc_dp(arr, mask)

    implicit none

    real(dp),   dimension(:), intent(in)           :: arr
    logical,    dimension(:), intent(in), optional :: mask
    integer(i4)                                    :: imaxloc_dp

    integer(i4), dimension(1) :: imax

    if (present(mask)) then
       imax = maxloc(arr, 1, mask)
    else
       imax = maxloc(arr, 1)
    endif
    imaxloc_dp = imax(1)

  end function imaxloc_dp

  function imaxloc_sp(arr, mask)

    implicit none

    real(sp),   dimension(:), intent(in)           :: arr
    logical,    dimension(:), intent(in), optional :: mask
    integer(i4)                                    :: imaxloc_sp

    integer(i4), dimension(1) :: imax

    if (present(mask)) then
       imax = maxloc(arr, 1, mask)
    else
       imax = maxloc(arr, 1)
    endif
    imaxloc_sp = imax(1)

  end function imaxloc_sp

  ! ------------------------------------------------------------------

  function iminloc_i4(arr, mask)

    implicit none

    integer(i4), dimension(:), intent(in)           :: arr
    logical,     dimension(:), intent(in), optional :: mask
    integer(i4)                                     :: iminloc_i4

    integer(i4), dimension(1) :: imin

    if (present(mask)) then
       imin = minloc(arr, 1, mask)
    else
       imin = minloc(arr, 1)
    endif
    iminloc_i4 = imin(1)

  end function iminloc_i4

  function iminloc_i8(arr, mask)

    implicit none

    integer(i8), dimension(:), intent(in)           :: arr
    logical,     dimension(:), intent(in), optional :: mask
    integer(i4)                                     :: iminloc_i8

    integer(i4), dimension(1) :: imin

    if (present(mask)) then
       imin = minloc(arr, 1, mask)
    else
       imin = minloc(arr, 1)
    endif
    iminloc_i8 = imin(1)

  end function iminloc_i8

  function iminloc_dp(arr, mask)

    implicit none

    real(dp),   dimension(:), intent(in)           :: arr
    logical,    dimension(:), intent(in), optional :: mask
    integer(i4)                                    :: iminloc_dp

    integer(i4), dimension(1) :: imin

    if (present(mask)) then
       imin = minloc(arr, 1, mask)
    else
       imin = minloc(arr, 1)
    endif
    iminloc_dp = imin(1)

  end function iminloc_dp

  function iminloc_sp(arr, mask)

    implicit none

    real(sp),   dimension(:), intent(in)           :: arr
    logical,    dimension(:), intent(in), optional :: mask
    integer(i4)                                    :: iminloc_sp

    integer(i4), dimension(1) :: imin

    if (present(mask)) then
       imin = minloc(arr, 1, mask)
    else
       imin = minloc(arr, 1)
    endif
    iminloc_sp = imin(1)

  end function iminloc_sp

  ! ------------------------------------------------------------------

  function linspace_i4(lower, upper, nstep)

    implicit none

    integer(i4), intent(in)       :: lower
    integer(i4), intent(in)       :: upper
    integer(i4), intent(in)       :: nstep
    integer(i4), dimension(nstep) :: linspace_i4

    linspace_i4 = lower + nint(arange(0.0_dp,real(nstep-1_i4,dp))/real(nstep-1_i4,dp) * real(upper-lower,dp), i4)

  end function linspace_i4

  function linspace_i8(lower, upper, nstep)

    implicit none

    integer(i8), intent(in)       :: lower
    integer(i8), intent(in)       :: upper
    integer(i4), intent(in)       :: nstep
    integer(i8), dimension(nstep) :: linspace_i8

    linspace_i8 = lower + nint(arange(0.0_dp,real(nstep-1_i4,dp))/real(nstep-1_i4,dp) * real(upper-lower,dp), i8)

  end function linspace_i8

  function linspace_dp(lower, upper, nstep)

    implicit none

    real(dp),    intent(in)       :: lower
    real(dp),    intent(in)       :: upper
    integer(i4), intent(in)       :: nstep
    real(dp),    dimension(nstep) :: linspace_dp

    linspace_dp = lower + arange(0.0_dp,real(nstep-1_i4,dp))/real(nstep-1_i4,dp) * (upper-lower)

  end function linspace_dp

  function linspace_sp(lower, upper, nstep)

    implicit none

    real(sp),    intent(in)       :: lower
    real(sp),    intent(in)       :: upper
    integer(i4), intent(in)       :: nstep
    real(sp),    dimension(nstep) :: linspace_sp

    linspace_sp = lower + arange(0.0_sp,real(nstep-1_i4,sp))/real(nstep-1_i4,sp) * (upper-lower)

  end function linspace_sp

  ! ------------------------------------------------------------------

  logical elemental pure function is_close_dp(a, b, rtol, atol, equal_nan) result(boolean)

    real(dp), intent(in)            :: a
    real(dp), intent(in)            :: b
    real(dp), intent(in), optional  :: rtol, atol
    logical, intent(in), optional   :: equal_nan

    real(dp)  :: rt, at
    logical   :: n

    rt = 1.0E-05_dp
    at = 1.0E-08_dp
    n = .false.
    if (present(rtol)) rt = rtol
    if (present(atol)) at = atol
    if (present(equal_nan)) n = equal_nan

    if ((rt < 0._dp).or.(at < 0._dp)) error stop
    boolean = (a == b)
    if (boolean) return
    boolean = (n.and.(is_nan_dp(a).or.is_nan_dp(b)))
    if (boolean) return
    if (.not.is_finite_dp(a) .or. .not.is_finite_dp(b)) return

    boolean = abs(a - b) <= max(rt * max(abs(a),abs(b)), at)

  end function is_close_dp



  logical elemental pure function is_close_sp(a, b, rtol, atol, equal_nan) result(boolean)

    real(sp), intent(in)            :: a
    real(sp), intent(in)            :: b
    real(sp), intent(in), optional  :: rtol, atol
    logical, intent(in), optional   :: equal_nan

    real(sp)  :: rt, at
    logical   :: n

    rt = 1.0E-05_sp
    at = 1.0E-08_sp
    n = .false.
    if (present(rtol)) rt = rtol
    if (present(atol)) at = atol
    if (present(equal_nan)) n = equal_nan

    if ((rt < 0._sp).or.(at < 0._sp)) error stop
    boolean = (a == b)
    if (boolean) return
    boolean = (n.and.(is_nan_sp(a).or.is_nan_sp(b)))
    if (boolean) return
    if (.not.is_finite_sp(a) .or. .not.is_finite_sp(b)) return

    boolean = abs(a - b) <= max(rt * max(abs(a),abs(b)), at)

  end function is_close_sp

  ! ------------------------------------------------------------------

  logical elemental pure function equal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = (a == b)
    if (boolean) return
    boolean = .not. ((epsilon(1.0_dp) * abs(b) - abs(a - b)) < 0.0_dp)

  end function equal_dp


  logical elemental pure function equal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = (a == b)
    if (boolean) return
    boolean = .not. ((epsilon(1.0_sp) * abs(b) - abs(a - b)) < 0.0_sp)

  end function equal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function greaterequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = equal_dp(a, b).or.(a > b)

  end function greaterequal_dp


  logical elemental pure function greaterequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = equal_sp(a, b).or.(a > b)

  end function greaterequal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function lesserequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = equal_dp(a, b).or.(a < b)

  end function lesserequal_dp


  logical elemental pure function lesserequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = equal_sp(a, b).or.(a < b)

  end function lesserequal_sp

  ! ------------------------------------------------------------------

  logical elemental pure function notequal_dp(a, b) result(boolean)

    real(dp), intent(in) :: a
    real(dp), intent(in) :: b

    boolean = .not.equal_dp(a, b)

  end function notequal_dp


  logical elemental pure function notequal_sp(a, b) result(boolean)

    real(sp), intent(in) :: a
    real(sp), intent(in) :: b

    boolean = .not.equal_sp(a, b)

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
