!> \file mo_corr.f90
!> \copydoc mo_corr

!> \brief Provides autocorrelation function calculations.
!> \author Sebastian Mueller
!> \date Dec 2019
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_corr

  USE mo_kind, ONLY : i4, sp, dp

  Implicit NONE

  PUBLIC :: autocorr     ! Autocorrelation coefficient at lag k = autocoeffk(k)/autocoeffk(0)

  ! ------------------------------------------------------------------

  !>        \brief Autocorrelation function with lag k.

  !>        \details Element at lag k of autocorrelation function
  !!             \f[ ACF(x,k) = \frac{s_{x,k}}{s_{x,0}} \f]
  !!         where \f$ s_{x,k} \f$ is the autocorrelation coefficient
  !!             \f[ s_{x,k} = \langle (x_i-\bar{x})(x_{i+k}-\bar{x})\rangle \f]
  !!
  !!         If an optional mask is given, the calculations are only over those locations that correspond
  !!         to true values in the mask.\n
  !!         \f$ x \f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!         \b Example
  !!
  !!         Autocorrelation of 0 time steps
  !!         \code{.f90}
  !!         ak = autocorr(x, 0, mask=mask)
  !!         ---> ak = 1
  !!         \endcode

  !>        \param[in]  "real(sp/dp) :: x(:)"                 Time series.
  !>        \param[in]  "integer(i4) :: k[(:)]"               Lag for autocorrelation.
  !>        \param[in]  "optional, logical     :: mask(:)"    1D-array of logical values with `size(vec)`.
  !!                                                          If present, only those locations in vec corresponding
  !!                                                          to the true values in mask are used.
  !>        \retval    "real(sp/dp) :: ak[(:)]"               Coefficient of autocorrelation function at lag k.

  !>        \author Matthias Cuntz
  !>        \date Nov 2011

  !>        \author Stephan Thober
  !>        \date Nov 2012
  !!          - added 1d version

  !>        \author Sebastion Mueller
  !>        \date Dec 2019
  !!          - rewritten

  INTERFACE autocorr
    MODULE PROCEDURE autocorr_sp, autocorr_dp, autocorr_1d_sp, autocorr_1d_dp
  END INTERFACE autocorr

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  FUNCTION autocorr_dp(x, k, mask) result(acf)

    USE mo_moment, ONLY: moment

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: k
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: acf

    INTEGER(i4) :: kk  ! absolute value of lag k
    INTEGER(i4) :: nn  ! number of true values in mask
    REAL(dp) :: n   ! real of nn
    REAL(dp) :: mean, var  ! moments
    REAL(dp), DIMENSION(size(x)) :: x_shift
    LOGICAL, DIMENSION(size(x)) :: maske

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error autocorr_dp: size(mask) /= size(x)'
      maske = mask
    end if

    ! calculate mean and population variance of x
    call moment(x, mean=mean, variance=var, mask=maske, sample=.false.)
    ! shift x to 0 mean
    x_shift = x - mean
    ! use absolute value of k
    kk = abs(k)
    nn = size(x)
    n = real(count(maske(1 : nn - kk).and.maske(1 + kk : nn)), dp)
    ! calculate the auto-correlation function
    acf = sum(x_shift(1 : nn - kk) * x_shift(1 + kk : nn), mask = (maske(1 : nn - kk) .and. maske(1 + kk : nn))) / n / var

  END FUNCTION autocorr_dp

  FUNCTION autocorr_sp(x, k, mask) result(acf)

    USE mo_moment, ONLY: moment

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: k
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: acf

    INTEGER(i4) :: kk  ! absolute value of lag k
    INTEGER(i4) :: nn  ! number of true values in mask
    REAL(sp) :: n   ! real of nn
    REAL(sp) :: mean, var  ! moments
    REAL(sp), DIMENSION(size(x)) :: x_shift
    LOGICAL, DIMENSION(size(x)) :: maske

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error autocorr_sp: size(mask) /= size(x)'
      maske = mask
    end if

    ! calculate mean and population variance of x
    call moment(x, mean=mean, variance=var, mask=maske, sample=.false.)
    ! shift x to 0 mean
    x_shift = x - mean
    ! use absolute value of k
    kk = abs(k)
    nn = size(x)
    n = real(count(maske(1 : nn - kk).and.maske(1 + kk : nn)), sp)
    ! calculate the auto-correlation function
    acf = sum(x_shift(1 : nn - kk) * x_shift(1 + kk : nn), mask = (maske(1 : nn - kk) .and. maske(1 + kk : nn))) / n / var

  END FUNCTION autocorr_sp

  FUNCTION autocorr_1d_dp(x, k, mask) result(acf)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), DIMENSION(:), INTENT(IN) :: k
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    INTEGER(i4) :: i
    REAL(dp), DIMENSION(size(k)) :: acf

    if (present(mask)) then
      do i = 1, size(k)
        acf(i) = autocorr(x, k(i), mask)
      end do
    else
      do i = 1, size(k)
        acf(i) = autocorr(x, k(i))
      end do
    end if


  END FUNCTION autocorr_1d_dp

  FUNCTION autocorr_1d_sp(x, k, mask) result(acf)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), DIMENSION(:), INTENT(IN) :: k
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    INTEGER(i4) :: i
    REAL(sp), DIMENSION(size(k)) :: acf

    if (present(mask)) then
      do i = 1, size(k)
        acf(i) = autocorr(x, k(i), mask)
      end do
    else
      do i = 1, size(k)
        acf(i) = autocorr(x, k(i))
      end do
    end if

  END FUNCTION autocorr_1d_sp

END MODULE mo_corr
