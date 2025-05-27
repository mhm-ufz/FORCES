!> \file mo_mad.f90
!> \copydoc mo_mad

!> \brief Median absolute deviation test.
!> \details This module provides a median absolute deviation (MAD) test.
!> \authors Matthias Cuntz, Mathias Zink
!> \date 2011 - 2012
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_mad

  USE mo_kind,       ONLY: i4, sp, dp
  USE mo_percentile, ONLY: median

  Implicit NONE

  PUBLIC :: mad             ! Mean absolute deviation test

  ! ------------------------------------------------------------------

  !>       \brief Mean absolute deviation test.

  !>       \details Mean absolute deviation test with optional z-value (default: 7) and mask,
  !!       and 3 optional variants:
  !!
  !!         - 0: raw values (default)
  !!         - 1: 1st derivative
  !!         - 2: 2nd derivative
  !!
  !!       Returns mask with true everywhere except where `0<(median-MAD*z/0.6745)` or `>(md+MAD*z/0.6745)` \n
  !!
  !!       If an optional mask is given, the mad test is only performed on those locations that correspond
  !!       to true values in the mask.\n
  !!
  !!       If tout is given mad returns the array with the enteries exceeding the treshold
  !!       being set to the threshold. With this setting arrays are accepted. \n
  !!       tout accepts:
  !!
  !!         - u: upper values are cut at the threshold
  !!         - l: lower values are cut at the threshold
  !!         - b: upper and lower values are cut at the threshold
  !!
  !!       With this setting only the variant 0 is available (no argument implemented).
  !!
  !!       \b Example
  !!
  !!       \code{.f90}
  !!             vec = (/ -0.25,0.68,0.94,1.15,2.26,2.35,2.37,2.40,2.47,2.54,2.62, &
  !!                       2.64,2.90,2.92,2.92,2.93,3.21,3.26,3.30,3.59,3.68,4.30, &
  !!                       4.64,5.34,5.42,8.01 /)
  !!             mask(:) = true
  !!             ! Sets last entry false
  !!             mask = mask .and. mad(vec, z=4., deriv=0, mask=mask)
  !!       \endcode
  !!
  !!       See also example in test directory.


  !>       \param[in]  "real(sp/dp) :: vec(:)"               1D-array with input numbers
  !>       \param[in]  "real(sp/dp) :: arr"                  nD-array with input numbers
  !>       \param[in]  "real(sp/dp), optional :: z"          Input is allowed to deviate maximum z standard deviations
  !!                                                         from the median (default: 7).
  !>       \param[in]  "integer(i4), optional :: deriv"      0: Act on raw input (default: 0)\n
  !!                                                         1: Use first derivatives\n
  !!                                                         2: Use 2nd derivatives
  !>       \param[in]  "logical, optional     :: mask(:)"    1D-array of logical values with size(vec).
  !!                                                         If present, only those locations in vec corresponding to the true
  !!                                                         values in mask are used. nD-array if tout is used.
  !>       \param[in]  "character(1):: tout"                 u: Trim only values above mad\n
  !!                                                         l: Trim only values below mad\n
  !!                                                          b: Trim values below and above mad

  !>       \retval "logical or real(sp/dp), dimension(size(arr)) :: out"        mask with true everywhere except where input
  !!                                                                            deviates more than z standard deviations from
  !!                                                                            median. If tout five, threshold value is returned.

  !>     \note
  !!           1st derivative is
  !!
  !!               d = datin[1:n]-datin[0:n-1]
  !!
  !!           because mean of left and right would give 0 for spikes.
  !!
  !>        \author Matthias Cuntz
  !>        \date Mar 2011

  !>        \author Mathhias Kelbling
  !>        \date May 2018
  !!          - mad_val added by Matthias Kelbling, May 2018

  ! ------------------------------------------------------------------

  INTERFACE mad
     MODULE PROCEDURE mad_sp, mad_dp, mad_val_dp, mad_val_sp
  END INTERFACE mad

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  FUNCTION mad_dp(arr, z, mask, deriv)

    IMPLICIT NONE

    REAL(dp),    DIMENSION(:),           INTENT(IN) :: arr
    REAL(dp),                  OPTIONAL, INTENT(IN) :: z
    LOGICAL,     DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    INTEGER(i4),               OPTIONAL, INTENT(IN) :: deriv
    LOGICAL,     DIMENSION(size(arr))               :: mad_dp

    LOGICAL,  DIMENSION(size(arr)) :: maske
    REAL(dp), DIMENSION(size(arr)) :: d
    LOGICAL,  DIMENSION(size(arr)) :: dmask
    INTEGER(i4) :: n, ideriv
    !INTEGER(i4) :: m
    REAL(dp)    :: iz, med, mabsdev, thresh

    n = size(arr)
    maske(:) = .true.
    if (present(mask)) then
       if (size(mask) /= n) stop 'Error mad_dp: size(mask) /= size(arr)'
       maske = mask
    endif
    if (present(z)) then
       iz = z
    else
       iz = 7.0_dp
    endif
    if (present(deriv)) then
       ideriv = deriv
    else
       ideriv = 0
    endif

    select case(ideriv)
    case(0) ! pure values
       !m       = count(maske)
       med     = median(arr,mask=maske)
       mabsdev = median(abs(arr-med),mask=maske)
       thresh  = mabsdev * iz/0.6745_dp
       mad_dp     = (arr .ge. (med-thresh)) .and. (arr .le. (med+thresh)) .and. maske
    case(1) ! 1st derivative
       ! d(1:n-2) = 0.5_dp* (arr(3:n) - arr(1:n-2)) ! does not work -> ask Clemens & Matthias M
       d(1:n-1)     = arr(2:n) - arr(1:n-1)
       dmask(1:n-1) = maske(2:n) .and. maske(1:n-1)
       !m            = count(dmask(1:n-1))
       med          = median(d(1:n-1),mask=dmask(1:n-1))
       mabsdev      = median(abs(d(1:n-1)-med),mask=dmask(1:n-1))
       thresh       = mabsdev * iz/0.6745_dp
       mad_dp(n)       = .true.
       mad_dp(1:n-1)   = (d(1:n-1) .ge. (med-thresh)) .and. (d(1:n-1) .le. (med+thresh)) .and. dmask(1:n-1)
    case(2) ! -2nd derivative
       d(1:n-2)     = arr(2:n-1) + arr(2:n-1) - arr(1:n-2) - arr(3:n)
       dmask(1:n-2) = maske(2:n-1) .and. maske(1:n-2) .and. maske(3:n)
       !m            = count(dmask(1:n-2))
       med          = median(d(1:n-2),mask=dmask(1:n-2))
       mabsdev      = median(abs(d(1:n-2)-med),mask=dmask(1:n-2))
       thresh       = mabsdev * iz/0.6745_dp
       mad_dp(1)       = .true.
       mad_dp(n)       = .true.
       mad_dp(2:n-1)   = (d(1:n-2) .ge. (med-thresh)) .and. (d(1:n-2) .le. (med+thresh)) .and. dmask(1:n-2)
    case default
       stop 'Unimplemented option in mad_dp'
    end select

  END FUNCTION mad_dp


  FUNCTION mad_sp(arr, z, mask, deriv)

    IMPLICIT NONE

    REAL(sp),    DIMENSION(:),           INTENT(IN) :: arr
    REAL(sp),                  OPTIONAL, INTENT(IN) :: z
    LOGICAL,     DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    INTEGER(i4),               OPTIONAL, INTENT(IN) :: deriv
    LOGICAL,     DIMENSION(size(arr))               :: mad_sp

    LOGICAL,  DIMENSION(size(arr)) :: maske
    REAL(sp), DIMENSION(size(arr)) :: d
    LOGICAL,  DIMENSION(size(arr)) :: dmask
    INTEGER(i4) :: n, ideriv
    !INTEGER(i4) :: m
    REAL(sp)    :: iz, med, mabsdev, thresh

    n = size(arr)
    maske(:) = .true.
    if (present(mask)) then
       if (size(mask) /= n) stop 'Error mad_sp: size(mask) /= size(arr)'
       maske = mask
    endif
    if (present(z)) then
       iz = z
    else
       iz = 7.0_sp
    endif
    if (present(deriv)) then
       ideriv = deriv
    else
       ideriv = 0
    endif

    select case(ideriv)
    case(0) ! pure values
       !m       = count(maske)
       med     = median(arr,mask=maske)
       mabsdev = median(abs(arr-med),mask=maske)
       thresh  = mabsdev * iz/0.6745_sp
       mad_sp     = (arr .ge. (med-thresh)) .and. (arr .le. (med+thresh)) .and. maske
    case(1) ! 1st derivative
       ! d(1:n-2) = 0.5_sp* (arr(3:n) - arr(1:n-2)) ! does not work -> ask Clemens & Matthias M
       d(1:n-1)     = arr(2:n) - arr(1:n-1)
       dmask(1:n-1) = maske(2:n) .and. maske(1:n-1)
       !m            = count(dmask(1:n-1))
       med          = median(d(1:n-1),mask=dmask(1:n-1))
       mabsdev      = median(abs(d(1:n-1)-med),mask=dmask(1:n-1))
       thresh       = mabsdev * iz/0.6745_sp
       mad_sp(n)       = .true.
       mad_sp(1:n-1)   = (d(1:n-1) .ge. (med-thresh)) .and. (d(1:n-1) .le. (med+thresh)) .and. dmask(1:n-1)
    case(2) ! -2nd derivative
       d(1:n-2)     = arr(2:n-1) + arr(2:n-1) - arr(1:n-2) - arr(3:n)
       dmask(1:n-2) = maske(2:n-1) .and. maske(1:n-2) .and. maske(3:n)
       !m            = count(dmask(1:n-2))
       med          = median(d(1:n-2),mask=dmask(1:n-2))
       mabsdev      = median(abs(d(1:n-2)-med),mask=dmask(1:n-2))
       thresh       = mabsdev * iz/0.6745_sp
       mad_sp(1)       = .true.
       mad_sp(n)       = .true.
       mad_sp(2:n-1)   = (d(1:n-2) .ge. (med-thresh)) .and. (d(1:n-2) .le. (med+thresh)) .and. dmask(1:n-2)
    case default
       stop 'Unimplemented option in mad_sp'
    end select

  END FUNCTION mad_sp

  ! ------------------------------------------------------------------

  FUNCTION mad_val_dp(arr, z, mask, tout, mval)

    IMPLICIT NONE

    REAL(dp),    DIMENSION(:),           INTENT(IN) :: arr
    REAL(dp),                  OPTIONAL, INTENT(IN) :: z, mval
    LOGICAL,     DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp),    DIMENSION(size(arr))               :: mad_val_dp
    CHARACTER(1)                                    :: tout ! type out
    ! u : cut upper; l : cut lower; b : cut upper and lower

    LOGICAL,  DIMENSION(size(arr)) :: maske
    INTEGER(i4) :: n
    REAL(dp)    :: iz, med, mabsdev, thresh

    n = size(arr)
    maske(:) = .true.
    if (present(mask)) then
       if (size(mask) /= n) stop 'Error mad_val_dp: size(mask) /= size(arr)'
       maske = mask
    endif
    if (present(z)) then
       iz = z
    else
       iz = 7.0_dp
    endif

    if (present(mval)) then
       where (abs(arr - mval) .lt. tiny(1._dp) ) maske = .false.
       ! reset if no values remain
       if (.not. any(maske)) then
          where ( abs(arr - mval) .lt. tiny(1._dp) ) maske = .true.
       end if
    endif

    med     = median(arr,mask=maske)
    mabsdev = median(abs(arr-med),mask=maske)
    thresh  = mabsdev * iz/0.6745_dp
    mad_val_dp = arr

    select case(tout)
    case("u")
      ! print *, "The threshold is set to", med, "+", thresh
      where ((mad_val_dp .gt. (med+thresh)) &
           .and. maske) mad_val_dp = med+thresh
    case("l")
      ! print *, "The threshold is set to", med, "-", thresh
      where ((mad_val_dp .lt. (med-thresh)) &
           .and. maske) mad_val_dp = med-thresh
    case("b")
      ! print *, "The threshold is set to", med, "+/-", thresh
      where ((mad_val_dp .gt. (med+thresh)) &
           .and. maske) mad_val_dp = med+thresh
      where ((mad_val_dp .lt. (med-thresh)) &
           .and. maske) mad_val_dp = med-thresh
    case default
       stop 'Unimplemented option in mad_val_dp'
    end select

  END FUNCTION mad_val_dp

  ! ------------------------------------------------------------------

  FUNCTION mad_val_sp(arr, z, mask, tout, mval)

    IMPLICIT NONE

    REAL(sp),    DIMENSION(:),           INTENT(IN) :: arr
    REAL(sp),                  OPTIONAL, INTENT(IN) :: z, mval
    LOGICAL,     DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp),    DIMENSION(size(arr))               :: mad_val_sp
    CHARACTER(1)                                    :: tout ! type out
    ! u : cut upper; l : cut lower; b : cut upper and lower

    LOGICAL,  DIMENSION(size(arr)) :: maske
    INTEGER(i4) :: n
    REAL(sp)    :: iz, med, mabsdev, thresh

    n = size(arr)
    maske(:) = .true.
    if (present(mask)) then
       if (size(mask) /= n) stop 'Error mad_val_sp: size(mask) /= size(arr)'
       maske = mask
    endif
    if (present(z)) then
       iz = z
    else
       iz = 7.0_sp
    endif

    if (present(mval)) then
       where (abs(arr - mval) .lt. tiny(1._sp)) maske = .false.
       ! reset if no values remain
       if (.not. any(maske)) then
          where ( abs(arr - mval) .lt. tiny(1._dp) ) maske = .true.
       end if
    endif

    med     = median(arr,mask=maske)
    mabsdev = median(abs(arr-med),mask=maske)
    thresh  = mabsdev * iz/0.6745_sp
    mad_val_sp = arr
    select case(tout)
    case("u")
      print *, "The threshold is set to", med, "+", thresh
      where ((mad_val_sp .gt. (med+thresh)) &
           .and. maske) mad_val_sp = med+thresh
    case("l")
      print *, "The threshold is set to", med, "-", thresh
      where ((mad_val_sp .lt. (med-thresh)) &
           .and. maske) mad_val_sp = med-thresh
    case("b")
      print *, "The threshold is set to", med, "+/-", thresh
      where ((mad_val_sp .gt. (med+thresh)) &
           .and. maske) mad_val_sp = med+thresh
      where ((mad_val_sp .lt. (med-thresh)) &
           .and. maske) mad_val_sp = med-thresh
    case default
       stop 'Unimplemented option in mad_val_sp'
    end select

  END FUNCTION mad_val_sp

  ! ------------------------------------------------------------------


END MODULE mo_mad
