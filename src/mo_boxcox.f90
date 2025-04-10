!> \file mo_boxcox.f90
!> \copydoc mo_boxcox

!> \brief Box-Cox transformation of data.
!> \details This module contains routines to calculate the Box-Cox transformation
!!          as well as estimating the best exponent for the Box-Cox transformation
!> \changelog
!! - March 2011, Matthias Cuntz
!!   - modified Python code of Travis Oliphant (2002): boxcox, llf_boxcox
!! - Dec 2019: Robert Schweppe:
!!   - removed NR code (get_boxcox)
!> \author Mathias Cuntz
!> \date Aug 2011
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_boxcox

  USE mo_kind, ONLY : sp, dp
  USE mo_utils, only : le

  IMPLICIT NONE

  PUBLIC :: boxcox
  PUBLIC :: invboxcox

  ! ------------------------------------------------------------------

  !>        \brief Transform a positive dataset with a Box-Cox power transformation.

  !>        \details Calculate Box-Cox power transformed values given the original values and the exponent lambda.\n
  !!          \f[ w_\text{Box-Cox}(x) =
  !!                          \begin{cases}
  !!                           \frac{x^\lambda - 1}{\lambda}&\text{, if }\lambda \neq 0 \\
  !!                           \ln{x}&\text{, if }\lambda = 0
  !!                          \end{cases} \f]
  !!         If an optional mask is given, then the Box-Cox transformation is only performed on
  !!         those locations that correspond to true values in the mask.\n
  !!         \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!         \f$x\f$ can be scalar or vector.\n
  !!
  !!         \b Example
  !!
  !!         \code{.f90}
  !!          out = boxcox(x, lmbda, mask=mask)
  !!         \endcode
  !!
  !!         See also test folder for a detailed example, "pf_tests/test_mo_boxcox".


  !>        \param[in]  "real(sp/dp) :: x"           Scalar/1D-array with input numbers (`>0.`)
  !>        \param[in]  "real(sp/dp) :: lmbda"       Exponent power of Box-Cox transform (`>= 0.`)
  !>        \param[in]  "logical, optional :: mask"  Scalar/1D-array of logical values with `size(x)`.
  !!                                                 If present, only those locations in vec corresponding
  !!                                                 to the true values in mask are used.
  !>        \retval "real(sp/dp) :: boxcox"     Power transformed values (at `mask=.true.`)

  !>     \note Input values must be positive, i.e. \f$x > 0\f$.

  !>     \author Matthias Cuntz
  !>     \date March 2011
  !!            - Modified Python code of Travis Oliphant (2002): boxcox, llf_boxcox, get_boxcox
  !!            - Modified numerical recipes: brent, mnbrak, swap, shft
  !>     \date Dec 2021
  !!            - Updated doxygen docs
  INTERFACE boxcox
    MODULE PROCEDURE boxcox_sp, boxcox_dp
  END INTERFACE boxcox

  ! ------------------------------------------------------------------

  !>        \brief Back-transformation of Box-Cox-transformed data.

  !>        \details Calculate the inverse Box-Cox given the transformed values and the exponent lambda.
  !!          \f[ w_\text{Box-Cox}^{-1}(y) =
  !!                          \begin{cases}
  !!                           (\lambda y + 1)^{\frac{1}{\lambda}}&\text{, if }\lambda \neq 0 \\
  !!                           e^{y}&\text{, if }\lambda = 0
  !!                          \end{cases} \f]
  !!         If an optional mask is given, then the inverse Box-Cox transformation is only performed on
  !!         those locations that correspond to true values in the mask.\n
  !!         \f$y\f$ can be single or double precision. The result will have the same numerical precision.\n
  !!         \f$y\f$ can be scalar or vector.
  !!
  !!         \b Example
  !!
  !!         \code{.f90}
  !!         out = invboxcox(x, lmbda, mask=mask)
  !!         \endcode
  !!
  !!         See also test folder for a detailed example, "pf_tests/test_mo_boxcox".

  !>        \param[in]  "real(sp/dp) :: y"              Scalar/1D-array with input numbers (`>0.`)
  !>        \param[in]  "real(sp/dp) :: lmbda"          Exponent power of Box-Cox transform (`>= 0.`)
  !>        \param[in]  "optional, logical :: mask"     1D-array of logical values with `size(x)`.
  !!                                                    If present, only those locations in vec corresponding to the
  !!                                                    true values in mask are used.
  !!                                                    Only applicable if `x` is a 1D-array.
  !>        \retval    "real(sp/dp) :: invboxcox"       Back transformed values (at `mask=.true.`)

  !>     \author Matthias Cuntz
  !>     \author Juliane Mai
  !>     \author Sebastian Müller
  !>     \date March 2011
  !!            - Modified MC: Python code of Travis Oliphant (2002): boxcox, llf_boxcox, get_boxcox
  !!            - Modified MC: numerical recipes: brent, mnbrak, swap, shft
  !!            - Modified JM: scalar version of invboxcox
  INTERFACE invboxcox
     MODULE PROCEDURE invboxcox_0d_sp, invboxcox_0d_dp, invboxcox_1d_sp, invboxcox_1d_dp
  END INTERFACE invboxcox

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  FUNCTION boxcox_sp(x, lmbda, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(in) :: x
    REAL(sp), INTENT(in) :: lmbda
    LOGICAL, DIMENSION(:), INTENT(in), OPTIONAL :: mask
    REAL(sp), DIMENSION(size(x)) :: boxcox_sp

    LOGICAL, DIMENSION(size(x)) :: maske
    REAL(sp) :: lmbda1

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error boxcox_sp: size(mask) /= size(x)'
      maske = mask
    endif
    if (any((le(x, 0.0_sp)) .and. maske)) stop 'Error boxcox_sp: x <= 0'
    if (abs(lmbda) < tiny(0.0_sp)) then
      where (maske)
        boxcox_sp = log(x)
      elsewhere
        boxcox_sp = x
      end where
    else
      lmbda1 = 1.0_sp / lmbda
      boxcox_sp = merge((exp(lmbda * log(x)) - 1.0_sp) * lmbda1, x, maske)
    endif

  END FUNCTION boxcox_sp


  FUNCTION boxcox_dp(x, lmbda, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(in) :: x
    REAL(dp), INTENT(in) :: lmbda
    LOGICAL, DIMENSION(:), INTENT(in), OPTIONAL :: mask
    REAL(dp), DIMENSION(size(x)) :: boxcox_dp

    LOGICAL, DIMENSION(size(x)) :: maske
    REAL(dp) :: lmbda1

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error boxcox_dp: size(mask) /= size(x)'
      maske = mask
    endif
    if (any((le(x, 0.0_dp)) .and. maske)) then
      stop 'Error boxcox_dp: x <= 0'
    end if
    if (abs(lmbda) < tiny(0.0_dp)) then
      where (maske)
        boxcox_dp = log(x)
      elsewhere
        boxcox_dp = x
      end where
    else
      lmbda1 = 1.0_dp / lmbda
      boxcox_dp = merge((exp(lmbda * log(x)) - 1.0_dp) * lmbda1, x, maske)
    endif

  END FUNCTION boxcox_dp

  ! ------------------------------------------------------------------

  FUNCTION invboxcox_1d_sp(x, lmbda, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(in) :: x
    REAL(sp), INTENT(in) :: lmbda
    LOGICAL, DIMENSION(:), INTENT(in), OPTIONAL :: mask
    REAL(sp), DIMENSION(size(x)) :: invboxcox_1d_sp

    LOGICAL, DIMENSION(size(x)) :: maske
    REAL(sp) :: lmbda1
    REAL(sp), DIMENSION(size(x)) :: temp

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error invboxcox_1d_sp: size(mask) /= size(x)'
      maske = mask
    endif
    if (abs(lmbda) < tiny(0.0_sp)) then
      where (maske)
        invboxcox_1d_sp = exp(x)
      elsewhere
        invboxcox_1d_sp = x
      end where
    else
      lmbda1 = 1.0_sp / lmbda
      temp = lmbda * x + 1.0_sp
      where (temp > 0.0_sp)
        temp = exp(lmbda1 * log(temp))
      elsewhere
        temp = 0.0_sp
      end where
      invboxcox_1d_sp = merge(temp, x, maske)
    endif

  END FUNCTION invboxcox_1d_sp

  FUNCTION invboxcox_1d_dp(x, lmbda, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(in) :: x
    REAL(dp), INTENT(in) :: lmbda
    LOGICAL, DIMENSION(:), INTENT(in), OPTIONAL :: mask
    REAL(dp), DIMENSION(size(x)) :: invboxcox_1d_dp

    LOGICAL, DIMENSION(size(x)) :: maske
    REAL(dp) :: lmbda1
    REAL(dp), DIMENSION(size(x)) :: temp

    maske(:) = .true.
    if (present(mask)) then
      if (size(mask) /= size(x)) stop 'Error invboxcox_1d_dp: size(mask) /= size(x)'
      maske = mask
    endif
    if (abs(lmbda) < tiny(0.0_dp)) then
      where (maske)
        invboxcox_1d_dp = exp(x)
      elsewhere
        invboxcox_1d_dp = x
      end where
    else
      lmbda1 = 1.0_dp / lmbda
      temp = lmbda * x + 1.0_dp
      where (temp > 0.0_dp)
        temp = exp(lmbda1 * log(temp))
      elsewhere
        temp = 0.0_dp
      end where
      invboxcox_1d_dp = merge(temp, x, maske)
    endif

  END FUNCTION invboxcox_1d_dp

  FUNCTION invboxcox_0d_sp(x, lmbda)

    IMPLICIT NONE

    REAL(sp), INTENT(in) :: x
    REAL(sp), INTENT(in) :: lmbda
    REAL(sp) :: invboxcox_0d_sp

    REAL(sp) :: lmbda1
    REAL(sp) :: temp

    if (abs(lmbda) < tiny(0.0_sp)) then
      invboxcox_0d_sp = exp(x)
    else
      lmbda1 = 1.0_sp / lmbda
      temp = lmbda * x + 1.0_sp
      if (temp > 0.0_sp) then
        temp = exp(lmbda1 * log(temp))
      else
        temp = 0.0_sp
      end if
      invboxcox_0d_sp = temp
    endif

  END FUNCTION invboxcox_0d_sp

  FUNCTION invboxcox_0d_dp(x, lmbda)

    IMPLICIT NONE

    REAL(dp), INTENT(in) :: x
    REAL(dp), INTENT(in) :: lmbda
    REAL(dp) :: invboxcox_0d_dp

    REAL(dp) :: lmbda1
    REAL(dp) :: temp

    if (abs(lmbda) < tiny(0.0_dp)) then
      invboxcox_0d_dp = exp(x)
    else
      lmbda1 = 1.0_dp / lmbda
      temp = lmbda * x + 1.0_dp
      if (temp > 0.0_dp) then
        temp = exp(lmbda1 * log(temp))
      else
        temp = 0.0_dp
      end if
      invboxcox_0d_dp = temp
    endif

  END FUNCTION invboxcox_0d_dp

END MODULE mo_boxcox
