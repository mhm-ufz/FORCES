!> \file mo_errormeasures.f90
!> \copydoc mo_errormeasures

!> \brief Calculation of error measures.
!> \details This module contains routines for the masked calculation of
!! error measures like MSE, RMSE, BIAS, SSE, NSE, ...
!> \note all except variance and standard deviation are population and not sample moments,
!!       i.e. they are normally divided by n and not (n-1)
!> \authors Mathias Zink
!> \date Aug 2012
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_errormeasures

  USE mo_kind, ONLY : i4, sp, dp

  IMPLICIT NONE

  PUBLIC :: BIAS                         ! bias
  PUBLIC :: KGE                          ! Kling-Gupta efficiency measure
  PUBLIC :: KGEnocorr                    ! KGE without correlation
  PUBLIC :: LNNSE                        ! Logarithmic Nash Sutcliffe efficiency
  PUBLIC :: MAE                          ! Mean of absolute errors
  PUBLIC :: MSE                          ! Mean of squared errors
  PUBLIC :: NSE                          ! Nash Sutcliffe efficiency
  PUBLIC :: SSE                          ! Sum of squared errors
  PUBLIC :: SAE                          ! Sum of absolute errors
  PUBLIC :: RMSE                         ! Root mean squared error
  PUBLIC :: WNSE                         ! weighted NSE

  ! ------------------------------------------------------------------

  !>      \brief Calculates bias.

  !>      \details  Calculates the bias
  !!
  !!      \f[BIAS = \bar y - \bar x\f]
  !!
  !!      Where \f$ \bar y \f$ and \f$ \bar x \f$ are means of the data points.
  !!
  !!      If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!      \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!      \b Example
  !!
  !!      \code{.f90}
  !!         vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!         vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!         m   = BIAS(vec1, vec2, mask=(vec >= 0.))
  !!         --> m = 0.0
  !!      \endcode
  !!
  !!     See also example in test directory.


  !>      \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>      \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.

  !>      \returns    "real(sp/dp) :: BIAS"                     Bias.

  !>     \note
  !!     Input values must be floating points.

  !>      \authors Matthias Zink
  !>      \date Sept 2012
  INTERFACE BIAS
    MODULE PROCEDURE BIAS_sp_1d, BIAS_dp_1d, BIAS_sp_2d, BIAS_dp_2d, BIAS_sp_3d, BIAS_dp_3d
  END INTERFACE BIAS

  ! ------------------------------------------------------------------

  !>        \brief Kling-Gupta-Efficiency measure.

  !>        \details
  !!        The Kling-Gupta model efficiency coefficient \f$ KGE \f$ is
  !!            \f[ KGE = 1 - \sqrt{( (1-r)^2 + (1-\alpha)^2 + (1-\beta)^2 )} \f]
  !!        where \n
  !!            \f$ r \f$      = Pearson product-moment correlation coefficient \n
  !!            \f$ \alpha \f$ = ratio of simulated mean to observed mean  \n
  !!            \f$ \beta  \f$ = ratio of simulated standard deviation to
  !!                             observed standard deviation \n
  !!        This three measures are calculated between two arrays (1d, 2d, or 3d).
  !!        Usually, one is an observation and the second is a modelled variable.\n
  !!
  !!        The higher the KGE the better the observation and simulation are matching.
  !!        The upper limit of KGE is 1.\n
  !!
  !!        Therefore, if you apply a minimization algorithm to calibrate regarding
  !!        KGE you have to use the objective function
  !!            \f[ obj\_value = 1.0 - KGE \f]
  !!        which has then the optimum at 0.0.
  !!        (Like for the NSE where you always optimize 1-NSE.)\n
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        para = (/ 1., 2, 3., -999., 5., 6. /)
  !!        kge = kge(x,y,mask=mask)
  !!        \endcode
  !!
  !!        \b Literature
  !!
  !>        1. Gupta, Hoshin V., et al.
  !!           _"Decomposition of the mean squared error and NSE performance criteria:
  !!           Implications for improving hydrological modelling"_.
  !!           Journal of Hydrology 377.1 (2009): 80-91.
  !!
  !>        \param[in]  "real(sp/dp)   :: x, y"           1D/2D/3D-array with input numbers
  !>        \param[in]  "logical, optional     :: mask"   1D/2D/3D-array of logical values with size(x/y).
  !>        \retval     "real(sp/dp) ::kge"               Kling-Gupta-Efficiency (value less equal 1.0)

  !>       \note Input values must be floating points. \n

  !>        \author Rohini Kumar
  !>        \date August 2014

  !>        \author R. Kumar, J. Mai, & O. Rakovec
  !>        \date Sep. 2014
  !!          - remove double packing of input data (bug)
  !!          - KGE instead of 1.0-KGE
  !!          - 1d, 2d, 3d, version in sp and dp

  INTERFACE KGE
    MODULE PROCEDURE KGE_dp_1d, KGE_dp_2d, KGE_dp_3d, KGE_sp_1d, KGE_sp_2d, KGE_sp_3d
  END INTERFACE KGE

  ! ------------------------------------------------------------------

  !>        \brief Kling-Gupta-Efficiency measure without correlation

  !>        \details The modified Kling-Gupta model efficiency coefficient \f$ KGEnocorr \f$ is
  !!            \f[ KGEnocorr = 1 - \sqrt{( (1-\alpha)^2 + (1-\beta)^2 )} \f]
  !!        where \n
  !!            \f$ \alpha \f$ = ratio of simulated mean to observed mean  \n
  !!            \f$ \beta  \f$ = ratio of simulated standard deviation to
  !!                             observed standard deviation \n
  !!        This two measures are calculated between two arrays (1d, 2d, or 3d).
  !!        Usually, one is an observation and the second is a modelled variable.\n
  !!
  !!        The higher the KGEnocorr the better the observation and simulation are matching.
  !!        The upper limit of KGEnocorr is 1.\n
  !!
  !!        Therefore, if you apply a minimization algorithm to calibrate regarding
  !!        KGEnocorr you have to use the objective function
  !!            \f[ obj\_value = 1.0 - KGEnocorr \f]
  !!        which has then the optimum at 0.0.
  !!        (Like for the NSE where you always optimize 1-NSE.)\n
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        para = (/ 1., 2, 3., -999., 5., 6. /)
  !!        kgenocorr = kgenocorr(x,y,mask=mask)
  !!        \endcode
  !!
  !!        \b Literature
  !!
  !!        1. Gupta, Hoshin V., et al.
  !!           _"Decomposition of the mean squared error and NSE performance criteria:
  !!           Implications for improving hydrological modelling"_.
  !!           Journal of Hydrology 377.1 (2009): 80-91.
  !!
  !>        \param[in]  "real(sp/dp)   :: x, y"           1D/2D/3D-array with input numbers
  !>        \param[in]  "logical, optional     :: mask"   1D/2D/3D-array of logical values with size(x/y).
  !>        \retval     "real(sp/dp) :: kgenocorr"        Kling-Gupta-Efficiency without correlation (value less equal 1.0)

  !>       \note Input values must be floating points. \n

  !>        \author Rohini Kumar
  !>        \date Aug 2014

  !>        \author M. Schroen
  !>        \date Jul 2017
  !!          - add KGEnocorr

  !>        \author R. Kumar, J. Mai, & O. Rakovec
  !>        \date Sep 2014
  !!          - remove double packing of input data (bug)
  !!          - KGE instead of 1.0-KGE
  !!          - 1d, 2d, 3d, version in sp and dp

  INTERFACE KGEnocorr
    MODULE PROCEDURE KGEnocorr_dp_1d, KGEnocorr_dp_2d, KGEnocorr_dp_3d, KGEnocorr_sp_1d, KGEnocorr_sp_2d, KGEnocorr_sp_3d
  END INTERFACE KGEnocorr


  ! ------------------------------------------------------------------

  !>        \brief Logarithmic Nash Sutcliffe Efficiency.

  !>        \details  Calculates the Logarithmic Nash Sutcliffe Efficiency
  !!
  !!        \f[LNNSE = \frac{\sum_i(\ln(y_i) - \ln(x_i))^2}  {\sum_i (\ln(x_i) - \ln(\bar x))^2 }\f]
  !!
  !!        where \f$ x\f$ is the observation and \f$ y\f$ is the modelled data.\n
  !!
  !!        If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!        Note that the mask is intent inout, since values which are less or equal zero will be masked additionally.
  !!        \f$ x \f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = LNNSE(vec1, vec2, mask=(vec >= 0.))
  !!        --> m = 1.0
  !!        \endcode
  !!
  !!        See also example in test directory.

  !>      \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>      \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.

  !>      \retval    "real(sp/dp) :: LNNSE"                     LNNSE.

  !>     \note
  !!     Input values must be floating points.

  !>      \author Juliane Mai
  !>      \date May 2013

  !>      \author Rohini Kumar
  !>      \date May 2013
  !!        - mean of logQ
  INTERFACE LNNSE
    MODULE PROCEDURE LNNSE_sp_1d, LNNSE_dp_1d, LNNSE_dp_2d, LNNSE_sp_2d, LNNSE_sp_3d, LNNSE_dp_3d
  END INTERFACE LNNSE

  ! ------------------------------------------------------------------

  !>        \brief Mean absolute error.

  !>        \details Calculates the mean absolute error,
  !!
  !!        \f[ MAE = \sum_i\frac{|y_i - x_i|}{N_\text{mask}} \f]
  !!
  !!        If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!        \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = MAE(vec1, vec2, mask=(vec >= 0.))
  !!        --> m = 0.0
  !!        \endcode
  !!
  !!        See also example in test directory.

  !>      \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>      \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.

  !>      \returns    "real(sp/dp) :: MAE"                     MAE.

  !>     \note
  !!     Input values must be floating points.

  !>      \authors Matthias Zink
  !>      \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE MAE
    MODULE PROCEDURE MAE_sp_1d, MAE_dp_1d, MAE_sp_2d, MAE_dp_2d, MAE_sp_3d, MAE_dp_3d
  END INTERFACE MAE

  ! ------------------------------------------------------------------

  !>        \brief Mean squared error.

  !>        \details Calculates the mean squared error
  !!
  !!        \f[ MSE = \sum_i\frac{(y_i - x_i)^2}{N_\text{mask}} \f]
  !!
  !!        If an optional mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!        x and y can be single or double precision. The result will have the same numerical precision.
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = MSE(vec1, vec2, mask=(vec >= 0.))
  !!        --> m = 0.0
  !!        \endcode
  !!
  !!        See also example in test directory.

  !>      \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>      \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.
  !>      \retval    "real(sp/dp) :: MSE"                     MSE.

  !>     \note
  !!     Input values must be floating points.

  !>        \authors Matthias Zink
  !>        \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE MSE
    MODULE PROCEDURE MSE_sp_1d, MSE_dp_1d, MSE_sp_2d, MSE_dp_2d, MSE_sp_3d, MSE_dp_3d
  END INTERFACE MSE

  ! ------------------------------------------------------------------

  !>        \brief Nash Sutcliffe Efficiency.

  !>        \details Calculates the Nash Sutcliffe Efficiency
  !!
  !!        \f[NSE = \frac{\sum_i(y_i - x_i)^2}  {\sum_i (x_i - \bar x)^2 }\f]
  !!
  !!        where \f$ x\f$ is the observation and \f$ y\f$ is the modelled data.
  !!
  !!        If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!        \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = NSE(vec1, vec2, mask=(vec >= 0.))
  !!        --> m = 1.0
  !!        \endcode
  !!
  !!        See also example in test directory.
  !!
  !!        \b Literature
  !!
  !!        1. Nash, J., & Sutcliffe, J. (1970). _River flow forecasting through conceptual models part I: A discussion of
  !!           principles_. Journal of Hydrology, 10(3), 282-290. doi:10.1016/0022-1694(70)90255-6
  !!
  !>      \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>      \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.
  !>      \retval    "real(sp/dp) :: NSE"                     NSE.

  !>     \note
  !!     Input values must be floating points.

  !>        \authors Matthias Zink
  !>        \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE NSE
    MODULE PROCEDURE NSE_sp_1d, NSE_dp_1d, NSE_dp_2d, NSE_sp_2d, NSE_sp_3d, NSE_dp_3d
  END INTERFACE NSE

  ! ------------------------------------------------------------------

  !>     \brief  Sum of absolute errors.

  !>     \details Calculates the sum of absolute errors
  !!
  !!     \f[ SAE = \sum_i|y_i - x_i| \f]
  !!
  !!     If an optional mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!     \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!     \b Example
  !!
  !!     \code{.f90}
  !!     vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!     vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!     m = NSE(vec1, vec2, mask=(vec >= 0.))
  !!     --> m = 0.0
  !!     \endcode
  !!
  !!     See also example in test directory.

  !>     \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>     \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!         If present, only those locations in vec corresponding to the true values in mask are used.

  !>     \returns    "real(sp/dp) :: NSE"                     NSE.

  !>     \note
  !!     Input values must be floating points.

  !>     \authors Matthias Zink
  !>     \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE SAE
    MODULE PROCEDURE SAE_sp_1d, SAE_dp_1d, SAE_sp_2d, SAE_dp_2d, SAE_sp_3d, SAE_dp_3d
  END INTERFACE SAE

  ! ------------------------------------------------------------------

  !>    \brief Sum of squared errors

  !>    \details Calculates the sum of squared errors
  !!
  !!    \f[ SSE = \sum_i(y_i - x_i)^2 \f]
  !!
  !!    If an optional mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!    \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    m   = SSE(vec1, vec2, mask=(vec >= 0.))
  !!    --> m = 0.0
  !!    \endcode
  !!
  !!    See also example in test directory.

  !>    \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>    \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!    If present, only those locations in vec corresponding to the true values in mask are used.
  !>    \retval    "real(sp/dp) :: SSE"                     SSE.

  !>    \note
  !!    Input values must be floating points.

  !>    \authors Matthias Zink
  !>    \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE SSE
    MODULE PROCEDURE SSE_sp_1d, SSE_dp_1d, SSE_sp_2d, SSE_dp_2d, SSE_sp_3d, SSE_dp_3d
  END INTERFACE SSE

  ! ------------------------------------------------------------------

  !>    \brief RMS Error.

  !>    \details Calculates the root-mean-square error
  !!
  !!    \f[ RMSE = \sqrt{\frac{\sum_i{(y_i - x_i)^2}} {{N_\text{count}}}} \f]
  !!
  !!    If an optional mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!    \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    m   = RMSE(vec1, vec2, mask=(vec >= 0.))
  !!    --> m = 0.0
  !!    \endcode
  !!
  !!    See also example in test directory.

  !>    \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>    \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!    If present, only those locations in vec corresponding to the true values in mask are used.
  !>    \retval    "real(sp/dp) :: RMSE"                     RMSE.

  !>    \note
  !!    Input values must be floating points.

  !>    \authors Matthias Zink
  !>    \date Sept 2012

  ! ------------------------------------------------------------------

  INTERFACE RMSE
    MODULE PROCEDURE RMSE_sp_1d, RMSE_dp_1d, RMSE_sp_2d, RMSE_dp_2d, RMSE_sp_3d, RMSE_dp_3d
  END INTERFACE RMSE

  ! ------------------------------------------------------------------

  !>    \brief weighted Nash Sutcliffe Efficiency.

  !>    Calculates the weighted Nash Sutcliffe Efficiency
  !!
  !!    \f[ wNSE = \frac{\sum_i {x_i (y_i - x_i)^2}} {\sum_i{ x_i (x_i - \bar x)^2}} \f]
  !!
  !!    where \f$ x\f$ is the observation and \f$ y\f$ is the modelled data.
  !!    This objective function is introduced in Hundecha and Bardossy, 2004.
  !!
  !!    If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!    \f$ x\f$ and \f$ y\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    vec2 = (/ 1., 2, 3., -999., 5., 6. /)
  !!    m   = wNSE(vec1, vec2, mask=(vec >= 0.))
  !!    --> m = 1.0
  !!    \endcode
  !!
  !!    See also example in test directory.
  !!
  !!    \b Literature
  !!
  !!    1.   Nash, J., & Sutcliffe, J. (1970). _River flow forecasting through conceptual models part I: A discussion of
  !!                principles_. Journal of Hydrology, 10(3), 282-290. doi:10.1016/0022-1694(70)90255-6\n
  !!    2.   Hundecha and Bardossy (2004). _Modeling of the effect of land use changes on the runoff generation of a river
  !!                domain through parameter regionalization of a watershed model_. Journal of Hydrology, 292, 281-295
  !!
  !>    \param[in]  "real(sp/dp), dimension()     :: x, y"    1D/2D/3D-array with input numbers.
  !>    \param[in]  "logical, optional     :: mask"         1D/2D/Array-array of logical values with `size(x/y)`.
  !!    If present, only those locations in vec corresponding to the true values in mask are used.
  !>    \retval    "real(sp/dp) :: wNSE"                     wNSE.

  !>    \note
  !!    Input values must be floating points.

  !>    \authors Matthias Zink & Bjoern Guse
  !>    \date May 2018

  ! ------------------------------------------------------------------

  INTERFACE wNSE
     MODULE PROCEDURE wNSE_sp_1d, wNSE_dp_1d, wNSE_dp_2d, wNSE_sp_2d, wNSE_sp_3d, wNSE_dp_3d
  END INTERFACE wNSE

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  FUNCTION BIAS_sp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: BIAS_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    !
    if (n .LE. 1_i4) stop 'BIAS_sp_1d: number of arguments must be at least 2'
    !
    BIAS_sp_1d = average(y, mask = maske) - average(x, mask = maske)

  END FUNCTION BIAS_sp_1d

  FUNCTION BIAS_dp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: BIAS_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'BIAS_dp_1d: number of arguments must be at least 2'
    !
    BIAS_dp_1d = average(y, mask = maske) - average(x, mask = maske)

  END FUNCTION BIAS_dp_1d

  FUNCTION BIAS_sp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: BIAS_sp_2d

    INTEGER(i4) :: n

    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    !
    if (n .LE. 1_i4) stop 'BIAS_sp_2d: number of arguments must be at least 2'
    !
    BIAS_sp_2d = average(reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(y, dim = 1) * size(y, dim = 2)/))) - &
            average(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
                    mask = reshape(maske, (/size(x, dim = 1) * size(x, dim = 2)/)))
    !
  END FUNCTION BIAS_sp_2d

  FUNCTION BIAS_dp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: BIAS_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    !
    if (n .LE. 1_i4) stop 'BIAS_dp_2d: number of arguments must be at least 2'
    !
    BIAS_dp_2d = average(reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(y, dim = 1) * size(y, dim = 2)/))) - &
            average(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
                    mask = reshape(maske, (/size(x, dim = 1) * size(x, dim = 2)/)))
    !
  END FUNCTION BIAS_dp_2d

  FUNCTION BIAS_sp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: BIAS_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), &
            size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    !
    ! not really sopisticated, it has to be checked if the 3 numbers of x and y are matching in arry position
    if (n .LE. 1_i4) stop 'BIAS_sp_3d: number of arguments must be at least 2'
    !
    BIAS_sp_3d = average(reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske, (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/))) - &
            average(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
                    mask = reshape(maske, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    !
  END FUNCTION BIAS_sp_3d

  FUNCTION BIAS_dp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: BIAS_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), &
            size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'BIAS_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    !
    ! not really sopisticated, it has to be checked if the 3 numbers of x and y are matching in arry position
    if (n .LE. 1_i4) stop 'BIAS_dp_3d: number of arguments must be at least 2'
    !
    BIAS_dp_3d = average(reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske, (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/))) - &
            average(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
                    mask = reshape(maske, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    !
  END FUNCTION BIAS_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION KGE_sp_1d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGE_sp_1d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(sp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_sp_1d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(x, mask = maske)
    mu_Sim = average(y, mask = maske)
    ! Standard Deviation
    sigma_Obs = stddev(x, mask = maske)
    sigma_Sim = stddev(y, mask = maske)
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(x, y, mask = maske) * real(n, sp) / real(n - 1, sp)
    !
    KGE_sp_1d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_sp - pearson_coor)**2             &
            )

  END FUNCTION KGE_sp_1d

  FUNCTION KGE_sp_2d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGE_sp_2d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske
    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(sp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_sp_2d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    mu_Sim = average(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/))) * &
            real(n, sp) / real(n - 1, sp)
    !
    KGE_sp_2d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_sp - pearson_coor)**2             &
            )

  END FUNCTION KGE_sp_2d

  FUNCTION KGE_sp_3d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGE_sp_3d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske
    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(sp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_sp_3d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    mu_Sim = average(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/))) * &
            real(n, sp) / real(n - 1, sp)
    !
    KGE_sp_3d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_sp - pearson_coor)**2             &
            )

  END FUNCTION KGE_sp_3d

  FUNCTION KGE_dp_1d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGE_dp_1d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(dp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_dp_1d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(x, mask = maske)
    mu_Sim = average(y, mask = maske)
    ! Standard Deviation
    sigma_Obs = stddev(x, mask = maske)
    sigma_Sim = stddev(y, mask = maske)
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(x, y, mask = maske) * real(n, dp) / real(n - 1, dp)
    !
    KGE_dp_1d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_dp - pearson_coor)**2             &
            )

  END FUNCTION KGE_dp_1d

  FUNCTION KGE_dp_2d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGE_dp_2d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske
    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(dp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_dp_2d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    mu_Sim = average(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/))) * &
            real(n, dp) / real(n - 1, dp)
    !
    KGE_dp_2d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_dp - pearson_coor)**2             &
            )

  END FUNCTION KGE_dp_2d

  FUNCTION KGE_dp_3d(x, y, mask)

    USE mo_moment, ONLY : average, stddev, correlation

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGE_dp_3d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske
    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y
    REAL(dp) :: pearson_coor         ! Pearson Corr. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGE_dp_3d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    mu_Sim = average(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Pearson product-moment correlation coefficient is with (N-1) not N
    pearson_coor = correlation(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/))) * &
            real(n, dp) / real(n - 1, dp)
    !
    KGE_dp_3d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 + &
                    (1.0_dp - pearson_coor)**2             &
            )

  END FUNCTION KGE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION KGEnocorr_sp_1d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGEnocorr_sp_1d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_sp_1d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(x, mask = maske)
    mu_Sim = average(y, mask = maske)
    ! Standard Deviation
    sigma_Obs = stddev(x, mask = maske)
    sigma_Sim = stddev(y, mask = maske)

    !
    KGEnocorr_sp_1d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_sp_1d

  FUNCTION KGEnocorr_sp_2d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGEnocorr_sp_2d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske
    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_sp_2d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    mu_Sim = average(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    !
    KGEnocorr_sp_2d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_sp_2d

  FUNCTION KGEnocorr_sp_3d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: KGEnocorr_sp_3d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske
    REAL(sp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(sp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_sp_3d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    mu_Sim = average(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))

    !
    KGEnocorr_sp_3d = 1.0 - SQRT(&
            (1.0_sp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_sp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_sp_3d

  FUNCTION KGEnocorr_dp_1d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGEnocorr_dp_1d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_dp_1d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(x, mask = maske)
    mu_Sim = average(y, mask = maske)
    ! Standard Deviation
    sigma_Obs = stddev(x, mask = maske)
    sigma_Sim = stddev(y, mask = maske)

    !
    KGEnocorr_dp_1d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_dp_1d

  FUNCTION KGEnocorr_dp_2d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGEnocorr_dp_2d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske
    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_dp_2d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    mu_Sim = average(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(y, dim = 1) * size(y, dim = 2)/)))
    !
    KGEnocorr_dp_2d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_dp_2d

  FUNCTION KGEnocorr_dp_3d(x, y, mask)

    USE mo_moment, ONLY : average, stddev

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: KGEnocorr_dp_3d

    ! local variables
    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske
    REAL(dp) :: mu_Obs, mu_Sim       ! Mean          of x and y
    REAL(dp) :: sigma_Obs, sigma_Sim ! Standard dev. of x and y

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'KGEnocorr_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'KGEnocorr_dp_3d: sample size must be at least 2'

    ! Mean
    mu_Obs = average(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    mu_Sim = average(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))
    ! Standard Deviation
    sigma_Obs = stddev(&
            reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    sigma_Sim = stddev(&
            reshape(y(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(y, dim = 1) * size(y, dim = 2) * size(y, dim = 3)/)))

    !
    KGEnocorr_dp_3d = 1.0 - SQRT(&
            (1.0_dp - (mu_Sim / mu_Obs))**2 + &
                    (1.0_dp - (sigma_Sim / sigma_Obs))**2 &
            )

  END FUNCTION KGEnocorr_dp_3d


  ! ------------------------------------------------------------------

  FUNCTION LNNSE_sp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(INOUT) :: mask
    REAL(sp) :: LNNSE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    REAL(sp), DIMENSION(size(x)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_sp) .or. y .lt. tiny(1.0_sp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_sp_1d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_sp
    logy = 0.0_sp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(logx, mask = maske)

    ! NSE
    v1 = merge(logy - logx, 0.0_sp, maske)
    v2 = merge(logx - xmean, 0.0_sp, maske)
    LNNSE_sp_1d = 1.0_sp - dot_product(v1, v1) / dot_product(v2, v2)

  END FUNCTION LNNSE_sp_1d

  ! ------------------------------------------------------------------

  FUNCTION LNNSE_dp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(INOUT) :: mask
    REAL(dp) :: LNNSE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    REAL(dp), DIMENSION(size(x)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_dp) .or. y .lt. tiny(1.0_dp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_dp_1d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_dp
    logy = 0.0_dp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(logx, mask = maske)

    ! NSE
    v1 = merge(logy - logx, 0.0_dp, maske)
    v2 = merge(logx - xmean, 0.0_dp, maske)
    LNNSE_dp_1d = 1.0_dp - sum(v1 * v1, mask = maske) / sum(v2 * v2, mask = maske)

  END FUNCTION LNNSE_dp_1d

  ! ------------------------------------------------------------------

  FUNCTION LNNSE_sp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(INOUT) :: mask
    REAL(sp) :: LNNSE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    REAL(sp), DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_sp) .or. y .lt. tiny(1.0_sp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_sp_2d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_sp
    logy = 0.0_sp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(pack(logx, maske))

    ! NSE
    v1 = merge(logy - logx, 0.0_sp, maske)
    v2 = merge(logx - xmean, 0.0_sp, maske)
    LNNSE_sp_2d = 1.0_sp - sum(v1 * v1, mask = maske) / sum(v2 * v2, mask = maske)

  END FUNCTION LNNSE_sp_2d

  ! ------------------------------------------------------------------

  FUNCTION LNNSE_dp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(INOUT) :: mask
    REAL(dp) :: LNNSE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    REAL(dp), DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_dp) .or. y .lt. tiny(1.0_dp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_dp_2d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_dp
    logy = 0.0_dp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(pack(logx, maske))

    ! NSE
    v1 = merge(logy - logx, 0.0_dp, maske)
    v2 = merge(logx - xmean, 0.0_dp, maske)
    LNNSE_dp_2d = 1.0_dp - sum(v1 * v1, mask = maske) / sum(v2 * v2, mask = maske)

  END FUNCTION LNNSE_dp_2d

  ! ------------------------------------------------------------------

  FUNCTION LNNSE_sp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(INOUT) :: mask
    REAL(sp) :: LNNSE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    REAL(sp), DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_sp) .or. y .lt. tiny(1.0_sp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_sp_3d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_sp
    logy = 0.0_sp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(pack(logx, maske))

    ! NSE
    v1 = merge(logy - logx, 0.0_sp, maske)
    v2 = merge(logx - xmean, 0.0_sp, maske)
    LNNSE_sp_3d = 1.0_sp - sum(v1 * v1, mask = maske) / sum(v2 * v2, mask = maske)

  END FUNCTION LNNSE_sp_3d

  ! ------------------------------------------------------------------

  FUNCTION LNNSE_dp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(INOUT) :: mask
    REAL(dp) :: LNNSE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    REAL(dp), DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: logx, logy, v1, v2
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'LNNSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
    else
      maske = .true.
    end if

    ! mask all negative and zero entries
    where (x .lt. tiny(1.0_dp) .or. y .lt. tiny(1.0_dp))
      maske = .false.
    end where
    n = count(maske)
    if (n .LE. 1_i4) stop 'LNNSE_dp_3d: number of arguments must be at least 2'

    ! logarithms
    logx = 0.0_dp
    logy = 0.0_dp
    where (maske)
      logx = log(x)
      logy = log(y)
    end where

    ! mean of x
    xmean = average(pack(logx, maske))

    ! NSE
    v1 = merge(logy - logx, 0.0_dp, maske)
    v2 = merge(logx - xmean, 0.0_dp, maske)
    LNNSE_dp_3d = 1.0_dp - sum(v1 * v1, mask = maske) / sum(v2 * v2, mask = maske)

  END FUNCTION LNNSE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION MAE_sp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MAE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'MAE_sp_1d: number of arguments must be at least 2'
    !
    MAE_sp_1d = SAE_sp_1d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MAE_sp_1d

  FUNCTION MAE_dp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MAE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'MAE_dp_1d: number of arguments must be at least 2'
    !
    MAE_dp_1d = SAE_dp_1d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MAE_dp_1d

  FUNCTION MAE_sp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MAE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'MAE_sp_2d: number of arguments must be at least 2'
    !
    MAE_sp_2d = SAE_sp_2d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MAE_sp_2d

  FUNCTION MAE_dp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MAE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'MAE_dp_2d: number of arguments must be at least 2'
    !
    MAE_dp_2d = SAE_dp_2d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MAE_dp_2d

  FUNCTION MAE_sp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MAE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'MAE_sp_3d: number of arguments must be at least 2'
    !
    MAE_sp_3d = SAE_sp_3d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MAE_sp_3d

  FUNCTION MAE_dp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MAE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MAE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'MAE_dp_3d: number of arguments must be at least 2'
    !
    MAE_dp_3d = SAE_dp_3d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MAE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION MSE_sp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MSE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'MSE_sp_1d: number of arguments must be at least 2'
    !
    MSE_sp_1d = SSE_sp_1d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MSE_sp_1d

  FUNCTION MSE_dp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MSE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'MSE_dp_1d: number of arguments must be at least 2'
    !
    MSE_dp_1d = SSE_dp_1d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MSE_dp_1d

  FUNCTION MSE_sp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MSE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'MSE_sp_2d: number of arguments must be at least 2'
    !
    MSE_sp_2d = SSE_sp_2d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MSE_sp_2d

  FUNCTION MSE_dp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MSE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'MSE_dp_2d: number of arguments must be at least 2'
    !
    MSE_dp_2d = SSE_dp_2d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MSE_dp_2d

  FUNCTION MSE_sp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: MSE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'MSE_sp_3d: number of arguments must be at least 2'
    !
    MSE_sp_3d = SSE_sp_3d(x, y, mask = maske) / real(n, sp)

  END FUNCTION MSE_sp_3d

  FUNCTION MSE_dp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: MSE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'MSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'MSE_dp_3d: number of arguments must be at least 2'
    !
    MSE_dp_3d = SSE_dp_3d(x, y, mask = maske) / real(n, dp)

  END FUNCTION MSE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION NSE_sp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: NSE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    REAL(sp), DIMENSION(size(x)) :: v1, v2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'NSE_sp_1d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(x, mask = maske)
    !
    v1 = merge(y - x, 0.0_sp, maske)
    v2 = merge(x - xmean, 0.0_sp, maske)
    !
    NSE_sp_1d = 1.0_sp - dot_product(v1, v1) / dot_product(v2, v2)

  END FUNCTION NSE_sp_1d

  FUNCTION NSE_dp_1d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: NSE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    REAL(dp), DIMENSION(size(x)) :: v1, v2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'NSE_dp_1d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(x, mask = maske)
    !
    v1 = merge(y - x, 0.0_dp, maske)
    v2 = merge(x - xmean, 0.0_dp, maske)
    !
    NSE_dp_1d = 1.0_dp - dot_product(v1, v1) / dot_product(v2, v2)

  END FUNCTION NSE_dp_1d

  FUNCTION NSE_sp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: NSE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    !
    if (n .LE. 1_i4) stop 'NSE_sp_2d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    !
    NSE_sp_2d = 1.0_sp - sum((y - x) * (y - x), mask = maske) / sum((x - xmean) * (x - xmean), mask = maske)
    !
  END FUNCTION NSE_sp_2d

  FUNCTION NSE_dp_2d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: NSE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    !
    if (n .LE. 1_i4) stop 'NSE_dp_2d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)), &
            mask = reshape(maske(:, :), (/size(x, dim = 1) * size(x, dim = 2)/)))
    !
    NSE_dp_2d = 1.0_dp - sum((y - x) * (y - x), mask = maske) / sum((x - xmean) * (x - xmean), mask = maske)
    !
  END FUNCTION NSE_dp_2d

  FUNCTION NSE_sp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: NSE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp) :: xmean
    LOGICAL, DIMENSION(size(x, dim = 1), &
            size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    !
    if (n .LE. 1_i4) stop 'NSE_sp_3d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    !
    NSE_sp_3d = 1.0_sp - sum((y - x) * (y - x), mask = maske) / sum((x - xmean) * (x - xmean), mask = maske)
    !
  END FUNCTION NSE_sp_3d

  FUNCTION NSE_dp_3d(x, y, mask)

    USE mo_moment, ONLY : average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: NSE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(dp) :: xmean
    LOGICAL, DIMENSION(size(x, dim = 1), &
            size(x, dim = 2), size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'NSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    !
    if (n .LE. 1_i4) stop 'NSE_dp_3d: number of arguments must be at least 2'
    ! Average of x
    xmean = average(reshape(x(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske(:, :, :), (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)))
    !
    NSE_dp_3d = 1.0_dp - sum((y - x) * (y - x), mask = maske) / sum((x - xmean) * (x - xmean), mask = maske)
    !
  END FUNCTION NSE_dp_3d


  ! ------------------------------------------------------------------

  FUNCTION SAE_sp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SAE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SAE_sp_1d: number of arguments must be at least 2'
    !
    SAE_sp_1d = sum(abs(y - x), mask = maske)

  END FUNCTION SAE_sp_1d

  FUNCTION SAE_dp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SAE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SAE_dp_1d: number of arguments must be at least 2'
    !
    SAE_dp_1d = sum(abs(y - x), mask = maske)

  END FUNCTION SAE_dp_1d

  FUNCTION SAE_sp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SAE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'SAE_sp_2d: number of arguments must be at least 2'
    !
    SAE_sp_2d = SAE_sp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)/)))

  END FUNCTION SAE_sp_2d

  FUNCTION SAE_dp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SAE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'SAE_dp_2d: number of arguments must be at least 2'
    !
    SAE_dp_2d = SAE_dp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)/)))

  END FUNCTION SAE_dp_2d

  FUNCTION SAE_sp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SAE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'SAE_sp_3d: number of arguments must be at least 2'
    !
    SAE_sp_3d = SAE_sp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2) &
                    * size(maske, dim = 3)/)))

  END FUNCTION SAE_sp_3d

  FUNCTION SAE_dp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SAE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SAE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'SAE_dp_3d: number of arguments must be at least 2'
    !
    SAE_dp_3d = SAE_dp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2) &
                    * size(maske, dim = 3)/)))

  END FUNCTION SAE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION SSE_sp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SSE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    !
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_sp_1d: number of arguments must be at least 2'
    !
    SSE_sp_1d = sum((y - x)**2_i4, mask = maske)

  END FUNCTION SSE_sp_1d

  FUNCTION SSE_dp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SSE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_dp_1d: number of arguments must be at least 2'
    !
    SSE_dp_1d = sum((y - x)**2_i4, mask = maske)

  END FUNCTION SSE_dp_1d

  FUNCTION SSE_sp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SSE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_sp_2d: number of arguments must be at least 2'
    !
    SSE_sp_2d = SSE_sp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)/)))

  END FUNCTION SSE_sp_2d

  FUNCTION SSE_dp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SSE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_dp_2d: number of arguments must be at least 2'
    !
    SSE_dp_2d = SSE_dp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)/)))

  END FUNCTION SSE_dp_2d

  FUNCTION SSE_sp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: SSE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_sp_3d: number of arguments must be at least 2'
    !
    SSE_sp_3d = SSE_sp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)   &
                    * size(maske, dim = 3)/)))

  END FUNCTION SSE_sp_3d

  FUNCTION SSE_dp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: SSE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'SSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x)
    end if
    if (n .LE. 1_i4) stop 'SSE_dp_3d: number of arguments must be at least 2'
    !
    SSE_dp_3d = SSE_dp_1d(reshape(x, (/size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)/)), &
            reshape(y, (/size(y, dim = 1) * size(y, dim = 2) * size(x, dim = 3)/)), &
            mask = reshape(maske, (/size(maske, dim = 1) * size(maske, dim = 2)   &
                    * size(maske, dim = 3)/)))

  END FUNCTION SSE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION RMSE_sp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: RMSE_sp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'RMSE_sp_1d: number of arguments must be at least 2'
    !
    RMSE_sp_1d = sqrt(MSE_sp_1d(x, y, mask = maske))

  END FUNCTION RMSE_sp_1d

  FUNCTION RMSE_dp_1d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: RMSE_dp_1d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1)
    end if
    if (n .LE. 1_i4) stop 'RMSE_dp_1d: number of arguments must be at least 2'
    !
    RMSE_dp_1d = sqrt(MSE_dp_1d(x, y, mask = maske))

  END FUNCTION RMSE_dp_1d

  FUNCTION RMSE_sp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: RMSE_sp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'RMSE_sp_2d: number of arguments must be at least 2'
    !
    RMSE_sp_2d = sqrt(MSE_sp_2d(x, y, mask = maske))

  END FUNCTION RMSE_sp_2d

  FUNCTION RMSE_dp_2d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: RMSE_dp_2d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2)
    end if
    if (n .LE. 1_i4) stop 'RMSE_dp_2d: number of arguments must be at least 2'
    !
    RMSE_dp_2d = sqrt(MSE_dp_2d(x, y, mask = maske))

  END FUNCTION RMSE_dp_2d

  FUNCTION RMSE_sp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: RMSE_sp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'RMSE_sp_3d: number of arguments must be at least 2'
    !
    RMSE_sp_3d = sqrt(MSE_sp_3d(x, y, mask = maske))

  END FUNCTION RMSE_sp_3d

  FUNCTION RMSE_dp_3d(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:, :, :), INTENT(IN) :: x, y
    LOGICAL, DIMENSION(:, :, :), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: RMSE_dp_3d

    INTEGER(i4) :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    LOGICAL, DIMENSION(size(x, dim = 1), size(x, dim = 2), &
            size(x, dim = 3)) :: maske

    if (present(mask)) then
      shapemask = shape(mask)
    else
      shapemask = shape(x)
    end if
    if ((any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask))) &
            stop 'RMSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
      maske = mask
      n = count(maske)
    else
      maske = .true.
      n = size(x, dim = 1) * size(x, dim = 2) * size(x, dim = 3)
    end if
    if (n .LE. 1_i4) stop 'RMSE_dp_3d: number of arguments must be at least 2'
    !
    RMSE_dp_3d = sqrt(MSE_dp_3d(x, y, mask = maske))

  END FUNCTION RMSE_dp_3d

  ! ------------------------------------------------------------------

  FUNCTION wNSE_sp_1d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:),           INTENT(IN)      :: x, y
    LOGICAL,  DIMENSION(:), OPTIONAL, INTENT(IN)      :: mask
    REAL(sp)                                          :: wNSE_sp_1d

    INTEGER(i4)                            :: n
    INTEGER(i4), DIMENSION(size(shape(x))) :: shapemask
    REAL(sp)                               :: xmean
    REAL(sp), DIMENSION(size(x))           :: v1, v2, ww
    LOGICAL,  DIMENSION(size(x))           :: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_sp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n = count(maske)
    else
       maske = .true.
       n = size(x)
    end if
    if (n .LE. 1_i4) stop 'wNSE_sp_1d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(x, mask=maske)
    !
    v1 = merge(y - x    , 0.0_sp, maske)
    v2 = merge(x - xmean, 0.0_sp, maske)
    ww = merge(x        , 0.0_sp, maske)
    !
    wNSE_sp_1d = 1.0_sp - dot_product(ww * v1,v1) / dot_product(ww * v2,v2)

  END FUNCTION wNSE_sp_1d

  FUNCTION wNSE_dp_1d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:),           INTENT(IN)      :: x, y
    LOGICAL,  DIMENSION(:), OPTIONAL, INTENT(IN)      :: mask
    REAL(dp)                                          :: wNSE_dp_1d

    INTEGER(i4)                                       :: n
    INTEGER(i4), DIMENSION(size(shape(x)) )           :: shapemask
    REAL(dp)                                          :: xmean
    REAL(dp), DIMENSION(size(x))                      :: v1, v2, ww
    LOGICAL,  DIMENSION(size(x))                      :: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_dp_1d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n = count(maske)
    else
       maske = .true.
       n = size(x)
    end if
    if (n .LE. 1_i4) stop 'wNSE_dp_1d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(x, mask=maske)
    !
    v1 = merge(y - x    , 0.0_dp, maske)
    v2 = merge(x - xmean, 0.0_dp, maske)
    ww = merge(x        , 0.0_dp, maske)
    !
    wNSE_dp_1d = 1.0_dp - dot_product(ww * v1,v1) / dot_product(ww * v2,v2)

  END FUNCTION wNSE_dp_1d

  FUNCTION wNSE_sp_2d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:,:),           INTENT(IN)    :: x, y
    LOGICAL,  DIMENSION(:,:), OPTIONAL, INTENT(IN)    :: mask
    REAL(sp)                                          :: wNSE_sp_2d

    INTEGER(i4)                                       :: n
    INTEGER(i4), DIMENSION(size(shape(x)) )           :: shapemask
    REAL(sp)                                          :: xmean
    LOGICAL, DIMENSION(size(x, dim=1), size(x, dim=2)):: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_sp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n     = count(maske)
    else
       maske = .true.
       n     = size(x, dim=1) * size(x, dim=2)
    end if
    !
    if (n .LE. 1_i4) stop 'wNSE_sp_2d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:,:), (/size(x, dim=1)*size(x, dim=2)/)), &
         mask=reshape(maske(:,:), (/size(x, dim=1)*size(x, dim=2)/)))
    !
    wNSE_sp_2d = 1.0_sp - sum(x * (y-x)*(y-x), mask=maske) / sum(x * (x-xmean)*(x-xmean), mask=maske)
    !
  END FUNCTION wNSE_sp_2d

  FUNCTION wNSE_dp_2d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:,:),           INTENT(IN)    :: x, y
    LOGICAL,  DIMENSION(:,:), OPTIONAL, INTENT(IN)    :: mask
    REAL(dp)                                          :: wNSE_dp_2d

    INTEGER(i4)                                       :: n
    INTEGER(i4), DIMENSION(size(shape(x)) )           :: shapemask
    REAL(dp)                                          :: xmean
    LOGICAL, DIMENSION(size(x, dim=1), size(x, dim=2)):: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_dp_2d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n     = count(maske)
    else
       maske = .true.
       n     = size(x, dim=1) * size(x, dim=2)
    end if
    !
    if (n .LE. 1_i4) stop 'wNSE_dp_2d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:,:), (/size(x, dim=1)*size(x, dim=2)/)), &
         mask=reshape(maske(:,:), (/size(x, dim=1)*size(x, dim=2)/)))
    !
    wNSE_dp_2d = 1.0_dp - sum(x * (y-x)*(y-x), mask=maske) / sum(x * (x-xmean)*(x-xmean), mask=maske)
    !
  END FUNCTION wNSE_dp_2d

  FUNCTION wNSE_sp_3d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(sp), DIMENSION(:,:,:),           INTENT(IN)  :: x, y
    LOGICAL,  DIMENSION(:,:,:), OPTIONAL, INTENT(IN)  :: mask
    REAL(sp)                                          :: wNSE_sp_3d

    INTEGER(i4)                                       :: n
    INTEGER(i4), DIMENSION(size(shape(x)) )           :: shapemask
    REAL(sp)                                          :: xmean
    LOGICAL,  DIMENSION(size(x, dim=1), &
         size(x, dim=2), size(x, dim=3))    :: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_sp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n     = count(maske)
    else
       maske = .true.
       n     = size(x, dim=1) * size(x, dim=2) * size(x, dim=3)
    end if
    !
    if (n .LE. 1_i4) stop 'wNSE_sp_3d: number of arguments must be at least 2'
    ! mean of x
    xmean = average(reshape(x(:,:,:), (/size(x, dim=1)*size(x, dim=2)*size(x, dim=3)/)), &
         mask=reshape(maske(:,:,:), (/size(x, dim=1)*size(x, dim=2)*size(x, dim=3)/)))
    !
    wNSE_sp_3d = 1.0_sp - sum(x * (y-x)*(y-x), mask=maske) / sum(x * (x-xmean)*(x-xmean), mask=maske)
    !
  END FUNCTION wNSE_sp_3d

  FUNCTION wNSE_dp_3d(x, y, mask)

    USE mo_moment, ONLY: average

    IMPLICIT NONE

    REAL(dp), DIMENSION(:,:,:),           INTENT(IN)  :: x, y
    LOGICAL,  DIMENSION(:,:,:), OPTIONAL, INTENT(IN)  :: mask
    REAL(dp)                                          :: wNSE_dp_3d

    INTEGER(i4)                                       :: n
    INTEGER(i4), DIMENSION(size(shape(x)) )           :: shapemask
    REAL(dp)                                          :: xmean
    LOGICAL,  DIMENSION(size(x, dim=1), &
         size(x, dim=2), size(x, dim=3))    :: maske

    if (present(mask)) then
       shapemask = shape(mask)
    else
       shapemask =  shape(x)
    end if
    if ( (any(shape(x) .NE. shape(y))) .OR. (any(shape(x) .NE. shapemask)) ) &
         stop 'wNSE_dp_3d: shapes of inputs(x,y) or mask are not matching'
    !
    if (present(mask)) then
       maske = mask
       n     = count(maske)
    else
       maske = .true.
       n     = size(x, dim=1) * size(x, dim=2) * size(x, dim=3)
    end if
    !
    if (n .LE. 1_i4) stop 'wNSE_dp_3d: number of arguments must be at least 2'
    ! Average of x
    xmean = average(reshape(x(:,:,:), (/size(x, dim=1)*size(x, dim=2)*size(x, dim=3)/)), &
         mask=reshape(maske(:,:,:), (/size(x, dim=1)*size(x, dim=2)*size(x, dim=3)/)))
    !
    wNSE_dp_3d = 1.0_dp - sum(x * (y-x)*(y-x), mask=maske) / sum(x * (x-xmean)*(x-xmean), mask=maske)
    !
  END FUNCTION wNSE_dp_3d

END MODULE mo_errormeasures
