!> \file    mo_moment.f90
!> \brief \copybrief mo_moment
!> \details \copydetails mo_moment

!> \brief   Statistical moments.
!> \details This module contains routines for the masked calculation of
!! statistical properties such as moments and mixed moments of input vectors
!! \note all except variance and standard deviation are population and not sample moments,
!!       i.e. they are normally divided by n and not (n-1)
!> \par Literature
!!   Central moments and error variances
!!       LH Benedict & RD Gould, Towards better uncertainty estimates for turbulence statistics,
!!           Experiments in Fluids 22, 129-136, 1996
!> \changelog
!! - Matthias Cuntz, Nov 2011
!!   - written
!! - Matthias Cuntz, Dec 2011
!!   - mod. correlation, covariance
!! - M. Schroen, Sep 2015
!!   - average/mean for single value
!! - S. Mueller, Dec 2019
!!   - remove citations for common sence
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_moment

  USE mo_kind, ONLY : i4, sp, dp

  IMPLICIT NONE

  PUBLIC :: absdev                        ! Mean absolute deviation from mean of an array
  PUBLIC :: average                       ! 1st moment of an array (same as mean)
  PUBLIC :: central_moment                ! Arbitrary central moment of an array
  PUBLIC :: central_moment_var            ! Central moment error variance
  PUBLIC :: correlation                   ! Correlation between two arrays
  PUBLIC :: covariance                    ! Covariance between two arrays
  PUBLIC :: kurtosis                      ! 4th moment of an array
  PUBLIC :: mean                          ! 1st moment of an array
  PUBLIC :: mixed_central_moment          ! Arbitrary mixed central moment
  PUBLIC :: mixed_central_moment_var      ! Arbitrary mixed central moment error variance
  PUBLIC :: moment                        ! 1st to 4th moments of an array
  PUBLIC :: skewness                      ! 3rd moment of an array
  PUBLIC :: stddev                        ! Sqrt of 2nd moment of an array
  PUBLIC :: variance                      ! 2nd moment of an array

  ! ------------------------------------------------------------------

  !>    \brief Mean absolute deviation from mean.

  !>    \details
  !!    Calculates the mean absolute deviations from the mean
  !!
  !!    \f[ ABSDEV = \sum_i\frac{|x_i-\bar x|}{N} \f]
  !!
  !!    If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!    \f$ x\f$ can be single or double precision. The result will have the same numerical precision.\n
  !!
  !!    \b Example
  !!
  !!        vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = absdev(vec, mask=(vec >= 0.))
  !!
  !!    See also example in pf_tests directory.


  !>    \param[in]  "real(sp/dp) :: dat(:)"               1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with `size(dat)`.
  !!                                                      If present, only those locations in `vec`
  !!                                                      corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: absdev"               Mean absolute deviations from average.

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE absdev
    MODULE PROCEDURE absdev_sp, absdev_dp
  END INTERFACE absdev

  ! ------------------------------------------------------------------

  !>    \brief Mean of vector.

  !>    \details
  !!    Calculates the average value of a vector, i.e. the first moment of a series of numbers:
  !!
  !!    \f[ AVE = \sum_i \frac{x_i}{N} \f]
  !!
  !!    If an optional mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$ x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!        vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = average(vec, mask=(vec >= 0.))
  !!        --> m = 3.4
  !!
  !!    See also example in pf_tests directory.

  !>    \param[in]  "real(sp/dp) :: dat(:)"         1D-array with input numbers

  !>    \retval     "real(sp/dp) :: average"        Average of all elements in dat


  !>    \param[in]  "logical, optional :: mask(:)"  1D-array of logical values with `size(dat)`.
  !!                                                If present, only those locations in dat
  !!                                                corresponding to the true values in mask are used.

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE average
    MODULE PROCEDURE average_sp, average_dp
  END INTERFACE average

  ! ------------------------------------------------------------------

  !>    \brief R-central moment

  !>    \details
  !!    Calculates the central moment of a vector, i.e. the r-central moment of a series of numbers:
  !!
  !!    \f[ \mu_r = \sum_i\frac{(x_i-\bar x)^r}{N} \f]
  !!
  !!    Note that the variance is the second central moment: `variance(x) = central_moment(x,2)`\n
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    x can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!        vec = (/ 1., 2, 3., 4., 5., 6. /)
  !!        m   = central_moment(vec, 2, mask=(vec >= 0.))
  !!        --> m = 2.91666
  !!
  !!    See also example in pf_tests directory.
  !!
  !>    \b Literature
  !!    1.  LH Benedict & RD Gould, _Towards better uncertainty estimates for turbulence statistics_.
  !!        Experiments in Fluids 22, 129-136, 1996
  !!
  !>    \param[in]  "real(sp/dp) :: dat(:)"         1D-array with input numbers.
  !>    \param[in]  "integer(i4) :: r"              Order of the central moment, i.e. r=2 is variance.
  !>    \param[in]  "logical, optional :: mask(:)"  1D-array of logical values with size(dat).
  !!                                                If present, only those locations in `dat`
  !!                                                corresponding to the true values in mask are used.
  !>    \retval "real(sp/dp) :: central_moment"     R-th central moment of elements in `dat`.

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------
  INTERFACE central_moment
    MODULE PROCEDURE central_moment_sp, central_moment_dp
  END INTERFACE central_moment

  ! ------------------------------------------------------------------

  !>    \brief R-central moment variance

  !>    \details
  !!    Calculates the sampling variance of the central moment of a vector.
  !!    `central_moment_var` is something like the "error variance" of the r-th order central moment sampling statistic.\n
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.\n
  !!
  !!    \b Example
  !!
  !!        vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!        m   = central_moment(vec, 2, mask=(vec >= 0.))
  !!        em  = central_moment_var(vec, 2, mask=(vec >= 0.))
  !!
  !!    See also example in pf_tests directory.
  !!
  !!    \b Literature
  !!    1.  LH Benedict & RD Gould, _Towards better uncertainty estimates for turbulence statistics_,
  !!        Experiments in Fluids 22, 129-136, 1996
  !!
  !>    \param[in]  "real(sp/dp) :: dat(:)"                 1D-array with input numbers.
  !>    \param[in]  "integer(i4) :: r"                      Order of the central moment, i.e. r=2 is variance.
  !>    \param[in]  "logical, optional :: mask(:)"          1D-array of logical values with `size(dat)`.
  !!                                                        If present, only those locations in dat
  !!                                                        corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: central_moment_var"     Sampling variance of r-th central moment of elements in dat

  !>    \note  Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  INTERFACE central_moment_var
    MODULE PROCEDURE central_moment_var_sp, central_moment_var_dp
  END INTERFACE central_moment_var

  ! ------------------------------------------------------------------

  !>    \brief Correlation between two vectors.

  !>    \details
  !!    Calculates the correlation between two input vectors, i.e. the covariance divided by the standard deviations:\n
  !!
  !!    \f[\langle x y\rangle = \sum_i\frac{(x_i-\bar x)(y_i-\bar y)}{N\sqrt{\mu_2(x)\mu_2(y)}}\f]
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!        vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!        vec2 = (/ 1.3, 2.7, 3.9, 5.1, 6., 8. /)
  !!        m   = correlation(vec1, vec2, mask=((vec1 >= 0.) .and. (vec2 >= 0.)))
  !!
  !!    See also example in pf_tests directory.

  !>    \param[in]  "real(sp/dp) :: x(:)"           First 1D-array with input numbers.
  !>    \param[in]  "real(sp/dp) :: y(:)"           Second 1D-array with input numbers.
  !>    \retval     "real(sp/dp) :: correlation"    Correlation between \f$x\f$ and \f$y\f$.
  !>    \param[in]  "logical, optional :: mask(:)"  1D-array of logical values with `size(x)`.
  !!                                                If present, only those locations in dat
  !!                                                corresponding to the true values in mask are used.

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  !>    \date Dec 2011
  !!        - covariance as <(x-<x>)(y-<y>)> instead of <xy>-<x><y>

  ! ------------------------------------------------------------------

  INTERFACE correlation
    MODULE PROCEDURE correlation_sp, correlation_dp
  END INTERFACE correlation

  ! ------------------------------------------------------------------

  !>    \brief Covariance between vectors

  !>    \details
  !!    Calculates the covariance between two input vectors:\n
  !!
  !!    \f[Cov(x,y) = \sum_i\frac{(x_i-\bar x)(y_i-\bar y)}{N}\f]
  !!
  !!    If an optional mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!         vec2 = (/ 1.3, 2.7, 3.9, 5.1, 6., 8. /)
  !!         m   = covariance(vec1, vec2, mask=((vec1 >= 0.) .and. (vec2 >= 0.)))
  !!
  !!    See also example in test directory.

  !>    \param[in]  "real(sp/dp) :: x(:)"           First 1D-array with input numbers.
  !>    \param[in]  "real(sp/dp) :: y(:)"           Second 1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"  1D-array of logical values with size(x).
  !!                                                If present, only those locations in dat
  !!                                                corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: covariance"     Covariance between x and y.

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  !>    \date Dec 2011
  !!        - covariance as <(x-<x>)(y-<y>)> instead of <xy>-<x><y>

  ! ------------------------------------------------------------------

  INTERFACE covariance
    MODULE PROCEDURE covariance_sp, covariance_dp
  END INTERFACE covariance

  ! ------------------------------------------------------------------

  !>    \brief Kurtosis of a vector.

  !>    \details
  !!    Calculates the kurtosis of a vector, also called excess kurtosis:
  !!
  !!    \f[ Kurt(x) = \verb|central_moment(x,4) / variance(x)**2 - 3|
  !!    = \sum_i\frac{1}{N}\left(\frac{(x_i-\bar x)^2}{\mu_2(x)}\right)^2 - 3\f]
  !!
  !!    If an optional mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!         m   = kurtosis(vec, mask=(vec >= 0.))
  !!
  !!    See also example in test directory.


  !>    \param[in]  "real(sp/dp) :: dat(:)"         1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"  1D-array of logical values with `size(dat)`.
  !!                                                If present, only those locations in dat
  !!                                                corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: kurtosis"       Kurtosis of all elements in dat.

  !>    \note  Input values must be floating points.

  !>    \authors Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE kurtosis
    MODULE PROCEDURE kurtosis_sp, kurtosis_dp
  END INTERFACE kurtosis

  ! ------------------------------------------------------------------

  !>    \brief Mean of a vector.

  !>    \details
  !!    Calculates the average value of a vector, i.e. the first moment of a series of numbers:
  !!
  !!    \f[\bar x = sum_i \frac{x_i}{N}
  !!
  !!    If an optional mask is given, the mean is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!    \code{.f90}
  !!    vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!    m   = mean(vec, mask=(vec >= 0.))
  !!    \endcode
  !!    See also example in test directory.
  !!
  !>    \param[in]  "real(sp/dp) :: dat(:)"           1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"    1D-array of logical values with `size(dat)`.
  !!                                                  If present, only those locations in dat
  !!                                                  corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: mean"             Average of all elements in dat.

  !>    \note  Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE mean
    MODULE PROCEDURE mean_sp, mean_dp
  END INTERFACE mean

  ! ------------------------------------------------------------------

  !>    \brief R-s mixed central moment between vectors.

  !>    \details
  !!    Calculates the r,s-th mixed central moment between two vectors:
  !!
  !!    \f[\mu_{r-s} = \sum_i\frac{(x_i-\bar{x})^r(y_i-\bar{y})^s}{N}\f]
  !!
  !!    Note that covariace(x,y) = mixed_central_moment(x,y,1,1)
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!         vec2 = (/ 1.3, 2.7, 3.9, 5.1, 6., 8. /)
  !!         m   = mixed_central_moment(vec1, vec2, 2, 2, mask=((vec1 >= 0.) .and. (vec2 >= 0.)))
  !!
  !!    See also example in test directory.
  !!
  !!    \b Literature
  !!
  !!    1. LH Benedict & RD Gould, _Towards better uncertainty estimates for turbulence statistics_,
  !!            Experiments in Fluids 22, 129-136, 1996
  !!
  !>    \param[in]  "real(sp/dp) :: x(:)"                 First 1D-array
  !>    \param[in]  "real(sp/dp) :: y(:)"                 Second 1D-array
  !>    \param[in]  "integer(i4) :: r"                    Order of x
  !>    \param[in]  "integer(i4) :: s"                    Order of y
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with size(x).
  !!                                                      If present, only those locations in dat
  !!                                                      corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: mixed_central_moment" r,s-th mixed central moment between x and y

  !>    \note Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE mixed_central_moment
    MODULE PROCEDURE mixed_central_moment_sp, mixed_central_moment_dp
  END INTERFACE mixed_central_moment

  ! ------------------------------------------------------------------

  !>    \brief Mixed central moment variance.

  !>    \details
  !!    Calculates the sample variance of r,s-th mixed central moment between two vectors.
  !!    `mixed_central_moment_var` is something like the "error variance" of
  !!    the r,s-th order mixed central moment sampling statistic.
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec1 = (/ 1., 2, 3., -999., 5., 6. /)
  !!         vec2 = (/ 1.3, 2.7, 3.9, 5.1, 6., 8. /)
  !!         em   = mixed_central_moment_var(vec1, vec2, 2, 2, mask=((vec1 >= 0.) .and. (vec2 >= 0.)))
  !!
  !!    See also example in test directory.
  !!
  !!    \b Literature
  !!
  !!    1. LH Benedict & RD Gould, _Towards better uncertainty estimates for turbulence statistics_,
  !!            Experiments in Fluids 22, 129-136, 1996
  !!
  !>    \param[in]  "real(sp/dp) :: x(:)"                       First 1D-array
  !>    \param[in]  "real(sp/dp) :: y(:)"                       Second 1D-array
  !>    \param[in]  "integer(i4) :: r"                          Order of x
  !>    \param[in]  "integer(i4) :: s"                          Order of y
  !>    \param[in]  "logical, optional :: mask(:)"              1D-array of logical values with size(x).
  !!                                                            If present, only those locations in dat
  !!                                                            corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: mixed_central_moment_var"   Sampling variance of r,s-th mixed central moment between x and y

  !>    \note  Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  INTERFACE mixed_central_moment_var
    MODULE PROCEDURE mixed_central_moment_var_sp, mixed_central_moment_var_dp
  END INTERFACE mixed_central_moment_var

  ! ------------------------------------------------------------------

  !>    \brief First four moments, stddev and mean absolute devation.

  !>    \details
  !!    Calculates the first four sample/population moments of a vector, i.e. mean, variance, skewness, and kurtosis,
  !!    as well as standard deviation and mean absolute devation.\n
  !!
  !!    All output is optional.
  !!
  !!    If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec = (/ 1., 2, 3., 4., 5., 6. /)
  !!         call moment(vec, average=average, variance=variance, skewness=skewness, kurtosis=kurtosis, &
  !!                    mean=mean, stddev=stddev, absdev=absdev, mask=mask, sample=sample)
  !!         --> average = 3.5

  !>    \param[in]  "real(sp/dp) :: dat(:)"               1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with `size(dat)`.
  !!                                                      If present, only those locations in vec
  !!                                                      corresponding to the true values in mask are used.
  !>    \param[in]  "logical, optional :: sample"         Logical value.
  !!                                                      If present and False, the population variance and
  !!                                                      std-dev will be calculated (divide by n).

  !>    \param[out] "real(sp/dp), optional :: average"    Average of input vector.
  !>    \param[out] "real(sp/dp), optional :: variance"   Sample variance of input vector (either a sample or pupulation moment).
  !>    \param[out] "real(sp/dp), optional :: skewness"   Skewness of input vector.
  !>    \param[out] "real(sp/dp), optional :: kurtosis"   Excess kurtosis of input vector.
  !>    \param[out] "real(sp/dp), optional :: mean"       Same as average.
  !>    \param[out] "real(sp/dp), optional :: stddev"     Sqaure root of variance (either a sample or pupulation moment).
  !>    \param[out] "real(sp/dp), optional :: absdev"     Mean absolute deviations from average.

  !>    \note  Input values must be floating points. Inpt and all optional outputs must have same kind.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  !>    \author Sebastian Mueller
  !>    \date Dec 2019
  !!        -   added optional sample input-para to switch sample to population variance and std-dev.

  ! ------------------------------------------------------------------

  INTERFACE moment
    MODULE PROCEDURE moment_sp, moment_dp
  END INTERFACE moment

  ! ------------------------------------------------------------------

  !>    \brief Skewness of a vector

  !>    \details
  !!    Calculates the skewness of a vector, i.e. the third standardised moment:
  !!
  !!    \f[\tilde \mu_3 = \sum_i\left(\frac{(x-\bar x)}{\sigma_x}\right)^3\f]
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!         m   = skewness(vec, mask=(vec >= 0.))
  !!
  !!    See also example in test directory.


  !>    \param[in]  "real(sp/dp) :: dat(:)"               1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with `size(dat)`.
  !!                                                      If present, only those locations in vec
  !!                                                      corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: skewness"             Skewness of all elements in dat.

  !>    \note   Input values must be floating points.

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  INTERFACE skewness
    MODULE PROCEDURE skewness_sp, skewness_dp
  END INTERFACE skewness

  ! ------------------------------------------------------------------

  !>    \brief  Standard deviation of a vector.

  !>    \details
  !!    Calculates the sample standard deviation of a vector, i.e. the square root of the second moment
  !!
  !!    \f[\sigma_x = \sqrt{\sum_i\frac{(x_i-\bar x)^2}{N-1}}\f]
  !!
  !!    If an optinal mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.
  !!
  !!    \b Example
  !!
  !!         vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!         m   = stddev(vec, mask=(vec >= 0.))
  !!
  !!    See also example in test directory

  !>    \param[in]  "real(sp/dp) :: dat(:)"               1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with `size(dat)`.
  !!                                                      If present, only those locations in vec
  !!                                                      corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: stddev"               Sample standard deviation of all elements in dat.

  !>    \note
  !!    Input values must be floating points.\n
  !!    This is the sample standard deviation. The population standard deviation is:
  !!            `popstddev = stddev * sqrt((n-1)/n)`

  !>    \author Matthias Cuntz
  !>    \date Nov 2011

  ! ------------------------------------------------------------------

  INTERFACE stddev
    MODULE PROCEDURE stddev_sp, stddev_dp
  END INTERFACE stddev

  ! ------------------------------------------------------------------

  !>    \brief  Standard deviation of a vector.

  !>    \details
  !!    Calculates the sample variance of a vector, i.e. the second moment
  !!
  !!    \f[\sigma_x^2 = \sum_i\frac{(x_i-\bar x)^2}{N-1}\f]
  !!
  !!    If an optional mask is given, the average is only over those locations that correspond to true values in the mask.
  !!    \f$x\f$ can be single or double precision. The result will have the same numerical precision.

   !!
  !!    \b Example
  !!
  !!         vec = (/ 1., 2, 3., -999., 5., 6. /)
  !!         m   = variance(vec, mask=(vec >= 0.))
  !!
  !!    See also example in test directory

  !>    \param[in]  "real(sp/dp) :: dat(:)"               1D-array with input numbers.
  !>    \param[in]  "logical, optional :: mask(:)"        1D-array of logical values with `size(dat)`.
  !!                                                      If present, only those locations in vec
  !!                                                      corresponding to the true values in mask are used.
  !>    \retval     "real(sp/dp) :: variance"             Sample variance of all elements in dat.

  !>    \note
  !!    Input values must be floating points.\n
  !!    This is the sample variance. The population variance is:
  !!             `var = variance * (n-1)/n`

  !>    \author Matthias Cuntz
  !>    \date Nov 2011
  INTERFACE variance
    MODULE PROCEDURE variance_sp, variance_dp
  END INTERFACE variance

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  FUNCTION absdev_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: absdev_dp

    REAL(dp) :: n

    REAL(dp) :: ave
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error absdev_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'absdev_dp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    ! Sum of absolute deviation
    absdev_dp = sum(abs(dat(:) - ave), mask = maske) / n

  END FUNCTION absdev_dp


  FUNCTION absdev_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: absdev_sp

    REAL(sp) :: n

    REAL(sp) :: ave
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error absdev_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'absdev_sp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    ! Sum of absolute deviation
    absdev_sp = sum(abs(dat(:) - ave), mask = maske) / n

  END FUNCTION absdev_sp

  ! ------------------------------------------------------------------

  FUNCTION average_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: average_dp

    REAL(dp) :: n
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error average_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if

    ! Average
    average_dp = sum(dat(:), mask = maske) / n

  END FUNCTION average_dp


  FUNCTION average_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: average_sp

    REAL(sp) :: n
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error average_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if

    ! Average
    average_sp = sum(dat(:), mask = maske) / n

  END FUNCTION average_sp

  ! ------------------------------------------------------------------

  FUNCTION central_moment_dp(x, r, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: r
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: central_moment_dp

    REAL(dp) :: n, mx
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r .lt. 0) then
      central_moment_dp = 0.0_dp
      return
    end if
    if (r .eq. 0) then
      central_moment_dp = 1.0_dp
      return
    end if

    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error central_moment_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (2.0_dp + tiny(2.0_dp))) stop 'central_moment_dp: n must be at least 3'

    ! average
    mx = sum(x, mask = maske) / n
    ! central moment
    central_moment_dp = sum((x - mx)**r, mask = maske) / n

  END FUNCTION central_moment_dp


  FUNCTION central_moment_sp(x, r, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: r
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: central_moment_sp

    REAL(sp) :: n, mx
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r .lt. 0) then
      central_moment_sp = 0.0_sp
      return
    end if
    if (r .eq. 0) then
      central_moment_sp = 1.0_sp
      return
    end if

    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error central_moment_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (2.0_sp + tiny(2.0_sp))) stop 'central_moment_sp: n must be at least 3'

    ! average
    mx = sum(x, mask = maske) / n
    ! central moment
    central_moment_sp = sum((x - mx)**r, mask = maske) / n

  END FUNCTION central_moment_sp

  ! ------------------------------------------------------------------

  FUNCTION central_moment_var_dp(x, r, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: r
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: central_moment_var_dp

    REAL(dp) :: n, rr, u2r, ur, urm1, urp1, u2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r.le.1) then
      central_moment_var_dp = 0.0_dp
      return
    end if

    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error central_moment_var_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (2.0_dp + tiny(2.0_dp))) stop 'central_moment_var_dp: n must be at least 3'

    u2r = central_moment(x, 2 * r, mask = maske)
    ur = central_moment(x, r, mask = maske)
    urm1 = central_moment(x, r - 1, mask = maske)
    u2 = central_moment(x, 2, mask = maske)
    urp1 = central_moment(x, r + 1, mask = maske)
    rr = real(r, dp)
    central_moment_var_dp = (u2r - ur * ur + rr * rr * urm1 * urm1 * u2 - 2.0_dp * rr * urp1 * urm1) / n

  END FUNCTION central_moment_var_dp


  FUNCTION central_moment_var_sp(x, r, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    INTEGER(i4), INTENT(IN) :: r
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: central_moment_var_sp

    REAL(sp) :: n, rr, u2r, ur, urm1, urp1, u2
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r.le.1) then
      central_moment_var_sp = 0.0_sp
      return
    end if

    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error central_moment_var_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (2.0_sp + tiny(2.0_sp))) stop 'central_moment_var_sp: n must be at least 3'

    u2r = central_moment(x, 2 * r, mask = maske)
    ur = central_moment(x, r, mask = maske)
    urm1 = central_moment(x, r - 1, mask = maske)
    u2 = central_moment(x, 2, mask = maske)
    urp1 = central_moment(x, r + 1, mask = maske)
    rr = real(r, sp)
    central_moment_var_sp = (u2r - ur * ur + rr * rr * urm1 * urm1 * u2 - 2.0_sp * rr * urp1 * urm1) / n

  END FUNCTION central_moment_var_sp

  ! ------------------------------------------------------------------

  FUNCTION correlation_dp(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), DIMENSION(:), INTENT(IN) :: y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: correlation_dp

    REAL(dp) :: n
    REAL(dp) :: mx, my
    REAL(dp) :: sx, sy, covar
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error correlation_dp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error correlation_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'correlation_dp: n must be at least 2'

    ! Mean and Stddev of x and y
    call moment(x, mx, stddev = sx, mask = maske)
    call moment(y, my, stddev = sy, mask = maske)
    covar = sum((x - mx) * (y - my), mask = maske) / n
    ! correlation
    correlation_dp = covar / (sx * sy)

  END FUNCTION correlation_dp


  FUNCTION correlation_sp(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), DIMENSION(:), INTENT(IN) :: y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: correlation_sp

    REAL(sp) :: n
    REAL(sp) :: mx, my
    REAL(sp) :: sx, sy, covar
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error correlation_sp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error correlation_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'correlation_sp: n must be at least 2'

    ! Mean and Stddev of x and y
    call moment(x, mx, stddev = sx, mask = maske)
    call moment(y, my, stddev = sy, mask = maske)
    covar = sum((x - mx) * (y - my), mask = maske) / n
    ! correlation
    correlation_sp = covar / (sx * sy)

  END FUNCTION correlation_sp

  ! ------------------------------------------------------------------

  FUNCTION covariance_dp(x, y, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), DIMENSION(:), INTENT(IN) :: y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: covariance_dp

    REAL(dp) :: n
    REAL(dp) :: mx, my
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error covariance_dp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error covariance_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'covariance_dp: n must be at least 2'

    ! Mean of x and y
    mx = mean(x, mask = maske)
    my = mean(y, mask = maske)
    covariance_dp = sum((x - mx) * (y - my), mask = maske) / n

  END FUNCTION covariance_dp


  FUNCTION covariance_sp(x, y, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), DIMENSION(:), INTENT(IN) :: y
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: covariance_sp

    REAL(sp) :: n
    REAL(sp) :: mx, my
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error covariance_sp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error covariance_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'covariance_sp: n must be at least 2'

    ! Mean of x and y
    mx = mean(x, mask = maske)
    my = mean(y, mask = maske)
    covariance_sp = sum((x - mx) * (y - my), mask = maske) / n

  END FUNCTION covariance_sp

  ! ------------------------------------------------------------------

  FUNCTION kurtosis_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: kurtosis_dp

    REAL(dp) :: n

    REAL(dp) :: ep, ave, var
    REAL(dp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error kurtosis_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'kurtosis_dp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_dp)
    if (abs(var) .lt. tiny(0.0_dp)) stop 'kurtosis_dp: no kurtosis when zero variance'
    ! Kurtosis
    p(:) = p(:) * s(:) * s(:)
    kurtosis_dp = sum(p(:), mask = maske)
    kurtosis_dp = kurtosis_dp / (n * var * var) - 3.0_dp

  END FUNCTION kurtosis_dp


  FUNCTION kurtosis_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: kurtosis_sp

    REAL(sp) :: n

    REAL(sp) :: ep, ave, var
    REAL(sp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error kurtosis_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'kurtosis_sp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_sp)
    if (abs(var) .lt. tiny(0.0_sp)) stop 'kurtosis_sp: no kurtosis when zero variance'
    ! Kurtosis
    p(:) = p(:) * s(:) * s(:)
    kurtosis_sp = sum(p(:), mask = maske)
    kurtosis_sp = kurtosis_sp / (n * var * var) - 3.0_sp

  END FUNCTION kurtosis_sp

  ! ------------------------------------------------------------------

  FUNCTION mean_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: mean_dp

    REAL(dp) :: n

    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error mean_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if

    ! Mean
    mean_dp = sum(dat(:), mask = maske) / n

  END FUNCTION mean_dp

  !> \copydoc mean
  FUNCTION mean_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: mean_sp

    REAL(sp) :: n

    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error mean_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if

    ! Mean
    mean_sp = sum(dat(:), mask = maske) / n

  END FUNCTION mean_sp

  ! ------------------------------------------------------------------

  FUNCTION mixed_central_moment_dp(x, y, r, s, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), INTENT(IN) :: r
    INTEGER(i4), INTENT(IN) :: s
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: mixed_central_moment_dp

    REAL(dp) :: n, mx, my
    REAL(dp), DIMENSION(size(x)) :: xx, yy
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r.lt.0 .or. s.lt.0) then
      mixed_central_moment_dp = 0.0_dp
      return
    end if

    if (size(x) .ne. size(y)) stop 'Error mixed_central_moment_dp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error mixed_central_moment_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (2.0_dp + tiny(2.0_dp))) stop 'mixed_central_moment_dp: n must be at least 3'

    ! Averages of x and y
    mx = sum(x, mask = maske) / n
    my = sum(y, mask = maske) / n
    ! Mixed central moment
    if (r>0) then
      xx = (x - mx)**r
    else
      xx = 1._dp
    end if
    if (s>0) then
      yy = (y - my)**s
    else
      yy = 1._dp
    end if
    mixed_central_moment_dp = sum(xx * yy, mask = maske) / n

  END FUNCTION mixed_central_moment_dp


  FUNCTION mixed_central_moment_sp(x, y, r, s, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), INTENT(IN) :: r
    INTEGER(i4), INTENT(IN) :: s
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: mixed_central_moment_sp

    REAL(sp) :: n, mx, my
    REAL(sp), DIMENSION(size(x)) :: xx, yy
    LOGICAL, DIMENSION(size(x)) :: maske

    if (r.lt.0 .or. s.lt.0) then
      mixed_central_moment_sp = 0.0_sp
      return
    end if

    if (size(x) .ne. size(y)) stop 'Error mixed_central_moment_sp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error mixed_central_moment_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (2.0_sp + tiny(2.0_sp))) stop 'mixed_central_moment_sp: n must be at least 3'

    ! Averages of x and y
    mx = sum(x, mask = maske) / n
    my = sum(y, mask = maske) / n
    ! Mixed central moment
    if (r>0) then
      xx = (x - mx)**r
    else
      xx = 1._sp
    end if
    if (s>0) then
      yy = (y - my)**s
    else
      yy = 1._sp
    end if
    mixed_central_moment_sp = sum(xx * yy, mask = maske) / n

  END FUNCTION mixed_central_moment_sp

  ! ------------------------------------------------------------------

  FUNCTION mixed_central_moment_var_dp(x, y, r, s, mask)
    ! Error variance of mixed central moment (Benedict & Gould 1996)
    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: x
    REAL(dp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), INTENT(IN) :: r
    INTEGER(i4), INTENT(IN) :: s
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: mixed_central_moment_var_dp

    REAL(dp) :: u2r2s, urs, urm1s, u20, urp1s, ursm1, u02, ursp1, u11
    REAL(dp) :: n, rr, ss
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error mixed_central_moment_var_dp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error mixed_central_moment_var_dp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(x), dp)
    end if
    if (n .le. (2.0_dp + tiny(2.0_dp))) stop 'mixed_central_moment_var_dp: n must be at least 3'

    u2r2s = mixed_central_moment(x, y, 2 * r, 2 * s, mask = maske)
    urs = mixed_central_moment(x, y, r, s, mask = maske)
    urm1s = mixed_central_moment(x, y, r - 1, s, mask = maske)
    u20 = mixed_central_moment(x, y, 2, 0, mask = maske)
    urp1s = mixed_central_moment(x, y, r + 1, s, mask = maske)
    ursm1 = mixed_central_moment(x, y, r, s - 1, mask = maske)
    u02 = mixed_central_moment(x, y, 0, 2, mask = maske)
    ursp1 = mixed_central_moment(x, y, r, s + 1, mask = maske)
    u11 = mixed_central_moment(x, y, 1, 1, mask = maske)
    rr = real(r, dp)
    ss = real(s, dp)

    mixed_central_moment_var_dp = (u2r2s - urs * urs &
            + rr * rr**u20 * urm1s * urm1s + ss * ss * u02 * ursm1 * ursm1 &
            + 2.0_dp * rr * ss * u11 * urm1s * ursm1 &
            - 2.0_dp * rr * urp1s * urm1s - 2.0_dp * ss * ursp1 * ursm1) / n

  END FUNCTION mixed_central_moment_var_dp


  FUNCTION mixed_central_moment_var_sp(x, y, r, s, mask)
    ! Error variance of mixed central moment (Benedict & Gould 1996)
    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: x
    REAL(sp), DIMENSION(:), INTENT(IN) :: y
    INTEGER(i4), INTENT(IN) :: r
    INTEGER(i4), INTENT(IN) :: s
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: mixed_central_moment_var_sp

    REAL(sp) :: u2r2s, urs, urm1s, u20, urp1s, ursm1, u02, ursp1, u11
    REAL(sp) :: n, rr, ss
    LOGICAL, DIMENSION(size(x)) :: maske

    if (size(x) .ne. size(y)) stop 'Error mixed_central_moment_var_sp: size(x) .ne. size(y)'
    if (present(mask)) then
      if (size(mask) .ne. size(x)) stop 'Error mixed_central_moment_var_sp: size(mask) .ne. size(x)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(x), sp)
    end if
    if (n .le. (2.0_sp + tiny(2.0_sp))) stop 'mixed_central_moment_var_sp: n must be at least 3'

    u2r2s = mixed_central_moment(x, y, 2 * r, 2 * s, mask = maske)
    urs = mixed_central_moment(x, y, r, s, mask = maske)
    urm1s = mixed_central_moment(x, y, r - 1, s, mask = maske)
    u20 = mixed_central_moment(x, y, 2, 0, mask = maske)
    urp1s = mixed_central_moment(x, y, r + 1, s, mask = maske)
    ursm1 = mixed_central_moment(x, y, r, s - 1, mask = maske)
    u02 = mixed_central_moment(x, y, 0, 2, mask = maske)
    ursp1 = mixed_central_moment(x, y, r, s + 1, mask = maske)
    u11 = mixed_central_moment(x, y, 1, 1, mask = maske)
    rr = real(r, sp)
    ss = real(s, sp)

    mixed_central_moment_var_sp = (u2r2s - urs * urs &
            + rr * rr**u20 * urm1s * urm1s + ss * ss * u02 * ursm1 * ursm1 &
            + 2.0_sp * rr * ss * u11 * urm1s * ursm1 &
            - 2.0_sp * rr * urp1s * urm1s - 2.0_sp * ss * ursp1 * ursm1) / n

  END FUNCTION mixed_central_moment_var_sp

  ! ------------------------------------------------------------------

  SUBROUTINE moment_dp(dat, average, variance, skewness, kurtosis, mean, stddev, absdev, mask, sample)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    REAL(dp), OPTIONAL, INTENT(OUT) :: average
    REAL(dp), OPTIONAL, INTENT(OUT) :: variance
    REAL(dp), OPTIONAL, INTENT(OUT) :: skewness
    REAL(dp), OPTIONAL, INTENT(OUT) :: kurtosis
    REAL(dp), OPTIONAL, INTENT(OUT) :: mean
    REAL(dp), OPTIONAL, INTENT(OUT) :: stddev
    REAL(dp), OPTIONAL, INTENT(OUT) :: absdev
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    LOGICAL, OPTIONAL, INTENT(IN) :: sample

    REAL(dp) :: n, div_n  ! divisor depending if sample or population moments

    REAL(dp) :: ep, ave, var
    REAL(dp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error moment_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'moment_dp: n must be at least 2'

    ! set divisor for population (n) or sample (n-1) variance
    div_n = n - 1.0_dp
    if (present(sample)) then
      if (.not. sample) div_n = n
    end if

    ! Any optional argument
    if (.not. (present(average) .or. present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(mean) .or. present(stddev) .or. present(absdev))) return
    ! Average
    ave = sum(dat(:), mask = maske) / n
    if (present(average)) average = ave
    if (present(mean))    mean = ave
    if (.not. (present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(stddev) .or. present(absdev))) return
    ! Absolute deviation
    s(:) = dat(:) - ave
    if (present(absdev)) absdev = sum(abs(s(:)), mask = maske) / n
    ! Variance / Standard deviation
    if (.not. (present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(stddev))) return
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / div_n
    if (present(variance)) variance = var
    ! Standard deviation
    if (present(stddev))   stddev = sqrt(var)
    if (.not. (present(skewness) .or. present(kurtosis))) return
    ! Skewness
    if (abs(var) .lt. tiny(0.0_dp)) stop 'moment_dp: no skewness or kurtosis when zero variance'
    p(:) = p(:) * s(:)
    if (present(skewness)) then
      skewness = sum(p(:), mask = maske)
      skewness = skewness / (n * stddev * stddev * stddev)
    end if
    ! Kurtosis
    if (present(kurtosis)) then
      p(:) = p(:) * s(:)
      kurtosis = sum(p(:), mask = maske)
      kurtosis = kurtosis / (n * variance * variance) - 3.0_dp
    end if

  END SUBROUTINE moment_dp


  SUBROUTINE moment_sp(dat, average, variance, skewness, kurtosis, mean, stddev, absdev, mask, sample)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    REAL(sp), OPTIONAL, INTENT(OUT) :: average
    REAL(sp), OPTIONAL, INTENT(OUT) :: variance
    REAL(sp), OPTIONAL, INTENT(OUT) :: skewness
    REAL(sp), OPTIONAL, INTENT(OUT) :: kurtosis
    REAL(sp), OPTIONAL, INTENT(OUT) :: mean
    REAL(sp), OPTIONAL, INTENT(OUT) :: stddev
    REAL(sp), OPTIONAL, INTENT(OUT) :: absdev
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    LOGICAL, OPTIONAL, INTENT(IN) :: sample

    REAL(sp) :: n, div_n  ! divisor depending if sample or population moments

    REAL(sp) :: ep, ave, var
    REAL(sp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error moment_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'moment_sp: n must be at least 2'

    ! set divisor for population (n) or sample (n-1) variance
    div_n = n - 1.0_sp
    if (present(sample)) then
      if (.not. sample) div_n = n
    end if

    ! Any optional argument
    if (.not. (present(average) .or. present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(mean) .or. present(stddev) .or. present(absdev))) return
    ! Average
    ave = sum(dat(:), mask = maske) / n
    if (present(average)) average = ave
    if (present(mean))    mean = ave
    if (.not. (present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(stddev) .or. present(absdev))) return
    ! Absolute deviation
    s(:) = dat(:) - ave
    if (present(absdev)) absdev = sum(abs(s(:)), mask = maske) / n
    ! Variance / Standard deviation
    if (.not. (present(variance) .or. present(skewness) .or. &
            present(kurtosis) .or. present(stddev))) return
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / div_n
    if (present(variance)) variance = var
    ! Standard deviation
    if (present(stddev))   stddev = sqrt(var)
    if (.not. (present(skewness) .or. present(kurtosis))) return
    ! Skewness
    if (abs(var) .lt. tiny(0.0_sp)) stop 'moment_sp: no skewness or kurtosis when zero variance'
    p(:) = p(:) * s(:)
    if (present(skewness)) then
      skewness = sum(p(:), mask = maske)
      skewness = skewness / (n * stddev * stddev * stddev)
    end if
    ! Kurtosis
    if (present(kurtosis)) then
      p(:) = p(:) * s(:)
      kurtosis = sum(p(:), mask = maske)
      kurtosis = kurtosis / (n * variance * variance) - 3.0_sp
    end if

  END SUBROUTINE moment_sp

  ! ------------------------------------------------------------------

  FUNCTION stddev_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: stddev_dp

    REAL(dp) :: n

    REAL(dp) :: ep, ave, var
    REAL(dp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error stddev_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'stddev_dp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_dp)
    stddev_dp = sqrt(var)

  END FUNCTION stddev_dp


  FUNCTION stddev_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: stddev_sp

    REAL(sp) :: n

    REAL(sp) :: ep, ave, var
    REAL(sp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error stddev_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'stddev_sp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_sp)
    stddev_sp = sqrt(var)

  END FUNCTION stddev_sp

  ! ------------------------------------------------------------------

  FUNCTION skewness_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: skewness_dp

    REAL(dp) :: n

    REAL(dp) :: ep, ave, var, stddev
    REAL(dp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error skewness_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'skewness_dp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_dp)
    stddev = sqrt(var)
    ! Skewness
    if (abs(var) .lt. tiny(0.0_dp)) stop 'skewness_dp: no skewness when zero variance'
    p(:) = p(:) * s(:)
    skewness_dp = sum(p(:), mask = maske)
    skewness_dp = skewness_dp / (n * stddev * stddev * stddev)

  END FUNCTION skewness_dp


  FUNCTION skewness_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: skewness_sp

    REAL(sp) :: n

    REAL(sp) :: ep, ave, var, stddev
    REAL(sp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error skewness_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'skewness_sp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    var = (var - ep * ep / n) / (n - 1.0_sp)
    stddev = sqrt(var)
    ! Skewness
    if (abs(var) .lt. tiny(0.0_sp)) stop 'skewness_sp: no skewness when zero variance'
    p(:) = p(:) * s(:)
    skewness_sp = sum(p(:), mask = maske)
    skewness_sp = skewness_sp / (n * stddev * stddev * stddev)

  END FUNCTION skewness_sp

  ! ------------------------------------------------------------------

  FUNCTION variance_dp(dat, mask)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(dp) :: variance_dp

    REAL(dp) :: n

    REAL(dp) :: ep, ave, var
    REAL(dp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error variance_dp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), dp)
    else
      maske(:) = .true.
      n = real(size(dat), dp)
    end if
    if (n .le. (1.0_dp + tiny(1.0_dp))) stop 'variance_dp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    variance_dp = (var - ep * ep / n) / (n - 1.0_dp)

  END FUNCTION variance_dp


  FUNCTION variance_sp(dat, mask)

    IMPLICIT NONE

    REAL(sp), DIMENSION(:), INTENT(IN) :: dat
    LOGICAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: mask
    REAL(sp) :: variance_sp

    REAL(sp) :: n

    REAL(sp) :: ep, ave, var
    REAL(sp), DIMENSION(size(dat)) :: p, s
    LOGICAL, DIMENSION(size(dat)) :: maske

    if (present(mask)) then
      if (size(mask) .ne. size(dat)) stop 'Error variance_sp: size(mask) .ne. size(dat)'
      maske = mask
      n = real(count(maske), sp)
    else
      maske(:) = .true.
      n = real(size(dat), sp)
    end if
    if (n .le. (1.0_sp + tiny(1.0_sp))) stop 'variance_sp: n must be at least 2'

    ! Average
    ave = sum(dat(:), mask = maske) / n
    s(:) = dat(:) - ave
    ! Variance / Standard deviation
    ep = sum(s(:), mask = maske)
    p(:) = s(:) * s(:)
    var = sum(p(:), mask = maske)
    variance_sp = (var - ep * ep / n) / (n - 1.0_sp)

  END FUNCTION variance_sp

  ! ------------------------------------------------------------------

END MODULE mo_moment
