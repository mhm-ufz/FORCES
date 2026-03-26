MODULE mo_histogram

  ! This module contains routines for the histogram generation of input vectors

  ! Literature

  ! Written Dec 2024, Pallav Shrestha

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

  ! Copyright 2024 Pallav Shrestha

  USE mo_kind, ONLY : i4, sp, dp

  PUBLIC :: histogram    ! Generate histogram

  ! ------------------------------------------------------------------

  !     NAME
  !         histogram

  !     PURPOSE
  !         Calculates the mean absolute deviations from the mean
  !             histogram = sum(abs(x-mean(x)))/n
  !
  !         If an optinal mask is given, the calculations are over those locations that correspond to true values in the mask.
  !         x can be single or double precision. The result will have the same numerical precision.

  !     CALLING SEQUENCE
  !         out = histogram(dat, mask=mask)

  !     INTENT(IN)
  !         real(sp/dp) :: dat(:)     1D-array with input numbers

  !     INTENT(INOUT)
  !         None

  !     INTENT(OUT)
  !         real(sp/dp) :: histogram     mean absolute deviations from average

  !     INTENT(IN), OPTIONAL
  !         logical :: mask(:)        1D-array of logical values with size(dat).
  !                                   If present, only those locations in vec corresponding to the true values in mask are used.

  !     INTENT(INOUT), OPTIONAL
  !         None

  !     INTENT(OUT), OPTIONAL
  !         None

  !     RESTRICTIONS
  !         Input values must be floating points.

  !     EXAMPLE
  !         vec = (/ 1., 2, 3., -999., 5., 6. /)
  !         m   = histogram(vec, mask=(vec >= 0.))
  !         -> see also example in test directory

  !     HISTORY
  !         Written,  Pallav Shrestha, Dec 2024

  INTERFACE histogram
    MODULE PROCEDURE histogram_dp
  END INTERFACE histogram
  
  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------


FUNCTION histogram_dp(data, num_bins, min_edge, max_edge)
    
    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: data
    INTEGER(i4), INTENT(IN) :: num_bins
    REAL(dp), INTENT(IN) :: min_edge, max_edge
    INTEGER(i4), DIMENSION(:), ALLOCATABLE :: histogram_dp

    ! ! Outputs
    ! INTEGER(i4), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: hist_count

    ! Local variables
    REAL(dp), DIMENSION(:), ALLOCATABLE:: hist_bins
    REAL(dp) :: bin_width
    INTEGER(i4) :: i, bin_index

    ! Validate inputs
    IF (num_bins <= 0) THEN
        PRINT *, "Error histogram_dp: num_bins must be greater than 0."
        STOP
    END IF

    IF (min_edge >= max_edge) THEN
        PRINT *, "Error histogram_dp: min_edge must be less than max_edge."
        STOP
    END IF

    ! Initialize bins and histogram
    ALLOCATE(histogram_dp(num_bins))
    ALLOCATE(hist_bins(num_bins + 1))  ! One extra for defining edges
    histogram_dp = 0

    ! Define bin edges
    bin_width = (max_edge - min_edge) / num_bins

    DO i = 1, num_bins + 1
        hist_bins(i) = min_edge + (i - 1) * bin_width
    END DO

    ! Compute histogram
    DO i = 1, SIZE(data)

        if (data(i) < min_edge) then
           ! out-of-range to the left 
           bin_index = 1
        else if (data(i) > max_edge) then
           ! out-of-range to the right 
            bin_index = num_bins
        else
           ! within range
            bin_index = INT((data(i) - min_edge) / bin_width) + 1
        end if

        ! Ensure the last bin includes the maximum value
        IF (bin_index > num_bins) bin_index = num_bins

        histogram_dp(bin_index) = histogram_dp(bin_index) + 1
        
    END DO

END FUNCTION histogram_dp


END MODULE mo_histogram
