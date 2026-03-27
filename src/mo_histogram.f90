!> \file    mo_histogram.f90
!> \copydoc mo_histogram

!> \brief   Histogram generation utilities.
!> \details This module provides routines to compute histogram bin counts for
!>          one-dimensional input data.
!> \authors Pallav Shrestha
!> \date    Dec 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_histogram

  USE mo_kind, ONLY : i4, sp, dp
  use mo_message, ONLY : error_message

  PUBLIC :: histogram    ! Generate histogram

  ! ------------------------------------------------------------------
  !> \brief Compute histogram bin counts.
  !> \details This generic interface currently dispatches to the
  !>          double-precision implementation for one-dimensional arrays.
  !>          Values below `min_edge` are accumulated in the first bin, values
  !>          above `max_edge` are accumulated in the last bin, and `max_edge`
  !>          itself is included in the last bin.
  !> \param[in] data Input values to bin.
  !> \param[in] num_bins Number of equally spaced bins.
  !> \param[in] min_edge Lower edge of the histogram range.
  !> \param[in] max_edge Upper edge of the histogram range.
  !> \return Histogram counts for each bin.
  INTERFACE histogram
    MODULE PROCEDURE histogram_dp
  END INTERFACE histogram

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------
  !> \brief Compute a histogram for a double-precision vector.
  !> \details Creates `num_bins` equally spaced bins over the interval
  !>          `[min_edge, max_edge]` and counts how many elements of `data`
  !>          fall into each bin. Values below `min_edge` are assigned to the
  !>          first bin, values above `max_edge` are assigned to the last bin,
  !>          and `max_edge` is included in the last bin.
  !> \return Histogram counts for each bin.
  FUNCTION histogram_dp(data, num_bins, min_edge, max_edge)

    IMPLICIT NONE

    REAL(dp), DIMENSION(:), INTENT(IN) :: data !< Input data vector.
    INTEGER(i4), INTENT(IN) :: num_bins !< Number of equally spaced bins.
    REAL(dp), INTENT(IN) :: min_edge !< Lower edge of the histogram range.
    REAL(dp), INTENT(IN) :: max_edge !< Upper edge of the histogram range.
    INTEGER(i4), DIMENSION(:), ALLOCATABLE :: histogram_dp !< Histogram counts for each bin.

    ! Local variables
    REAL(dp), DIMENSION(:), ALLOCATABLE:: hist_bins
    REAL(dp) :: bin_width
    INTEGER(i4) :: i, bin_index

    ! Validate inputs
    IF (num_bins <= 0) call error_message("Error histogram_dp: num_bins must be greater than 0.")
    IF (min_edge >= max_edge) call error_message("Error histogram_dp: min_edge must be less than max_edge.")

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
