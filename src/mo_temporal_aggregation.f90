!> \file mo_temporal_aggregation.f90
!> \copydoc mo_temporal_aggregation

!> \brief Temporal aggregation for time series (averaging)
!> \details This module does temporal aggregation (averaging) of time series
!> \changelog
!! - Pallav Shrestha, Jun 2019
!!   - changed the output argument I/O from INOUT to OUT
!> \authors Oldrich Rakovec, Rohini Kumar, Pallav Shrestha
!> \date October 2015
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_temporal_aggregation

  use mo_kind, ONLY : i4, dp
  use mo_julian, ONLY : julday, dec2date
  use mo_constants, ONLY : eps_dp

  IMPLICIT NONE

  PUBLIC :: day2mon_average    ! converts daily time series to monthly
  PUBLIC :: hour2day_average   ! converts hourly time series to daily
  PUBLIC :: day2mon_sum        ! converts daily time series to monthly sums

  ! ------------------------------------------------------------------

  !>        \brief Day-to-month average (day2mon_average)

  !>        \details converts daily time series to monthly

  !>        \param[in] "real(sp/dp) :: daily_data(:)"   array of daily time series
  !>        \param[in] "integer(i4) :: year"            year of the starting time
  !>        \param[in] "integer(i4) :: month"           month of the starting time
  !>        \param[in] "integer(i4) :: day"             day of the starting time
  !>        \param[out] "real(sp/dp) :: mon_average(:)"  array of monthly averaged values
  !>        \param[in] "real(sp/dp), optional :: misval"          missing value definition
  !>        \param[in] "logical, optional     :: rm_misval"       switch to exclude missing values

  !>        \authors Oldrich Rakovec, Rohini Kumar
  !>        \date Oct 2015

  INTERFACE day2mon_average
    MODULE PROCEDURE day2mon_average_dp
  END INTERFACE day2mon_average

  ! ------------------------------------------------------------------

  !>        \brief Hour-to-day average (hour2day_average)

  !>        \details converts hourly time series to daily

  !>        \param[in] "real(sp/dp) :: hourly_data(:)"  array of hourly time series
  !>        \param[in] "integer(i4) :: year"            year of the starting time
  !>        \param[in] "integer(i4) :: month"           month of the starting time
  !>        \param[in] "integer(i4) :: day"             day of the starting time
  !>        \param[in] "integer(i4) :: hour"            hour of the starting time
  !>        \param[inout] "real(sp/dp) :: day_average(:)"         array of daily averaged values
  !>        \param[in] "real(sp/dp), optional :: misval"          missing value definition
  !>        \param[in] "logical, optional     :: rm_misval"       switch to exclude missing values

  !>        \note Hours values should be from 0 to 23 (NOT from 1 to 24!)

  !>        \author Oldrich Rakovec, Rohini Kumar
  !>        \date Oct 2015

  INTERFACE hour2day_average
    MODULE PROCEDURE hour2day_average_dp
  END INTERFACE hour2day_average

  ! ------------------------------------------------------------------

  !>        \brief Day-to-month sum (day2mon_sum)

  !>        \details converts daily time series to monthly sums

  !>        \param[in] "real(sp/dp) :: daily_data(:)"   array of daily time series
  !>        \param[in] "integer(i4) :: year"            year of the starting time
  !>        \param[in] "integer(i4) :: month"           month of the starting time
  !>        \param[in] "integer(i4) :: day"             day of the starting time
  !>        \param[out] "real(sp/dp) :: mon_sum(:)"     array of monthly summed values
  !>        \param[in] "real(sp/dp), optional :: misval"          missing value definition
  !>        \param[in] "logical, optional     :: rm_misval"       switch to exclude missing values

  !>        \author Pallav Kumar Shrestha
  !>        \date Apr 2019

  INTERFACE day2mon_sum
    MODULE PROCEDURE day2mon_sum_dp
  END INTERFACE day2mon_sum

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  SUBROUTINE day2mon_average_dp(daily_data, yearS, monthS, dayS, mon_avg, misval, rm_misval)

    IMPLICIT NONE

    REAL(dp), dimension(:), INTENT(IN) :: daily_data      ! array of daily data
    INTEGER(i4), INTENT(IN) :: yearS           ! year of the initial time step
    INTEGER(i4), INTENT(IN) :: monthS          ! month of the initial time step
    INTEGER(i4), INTENT(IN) :: dayS            ! day of the initial time step

    REAL(dp), dimension(:), allocatable, INTENT(OUT) :: mon_avg         ! array of the monthly averages

    REAL(dp), optional, INTENT(IN) :: misval          ! missing value definition
    logical, optional, INTENT(IN) :: rm_misval       ! switch to remove missing values

    ! local variables
    INTEGER(i4) :: ndays, tt, kk      ! number of days, indices
    INTEGER(i4) :: start_day, end_day ! size of input array, size of days
    INTEGER(i4) :: y, m
    INTEGER(i4) :: year, month, day    ! variables for date
    INTEGER(i4) :: yearE, monthE, dayE ! vatiables for End date
    REAL(dp) :: newTime

    REAL(dp), dimension(:, :), allocatable :: nCounter_m       ! counter number of days in months (w/ data)
    REAL(dp), dimension(:, :), allocatable :: nCounter_m_full  ! counter number of days in months (complete)
    REAL(dp), dimension(:, :), allocatable :: mon_sum          ! monthly sum

    INTEGER(i4) :: nmonths     ! number of days, number of months
    LOGICAL :: remove      ! switch for considering missing data
    REAL(dp) :: missing  ! switch for reading missing value or default -9999.

    if (present(misval)) then
      missing = misval
    else
      missing = -9999._dp
    end if

    if (present(rm_misval)) then
      remove = rm_misval
    else
      remove = .FALSE.
    end if

    ! get total number of days
    ndays = SIZE(daily_data)

    ! assign initial julian day
    start_day = julday(dayS, monthS, yearS)

    ! calculate last julian day
    end_day = start_day + ndays - 1_i4

    ! get year, month and day of the end date:
    call dec2date(real(end_day, dp), yy = yearE, mm = monthE, dd = dayE)

    ! get number of days with data for each month
    allocate(nCounter_m(yearS : yearE, 12))
    allocate(nCounter_m_full(yearS : yearE, 12))
    allocate(mon_sum(yearS : yearE, 12))
    nCounter_m(:, :) = 0
    nCounter_m_full(:, :) = 0
    mon_sum(:, :) = 0.0_dp

    newTime = real(start_day, dp)
    ! calculate monthly sums
    do tt = 1, (end_day - start_day + 1)
      call dec2date((newTime + tt - 1), yy = year, mm = month, dd = day)
      nCounter_m_full(year, month) = nCounter_m_full(year, month) + 1.0_dp
      if (abs(daily_data(tt) - missing) .lt. eps_dp) cycle
      mon_sum(year, month) = mon_sum(year, month) + daily_data(tt)
      nCounter_m(year, month) = nCounter_m(year, month) + 1.0_dp
    end do

    ! calculate number of months
    nmonths = 0
    do y = yearS, yearE
      do m = 1, 12
        if ((y .EQ. yearS) .AND. (m .LT. monthS)) cycle
        if ((y .EQ. yearE) .AND. (m .GT. monthE)) cycle
        nmonths = nmonths + 1
      end do
    end do

    ! calculate monthly averages
    if(allocated(mon_avg)) deallocate(mon_avg)
    allocate(mon_avg(nmonths))
    mon_avg(:) = missing
    kk = 0
    do y = yearS, yearE
      do m = 1, 12
        if ((y .EQ. yearS) .AND. (m .LT. monthS)) cycle
        if ((y .EQ. yearE) .AND. (m .GT. monthE)) cycle
        kk = kk + 1
        if ((nCounter_m(y, m) .GT. 0) .AND. &
                (abs(nCounter_m_full(y, m) - nCounter_m(y, m)) .LT. eps_dp)) then
          mon_avg(kk) = mon_sum(y, m) / real(nCounter_m(y, m), dp)
        else if ((nCounter_m(y, m) .GT. 0) .AND. remove) then
          mon_avg(kk) = mon_sum(y, m) / real(nCounter_m(y, m), dp)
        end if
      end do
    end do

    deallocate(nCounter_m_full)
    deallocate(nCounter_m)
    deallocate(mon_sum)

  END SUBROUTINE day2mon_average_dp

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE hour2day_average_dp(hourly_data, yearS, monthS, dayS, hourS, day_avg, misval, rm_misval)

    IMPLICIT NONE

    REAL(dp), dimension(:), INTENT(IN) :: hourly_data     ! array of hourly data
    INTEGER(i4), INTENT(IN) :: yearS           ! year of the initial time step
    INTEGER(i4), INTENT(IN) :: monthS          ! month of the initial time step
    INTEGER(i4), INTENT(IN) :: dayS            ! day of the initial time step
    INTEGER(i4), INTENT(IN) :: hourS           ! hour of the initial time step

    REAL(dp), dimension(:), allocatable, INTENT(INOUT) :: day_avg         ! array of the daily averages

    REAL(dp), optional, INTENT(IN) :: misval          ! missing value definition
    logical, optional, INTENT(IN) :: rm_misval       ! switch to remove missing values

    ! local variables
    INTEGER(i4) :: nhours, ndays_dummy, tt, dd, kk
    REAL(dp) :: start_day, end_day   ! assign julian values
    INTEGER(i4) :: yearE, monthE, dayE, hourE, hourEd ! End dates, incl. Dummy

    REAL(dp), dimension(:), allocatable :: nCounter_h       ! counter number of hours in day (w/ data)
    REAL(dp), dimension(:), allocatable :: nCounter_h_full  ! counter number of hours in day (complete)
    REAL(dp), dimension(:), allocatable :: day_sum          ! daily sum

    LOGICAL :: remove   ! switch for considering missing data
    REAL(dp) :: missing  ! switch for reading missing value or default -9999.

    if (present(misval)) then
      missing = misval
    else
      missing = -9999._dp
    end if

    if (present(rm_misval)) then
      remove = rm_misval
    else
      remove = .FALSE.
    end if

    ! get total number of hours
    nhours = SIZE(hourly_data)
    ! assign initial julian day
    start_day = julday(dayS, monthS, yearS) - 0.5_dp + real(hourS, dp) / 24._dp

    ! calculate last julian day
    end_day = start_day + real(nhours - 1._dp, dp) / 24._dp

    ! get year, month and day of the end date
    call dec2date(end_day, yy = yearE, mm = monthE, dd = dayE, hh = hourE)

    ! get largerst possible number of calendar days
    ndays_dummy = ceiling(real(nhours, dp) / 24._dp + 2._dp)

    allocate(day_sum(ndays_dummy))
    allocate(nCounter_h(ndays_dummy))
    allocate(nCounter_h_full(ndays_dummy))
    day_sum(:) = 0.0_dp
    nCounter_h(:) = 0
    nCounter_h_full(:) = 0

    ! calculate daily sums
    dd = 1
    do tt = 1, nhours
      call dec2date(start_day + real(tt - 1, dp) / 24._dp, hh = hourEd)
      nCounter_h_full(dd) = nCounter_h_full(dd) + 1
      if (abs(hourly_data(tt) - missing) .lt. eps_dp) then
        day_sum(dd) = day_sum(dd)
      else
        day_sum(dd) = day_sum(dd) + hourly_data(tt)
        nCounter_h(dd) = nCounter_h(dd) + 1
      end if
      if ((hourEd .EQ. 23) .AND. (tt .LT. nhours)) dd = dd + 1
    end do

    ! dd is the total number of calendar days, between hourS and hourE
    allocate(day_avg(dd))
    day_avg(:) = missing

    ! calculate daily average
    do kk = 1, dd
      if ((nCounter_h(kk) .GT. 0) .AND. &
              (abs(nCounter_h_full(kk) - nCounter_h(kk)) .LT. eps_dp)) then
        day_avg(kk) = day_sum(kk) / real(nCounter_h(kk), dp)
      else if ((nCounter_h(kk) .GT. 0) .AND. remove) then
        day_avg(kk) = day_sum(kk) / real(nCounter_h(kk), dp)
      end if
    end do

    deallocate(nCounter_h_full)
    deallocate(nCounter_h)
    deallocate(day_sum)

  END SUBROUTINE hour2day_average_dp

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE day2mon_sum_dp(daily_data, yearS, monthS, dayS, mon_sum, misval, rm_misval)

    IMPLICIT NONE

    REAL(dp), dimension(:), INTENT(IN) :: daily_data      ! array of daily data
    INTEGER(i4), INTENT(IN) :: yearS           ! year of the initial time step
    INTEGER(i4), INTENT(IN) :: monthS          ! month of the initial time step
    INTEGER(i4), INTENT(IN) :: dayS            ! day of the initial time step

    REAL(dp), dimension(:), allocatable, INTENT(OUT) :: mon_sum         ! array of the monthly sums

    REAL(dp), optional, INTENT(IN) :: misval          ! missing value definition
    logical, optional, INTENT(IN) :: rm_misval       ! switch to remove missing values

    ! local variables
    INTEGER(i4) :: ndays, tt, kk      ! number of days, indices
    INTEGER(i4) :: start_day, end_day ! size of input array, size of days
    INTEGER(i4) :: y, m
    INTEGER(i4) :: year, month, day    ! variables for date
    INTEGER(i4) :: yearE, monthE, dayE ! vatiables for End date
    REAL(dp) :: newTime

    REAL(dp), dimension(:, :), allocatable :: nCounter_m       ! counter number of days in months (w/ data)
    REAL(dp), dimension(:, :), allocatable :: nCounter_m_full  ! counter number of days in months (complete)
    REAL(dp), dimension(:, :), allocatable :: mon_sum_temp_2d  ! monthly sum temporary variable
    REAL(dp), dimension(:),    allocatable :: mon_sum_temp_1d  ! monthly sum temporary variable

    INTEGER(i4) :: nmonths     ! number of days, number of months
    LOGICAL :: remove      ! switch for considering missing data
    REAL(dp) :: missing  ! switch for reading missing value or default -9999.

    if (present(misval)) then
      missing = misval
    else
      missing = -9999._dp
    end if

    if (present(rm_misval)) then
      remove = rm_misval
    else
      remove = .FALSE.
    end if

    ! get total number of days
    ndays = SIZE(daily_data)

    ! assign initial julian day
    start_day = julday(dayS, monthS, yearS)

    ! calculate last julian day
    end_day = start_day + ndays - 1_i4

    ! get year, month and day of the end date:
    call dec2date(real(end_day, dp), yy = yearE, mm = monthE, dd = dayE)

    ! get number of days with data for each month
    allocate(nCounter_m(yearS : yearE, 12))
    allocate(nCounter_m_full(yearS : yearE, 12))
    allocate(mon_sum_temp_2d(yearS : yearE, 12))
    nCounter_m(:, :) = 0
    nCounter_m_full(:, :) = 0
    mon_sum_temp_2d(:, :) = 0.0_dp

    newTime = real(start_day, dp)
    ! calculate monthly sums
    do tt = 1, (end_day - start_day + 1)
      call dec2date((newTime + tt - 1), yy = year, mm = month, dd = day)
      nCounter_m_full(year, month) = nCounter_m_full(year, month) + 1.0_dp
      if (abs(daily_data(tt) - missing) .lt. eps_dp) cycle
      mon_sum_temp_2d(year, month) = mon_sum_temp_2d(year, month) + daily_data(tt)
      nCounter_m(year, month) = nCounter_m(year, month) + 1.0_dp
    end do

    ! calculate number of months
    nmonths = 0
    do y = yearS, yearE
      do m = 1, 12
        if ((y .EQ. yearS) .AND. (m .LT. monthS)) cycle
        if ((y .EQ. yearE) .AND. (m .GT. monthE)) cycle
        nmonths = nmonths + 1
      end do
    end do


    ! store monthly sums
    allocate(mon_sum_temp_1d(nmonths))
    mon_sum_temp_1d(:) = missing
    kk = 0
    do y = yearS, yearE
      do m = 1, 12
        if ((y .EQ. yearS) .AND. (m .LT. monthS)) cycle
        if ((y .EQ. yearE) .AND. (m .GT. monthE)) cycle
        kk = kk + 1
        if ((nCounter_m(y, m) .GT. 0) .AND. &
                (abs(nCounter_m_full(y, m) - nCounter_m(y, m)) .LT. eps_dp)) then
          mon_sum_temp_1d(kk) = mon_sum_temp_2d(y, m)
        else if ((nCounter_m(y, m) .GT. 0) .AND. remove) then
          mon_sum_temp_1d(kk) = mon_sum_temp_2d(y, m)
        end if
      end do
    end do

    if(allocated(mon_sum)) deallocate(mon_sum)
    allocate(mon_sum(nmonths))
    mon_sum = mon_sum_temp_1d

    deallocate(nCounter_m_full)
    deallocate(nCounter_m)
    deallocate(mon_sum_temp_2d)
    deallocate(mon_sum_temp_1d)

  END SUBROUTINE day2mon_sum_dp

END MODULE mo_temporal_aggregation
