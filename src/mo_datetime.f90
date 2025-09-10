!> \file    mo_datetime.f90
!> \copydoc mo_datetime

!> \brief   Types to deal with datetimes.
!> \details This module provides four types to deal with date and time
!!          1. \ref puredate : containing year, month and day
!!          2. \ref puretime : containing hour, minute and second
!!          3. \ref datetime : combination of date and time
!!          4. \ref timedelta : difference between two datetimes (or dates) in days and (sub-day) seconds
!!
!!          These type can be used in arithmetic operations (+, -, *, /) and can be compared (<, >, <=, >=, ==, /=)
!!          where it makes sense.
!!
!!          The following example demonstrates the functionality:
!!          \code{.f90}
!!          program main
!!            use mo_datetime, only: puredate, puretime, datetime, timedelta, one_day, midday, DAY_SECONDS, HOUR_SECONDS
!!            implicit none
!!            type(datetime) :: date1, date2, date3, date4, date5
!!            type(puredate) :: day1
!!            type(puretime) :: time1
!!            type(timedelta) :: delta1
!!
!!            ! create dates add time-deltas
!!            date1 = datetime(2000, 2, 28)
!!            date2 = date1 + one_day()
!!            print*, date2%str()
!!            date3 = date1 + 2 * one_day()
!!            print*, date3%str()
!!
!!            ! substract half a day
!!            delta1 = one_day() / 2
!!            date4 = date3 - delta1
!!
!!            ! compare dates/times
!!            print*, 'is midday: ', date4%time() == midday()
!!            print*, 'date4 after date2: ', date4 > date2
!!
!!            ! create from date and time
!!            date5 = datetime(date1%date(), date4%time())
!!            print*, date5%str()
!!
!!            ! create from datetime string
!!            date5 = datetime('2023-05-08 12:32:30')
!!            day1 = puredate('2023-05-08')
!!            time1 = puretime('12:32:30')
!!            print*, date5 == time1%with_date(day1)
!!            print*, date5 == day1%with_time(time1)
!!            print*, date5 == datetime(day1, time1)
!!
!!            ! use cf-convention string and value
!!            date5 = datetime('seconds since 1992-10-8 15:15:42', DAY_SECONDS - HOUR_SECONDS)
!!            print*, date5%str()
!!          end program main
!!          \endcode
!!
!!          Several special constants are provided as well:
!!          - integer constants for duration ratios:
!!            - \ref year_days : days in standard year (365)
!!            - \ref leap_year_days : days in leap year (366)
!!            - \ref year_months : months in year (12)
!!            - \ref week_days : days in week (7)
!!            - \ref day_hours : hours in day (24)
!!            - \ref hour_minutes : minutes in hour (60)
!!            - \ref minute_seconds : seconds in minute (60)
!!            - \ref day_minutes : minutes in day (1440)
!!            - \ref day_seconds : seconds in day (86400)
!!            - \ref hour_seconds : seconds in hour (3600)
!!            - \ref week_hours : hours in week (168)
!!            - \ref week_minutes : minutes in week (10080)
!!            - \ref week_seconds : seconds in week (604800)
!!
!!          Provided convenience routines:
!!          - \ref midnight and \ref midday : \ref puretime for special day times
!!          - \ref day_hour : \ref puretime for each hour of the day (0-23)
!!          - \ref zero_delta , \ref one_week , \ref one_day , \ref one_hour , \ref one_minute and \ref one_second :
!!            special \ref timedelta values
!!          - \ref currently : \copybrief currently
!!          - \ref today : \copybrief today
!!          - \ref now : \copybrief now
!!          - \ref is_leap_year : \copybrief is_leap_year
!!          - \ref days_in_month : \copybrief days_in_month
!!          - \ref days_in_year : \copybrief days_in_year
!!
!!          A date is assumed to be given in the gregorian calender.
!!          That means, there is a leap year (February has 29 days instead of 28) if:
!!          - year is divisible by 4
!!          - year *is not* divisible by 100 or it *is* divisible by 400
!!
!!          \note Dates before 1582-10-15 should be used with caution.
!!          The gregorian calender replaced the julian calender and advanced the date by
!!          10 days: Thursday 4 October 1582 was followed by Friday 15 October 1582.
!!          Using this module for erlier dates will assume the *proleptic gregorian* calendar.
!!
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2023
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
module mo_datetime

  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_string_utils, only : num2str, divide_string
  use mo_julian, only : dec2date, date2dec

  implicit none

  public :: puredate
  public :: puretime
  public :: datetime
  public :: timedelta
  ! system time
  public :: today
  public :: now
  public :: currently
  ! constants
  public :: zero_delta
  public :: one_day
  public :: one_hour
  public :: one_minute
  public :: one_second
  public :: one_week
  public :: midnight
  public :: midday
  public :: day_hour
  ! checking
  public :: is_leap_year
  public :: days_in_month
  public :: days_in_year
  ! helpers
  public :: decode_cf_time_units
  public :: delta_from_string

  private

  integer(i4), parameter, public :: YEAR_DAYS = 365_i4 !< days in standard year
  integer(i4), parameter, public :: LEAP_YEAR_DAYS = 366_i4 !< days in leap year
  integer(i4), parameter, public :: YEAR_MONTHS = 12_i4 !< months in year
  integer(i4), parameter, public :: WEEK_DAYS = 7_i4 !< days in week
  integer(i4), parameter, public :: DAY_HOURS = 24_i4 !< hours in day
  integer(i4), parameter, public :: CLOCK_HOURS = 12_i4 !< hours on a clock
  integer(i4), parameter, public :: HOUR_MINUTES = 60_i4 !< minutes in hour
  integer(i4), parameter, public :: MINUTE_SECONDS = 60_i4 !< seconds in minute
  integer(i4), parameter, public :: DAY_MINUTES = DAY_HOURS * HOUR_MINUTES !< minutes in day
  integer(i4), parameter, public :: DAY_SECONDS = DAY_MINUTES * MINUTE_SECONDS !< seconds in day
  integer(i4), parameter, public :: HOUR_SECONDS = HOUR_MINUTES * MINUTE_SECONDS !< seconds in hour
  integer(i4), parameter, public :: WEEK_HOURS = WEEK_DAYS * DAY_HOURS !< hours in week
  integer(i4), parameter, public :: WEEK_MINUTES = WEEK_DAYS * DAY_MINUTES !< minutes in week
  integer(i4), parameter, public :: WEEK_SECONDS = WEEK_DAYS * DAY_SECONDS !< seconds in week
  integer(i4), parameter :: MIN_YEAR = 1_i4 !< minimum for year
  integer(i4), parameter :: MAX_YEAR = 9999_i4 !< maximum for year

  !> \class   puredate
  !> \brief   This is a container to hold only a date.
  type puredate
    integer(i4), public :: year = 1_i4                     !< 1 <= year <= 9999
    integer(i4), public :: month = 1_i4                    !< 1 <= month <= 12
    integer(i4), public :: day = 1_i4                      !< 1 <= day <= number of days in the given month and year
  contains
    procedure, public :: replace => d_replace
    procedure, public :: with_time => dt_from_date_time
    procedure, public :: to_datetime
    procedure, public :: to_ordinal
    procedure, public :: str => d_str
    procedure, public :: julian => d_julian
    procedure, public :: weekday => d_weekday
    procedure, public :: doy => d_doy
    procedure, public :: is_new_year => d_is_new_year
    procedure, public :: is_new_month => d_is_new_month
    procedure, public :: is_new_week => d_is_new_week
    procedure, public :: next_new_year => d_next_new_year
    procedure, public :: next_new_month => d_next_new_month
    procedure, public :: next_new_week => d_next_new_week
    procedure, public :: next_new_day => d_next_new_day
    procedure, public :: previous_new_year => d_previous_new_year
    procedure, public :: previous_new_month => d_previous_new_month
    procedure, public :: previous_new_week => d_previous_new_week
    procedure, public :: previous_new_day => d_previous_new_day
    procedure, public :: year_start => d_year_start
    procedure, public :: month_start => d_month_start
    procedure, public :: week_start => d_week_start
    procedure, private :: d_eq, d_eq_dt
    generic, public :: operator(==) => d_eq, d_eq_dt
    procedure, private :: d_neq, d_neq_dt
    generic, public :: operator(/=) => d_neq, d_neq_dt
    procedure, private :: d_lt, d_lt_dt
    generic, public :: operator(<) => d_lt, d_lt_dt
    procedure, private :: d_gt, d_gt_dt
    generic, public :: operator(>) => d_gt, d_gt_dt
    procedure, private :: d_leq, d_leq_dt
    generic, public :: operator(<=) => d_leq, d_leq_dt
    procedure, private :: d_geq, d_geq_dt
    generic, public :: operator(>=) => d_geq, d_geq_dt
    procedure, private :: d_add_td
    procedure, pass(this), private :: td_add_d
    generic, public :: operator(+) => d_add_td, td_add_d
    procedure, private :: d_sub_td, d_sub_d, d_sub_dt
    generic, public :: operator(-) => d_sub_td, d_sub_d, d_sub_dt
  end type puredate

  !> \class   puretime
  !> \brief   This is a container to hold only a time.
  type puretime
    integer(i4), public :: hour = 0_i4                     !< 0 <= hour < 24
    integer(i4), public :: minute = 0_i4                   !< 0 <= minute < 60
    integer(i4), public :: second = 0_i4                   !< 0 <= second < 60
  contains
    procedure, public :: replace => t_replace
    procedure, public, pass(in_time) :: with_date => dt_from_date_time
    procedure, public :: str => t_str
    procedure, public :: day_second => t_day_second
    procedure, public :: is_midnight => t_is_new_day
    procedure, public :: is_new_day => t_is_new_day
    procedure, public :: is_new_hour => t_is_new_hour
    procedure, public :: is_new_minute => t_is_new_minute
    procedure, public :: next_new_hour => t_next_new_hour
    procedure, public :: next_new_minute => t_next_new_minute
    procedure, public :: previous_new_hour => t_previous_new_hour
    procedure, public :: previous_new_minute => t_previous_new_minute
    procedure, public :: hour_start => t_hour_start
    procedure, public :: minute_start => t_minute_start
    procedure, private :: t_copy
    generic, public :: assignment(=) => t_copy
    procedure, private :: t_eq
    generic, public :: operator(==) => t_eq
    procedure, private :: t_neq
    generic, public :: operator(/=) => t_neq
    procedure, private :: t_lt
    generic, public :: operator(<) => t_lt
    procedure, private :: t_gt
    generic, public :: operator(>) => t_gt
    procedure, private :: t_leq
    generic, public :: operator(<=) => t_leq
    procedure, private :: t_geq
    generic, public :: operator(>=) => t_geq
    procedure, private :: t_add_td
    procedure, pass(this), private :: td_add_t
    generic, public :: operator(+) => t_add_td, td_add_t
    procedure, private :: t_sub_td, t_sub_t
    generic, public :: operator(-) => t_sub_td, t_sub_t
  end type puretime

  !> \class   datetime
  !> \brief   This is a container to hold a date-time.
  type datetime
    integer(i4), public :: year = 1_i4                     !< 1 <= year <= 9999
    integer(i4), public :: month = 1_i4                    !< 1 <= month <= 12
    integer(i4), public :: day = 1_i4                      !< 1 <= day <= number of days in the given month and year
    integer(i4), public :: hour = 0_i4                     !< 0 <= hour < 24
    integer(i4), public :: minute = 0_i4                   !< 0 <= minute < 60
    integer(i4), public :: second = 0_i4                   !< 0 <= second < 60
  contains
    procedure, public :: replace => dt_replace
    procedure, public :: date => get_date
    procedure, public :: time => get_time
    procedure, public :: str => dt_str
    procedure, public :: julian => dt_julian
    procedure, public :: weekday => dt_weekday
    procedure, public :: doy => dt_doy
    procedure, public :: date_to_ordinal => dt_date_to_ordinal
    procedure, public :: day_second => dt_day_second
    procedure, public :: is_new_year
    procedure, public :: is_new_month
    procedure, public :: is_new_week
    procedure, public :: is_new_day
    procedure, public :: is_new_hour
    procedure, public :: is_new_minute
    procedure, public :: next_new_year => dt_next_new_year
    procedure, public :: next_new_month => dt_next_new_month
    procedure, public :: next_new_week => dt_next_new_week
    procedure, public :: next_new_day => dt_next_new_day
    procedure, public :: next_new_hour => dt_next_new_hour
    procedure, public :: next_new_minute => dt_next_new_minute
    procedure, public :: previous_new_year => dt_previous_new_year
    procedure, public :: previous_new_month => dt_previous_new_month
    procedure, public :: previous_new_week => dt_previous_new_week
    procedure, public :: previous_new_day => dt_previous_new_day
    procedure, public :: previous_new_hour => dt_previous_new_hour
    procedure, public :: previous_new_minute => dt_previous_new_minute
    procedure, public :: year_start => dt_year_start
    procedure, public :: month_start => dt_month_start
    procedure, public :: week_start => dt_week_start
    procedure, public :: day_start => dt_day_start
    procedure, public :: hour_start => dt_hour_start
    procedure, public :: minute_start => dt_minute_start
    procedure, public :: add => dt_add
    procedure, public :: sub => dt_sub
    procedure, private :: dt_copy_dt, dt_copy_d
    generic, public :: assignment(=) => dt_copy_dt, dt_copy_d
    procedure, private :: dt_eq, dt_eq_d
    generic, public :: operator(==) => dt_eq, dt_eq_d
    procedure, private :: dt_neq, dt_neq_d
    generic, public :: operator(/=) => dt_neq, dt_neq_d
    procedure, private :: dt_lt, dt_lt_d
    generic, public :: operator(<) => dt_lt, dt_lt_d
    procedure, private :: dt_gt, dt_gt_d
    generic, public :: operator(>) => dt_gt, dt_gt_d
    procedure, private :: dt_leq, dt_leq_d
    generic, public :: operator(<=) => dt_leq, dt_leq_d
    procedure, private :: dt_geq, dt_geq_d
    generic, public :: operator(>=) => dt_geq, dt_geq_d
    procedure, private :: dt_add_td
    procedure, pass(this), private :: td_add_dt
    generic, public :: operator(+) => dt_add_td, td_add_dt
    procedure, private :: dt_sub_td, dt_sub_dt, dt_sub_d
    generic, public :: operator(-) => dt_sub_td, dt_sub_dt, dt_sub_d
  end type datetime

  !> \class   timedelta
  !> \brief   This is a container to hold a defined time span.
  type timedelta
    integer(i4), public :: days = 0_i4                     !< days of the time-span
    integer(i4), public :: seconds = 0_i4                  !< second of the time-span
  contains
    procedure, public :: abs => td_abs
    procedure, public :: total_seconds => td_total_seconds
    procedure, private :: td_copy
    generic, public :: assignment(=) => td_copy
    procedure, private :: td_eq
    generic, public :: operator(==) => td_eq
    procedure, private :: td_neq
    generic, public :: operator(/=) => td_neq
    procedure, private :: td_lt
    generic, public :: operator(<) => td_lt
    procedure, private :: td_gt
    generic, public :: operator(>) => td_gt
    procedure, private :: td_leq
    generic, public :: operator(<=) => td_leq
    procedure, private :: td_geq
    generic, public :: operator(>=) => td_geq
    procedure, private :: td_add, td_pos
    generic, public :: operator(+) => td_add, td_pos
    procedure, private :: td_sub, td_neg
    generic, public :: operator(-) => td_sub, td_neg
    procedure, private :: td_mul1, td_mul1_dp
    procedure, pass(this), private :: td_mul2, td_mul2_dp
    generic, public :: operator(*) => td_mul1, td_mul2, td_mul1_dp, td_mul2_dp
    procedure, private :: td_div, td_div_dp, td_div_td
    generic, public :: operator(/) => td_div, td_div_dp, td_div_td
  end type timedelta

  ! constructor interface for date
  interface puredate
    procedure d_init
    procedure d_from_string
    procedure d_from_julian
  end interface puredate

  ! constructor interface for time
  interface puretime
    procedure t_init
    procedure t_from_string
    procedure t_from_day_second
  end interface puretime

  ! constructor interface for datetime
  interface datetime
    procedure dt_init
    procedure dt_from_string
    procedure dt_from_date_time
    procedure dt_from_cf
    procedure dt_from_julian
  end interface datetime

  ! constructor interface timedelta
  interface timedelta
    procedure td_init
  end interface timedelta

contains

  pure integer(i4) function default(component)
    integer(i4), intent(in), optional :: component
    if (present(component)) then
      default = component
    else
      default = 0_i4
    end if
  end function default

  !> \brief check datetime equality with components
  pure logical function datetime_eq(y1, y2, m1, m2, d1, d2, h1, h2, min1, min2, s1, s2)
    integer(i4), intent(in) :: y1, y2, m1, m2, d1, d2
    integer(i4), intent(in), optional :: h1, h2, min1, min2, s1, s2
    datetime_eq = .false.
    if (y1 /= y2) return
    if (m1 /= m2) return
    if (d1 /= d2) return
    ! assume 0 as default for time components
    if (default(h1) /= default(h2)) return
    if (default(min1) /= default(min2)) return
    if (default(s1) /= default(s2)) return
    datetime_eq = .true.
  end function datetime_eq

  !> \brief day of the year
  pure integer(i4) function date_to_doy(year, month, day)
    implicit none
    integer(i4), intent(in) :: year           !< 1 <= year <= 9999
    integer(i4), intent(in) :: month          !< 1 <= month <= 12
    integer(i4), intent(in) :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4) :: i
    date_to_doy = day
    do i=1_i4, month-1_i4
      date_to_doy = date_to_doy + days_in_month(year=year, month=i)
    end do
  end function date_to_doy

  !> \brief convert date to number of days since year 1
  pure integer(i4) function date_to_ordinal(year, month, day)
    implicit none
    integer(i4), intent(in) :: year           !< 1 <= year <= 9999
    integer(i4), intent(in) :: month          !< 1 <= month <= 12
    integer(i4), intent(in) :: day            !< 1 <= day <= number of days in the given month and year
    date_to_ordinal = days_before_year(year) + date_to_doy(year, month, day)
  end function date_to_ordinal

  !> \brief time to second of the day
  pure integer(i4) function day_second(hour, minute, second)
    implicit none
    integer(i4), intent(in) :: hour           !< 0 <= hour < 24
    integer(i4), intent(in) :: minute         !< 0 <= minute < 60
    integer(i4), intent(in) :: second         !< 0 <= second < 60
    day_second = hour * HOUR_SECONDS + minute * MINUTE_SECONDS + second
  end function day_second

  !> \brief get \ref timedelta from given string
  type(timedelta) function delta_from_string(str)
    character(*), intent(in) :: str !< "days", "hours", "minutes", "seconds"
    select case(trim(str))
      case("days")
        delta_from_string = td_init(days=1_i4)
      case("hours")
        delta_from_string = td_init(hours=1_i4)
      case("minutes")
        delta_from_string = td_init(minutes=1_i4)
      case("seconds")
        delta_from_string = td_init(seconds=1_i4)
      case default
        call error_message("datetime: units not valid for a cf-convention time. Got: ", str)
    end select
  end function delta_from_string

  !> \brief reference datetime and delta from cf-string for time units
  subroutine decode_cf_time_units(string, delta, ref_datetime)
    character(*), intent(in) :: string
    type(timedelta), intent(out) :: delta
    type(datetime), intent(out) :: ref_datetime
    type(puredate) :: ref_date
    type(puretime) :: ref_time
    character(256), dimension(:), allocatable :: str_arr
    call divide_string(trim(string), ' ', str_arr)
    delta = delta_from_string(str_arr(1))
    if (trim(str_arr(2)) /= "since") call error_message("datetime: expected 'since' for cf-convetion. Got: ", trim(str_arr(2)))
    ref_date = d_from_string(str_arr(3))
    ref_time = midnight()
    if(size(str_arr) > 3_i4) ref_time = t_from_string(str_arr(4))
    ref_datetime = dt_from_date_time(ref_date, ref_time)
  end subroutine decode_cf_time_units

  ! CONSTANT DELTAS

  !> \brief zero time delta
  pure type(timedelta) function zero_delta()
    zero_delta%days = 0_i4
    zero_delta%seconds = 0_i4
  end function zero_delta

  !> \brief one week time delta
  pure type(timedelta) function one_week()
    one_week%days = WEEK_DAYS
    one_week%seconds = 0_i4
  end function one_week

  !> \brief one day time delta
  pure type(timedelta) function one_day()
    one_day%days = 1_i4
    one_day%seconds = 0_i4
  end function one_day

  !> \brief one hour time delta
  pure type(timedelta) function one_hour()
    one_hour%days = 0_i4
    one_hour%seconds = HOUR_SECONDS
  end function one_hour

  !> \brief one minute time delta
  pure type(timedelta) function one_minute()
    one_minute%days = 0_i4
    one_minute%seconds = MINUTE_SECONDS
  end function one_minute

  !> \brief one second time delta
  pure type(timedelta) function one_second()
    one_second%days = 0_i4
    one_second%seconds = 1_i4
  end function one_second

  ! DAYTIMES

  !> \brief midnight (00:00)
  pure type(puretime) function midnight()
    midnight%hour = 0_i4
    midnight%minute = 0_i4
    midnight%second = 0_i4
  end function midnight

  !> \brief midday (12:00)
  pure type(puretime) function midday()
    midday%hour = CLOCK_HOURS
    midday%minute = 0_i4
    midday%second = 0_i4
  end function midday

  !> \brief time for given hour
  pure type(puretime) function day_hour(hour)
    integer(i4), intent(in) :: hour           !< hour
    day_hour%hour = modulo(hour, DAY_HOURS)
    day_hour%minute = 0_i4
    day_hour%second = 0_i4
  end function day_hour

  ! CURRENT TIME/DATE

  !> \brief get current \ref datetime
  type(datetime) function now()
    integer(i4) :: values(8)
    call date_and_time(values=values)
    now = dt_init(year=values(1), month=values(2), day=values(3), hour=values(5), minute=values(6), second=values(7))
  end function now

  !> \brief get todays \ref puredate
  type(puredate) function today()
    type(datetime) :: temp
    temp = now()
    today = temp%date()
  end function today

  !> \brief get current \ref puretime
  type(puretime) function currently()
    type(datetime) :: temp
    temp = now()
    currently = temp%time()
  end function currently

  !> \brief day of the week
  pure integer(i4) function weekday(year, month, day)
    implicit none
    integer(i4), intent(in) :: year           !< 1 <= year <= 9999
    integer(i4), intent(in) :: month          !< 1 <= month <= 12
    integer(i4), intent(in) :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4) :: year_j, year_k, mon, yea
    ! Zeller's congruence
    yea = year
    mon = month
    ! jan + feb are 13. and 14. month of previous year
    if (mon < 3_i4) then
      mon = mon + YEAR_MONTHS
      yea = yea - 1_i4
    end if
    year_j = yea / 100_i4
    year_k = mod(yea, 100_i4)
    weekday = mod(day + (13_i4*(mon+1_i4))/5_i4 + year_k + year_k/4_i4 + year_j/4_i4 + 5_i4*year_j, WEEK_DAYS)
    ! convert counting
    weekday = weekday - 1_i4
    if (weekday < 1_i4) weekday = weekday + WEEK_DAYS
  end function weekday

  !> \brief whether a given year is a leap year
  pure logical function is_leap_year(year)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    is_leap_year = mod(year, 4_i4) == 0_i4 .and. (mod(year, 100_i4) /= 0_i4 .or. mod(year, 400_i4) == 0_i4)
  end function is_leap_year

  !> \brief number of days in a given month
  pure integer(i4) function days_in_month(year, month)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    integer(i4), intent(in) :: month                    !< 1 <= month <= 12
    ! february is the special case
    if (month == 2_i4) then
      days_in_month = 28_i4
      if (is_leap_year(year)) days_in_month = 29_i4
      return
    end if
    ! even months before august (except february) and odd months from august on have 30 days, others 31
    if ((mod(month, 2) == 0 .and. month < 8) .or. (mod(month, 2) == 1 .and. month >= 8)) then
      days_in_month = 30_i4
    else
      days_in_month = 31_i4
    end if
  end function days_in_month

  !> \brief number of days in a given year
  pure integer(i4) function days_in_year(year)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    days_in_year = YEAR_DAYS
    if (is_leap_year(year)) days_in_year = LEAP_YEAR_DAYS
  end function days_in_year

  !> \brief number of days before a given year since year 1
  pure integer(i4) function days_before_year(year)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    integer(i4) :: y
    y = year - 1_i4
    days_before_year = y*YEAR_DAYS + y/4_i4 - y/100_i4 + y/400_i4
  end function days_before_year

  !> \brief get date from day of the year
  pure subroutine doy_to_month_day(year, doy, month, day)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    integer(i4), intent(in) :: doy                      !< 1 <= doy <= days_in_year (will be capped)
    integer(i4), intent(out), optional :: month         !< month for the given doy
    integer(i4), intent(out), optional :: day           !< day in month for the given doy
    integer(i4) :: i, dim, remain
    ! for pure function, we can't raise errors, so we force doy to be valid
    remain = min(max(doy, 1_i4), days_in_year(year))
    do i=1_i4, YEAR_MONTHS
      dim = days_in_month(year=year, month=i)
      if (remain <= dim) exit
      remain = remain - dim
    end do
    if (present(month)) month = i
    if (present(day)) day = remain
  end subroutine doy_to_month_day

  !> \brief get time from second of the day
  pure subroutine day_second_to_time(sod, hour, minute, second)
    implicit none
    integer(i4), intent(in) :: sod                      !< 0 <= sod < 86400
    integer(i4), intent(out), optional :: hour          !< 0 <= hour < 24
    integer(i4), intent(out), optional :: minute        !< 0 <= minute < 60
    integer(i4), intent(out), optional :: second        !< 0 <= second < 60
    integer(i4) :: remain_sec
    ! for pure function, we can't raise errors, so we force sod to be valid
    remain_sec = min(max(sod, 0_i4), DAY_SECONDS - 1_i4)
    if (present(hour)) hour = remain_sec / HOUR_SECONDS
    remain_sec = mod(remain_sec, HOUR_SECONDS)
    if (present(minute)) minute = remain_sec / MINUTE_SECONDS
    remain_sec = mod(remain_sec, MINUTE_SECONDS)
    if (present(second)) second = remain_sec
  end subroutine day_second_to_time

  !> \brief check if a given year is valid
  subroutine check_year(year)
    implicit none
    integer(i4), intent(in) :: year            !< 1 <= year <= 9999
    if (year < MIN_YEAR .or. year > MAX_YEAR) &
      call error_message("datetime: year is out of range. Got: ", num2str(year)) ! LCOV_EXCL_LINE
  end subroutine check_year

  !> \brief check if a given month is valid
  subroutine check_month(month)
    implicit none
    integer(i4), intent(in) :: month           !< 1 <= month <= 12
    if (month < 1_i4 .or. month > YEAR_MONTHS) &
      call error_message("datetime: month is out of range. Got: ", num2str(month)) ! LCOV_EXCL_LINE
  end subroutine check_month

  !> \brief check if a given day is valid
  subroutine check_day(year, month, day)
    implicit none
    integer(i4), intent(in) :: year           !< 1 <= year <= 9999
    integer(i4), intent(in) :: month          !< 1 <= month <= 12
    integer(i4), intent(in) :: day            !< 1 <= day <= number of days in the given month and year
    if (day < 1_i4 .or. day > days_in_month(year, month)) &
      call error_message("datetime: day is out of range. Got: ", num2str(day)) ! LCOV_EXCL_LINE
  end subroutine check_day

  !> \brief check if a given hour is valid
  subroutine check_hour(hour)
    implicit none
    integer(i4), intent(in), optional :: hour           !< 0 <= hour < 24
    if (hour < 0_i4 .or. hour >= DAY_HOURS) &
      call error_message("datetime: hour is out of range. Got: ", num2str(hour)) ! LCOV_EXCL_LINE
  end subroutine check_hour

  !> \brief check if a given minute is valid
  subroutine check_minute(minute)
    implicit none
    integer(i4), intent(in), optional :: minute         !< 0 <= minute < 60
    if (minute < 0_i4 .or. minute >= HOUR_MINUTES) &
      call error_message("datetime: minute is out of range. Got: ", num2str(minute)) ! LCOV_EXCL_LINE
  end subroutine check_minute

  !> \brief check if a given second is valid
  subroutine check_second(second)
    implicit none
    integer(i4), intent(in), optional :: second         !< 0 <= second < 60
    if (second < 0_i4 .or. second >= MINUTE_SECONDS) &
      call error_message("datetime: second is out of range. Got: ", num2str(second)) ! LCOV_EXCL_LINE
  end subroutine check_second

  !> \brief check if a datetime is valid
  subroutine check_datetime(year, month, day, hour, minute, second)
    implicit none
    integer(i4), intent(in), optional :: year           !< 1 <= year <= 9999
    integer(i4), intent(in), optional :: month          !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 0 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 0 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 0 <= second < 60
    ! sanity check for day
    if (present(day) .and. .not. (present(year) .and. present(month))) &
      call error_message("check_datetime: to validate a given 'day', 'year' and 'month' are required.") ! LCOV_EXCL_LINE
    ! check components
    if (present(year)) call check_year(year)
    if (present(month)) call check_month(month)
    if (present(day)) call check_day(year, month, day)
    if (present(hour)) call check_hour(hour)
    if (present(minute)) call check_minute(minute)
    if (present(second)) call check_second(second)
  end subroutine check_datetime

  ! DATETIME

  !> \brief initialize a datetime
  function dt_init(year, month, day, hour, minute, second) result(out)
    implicit none
    integer(i4), intent(in), optional :: year           !< 1 (default) <= year <= 9999
    integer(i4), intent(in), optional :: month          !< 1 (default) <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 (default) <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 0 (default) <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 0 (default) <= minute < 60
    integer(i4), intent(in), optional :: second         !< 0 (default) <= second < 60
    type(datetime) :: out
    out%year = 1_i4
    if (present(year)) out%year = year
    out%month = 1_i4
    if (present(month)) out%month = month
    out%day = 1_i4
    if (present(day)) out%day = day
    out%hour = 0_i4
    if (present(hour)) out%hour = hour
    out%minute = 0_i4
    if (present(minute)) out%minute = minute
    out%second = 0_i4
    if (present(second)) out%second = second
    ! check if datetime is valid
    call check_datetime(year=out%year, month=out%month, day=out%day, hour=out%hour, minute=out%minute, second=out%second)
  end function dt_init

  !> \brief datetime from string
  type(datetime) function dt_from_string(string)
    character(*), intent(in) :: string
    type(puredate) :: in_date
    type(puretime) :: in_time
    character(256), dimension(:), allocatable :: str_arr
    if ( index(string, "T") > 0 ) then
      call divide_string(trim(string), 'T', str_arr)
    else
      call divide_string(trim(string), ' ', str_arr)
    end if
    in_date = d_from_string(str_arr(1))
    in_time = midnight()
    if(size(str_arr) > 1_i4) in_time = t_from_string(str_arr(2))
    dt_from_string = dt_from_date_time(in_date, in_time)
  end function dt_from_string

  !> \brief datetime from cf-string and value
  type(datetime) function dt_from_cf(string, value)
    character(*), intent(in) :: string
    integer(i4), intent(in) :: value
    type(timedelta) :: delta
    call decode_cf_time_units(string, delta, dt_from_cf)
    dt_from_cf = dt_from_cf + value * delta
  end function dt_from_cf

  !> \brief datetime from date and time
  pure function dt_from_date_time(in_date, in_time) result(out)
    implicit none
    class(puredate), intent(in) :: in_date                !< date to use
    class(puretime), intent(in), optional :: in_time      !< time to use (midnight by default)
    type(datetime) :: out
    type(puretime) :: in_time_
    in_time_ = midnight()
    if (present(in_time)) in_time_ = in_time
    out%year = in_date%year
    out%month = in_date%month
    out%day = in_date%day
    out%hour = in_time_%hour
    out%minute = in_time_%minute
    out%second = in_time_%second
  end function dt_from_date_time

  !> \brief datetime from fractional julian day
  pure type(datetime) function dt_from_julian(julian, calendar)
    real(dp), intent(in) :: julian                !< fractional julian day
    integer(i4), intent(in), optional :: calendar !< The calendar to use, the global calendar will be used by default
    integer(i4) :: year, month, day, hour, minute, second
    call dec2date(julian, yy=year, mm=month, dd=day, hh=hour, nn=minute, ss=second, calendar=calendar)
    dt_from_julian%year = year
    dt_from_julian%month = month
    dt_from_julian%day = day
    dt_from_julian%hour = hour
    dt_from_julian%minute = minute
    dt_from_julian%second = second
  end function dt_from_julian

  !> \brief new datetime with specified fields
  type(datetime) function dt_replace(this, year, month, day, hour, minute, second)
    implicit none
    class(datetime), intent(in) :: this
    integer(i4), intent(in), optional :: year           !< 1 <= year <= 9999
    integer(i4), intent(in), optional :: month          !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 0 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 0 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 0 <= second < 60
    integer(i4) :: new_year, new_month, new_day, new_hour, new_minute, new_second
    new_year = this%year
    new_month = this%month
    new_day = this%day
    new_hour = this%hour
    new_minute = this%minute
    new_second = this%second
    if (present(year)) new_year = year
    if (present(month)) new_month = month
    if (present(day)) new_day = day
    if (present(hour)) new_hour = hour
    if (present(minute)) new_minute = minute
    if (present(second)) new_second = second
    dt_replace = dt_init(new_year, new_month, new_day, new_hour, new_minute, new_second)
  end function dt_replace

  !> \brief copy a datetime
  pure subroutine dt_copy_dt(this, that)
    implicit none
    class(datetime), intent(inout) :: this
    class(datetime), intent(in) :: that
    this%year = that%year
    this%month = that%month
    this%day = that%day
    this%hour = that%hour
    this%minute = that%minute
    this%second = that%second
  end subroutine dt_copy_dt

  !> \brief copy a datetime from a date
  pure subroutine dt_copy_d(this, that)
    implicit none
    class(datetime), intent(inout) :: this
    class(puredate), intent(in) :: that
    this%year = that%year
    this%month = that%month
    this%day = that%day
    this%hour = 0_i4
    this%minute = 0_i4
    this%second = 0_i4
  end subroutine dt_copy_d

  !> \brief date of the datetime
  pure type(puredate) function get_date(this)
    implicit none
    class(datetime), intent(in) :: this
    get_date%year = this%year
    get_date%month = this%month
    get_date%day = this%day
  end function get_date

  !> \brief time of the datetime
  pure type(puretime) function get_time(this)
    implicit none
    class(datetime), intent(in) :: this
    get_time%hour = this%hour
    get_time%minute = this%minute
    get_time%second = this%second
  end function get_time

  !> \brief string representation of the datetime
  pure character(19) function dt_str(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_str = d_str(this%date()) // " " // t_str(this%time())
  end function dt_str

  !> \brief datetime as fractional julian day
  pure real(dp) function dt_julian(this, calendar)
    implicit none
    class(datetime), intent(in) :: this
    integer(i4), intent(in), optional :: calendar !< The calendar to use, the global calendar will be used by default
    dt_julian = date2dec(yy=this%year, mm=this%month, dd=this%day, hh=this%hour, nn=this%minute, ss=this%second, calendar=calendar)
  end function dt_julian

  !> \brief day of the week
  pure integer(i4) function dt_weekday(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_weekday = weekday(this%year, this%month, this%day)
  end function dt_weekday

  !> \brief day of the year
  pure integer(i4) function dt_doy(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_doy = date_to_doy(this%year, this%month, this%day)
  end function dt_doy

  !> \brief convert date to number of days since year 1
  pure integer(i4) function dt_date_to_ordinal(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_date_to_ordinal = date_to_ordinal(this%year, this%month, this%day)
  end function dt_date_to_ordinal

  !> \brief time to second of the day
  pure integer(i4) function dt_day_second(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_day_second = day_second(this%hour, this%minute, this%second)
  end function dt_day_second

  !> \brief datetime is a new year
  pure logical function is_new_year(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_year = this%is_new_month() .and. this%month == 1_i4
  end function is_new_year

  !> \brief datetime is a new month
  pure logical function is_new_month(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_month = this%is_new_day() .and. this%day == 1_i4
  end function is_new_month

  !> \brief datetime is a new week
  pure logical function is_new_week(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_week = this%is_new_day() .and. this%weekday() == 1_i4
  end function is_new_week

  !> \brief datetime is a new day
  pure logical function is_new_day(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_day = this%is_new_hour() .and. this%hour == 0_i4
  end function is_new_day

  !> \brief datetime is a new hour
  pure logical function is_new_hour(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_hour = this%is_new_minute() .and. this%minute == 0_i4
  end function is_new_hour

  !> \brief datetime is a new month
  pure logical function is_new_minute(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_minute = this%second == 0_i4
  end function is_new_minute

  !> \brief next new year from this date
  type(datetime) function dt_next_new_year(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_next_new_year%year = this%year+1_i4
  end function dt_next_new_year

  !> \brief next new month from this date
  type(datetime) function dt_next_new_month(this)
    implicit none
    class(datetime), intent(in) :: this
    if (this%month == YEAR_MONTHS) then
      dt_next_new_month%year = this%year+1_i4
    else
      dt_next_new_month%year = this%year
      dt_next_new_month%month = this%month+1_i4
    end if
  end function dt_next_new_month

  !> \brief next new week from this date
  type(datetime) function dt_next_new_week(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_next_new_week%year = this%year
    dt_next_new_week%month = this%month
    dt_next_new_week%day = this%day
    dt_next_new_week = dt_next_new_week + (WEEK_DAYS - this%weekday() + 1_i4) * one_day()
  end function dt_next_new_week

  !> \brief next new day from this date
  type(datetime) function dt_next_new_day(this)
    implicit none
    class(datetime), intent(in) :: this
    if (this%day == days_in_month(this%year, this%month)) then
      if (this%month == YEAR_MONTHS) then
        dt_next_new_day%year = this%year + 1_i4
      else
        dt_next_new_day%month = this%month + 1_i4
        dt_next_new_day%year = this%year
      end if
    else
      dt_next_new_day%day = this%day + 1_i4
      dt_next_new_day%month = this%month
      dt_next_new_day%year = this%year
    end if
  end function dt_next_new_day

  !> \brief next new hour from this date
  type(datetime) function dt_next_new_hour(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_next_new_hour%year = this%year
    dt_next_new_hour%month = this%month
    dt_next_new_hour%day = this%day
    dt_next_new_hour%hour = this%hour
    dt_next_new_hour = dt_next_new_hour + one_hour()
  end function dt_next_new_hour

  !> \brief next new minute from this date
  type(datetime) function dt_next_new_minute(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_next_new_minute%year = this%year
    dt_next_new_minute%month = this%month
    dt_next_new_minute%day = this%day
    dt_next_new_minute%hour = this%hour
    dt_next_new_minute%minute = this%minute
    dt_next_new_minute = dt_next_new_minute + one_minute()
  end function dt_next_new_minute

  !> \brief previous new year from this date
  type(datetime) function dt_previous_new_year(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_previous_new_year%year = this%year-1_i4
  end function dt_previous_new_year

  !> \brief previous new month from this date
  type(datetime) function dt_previous_new_month(this)
    implicit none
    class(datetime), intent(in) :: this
    if (this%month == 1_i4) then
      dt_previous_new_month%year = this%year-1_i4
      dt_previous_new_month%month = YEAR_MONTHS
    else
      dt_previous_new_month%year = this%year
      dt_previous_new_month%month = this%month-1_i4
    end if
  end function dt_previous_new_month

  !> \brief previous new week from this date
  type(datetime) function dt_previous_new_week(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_previous_new_week = this%day_start() + (1_i4 - this%weekday() - WEEK_DAYS) * one_day()
  end function dt_previous_new_week

  !> \brief previous new day from this date
  type(datetime) function dt_previous_new_day(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_previous_new_day = this%day_start() - one_day()
  end function dt_previous_new_day

  !> \brief previous new hour from this time
  type(datetime) function dt_previous_new_hour(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_previous_new_hour = this%hour_start() - one_hour()
  end function dt_previous_new_hour

  !> \brief previous new minute from this time
  type(datetime) function dt_previous_new_minute(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_previous_new_minute = this%minute_start() - one_minute()
  end function dt_previous_new_minute

  !> \brief start of current year
  type(datetime) function dt_year_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_year_start%year = this%year
  end function dt_year_start

  !> \brief start of current month
  type(datetime) function dt_month_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_month_start%year = this%year
    dt_month_start%month = this%month
  end function dt_month_start

  !> \brief start of current week
  type(datetime) function dt_week_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_week_start = this%day_start() + (1_i4 - this%weekday()) * one_day()
  end function dt_week_start

  !> \brief start of current day
  type(datetime) function dt_day_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_day_start%year = this%year
    dt_day_start%month = this%month
    dt_day_start%day = this%day
  end function dt_day_start

  !> \brief start of current hour
  type(datetime) function dt_hour_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_hour_start%year = this%year
    dt_hour_start%month = this%month
    dt_hour_start%day = this%day
    dt_hour_start%hour = this%hour
  end function dt_hour_start

  !> \brief start of current minute
  type(datetime) function dt_minute_start(this)
    implicit none
    class(datetime), intent(in) :: this
    dt_minute_start%year = this%year
    dt_minute_start%month = this%month
    dt_minute_start%day = this%day
    dt_minute_start%hour = this%hour
    dt_minute_start%minute = this%minute
  end function dt_minute_start

  !> \brief add a timedelta to a datetime in-place
  subroutine dt_add(this, delta)
    implicit none
    class(datetime), intent(inout) :: this
    class(timedelta), intent(in) :: delta
    integer(i4) :: doy, sod, tmp_sec, tmp_day, new_sod, new_doy, new_year
    doy = this%doy()
    sod = this%day_second()
    tmp_sec = sod + delta%seconds
    tmp_day = tmp_sec / DAY_SECONDS
    new_sod = mod(tmp_sec, DAY_SECONDS)
    new_doy = doy + tmp_day + delta%days
    new_year = this%year
    do while(new_doy > days_in_year(new_year))
      new_doy = new_doy - days_in_year(new_year)
      new_year = new_year + 1_i4
    end do
    do while(new_doy < 1_i4)
      new_year = new_year - 1_i4
      new_doy = new_doy + days_in_year(new_year)
    end do
    ! update datetime
    this%year = new_year
    call doy_to_month_day(this%year, new_doy, this%month, this%day)
    call day_second_to_time(new_sod, this%hour, this%minute, this%second)
  end subroutine dt_add

  !> \brief subtract a timedelta from a datetime in-place
  subroutine dt_sub(this, delta)
    implicit none
    class(datetime), intent(inout) :: this
    class(timedelta), intent(in) :: delta
    integer(i4) :: doy, sod, new_sod, new_doy, new_year
    doy = this%doy()
    sod = this%day_second()
    if (delta%seconds > sod) then
      new_sod = sod - delta%seconds + DAY_SECONDS
      new_doy = doy - delta%days - 1_i4
    else
      new_sod = sod - delta%seconds
      new_doy = doy - delta%days
    end if
    new_year = this%year
    do while(new_doy > days_in_year(new_year))
      new_doy = new_doy - days_in_year(new_year)
      new_year = new_year + 1_i4
    end do
    do while(new_doy < 1_i4)
      new_year = new_year - 1_i4
      new_doy = new_doy + days_in_year(new_year)
    end do
    ! update datetime
    this%year = new_year
    call doy_to_month_day(this%year, new_doy, this%month, this%day)
    call day_second_to_time(new_sod, this%hour, this%minute, this%second)
  end subroutine dt_sub

  !> \brief (==) equal comparison of datetimes
  pure logical function dt_eq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_eq = datetime_eq( &
      this%year, that%year, this%month, that%month, this%day, that%day, &
      this%hour, that%hour, this%minute, that%minute, this%second, that%second)
  end function dt_eq

  !> \brief (==) equal comparison of datetime and date
  pure logical function dt_eq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    dt_eq_d = datetime_eq( &
      this%year, that%year, this%month, that%month, this%day, that%day, &
      this%hour, 0_i4, this%minute, 0_i4, this%second, 0_i4)
  end function dt_eq_d

  !> \brief (/=) not equal comparison of datetimes
  pure logical function dt_neq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_neq = .not. dt_eq(this, that)
  end function dt_neq

  !> \brief (/=) not equal comparison of datetime and date
  pure logical function dt_neq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    dt_neq_d = .not. dt_eq_d(this, that)
  end function dt_neq_d

  !> \brief (<) less than comparison of datetimes
  pure logical function dt_lt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    integer(i4) :: this_ordinal, that_ordinal
    this_ordinal = this%date_to_ordinal()
    that_ordinal = that%date_to_ordinal()
    dt_lt = this_ordinal < that_ordinal .or. (this_ordinal == that_ordinal .and. this%day_second() < that%day_second())
  end function dt_lt

  !> \brief (<) less than comparison of datetime and date
  pure logical function dt_lt_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    ! they need to be unequal
    dt_lt_d = this%date_to_ordinal() < that%to_ordinal()
  end function dt_lt_d

  !> \brief (>) greater than comparison of datetimes
  pure logical function dt_gt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_gt = dt_neq(this, that) .and. .not. dt_lt(this, that)
  end function dt_gt

  !> \brief (>) greater than comparison of datetime and date
  pure logical function dt_gt_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    integer(i4) :: this_ordinal, that_ordinal
    ! they need to be unequal
    this_ordinal = this%date_to_ordinal()
    that_ordinal = that%to_ordinal()
    dt_gt_d = this_ordinal > that_ordinal .or. (this_ordinal == that_ordinal .and. this%day_second() > 0_i4)
  end function dt_gt_d

  !> \brief (<=) less than or equal comparison of datetimes
  pure logical function dt_leq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_leq = dt_lt(this, that) .or. dt_eq(this, that)
  end function dt_leq

  !> \brief (<=) less than or equal comparison of datetime and date
  pure logical function dt_leq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    integer(i4) :: this_ordinal, that_ordinal
    this_ordinal = this%date_to_ordinal()
    that_ordinal = that%to_ordinal()
    dt_leq_d = this_ordinal < that_ordinal .or. (this_ordinal == that_ordinal .and. this%day_second() == 0_i4)
  end function dt_leq_d

  !> \brief (>=) greater than or equal comparison of datetimes
  pure logical function dt_geq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_geq = dt_gt(this, that) .or. dt_eq(this, that)
  end function dt_geq

  !> \brief (<=) less than or equal comparison of datetime and date
  pure logical function dt_geq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    dt_geq_d = this%date_to_ordinal() >= that%to_ordinal()
  end function dt_geq_d

  !> \brief (+) add a timedelta to a datetime
  pure type(datetime) function dt_add_td(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    type(timedelta) :: temp
    type(puredate) :: new_date
    type(puretime) :: new_time
    ! handle sub-day timing
    temp = td_init(days=that%days, seconds=this%second+that%seconds, minutes=this%minute, hours=this%hour)
    ! use date/time methods
    new_date = this%date() + temp
    new_time = t_from_day_second(temp%seconds)
    dt_add_td = dt_from_date_time(new_date, new_time)
  end function dt_add_td

  !> \brief (+) add a timedelta to a datetime
  pure type(datetime) function td_add_dt(that, this)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    td_add_dt = dt_add_td(this, that)
  end function td_add_dt

  !> \brief (-) subtract a timedelta from a datetime
  pure type(datetime) function dt_sub_td(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    dt_sub_td = this + (-that)
  end function dt_sub_td

  !> \brief (-) difference between two datetimes
  pure type(timedelta) function dt_sub_dt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    type(timedelta) :: tmp_this, tmp_that
    integer(i4) :: minyear, maxyear, days_this, days_that, day_year_diff, i
    minyear = min(this%year, that%year)
    maxyear = max(this%year, that%year)
    ! get year difference in days
    day_year_diff = 0_i4
    do i=minyear, maxyear-1_i4
      day_year_diff = day_year_diff + days_in_year(i)
    end do
    days_this = this%doy()
    days_that = that%doy()
    if (this%year < that%year) days_that = days_that + day_year_diff
    if (this%year > that%year) days_this = days_this + day_year_diff
    ! substract the differences of both dates to <min_year-1>-12-31
    tmp_this = timedelta(days=days_this, seconds=this%second, minutes=this%minute, hours=this%hour)
    tmp_that = timedelta(days=days_that, seconds=that%second, minutes=that%minute, hours=that%hour)
    dt_sub_dt = tmp_this - tmp_that
  end function dt_sub_dt

  !> \brief (-) difference between datetime and date
  pure type(timedelta) function dt_sub_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(puredate), intent(in) :: that
    dt_sub_d = dt_sub_dt(this, that%to_datetime())
  end function dt_sub_d

  ! DATE

  !> \brief initialize a date
  function d_init(year, month, day) result(out)
    implicit none
    integer(i4), intent(in), optional :: year                     !< 1 (default) <= year <= 9999
    integer(i4), intent(in), optional :: month                    !< 1 (default) <= month <= 12
    integer(i4), intent(in), optional :: day                      !< 1 (default) <= day <= number of days for given month and year
    type(puredate) :: out
    out%year = 1_i4
    if (present(year)) out%year = year
    out%month = 1_i4
    if (present(month)) out%month = month
    out%day = 1_i4
    if (present(day)) out%day = day
    call check_datetime(year=out%year, month=out%month, day=out%day)
  end function d_init

  !> \brief date from string
  type(puredate) function d_from_string(string)
    character(*), intent(in) :: string
    character(256), dimension(:), allocatable :: date_str
    integer(i4) :: year, month, day
    call divide_string(trim(string), '-', date_str)
    year = 1_i4
    month = 1_i4
    day = 1_i4
    if (len_trim(date_str(1)) > 0) read(date_str(1), *) year
    if (size(date_str) > 1) read(date_str(2), *) month
    if (size(date_str) > 2) read(date_str(3), *) day
    d_from_string = d_init(year=year, month=month, day=day)
  end function d_from_string

  !> \brief date from fractional julian day
  pure type(puredate) function d_from_julian(julian, calendar)
    real(dp), intent(in) :: julian                !< fractional julian day
    integer(i4), intent(in), optional :: calendar !< The calendar to use, the global calendar will be used by default
    integer(i4) :: year, month, day
    call dec2date(julian, yy=year, mm=month, dd=day, calendar=calendar)
    d_from_julian%year = year
    d_from_julian%month = month
    d_from_julian%day = day
  end function d_from_julian

  !> \brief new date with specified fields
  type(puredate) function d_replace(this, year, month, day)
    implicit none
    class(puredate), intent(in) :: this
    integer(i4), intent(in), optional :: year                     !< 1 <= year <= 9999
    integer(i4), intent(in), optional :: month                    !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day                      !< 1 <= day <= number of days in the given month and year
    integer(i4) :: new_year, new_month, new_day
    new_year = this%year
    new_month = this%month
    new_day = this%day
    if (present(year)) new_year = year
    if (present(month)) new_month = month
    if (present(day)) new_day = day
    d_replace = d_init(new_year, new_month, new_day)
  end function d_replace

  !> \brief convert date to a datetime
  pure type(datetime) function to_datetime(this)
    implicit none
    class(puredate), intent(in) :: this
    to_datetime = dt_from_date_time(this)
  end function to_datetime

  !> \brief convert date to number of days since year 1
  pure integer(i4) function to_ordinal(this)
    implicit none
    class(puredate), intent(in) :: this
    to_ordinal = date_to_ordinal(this%year, this%month, this%day)
  end function to_ordinal

  !> \brief string representation of the date
  pure character(10) function d_str(this)
    implicit none
    class(puredate), intent(in) :: this
    write(d_str, "(i4.4, '-' ,i2.2, '-', i2.2)") this%year, this%month, this%day
  end function d_str

  !> \brief date as fractional julian day
  pure real(dp) function d_julian(this, calendar)
    implicit none
    class(puredate), intent(in) :: this
    integer(i4), intent(in), optional :: calendar !< The calendar to use, the global calendar will be used by default
    d_julian = date2dec(yy=this%year, mm=this%month, dd=this%day, calendar=calendar)
  end function d_julian

  !> \brief day of the week
  pure integer(i4) function d_weekday(this)
    implicit none
    class(puredate), intent(in) :: this
    d_weekday = weekday(this%year, this%month, this%day)
  end function d_weekday

  !> \brief day of the year
  pure integer(i4) function d_doy(this)
    implicit none
    class(puredate), intent(in) :: this
    d_doy = date_to_doy(this%year, this%month, this%day)
  end function d_doy

  !> \brief date is a new year
  pure logical function d_is_new_year(this)
    implicit none
    class(puredate), intent(in) :: this
    d_is_new_year = this%is_new_month() .and. this%month == 1_i4
  end function d_is_new_year

  !> \brief date is a new month
  pure logical function d_is_new_month(this)
    implicit none
    class(puredate), intent(in) :: this
    d_is_new_month = this%day == 1_i4
  end function d_is_new_month

  !> \brief date is a new week
  pure logical function d_is_new_week(this)
    implicit none
    class(puredate), intent(in) :: this
    d_is_new_week = this%weekday() == 1_i4
  end function d_is_new_week

  !> \brief next new year from this date
  type(puredate) function d_next_new_year(this)
    implicit none
    class(puredate), intent(in) :: this
    d_next_new_year%year = this%year+1_i4
  end function d_next_new_year

  !> \brief next new month from this date
  type(puredate) function d_next_new_month(this)
    implicit none
    class(puredate), intent(in) :: this
    if (this%month == YEAR_MONTHS) then
      d_next_new_month%year = this%year+1_i4
    else
      d_next_new_month%year = this%year
      d_next_new_month%month = this%month+1_i4
    end if
  end function d_next_new_month

  !> \brief next new week from this date
  type(puredate) function d_next_new_week(this)
    implicit none
    class(puredate), intent(in) :: this
    d_next_new_week = this + (WEEK_DAYS - this%weekday() + 1_i4) * one_day()
  end function d_next_new_week

  !> \brief next new day from this date
  type(puredate) function d_next_new_day(this)
    implicit none
    class(puredate), intent(in) :: this
    if (this%day == days_in_month(this%year, this%month)) then
      if (this%month == YEAR_MONTHS) then
        d_next_new_day%year = this%year + 1_i4
      else
        d_next_new_day%month = this%month + 1_i4
        d_next_new_day%year = this%year
      end if
    else
      d_next_new_day%day = this%day + 1_i4
      d_next_new_day%month = this%month
      d_next_new_day%year = this%year
    end if
  end function d_next_new_day

  !> \brief previous new year from this date
  type(puredate) function d_previous_new_year(this)
    implicit none
    class(puredate), intent(in) :: this
    d_previous_new_year%year = this%year-1_i4
  end function d_previous_new_year

  !> \brief previous new month from this date
  type(puredate) function d_previous_new_month(this)
    implicit none
    class(puredate), intent(in) :: this
    if (this%month == 1_i4) then
      d_previous_new_month%year = this%year-1_i4
      d_previous_new_month%month = YEAR_MONTHS
    else
      d_previous_new_month%year = this%year
      d_previous_new_month%month = this%month - 1_i4
    end if
  end function d_previous_new_month

  !> \brief previous new week from this date
  type(puredate) function d_previous_new_week(this)
    implicit none
    class(puredate), intent(in) :: this
    d_previous_new_week = this + (1_i4 - this%weekday() - WEEK_DAYS) * one_day()
  end function d_previous_new_week

  !> \brief previous new day from this date
  type(puredate) function d_previous_new_day(this)
    implicit none
    class(puredate), intent(in) :: this
    d_previous_new_day = this - one_day()
  end function d_previous_new_day

  !> \brief previous new year from this date
  type(puredate) function d_year_start(this)
    implicit none
    class(puredate), intent(in) :: this
    d_year_start%year = this%year
  end function d_year_start

  !> \brief previous new month from this date
  type(puredate) function d_month_start(this)
    implicit none
    class(puredate), intent(in) :: this
    d_month_start%year = this%year
    d_month_start%month = this%month
  end function d_month_start

  !> \brief previous new week from this date
  type(puredate) function d_week_start(this)
    implicit none
    class(puredate), intent(in) :: this
    d_week_start = this + (1_i4 - this%weekday()) * one_day()
  end function d_week_start

  !> \brief (==) equal comparison of dates
  pure logical function d_eq(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_eq = this%to_ordinal() == that%to_ordinal()
  end function d_eq

  !> \brief (==) equal comparison of date and datetime
  pure logical function d_eq_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    d_eq_dt = datetime_eq( &
      this%year, that%year, this%month, that%month, this%day, that%day, &
      0_i4, that%hour, 0_i4, that%minute, 0_i4, that%second)
  end function d_eq_dt

  !> \brief (/=) not equal comparison of dates
  pure logical function d_neq(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_neq = .not. d_eq(this, that)
  end function d_neq

  !> \brief (/=) not equal comparison of date and datetime
  pure logical function d_neq_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    d_neq_dt = .not. d_eq_dt(this, that)
  end function d_neq_dt

  !> \brief (<) less than comparison of dates
  pure logical function d_lt(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_lt = this%to_ordinal() < that%to_ordinal()
  end function d_lt

  !> \brief (<) less than comparison of date and datetime
  pure logical function d_lt_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    integer(i4) :: this_ordinal, that_ordinal
    ! they need to be unequal
    this_ordinal = this%to_ordinal()
    that_ordinal = that%date_to_ordinal()
    d_lt_dt = this_ordinal < that_ordinal .or. (this_ordinal == that_ordinal .and. that%day_second() > 0_i4)
  end function d_lt_dt

  !> \brief (>) greater than comparison of dates
  pure logical function d_gt(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_gt = d_neq(this, that) .and. .not. d_lt(this, that)
  end function d_gt

  !> \brief (>) greater than comparison of date and datetime
  pure logical function d_gt_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    d_gt_dt = this%to_ordinal() > that%date_to_ordinal()
  end function d_gt_dt

  !> \brief (<=) less than or equal comparison of dates
  pure logical function d_leq(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_leq = d_lt(this, that) .or. d_eq(this, that)
  end function d_leq

  !> \brief (<=) less than or equal comparison of date and datetime
  pure logical function d_leq_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    d_leq_dt = this%to_ordinal() <= that%date_to_ordinal()
  end function d_leq_dt

  !> \brief (>=) greater than or equal comparison of dates
  pure logical function d_geq(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    d_geq = d_gt(this, that) .or. d_eq(this, that)
  end function d_geq

  !> \brief (>=) greater than or equal comparison of date and datetime
  pure logical function d_geq_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    integer(i4) :: this_ordinal, that_ordinal
    ! they need to be unequal
    this_ordinal = this%to_ordinal()
    that_ordinal = that%date_to_ordinal()
    d_geq_dt = this_ordinal > that_ordinal .or. (this_ordinal == that_ordinal .and. that%day_second() == 0_i4)
  end function d_geq_dt

  !> \brief (+) add a timedelta to a date
  pure type(puredate) function d_add_td(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(timedelta), intent(in) :: that
    integer(i4) :: new_year, new_month, new_day, day_delta, diy
    ! find the new year
    new_year = this%year
    day_delta = this%doy() + that%days
    if (day_delta > 0_i4) then
      do while (.true.)
        diy = days_in_year(new_year)
        if (day_delta <= diy) exit
        new_year = new_year + 1_i4
        day_delta = day_delta - diy
      end do
    else
      do while (.true.)
        new_year = new_year - 1_i4
        diy = days_in_year(new_year)
        day_delta = day_delta + diy
        if (day_delta > 0_i4) exit
      end do
    end if
    ! get date from new year and doy
    call doy_to_month_day(year=new_year, doy=day_delta, month=new_month, day=new_day)
    d_add_td%year = new_year
    d_add_td%month = new_month
    d_add_td%day = new_day
  end function d_add_td

  !> \brief (+) add a timedelta to a date
  pure type(puredate) function td_add_d(that, this)
    implicit none
    class(puredate), intent(in) :: this
    class(timedelta), intent(in) :: that
    td_add_d = d_add_td(this, that)
  end function td_add_d

  !> \brief (-) subtract a timedelta from a date
  pure type(puredate) function d_sub_td(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(timedelta), intent(in) :: that
    d_sub_td = this + (-that)
  end function d_sub_td

  !> \brief (-) difference between two dates
  pure type(timedelta) function d_sub_d(this, that)
    implicit none
    class(puredate), intent(in) :: this, that
    ! use datetime routine
    d_sub_d = this%to_datetime() - that%to_datetime()
  end function d_sub_d

  !> \brief (-) difference between date and datetime
  pure type(timedelta) function d_sub_dt(this, that)
    implicit none
    class(puredate), intent(in) :: this
    class(datetime), intent(in) :: that
    ! use datetime routine
    d_sub_dt = this%to_datetime() - that
  end function d_sub_dt

  ! TIME

  !> \brief initialize a time
  function t_init(hour, minute, second) result(out)
    implicit none
    integer(i4), intent(in) :: hour                     !< 0 <= hour < 24
    integer(i4), intent(in) :: minute                   !< 0 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 0 (default) <= second < 60
    type(puretime) :: out
    out%hour = hour
    out%minute = minute
    out%second = 0_i4
    if (present(second)) out%second = second
    ! check if datetime is valid
    call check_datetime(hour=out%hour, minute=out%minute, second=out%second)
  end function t_init

  !> \brief time from string
  type(puretime) function t_from_string(string)
    character(*), intent(in) :: string
    character(256), dimension(:), allocatable :: time_str
    integer(i4) :: hour, minute, second
    call divide_string(trim(string), ':', time_str)
    hour = 0_i4
    minute = 0_i4
    second = 0_i4
    if (len_trim(time_str(1)) > 0) read(time_str(1), *) hour
    if (size(time_str) > 1) read(time_str(2), *) minute
    if (size(time_str) > 2) read(time_str(3), *) second
    t_from_string = t_init(hour=hour, minute=minute, second=second)
  end function t_from_string

  !> \brief time from day second
  pure type(puretime) function t_from_day_second(day_second)
    implicit none
    integer(i4), intent(in) :: day_second !< second of the day (will be capped)
    integer(i4) :: temp_seconds
    ! cap second for pure function (no error raise possible)
    temp_seconds = min(max(day_second, 0_i4), DAY_SECONDS-1_i4)
    ! calculate hour, minute and second
    t_from_day_second%hour = temp_seconds / HOUR_SECONDS
    temp_seconds = mod(temp_seconds, HOUR_SECONDS)
    t_from_day_second%minute = temp_seconds / MINUTE_SECONDS
    t_from_day_second%second = mod(temp_seconds, MINUTE_SECONDS)
  end function t_from_day_second

  !> \brief copy a time
  pure subroutine t_copy(this, that)
    implicit none
    class(puretime), intent(inout) :: this
    class(puretime), intent(in) :: that
    this%hour = that%hour
    this%minute = that%minute
    this%second = that%second
  end subroutine t_copy

  !> \brief new time with specified fields
  type(puretime) function t_replace(this, hour, minute, second)
    implicit none
    class(puretime), intent(in) :: this
    integer(i4), intent(in), optional :: hour           !< 0 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 0 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 0 <= second < 60
    integer(i4) :: new_hour, new_minute, new_second
    new_hour = this%hour
    new_minute = this%minute
    new_second = this%second
    if (present(hour)) new_hour = hour
    if (present(minute)) new_minute = minute
    if (present(second)) new_second = second
    t_replace = t_init(new_hour, new_minute, new_second)
  end function t_replace

  !> \brief string representation of the time
  pure character(8) function t_str(this)
    implicit none
    class(puretime), intent(in) :: this
    write(t_str, "(i2.2, ':', i2.2, ':', i2.2)") this%hour, this%minute, this%second
  end function t_str

  !> \brief time to second of the day
  pure integer(i4) function t_day_second(this)
    implicit none
    class(puretime), intent(in) :: this
    t_day_second = day_second(this%hour, this%minute, this%second)
  end function t_day_second

  !> \brief time is a new day / midnight
  pure logical function t_is_new_day(this)
    implicit none
    class(puretime), intent(in) :: this
    t_is_new_day = this%is_new_hour() .and. this%hour == 0_i4
  end function t_is_new_day

  !> \brief time is a new hour
  pure logical function t_is_new_hour(this)
    implicit none
    class(puretime), intent(in) :: this
    t_is_new_hour = this%is_new_minute() .and. this%minute == 0_i4
  end function t_is_new_hour

  !> \brief time is a new month
  pure logical function t_is_new_minute(this)
    implicit none
    class(puretime), intent(in) :: this
    t_is_new_minute = this%second == 0_i4
  end function t_is_new_minute

  !> \brief next new hour from this time
  type(puretime) function t_next_new_hour(this)
    implicit none
    class(puretime), intent(in) :: this
    t_next_new_hour%hour = mod(this%hour + 1_i4, DAY_HOURS)
  end function t_next_new_hour

  !> \brief next new minute from this time
  type(puretime) function t_next_new_minute(this)
    implicit none
    class(puretime), intent(in) :: this
    t_next_new_minute%hour = this%hour + (this%minute + 1_i4) / HOUR_MINUTES
    t_next_new_minute%minute = mod(this%minute + 1_i4, HOUR_MINUTES)
  end function t_next_new_minute

  !> \brief previous new hour from this time
  type(puretime) function t_previous_new_hour(this)
    implicit none
    class(puretime), intent(in) :: this
    t_previous_new_hour%hour = modulo(this%hour - 1_i4, DAY_HOURS)
  end function t_previous_new_hour

  !> \brief previous new minute from this time
  type(puretime) function t_previous_new_minute(this)
    implicit none
    class(puretime), intent(in) :: this
    t_previous_new_minute%minute = modulo(this%minute - 1_i4, HOUR_MINUTES)
    if (this%minute == 0_i4) then
      t_previous_new_minute%hour = modulo(this%hour - 1_i4, DAY_HOURS)
    else
      t_previous_new_minute%hour = this%hour
    end if
  end function t_previous_new_minute

  !> \brief start of current hour
  type(puretime) function t_hour_start(this)
    implicit none
    class(puretime), intent(in) :: this
    t_hour_start%hour = this%hour
  end function t_hour_start

  !> \brief start of current minute
  type(puretime) function t_minute_start(this)
    implicit none
    class(puretime), intent(in) :: this
    t_minute_start%hour = this%hour
    t_minute_start%minute = this%minute
  end function t_minute_start

  !> \brief (==) equal comparison of times
  pure logical function t_eq(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_eq = this%day_second() == that%day_second()
  end function t_eq

  !> \brief (/=) not equal comparison of times
  pure logical function t_neq(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_neq = .not. t_eq(this, that)
  end function t_neq

  !> \brief (<) less than comparison of times
  pure logical function t_lt(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_lt = this%day_second() < that%day_second()
  end function t_lt

  !> \brief (>) greater than comparison of times
  pure logical function t_gt(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_gt = this%day_second() > that%day_second()
  end function t_gt

  !> \brief (<=) less than or equal comparison of times
  pure logical function t_leq(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_leq = this%day_second() <= that%day_second()
  end function t_leq

  !> \brief (>=) greater than or equal comparison of times
  pure logical function t_geq(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    t_geq = this%day_second() >= that%day_second()
  end function t_geq

  !> \brief (+) add a timedelta to a time
  pure type(puretime) function t_add_td(this, that)
    implicit none
    class(puretime), intent(in) :: this
    class(timedelta), intent(in) :: that
    ! ignore days in timedelta and do a module 24h
    t_add_td = t_from_day_second(int(modulo(int(this%day_second(), i8) + that%total_seconds(), int(DAY_SECONDS, i8)), i4))
  end function t_add_td

  !> \brief (+) add a timedelta to a time
  pure type(puretime) function td_add_t(that, this)
    implicit none
    class(puretime), intent(in) :: this
    class(timedelta), intent(in) :: that
    td_add_t = t_add_td(this, that)
  end function td_add_t

  !> \brief (-) subtract a timedelta from a time
  pure type(puretime) function t_sub_td(this, that)
    implicit none
    class(puretime), intent(in) :: this
    class(timedelta), intent(in) :: that
    t_sub_td = this + (-that)
  end function t_sub_td

  !> \brief (-) difference between two times
  pure type(timedelta) function t_sub_t(this, that)
    implicit none
    class(puretime), intent(in) :: this, that
    ! use datetime routine
    t_sub_t = timedelta(seconds=this%day_second() - that%day_second())
  end function t_sub_t

  ! TIMEDELTA

  !> \brief initialize a timedelta
  pure function td_init(days, seconds, minutes, hours, weeks) result(out)
    implicit none
    integer(i4), intent(in), optional :: days           !< days defining time-span
    integer(i4), intent(in), optional :: seconds        !< seconds defining time-span
    integer(i4), intent(in), optional :: minutes        !< minutes defining time-span
    integer(i4), intent(in), optional :: hours          !< hours defining time-span
    integer(i4), intent(in), optional :: weeks          !< weeks defining time-span
    type(timedelta) :: out
    integer(i4) :: neg_days, remain_sec

    if (present(days)) out%days = days
    if (present(weeks)) out%days = out%days + weeks * WEEK_DAYS
    if (present(seconds)) out%seconds = seconds
    if (present(minutes)) out%seconds = out%seconds + minutes * MINUTE_SECONDS
    if (present(hours)) out%seconds = out%seconds + hours * HOUR_SECONDS

    ! force: 0 <= seconds < 86400
    if (out%seconds < 0) then
      neg_days = abs(out%seconds) / DAY_SECONDS
      remain_sec = mod(abs(out%seconds), DAY_SECONDS)
      ! add full days in negative seconds
      out%seconds = out%seconds + neg_days * DAY_SECONDS
      out%days = out%days - neg_days
      ! add one days to remaining seconds if still negative
      if (remain_sec > 0) then
        out%seconds = out%seconds + DAY_SECONDS
        out%days = out%days - 1_i4
      end if
    end if
    if (out%seconds >= DAY_SECONDS) then
      neg_days = out%seconds / DAY_SECONDS
      out%seconds = out%seconds - neg_days * DAY_SECONDS
      out%days = out%days + neg_days
    end if
  end function td_init

  !> \brief absolute timedelta
  pure type(timedelta) function td_abs(this)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4) :: days, seconds
    if (this%days < 0_i4) then
      days = -this%days
      seconds = -this%seconds
    else
      days = this%days
      seconds = this%seconds
    end if
    td_abs = timedelta(days=days, seconds=seconds)
  end function td_abs

  !> \brief timedelta in seconds (may need i8)
  pure integer(i8) function td_total_seconds(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_total_seconds = int(this%days, i8) * int(DAY_SECONDS, i8) + int(this%seconds, i8)
  end function td_total_seconds

  pure type(timedelta) function from_total_seconds(total_seconds)
    integer(i8), intent(in) :: total_seconds
    integer(i8) :: daysec
    daysec = int(DAY_SECONDS, i8)
    from_total_seconds = timedelta(days=int(total_seconds / daysec, i4), seconds=int(mod(total_seconds, daysec), i4))
  end function from_total_seconds

  !> \brief copy a timedelta
  pure subroutine td_copy(this, that)
    implicit none
    class(timedelta), intent(inout) :: this
    class(timedelta), intent(in) :: that
    this%days = that%days
    this%seconds = that%seconds
  end subroutine td_copy

  !> \brief (==) equal comparison of timedeltas
  pure logical function td_eq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_eq = this%total_seconds() == that%total_seconds()
  end function td_eq

  !> \brief (/=) not equal comparison of timedeltas
  pure logical function td_neq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_neq = .not. td_eq(this, that)
  end function td_neq

  !> \brief (<) less than comparison of timedeltas
  pure logical function td_lt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_lt = this%total_seconds() < that%total_seconds()
  end function td_lt

  !> \brief (>) greater than comparison of timedeltas
  pure logical function td_gt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_gt = this%total_seconds() > that%total_seconds()
  end function td_gt

  !> \brief (<=) less than or equal comparison of timedeltas
  pure logical function td_leq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_leq = this%total_seconds() <= that%total_seconds()
  end function td_leq

  !> \brief (>=) greater than or equal comparison of timedeltas
  pure logical function td_geq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_geq = this%total_seconds() >= that%total_seconds()
  end function td_geq

  !> \brief (+) adding two timedeltas
  pure type(timedelta) function td_add(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_add = timedelta(days=this%days+that%days, seconds=this%seconds+that%seconds)
  end function td_add

  !> \brief (+) adding two timedeltas
  pure type(timedelta) function td_sub(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_sub = timedelta(days=this%days-that%days, seconds=this%seconds-that%seconds)
  end function td_sub

  !> \brief (-) negative timedelta
  pure type(timedelta) function td_neg(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_neg = timedelta(days=-this%days, seconds=-this%seconds)
  end function td_neg

  !> \brief (+) positive timedelta
  pure type(timedelta) function td_pos(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_pos = this
  end function td_pos

  !> \brief (*) multiply a timedelta with an integer
  pure type(timedelta) function td_mul1(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul1 = timedelta(days=this%days*that, seconds=this%seconds*that)
  end function td_mul1

  !> \brief (*) multiply a timedelta with an integer
  pure type(timedelta) function td_mul2(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul2 = td_mul1(this, that)
  end function td_mul2

  !> \brief (*) multiply a timedelta with a real
  pure type(timedelta) function td_mul1_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul1_dp = from_total_seconds(int(this%total_seconds() * that, i8))
  end function td_mul1_dp

  !> \brief (*) multiply a timedelta with a real
  pure type(timedelta) function td_mul2_dp(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul2_dp = td_mul1_dp(this, that)
  end function td_mul2_dp

  !> \brief (/) divide a timedelta by an integer
  pure type(timedelta) function td_div(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_div = from_total_seconds(this%total_seconds() / int(that, i8))
  end function td_div

  !> \brief (/) divide a timedelta by a real
  pure type(timedelta) function td_div_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_div_dp = this * (1.0_dp / that)
  end function td_div_dp

  !> \brief (/) divide a timedelta by a timedelta
  pure real(dp) function td_div_td(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_div_td = real(this%total_seconds(), dp) / real(that%total_seconds(), dp)
  end function td_div_td

end module mo_datetime
