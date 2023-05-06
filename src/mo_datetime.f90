!> \file    mo_datetime.f90
!> \brief   \copybrief mo_datetime
!> \details \copydetails mo_datetime

!> \brief   Types to deal with datetimes.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Apr 2023
!> \copyright Copyright 2005-\today, the mHM Developers, Luis Samaniego, Sabine Attinger: All rights reserved.
!! mHM is released under the LGPLv3+ license \license_note
module mo_datetime

  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_string_utils, only : num2str

  implicit none

  public :: is_leap_year, days_in_month, days_in_year

  public :: date
  public :: time
  public :: datetime
  public :: timedelta
  public :: today
  public :: now
  public :: zero_delta
  public :: one_day
  public :: one_hour
  public :: one_minute
  public :: one_second
  public :: one_week

  private

  integer(i4), parameter, public :: YEAR_DAYS = 365_i4 !< days in standard year
  integer(i4), parameter, public :: LEAP_YEAR_DAYS = 366_i4 !< days in leap year
  integer(i4), parameter, public :: YEAR_MONTHS = 12_i4 !< months in year
  integer(i4), parameter, public :: WEEK_DAYS = 7_i4 !< days in week
  integer(i4), parameter, public :: DAY_HOURS = 24_i4 !< hours in day
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

  !> \class   date
  !> \brief   This is a container to hold a date.
  type date
    integer(i4), public :: year                     !< 1 <= year <= 9999
    integer(i4), public :: month                    !< 1 <= month <= 12
    integer(i4), public :: day                      !< 1 <= day <= number of days in the given month and year
  contains
    !> \copydoc mo_datetime::d_replace
    procedure, public :: replace => d_replace !< \see mo_datetime::d_replace
    !> \copydoc mo_datetime::to_datetime
    procedure, public :: to_datetime !< \see mo_datetime::to_datetime
    !> \copydoc mo_datetime::d_str
    procedure, public :: str => d_str !< \see mo_datetime::d_str
    !> \copydoc mo_datetime::d_weekday
    procedure, public :: weekday => d_weekday !< \see mo_datetime::d_weekday
    !> \copydoc mo_datetime::d_doy
    procedure, public :: doy => d_doy !< \see mo_datetime::d_doy
    !> \copydoc mo_datetime::is_new_year
    procedure, public :: is_new_year => d_is_new_year !< \see mo_datetime::is_new_year
    !> \copydoc mo_datetime::is_new_month
    procedure, public :: is_new_month => d_is_new_month !< \see mo_datetime::is_new_month
    !> \copydoc mo_datetime::is_new_week
    procedure, public :: is_new_week => d_is_new_week !< \see mo_datetime::is_new_week
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
  end type date

  !> \class   time
  !> \brief   This is a container to hold a time.
  type time
    integer(i4), public :: hour                     !< 1 <= hour < 24
    integer(i4), public :: minute                   !< 1 <= minute < 60
    integer(i4), public :: second                   !< 1 <= second < 60
  contains
    !> \copydoc mo_datetime::t_replace
    procedure, public :: replace => t_replace !< \see mo_datetime::t_replace
    !> \copydoc mo_datetime::d_str
    procedure, public :: str => t_str !< \see mo_datetime::t_str
    !> \copydoc mo_datetime::day_second
    procedure, public :: day_second !< \see mo_datetime::day_second
    !> \copydoc mo_datetime::t_is_new_day
    procedure, public :: is_midnight => t_is_new_day !< \see mo_datetime::t_is_new_day
    !> \copydoc mo_datetime::t_is_new_day
    procedure, public :: is_new_day => t_is_new_day !< \see mo_datetime::t_is_new_day
    !> \copydoc mo_datetime::t_is_new_hour
    procedure, public :: is_new_hour => t_is_new_hour !< \see mo_datetime::t_is_new_hour
    !> \copydoc mo_datetime::t_is_new_minute
    procedure, public :: is_new_minute => t_is_new_minute !< \see mo_datetime::t_is_new_minute
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
  end type time

  !> \class   datetime
  !> \brief   This is a container to hold a date-time.
  type datetime
    integer(i4), public :: year                     !< 1 <= year <= 9999
    integer(i4), public :: month                    !< 1 <= month <= 12
    integer(i4), public :: day                      !< 1 <= day <= number of days in the given month and year
    integer(i4), public :: hour                     !< 1 <= hour < 24
    integer(i4), public :: minute                   !< 1 <= minute < 60
    integer(i4), public :: second                   !< 1 <= second < 60
  contains
    !> \copydoc mo_datetime::dt_replace
    procedure, public :: replace => dt_replace !< \see mo_datetime::dt_replace
    !> \copydoc mo_datetime::get_date
    procedure, public :: date => get_date !< \see mo_datetime::get_date
    !> \copydoc mo_datetime::get_time
    procedure, public :: time => get_time !< \see mo_datetime::get_time
    !> \copydoc mo_datetime::dt_str
    procedure, public :: str => dt_str !< \see mo_datetime::dt_str
    !> \copydoc mo_datetime::dt_weekday
    procedure, public :: weekday => dt_weekday !< \see mo_datetime::dt_weekday
    !> \copydoc mo_datetime::dt_doy
    procedure, public :: doy => dt_doy !< \see mo_datetime::dt_doy
    !> \copydoc mo_datetime::is_new_year
    procedure, public :: is_new_year !< \see mo_datetime::is_new_year
    !> \copydoc mo_datetime::is_new_month
    procedure, public :: is_new_month !< \see mo_datetime::is_new_month
    !> \copydoc mo_datetime::is_new_week
    procedure, public :: is_new_week !< \see mo_datetime::is_new_week
    !> \copydoc mo_datetime::is_new_day
    procedure, public :: is_new_day !< \see mo_datetime::is_new_day
    !> \copydoc mo_datetime::is_new_hour
    procedure, public :: is_new_hour !< \see mo_datetime::is_new_hour
    !> \copydoc mo_datetime::is_new_minute
    procedure, public :: is_new_minute !< \see mo_datetime::is_new_minute
    procedure, private :: dt_copy_dt, dt_copy_d
    generic :: assignment(=) => dt_copy_dt, dt_copy_d
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
    integer(i4), public :: days                     !< days of the time-span
    integer(i4), public :: seconds                  !< second of the time-span
  contains
    !> \copydoc mo_datetime::td_abs
    procedure, public :: abs => td_abs !< \see mo_datetime::td_abs
    !> \copydoc mo_datetime::td_total_seconds
    procedure, public :: total_seconds => td_total_seconds !< \see mo_datetime::td_total_seconds
    procedure, private :: td_copy
    generic :: assignment(=) => td_copy
    procedure, private :: td_eq
    generic :: operator(==) => td_eq
    procedure, private :: td_neq
    generic :: operator(/=) => td_neq
    procedure, private :: td_lt
    generic :: operator(<) => td_lt
    procedure, private :: td_gt
    generic :: operator(>) => td_gt
    procedure, private :: td_leq
    generic :: operator(<=) => td_leq
    procedure, private :: td_geq
    generic :: operator(>=) => td_geq
    procedure, private :: td_add
    procedure, private :: td_pos
    generic :: operator(+) => td_add, td_pos
    procedure, private :: td_sub
    procedure, private :: td_neg
    generic :: operator(-) => td_sub, td_neg
    procedure, private :: td_mul1
    procedure, pass(this), private :: td_mul2
    procedure, private :: td_mul1_dp
    procedure, pass(this), private :: td_mul2_dp
    generic, public :: operator(*) => td_mul1, td_mul2, td_mul1_dp, td_mul2_dp
    procedure, private :: td_div
    procedure, private :: td_div_dp
    procedure, private :: td_div_td
    generic, public :: operator(/) => td_div, td_div_dp, td_div_td
  end type timedelta

  !> \class   timedelta_c
  !> \brief   This is a container to hold a constant time span.
  type, extends(timedelta) :: timedelta_c
  end type timedelta_c

  ! intel fortran compiler can't use type with interface to construct parameter variables
  type(timedelta_c), parameter :: zero_delta = timedelta_c(0_i4, 0_i4)            !< zero time delta
  type(timedelta_c), parameter :: one_week = timedelta_c(WEEK_DAYS, 0_i4)         !< one week time delta
  type(timedelta_c), parameter :: one_day = timedelta_c(1_i4, 0_i4)               !< one day time delta
  type(timedelta_c), parameter :: one_hour = timedelta_c(0_i4, HOUR_SECONDS)      !< one hour time delta
  type(timedelta_c), parameter :: one_minute = timedelta_c(0_i4, MINUTE_SECONDS)  !< one minute time delta
  type(timedelta_c), parameter :: one_second = timedelta_c(0_i4, 1_i4)            !< one second time delta

  ! constructor interface for date
  interface date
    procedure init_date
    procedure date_from_string
  end interface date

  ! constructor interface for time
  interface time
    procedure init_time
    procedure time_from_string
  end interface time

  ! constructor interface for datetime
  interface datetime
    procedure init_datetime
    procedure datetime_from_string
  end interface datetime

  ! constructor interface timedelta
  interface timedelta
    procedure init_timedelta
  end interface timedelta

contains

  !> \brief get current datetime
  type(datetime) function now()
    integer(i4) :: values(8)
    call date_and_time(values=values)
    now = datetime(year=values(1), month=values(2), day=values(3), hour=values(5), minute=values(6), second=values(7))
  end function now

  !> \brief get current date
  type(date) function today()
    type(datetime) :: temp
    temp = now()
    today = temp%date()
  end function today

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

  !> \brief number of days in a given year
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
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    if (hour < 0_i4 .or. hour >= DAY_HOURS) &
      call error_message("datetime: hour is out of range. Got: ", num2str(hour)) ! LCOV_EXCL_LINE
  end subroutine check_hour

  !> \brief check if a given minute is valid
  subroutine check_minute(minute)
    implicit none
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    if (minute < 0_i4 .or. minute >= HOUR_MINUTES) &
      call error_message("datetime: minute is out of range. Got: ", num2str(minute)) ! LCOV_EXCL_LINE
  end subroutine check_minute

  !> \brief check if a given second is valid
  subroutine check_second(second)
    implicit none
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
    if (second < 0_i4 .or. second >= MINUTE_SECONDS) &
      call error_message("datetime: second is out of range. Got: ", num2str(second)) ! LCOV_EXCL_LINE
  end subroutine check_second

  !> \brief check if a datetime is valid
  subroutine check_datetime(year, month, day, hour, minute, second)
    implicit none
    integer(i4), intent(in), optional :: year           !< 1 <= year <= 9999
    integer(i4), intent(in), optional :: month          !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
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
  function init_datetime(year, month, day, hour, minute, second) result(out)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    integer(i4), intent(in) :: month                    !< 1 <= month <= 12
    integer(i4), intent(in) :: day                      !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
    type(datetime) :: out

    out%year = year
    out%month = month
    out%day = day
    out%hour = 0_i4
    if (present(hour)) out%hour = hour
    out%minute = 0_i4
    if (present(minute)) out%minute = minute
    out%second = 0_i4
    if (present(second)) out%second = second
    ! check if datetime is valid
    call check_datetime(year=out%year, month=out%month, day=out%day, hour=out%hour, minute=out%minute, second=out%second)
  end function init_datetime

  !> \brief datetime from string
  type(datetime) function datetime_from_string(string)
    use mo_string_utils, only : divide_string
    character(*), intent(in) :: string
    character(256), dimension(:), allocatable :: str_arr, date_str, time_str
    integer(i4) :: year, month, day, hour, minute, second
    call divide_string(trim(string), ' ', str_arr)
    ! determine reference time at '-' and convert to integer
    call divide_string(trim(str_arr(1)), '-', date_str)
    read(date_str(1), *) year
    read(date_str(2), *) month
    read(date_str(3), *) day
    ! if existing also read in the time
    hour = 0_i4
    minute = 0_i4
    second = 0_i4
    if(size(str_arr) > 1_i4) then
      call divide_string(trim(str_arr(2)), ':', time_str)
      read(time_str(1), *) hour
      read(time_str(2), *) minute
      read(time_str(3), *) second
    end if
    datetime_from_string = datetime(year=year, month=month, day=day, hour=hour, minute=minute, second=second)
  end function datetime_from_string

  !> \brief new datetime with specified fields
  type(datetime) function dt_replace(this, year, month, day, hour, minute, second)
    implicit none
    class(datetime), intent(in) :: this
    integer(i4), intent(in), optional :: year           !< 1 <= year <= 9999
    integer(i4), intent(in), optional :: month          !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
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
    dt_replace = datetime(new_year, new_month, new_day, new_hour, new_minute, new_second)
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
    class(date), intent(in) :: that
    this%year = that%year
    this%month = that%month
    this%day = that%day
    this%hour = 0_i4
    this%minute = 0_i4
    this%second = 0_i4
  end subroutine dt_copy_d

  !> \brief date of the datetime
  pure type(date) function get_date(this)
    implicit none
    class(datetime), intent(in) :: this
    get_date%year = this%year
    get_date%month = this%month
    get_date%day = this%day
  end function get_date

  !> \brief time of the datetime
  pure type(time) function get_time(this)
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
    write(dt_str, "(i4.4, '-' ,i2.2, '-', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2)") &
      this%year, this%month, this%day, this%hour, this%minute, this%second
  end function dt_str

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
    dt_doy = d_doy(this%date())
  end function dt_doy

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

  !> \brief equal comparison of datetimes
  pure logical function dt_eq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_eq = this%year == that%year &
      .and. this%month == that%month &
      .and. this%day == that%day &
      .and. this%hour == that%hour &
      .and. this%minute == that%minute &
      .and. this%second == that%second
  end function dt_eq

  !> \brief equal comparison of datetime and date
  pure logical function dt_eq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    dt_eq_d = dt_eq(this, that%to_datetime())
  end function dt_eq_d

  !> \brief not equal comparison of datetimes
  pure logical function dt_neq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_neq = .not. dt_eq(this, that)
  end function dt_neq

  !> \brief not equal comparison of datetime and date
  pure logical function dt_neq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    dt_neq_d = .not. dt_eq_d(this, that)
  end function dt_neq_d

  !> \brief less than comparison of datetimes
  pure logical function dt_lt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    ! they need to be unequal
    dt_lt = dt_neq(this, that)
    if (.not. dt_lt) return
    ! now check each component from biggest to smallest
    if (this%year < that%year) return ! true
    if (this%year > that%year) then
      dt_lt = .false.
      return
    endif
    ! year equal
    if (this%month < that%month) return ! true
    if (this%month > that%month) then
      dt_lt = .false.
      return
    endif
    ! year+month equal
    if (this%day < that%day) return ! true
    if (this%day > that%day) then
      dt_lt = .false.
      return
    endif
    ! year+month+day equal
    if (this%hour < that%hour) return ! true
    if (this%hour > that%hour) then
      dt_lt = .false.
      return
    endif
    ! year+month+day+hour equal
    if (this%minute < that%minute) return ! true
    if (this%minute > that%minute) then
      dt_lt = .false.
      return
    endif
    ! year+month+day+hour+minute equal
    if (this%second < that%second) return ! true
    if (this%second > that%second) then
      dt_lt = .false.
      return
    endif
    ! they can't be all equal since that was checked first
  end function dt_lt

  !> \brief less than comparison of datetime and date
  pure logical function dt_lt_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    ! they need to be unequal
    dt_lt_d = dt_lt(this, that%to_datetime())
  end function dt_lt_d

  !> \brief greater than comparison of datetimes
  pure logical function dt_gt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_gt = dt_neq(this, that) .and. .not. dt_lt(this, that)
  end function dt_gt

  !> \brief greater than comparison of datetime and date
  pure logical function dt_gt_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    ! they need to be unequal
    dt_gt_d = dt_gt(this, that%to_datetime())
  end function dt_gt_d

  !> \brief less than or equal comparison of datetimes
  pure logical function dt_leq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_leq = dt_lt(this, that) .or. dt_eq(this, that)
  end function dt_leq

  !> \brief less than or equal comparison of datetime and date
  pure logical function dt_leq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    ! they need to be unequal
    dt_leq_d = dt_leq(this, that%to_datetime())
  end function dt_leq_d

  !> \brief greater than or equal comparison of datetimes
  pure logical function dt_geq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_geq = dt_gt(this, that) .or. dt_eq(this, that)
  end function dt_geq

  !> \brief less than or equal comparison of datetime and date
  pure logical function dt_geq_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    ! they need to be unequal
    dt_geq_d = dt_geq(this, that%to_datetime())
  end function dt_geq_d

  !> \brief add a timedelta to a datetime
  pure type(datetime) function dt_add_td(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    type(timedelta) :: temp
    integer(i4) :: new_year, new_month, new_day, new_hour, new_minute, new_second, temp_seconds, day_delta, diy

    ! handle sub-day timing
    temp = timedelta(days=that%days, seconds=this%second+that%seconds, minutes=this%minute, hours=this%hour)
    ! find the new year
    new_year = this%year
    day_delta = this%doy() + temp%days
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

    ! remaining seconds are less than a day and are always positive
    temp_seconds = temp%seconds
    new_hour = temp_seconds / HOUR_SECONDS
    temp_seconds = mod(temp_seconds, HOUR_SECONDS)
    new_minute = temp_seconds / MINUTE_SECONDS
    new_second = mod(temp_seconds, MINUTE_SECONDS)
    ! create datetime
    dt_add_td%year = new_year
    dt_add_td%month = new_month
    dt_add_td%day = new_day
    dt_add_td%hour = new_hour
    dt_add_td%minute = new_minute
    dt_add_td%second = new_second
  end function dt_add_td

  !> \brief add a timedelta to a datetime
  pure type(datetime) function td_add_dt(that, this)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    td_add_dt = dt_add_td(this, that)
  end function td_add_dt

  !> \brief subtract a timedelta from a datetime
  pure type(datetime) function dt_sub_td(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    dt_sub_td = this + (-that)
  end function dt_sub_td

  !> \brief difference between two datetimes
  pure type(timedelta) function dt_sub_dt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    type(timedelta) :: tmp_this, tmp_that
    integer(i4) :: min_year, max_year, days_this, days_that, day_year_diff, i
    min_year = min(this%year, that%year)
    max_year = max(this%year, that%year)
    ! get year difference in days
    day_year_diff = 0_i4
    do i=min_year, max_year-1_i4
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

  !> \brief difference between datetime and date
  pure type(timedelta) function dt_sub_d(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(date), intent(in) :: that
    dt_sub_d = dt_sub_dt(this, that%to_datetime())
  end function dt_sub_d

  ! DATE

  !> \brief initialize a date
  function init_date(year, month, day) result(out)
    implicit none
    integer(i4), intent(in) :: year                     !< 1 <= year <= 9999
    integer(i4), intent(in) :: month                    !< 1 <= month <= 12
    integer(i4), intent(in) :: day                      !< 1 <= day <= number of days in the given month and year
    type(date) :: out
    out%year = year
    out%month = month
    out%day = day
    call check_datetime(year=out%year, month=out%month, day=out%day)
  end function init_date

  !> \brief date from string
  type(date) function date_from_string(string)
    use mo_string_utils, only : divide_string
    character(*), intent(in) :: string
    character(256), dimension(:), allocatable :: date_str
    integer(i4) :: year, month, day
    call divide_string(trim(string), '-', date_str)
    read(date_str(1), *) year
    read(date_str(2), *) month
    read(date_str(3), *) day
    date_from_string = date(year=year, month=month, day=day)
  end function date_from_string

  !> \brief new date with specified fields
  type(date) function d_replace(this, year, month, day)
    implicit none
    class(date), intent(in) :: this
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
    d_replace = date(new_year, new_month, new_day)
  end function d_replace

  !> \brief convert date to a datetime
  pure type(datetime) function to_datetime(this)
    implicit none
    class(date), intent(in) :: this
    to_datetime%year = this%year
    to_datetime%month = this%month
    to_datetime%day = this%day
    to_datetime%hour = 0_i4
    to_datetime%minute = 0_i4
    to_datetime%second = 0_i4
  end function to_datetime

  !> \brief string representation of the date
  pure character(19) function d_str(this)
    implicit none
    class(date), intent(in) :: this
    write(d_str, "(i4.4, '-' ,i2.2, '-', i2.2)") this%year, this%month, this%day
  end function d_str

  !> \brief day of the week
  pure integer(i4) function d_weekday(this)
    implicit none
    class(date), intent(in) :: this
    d_weekday = weekday(this%year, this%month, this%day)
  end function d_weekday

  !> \brief day of the year
  pure integer(i4) function d_doy(this)
    implicit none
    class(date), intent(in) :: this
    integer(i4) :: i
    d_doy = this%day
    do i=1_i4, this%month-1_i4
      d_doy = d_doy + days_in_month(year=this%year, month=i)
    end do
  end function d_doy

  !> \brief date is a new year
  pure logical function d_is_new_year(this)
    implicit none
    class(date), intent(in) :: this
    d_is_new_year = this%is_new_month() .and. this%month == 1_i4
  end function d_is_new_year

  !> \brief date is a new month
  pure logical function d_is_new_month(this)
    implicit none
    class(date), intent(in) :: this
    d_is_new_month = this%day == 1_i4
  end function d_is_new_month

  !> \brief date is a new week
  pure logical function d_is_new_week(this)
    implicit none
    class(date), intent(in) :: this
    d_is_new_week = this%weekday() == 1_i4
  end function d_is_new_week

  !> \brief equal comparison of dates
  pure logical function d_eq(this, that)
    implicit none
    class(date), intent(in) :: this, that
    d_eq = this%year == that%year &
      .and. this%month == that%month &
      .and. this%day == that%day
  end function d_eq

  !> \brief equal comparison of date and datetime
  pure logical function d_eq_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_eq_dt = dt_eq(this%to_datetime(), that)
  end function d_eq_dt

  !> \brief not equal comparison of dates
  pure logical function d_neq(this, that)
    implicit none
    class(date), intent(in) :: this, that
    d_neq = .not. d_eq(this, that)
  end function d_neq

  !> \brief not equal comparison of date and datetime
  pure logical function d_neq_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_neq_dt = dt_neq(this%to_datetime(), that)
  end function d_neq_dt

  !> \brief less than comparison of dates
  pure logical function d_lt(this, that)
    implicit none
    class(date), intent(in) :: this, that
    ! they need to be unequal
    d_lt = d_neq(this, that)
    if (.not. d_lt) return
    ! now check each component from biggest to smallest
    if (this%year < that%year) return ! true
    if (this%year > that%year) then
      d_lt = .false.
      return
    endif
    ! year equal
    if (this%month < that%month) return ! true
    if (this%month > that%month) then
      d_lt = .false.
      return
    endif
    ! year+month equal
    if (this%day < that%day) return ! true
    if (this%day > that%day) then
      d_lt = .false.
      return
    endif
    ! they can't be all equal since that was checked first
  end function d_lt

  !> \brief less than comparison of date and datetime
  pure logical function d_lt_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_lt_dt = dt_lt(this%to_datetime(), that)
  end function d_lt_dt

  !> \brief greater than comparison of dates
  pure logical function d_gt(this, that)
    implicit none
    class(date), intent(in) :: this, that
    d_gt = d_neq(this, that) .and. .not. d_lt(this, that)
  end function d_gt

  !> \brief greater than comparison of date and datetime
  pure logical function d_gt_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_gt_dt = dt_gt(this%to_datetime(), that)
  end function d_gt_dt

  !> \brief less than or equal comparison of dates
  pure logical function d_leq(this, that)
    implicit none
    class(date), intent(in) :: this, that
    d_leq = d_lt(this, that) .or. d_eq(this, that)
  end function d_leq

  !> \brief less than or equal comparison of date and datetime
  pure logical function d_leq_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_leq_dt = dt_leq(this%to_datetime(), that)
  end function d_leq_dt

  !> \brief greater than or equal comparison of dates
  pure logical function d_geq(this, that)
    implicit none
    class(date), intent(in) :: this, that
    d_geq = d_gt(this, that) .or. d_eq(this, that)
  end function d_geq

  !> \brief greater than or equal comparison of date and datetime
  pure logical function d_geq_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    d_geq_dt = dt_geq(this%to_datetime(), that)
  end function d_geq_dt

  !> \brief add a timedelta to a date
  pure type(date) function d_add_td(this, that)
    implicit none
    class(date), intent(in) :: this
    class(timedelta), intent(in) :: that
    type(datetime) :: temp
    ! use datetime routines
    temp = this%to_datetime()
    temp = temp + that
    ! ignore seconds
    d_add_td = temp%date()
  end function d_add_td

  !> \brief add a timedelta to a date
  pure type(date) function td_add_d(that, this)
    implicit none
    class(date), intent(in) :: this
    class(timedelta), intent(in) :: that
    td_add_d = d_add_td(this, that)
  end function td_add_d

  !> \brief subtract a timedelta from a date
  pure type(date) function d_sub_td(this, that)
    implicit none
    class(date), intent(in) :: this
    class(timedelta), intent(in) :: that
    d_sub_td = this + (-that)
  end function d_sub_td

  !> \brief difference between two dates
  pure type(timedelta) function d_sub_d(this, that)
    implicit none
    class(date), intent(in) :: this, that
    ! use datetime routine
    d_sub_d = this%to_datetime() - that%to_datetime()
  end function d_sub_d

  !> \brief difference between date and datetime
  pure type(timedelta) function d_sub_dt(this, that)
    implicit none
    class(date), intent(in) :: this
    class(datetime), intent(in) :: that
    ! use datetime routine
    d_sub_dt = this%to_datetime() - that
  end function d_sub_dt

  ! TIME

  !> \brief initialize a time
  function init_time(hour, minute, second) result(out)
    implicit none
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
    type(time) :: out
    out%hour = 0_i4
    if (present(hour)) out%hour = hour
    out%minute = 0_i4
    if (present(minute)) out%minute = minute
    out%second = 0_i4
    if (present(second)) out%second = second
    ! check if datetime is valid
    call check_datetime(hour=out%hour, minute=out%minute, second=out%second)
  end function init_time

  !> \brief time from string
  type(time) function time_from_string(string)
    use mo_string_utils, only : divide_string
    character(*), intent(in) :: string
    character(256), dimension(:), allocatable :: time_str
    integer(i4) :: hour, minute, second
    call divide_string(trim(string), ':', time_str)
    read(time_str(1), *) hour
    read(time_str(2), *) minute
    read(time_str(3), *) second
    time_from_string = time(hour=hour, minute=minute, second=second)
  end function time_from_string

  !> \brief new time with specified fields
  type(time) function t_replace(this, hour, minute, second)
    implicit none
    class(time), intent(in) :: this
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60
    integer(i4) :: new_hour, new_minute, new_second
    new_hour = this%hour
    new_minute = this%minute
    new_second = this%second
    if (present(hour)) new_hour = hour
    if (present(minute)) new_minute = minute
    if (present(second)) new_second = second
    t_replace = time(new_hour, new_minute, new_second)
  end function t_replace

  !> \brief string representation of the time
  pure character(19) function t_str(this)
    implicit none
    class(time), intent(in) :: this
    write(t_str, "(i2.2, ':', i2.2, ':', i2.2)") this%hour, this%minute, this%second
  end function t_str

  !> \brief time to second of the day
  pure integer(i4) function day_second(this)
    implicit none
    class(time), intent(in) :: this
    day_second = this%hour * HOUR_SECONDS + this%minute * MINUTE_SECONDS + this%second
  end function day_second

  !> \brief time is a new day / midnight
  pure logical function t_is_new_day(this)
    implicit none
    class(time), intent(in) :: this
    t_is_new_day = this%is_new_hour() .and. this%hour == 0_i4
  end function t_is_new_day

  !> \brief time is a new hour
  pure logical function t_is_new_hour(this)
    implicit none
    class(time), intent(in) :: this
    t_is_new_hour = this%is_new_minute() .and. this%minute == 0_i4
  end function t_is_new_hour

  !> \brief time is a new month
  pure logical function t_is_new_minute(this)
    implicit none
    class(time), intent(in) :: this
    t_is_new_minute = this%second == 0_i4
  end function t_is_new_minute

  !> \brief equal comparison of times
  pure logical function t_eq(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_eq = this%day_second() == that%day_second()
  end function t_eq

  !> \brief not equal comparison of times
  pure logical function t_neq(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_neq = .not. t_eq(this, that)
  end function t_neq

  !> \brief less than comparison of times
  pure logical function t_lt(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_lt = this%day_second() < that%day_second()
  end function t_lt

  !> \brief greater than comparison of times
  pure logical function t_gt(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_gt = this%day_second() > that%day_second()
  end function t_gt

  !> \brief less than or equal comparison of times
  pure logical function t_leq(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_leq = this%day_second() <= that%day_second()
  end function t_leq

  !> \brief greater than or equal comparison of times
  pure logical function t_geq(this, that)
    implicit none
    class(time), intent(in) :: this, that
    t_geq = this%day_second() >= that%day_second()
  end function t_geq

  ! TIMEDELTA

  !> \brief initialize a timedelta
  pure function init_timedelta(days, seconds, minutes, hours, weeks) result(out)
    implicit none
    integer(i4), intent(in), optional :: days           !< days defining time-span
    integer(i4), intent(in), optional :: seconds        !< seconds defining time-span
    integer(i4), intent(in), optional :: minutes        !< minutes defining time-span
    integer(i4), intent(in), optional :: hours          !< hours defining time-span
    integer(i4), intent(in), optional :: weeks          !< weeks defining time-span
    type(timedelta) :: out
    integer(i4) :: neg_days, remain_sec

    out%days = 0
    if (present(days)) out%days = days
    if (present(weeks)) out%days = out%days + weeks * WEEK_DAYS
    out%seconds = 0
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
  end function init_timedelta

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
    this%days=that%days
    this%seconds=that%seconds
  end subroutine td_copy

  !> \brief equal comparison of timedeltas
  pure logical function td_eq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_eq = this%total_seconds() == that%total_seconds()
  end function td_eq

  !> \brief not equal comparison of timedeltas
  pure logical function td_neq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_neq = .not. td_eq(this, that)
  end function td_neq

  !> \brief less than comparison of timedeltas
  pure logical function td_lt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_lt = this%total_seconds() < that%total_seconds()
  end function td_lt

  !> \brief greater than comparison of timedeltas
  pure logical function td_gt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_gt = this%total_seconds() > that%total_seconds()
  end function td_gt

  !> \brief less than or equal comparison of timedeltas
  pure logical function td_leq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_leq = this%total_seconds() <= that%total_seconds()
  end function td_leq

  !> \brief greater than or equal comparison of timedeltas
  pure logical function td_geq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_geq = this%total_seconds() >= that%total_seconds()
  end function td_geq

  !> \brief adding two timedeltas
  pure type(timedelta) function td_add(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_add = timedelta(days=this%days+that%days, seconds=this%seconds+that%seconds)
  end function td_add

  !> \brief adding two timedeltas
  pure type(timedelta) function td_sub(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_sub = timedelta(days=this%days-that%days, seconds=this%seconds-that%seconds)
  end function td_sub

  !> \brief negative timedelta
  pure type(timedelta) function td_neg(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_neg = timedelta(days=-this%days, seconds=-this%seconds)
  end function td_neg

  !> \brief positive timedelta
  pure type(timedelta) function td_pos(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_pos = this
  end function td_pos

  !> \brief multiply a timedelta with an integer
  pure type(timedelta) function td_mul1(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul1 = timedelta(days=this%days*that, seconds=this%seconds*that)
  end function td_mul1

  !> \brief multiply a timedelta with an integer
  pure type(timedelta) function td_mul2(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul2 = td_mul1(this, that)
  end function td_mul2

  !> \brief multiply a timedelta with a real
  pure type(timedelta) function td_mul1_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul1_dp = from_total_seconds(int(this%total_seconds() * that, i8))
  end function td_mul1_dp

  !> \brief multiply a timedelta with a real
  pure type(timedelta) function td_mul2_dp(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul2_dp = td_mul1_dp(this, that)
  end function td_mul2_dp

  !> \brief divide a timedelta by an integer
  pure type(timedelta) function td_div(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_div = from_total_seconds(this%total_seconds() / int(that, i8))
  end function td_div

  !> \brief divide a timedelta by a real
  pure type(timedelta) function td_div_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_div_dp = this * (1.0_dp / that)
  end function td_div_dp

  !> \brief divide a timedelta by a timedelta
  pure real(dp) function td_div_td(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_div_td = real(this%total_seconds(), dp) / real(that%total_seconds(), dp)
  end function td_div_td

end module mo_datetime
