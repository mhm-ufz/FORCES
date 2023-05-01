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

  public :: datetime
  public :: timedelta
  public :: max_delta
  public :: min_delta
  public :: zero_delta
  public :: one_day
  public :: one_hour
  public :: one_minute
  public :: one_second
  public :: one_week

  private

  integer(i4), parameter :: MINYEAR = 1
  integer(i4), parameter :: MAXYEAR = 9999

  !> \class   datetime
  !> \brief   This is a container to hold a date-time.
  type datetime
    integer(i4) :: year                     !< MINYEAR <= year <= MAXYEAR
    integer(i4) :: month                    !< 1 <= month <= 12
    integer(i4) :: day                      !< 1 <= day <= number of days in the given month and year
    integer(i4) :: hour                     !< 1 <= hour < 24
    integer(i4) :: minute                   !< 1 <= minute < 60
    integer(i4) :: second                   !< 1 <= second < 60
  contains
    procedure, public :: doy => dt_doy
    procedure, public :: is_new_year
    procedure, public :: is_new_month
    procedure, public :: is_new_day
    procedure, public :: is_new_hour
    procedure, public :: is_new_minute
    procedure, private :: dt_eq
    generic, public :: operator(==) => dt_eq
    procedure, private :: dt_neq
    generic, public :: operator(/=) => dt_neq
    procedure, private :: dt_lt
    generic, public :: operator(<) => dt_lt
    procedure, private :: dt_gt
    generic, public :: operator(>) => dt_gt
    procedure, private :: dt_leq
    generic, public :: operator(<=) => dt_leq
    procedure, private :: dt_geq
    generic, public :: operator(>=) => dt_geq
    procedure, private :: dt_add1
    procedure, pass(this), private :: dt_add2
    generic, public :: operator(+) => dt_add1, dt_add2
    procedure, private :: dt_sub_td
    procedure, private :: dt_sub_dt
    generic, public :: operator(-) => dt_sub_td, dt_sub_dt
  end type datetime

  !> \class   timedelta
  !> \brief   This is a container to hold a defined time span.
  type timedelta
    integer(i4) :: days                     !< days of the time-span
    integer(i4) :: seconds                  !< second of the time-span
  contains
    procedure, public :: abs => td_abs
    procedure, public :: total_seconds => td_total_seconds
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

  ! constructor interface for datetime
  interface datetime
    procedure init_datetime
  end interface datetime

  ! constructor interface timedelta
  interface timedelta
    procedure init_timedelta
  end interface timedelta

  type(timedelta), save, protected :: max_delta = timedelta(days=999999999_i4, seconds=86399_i4) !< max time delta
  type(timedelta), save, protected :: min_delta = timedelta(days=-999999999_i4, seconds=0_i4)    !< min time delta
  type(timedelta), save, protected :: zero_delta = timedelta(days=0_i4, seconds=0_i4)            !< zero time delta
  type(timedelta), save, protected :: one_week = timedelta(days=7_i4, seconds=0_i4)              !< one week time delta
  type(timedelta), save, protected :: one_day = timedelta(days=1_i4, seconds=0_i4)               !< one day time delta
  type(timedelta), save, protected :: one_hour = timedelta(days=0_i4, seconds=3600_i4)           !< one hour time delta
  type(timedelta), save, protected :: one_minute = timedelta(days=0_i4, seconds=60_i4)           !< one minute time delta
  type(timedelta), save, protected :: one_second = timedelta(days=0_i4, seconds=1_i4)            !< one second time delta

contains

  !> \brief number of days in a given year
  logical function is_leap_year(year)
    implicit none
    integer(i4), intent(in) :: year                     !< MINYEAR <= year <= MAXYEAR
    call check_datetime(year=year)
    is_leap_year = mod(year, 4_i4) == 0_i4 .and. (mod(year, 100_i4) /= 0_i4 .or. mod(year, 400_i4) == 0_i4)
  end function is_leap_year

  !> \brief number of days in a given month
  integer(i4) function days_in_month(year, month)
    implicit none
    integer(i4), intent(in) :: year                     !< MINYEAR <= year <= MAXYEAR
    integer(i4), intent(in) :: month                    !< 1 <= month <= 12
    call check_datetime(year=year, month=month)
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
  integer(i4) function days_in_year(year)
    implicit none
    integer(i4), intent(in) :: year                     !< MINYEAR <= year <= MAXYEAR
    days_in_year = 365_i4
    if (is_leap_year(year)) days_in_year = 366_i4
  end function days_in_year

  !> \brief get date from day of the year
  subroutine doy_to_month_day(year, doy, month, day)
    implicit none
    integer(i4), intent(in) :: year                     !< MINYEAR <= year <= MAXYEAR
    integer(i4), intent(in) :: doy                      !< 1 <= doy <= days_in_year
    integer(i4), intent(out), optional :: month         !< month for the given doy
    integer(i4), intent(out), optional :: day           !< day in month for the given doy
    integer(i4) :: i, dim, remain

    if (doy < 1_i4 .or. doy > days_in_year(year)) &
      call error_message("Given day of the year is out of range. Got: ", num2str(doy))
    remain = doy
    do i=1_i4, 12_i4
      dim = days_in_month(year=year, month=i)
      if (remain <= dim) exit
      remain = remain - dim
    end do
    if (present(month)) month = i
    if (present(day)) day = remain
  end subroutine doy_to_month_day

  !> \brief check if a datetime is valid
  subroutine check_datetime(year, month, day, hour, minute, second)
    implicit none
    integer(i4), intent(in), optional :: year           !< MINYEAR <= year <= MAXYEAR
    integer(i4), intent(in), optional :: month          !< 1 <= month <= 12
    integer(i4), intent(in), optional :: day            !< 1 <= day <= number of days in the given month and year
    integer(i4), intent(in), optional :: hour           !< 1 <= hour < 24
    integer(i4), intent(in), optional :: minute         !< 1 <= minute < 60
    integer(i4), intent(in), optional :: second         !< 1 <= second < 60

    ! year
    if (present(year)) then
      if (year < MINYEAR .or. year > MAXYEAR) &
        call error_message("datetime: year is out of range. Got: ", num2str(year))
    end if
    ! month
    if (present(month)) then
      if (month < 1 .or. month > 12) &
        call error_message("datetime: month is out of range. Got: ", num2str(month))
    end if
    ! day
    if (present(day)) then
      if (.not. (present(year) .and. present(month))) &
        call error_message("check_datetime: to validate a given 'day', 'year' and 'month' are required.")
      if (day < 1 .or. day > days_in_month(year, month)) &
        call error_message("datetime: day is out of range. Got: ", num2str(day))
    end if
    ! hour
    if (present(hour)) then
      if (hour < 0 .or. hour > 23) &
        call error_message("datetime: hour is out of range. Got: ", num2str(hour))
    end if
    ! minute
    if (present(minute)) then
      if (minute < 0 .or. minute > 59) &
        call error_message("datetime: minute is out of range. Got: ", num2str(minute))
    end if
    ! second
    if (present(second)) then
      if (second < 0 .or. second > 59) &
        call error_message("datetime: second is out of range. Got: ", num2str(second))
    end if
  end subroutine check_datetime

  !> \brief initialize a datetime
  function init_datetime(year, month, day, hour, minute, second) result(out)
    implicit none
    integer(i4), intent(in) :: year                     !< MINYEAR <= year <= MAXYEAR
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

  !> \brief day of the year
  integer(i4) function dt_doy(this)
    implicit none
    class(datetime), intent(in) :: this
    integer(i4) :: i
    dt_doy = this%day
    do i=1_i4, this%month-1_i4
      dt_doy = dt_doy + days_in_month(year=this%year, month=i)
    end do
  end function dt_doy

  !> \brief datetime is a new year
  logical function is_new_year(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_year = this%is_new_month() .and. this%month == 1_i4
  end function is_new_year

  !> \brief datetime is a new month
  logical function is_new_month(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_month = this%is_new_day() .and. this%day == 1_i4
  end function is_new_month

  !> \brief datetime is a new day
  logical function is_new_day(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_day = this%is_new_hour() .and. this%hour == 0_i4
  end function is_new_day

  !> \brief datetime is a new hour
  logical function is_new_hour(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_hour = this%is_new_minute() .and. this%minute == 0_i4
  end function is_new_hour

  !> \brief datetime is a new month
  logical function is_new_minute(this)
    implicit none
    class(datetime), intent(in) :: this
    is_new_minute = this%second == 0_i4
  end function is_new_minute

  !> \brief equal comparison of datetimes
  logical function dt_eq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_eq = this%year == that%year &
      .and. this%month == that%month &
      .and. this%day == that%day &
      .and. this%hour == that%hour &
      .and. this%minute == that%minute &
      .and. this%second == that%second
  end function dt_eq

  !> \brief not equal comparison of datetimes
  logical function dt_neq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_neq = .not. dt_eq(this, that)
  end function dt_neq

  !> \brief less than comparison of datetimes
  logical function dt_lt(this, that)
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

  !> \brief greater than comparison of datetimes
  logical function dt_gt(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_gt = dt_neq(this, that) .and. .not. dt_lt(this, that)
  end function dt_gt

  !> \brief less than or equal comparison of datetimes
  logical function dt_leq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_leq = dt_lt(this, that) .or. dt_eq(this, that)
  end function dt_leq

  !> \brief greater than or equal comparison of datetimes
  logical function dt_geq(this, that)
    implicit none
    class(datetime), intent(in) :: this, that
    dt_geq = dt_gt(this, that) .or. dt_eq(this, that)
  end function dt_geq

  !> \brief add a timedelta to a datetime
  type(datetime) function dt_add1(this, that)
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
    new_hour = temp_seconds / 3600_i4
    temp_seconds = mod(temp_seconds, 3600_i4)
    new_minute = temp_seconds / 60_i4
    new_second = mod(temp_seconds, 60_i4)
    ! create datetime
    dt_add1 = datetime(year=new_year, month=new_month, day=new_day, hour=new_hour, minute=new_minute, second=new_second)
  end function dt_add1

  !> \brief add a timedelta to a datetime
  type(datetime) function dt_add2(that, this)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    dt_add2 = dt_add1(this, that)
  end function dt_add2

  !> \brief subtract a timedelta from a datetime
  type(datetime) function dt_sub_td(this, that)
    implicit none
    class(datetime), intent(in) :: this
    class(timedelta), intent(in) :: that
    dt_sub_td = this + (-that)
  end function dt_sub_td

  !> \brief difference between two datetimes
  type(timedelta) function dt_sub_dt(this, that)
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

  !> \brief initialize a timedelta
  function init_timedelta(days, seconds, minutes, hours, weeks) result(out)
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
    if (present(weeks)) out%days = out%days + weeks * 7_i4
    out%seconds = 0
    if (present(seconds)) out%seconds = seconds
    if (present(minutes)) out%seconds = out%seconds + minutes * 60_i4
    if (present(hours)) out%seconds = out%seconds + hours * 3600_i4

    ! force: 0 <= seconds < 86400
    if (out%seconds < 0) then
      neg_days = abs(out%seconds) / 86400_i4
      remain_sec = mod(abs(out%seconds), 86400_i4)
      ! add full days in negative seconds
      out%seconds = out%seconds + neg_days * 86400_i4
      out%days = out%days - neg_days
      ! add one days to remaining seconds if still negative
      if (remain_sec > 0) then
        out%seconds = out%seconds + 86400_i4
        out%days = out%days - 1_i4
      end if
    end if
    if (out%seconds >= 86400_i4) then
      neg_days = out%seconds / 86400_i4
      out%seconds = out%seconds - neg_days * 86400_i4
      out%days = out%days + neg_days
    end if
    if (out < min_delta .or. out > max_delta) &
      call error_message("timedelta out of range.")
  end function init_timedelta

  !> \brief absolute timedelta
  type(timedelta) function td_abs(this)
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
  integer(i8) function td_total_seconds(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_total_seconds = int(this%days, i8) * 86400_i8 + int(this%seconds, i8)
  end function td_total_seconds

  type(timedelta) function from_total_seconds(total_seconds)
    integer(i8), intent(in) :: total_seconds
    if (total_seconds > max_delta%total_seconds() .or. total_seconds < min_delta%total_seconds()) &
      call error_message("timedelta: given total seconds are out of range. Got: ", num2str(total_seconds))
    from_total_seconds = timedelta(days=int(total_seconds / 86400_i8, i4), seconds=int(mod(total_seconds, 86400_i8), i4))
  end function from_total_seconds

  !> \brief equal comparison of timedeltas
  logical function td_eq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_eq = this%total_seconds() == that%total_seconds()
  end function td_eq

  !> \brief not equal comparison of timedeltas
  logical function td_neq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_neq = .not. td_eq(this, that)
  end function td_neq

  !> \brief less than comparison of timedeltas
  logical function td_lt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_lt = this%total_seconds() < that%total_seconds()
  end function td_lt

  !> \brief greater than comparison of timedeltas
  logical function td_gt(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_gt = this%total_seconds() > that%total_seconds()
  end function td_gt

  !> \brief less than or equal comparison of timedeltas
  logical function td_leq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_leq = this%total_seconds() <= that%total_seconds()
  end function td_leq

  !> \brief greater than or equal comparison of timedeltas
  logical function td_geq(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_geq = this%total_seconds() >= that%total_seconds()
  end function td_geq

  !> \brief adding two timedeltas
  type(timedelta) function td_add(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_add = timedelta(days=this%days+that%days, seconds=this%seconds+that%seconds)
  end function td_add

  !> \brief adding two timedeltas
  type(timedelta) function td_sub(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_sub = timedelta(days=this%days-that%days, seconds=this%seconds-that%seconds)
  end function td_sub

  !> \brief negative timedelta
  type(timedelta) function td_neg(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_neg = timedelta(days=-this%days, seconds=-this%seconds)
  end function td_neg

  !> \brief positive timedelta
  type(timedelta) function td_pos(this)
    implicit none
    class(timedelta), intent(in) :: this
    td_pos = this
  end function td_pos

  !> \brief multiply a timedelta with an integer
  type(timedelta) function td_mul1(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul1 = timedelta(days=this%days*that, seconds=this%seconds*that)
  end function td_mul1

  !> \brief multiply a timedelta with an integer
  type(timedelta) function td_mul2(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_mul2 = td_mul1(this, that)
  end function td_mul2

  !> \brief multiply a timedelta with a real
  type(timedelta) function td_mul1_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul1_dp = from_total_seconds(int(this%total_seconds() * that, i8))
  end function td_mul1_dp

  !> \brief multiply a timedelta with a real
  type(timedelta) function td_mul2_dp(that, this)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_mul2_dp = td_mul1_dp(this, that)
  end function td_mul2_dp

  !> \brief divide a timedelta by an integer
  type(timedelta) function td_div(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    integer(i4), intent(in) :: that
    td_div = from_total_seconds(this%total_seconds() / int(that, i8))
  end function td_div

  !> \brief divide a timedelta by a real
  type(timedelta) function td_div_dp(this, that)
    implicit none
    class(timedelta), intent(in) :: this
    real(dp), intent(in) :: that
    td_div_dp = this * (1.0_dp / that)
  end function td_div_dp

  !> \brief divide a timedelta by a timedelta
  real(dp) function td_div_td(this, that)
    implicit none
    class(timedelta), intent(in) :: this, that
    td_div_td = real(this%total_seconds(), dp) / real(that%total_seconds(), dp)
  end function td_div_td

end module mo_datetime
