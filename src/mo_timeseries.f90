!> \file    mo_timeseries.f90
!> \copydoc mo_timeseries

!> \brief   Time-axis representation and resampling.
!> \details This module provides a CF-compatible integer time-axis type and a
!!          reusable resampler for caller-owned time-series arrays. Datetime
!!          objects are used at API boundaries, while resampling calculations
!!          use integer time offsets.
!! \par Examples
!! - \ref 04_points_timeseries_resample.f90 : \copybrief 04_points_timeseries_resample.f90
!!   \include 04_points_timeseries_resample.f90
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Jun 2026
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_timeseries

  use mo_datetime, only: datetime, timedelta, delta_from_string, decode_cf_time_units, time_units_delta, time_values, &
                         yearly, monthly, daily, no_time, hourly, varying, &
                         start_timestamp, center_timestamp, end_timestamp, instant_timestamp, &
                         infer_time_bounds, infer_time_timestep_from_bounds, infer_time_timestep_from_values
  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_orderpack, only: omedian
  use mo_utils, only: optval

  implicit none

  private

  public :: yearly, monthly, daily, no_time, hourly, varying
  public :: start_timestamp, center_timestamp, end_timestamp, instant_timestamp

  !> \name Time-Series Support
  !> \brief Constants selecting whether values represent instants or intervals.
  !!@{
  integer(i4), public, parameter :: ts_instant = 0_i4 !< values are tied to time stamps
  integer(i4), public, parameter :: ts_interval = 1_i4 !< values describe time intervals
  !!@}

  !> \name Interval Methods
  !> \brief Constants selecting the meaning of interval values.
  !!@{
  integer(i4), public, parameter :: ts_none = 0_i4 !< no interval method
  integer(i4), public, parameter :: ts_mean = 1_i4 !< interval mean
  integer(i4), public, parameter :: ts_sum = 2_i4 !< interval sum
  integer(i4), public, parameter :: ts_min = 3_i4 !< interval minimum
  integer(i4), public, parameter :: ts_max = 4_i4 !< interval maximum
  integer(i4), public, parameter :: ts_median = 5_i4 !< interval median
  !!@}

  !> \name Instant Interpolation
  !> \brief Constants selecting interpolation for instant time series.
  !!@{
  integer(i4), public, parameter :: ts_previous = 0_i4 !< use previous source value
  integer(i4), public, parameter :: ts_nearest = 1_i4 !< use nearest source value
  integer(i4), public, parameter :: ts_linear = 2_i4 !< linear interpolation
  !!@}

  !> \class time_t
  !> \brief CF-compatible integer time axis.
  !> \details The axis stores integer time values and optional bounds together
  !!          with their reference time and unit delta. Public methods expose
  !!          datetimes where this is useful, but calculations can use the
  !!          integer axis.
  type, public :: time_t
    integer(i4), allocatable :: values(:)        !< time coordinate values
    integer(i4), allocatable :: bounds(:, :)     !< time bounds with shape (2,n_times)
    character(:), allocatable :: units           !< CF time units
    type(datetime) :: ref_time                   !< reference time in CF units
    type(timedelta) :: delta                     !< one unit of the integer time axis
    integer(i4) :: timestep = daily              !< time-step indicator
    integer(i4) :: timestamp = end_timestamp     !< timestamp location selector
  contains
    procedure, public :: init => time_init
    procedure, public :: init_discrete => time_init_discrete
    procedure, public :: init_cf => time_init_cf
    procedure, public :: n_times => time_n_times
    procedure, public :: has_bounds => time_has_bounds
    procedure, public :: time => time_at
    procedure, public :: lower => time_lower
    procedure, public :: upper => time_upper
    procedure, public :: to_cf => time_to_cf
    procedure, public :: copy_to => time_copy_to
    procedure, public :: window_indices => time_window_indices
    procedure, public :: resampled => time_resampled
    procedure, private :: validate => time_validate
    procedure, private :: check_index => time_check_index
    procedure, private :: integer_value => time_integer_value
    procedure, private :: value_seconds => time_value_seconds
    procedure, private :: bound_seconds => time_bound_seconds
  end type time_t

  !> \class resampler_t
  !> \brief Reusable resampler between two time axes.
  !> \details The resampler owns source/target time-axis metadata and
  !!          precomputed integer-time mappings. Values stay caller-owned and
  !!          are supplied to \ref execute.
  type, public :: resampler_t
    type(time_t) :: source                         !< source time axis
    type(time_t) :: target                         !< target time axis
    integer(i4) :: support = ts_interval           !< \ref ts_instant or \ref ts_interval
    integer(i4) :: method = ts_mean                !< interval method
    integer(i4) :: interpolation = ts_nearest      !< instant interpolation method
    integer(i8), allocatable :: source_values(:)   !< source timestamps in common seconds
    integer(i8), allocatable :: target_values(:)   !< target timestamps in common seconds
    integer(i8), allocatable :: source_bounds(:, :) !< source bounds in common seconds
    integer(i8), allocatable :: target_bounds(:, :) !< target bounds in common seconds
    integer(i4), allocatable :: left(:)            !< source index for instant interpolation
    real(dp), allocatable :: weight(:)             !< linear instant interpolation weight
    integer(i4), allocatable :: first(:)           !< first overlapping source interval per target
    integer(i4), allocatable :: last(:)            !< last overlapping source interval per target
  contains
    procedure, public :: init => resampler_init
    procedure, private :: resampler_execute_1d
    procedure, private :: resampler_execute_2d
    generic, public :: execute => resampler_execute_1d, resampler_execute_2d
    procedure, private :: precompute_instant
    procedure, private :: precompute_interval
    procedure, private :: execute_instant_1d
    procedure, private :: execute_instant_2d
    procedure, private :: execute_interval_1d
    procedure, private :: execute_interval_2d
    procedure, private :: init_interval_value
    procedure, private :: validate_config => resampler_validate_config
  end type resampler_t

contains

  !> \brief Initialize a contiguous time axis from datetimes.
  subroutine time_init(self, timeframe_start, timeframe_end, timestep, delta, timestamp, ref_time)
    class(time_t), intent(inout) :: self
    type(datetime), intent(in) :: timeframe_start !< start of first interval
    type(datetime), intent(in) :: timeframe_end !< end of final interval
    integer(i4), optional, intent(in) :: timestep !< time-step indicator or positive hourly step (default: \ref daily)
    character(*), optional, intent(in) :: delta !< explicit unit, for example "hours" or "days"
    integer(i4), optional, intent(in) :: timestamp !< timestamp location selector (default: \ref end_timestamp)
    type(datetime), optional, intent(in) :: ref_time !< reference time, defaults to timeframe_start

    character(:), allocatable :: units_delta
    type(datetime) :: previous_time, current_time
    integer(i4) :: t_start, t_end, t_stamp
    integer(i4) :: i, n_times

    if (timeframe_end <= timeframe_start) call error_message("time%init: invalid time frame")
    if (allocated(self%values)) deallocate(self%values)
    if (allocated(self%bounds)) deallocate(self%bounds)
    self%timestep = optval(timestep, daily)
    self%timestamp = optval(timestamp, end_timestamp)
    units_delta = trim(optval(delta, time_units_delta(self%timestep, self%timestamp)))
    self%delta = delta_from_string(units_delta)
    self%ref_time = timeframe_start
    if (present(ref_time)) self%ref_time = ref_time
    self%units = units_delta // " since " // self%ref_time%str()

    n_times = 0_i4
    previous_time = timeframe_start
    do while(previous_time < timeframe_end)
      current_time = time_next(previous_time, self%timestep, delta, present(delta))
      if (current_time > timeframe_end) call error_message("time%init: timestep does not align")
      n_times = n_times + 1_i4
      previous_time = current_time
    end do

    allocate(self%values(n_times), self%bounds(2_i4, n_times))
    previous_time = timeframe_start
    do i = 1_i4, n_times
      current_time = time_next(previous_time, self%timestep, delta, present(delta))
      call time_values(self%ref_time, previous_time, current_time, self%delta, self%timestamp, t_start, t_end, t_stamp)
      self%bounds(:, i) = [t_start, t_end]
      self%values(i) = t_stamp
      previous_time = current_time
    end do
  end subroutine time_init

  !> \brief Initialize a discrete instant time axis from datetimes.
  subroutine time_init_discrete(self, timeframe_start, timeframe_end, timestep, delta, ref_time, include_start, include_end)
    class(time_t), intent(inout) :: self
    type(datetime), intent(in) :: timeframe_start !< first possible timestamp
    type(datetime), intent(in) :: timeframe_end !< final frame edge
    integer(i4), optional, intent(in) :: timestep !< time-step indicator or positive hourly step (default: \ref daily)
    character(*), optional, intent(in) :: delta !< explicit unit, for example "hours" or "days"
    type(datetime), optional, intent(in) :: ref_time !< reference time, defaults to timeframe_start
    logical, optional, intent(in) :: include_start !< include timeframe_start as timestamp (default: true)
    logical, optional, intent(in) :: include_end !< include timeframe_end as timestamp (default: false)

    character(:), allocatable :: units_delta
    type(datetime) :: current_time
    logical :: include_start_, include_end_
    integer(i4), allocatable :: tmp_values(:)
    integer(i4) :: n_times

    if (timeframe_end <= timeframe_start) call error_message("time%init_discrete: invalid time frame")
    if (allocated(self%values)) deallocate(self%values)
    if (allocated(self%bounds)) deallocate(self%bounds)
    self%timestep = optval(timestep, daily)
    self%timestamp = instant_timestamp
    units_delta = trim(optval(delta, time_units_delta(self%timestep, self%timestamp)))
    self%delta = delta_from_string(units_delta)
    self%ref_time = timeframe_start
    if (present(ref_time)) self%ref_time = ref_time
    self%units = units_delta // " since " // self%ref_time%str()
    include_start_ = optval(include_start, .true.)
    include_end_ = optval(include_end, .false.)

    n_times = 0_i4
    current_time = timeframe_start
    if (.not.include_start_) current_time = time_next(current_time, self%timestep, delta, present(delta))
    do while(current_time < timeframe_end .or. (include_end_ .and. current_time == timeframe_end))
      n_times = n_times + 1_i4
      current_time = time_next(current_time, self%timestep, delta, present(delta))
    end do
    if (n_times < 1_i4) call error_message("time%init_discrete: empty time axis")

    allocate(tmp_values(n_times))
    current_time = timeframe_start
    if (.not.include_start_) current_time = time_next(current_time, self%timestep, delta, present(delta))
    n_times = 0_i4
    do while(current_time < timeframe_end .or. (include_end_ .and. current_time == timeframe_end))
      n_times = n_times + 1_i4
      tmp_values(n_times) = self%integer_value(current_time)
      current_time = time_next(current_time, self%timestep, delta, present(delta))
    end do
    call move_alloc(tmp_values, self%values)
    call self%validate()
  end subroutine time_init_discrete

  !> \brief Initialize a time axis from CF-style integer values and optional bounds.
  subroutine time_init_cf(self, values, units, bounds, timestamp, infer_bounds)
    class(time_t), intent(inout) :: self
    integer(i4), intent(in) :: values(:) !< CF time coordinate values
    character(*), intent(in) :: units !< CF time units
    integer(i4), optional, intent(in) :: bounds(:, :) !< optional bounds with shape (2,n_times)
    integer(i4), optional, intent(in) :: timestamp !< timestamp location if bounds are inferred (default: \ref end_timestamp)
    logical, optional, intent(in) :: infer_bounds !< infer bounds when bounds are omitted (default: false)

    if (size(values) < 1_i4) call error_message("time%init_cf: empty time axis")
    if (allocated(self%values)) deallocate(self%values)
    if (allocated(self%bounds)) deallocate(self%bounds)
    self%units = trim(units)
    call decode_cf_time_units(self%units, self%delta, self%ref_time)
    allocate(self%values(size(values)), source=values)
    if (present(bounds)) then
      self%timestamp = optval(timestamp, end_timestamp)
      if (size(bounds, 1) /= 2_i4 .or. size(bounds, 2) /= size(values)) call error_message("time%init_cf: invalid bounds shape")
      allocate(self%bounds(2_i4, size(values)), source=bounds)
      self%timestep = infer_time_timestep_from_bounds(self%bounds, self%delta, self%ref_time)
    else if (optval(infer_bounds, .false.)) then
      self%timestamp = optval(timestamp, end_timestamp)
      call infer_time_bounds(self%values, self%timestamp, self%bounds)
      self%timestep = infer_time_timestep_from_bounds(self%bounds, self%delta, self%ref_time)
    else
      self%timestamp = instant_timestamp
      self%timestep = infer_time_timestep_from_values(self%values, self%delta, self%ref_time)
    end if
    call self%validate()
  end subroutine time_init_cf

  !> \brief Return number of time steps.
  integer(i4) function time_n_times(self) result(n_times)
    class(time_t), intent(in) :: self
    if (.not.allocated(self%values)) then
      n_times = 0_i4
    else
      n_times = size(self%values)
    end if
  end function time_n_times

  !> \brief Return whether the time axis has interval bounds.
  logical function time_has_bounds(self) result(has_bounds)
    class(time_t), intent(in) :: self
    has_bounds = allocated(self%bounds)
  end function time_has_bounds

  !> \brief Return timestamp as datetime.
  type(datetime) function time_at(self, i) result(time)
    class(time_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call self%check_index(i, "time%time")
    time = self%ref_time + self%values(i) * self%delta
  end function time_at

  !> \brief Return lower interval bound as datetime.
  type(datetime) function time_lower(self, i) result(time)
    class(time_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call self%check_index(i, "time%lower")
    if (.not.self%has_bounds()) call error_message("time%lower: axis has no bounds")
    time = self%ref_time + self%bounds(1_i4, i) * self%delta
  end function time_lower

  !> \brief Return upper interval bound as datetime.
  type(datetime) function time_upper(self, i) result(time)
    class(time_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call self%check_index(i, "time%upper")
    if (.not.self%has_bounds()) call error_message("time%upper: axis has no bounds")
    time = self%ref_time + self%bounds(2_i4, i) * self%delta
  end function time_upper

  !> \brief Copy CF-style values, bounds and units from the axis.
  subroutine time_to_cf(self, values, bounds, units)
    class(time_t), intent(in) :: self
    integer(i4), allocatable, intent(out) :: values(:) !< CF time coordinate values
    integer(i4), allocatable, optional, intent(out) :: bounds(:, :) !< CF time bounds
    character(:), allocatable, intent(out) :: units !< CF time units
    if (.not.allocated(self%values)) call error_message("time%to_cf: uninitialized axis")
    allocate(values(size(self%values)), source=self%values)
    if (present(bounds)) then
      if (allocated(bounds)) deallocate(bounds)
      if (self%has_bounds()) allocate(bounds(2_i4, size(self%values)), source=self%bounds)
    end if
    units = self%units
  end subroutine time_to_cf

  !> \brief Return first and last indices inside a datetime window.
  subroutine time_window_indices(self, timeframe_start, timeframe_end, first, last)
    class(time_t), intent(in) :: self
    type(datetime), intent(in) :: timeframe_start !< start of selected time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of selected time frame, including
    integer(i4), intent(out) :: first !< first selected one-based index
    integer(i4), intent(out) :: last !< last selected one-based index

    integer(i4) :: start_value, end_value
    integer(i4) :: n

    if (timeframe_end <= timeframe_start) call error_message("time%window_indices: invalid time frame")
    if (.not.allocated(self%values)) call error_message("time%window_indices: uninitialized axis")
    start_value = self%integer_value(timeframe_start)
    end_value = self%integer_value(timeframe_end)
    n = size(self%values)
    first = 1_i4
    do while(first <= n .and. self%values(first) <= start_value)
      first = first + 1_i4
    end do
    last = n
    do while(last >= 1_i4 .and. self%values(last) > end_value)
      last = last - 1_i4
    end do
    if (first > last) call error_message("time%window_indices: empty time frame")
  end subroutine time_window_indices

  !> \brief Derive a contiguous axis over the same time frame with a new resolution.
  type(time_t) function time_resampled(self, timestep, delta, timestamp, ref_time, timeframe_start, timeframe_end) result(out)
    class(time_t), intent(in) :: self
    integer(i4), optional, intent(in) :: timestep !< target time-step indicator or positive hourly step (default: \ref daily)
    character(*), optional, intent(in) :: delta !< explicit target unit
    integer(i4), optional, intent(in) :: timestamp !< target timestamp selector (default: current axis timestamp)
    type(datetime), optional, intent(in) :: ref_time !< target reference time (default: current axis reference time)
    type(datetime), optional, intent(in) :: timeframe_start !< target time-frame start (default: current lower bound)
    type(datetime), optional, intent(in) :: timeframe_end !< target time-frame end (default: current upper bound)

    integer(i4) :: timestamp_
    type(datetime) :: start_time, end_time, source_start, source_end

    if (.not.self%has_bounds()) call error_message("time%resampled: axis has no bounds")
    timestamp_ = optval(timestamp, self%timestamp)
    source_start = self%lower(1_i4)
    source_end = self%upper(self%n_times())
    start_time = source_start
    end_time = source_end
    if (present(timeframe_start)) start_time = timeframe_start
    if (present(timeframe_end)) end_time = timeframe_end
    if (start_time < source_start .or. end_time > source_end .or. end_time <= start_time) &
      call error_message("time%resampled: invalid time frame")
    if (present(ref_time)) then
      call out%init(start_time, end_time, timestep=timestep, delta=delta, timestamp=timestamp_, ref_time=ref_time)
    else
      call out%init(start_time, end_time, timestep=timestep, delta=delta, timestamp=timestamp_, ref_time=self%ref_time)
    end if
  end function time_resampled

  !> \brief Initialize a reusable resampler between two time axes.
  subroutine resampler_init(self, source_time, target_time, support, method, interpolation)
    class(resampler_t), intent(inout) :: self
    type(time_t), intent(in) :: source_time !< source time axis
    type(time_t), intent(in) :: target_time !< target time axis
    integer(i4), optional, intent(in) :: support !< \ref ts_instant or \ref ts_interval (default: \ref ts_interval)
    integer(i4), optional, intent(in) :: method !< interval method (default: \ref ts_mean for intervals, \ref ts_none for instants)
    integer(i4), optional, intent(in) :: interpolation !< instant interpolation selector (default: \ref ts_nearest)

    call source_time%copy_to(self%source)
    call target_time%copy_to(self%target)
    self%support = optval(support, ts_interval)
    self%interpolation = optval(interpolation, ts_nearest)
    if (present(method)) then
      self%method = method
    else if (self%support == ts_interval) then
      self%method = ts_mean
    else
      self%method = ts_none
    end if
    call self%validate_config()

    select case(self%support)
      case(ts_instant)
        call self%source%validate()
        call self%target%validate()
        call self%precompute_instant()
      case(ts_interval)
        call self%source%validate(require_bounds=.true.)
        call self%target%validate(require_bounds=.true.)
        call self%precompute_interval()
      case default
        call error_message("resampler%init: invalid support")
    end select
  end subroutine resampler_init

  !> \brief Resample one caller-owned series.
  subroutine resampler_execute_1d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:) !< source values with shape (source_time)
    real(dp), intent(out) :: target(:) !< target values with shape (target_time)

    if (size(source) /= self%source%n_times()) call error_message("resampler%execute: invalid source size")
    if (size(target) /= self%target%n_times()) call error_message("resampler%execute: invalid target size")
    select case(self%support)
      case(ts_instant)
        call self%execute_instant_1d(source, target)
      case(ts_interval)
        call self%execute_interval_1d(source, target)
      case default
        call error_message("resampler%execute: invalid support")
    end select
  end subroutine resampler_execute_1d

  !> \brief Resample several caller-owned series.
  subroutine resampler_execute_2d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:, :) !< source values with shape (n_series,source_time)
    real(dp), intent(out) :: target(:, :) !< target values with shape (n_series,target_time)

    if (size(source, 2) /= self%source%n_times()) call error_message("resampler%execute: invalid source time size")
    if (size(target, 2) /= self%target%n_times()) call error_message("resampler%execute: invalid target time size")
    if (size(source, 1) /= size(target, 1)) call error_message("resampler%execute: invalid series count")
    select case(self%support)
      case(ts_instant)
        call self%execute_instant_2d(source, target)
      case(ts_interval)
        call self%execute_interval_2d(source, target)
      case default
        call error_message("resampler%execute: invalid support")
    end select
  end subroutine resampler_execute_2d

  subroutine precompute_instant(self)
    class(resampler_t), intent(inout) :: self
    integer(i4) :: i, j
    integer(i4) :: n_source

    call self%source%value_seconds(self%source%ref_time, self%source_values)
    call self%target%value_seconds(self%source%ref_time, self%target_values)
    if (allocated(self%left)) deallocate(self%left)
    if (allocated(self%weight)) deallocate(self%weight)
    allocate(self%left(size(self%target_values)), self%weight(size(self%target_values)))
    self%weight = 0.0_dp
    n_source = size(self%source_values)
    j = 1_i4
    do i = 1_i4, size(self%target_values)
      do
        if (j >= n_source) exit
        if (self%source_values(j + 1_i4) > self%target_values(i)) exit
        j = j + 1_i4
      end do
      select case(self%interpolation)
        case(ts_previous)
          if (self%target_values(i) < self%source_values(1_i4)) call error_message("resampler%init: target time before source range")
          self%left(i) = j
        case(ts_nearest)
          if (self%target_values(i) <= self%source_values(1_i4)) then
            self%left(i) = 1_i4
          else if (self%target_values(i) >= self%source_values(n_source)) then
            self%left(i) = n_source
          else if (abs(self%source_values(j + 1_i4) - self%target_values(i)) < &
                   abs(self%target_values(i) - self%source_values(j))) then
            self%left(i) = j + 1_i4
          else
            self%left(i) = j
          end if
        case(ts_linear)
          if (self%target_values(i) < self%source_values(1_i4) .or. &
              self%target_values(i) > self%source_values(n_source)) &
            call error_message("resampler%init: target time outside source range")
          self%left(i) = j
          if (j < n_source) then
            if (self%source_values(j) == self%target_values(i)) cycle
            self%weight(i) = real(self%target_values(i) - self%source_values(j), dp) / &
                             real(self%source_values(j + 1_i4) - self%source_values(j), dp)
          end if
        case default
          call error_message("resampler%init: invalid interpolation")
      end select
    end do
  end subroutine precompute_instant

  subroutine precompute_interval(self)
    class(resampler_t), intent(inout) :: self
    integer(i4) :: i, j, k
    integer(i4) :: n_source
    integer(i8) :: lower, upper, target_width, total_overlap

    call self%source%bound_seconds(self%source%ref_time, self%source_bounds)
    call self%target%bound_seconds(self%source%ref_time, self%target_bounds)
    if (allocated(self%first)) deallocate(self%first)
    if (allocated(self%last)) deallocate(self%last)
    allocate(self%first(size(self%target_bounds, 2)), self%last(size(self%target_bounds, 2)))
    self%first = 0_i4
    self%last = 0_i4

    n_source = size(self%source_bounds, 2)
    j = 1_i4
    do i = 1_i4, size(self%target_bounds, 2)
      do
        if (j > n_source) exit
        if (self%source_bounds(2_i4, j) > self%target_bounds(1_i4, i)) exit
        j = j + 1_i4
      end do
      total_overlap = 0_i8
      target_width = self%target_bounds(2_i4, i) - self%target_bounds(1_i4, i)
      k = j
      do
        if (k > n_source) exit
        if (self%source_bounds(1_i4, k) >= self%target_bounds(2_i4, i)) exit
        lower = max(self%source_bounds(1_i4, k), self%target_bounds(1_i4, i))
        upper = min(self%source_bounds(2_i4, k), self%target_bounds(2_i4, i))
        if (upper > lower) then
          if (self%first(i) == 0_i4) self%first(i) = k
          self%last(i) = k
          total_overlap = total_overlap + upper - lower
        end if
        if (self%source_bounds(2_i4, k) >= self%target_bounds(2_i4, i)) exit
        k = k + 1_i4
      end do
      if (total_overlap <= 0_i8) call error_message("resampler%init: target interval not covered")
      if (total_overlap /= target_width) call error_message("resampler%init: target interval partially covered")
    end do
  end subroutine precompute_interval

  subroutine execute_instant_1d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:)
    real(dp), intent(out) :: target(:)
    integer(i4) :: i, j

    do i = 1_i4, size(target)
      j = self%left(i)
      if (self%weight(i) == 0.0_dp) then
        target(i) = source(j)
      else
        target(i) = source(j) + self%weight(i) * (source(j + 1_i4) - source(j))
      end if
    end do
  end subroutine execute_instant_1d

  subroutine execute_instant_2d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:, :)
    real(dp), intent(out) :: target(:, :)
    integer(i4) :: i, j

    do i = 1_i4, size(target, 2)
      j = self%left(i)
      if (self%weight(i) == 0.0_dp) then
        target(:, i) = source(:, j)
      else
        target(:, i) = source(:, j) + self%weight(i) * (source(:, j + 1_i4) - source(:, j))
      end if
    end do
  end subroutine execute_instant_2d

  subroutine execute_interval_1d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:)
    real(dp), intent(out) :: target(:)
    real(dp), allocatable :: median_values(:)
    real(dp) :: overlap, source_width, target_width
    integer(i4) :: i, j, n_overlap
    integer(i8) :: lower, upper

    do i = 1_i4, size(target)
      call self%init_interval_value(i, target(i), target_width)
      if (self%method == ts_median) then
        if (allocated(median_values)) deallocate(median_values)
        allocate(median_values(self%last(i) - self%first(i) + 1_i4))
        n_overlap = 0_i4
      end if
      do j = self%first(i), self%last(i)
        lower = max(self%source_bounds(1_i4, j), self%target_bounds(1_i4, i))
        upper = min(self%source_bounds(2_i4, j), self%target_bounds(2_i4, i))
        if (upper <= lower) cycle
        overlap = real(upper - lower, dp)
        select case(self%method)
          case(ts_mean)
            target(i) = target(i) + source(j) * overlap
          case(ts_sum)
            source_width = real(self%source_bounds(2_i4, j) - self%source_bounds(1_i4, j), dp)
            if (source_width <= 0.0_dp) call error_message("resampler%execute: invalid source interval")
            target(i) = target(i) + source(j) * overlap / source_width
          case(ts_min)
            target(i) = min(target(i), source(j))
          case(ts_max)
            target(i) = max(target(i), source(j))
          case(ts_median)
            n_overlap = n_overlap + 1_i4
            median_values(n_overlap) = source(j)
          case default
            call error_message("resampler%execute: unsupported interval method")
        end select
      end do
      select case(self%method)
        case(ts_mean)
          target(i) = target(i) / target_width
        case(ts_median)
          target(i) = omedian(median_values(1_i4:n_overlap))
      end select
    end do
  end subroutine execute_interval_1d

  subroutine execute_interval_2d(self, source, target)
    class(resampler_t), intent(in) :: self
    real(dp), intent(in) :: source(:, :)
    real(dp), intent(out) :: target(:, :)
    real(dp), allocatable :: median_values(:)
    real(dp) :: overlap, source_width, target_width, initial
    integer(i4) :: i, j, k, n_overlap
    integer(i8) :: lower, upper

    do i = 1_i4, size(target, 2)
      call self%init_interval_value(i, initial, target_width)
      target(:, i) = initial
      if (self%method == ts_median) allocate(median_values(self%last(i) - self%first(i) + 1_i4))
      do j = self%first(i), self%last(i)
        lower = max(self%source_bounds(1_i4, j), self%target_bounds(1_i4, i))
        upper = min(self%source_bounds(2_i4, j), self%target_bounds(2_i4, i))
        if (upper <= lower) cycle
        overlap = real(upper - lower, dp)
        select case(self%method)
          case(ts_mean)
            target(:, i) = target(:, i) + source(:, j) * overlap
          case(ts_sum)
            source_width = real(self%source_bounds(2_i4, j) - self%source_bounds(1_i4, j), dp)
            if (source_width <= 0.0_dp) call error_message("resampler%execute: invalid source interval")
            target(:, i) = target(:, i) + source(:, j) * overlap / source_width
          case(ts_min)
            target(:, i) = min(target(:, i), source(:, j))
          case(ts_max)
            target(:, i) = max(target(:, i), source(:, j))
          case(ts_median)
          case default
            call error_message("resampler%execute: unsupported interval method")
        end select
      end do
      if (self%method == ts_mean) target(:, i) = target(:, i) / target_width
      if (self%method == ts_median) then
        do k = 1_i4, size(target, 1)
          n_overlap = 0_i4
          do j = self%first(i), self%last(i)
            lower = max(self%source_bounds(1_i4, j), self%target_bounds(1_i4, i))
            upper = min(self%source_bounds(2_i4, j), self%target_bounds(2_i4, i))
            if (upper <= lower) cycle
            n_overlap = n_overlap + 1_i4
            median_values(n_overlap) = source(k, j)
          end do
          target(k, i) = omedian(median_values(1_i4:n_overlap))
        end do
        deallocate(median_values)
      end if
    end do
  end subroutine execute_interval_2d

  subroutine init_interval_value(self, target_index, value, target_width)
    class(resampler_t), intent(in) :: self
    integer(i4), intent(in) :: target_index
    real(dp), intent(out) :: value
    real(dp), intent(out) :: target_width

    target_width = real(self%target_bounds(2_i4, target_index) - self%target_bounds(1_i4, target_index), dp)
    select case(self%method)
      case(ts_mean, ts_sum)
        value = 0.0_dp
      case(ts_min)
        value = huge(1.0_dp)
      case(ts_max)
        value = -huge(1.0_dp)
      case(ts_median)
        value = 0.0_dp
      case default
        call error_message("resampler%execute: unsupported interval method")
    end select
  end subroutine init_interval_value

  subroutine resampler_validate_config(self)
    class(resampler_t), intent(in) :: self
    select case(self%support)
      case(ts_instant)
        if (self%method /= ts_none) call error_message("resampler%init: instant series needs ts_none method")
        select case(self%interpolation)
          case(ts_previous, ts_nearest, ts_linear)
          case default
            call error_message("resampler%init: invalid interpolation")
        end select
      case(ts_interval)
        select case(self%method)
          case(ts_mean, ts_sum, ts_min, ts_max, ts_median)
          case default
            call error_message("resampler%init: invalid interval method")
        end select
      case default
        call error_message("resampler%init: invalid support")
    end select
  end subroutine resampler_validate_config

  subroutine time_validate(self, require_bounds)
    class(time_t), intent(in) :: self
    logical, optional, intent(in) :: require_bounds
    integer(i4) :: i
    if (.not.allocated(self%values)) call error_message("time: uninitialized values")
    if (self%delta%total_seconds() <= 0_i8) call error_message("time: invalid delta")
    if (optval(require_bounds, .false.) .and. .not.self%has_bounds()) call error_message("time: uninitialized bounds")
    if (self%has_bounds()) then
      if (size(self%bounds, 1) /= 2_i4 .or. size(self%bounds, 2) /= size(self%values)) &
        call error_message("time: invalid bounds shape")
    end if
    do i = 1_i4, size(self%values)
      if (self%has_bounds()) then
        if (self%bounds(2_i4, i) <= self%bounds(1_i4, i)) call error_message("time: invalid bounds")
        if (self%values(i) < self%bounds(1_i4, i) .or. self%values(i) > self%bounds(2_i4, i)) &
          call error_message("time: value outside bounds")
      end if
      if (i > 1_i4) then
        if (self%values(i) <= self%values(i - 1_i4)) call error_message("time: values not monotonic")
        if (self%has_bounds()) then
          if (self%bounds(1_i4, i) < self%bounds(2_i4, i - 1_i4)) call error_message("time: overlapping bounds")
        end if
      end if
    end do
  end subroutine time_validate

  !> \brief Copy this axis into another axis without whole-object assignment.
  subroutine time_copy_to(self, dst)
    class(time_t), intent(in) :: self
    type(time_t), intent(inout) :: dst !< copied destination axis
    integer(i4), allocatable :: values(:), bounds(:, :)
    character(:), allocatable :: units

    call self%to_cf(values, bounds, units)
    if (self%has_bounds()) then
      call dst%init_cf(values, units, bounds=bounds, timestamp=self%timestamp)
    else
      call dst%init_cf(values, units, timestamp=instant_timestamp)
    end if
    dst%timestep = self%timestep
  end subroutine time_copy_to

  subroutine time_check_index(self, i, context)
    class(time_t), intent(in) :: self
    integer(i4), intent(in) :: i
    character(*), intent(in) :: context
    if (.not.allocated(self%values)) call error_message(context // ": uninitialized axis")
    if (i < 1_i4 .or. i > size(self%values)) call error_message(context // ": index out of range")
  end subroutine time_check_index

  subroutine time_value_seconds(self, ref_time, seconds)
    class(time_t), intent(in) :: self
    type(datetime), intent(in) :: ref_time
    integer(i8), allocatable, intent(out) :: seconds(:)
    type(timedelta) :: offset_delta
    integer(i8) :: offset, delta_seconds
    integer(i4) :: i
    offset_delta = self%ref_time - ref_time
    offset = offset_delta%total_seconds()
    delta_seconds = self%delta%total_seconds()
    allocate(seconds(size(self%values)))
    do i = 1_i4, size(self%values)
      seconds(i) = offset + int(self%values(i), i8) * delta_seconds
    end do
  end subroutine time_value_seconds

  subroutine time_bound_seconds(self, ref_time, seconds)
    class(time_t), intent(in) :: self
    type(datetime), intent(in) :: ref_time
    integer(i8), allocatable, intent(out) :: seconds(:, :)
    type(timedelta) :: offset_delta
    integer(i8) :: offset, delta_seconds
    integer(i4) :: i
    offset_delta = self%ref_time - ref_time
    offset = offset_delta%total_seconds()
    delta_seconds = self%delta%total_seconds()
    allocate(seconds(2_i4, size(self%values)))
    do i = 1_i4, size(self%values)
      seconds(1_i4, i) = offset + int(self%bounds(1_i4, i), i8) * delta_seconds
      seconds(2_i4, i) = offset + int(self%bounds(2_i4, i), i8) * delta_seconds
    end do
  end subroutine time_bound_seconds

  integer(i4) function time_integer_value(self, time) result(value)
    class(time_t), intent(in) :: self
    type(datetime), intent(in) :: time
    type(timedelta) :: diff
    real(dp) :: value_dp
    diff = time - self%ref_time
    value_dp = diff / self%delta
    value = nint(value_dp, i4)
  end function time_integer_value

  type(datetime) function time_next(current_time, timestep, delta, use_delta) result(next_time)
    type(datetime), intent(in) :: current_time
    integer(i4), intent(in) :: timestep
    character(*), optional, intent(in) :: delta
    logical, intent(in) :: use_delta

    if (use_delta) then
      if (.not.present(delta)) call error_message("time_next: missing delta")
      next_time = current_time + delta_from_string(delta)
      return
    end if
    select case(timestep)
      case(yearly)
        next_time = current_time%next_new_year()
      case(monthly)
        next_time = current_time%next_new_month()
      case(daily)
        next_time = current_time%next_new_day()
      case(varying, no_time)
        call error_message("time%init: invalid timestep")
      case default
        if (timestep <= 0_i4) call error_message("time%init: invalid timestep")
        next_time = current_time + timestep * delta_from_string("hours")
    end select
  end function time_next

end module mo_timeseries
