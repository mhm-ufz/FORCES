!> \file    mo_timeseries.f90
!> \copydoc mo_timeseries

!> \brief   Time-series representation and manipulation.
!> \details This module provides a one-dimensional time-series type with an
!!          explicit CF-compatible integer time axis. Datetime objects are used
!!          at API boundaries, while resampling calculations use integer time
!!          offsets.
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
                         start_timestamp, center_timestamp, end_timestamp
  use mo_kind, only: i4, i8, dp
  use mo_message, only: error_message
  use mo_orderpack, only: omedian
  use mo_utils, only: optval

  implicit none

  private

  public :: yearly, monthly, daily, no_time, hourly, varying
  public :: start_timestamp, center_timestamp, end_timestamp

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

  !> \class timeseries_axis_t
  !> \brief CF-compatible integer time axis.
  !> \details The axis stores integer time values and bounds together with their
  !!          reference time and unit delta. Public methods expose datetimes
  !!          where this is useful, but calculations can use the integer axis.
  type, public :: timeseries_axis_t
    integer(i4), allocatable :: values(:)        !< time coordinate values
    integer(i4), allocatable :: bounds(:, :)     !< time bounds with shape (2,n_times)
    character(:), allocatable :: units           !< CF time units
    type(datetime) :: ref_time                   !< reference time in CF units
    type(timedelta) :: delta                     !< one unit of the integer time axis
    integer(i4) :: timestep = daily              !< time-step indicator
    integer(i4) :: timestamp = end_timestamp     !< timestamp location selector
  contains
    procedure, public :: init => timeseries_axis_init
    procedure, public :: init_cf => timeseries_axis_init_cf
    procedure, public :: n_times => timeseries_axis_n_times
    procedure, public :: time => timeseries_axis_time
    procedure, public :: lower => timeseries_axis_lower
    procedure, public :: upper => timeseries_axis_upper
    procedure, public :: to_cf => timeseries_axis_to_cf
    procedure, public :: window_indices => timeseries_axis_window_indices
    procedure, public :: resampled => timeseries_axis_resampled
  end type timeseries_axis_t

  !> \class timeseries_t
  !> \brief One real-valued time series on an explicit time axis.
  type, public :: timeseries_t
    type(timeseries_axis_t) :: axis               !< time axis
    real(dp), allocatable :: values(:)            !< time-series values
    integer(i4) :: support = ts_interval          !< \ref ts_instant or \ref ts_interval
    integer(i4) :: method = ts_mean               !< interval method
  contains
    procedure, public :: init => timeseries_init
    procedure, public :: window => timeseries_window
    procedure, public :: resample => timeseries_resample
  end type timeseries_t

contains

  !> \brief Initialize a contiguous time axis from datetimes.
  subroutine timeseries_axis_init(self, timeframe_start, timeframe_end, timestep, delta, timestamp, ref_time)
    class(timeseries_axis_t), intent(inout) :: self
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

    if (timeframe_end <= timeframe_start) call error_message("timeseries_axis%init: invalid time frame")
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
      current_time = timeseries_axis_next(previous_time, self%timestep, delta, present(delta))
      if (current_time > timeframe_end) call error_message("timeseries_axis%init: timestep does not align")
      n_times = n_times + 1_i4
      previous_time = current_time
    end do

    allocate(self%values(n_times), self%bounds(2_i4, n_times))
    previous_time = timeframe_start
    do i = 1_i4, n_times
      current_time = timeseries_axis_next(previous_time, self%timestep, delta, present(delta))
      call time_values(self%ref_time, previous_time, current_time, self%delta, self%timestamp, t_start, t_end, t_stamp)
      self%bounds(:, i) = [t_start, t_end]
      self%values(i) = t_stamp
      previous_time = current_time
    end do
  end subroutine timeseries_axis_init

  !> \brief Initialize a time axis from CF-style integer values and optional bounds.
  subroutine timeseries_axis_init_cf(self, values, units, bounds, timestamp)
    class(timeseries_axis_t), intent(inout) :: self
    integer(i4), intent(in) :: values(:) !< CF time coordinate values
    character(*), intent(in) :: units !< CF time units
    integer(i4), optional, intent(in) :: bounds(:, :) !< optional bounds with shape (2,n_times)
    integer(i4), optional, intent(in) :: timestamp !< timestamp location if bounds are omitted (default: \ref end_timestamp)

    if (size(values) < 1_i4) call error_message("timeseries_axis%init_cf: empty time axis")
    if (allocated(self%values)) deallocate(self%values)
    if (allocated(self%bounds)) deallocate(self%bounds)
    self%timestamp = optval(timestamp, end_timestamp)
    self%units = trim(units)
    call decode_cf_time_units(self%units, self%delta, self%ref_time)
    allocate(self%values(size(values)), source=values)
    if (present(bounds)) then
      if (size(bounds, 1) /= 2_i4 .or. size(bounds, 2) /= size(values)) &
        call error_message("timeseries_axis%init_cf: invalid bounds shape")
      allocate(self%bounds(2_i4, size(values)), source=bounds)
    else
      call infer_bounds(self%values, self%timestamp, self%bounds)
    end if
    self%timestep = infer_timestep(self%bounds, self%delta, self%ref_time)
    call validate_axis(self)
  end subroutine timeseries_axis_init_cf

  !> \brief Return number of time steps.
  integer(i4) function timeseries_axis_n_times(self) result(n_times)
    class(timeseries_axis_t), intent(in) :: self
    if (.not.allocated(self%values)) then
      n_times = 0_i4
    else
      n_times = size(self%values)
    end if
  end function timeseries_axis_n_times

  !> \brief Return timestamp as datetime.
  type(datetime) function timeseries_axis_time(self, i) result(time)
    class(timeseries_axis_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call check_axis_index(self, i, "timeseries_axis%time")
    time = self%ref_time + self%values(i) * self%delta
  end function timeseries_axis_time

  !> \brief Return lower interval bound as datetime.
  type(datetime) function timeseries_axis_lower(self, i) result(time)
    class(timeseries_axis_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call check_axis_index(self, i, "timeseries_axis%lower")
    time = self%ref_time + self%bounds(1_i4, i) * self%delta
  end function timeseries_axis_lower

  !> \brief Return upper interval bound as datetime.
  type(datetime) function timeseries_axis_upper(self, i) result(time)
    class(timeseries_axis_t), intent(in) :: self
    integer(i4), intent(in) :: i !< one-based time index
    call check_axis_index(self, i, "timeseries_axis%upper")
    time = self%ref_time + self%bounds(2_i4, i) * self%delta
  end function timeseries_axis_upper

  !> \brief Copy CF-style values, bounds and units from the axis.
  subroutine timeseries_axis_to_cf(self, values, bounds, units)
    class(timeseries_axis_t), intent(in) :: self
    integer(i4), allocatable, intent(out) :: values(:) !< CF time coordinate values
    integer(i4), allocatable, intent(out) :: bounds(:, :) !< CF time bounds
    character(:), allocatable, intent(out) :: units !< CF time units
    if (.not.allocated(self%values)) call error_message("timeseries_axis%to_cf: uninitialized axis")
    allocate(values(size(self%values)), source=self%values)
    allocate(bounds(2_i4, size(self%values)), source=self%bounds)
    units = self%units
  end subroutine timeseries_axis_to_cf

  !> \brief Return first and last indices inside a datetime window.
  subroutine timeseries_axis_window_indices(self, timeframe_start, timeframe_end, first, last)
    class(timeseries_axis_t), intent(in) :: self
    type(datetime), intent(in) :: timeframe_start !< start of selected time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of selected time frame, including
    integer(i4), intent(out) :: first !< first selected one-based index
    integer(i4), intent(out) :: last !< last selected one-based index

    integer(i4) :: start_value, end_value
    integer(i4) :: n

    if (timeframe_end <= timeframe_start) call error_message("timeseries_axis%window_indices: invalid time frame")
    if (.not.allocated(self%values)) call error_message("timeseries_axis%window_indices: uninitialized axis")
    start_value = datetime_value(self, timeframe_start)
    end_value = datetime_value(self, timeframe_end)
    n = size(self%values)
    first = 1_i4
    do while(first <= n .and. self%values(first) <= start_value)
      first = first + 1_i4
    end do
    last = n
    do while(last >= 1_i4 .and. self%values(last) > end_value)
      last = last - 1_i4
    end do
    if (first > last) call error_message("timeseries_axis%window_indices: empty time frame")
  end subroutine timeseries_axis_window_indices

  !> \brief Derive a contiguous axis over the same time frame with a new resolution.
  subroutine timeseries_axis_resampled(self, out, timestep, delta, timestamp, ref_time, timeframe_start, timeframe_end)
    class(timeseries_axis_t), intent(in) :: self
    type(timeseries_axis_t), intent(out) :: out !< derived target axis
    integer(i4), optional, intent(in) :: timestep !< target time-step indicator or positive hourly step (default: \ref daily)
    character(*), optional, intent(in) :: delta !< explicit target unit
    integer(i4), optional, intent(in) :: timestamp !< target timestamp selector (default: current axis timestamp)
    type(datetime), optional, intent(in) :: ref_time !< target reference time (default: current axis reference time)
    type(datetime), optional, intent(in) :: timeframe_start !< target time-frame start (default: current lower bound)
    type(datetime), optional, intent(in) :: timeframe_end !< target time-frame end (default: current upper bound)

    integer(i4) :: timestamp_
    type(datetime) :: start_time, end_time, source_start, source_end

    if (.not.allocated(self%bounds)) call error_message("timeseries_axis%resampled: uninitialized axis")
    timestamp_ = optval(timestamp, self%timestamp)
    source_start = self%lower(1_i4)
    source_end = self%upper(self%n_times())
    start_time = source_start
    end_time = source_end
    if (present(timeframe_start)) then
      start_time = timeframe_start
    end if
    if (present(timeframe_end)) then
      end_time = timeframe_end
    end if
    if (start_time < source_start .or. end_time > source_end .or. end_time <= start_time) &
      call error_message("timeseries_axis%resampled: invalid time frame")
    if (present(ref_time)) then
      call out%init(start_time, end_time, timestep=timestep, delta=delta, &
                    timestamp=timestamp_, ref_time=ref_time)
    else
      call out%init(start_time, end_time, timestep=timestep, delta=delta, &
                    timestamp=timestamp_, ref_time=self%ref_time)
    end if
  end subroutine timeseries_axis_resampled

  !> \brief Initialize a time series.
  subroutine timeseries_init(self, values, axis, support, method)
    class(timeseries_t), intent(inout) :: self
    real(dp), intent(in) :: values(:) !< time-series values
    type(timeseries_axis_t), intent(in) :: axis !< time axis
    integer(i4), optional, intent(in) :: support !< \ref ts_instant or \ref ts_interval (default: \ref ts_interval)
    integer(i4), optional, intent(in) :: method !< interval method (default: \ref ts_mean for intervals, \ref ts_none for instants)

    if (axis%n_times() /= size(values)) call error_message("timeseries%init: value and axis sizes differ")
    self%axis = axis
    if (allocated(self%values)) deallocate(self%values)
    allocate(self%values(size(values)), source=values)
    self%support = optval(support, ts_interval)
    if (present(method)) then
      self%method = method
    else if (self%support == ts_interval) then
      self%method = ts_mean
    else
      self%method = ts_none
    end if
    call validate_series(self)
  end subroutine timeseries_init

  !> \brief Copy a datetime window into a new time series.
  subroutine timeseries_window(self, timeframe_start, timeframe_end, out)
    class(timeseries_t), intent(in) :: self
    type(datetime), intent(in) :: timeframe_start !< start of selected time frame, excluding
    type(datetime), intent(in) :: timeframe_end !< end of selected time frame, including
    type(timeseries_t), intent(out) :: out !< selected time series

    integer(i4) :: first, last
    type(timeseries_axis_t) :: axis
    type(datetime) :: lower_bound, upper_bound

    call self%axis%window_indices(timeframe_start, timeframe_end, first, last)
    if (self%support == ts_interval) then
      lower_bound = self%axis%lower(first)
      upper_bound = self%axis%upper(last)
      if (lower_bound /= timeframe_start .or. upper_bound /= timeframe_end) &
        call error_message("timeseries%window: interval window must match existing bounds")
    end if
    axis = self%axis
    axis%values = self%axis%values(first:last)
    axis%bounds = self%axis%bounds(:, first:last)
    call out%init(self%values(first:last), axis, support=self%support, method=self%method)
  end subroutine timeseries_window

  !> \brief Resample a time series to a target axis.
  subroutine timeseries_resample(self, target_axis, out, interpolation)
    class(timeseries_t), intent(in) :: self
    type(timeseries_axis_t), intent(in) :: target_axis !< target time axis
    type(timeseries_t), intent(out) :: out !< resampled time series
    integer(i4), optional, intent(in) :: interpolation !< interpolation selector for instant data (default: \ref ts_nearest)

    real(dp), allocatable :: out_values(:)

    call validate_series(self)
    call validate_axis(target_axis)
    allocate(out_values(target_axis%n_times()))
    select case(self%support)
      case(ts_instant)
        call resample_instant(self, target_axis, optval(interpolation, ts_nearest), out_values)
      case(ts_interval)
        call resample_interval(self, target_axis, out_values)
      case default
        call error_message("timeseries%resample: invalid support")
    end select
    call out%init(out_values, target_axis, support=self%support, method=self%method)
  end subroutine timeseries_resample

  subroutine resample_instant(series, target_axis, interpolation, out_values)
    type(timeseries_t), intent(in) :: series
    type(timeseries_axis_t), intent(in) :: target_axis
    integer(i4), intent(in) :: interpolation
    real(dp), intent(out) :: out_values(:)

    integer(i8), allocatable :: source_time(:), target_time(:)
    integer(i4) :: i, j
    real(dp) :: frac

    call axis_value_seconds(series%axis, series%axis%ref_time, source_time)
    call axis_value_seconds(target_axis, series%axis%ref_time, target_time)
    do i = 1_i4, size(target_time)
      select case(interpolation)
        case(ts_previous)
          j = previous_index(source_time, target_time(i))
          if (j < 1_i4) call error_message("timeseries%resample: target time before source range")
          out_values(i) = series%values(j)
        case(ts_nearest)
          j = nearest_index(source_time, target_time(i))
          out_values(i) = series%values(j)
        case(ts_linear)
          if (target_time(i) < source_time(1_i4) .or. target_time(i) > source_time(size(source_time))) &
            call error_message("timeseries%resample: target time outside source range")
          j = previous_index(source_time, target_time(i))
          if (source_time(j) == target_time(i) .or. j == size(source_time)) then
            out_values(i) = series%values(j)
          else
            frac = real(target_time(i) - source_time(j), dp) / real(source_time(j + 1_i4) - source_time(j), dp)
            out_values(i) = series%values(j) + frac * (series%values(j + 1_i4) - series%values(j))
          end if
        case default
          call error_message("timeseries%resample: invalid interpolation")
      end select
    end do
  end subroutine resample_instant

  subroutine resample_interval(series, target_axis, out_values)
    type(timeseries_t), intent(in) :: series
    type(timeseries_axis_t), intent(in) :: target_axis
    real(dp), intent(out) :: out_values(:)

    integer(i8), allocatable :: source_bounds(:, :), target_bounds(:, :)
    real(dp), allocatable :: median_values(:)
    real(dp) :: total_overlap, source_width, target_width, overlap
    integer(i4) :: i, j, n_overlap
    integer(i8) :: lower, upper

    call axis_bound_seconds(series%axis, series%axis%ref_time, source_bounds)
    call axis_bound_seconds(target_axis, series%axis%ref_time, target_bounds)

    do i = 1_i4, size(target_bounds, 2)
      total_overlap = 0.0_dp
      target_width = real(target_bounds(2_i4, i) - target_bounds(1_i4, i), dp)
      select case(series%method)
        case(ts_mean, ts_sum)
          out_values(i) = 0.0_dp
        case(ts_min)
          out_values(i) = huge(1.0_dp)
        case(ts_max)
          out_values(i) = -huge(1.0_dp)
        case(ts_median)
          if (allocated(median_values)) deallocate(median_values)
          allocate(median_values(size(series%values)))
          n_overlap = 0_i4
        case default
          call error_message("timeseries%resample: unsupported interval method")
      end select

      do j = 1_i4, size(source_bounds, 2)
        lower = max(source_bounds(1_i4, j), target_bounds(1_i4, i))
        upper = min(source_bounds(2_i4, j), target_bounds(2_i4, i))
        if (upper <= lower) cycle
        overlap = real(upper - lower, dp)
        total_overlap = total_overlap + overlap
        select case(series%method)
          case(ts_mean)
            out_values(i) = out_values(i) + series%values(j) * overlap
          case(ts_sum)
            source_width = real(source_bounds(2_i4, j) - source_bounds(1_i4, j), dp)
            if (source_width <= 0.0_dp) call error_message("timeseries%resample: invalid source interval")
            out_values(i) = out_values(i) + series%values(j) * overlap / source_width
          case(ts_min)
            out_values(i) = min(out_values(i), series%values(j))
          case(ts_max)
            out_values(i) = max(out_values(i), series%values(j))
          case(ts_median)
            n_overlap = n_overlap + 1_i4
            median_values(n_overlap) = series%values(j)
        end select
      end do

      if (total_overlap <= 0.0_dp) call error_message("timeseries%resample: target interval not covered")
      if (total_overlap /= target_width) call error_message("timeseries%resample: target interval partially covered")
      select case(series%method)
        case(ts_mean)
          out_values(i) = out_values(i) / total_overlap
        case(ts_median)
          out_values(i) = omedian(median_values(1_i4:n_overlap))
      end select
    end do
  end subroutine resample_interval

  subroutine validate_series(series)
    type(timeseries_t), intent(in) :: series
    call validate_axis(series%axis)
    if (.not.allocated(series%values)) call error_message("timeseries: uninitialized values")
    if (size(series%values) /= series%axis%n_times()) call error_message("timeseries: value and axis sizes differ")
    select case(series%support)
      case(ts_instant)
        if (series%method /= ts_none) call error_message("timeseries: instant series needs ts_none method")
      case(ts_interval)
        select case(series%method)
          case(ts_mean, ts_sum, ts_min, ts_max, ts_median)
          case default
            call error_message("timeseries: invalid interval method")
        end select
      case default
        call error_message("timeseries: invalid support")
    end select
  end subroutine validate_series

  subroutine validate_axis(axis)
    type(timeseries_axis_t), intent(in) :: axis
    integer(i4) :: i
    if (.not.allocated(axis%values)) call error_message("timeseries_axis: uninitialized values")
    if (.not.allocated(axis%bounds)) call error_message("timeseries_axis: uninitialized bounds")
    if (size(axis%bounds, 1) /= 2_i4 .or. size(axis%bounds, 2) /= size(axis%values)) &
      call error_message("timeseries_axis: invalid bounds shape")
    if (axis%delta%total_seconds() <= 0_i8) call error_message("timeseries_axis: invalid delta")
    do i = 1_i4, size(axis%values)
      if (axis%bounds(2_i4, i) <= axis%bounds(1_i4, i)) call error_message("timeseries_axis: invalid bounds")
      if (axis%values(i) < axis%bounds(1_i4, i) .or. axis%values(i) > axis%bounds(2_i4, i)) &
        call error_message("timeseries_axis: value outside bounds")
      if (i > 1_i4) then
        if (axis%values(i) <= axis%values(i - 1_i4)) call error_message("timeseries_axis: values not monotonic")
        if (axis%bounds(1_i4, i) < axis%bounds(2_i4, i - 1_i4)) &
          call error_message("timeseries_axis: overlapping bounds")
      end if
    end do
  end subroutine validate_axis

  subroutine check_axis_index(axis, i, context)
    type(timeseries_axis_t), intent(in) :: axis
    integer(i4), intent(in) :: i
    character(*), intent(in) :: context
    if (.not.allocated(axis%values)) call error_message(context // ": uninitialized axis")
    if (i < 1_i4 .or. i > size(axis%values)) call error_message(context // ": index out of range")
  end subroutine check_axis_index

  subroutine axis_value_seconds(axis, ref_time, seconds)
    type(timeseries_axis_t), intent(in) :: axis
    type(datetime), intent(in) :: ref_time
    integer(i8), allocatable, intent(out) :: seconds(:)
    type(timedelta) :: offset_delta
    integer(i8) :: offset, delta_seconds
    integer(i4) :: i
    offset_delta = axis%ref_time - ref_time
    offset = offset_delta%total_seconds()
    delta_seconds = axis%delta%total_seconds()
    allocate(seconds(size(axis%values)))
    do i = 1_i4, size(axis%values)
      seconds(i) = offset + int(axis%values(i), i8) * delta_seconds
    end do
  end subroutine axis_value_seconds

  subroutine axis_bound_seconds(axis, ref_time, seconds)
    type(timeseries_axis_t), intent(in) :: axis
    type(datetime), intent(in) :: ref_time
    integer(i8), allocatable, intent(out) :: seconds(:, :)
    type(timedelta) :: offset_delta
    integer(i8) :: offset, delta_seconds
    integer(i4) :: i
    offset_delta = axis%ref_time - ref_time
    offset = offset_delta%total_seconds()
    delta_seconds = axis%delta%total_seconds()
    allocate(seconds(2_i4, size(axis%values)))
    do i = 1_i4, size(axis%values)
      seconds(1_i4, i) = offset + int(axis%bounds(1_i4, i), i8) * delta_seconds
      seconds(2_i4, i) = offset + int(axis%bounds(2_i4, i), i8) * delta_seconds
    end do
  end subroutine axis_bound_seconds

  integer(i4) function previous_index(values, target) result(ind)
    integer(i8), intent(in) :: values(:)
    integer(i8), intent(in) :: target
    integer(i4) :: i
    ind = 0_i4
    do i = 1_i4, size(values)
      if (values(i) > target) exit
      ind = i
    end do
  end function previous_index

  integer(i4) function nearest_index(values, target) result(ind)
    integer(i8), intent(in) :: values(:)
    integer(i8), intent(in) :: target
    integer(i4) :: i
    integer(i8) :: best_dist, dist
    ind = 1_i4
    best_dist = abs(values(1_i4) - target)
    do i = 2_i4, size(values)
      dist = abs(values(i) - target)
      if (dist < best_dist) then
        ind = i
        best_dist = dist
      end if
    end do
  end function nearest_index

  integer(i4) function datetime_value(axis, time) result(value)
    type(timeseries_axis_t), intent(in) :: axis
    type(datetime), intent(in) :: time
    type(timedelta) :: diff
    real(dp) :: value_dp
    diff = time - axis%ref_time
    value_dp = diff / axis%delta
    value = nint(value_dp, i4)
  end function datetime_value

  type(datetime) function timeseries_axis_next(current_time, timestep, delta, use_delta) result(next_time)
    type(datetime), intent(in) :: current_time
    integer(i4), intent(in) :: timestep
    character(*), optional, intent(in) :: delta
    logical, intent(in) :: use_delta

    if (use_delta) then
      if (.not.present(delta)) call error_message("timeseries_axis_next: missing delta")
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
        call error_message("timeseries_axis%init: invalid timestep")
      case default
        if (timestep <= 0_i4) call error_message("timeseries_axis%init: invalid timestep")
        next_time = current_time + timestep * delta_from_string("hours")
    end select
  end function timeseries_axis_next

  subroutine infer_bounds(values, timestamp, bounds)
    integer(i4), intent(in) :: values(:)
    integer(i4), intent(in) :: timestamp
    integer(i4), allocatable, intent(out) :: bounds(:, :)
    integer(i4) :: n, dt, i

    n = size(values)
    allocate(bounds(2_i4, n))
    if (n == 1_i4) then
      dt = 1_i4
    else
      dt = values(2_i4) - values(1_i4)
    end if
    select case(timestamp)
      case(end_timestamp)
        bounds(2_i4, :) = values
        bounds(1_i4, 1_i4) = values(1_i4) - dt
        if (n > 1_i4) bounds(1_i4, 2_i4:n) = values(1_i4:n - 1_i4)
      case(start_timestamp)
        bounds(1_i4, :) = values
        if (n > 1_i4) bounds(2_i4, 1_i4:n - 1_i4) = values(2_i4:n)
        bounds(2_i4, n) = values(n) + dt
      case(center_timestamp)
        if (mod(dt, 2_i4) /= 0_i4) call error_message("timeseries_axis%init_cf: center bounds need even step")
        do i = 1_i4, n
          bounds(1_i4, i) = values(i) - dt / 2_i4
          bounds(2_i4, i) = values(i) + dt / 2_i4
        end do
      case default
        call error_message("timeseries_axis%init_cf: invalid timestamp")
    end select
  end subroutine infer_bounds

  integer(i4) function infer_timestep(bounds, delta, ref_time) result(timestep)
    integer(i4), intent(in) :: bounds(:, :)
    type(timedelta), intent(in) :: delta
    type(datetime), intent(in) :: ref_time
    integer(i4), allocatable :: diffs(:)
    type(timedelta) :: loc_delta
    type(datetime) :: loc_date
    logical :: is_monthly, is_yearly
    integer(i4) :: i, dt
    real(dp) :: dt_dp

    allocate(diffs(size(bounds, 2)))
    diffs = bounds(2_i4, :) - bounds(1_i4, :)
    dt = diffs(1_i4)
    if (all(diffs == dt)) then
      loc_delta = dt * delta
      if (loc_delta%total_seconds() == 86400_i8) then
        timestep = daily
      else if (loc_delta%total_seconds() == 3600_i8) then
        timestep = hourly
      else
        dt_dp = loc_delta%total_seconds() / 3600.0_dp
        timestep = nint(dt_dp, i4)
        if (abs(dt_dp - real(timestep, dp)) > 1.0e-12_dp) timestep = varying
      end if
    else
      is_monthly = .true.
      is_yearly = .true.
      do i = 1_i4, size(bounds, 2)
        loc_date = ref_time + bounds(2_i4, i) * delta
        is_monthly = is_monthly .and. loc_date%is_new_month()
        is_yearly = is_yearly .and. loc_date%is_new_year()
      end do
      if (is_yearly) then
        timestep = yearly
      else if (is_monthly) then
        timestep = monthly
      else
        timestep = varying
      end if
    end if
  end function infer_timestep

end module mo_timeseries
