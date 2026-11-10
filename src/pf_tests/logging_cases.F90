! Subprocess fixtures for fatal diagnostics and CLI output; no scientific files are written.
program logging_cases
  use mo_message, only: message, error_message, use_log, def_log_scope, show_msg, show_warn, show_err
  use mo_forces_message, only: forces_error => error_message, forces_warning => warn_message, forces_message => message
  use mo_logging, only: log_level, LOG_INFO, log_set_config
  use mo_append, only: append
  use mo_moment, only: stddev
  use mo_julian, only: setCalendar
  use mo_kind, only: dp
#ifdef MPI
  use mpi, only: MPI_Init, MPI_Finalize
#endif
  implicit none
  character(len=64) :: scenario
  integer, allocatable :: left(:,:)
  integer :: right(1,2)
  real(dp) :: result
#ifdef MPI
  integer :: ierr
#endif
  call get_command_argument(command_argument_count(), scenario)
#ifdef MPI
  call MPI_Init(ierr)
#endif
  log_level = LOG_INFO
  def_log_scope = 'mhm'
  select case(trim(scenario))
  case('fatal-plain')
    call forces_error('sentinel fatal')
  case('fatal-filtered')
    use_log = .true.
    call log_set_config(log_scope_filter='other')
    call forces_error('sentinel fatal')
  case('warning-fatal')
    use_log = .true.
    call log_set_config(log_scope_filter='other')
    call forces_warning('sentinel warning', raise=.true.)
  case('fatal-plain-hidden')
    call forces_error('sentinel hidden', show=.false.)
  case('fatal-hidden')
    use_log = .true.
    call forces_error('sentinel hidden', show=.false.)
  case('fatal-global-hidden')
    show_err = .false.
    call forces_error('sentinel hidden')
  case('warning-hidden')
    use_log = .true.
    call forces_warning('sentinel hidden', raise=.true., show=.false.)
  case('fatal-global-visible')
    use_log = .true.
    show_err = .false.
    call forces_error('sentinel visible')
  case('warning-global-visible')
    use_log = .true.
    show_warn = .false.
    call forces_warning('sentinel visible', raise=.true.)
  case('fatal-quiet')
    use_log = .true.
    log_level = 0
    call forces_error('sentinel quiet')
  case('append-shape')
    allocate(left(1,1))
    left = 0
    right = 0
    call append(left, right)
  case('moment-size')
    result = stddev([1.0_dp])
  case('calendar')
    call setCalendar('invalid')
  case default
    call error_message('Unknown logging fixture scenario', scope='')
  end select
#ifdef MPI
  call MPI_Finalize(ierr)
#endif
  ! Fatal fixtures must not return normally.
  error stop 2
end program logging_cases
