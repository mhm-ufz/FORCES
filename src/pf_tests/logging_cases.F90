! Subprocess fixtures for fatal diagnostics and CLI output; no scientific files are written.
program logging_cases
  use mo_message, only: message, error_message, use_log, def_log_scope, show_msg, show_warn, show_err
  use mo_forces_message, only: forces_error => error_message, forces_warning => warn_message, forces_message => message
  use mo_logging, only: log_level, LOG_INFO, log_set_config, log_unit, log_unit_error
  use mo_cli, only: cli_parser
  use mo_append, only: append
  use mo_moment, only: stddev
  use mo_julian, only: setCalendar
  use mo_kind, only: dp
#ifdef FORCES_WITH_NETCDF
  use mo_netcdf, only: NcDataset
#endif
#ifdef MPI
  use mpi, only: MPI_Init, MPI_Finalize
#endif
  implicit none
  character(len=64) :: scenario
  type(cli_parser) :: parser
  integer, allocatable :: left(:,:)
  integer :: right(1,2), record_length
  real(dp) :: result
#ifdef FORCES_WITH_NETCDF
  type(NcDataset) :: dataset
#endif
#ifdef MPI
  integer :: ierr
#endif
  ! Keep the blank scenario argument last so the parser can consume preceding options.
  call get_command_argument(command_argument_count(), scenario)
#ifdef MPI
  ! Help/version exit inside the parser; their plain writers require no MPI calls.
  if (scenario /= 'cli-help' .and. scenario /= 'cli-version') call MPI_Init(ierr)
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
#ifdef FORCES_WITH_NETCDF
  case('netcdf-mode')
    dataset = NcDataset('unused', '?')
#endif
  case('cli-help', 'cli-version', 'scope-default', 'scope-bare', 'scope-root', 'scope-forces', &
    'cli-file', 'cli-file-filtered', 'cli-file-missing', 'cli-file-empty', 'cli-file-fallback')
    use_log = .false.
    show_msg = .false.
    log_level = 0
    call log_set_config(log_scope_filter='other')
    parser = cli_parser(prog='logging-fixture', description='Plain CLI fixture.', &
      add_help_option=.true., add_version_option=.true., version='1.2.3', add_logger_options=.true.)
    if (use_log) error stop 2
    ! Also exercise presentation with an already active, restrictive logger.
    if (scenario == 'cli-help' .or. scenario == 'cli-version') use_log = .true.
    call parser%add_option(name='scenario', blank=.true., help='Fixture scenario')
    call parser%parse()
    ! Successful help/version parsing stops above, before configuring logging.
    if (.not. use_log) error stop 2
    log_level = LOG_INFO
    call message('root record', scope='')
    call forces_message('library record')
    if (scenario == 'cli-file' .or. scenario == 'cli-file-filtered') then
      if (log_unit /= log_unit_error) error stop 2
      inquire(unit=log_unit, recl=record_length)
      if (record_length /= 65536) error stop 2
      call forces_message(repeat('x', 4096))
      call forces_warning('warning record')
      call forces_error('recoverable error record', raise=.false.)
    end if
  case default
    call error_message('Unknown logging fixture scenario', scope='')
  end select
#ifdef MPI
  call MPI_Finalize(ierr)
#endif
  ! Fatal fixtures must not return normally.
  if (index(trim(scenario), 'scope-') /= 1 .and. index(trim(scenario), 'cli-file') /= 1) error stop 2
end program logging_cases
