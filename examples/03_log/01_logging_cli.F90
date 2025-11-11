!> \file    03_log/01_logging_cli.F90
!> \copydoc logging_cli

!> \example 03_log/01_logging_cli.F90
!> \copydoc logging_cli

!> \brief   Demonstrates configuring and using the logging framework via `mo_cli`.
!> \details The program exposes all logger command-line options, optionally redirects
!!          output into a user-provided file, and emits messages for every log level,
!!          including the plain/text helper macros. This serves as the canonical
!!          reference for integrating logging into an application.
!> \authors Sebastian Mueller
!> \date    Mar 2025
#include "logging.h"
program logging_cli
  use mo_logging
  use mo_cli, only : cli_parser
  implicit none
  integer :: unit
  type(cli_parser) :: parser

  parser = cli_parser( &
    description='Program with cli and logger.', &
    add_help_option=.true., add_logger_options=.true.)
  ! add log-path option
  call parser%add_option(name='log-path', help='Log file path.', has_value=.true.)
  call parser%parse()

  if (parser%option_was_read('log-path')) then
    open(newunit=unit, file=parser%option_value('log-path'), status='replace', action='write')
    log_info(*) 'Logging to file: ', parser%option_value('log-path') ! this is still printed
    ! redirect log units to the opened file
    log_unit = unit
    log_unit_error = unit
  end if

  ! log messages with different levels
  log_fatal(*) 'fatal that should stop the program'
  log_error(*) 'error that should be logged'
  log_warn(*) 'warning message'
  log_info(*) 'information message'
  log_debug(*) 'debug message'
  log_trace(*) 'trace message for detailed debugging'
  log_subtrace(*) 'subtrace message for very detailed debugging'

  ! special cases
  log_text(*) 'info text without level'
  log_write(LOG_INFO,*) 'write info macro'
  log_plain(LOG_WARN,*) 'plain warn macro without level'
  log_core(0,LOG_ERROR,*) 'core 0 error macro for MPI'
  log_plain_core(0,LOG_FATAL,*) 'plain core 0 fatal macro without level for MPI'
  log_root(LOG_INFO,*) 'root info macro for MPI'
  log_plain_root(LOG_WARN,*) 'plain root warn macro without level for MPI'

  ! close log file if opened
  if (parser%option_was_read('log-path')) close(unit)
end program logging_cli
