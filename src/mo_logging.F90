#include "logging.h"
!> \file    mo_logging.F90
!> \copydoc mo_logging

!> \brief   Module providing a logging framework.
!> \version 0.1
!> \authors Daan van Vugt, Sebastian Mueller, Robert Schweppe
!> \date    Sep 2022
!> \copyright flogging was originally released under the MIT license.
!> \details A simple logging framework derived from flogging (https://github.com/DaanVanVugt/flogging).
!!          To use logging you need to include `logging.h` at the top of your fortran file.
!!
!!          \note The file needs to processed by the pre-processor.
!!
!!          Afterwards you have a list of logging routines that could be used instead of `write`:
!!            - `log_fatal(format)`: level 1
!!            - `log_error(format)`: level 2
!!            - `log_warn(format)`: level 3
!!            - `log_info(format)`: level 4
!!            - `log_debug(format)`: level 5
!!            - `log_trace(format)`: level 6
!!            - `log_subtrace(format)`: level 7
!!          as required.
!!
!!          The following example demonstrates the functionality. The `mo_cli` module incorporates logger settings:
!!          \code{.f90}
!!          #include 'logging.h'
!!          program test_log
!!            use mo_logging
!!            use mo_cli, only : cli_parser
!!            implicit none
!!            type(cli_parser) :: parser
!!            parser = cli_parser( &
!!              description='Program with cli and logger.', &
!!              add_help_option=.true., &
!!              add_logger_options=.true.)
!!            call parser%parse()
!!            log_fatal(*) 'fatal'
!!            log_error(*) 'error'
!!            log_warn(*) 'warn'
!!            log_info(*) 'info'
!!            log_debug(*) 'debug'
!!            log_trace(*) 'trace'
!!            log_subtrace(*) 'subtrace'
!!          end program test_log
!!          \endcode
!!          You can call the program with:
!!          \code{.sh}
!!          $ ./prog --quiet
!!            test.F90:12    FATAL fatal
!!            test.F90:13    ERROR error
!!            test.F90:14     WARN warn
!!          \endcode
!!          You can see all cli logger options with:
!!          \code{.sh}
!!          $ ./prog -h
!!          \endcode
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_logging
  ! Copyright (c) 2016 Daan van Vugt
  !
  ! Permission is hereby granted, free of charge, to any person
  ! obtaining a copy of this software and associated documentation
  ! files (the "Software"), to deal in the Software without
  ! restriction, including without limitation the rights to use,
  ! copy, modify, merge, publish, distribute, sublicense, and/or sell
  ! copies of the Software, and to permit persons to whom the
  ! Software is furnished to do so, subject to the following
  ! conditions:
  !
  ! The above copyright notice and this permission notice shall be
  ! included in all copies or substantial portions of the Software.
  !
  ! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  ! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  ! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  ! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  ! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  ! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  ! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  ! OTHER DEALINGS IN THE SOFTWARE.

#ifdef NAG
  use f90_unix_env, only: isatty
  use f90_unix_env, only: gethostname
#endif
#ifdef INTEL
  use ifport, only: isatty
  use ifport, only: hostnm
#endif
  use mo_constants, only: stdout=>nout, stderr=>nerr

  implicit none
  ! Log levels
  integer, public, parameter :: NUM_LOG_LEVELS = 7 !< 1 through 7 (fatal through trace)
  integer, public, parameter :: LOG_FATAL = LOG_LEVEL_FATAL_DEF !< = 1, Runtime error causing termination
  integer, public, parameter :: LOG_ERROR = LOG_LEVEL_ERROR_DEF !< = 2, Runtime error
  integer, public, parameter :: LOG_WARN  = LOG_LEVEL_WARN_DEF !< = 3, Warning, but we can continue
  integer, public, parameter :: LOG_INFO  = LOG_LEVEL_INFO_DEF !< = 4, Interesting events
  integer, public, parameter :: LOG_DEBUG = LOG_LEVEL_DEBUG_DEF !< = 5, Detailed debug output, disable by compiling your program with -DDISABLE_LOG_DEBUG
  integer, public, parameter :: LOG_TRACE = LOG_LEVEL_TRACE_DEF !< = 6, Extremely detailed output, compile your program with -DENABLE_LOG_TRACE to enable
  integer, public, parameter :: LOG_SUBTRACE = LOG_LEVEL_SUBTRACE_DEF !< = 7, More Extremely detailed output, compile your program with -DENABLE_LOG_TRACE to enable

  integer, public, save :: log_unit = stderr !< By default, log to stderr for level > 2 (stdout has a bug with gfortran)
  integer, public, save :: log_unit_error = stderr !< By default, log to stderr for level <= 2
  integer, public, save :: minimum_log_level = LOG_INFO !< Note that more critical means a lower number
  logical, public, save :: show_file_and_line = .true. !< show file name and line number in log output

  public :: log_set_output_hostname
  public :: log_set_output_severity
  public :: log_set_output_date
  public :: log_set_output_time
  public :: log_set_output_fileline
  public :: log_set_skip_terminal_check
  public :: log_set_disable_colors
  public :: log_set_disable_format
  public :: log_set_config

  public :: logu, logp, logl, stput

  private
  !> Control start character
  character(len=*), parameter :: start = achar(27)
  !> Control reset character
  character(len=*), parameter :: reset = "0"
  !> Styles
  character(len=*), parameter :: bold = "1", dimmed = "2", &
    underline = "4", blink = "5", invert = "7", hidden = "8"

  ! Default settings for hostname and severity output
  logical, save :: output_hostname     = .false.
  logical, save :: output_severity     = .true.
  logical, save :: output_date         = .false.
  logical, save :: output_time         = .false.
  logical, save :: output_fileline     = .true.
  logical, save :: skip_terminal_check = .false.
  logical, save :: disable_colors      = .false.
  logical, save :: disable_format      = .false.

  !> These are the color codes corresponding to the loglevels above
  character(len=*), dimension(NUM_LOG_LEVELS), parameter :: color_codes = &
      ["31", "31", "33", "32", "35", "36", "36"]
  !> These are the styles corresponding to the loglevels above
  character(len=*), dimension(NUM_LOG_LEVELS), parameter :: style_codes = &
      [bold, reset, reset, reset, reset, reset, reset]

  !> Colors for other output
  character(len=*), parameter :: level_color = "20"

contains
  !> \brief write format string to given unit
  subroutine tput(lu, code)
    implicit none
    integer, intent(in) :: lu !< unit
    character(len=*), intent(in) :: code !< format code
    if (.not. disable_format) write(lu, '(a,"[",a,"m")', advance="no") start, code
  end subroutine tput

  !> \brief generate format string
  subroutine stput(str, code)
    implicit none
    character(len=*), intent(inout) :: str !< pre string
    character(len=*), intent(in)    :: code !< format code
    if (.not. disable_format) str = trim(str) // start // "[" // trim(code) // "m"
  end subroutine stput

  !> \brief Set the default for hostname output
  subroutine log_set_output_hostname(bool)
    logical, intent(in) :: bool
    output_hostname = bool
  end subroutine log_set_output_hostname

  !> \brief Set the default for severity output
  subroutine log_set_output_severity(bool)
    logical, intent(in) :: bool
    output_severity = bool
  end subroutine log_set_output_severity

  !> \brief Set the default for date output
  subroutine log_set_output_date(bool)
    logical, intent(in) :: bool
    output_date = bool
  end subroutine log_set_output_date

  !> \brief Set time-only date format
  subroutine log_set_output_time(bool)
    logical, intent(in) :: bool
    output_time = bool
  end subroutine log_set_output_time

  !> \brief Set the default for file/line output
  subroutine log_set_output_fileline(bool)
    logical, intent(in) :: bool
    output_fileline = bool
  end subroutine log_set_output_fileline

  !> \brief Whether or not to skip the terminal check
  subroutine log_set_skip_terminal_check(bool)
    logical, intent(in) :: bool
    skip_terminal_check = bool
  end subroutine log_set_skip_terminal_check

  !> \brief Disable colors altogether
  subroutine log_set_disable_colors(bool)
    logical, intent(in) :: bool
    disable_colors = bool
  end subroutine log_set_disable_colors

  !> \brief Disable formatting altogether
  subroutine log_set_disable_format(bool)
    logical, intent(in) :: bool
    disable_format = bool
  end subroutine log_set_disable_format

  !> \brief Output unit to log.
  !> \return unit number.
  function logu(level)
    integer, intent(in)           :: level     !< The log level of the current message
    integer                       :: logu      !< unit to log to (stderr for level <=2 else stdout by default)

    if (level .le. LOG_ERROR) then
      logu = log_unit_error
    else
      logu = log_unit
    endif
  end function logu

  !> \brief Output this log statement or not.
  !> \return true if this log message can be printed.
  function logp(level, only_n)
#ifdef USE_MPI
    use mpi
#endif
    integer, intent(in)           :: level     !< The log level of the current message
    integer, intent(in), optional :: only_n    !< Show only if the current mpi rank equals only_n
    logical                       :: logp      !< Output: true if this log message can be printed
#ifdef USE_MPI
    integer :: rank, ierr
#endif

    if (level .le. minimum_log_level) then
      logp = .true.
    else
      logp = .false.
    endif
#ifdef USE_MPI
    if (logp .and. present(only_n)) then
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      if (rank .ne. only_n) logp = .false.
    endif
#endif
  end function logp

  !> \brief Write a log lead containing level and optional info.
  !> \details The name is shortened to allow for longer log messages without needing continuations.
  !> \return The output log leader.
  function logl(level, filename, linenum)
    implicit none
    ! Input parameters
    integer                    :: level    !< The log level
    character(len=*), optional :: filename !< An optional filename to add to the log lead
    integer, optional          :: linenum  !< With line number
    character(len=300)         :: logl     !< The output log leader

    ! Internal parameters
    character(len=50), dimension(6) :: log_tmp !< The different parts of the log lead
    integer                         :: fn_len  !< Add extra spaces after part i
    integer       :: i, j, space_cnt !< The counter for the different parts
    character(4)  :: linenum_lj ! left-justified line number
    character(len=50) :: basename ! basename stripped from filename

    logical :: show_colors = .false.
    logical :: is_terminal = .false.
    i = 1

    ! Set level to 1 if it is too low, skip if too high
    if (level .lt. 1) level = 1
    if (level .gt. minimum_log_level .or. level .gt. NUM_LOG_LEVELS) return

    ! only show colors if we are outputting to a terminal
    if (skip_terminal_check) then
      show_colors = .not. disable_colors
    else
#ifdef NAG
      call isatty(stdout, is_terminal)
#else
      is_terminal = isatty(stdout)
#endif
      show_colors = is_terminal .and. .not. disable_colors
    endif
    ! This works in ifort and gfortran (log_unit is stdout here because log_lead is an internal string)

    ! Initialize log_tmp
    log_tmp = ""
    fn_len = 0

    ! Reset the colors if needed
    if (show_colors) call stput(log_tmp(i), reset) ! Do not increment i to add it before the next space

    ! Write date and time if wanted
    if (output_date .or. output_time) then
      log_tmp(i) = trim(log_tmp(i)) // log_datetime()
      i = i + 1
    endif

    ! Write hostname if requested
    if (output_hostname) then
      log_tmp(i) = trim(log_tmp(i)) // log_hostname()
      i = i + 1
    endif

#ifdef USE_MPI
    ! Write mpi id
    log_tmp(i) = trim(log_tmp(i)) // log_mpi_id()
    i = i + 1
#endif

    if (present(filename) .and. output_fileline .and. show_file_and_line) then
      call strip_path(filename, basename)
      log_tmp(i) = trim(log_tmp(i)) // trim(basename)
      if (present(linenum)) then
        ! Left-justify the line number and cap it to 4 characters
        write(linenum_lj, '(i4)') linenum
        log_tmp(i) = trim(log_tmp(i)) // ":" // adjustl(linenum_lj)
      endif
      ! How many extra spaces are needed to fill out to multiple of n characters
      fn_len = fn_len + len_trim(log_tmp(i))
      i = i+1
    endif

    ! Output severity level
    if (output_severity) then
      fn_len = fn_len + len_trim(log_severity(level, .false.))
      ! correctly set spaces when skipping filename/line
      space_cnt = mod(7-fn_len,8)+8
      if (space_cnt >= 8) space_cnt = space_cnt - 8
      log_tmp(i) = trim(log_tmp(i)) // spaces(space_cnt) // log_severity(level, show_colors)
    endif

    ! Set color based on severity level
    if (show_colors) then
      ! Set bold for errors (must go first, resets the color code otherwise)
      call stput(log_tmp(i), style_codes(level))
      call stput(log_tmp(i), color_codes(level))
    endif

    ! Concatenate trim(log_tmp(i)) with spaces in between
    logl = log_tmp(1)
    do j=2,i
      logl = trim(logl) // " " // trim(log_tmp(j))
    enddo
  end function logl

  !> \brief Get base name of a file path.
  subroutine strip_path(filepath, basename)
    ! keeping this subroutine to prevent circular dependencies: mo_os -> mo_message -> mo_logging -> mo_os
    character(len=*), intent(in) :: filepath !< The path to be stripped
    character(len=*), intent(out) :: basename !< The basename of the filepath
    integer :: last_sep_idx
    last_sep_idx = index(filepath, "/", .true.)
    basename = filepath(last_sep_idx+1:)
  end subroutine

  !> \brief Return the hostname in a 50 character string
  !> \return hostname.
  function log_hostname()
    character(len=50) log_hostname
#ifdef INTEL
    integer :: iError
#endif
#ifdef NAG
    call gethostname(log_hostname)
#endif
#ifdef INTEL
    iError = hostnm(log_hostname)
#endif
#ifdef GFORTRAN
    call hostnm(log_hostname)
#endif
  end function log_hostname

  !> \brief Return n spaces
  !> \return n spaces.
  function spaces(n)
    integer, intent(in) :: n !< Maximum is 30
    character(len=n)    :: spaces
    spaces = "                              "
  end function spaces

  !> \brief Return the severity level with colors etc in a 50 char string
  !> \return severity level.
  function log_severity(level, show_colors)
    integer, intent(in) :: level
    logical, intent(in) :: show_colors
    character(len=50) log_severity

    log_severity = ""
    if (show_colors) call stput(log_severity, level_color)
    if (level .eq. LOG_FATAL) then
      if (show_colors) then
        call stput(log_severity, bold)
        call stput(log_severity, color_codes(level)) ! error has the same color, for reading convenience
      endif
      log_severity = trim(log_severity) // "FATAL"
    elseif (level .eq. LOG_ERROR) then
      if (show_colors) call stput(log_severity, bold)
      log_severity = trim(log_severity) // "ERROR"
    elseif (level .eq. LOG_WARN) then
      log_severity = trim(log_severity) // "WARN"
    elseif (level .eq. LOG_INFO) then
      log_severity = trim(log_severity) // "INFO"
    elseif (level .eq. LOG_DEBUG) then
      log_severity = trim(log_severity) // "DEBUG"
    elseif (level .eq. LOG_TRACE) then
      log_severity = trim(log_severity) // "TRACE"
    elseif (level .eq. LOG_SUBTRACE) then
      log_severity = trim(log_severity) // "FINE"
    endif
    if (show_colors) call stput(log_severity, reset)
  end function log_severity

#ifdef USE_MPI
  !> \brief Return the mpi id of the current process
  !> \return MPI id.
  function log_mpi_id()
    use mpi
    character(50) :: log_mpi_id    !< The mpi id part of a log
    character(6)  :: mpi_id_lj     !< MPI id in string
    character(4)  :: id_fmt        !< The forhmat to print mpi_id_lj in
    integer :: rank, n_cpu, ierr

    call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, n_cpu, ierr)
    if (n_cpu .eq. 1) then
      log_mpi_id = ""
    else
      write(id_fmt, '(A,i1,A)') "(i", ceiling(log10(real(n_cpu))), ")"
      write(mpi_id_lj,id_fmt) rank
      write(log_mpi_id, '("#",a)') trim(adjustl(mpi_id_lj))
    endif
  end function log_mpi_id
#endif

  !> \brief Return the current date, formatted nicely
  !> \return date.
  function log_datetime()
    character(50) :: log_datetime !< Output the date here

    character(8)  :: date
    character(10) :: time
    character(5)  :: zone

    call date_and_time(date, time, zone)
    if (output_date .and. output_time) then
      write(log_datetime, '(a,"/",a,"/",a," ",a,":",a,":",a,".",a," ")') date(1:4), date(5:6), date(7:8), &
            time(1:2), time(3:4), time(5:6), time(8:10)
    else
      if (output_time) then
        write(log_datetime, '(a,":",a,":",a,".",a," ")') time(1:2), time(3:4), time(5:6), time(8:10)
      endif
      if (output_date) then
        write(log_datetime, '(a,"/",a,"/",a," ")') date(1:4), date(5:6), date(7:8)
      endif
    endif
  end function log_datetime

  !> \brief Set logging configuration
  subroutine log_set_config( &
    verbose, quiet, log_output_hostname, log_force_colors, log_no_colors, log_output_date, log_output_time, log_no_format)
    use mo_kind, only: i4
    implicit none
    integer(i4), optional, intent(in) :: verbose !< increase verbosity level
    integer(i4), optional, intent(in) :: quiet !< decrease verbosity level
    logical, optional, intent(in) :: log_output_hostname !< show hostname
    logical, optional, intent(in) :: log_force_colors !< force colors in output
    logical, optional, intent(in) :: log_no_colors !< disable colors
    logical, optional, intent(in) :: log_output_date !< add date to output
    logical, optional, intent(in) :: log_output_time !< add time to output
    logical, optional, intent(in) :: log_no_format !< disable formatting

    if ( present(verbose) ) minimum_log_level = min(NUM_LOG_LEVELS, minimum_log_level + verbose)
    if ( present(quiet) ) minimum_log_level = min(NUM_LOG_LEVELS, minimum_log_level - quiet)
    if ( present(log_output_hostname) ) output_hostname = log_output_hostname
    if ( present(log_force_colors) ) skip_terminal_check = log_force_colors
    if ( present(log_no_colors) ) disable_colors = log_no_colors
    if ( present(log_output_date) ) output_date = log_output_date
    if ( present(log_output_time) ) output_time = log_output_time
    if ( present(log_no_format) ) disable_format = log_no_format
  end subroutine log_set_config

end module mo_logging
