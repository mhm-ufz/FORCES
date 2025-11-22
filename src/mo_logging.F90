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
!!          Use the `--log-scope` CLI option (via `mo_cli`) to enable optional scope tags.
!!          Provide a comma-separated list (e.g. `--log-scope=dag,grid`) to restrict output
!!          to specific components, or omit the list (`--log-scope`) to show every scope.
!!
!! \par Examples
!! - \ref 01_logging_cli.F90 : \copybrief 01_logging_cli.F90
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

  use mo_constants, only: stdout=>nout, stderr=>nerr
  use mo_string_utils, only: tolower

  implicit none
  ! Log levels
  integer, public, parameter :: NUM_LOG_LEVELS = 7 !< 1 through 7 (fatal through trace)
  integer, public, parameter :: LOG_FATAL = LOG_LEVEL_FATAL_DEF !< = 1, Runtime error causing termination
  integer, public, parameter :: LOG_ERROR = LOG_LEVEL_ERROR_DEF !< = 2, Runtime error
  integer, public, parameter :: LOG_WARN  = LOG_LEVEL_WARN_DEF !< = 3, Warning, but we can continue
  integer, public, parameter :: LOG_INFO  = LOG_LEVEL_INFO_DEF !< = 4, Interesting events
  integer, public, parameter :: LOG_DEBUG = LOG_LEVEL_DEBUG_DEF !< = 5, Detailed debug output, disable by compiling your program with -DDISABLE_LOG_DEBUG
  integer, public, parameter :: LOG_TRACE = LOG_LEVEL_TRACE_DEF !< = 6, Extremely detailed output, compile your program with -DENABLE_LOG_TRACE to enable
  integer, public, parameter :: LOG_FINE = LOG_LEVEL_SUBTRACE_DEF !< = 7, More Extremely detailed output, compile your program with -DENABLE_LOG_TRACE to enable
  integer, public, parameter :: LOG_SUBTRACE = LOG_LEVEL_SUBTRACE_DEF !< = 7, More Extremely detailed output, compile your program with -DENABLE_LOG_TRACE to enable

  integer, public, save :: log_unit = stdout !< By default, log to stderr for level > 2 (stdout has a bug with gfortran)
  integer, public, save :: log_unit_error = stderr !< By default, log to stderr for level <= 2
  integer, public, save :: log_level = LOG_INFO !< Note that more critical means a lower number

  public :: log_set_config
  public :: logu, logp, logl

  private

  ! Default settings for optional log metadata
  logical, save :: output_date  = .false.
  logical, save :: output_time  = .false.
  logical, save :: output_line  = .false.
  type scope_filter_entry
    character(:), allocatable :: name
  end type scope_filter_entry
  type(scope_filter_entry), allocatable :: scope_filters(:)

contains

  !> \brief Output unit to log.
  !> \return unit number.
  function logu(level)
    integer, intent(in)           :: level     !< The log level of the current message
    integer                       :: logu      !< unit to log to (stderr for level <=2 else stdout by default)

    if (level <= LOG_ERROR) then
      logu = log_unit_error
    else
      logu = log_unit
    endif
  end function logu

  !> \brief Whether to print this log message.
  !> \return true if this log message can be printed.
  logical function logp(level, only_n, scope)
#ifdef MPI
    use mpi
#endif
    integer, intent(in)           :: level     !< The log level of the current message
    integer, intent(in), optional :: only_n    !< Show only if the current mpi rank equals only_n
    character(len=*), optional    :: scope     !< Optional scope identifier
#ifdef MPI
    integer :: rank, ierr
#endif

    logp = level <= log_level
#ifdef MPI
    if (logp .and. present(only_n)) then
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      if (rank /= only_n) logp = .false.
    endif
#endif
    if (.not. logp) return
    logp = scope_should_log(scope)

  end function logp

  !> \brief Write a log lead containing level and optional info.
  !> \details The name is shortened to allow for longer log messages without needing continuations.
  !> \return The output log leader.
  function logl(level, file, line, plain, scope)
    implicit none
    integer                    :: level !< The log level
    character(len=*), optional :: file  !< An optional filename to add to the log lead
    integer, optional          :: line  !< With line number
    logical, optional          :: plain !< Whether to output plain log lead without severity level
    character(len=*), optional :: scope !< Optional scope tag
    character(:), allocatable  :: logl  !< The output log lead
    ! Internal parameters
    character(len=50), dimension(7) :: log_tmp ! The different parts of the log lead
    character(len=300) :: log_buf
    integer       :: i, j ! The counter for the different parts
    character(4)  :: linenum_lj ! left-justified line number
    character(len=50) :: basename ! basename stripped from filename
    logical :: skip_level ! Whether to skip level output
    character(:), allocatable :: effective_scope
    i = 1

    ! If plain is set, skip level output
    skip_level = .false.
    if (present(plain)) skip_level = plain

    ! Set level to 1 if it is too low, skip if too high
    if (level < 1) level = 1
    if (level > log_level .or. level > NUM_LOG_LEVELS) return

    if (present(scope)) then
      effective_scope = scope
      if (trim(effective_scope) == "#") effective_scope = ""
    endif

    ! Initialize log_tmp
    log_tmp = ""

    ! Write date and time if wanted
    if (output_date .or. output_time) then
      log_tmp(i) = log_datetime()
      i = i + 1
    endif

#ifdef MPI
    ! Write mpi id
    log_tmp(i) = log_mpi_id()
    i = i + 1
#endif

    if (present(file) .and. output_line) then
      call strip_path(file, basename)
      log_tmp(i) = trim(basename)
      if (present(line)) then
        ! Left-justify the line number and cap it to 4 characters
        write(linenum_lj, '(i4)') line
        log_tmp(i) = trim(log_tmp(i)) // ":" // adjustl(linenum_lj)
      endif
      i = i+1
    endif

    if (allocated(effective_scope)) then
      if (len_trim(effective_scope) > 0) then
        log_tmp(i) = trim(effective_scope)
        i = i + 1
      endif
    endif

    if (.not. skip_level) then
      log_tmp(i) = log_level_label(level)
      i = i + 1
    endif

    ! Concatenate trim(log_tmp(i)) with spaces in between
    log_buf = ""
    do j=1,i-1
      if (len_trim(log_tmp(j)) == 0) cycle
      if (len_trim(log_buf) > 0) then
        log_buf = trim(log_buf) // " " // trim(log_tmp(j))
      else
        log_buf = trim(log_tmp(j))
      endif
    enddo
    if (len_trim(log_buf) > 0) then
      logl = trim(log_buf) // " "
    else
      logl = ""
    endif
  end function logl

  !> \brief Get base name of a file path.
  subroutine strip_path(filepath, basename)
    ! keeping this subroutine to prevent circular dependencies: mo_os -> mo_message -> mo_logging -> mo_os
    character(len=*), intent(in) :: filepath !< The path to be stripped
    character(len=*), intent(out) :: basename !< The basename of the filepath
    integer :: fwd_sep, back_sep
    fwd_sep  = index(filepath, "/", .true.)
    back_sep = index(filepath, "\", .true.)
    basename = filepath(max(fwd_sep, back_sep)+1:)
  end subroutine

  !> \brief Return the level label (e.g. [ERROR])
  !> \return level label text.
  function log_level_label(level)
    integer, intent(in) :: level
    character(len=16) :: log_level_label

    select case (level)
      case (LOG_FATAL)
        log_level_label = "[FATAL]"
      case (LOG_ERROR)
        log_level_label = "[ERROR]"
      case (LOG_WARN)
        log_level_label = "[WARN]"
      case (LOG_INFO)
        log_level_label = "[INFO]"
      case (LOG_DEBUG)
        log_level_label = "[DEBUG]"
      case (LOG_TRACE)
        log_level_label = "[TRACE]"
      case (LOG_FINE)
        log_level_label = "[FINE]"
      case default
        write(log_level_label, '("[LEVEL-",i0,"]")') level
    end select
  end function log_level_label

  subroutine append_scope_filter(raw)
    character(len=*), intent(in) :: raw
    character(:), allocatable :: norm
    type(scope_filter_entry) :: add_filter
    integer :: n
    norm = tolower(trim(raw))
    if (norm == "#") norm = ""
    if (allocated(scope_filters)) then
      do n = 1, size(scope_filters)
        if (allocated(scope_filters(n)%name)) then
          if (scope_filters(n)%name == norm) return
        end if
      end do
      add_filter%name = norm
      scope_filters = [scope_filters, add_filter]
    else
      if (len(norm) == 0) return
      allocate(scope_filters(1))
      scope_filters(1)%name = norm
    end if
  end subroutine append_scope_filter

  subroutine clear_scope_filter()
    if (allocated(scope_filters)) deallocate(scope_filters)
  end subroutine clear_scope_filter

  subroutine configure_scope_filter(list)
    character(len=*), intent(in) :: list
    integer :: i, lenlist
    character(:), allocatable :: token
    call clear_scope_filter()
    lenlist = len_trim(list)
    ! only root, no scopes. Catch: "", "#", ","
    if (lenlist == 0 .or. trim(list) == "#" .or. trim(list) == ",") return
    allocate(scope_filters(0)) ! trigger append_scope_filter with ""
    token = ""
    do i = 1, lenlist
      if (list(i:i) == ",") then
        call append_scope_filter(token)
        token = ""
      else
        token = token // list(i:i)
      end if
    end do
    call append_scope_filter(token)
  end subroutine configure_scope_filter

  logical function scope_should_log(scope)
    use mo_glob, only: glob_match
    character(len=*), optional, intent(in) :: scope
    character(:), allocatable :: norm
    integer :: i
    norm = ""
    if (present(scope)) norm = trim(scope)
    if (norm == "#") norm = ""
    norm = tolower(norm)
    if (.not. allocated(scope_filters)) then
      scope_should_log = (len(norm) == 0)
      return
    end if
    scope_should_log = .false.
    do i = 1, size(scope_filters)
      if (.not. allocated(scope_filters(i)%name)) cycle
      if (glob_match(scope_filters(i)%name, norm)) then
        scope_should_log = .true.
        exit
      end if
    end do
  end function scope_should_log

#ifdef MPI
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
  character(50) function log_datetime()
    character(8)  :: date
    character(10) :: time

    call date_and_time(date, time)
    if (output_date .and. output_time) then
      write(log_datetime, '(a,"-",a,"-",a," ",a,":",a,":",a,".",a," ")') &
        date(1:4), date(5:6), date(7:8), &
        time(1:2), time(3:4), time(5:6), time(8:10)
    else
      if (output_date) write(log_datetime, '(a,"-",a,"-",a," ")')       date(1:4), date(5:6), date(7:8)
      if (output_time) write(log_datetime, '(a,":",a,":",a,".",a," ")') time(1:2), time(3:4), time(5:6), time(8:10)
    endif
  end function log_datetime

  !> \brief Set logging configuration
  subroutine log_set_config( &
    verbose, quiet, log_output_date, log_output_time, log_output_line, log_scope_filter)
    use mo_kind, only: i4
    implicit none
    integer(i4), optional, intent(in) :: verbose !< increase verbosity level
    integer(i4), optional, intent(in) :: quiet !< decrease verbosity level
    logical, optional, intent(in) :: log_output_date !< add date to output
    logical, optional, intent(in) :: log_output_time !< add time to output
    logical, optional, intent(in) :: log_output_line !< add file/line to output
    character(len=*), optional, intent(in) :: log_scope_filter !< csv of allowed scopes (requires log_output_scope)
    integer :: level_shift
    level_shift = 0
    if ( present(verbose) ) level_shift = level_shift + int(verbose)
    if ( present(quiet) ) level_shift = level_shift - int(quiet)
    log_level = max(0, min(NUM_LOG_LEVELS, log_level + level_shift))
    if ( present(log_output_date) ) output_date = log_output_date
    if ( present(log_output_time) ) output_time = log_output_time
    if ( present(log_output_line) ) output_line = log_output_line
    if ( present(log_scope_filter) ) call configure_scope_filter(log_scope_filter)
  end subroutine log_set_config

end module mo_logging
