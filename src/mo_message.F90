#include "logging.h"
!> \file mo_message.f90
!> \copydoc mo_message

!> \brief Write out concatenated strings
!> \details Write out several strings concatenated on standard out or a given unit, either advancing or not.
!> \author Matthias Cuntz, Sebastian Mueller
!> \date Jul 2011, Dec 2019
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_message

  use mo_logging
  USE mo_constants, ONLY : nout, nerr

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: message         ! versatile routine to write out strings in file or on screen
  PUBLIC :: warn_message    ! write warning message to ERROR_UNIT
  PUBLIC :: error_message   ! write error message to ERROR_UNIT and call stop 1

  logical, public, save :: show_msg = .true.  !< global control switch to show normal messages
  logical, public, save :: show_warn = .true. !< global control switch to show warning messages
  logical, public, save :: show_err = .true.  !< global control switch to show error messages

  integer, public, save :: unit_msg = nout    !< global standard output unit (stdout by default)
  integer, public, save :: unit_warn = nerr   !< global standard warning unit (stderr by default)
  integer, public, save :: unit_err = nerr    !< global standard error unit (stderr by default)

  ! ------------------------------------------------------------------

CONTAINS

  function process_arguments(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16) result(outString)

    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t01
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t02
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t03
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t04
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t05
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t06
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t07
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t08
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t09
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t10
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t11
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t12
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t13
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t14
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t15
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t16

    CHARACTER(len = 32000) :: outString

    outString = ''
    ! start from back so that trim does not remove user desired blanks
    if (present(t16)) outString = t16 // trim(outString)
    if (present(t15)) outString = t15 // trim(outString)
    if (present(t14)) outString = t14 // trim(outString)
    if (present(t13)) outString = t13 // trim(outString)
    if (present(t12)) outString = t12 // trim(outString)
    if (present(t11)) outString = t11 // trim(outString)
    if (present(t10)) outString = t10 // trim(outString)
    if (present(t09)) outString = t09 // trim(outString)
    if (present(t08)) outString = t08 // trim(outString)
    if (present(t07)) outString = t07 // trim(outString)
    if (present(t06)) outString = t06 // trim(outString)
    if (present(t05)) outString = t05 // trim(outString)
    if (present(t04)) outString = t04 // trim(outString)
    if (present(t03)) outString = t03 // trim(outString)
    if (present(t02)) outString = t02 // trim(outString)
    if (present(t01)) outString = t01 // trim(outString)

  end function process_arguments


  !> \brief Write out an error message to stdout
  SUBROUTINE message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, reset_format)

    IMPLICIT NONE

    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t01  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t02  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t03  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t04  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t05  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t06  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t07  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t08  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t09  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t10  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t11  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t12  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t13  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t14  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t15  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t16  !< optional string arguments
    INTEGER, INTENT(IN), OPTIONAL :: uni  !< Unit to write to (default: stdout)
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: advance  !< add linebreak after message, default: 'yes', else 'no'
    LOGICAL, INTENT(IN), OPTIONAL :: show  !< control if message should be shown (show_msg as default)
    LOGICAL, INTENT(IN), OPTIONAL :: reset_format  !< Reset formatting (default: .false.)

    CHARACTER(len = 32000) :: outString
    CHARACTER(len = 10) :: format_string
    INTEGER :: uni_
    CHARACTER(len = 3) :: advance_
    logical :: reset_format_, show_

    show_ = show_msg
    if ( present(show) ) show_ = show
    ! short circuit if message should not be shown
    if (.not. show_ ) return

    uni_ = unit_msg
    advance_ = 'yes'
    reset_format_ = .false.
    if ( present(uni) ) uni_ = uni
    if ( present(advance) ) advance_ = advance
    if ( present(reset_format) ) reset_format_ = reset_format

    outString = process_arguments(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16)

    if ( reset_format_ ) then
      format_string = ""
      call stput(format_string, "0")
      outString = trim(format_string) // outString
    end if

    write(uni_, '(a)', advance = advance_) trim(outString)

  END SUBROUTINE message

  !> \brief Write out an error message to stderr and call stop 1.
  SUBROUTINE error_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, raise, reset_format)

    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t01  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t02  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t03  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t04  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t05  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t06  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t07  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t08  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t09  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t10  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t11  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t12  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t13  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t14  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t15  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t16  !< optional string arguments
    INTEGER, INTENT(IN), OPTIONAL :: uni  !< Unit to write to (default: stderr)
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: advance  !< add linebreak after message, default: 'yes', else 'no'
    LOGICAL, INTENT(IN), OPTIONAL :: show  !< control if message should be shown (show_err as default)
    LOGICAL, INTENT(IN), OPTIONAL :: raise  !< control if an exception is raised with error code 1 (.true. as default)
    LOGICAL, INTENT(IN), OPTIONAL :: reset_format  !< Reset formatting (default: .false.)

    INTEGER :: uni_
    logical :: show_, raise_

    show_ = show_err
    uni_ = unit_err
    raise_ = .true.
    if ( present(show) ) show_ = show
    if ( present(uni) ) uni_ = uni
    if ( present(raise) ) raise_ = raise

    call message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, uni_, advance, show_, reset_format)
    if ( raise_ ) stop 1

  END SUBROUTINE error_message

  !> \brief Write out a warning message to stderr.
  SUBROUTINE warn_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, raise, reset_format)

    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t01  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t02  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t03  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t04  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t05  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t06  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t07  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t08  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t09  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t10  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t11  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t12  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t13  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t14  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t15  !< optional string arguments
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: t16  !< optional string arguments
    INTEGER, INTENT(IN), OPTIONAL :: uni  !< Unit to write to (default: stderr)
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: advance  !< add linebreak after message, default: 'yes', else 'no'
    LOGICAL, INTENT(IN), OPTIONAL :: show  !< control if message should be shown (show_warn as default)
    LOGICAL, INTENT(IN), OPTIONAL :: raise  !< control if an exception is raised with error code 1 (.false. as default)
    LOGICAL, INTENT(IN), OPTIONAL :: reset_format  !< Reset formatting (default: .false.)

    INTEGER :: uni_
    logical :: show_, raise_

    show_ = show_warn
    uni_ = unit_warn
    raise_ = .false.
    if ( present(show) ) show_ = show
    if ( present(uni) ) uni_ = uni
    if ( present(raise) ) raise_ = raise

    call message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, uni_, advance, show_, reset_format)
    if ( raise_ ) stop 1

  END SUBROUTINE warn_message

END MODULE mo_message
