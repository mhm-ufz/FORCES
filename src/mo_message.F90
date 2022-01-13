!> \file mo_message.f90
!> \copydoc mo_message

!> \brief Write out concatenated strings
!> \details Write out several strings concatenated on standard out or a given unit, either advancing or not.
!> \author Matthias Cuntz, Sebastian Mueller
!> \date Jul 2011, Dec 2019

MODULE mo_message

  USE mo_constants, ONLY : nout, nerr

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: message         ! versatile routine to write out strings in file or on screen
  PUBLIC :: error_message   ! write error message to ERROR_UNIT and call stop 1

  ! ------------------------------------------------------------------

CONTAINS

  function process_arguments(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10) result(outString)

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

    CHARACTER(len = 32000) :: outString
#ifdef GFORTRAN
   CHARACTER(len=32000) :: tempString
#endif

    outString = ''
    ! start from back so that trim does not remove user desired blanks
#ifdef GFORTRAN
   ! GFORTRAN has problems with concatenation operator //
   ! It is also weird in write:
   !    write(outString,'(A,A)') t10, trim(outString)
   ! writes t10 twice into outString.
   tempString = outString
   if (present(t10)) write(outString,'(A,A)') t10, trim(tempString)
   tempString = outString
   if (present(t09)) write(outString,'(A,A)') t09, trim(tempString)
   tempString = outString
   if (present(t08)) write(outString,'(A,A)') t08, trim(tempString)
   tempString = outString
   if (present(t07)) write(outString,'(A,A)') t07, trim(tempString)
   tempString = outString
   if (present(t06)) write(outString,'(A,A)') t06, trim(tempString)
   tempString = outString
   if (present(t05)) write(outString,'(A,A)') t05, trim(tempString)
   tempString = outString
   if (present(t04)) write(outString,'(A,A)') t04, trim(tempString)
   tempString = outString
   if (present(t03)) write(outString,'(A,A)') t03, trim(tempString)
   tempString = outString
   if (present(t02)) write(outString,'(A,A)') t02, trim(tempString)
   tempString = outString
   if (present(t01)) write(outString,'(A,A)') t01, trim(tempString)
   tempString = outString
   if ((lle(trim(tempString),'') .and. lge(trim(tempString),''))) then
      write(outString,'(A,A)') trim(tempString), ' '
   end if
#else
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
    ! output at least one space otherwise some compilers get confused on Mac (empty assembler statement)
    if ((lle(trim(outString), '') .and. lge(trim(outString), ''))) then
      write(outString, '(A,A)') trim(outString) // ' '
    end if
#endif

  end function process_arguments


  !> \brief Write out an error message to stdout
  SUBROUTINE message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, uni, advance)

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
    INTEGER, INTENT(IN), OPTIONAL :: uni  !< Unit to write to (default: stdout)
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: advance  !< 'add linebreak after message, default: 'yes', else 'no'

    CHARACTER(len = 32000) :: outString
    INTEGER :: uniArg
    CHARACTER(len = 3) :: advanceArg

    if (present(uni)) then
      uniArg = uni
    else
      uniArg = nout
    end if
    if (present(advance)) then
      advanceArg = advance
    else
      advanceArg = 'yes'
    end if

    outString = process_arguments(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10)
    write(uniArg, '(a)', advance = advanceArg) trim(outString)

  END SUBROUTINE message

  !> \brief Write out an error message to stderr and call stop 1.
  SUBROUTINE error_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, uni, advance)

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
    INTEGER, INTENT(IN), OPTIONAL :: uni  !< Unit to write to (default: stderr)
    CHARACTER(len = *), INTENT(IN), OPTIONAL :: advance  !< 'add linebreak after message, default: 'yes', else 'no'

    INTEGER :: uniArg

    if (present(uni)) then
      uniArg = uni
    else
      uniArg = nerr
    end if
    call message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, uniArg, advance)
    stop 1

  END SUBROUTINE error_message

END MODULE mo_message
