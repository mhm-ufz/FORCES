!> \file mo_nml.f90
!> \copydoc mo_nml

!> \brief Deal with namelist files.
!> \details This module provides routines to open, close and position namelist files.
!! - Adapted from Echam5, (C) MPI-MET, Hamburg, Germany
!> \author L. Kornblueh, MPI, March 2001, original source
!> \changelog
!! - Jan 2011, Matthias Cuntz
!!   - compatible with gfortran <= version 4.3
!!   - all integer(i4)
!!   - quiet
!! - Jan 2013, Matthias Cuntz
!!   - close_nml with unit, open_nml quiet=.true. default position_nml swap first and status
!> \authors Matthias Cuntz
!> \date Jan 2011
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_nml

  USE mo_kind, ONLY : i4
  USE mo_string_utils, ONLY : tolower
  USE mo_message, ONLY : message, error_message

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: open_nml                                      ! open namelist file
  PUBLIC :: close_nml                                     ! close namelist file
  PUBLIC :: position_nml                                  ! position namelist file
  PUBLIC :: nunitnml                                      ! namelist unit
  PUBLIC :: POSITIONED, MISSING, LENGTH_ERROR, READ_ERROR ! return values from position_nml

  ! return values in optinal status of function 'position_nml'
  !> Information: file pointer set to namelist group
  INTEGER(i4), PARAMETER :: POSITIONED = 0
  !> Error: namelist group is missing
  INTEGER(i4), PARAMETER :: MISSING = 1
  !> Error: namelist group name too long
  INTEGER(i4), PARAMETER :: LENGTH_ERROR = 2
  !> Error occured during read of namelist file
  INTEGER(i4), PARAMETER :: READ_ERROR = 3

  !> default namelist unit
  INTEGER, SAVE :: nunitnml = -1

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  !>    \brief Open a namelist file.

  !>    \details Open a namelist file.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    call open_nml('namelist.txt',nnml)
  !!    \endcode
  !!    See also example in test directory

  !>        \param[in] "character(len=*) :: file"   namelist filename
  !>        \param[in] "integer          :: unit"   namelist unit
  !>       \param[in] "logical, optional :: quiet"   Be verbose or not (default: .true.)\n
  !!                                                            .true.:  no messages\n
  !!                                                            .false.: write out messages

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany
  !>    \date Jan 2013
  !!        - quiet=.true. default
  !>    \author Luis Samaniego
  !>    \date Nov 2013
  !!        - comparison statements == -> .eq., etc

  SUBROUTINE open_nml(file, unit, quiet)

    IMPLICIT NONE

    CHARACTER(len = *), INTENT(IN) :: file
    INTEGER, INTENT(IN) :: unit
    LOGICAL, INTENT(IN), OPTIONAL :: quiet
    INTEGER :: istat
    LOGICAL :: iquiet

    iquiet = .false.
    if (present(quiet)) iquiet = quiet

    nunitnml = unit
    if (.not. iquiet) CALL message('    This is namelist ', trim(file))
    OPEN (nunitnml, file = file, iostat = istat, status = 'old', action = 'read', delim = 'apostrophe')

    IF (istat .ne. 0) THEN
      CALL error_message('OPEN_NML: Could not open namelist file ', trim(file))
    END IF

  END SUBROUTINE open_nml

  ! ------------------------------------------------------------------

  !>    \brief Close a namelist file.

  !>    \details Close a namelist file.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    call close_nml()
  !!    ! or
  !!    call close_nml(unml)
  !!    \endcode
  !!    See also example in test directory

  !>    \param[in] "integer, optional :: unit"   namelist unit

  !>    \note
  !!    open_nml remembers the namelist unit in the public, save variable nunitnml.\n
  !!    close_nml uses nunitnml if unit is not given.

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany
  !>    \date Jan 2013
  !!        - unit
  SUBROUTINE close_nml(unit)

    IMPLICIT NONE

    INTEGER, INTENT(IN), OPTIONAL :: unit

    INTEGER :: istat, nnml

    nnml = nunitnml
    if (present(unit)) nnml = unit

    IF (nnml .lt. 0) CALL error_message('CLOSE_NML: No namelist file opened.')

    CLOSE(nnml, IOSTAT = istat)

    IF (istat .ne. 0) CALL error_message('CLOSE_NML: Could not close namelist file.')

    if (.not. present(unit)) nunitnml = -1

  END SUBROUTINE close_nml

  ! ------------------------------------------------------------------

  !>    \brief Position a namlist file.

  !>    \details Position namelist file pointer for reading a new namelist next.\n
  !!    It positions the namelist file at the correct place for reading\n
  !!    namelist /name/ (case independent).
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    call position_nml('myname',nnml)
  !!    \endcode

  !>    \param[in] "character(len=*) :: name"     namelist name (case independent)
  !>    \param[in] "integer, optional :: unit"    namelist unit (default: nunitnml)
  !>    \param[in] "logical, optional :: first"   start search at beginning,
  !!                                              i.e. rewind the namelist first (default: .true.)\n
  !!                                              .true.:  rewind\n
  !!                                              .false.: continue search from current file pointer
  !>    \param[out] "integer(i4), optional :: status"   Set on output to either of\n
  !!                                                    POSITIONED (0)   - correct\n
  !!                                                    MISSING (1)      - name not found\n
  !!                                                    LENGTH_ERROR (2) - namelist length longer then 256 characters\n
  !!                                                    READ_ERROR (3)   - error while reading namelist file

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany
  !>    \date Jan 2013
  !!        - swap first and status in call list
  SUBROUTINE position_nml(name, unit, status, first)

    IMPLICIT NONE

    CHARACTER(len = *), INTENT(in) :: name   ! namelist group name
    INTEGER, INTENT(in), OPTIONAL :: unit   ! file unit number
    INTEGER(i4), INTENT(out), OPTIONAL :: status ! error return value
    LOGICAL, INTENT(in), OPTIONAL :: first  ! default: true

    CHARACTER(len = 256) :: yline    ! line read
    CHARACTER(len = 256) :: test     ! uppercase namelist group name
    INTEGER(i4) :: stat     ! local copy of status variable
    INTEGER :: ios      ! status variable from read operation
    LOGICAL :: lrew     ! local copy of rewind flag
    INTEGER(i4) :: iunit    ! local copy of unit number
    INTEGER(i4) :: len_name ! length of requested namelist group name
    CHARACTER :: ytest    ! character to test for delimiter
    CHARACTER(len = 12) :: code     ! error code printed
    INTEGER(i4) :: ind      ! index from index routine
    INTEGER(i4) :: indc     ! index of comment character (!)

    lrew = .TRUE.
    IF (PRESENT(first)) lrew = first
    iunit = nunitnml
    IF (PRESENT(unit)) iunit = unit
    stat = MISSING
    code = 'MISSING'

    len_name = LEN_TRIM(name)

    IF (len_name .gt. LEN(test)) THEN
      stat = LENGTH_ERROR
      code = 'LENGTH_ERROR'
    END IF

    !test = '&'//tolower(name)
    write(test, '(A,A)') '&', tolower(name)

    ! Reposition file at beginning:
    IF (lrew) REWIND(iunit)

    ! Search start of namelist
    DO
      IF (stat .ne. MISSING) EXIT

      yline = ' '

      READ (iunit, *, IOSTAT = ios) yline
      IF (ios .lt. 0) THEN
        EXIT  ! MISSING
      ELSE IF (ios .gt. 0) THEN
        stat = READ_ERROR
        code = 'READ_ERROR'
        EXIT
      END IF

      yline = tolower(yline)

      ind = INDEX(yline, TRIM(test))

      IF (ind .eq. 0) CYCLE

      indc = INDEX(yline, '!')

      IF (indc .gt. 0 .AND. indc .lt. ind) CYCLE

      ! test for delimiter
      ytest = yline(ind + len_name + 1 : ind + len_name + 1)

      IF ((LGE(ytest, '0') .AND. LLE(ytest, '9')) .OR. &
              (LGE(ytest, 'a') .AND. LLE(ytest, 'z')) .OR. &
              ytest .eq. '_'                         .OR. &
              (LGE(ytest, 'A') .AND. LLE(ytest, 'Z'))) THEN
        CYCLE
      ELSE
        stat = POSITIONED
        BACKSPACE(iunit)
        EXIT
      END IF
    END DO

    IF (PRESENT(status)) status = stat
    SELECT CASE (stat)
    CASE (POSITIONED)
      RETURN
    CASE (MISSING)
      IF (PRESENT(status)) RETURN
    END SELECT

    ! Error if it reaches here
    CALL error_message('POSITION_NML: namelist /', trim(name) , '/ ', trim(code))

  END SUBROUTINE position_nml

END MODULE mo_nml
