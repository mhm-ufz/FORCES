!> \file mo_os.f90
!> \brief \copybrief mo_os
!> \details \copydetails mo_os

!> \brief Path and directory management.
!> \details Path handling and existence checks for files and directories.
!> \changelog
!! - Nicola Doering, Aug 2020
!!   - module implementation
!! - Sebastian Mueller, Jan 2023
!!   - changed signatures (path, answer, verbose, raise) for path_exists, path_isfile and path_isdir
!!   - respect show_msg and show_err from mo_message
!!   - simplify inquire logic
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_os

  USE mo_message, ONLY: error_message, message

  IMPLICIT NONE

  PUBLIC :: path_exists
  PUBLIC :: path_isfile
  PUBLIC :: path_isdir
  PUBLIC :: path_splitext
  PUBLIC :: path_split

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------
  !>        \brief Existence of a path
  !>        \details Checks whether a given path exists.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_exists(path, answer, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(OUT), OPTIONAL :: answer !< result
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if path does not exist (default: .false.)

    LOGICAL :: isfile, isdir, exists, raise_
    character(:), allocatable :: t_path

    raise_ = .false.
    if (present(raise)) raise_ = raise

    t_path = trim(path)

    call path_isfile(t_path, answer=isfile, verbose=.false., raise=.false.)
    call path_isdir(t_path, answer=isdir, verbose=.false., raise=.false.)
    exists = isfile .or. isdir

    if (.not. exists) call path_msg("Path does not exist: ", t_path, raise_, verbose)

    if (present(answer)) answer = exists

  END SUBROUTINE path_exists

  ! ------------------------------------------------------------------
  !>        \brief Whether the path describes a file.
  !>        \details Checks whether a given path exists and describes a file.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_isfile(path, answer, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(OUT), OPTIONAL :: answer !< result
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if file does not exist (default: .false.)

    LOGICAL :: isfile, isdir, raise_
    CHARACTER(:), allocatable :: t_path

    raise_ = .false.
    if (present(raise)) raise_ = raise

    t_path = trim(path)

    inquire(file=t_path, exist=isfile)
    ! gfortran/NAG need the check if it is not a directory explicitly
    call path_isdir(t_path, answer=isdir, verbose=.false., raise=.false.)
    isfile = isfile .and. (.not. isdir)

    if (.not. isfile) call path_msg("File does not exist: ", t_path, raise_, verbose)
    if (present(answer)) answer = isfile

  END SUBROUTINE path_isfile

  ! ------------------------------------------------------------------
  !>        \brief Whether the path describes a directory.
  !>        \details Checks whether a given path exists and describes a directory.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_isdir(path, answer, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(OUT), OPTIONAL :: answer !< result
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if dir does not exist (default: .false.)

    LOGICAL :: isdir, raise_
    CHARACTER(:), allocatable :: t_path

    raise_ = .false.
    if (present(raise)) raise_ = raise

    t_path = trim(path)

#ifdef INTEL
    ! intel has non-standard 'directory' argument
    inquire(directory=t_path, exist=isdir)
#else
    ! append "/" and check if it still exists
    inquire(file=t_path//'/', exist=isdir)
#endif

    if (.not. isdir) call path_msg("Directory does not exist: ", t_path, raise_, verbose)
    if (present(answer)) answer = isdir

  END SUBROUTINE path_isdir

  ! ------------------------------------------------------------------
  !>        \brief Splitting the path into root and ext
  !>        \details Splitting the path name into a pair root and ext.
  !!                 Here, ext stands for extension and has the extension portion
  !!                 of the specified path while root is everything except ext part.
  !!                 If the path describes a directory or there is no extension
  !!                 portion ext is returned empty.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_splitext(path, root, ext)

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    CHARACTER(LEN=*), INTENT(OUT) :: root !< root part of path without extension
    CHARACTER(LEN=*), INTENT(OUT) :: ext  !< extension of given path (starting with ".")

    INTEGER   :: i
    CHARACTER :: c
    LOGICAL :: isdir
    CHARACTER(LEN=len_trim(path)) :: t_path

    t_path = trim(path)
    i = len(t_path) - 1
    c = t_path(len(t_path):len(t_path))

    !Checking, whether the path describes a directory so it cannot end with an extension
    call path_isdir(t_path, answer=isdir, verbose=.false., raise=.false.)
    if (isdir) then
      i = len(t_path)
    else
      !running through the path, beginning at the end until a point is found that probably indicates
      !the seperation of a file name and its extension or a '/' occurs what means that the rest of the
      !path is consisting of directories
      do while (.not. (c .eq. '.' .or. c .eq. '/' .or. i .eq. 0))
        c = t_path(i:i)
        i = i - 1
      end do
      !checking whether the last symbol of the path is a point or the while-loop run through the whole path
      !without finding a point or ended at a '/'. In any case it is not possible to seperate an extension.
      if (i .eq. len(t_path) - 1 .or. i .eq. 0 .or. c .eq. '/') then
        i = len(t_path)
      endif
    endif

    root = t_path(1:i)
    ext = t_path(i + 1:len(t_path))
    return

  END SUBROUTINE path_splitext

  ! ------------------------------------------------------------------
  !>        \brief Splitting the path into head and tail
  !>        \details Splitting the path name into a pair head and tail.
  !!                 Here, tail is the last path name component and head is
  !!                 everything leading up to that.
  !!                 If the path ends with an '/' tail is returned empty and
  !!                 if there is no '/' in path head is returned empty.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_split(path, head, tail)

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    CHARACTER(LEN=*), INTENT(OUT) :: head !< everything leading up to the last path component
    CHARACTER(LEN=*), INTENT(OUT) :: tail !< last pathname component

    INTEGER   :: i
    CHARACTER :: c
    CHARACTER(LEN=len_trim(path)) :: t_path

    t_path = trim(path)
    i = len(t_path) - 1
    c = t_path(len(t_path):len(t_path))

    !running through the path, beginning at the end until a point is found that probably indicates
    !the seperation of a file name and its extension or a '/' occurs what means that the rest of the
    !path is consisting of directories
    do while (.not. (c .eq. '/' .or. i .eq. 0))
      c = t_path(i:i)
      i = i - 1
    end do
    !checking whether the while-loop run through the whole path without finding a '/'
    if (i .eq. 0) then
      head = ''
      tail = t_path
    else
      head = t_path(1:i + 1)
      tail = t_path(i + 2:len(t_path))
    endif

    return

  END SUBROUTINE path_split

  ! ------------------------------------------------------------------

  subroutine path_msg(msg, path, raise, verbose)
    CHARACTER(LEN=*), intent(in) :: msg
    CHARACTER(LEN=*), intent(in) :: path
    logical, intent(in) ::  raise
    logical, intent(in), optional ::  verbose
    if (raise) then
      call error_message(msg, path, show=verbose)
    else
      call message(msg, path, show=verbose)
    endif
  end subroutine path_msg

END MODULE mo_os
