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

  IMPLICIT NONE

  PUBLIC :: get_cwd
  PUBLIC :: change_dir
  PUBLIC :: path_exists
  PUBLIC :: path_isfile
  PUBLIC :: path_isdir
  PUBLIC :: path_splitext
  PUBLIC :: path_split

  !> The constant string used by the operating system to refer to the current directory.
  CHARACTER(len = *), PUBLIC, PARAMETER :: curdir = '.'
  !> The constant string used by the operating system to refer to the parent directory.
  CHARACTER(len = *), PUBLIC, PARAMETER :: pardir = '..'
  !> The character used by the operating system to separate pathname components.
  CHARACTER(len = *), PUBLIC, PARAMETER :: sep = '/'
  !> The character which separates the base filename from the extension.
  CHARACTER(len = *), PUBLIC, PARAMETER :: extsep = '.'
  !> The string used to separate (or, rather, terminate) lines on the current platform.
  CHARACTER(len = *), PUBLIC, PARAMETER :: linesep = '\n'
  !> The file path of the null device.
  CHARACTER(len = *), PUBLIC, PARAMETER :: devnull = '/dev/null'

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------
  !> \brief Get the current working directory.
  !> \author Sebastian Müller
  !> \date Mar 2023
  subroutine get_cwd(path, status, verbose, raise)
#ifdef NAG
    use f90_unix_dir, only : GETCWD
#endif
#ifdef INTEL
    use ifport, only : GETCWD
#endif
    USE mo_message, ONLY: error_message
    use mo_kind, only: i4
    implicit none

    character(*), intent(out) :: path !< the current working directory
    integer(i4), intent(out), optional :: status !< error status (will prevent error raise if present)
    logical, intent(in), optional :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    logical, intent(in), optional :: raise !< Throw an error if current directory can't be determined (default: .true.)

    integer(i4) :: status_
    logical :: raise_

    raise_ = .true.
    if ( present(raise) ) raise_ = raise
    ! prevent raise if error code should be returned
    raise_ = raise_ .and. .not. present(status)

#ifdef INTEL
    status_ = getcwd(path)
#else
    call getcwd(path, status_)
#endif

    if (status_ /= 0) call path_msg("Can't determine current working directory.", verbose=verbose, raise=raise_)
    if ( present(status) ) status = status_

  end subroutine get_cwd

  ! ------------------------------------------------------------------
  !> \brief Change current working directory.
  !> \author Sebastian Müller
  !> \date Mar 2023
  subroutine change_dir(path, status, verbose, raise)
#ifdef NAG
    use f90_unix_dir, only : CHDIR
#endif
#ifdef INTEL
    use ifport, only : CHDIR
#endif
    USE mo_message, ONLY: error_message
    use mo_kind, only: i4
    implicit none

    character(*), intent(in) :: path !< path to change CWD to
    integer(i4), intent(out), optional :: status !< error status (will prevent error raise if present)
    logical, intent(in), optional :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    logical, intent(in), optional :: raise !< Throw an error if directory can't be opened (default: .true.)

    integer(i4) :: status_
    logical :: raise_

    raise_ = .true.
    if ( present(raise) ) raise_ = raise
    ! prevent raise if error code should be returned
    raise_ = raise_ .and. .not. present(status)

#ifdef INTEL
    status_ = chdir(path)
#else
    call chdir(path, status_)
#endif

    if (status_ /= 0) call path_msg("Can't open directory: ", trim(path), verbose, raise_)
    if ( present(status) ) status = status_

  end subroutine change_dir

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

    LOGICAL :: isfile, isdir, exists
    character(:), allocatable :: t_path

    t_path = trim(path)

    call path_isfile(t_path, answer=isfile, verbose=.false., raise=.false.)
    call path_isdir(t_path, answer=isdir, verbose=.false., raise=.false.)
    exists = isfile .or. isdir

    if (.not. exists) call path_msg("Path does not exist: ", t_path, verbose, raise)
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

    LOGICAL :: isfile, isdir
    CHARACTER(:), allocatable :: t_path

    t_path = trim(path)

    inquire(file=t_path, exist=isfile)
    ! gfortran/NAG need the check if it is not a directory explicitly
    call path_isdir(t_path, answer=isdir, verbose=.false., raise=.false.)
    isfile = isfile .and. (.not. isdir)

    if (.not. isfile) call path_msg("File does not exist: ", t_path, verbose, raise)
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

    LOGICAL :: isdir
    CHARACTER(:), allocatable :: t_path

    t_path = trim(path)

#ifdef INTEL
    ! intel has non-standard 'directory' argument
    inquire(directory=t_path, exist=isdir)
#else
    ! append "/" and check if it still exists
    inquire(file=t_path//sep, exist=isdir)
#endif

    if (.not. isdir) call path_msg("Directory does not exist: ", t_path, verbose, raise)
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

    IMPLICIT NONE

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
      do while (.not. (c == extsep .or. c == sep .or. i == 0))
        c = t_path(i:i)
        i = i - 1
      end do
      !checking whether the last symbol of the path is a point or the while-loop run through the whole path
      !without finding a point or ended at a '/'. In any case it is not possible to seperate an extension.
      if (i == len(t_path) - 1 .or. i == 0 .or. c == sep) then
        i = len(t_path)
      endif
    endif

    root = t_path(1:i)
    ext = t_path(i + 1:len(t_path))

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

    IMPLICIT NONE

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
    do while (.not. (c == sep .or. i == 0))
      c = t_path(i:i)
      i = i - 1
    end do
    !checking whether the while-loop run through the whole path without finding a '/'
    if (i == 0) then
      head = ''
      tail = t_path
    else
      head = t_path(1:i + 1)
      tail = t_path(i + 2:len(t_path))
    endif

  END SUBROUTINE path_split

  ! ------------------------------------------------------------------

  subroutine path_msg(msg, path, verbose, raise)
    USE mo_message, ONLY: error_message, message
    implicit none

    CHARACTER(LEN=*), intent(in), optional :: msg
    CHARACTER(LEN=*), intent(in), optional :: path
    logical, intent(in), optional ::  verbose
    logical, intent(in), optional ::  raise
    logical :: raise_
    raise_ = .false.
    if (present(raise)) raise_ = raise
    if (raise_) then
      call error_message(msg, path, show=verbose)
    else
      call message(msg, path, show=verbose)
    endif
  end subroutine path_msg

END MODULE mo_os
