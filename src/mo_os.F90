!> \file mo_os.f90
!> \brief \copybrief mo_os
!> \details \copydetails mo_os

!> \brief Path and directory management.
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
  SUBROUTINE path_exists(path, result, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if path does not exist (default: .false.)
    LOGICAL, INTENT(OUT), OPTIONAL :: result !< result

    LOGICAL :: exists, raise_
    character(:), allocatable :: t_path
    CHARACTER(*), parameter :: msg = "Path does not exist: "

    raise_ = .false.
    if (present(raise)) raise_ = raise

    t_path = trim(path)
    inquire (file=t_path, exist=exists)

#ifdef INTEL
    if (.not. exists) inquire (directory=t_path, exist=exists)
#endif

    if (.not. exists) then
      if (raise_) then
        call error_message(msg, t_path, show=verbose)
      else
        call message(msg, t_path, show=verbose)
      endif
    endif

    if (present(result)) result = exists

  END SUBROUTINE path_exists

  ! ------------------------------------------------------------------
  !>        \brief Whether the path describes a file.
  !>        \details Checks whether a given path exists and describes a file.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_isfile(path, result, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(OUT), OPTIONAL :: result !< result
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if file does not exist (default: .false.)

    LOGICAL :: isfile, raise_, exists
    CHARACTER(:), allocatable :: t_path
    CHARACTER(*), parameter :: msg = "Path describes a directory and not a file: "

    t_path = trim(path)

    raise_ = .false.
    if (present(raise)) raise_ = raise

    isfile = .true.

    call path_exists(t_path, result=exists, verbose=verbose, raise=raise_)
    if (exists) then
      !checking whether the path is ending with '/' or '/.' which would indicates a directory
      if (t_path(len(t_path):len(t_path)) .eq. '/' .or. t_path(len(t_path) - 1:len(t_path)) .eq. '/.') then
        isfile = .false.
      else
        !checking whether would still exist if '/.' is added to the end, in this case it is a directory
#ifdef INTEL
        inquire (directory=t_path//'/.', exist=exists)
#else
        inquire (file=t_path//'/.', exist=exists)
#endif
        if (exists) then
          isfile = .false.
          if (raise_) then
            call error_message(msg, t_path, show=verbose)
          else
            call message(msg, t_path, show=verbose)
          endif
        endif
      endif
    else
      isfile = .false.
    endif

    if (present(result)) result = isfile

  END SUBROUTINE path_isfile

  ! ------------------------------------------------------------------
  !>        \brief Whether the path describes a directory.
  !>        \details Checks whether a given path exists and describes a directory.
  !>        \author Nicola Doering
  !>        \date Aug 2020
  SUBROUTINE path_isdir(path, result, verbose, raise)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(IN)  :: path !< given path
    LOGICAL, INTENT(OUT), OPTIONAL :: result !< result
    LOGICAL, INTENT(IN), OPTIONAL :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    LOGICAL, INTENT(IN), OPTIONAL :: raise !< Throw an error if dir does not exist (default: .false.)

    LOGICAL :: isdir, exists, raise_
    CHARACTER(:), allocatable :: t_path
    CHARACTER(*), parameter :: msg = "Path describes a file and not a directory: "

    t_path = trim(path)

    raise_ = .false.
    if (present(raise)) raise_ = raise

    isdir = .true.

    call path_exists(t_path, result=exists, verbose=verbose, raise=raise_)
    if (exists) then
      call path_isfile(t_path, result=exists, verbose=.false., raise=.false.)
      if (exists) then
        isdir = .false.
        if (raise_) then
          call error_message(msg, t_path, show=verbose)
        else
          call message(msg, t_path, show=verbose)
        endif
      endif
    else
      isdir = .false.
    endif

    if (present(result)) result = isdir

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

    !Checking, whether the path describes a directory so it cannot ends with an extension.
    call path_isdir(t_path, result=isdir, verbose=.false., raise=.false.)
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

END MODULE mo_os
