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
  !> \brief Existence of a path
  !> \details Checks whether a given path exists.
  !> \author Nicola Doering
  !> \date Aug 2020
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
  !> \brief Whether the path describes a file.
  !> \details Checks whether a given path exists and describes a file.
  !> \author Nicola Doering
  !> \date Aug 2020
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
  !> \brief Whether the path describes a directory.
  !> \details Checks whether a given path exists and describes a directory.
  !> \author Nicola Doering
  !> \date Aug 2020
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
  !> \brief Splitting the path into root and ext
  !> \details Splitting the path name into a pair root and ext.
  !!          Here, ext stands for extension and has the extension string
  !!          of the specified path while root is everything except this extension.
  !> \changelog
  !! - Sebastian Müller Mar 2023
  !!   - don't check for folder
  !!   - ignore leading dots in tail of the path
  !> \author Nicola Doering
  !> \date Aug 2020
  subroutine path_splitext(path, root, ext)

    implicit none

    character(len=*), intent(in)  :: path !< given path
    character(len=*), intent(out), optional :: root !< root part of path without extension
    character(len=*), intent(out), optional :: ext  !< extension of given path (starting with ".")

    integer   :: lead_i, dot_i
    character(len=len_trim(path)) :: head, tail, lead

    ! only deal with tail of the path
    call path_split(path, head, tail)

    ! check if sep should be added (head not empty and not root)
    if ( (len_trim(head) > 0) .and. (head(len_trim(head):len_trim(head)) /= sep) ) then
      lead = trim(head) // sep
    else
      lead = trim(head)
    end if

    ! ignore leading dots of the tail ("...a" has no extension)
    do lead_i = 1, len_trim(tail) + 1
      if ( lead_i > len_trim(tail) ) then
        ! only dots in tail
        if (present(ext)) ext = ""
        if (present(root)) root = trim(lead) // trim(tail)
        return
      end if
      if ( tail(lead_i:lead_i) /= extsep ) exit
    end do

    ! check for last dot in tail to split extension
    dot_i = index(trim(tail), extsep, back=.true.)
    ! last dot needs to come after leading dots to indicate an extension
    if ( dot_i > lead_i ) then
      ! dot_i is at least 2 here
      if (present(ext)) ext = tail(dot_i:len_trim(tail))
      if (present(root)) root = trim(lead) // tail(1:dot_i-1)
    else
      ! no dot found at all or the leading dots are found
      if (present(ext)) ext = ""
      if (present(root)) root = trim(lead) // trim(tail)
    end if

  end subroutine path_splitext

  ! ------------------------------------------------------------------
  !>\brief Splitting the path into head and tail
  !>\details Splitting the path name into a pair head and tail.
  !!         Here, tail is the last path name component and head is
  !!         everything leading up to that.
  !!         If the path ends with an '/' tail is returned empty and
  !!         if there is no '/' in path head is returned empty.
  !!         Trailing '/'es are stripped from head unless it is the root.
  !> \changelog
  !! - Sebastian Müller Mar 2023
  !!   - remove trailing '/' from head unleass it is root (e.g. '/' or '//' or '///' and so on)
  !!   - make head and tail optional
  !>\author Nicola Doering
  !>\date Aug 2020
  subroutine path_split(path, head, tail)

    implicit none

    character(len=*), intent(in)  :: path !< given path
    character(len=*), intent(out), optional :: head !< everything leading up to the last path component
    character(len=*), intent(out), optional :: tail !< last pathname component

    integer   :: i
    character(len=len_trim(path)) :: head_

    ! find last '/'
    i = index(trim(path), sep, back=.true.)

    if (i == 0) then
      ! no '/' found
      if (present(tail)) tail = trim(path)
      if (present(head)) head = ''
    else
      if (present(tail)) tail = path((i+1):len_trim(path))
      if (.not. present(head)) return
      head_ = path(1:i)
      ! remove trailing '/' from head unless it is root
      do i=len_trim(head_), 0, -1
        if (i == 0) exit ! no sep found
        if (head_(i:i) /= sep) exit
      end do
      if (i == 0) i = len_trim(head_) ! all characters are '/'
      head = head_(1:i)
    endif

  end subroutine path_split

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
