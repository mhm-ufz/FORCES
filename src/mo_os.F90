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
module mo_os

  use mo_kind, only: i4

  implicit none

  public :: get_cwd
  public :: change_dir
  public :: check_path_exists
  public :: check_path_isfile
  public :: check_path_isdir
  public :: path_exists
  public :: path_isfile
  public :: path_isdir
  public :: path_isabs
  public :: path_splitext
  public :: path_split
  public :: path_dirname
  public :: path_basename

  !> The constant string used by the operating system to refer to the current directory.
  character(len = *), public, parameter :: curdir = '.'
  !> The constant string used by the operating system to refer to the parent directory.
  character(len = *), public, parameter :: pardir = '..'
  !> The character used by the operating system to separate pathname components.
  character(len = *), public, parameter :: sep = '/'
  !> The character which separates the base filename from the extension.
  character(len = *), public, parameter :: extsep = '.'
  !> The string used to separate (or, rather, terminate) lines on the current platform.
  character(len = *), public, parameter :: linesep = '\n'
  !> The file path of the null device.
  character(len = *), public, parameter :: devnull = '/dev/null'
  !> Maximum length of a path component (folder/file names).
  integer(i4), public, save :: max_path_comp_len = 256_i4

  private

  ! ------------------------------------------------------------------

contains

  ! ------------------------------------------------------------------
  !> \brief Get the current working directory.
  !> \author Sebastian Müller
  !> \date Mar 2023
  subroutine get_cwd(path, status, verbose, raise)
#ifdef NAG
    use f90_unix_dir, only : getcwd
#endif
#ifdef INTEL
    use ifport, only : getcwd
#endif
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
    use f90_unix_dir, only : chdir
#endif
#ifdef INTEL
    use ifport, only : chdir
#endif
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
  !> \brief Checks whether a given path exists.
  !> \author Nicola Doering
  !> \date Aug 2020
  subroutine check_path_exists(path, answer, verbose, raise)

    implicit none

    character(len=*), intent(in)  :: path !< given path
    logical, intent(out), optional :: answer !< result
    logical, intent(in), optional :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    logical, intent(in), optional :: raise !< Throw an error if path does not exist (default: .false.)

    LOGICAL :: exists

    exists = path_exists(path)
    if (.not. exists) call path_msg("Path does not exist: ", path, verbose, raise)
    if (present(answer)) answer = exists

  end subroutine check_path_exists

  ! ------------------------------------------------------------------
  !> \brief Checks whether a given path exists and describes a file.
  !> \author Nicola Doering
  !> \date Aug 2020
  subroutine check_path_isfile(path, answer, verbose, raise)

    implicit none

    character(len=*), intent(in)  :: path !< given path
    logical, intent(out), optional :: answer !< result
    logical, intent(in), optional :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    logical, intent(in), optional :: raise !< Throw an error if file does not exist (default: .false.)

    LOGICAL :: isfile

    isfile = path_isfile(path)
    if (.not. isfile) call path_msg("File does not exist: ", path, verbose, raise)
    if (present(answer)) answer = isfile

  end subroutine check_path_isfile

  ! ------------------------------------------------------------------
  !> \brief Checks whether a given path exists and describes a directory.
  !> \author Nicola Doering
  !> \date Aug 2020
  subroutine check_path_isdir(path, answer, verbose, raise)

    implicit none

    character(len=*), intent(in)  :: path !< given path
    logical, intent(out), optional :: answer !< result
    logical, intent(in), optional :: verbose !< Be verbose or not (default: setting of SHOW_MSG/SHOW_ERR)
    logical, intent(in), optional :: raise !< Throw an error if dir does not exist (default: .false.)

    logical :: isdir

    isdir = path_isdir(path)
    if (.not. isdir) call path_msg("Directory does not exist: ", path, verbose, raise)
    if (present(answer)) answer = isdir

  end subroutine check_path_isdir

  ! ------------------------------------------------------------------
  !> \brief Return .true. if path refers to an existing path.
  !> \author Sebastian Mueller
  !> \date Mar 2023
  logical function path_exists(path)
    implicit none
    character(len=*), intent(in)  :: path !< given path

    path_exists = path_isfile(path) .or. path_isdir(path)

  end function path_exists

  ! ------------------------------------------------------------------
  !> \brief Return .true. if path is an existing regular file.
  !> \author Sebastian Mueller
  !> \date Mar 2023
  logical function path_isfile(path)
    implicit none
    character(len=*), intent(in)  :: path !< given path

    inquire(file=trim(path), exist=path_isfile)
    ! gfortran/NAG need the check if it is not a directory explicitly
    path_isfile = path_isfile .and. (.not. path_isdir(path))

  end function path_isfile

  ! ------------------------------------------------------------------
  !> \brief Return .true. if path is an existing directory.
  !> \author Sebastian Mueller
  !> \date Mar 2023
  logical function path_isdir(path)
    implicit none
    character(len=*), intent(in)  :: path !< given path

#ifdef INTEL
    ! intel has non-standard 'directory' argument
    inquire(directory=trim(path), exist=path_isdir)
#else
    ! append "/" and check if it still exists
    inquire(file=trim(path)//sep, exist=path_isdir)
#endif

  end function path_isdir

  ! ------------------------------------------------------------------
  !> \brief Return .true. if path is an absolute pathname.
  !> \author Sebastian Müller
  !> \date Mar 2023
  logical function path_isabs(path)
    use mo_string_utils, only : startswith
    implicit none
    character(len=*), intent(in)  :: path !< given path

    ! absolute posix path starts with '/'
    path_isabs = startswith(path, sep)

  end function path_isabs

  ! ------------------------------------------------------------------
  !> \brief Splitting the path into root and ext
  !> \details Splitting the path name into a pair root and ext.
  !!          Here, ext stands for extension and has the extension string
  !!          of the specified path while root is everything except this extension.
  !> \changelog
  !! - Sebastian Müller Mar 2023
  !!   - don't check for folder
  !!   - ignore leading dots in tail of the path
  !!   - make root and ext optional
  !> \author Nicola Doering
  !> \date Aug 2020
  subroutine path_splitext(path, root, ext)

    use mo_string_utils, only: endswith
    implicit none

    character(len=*), intent(in)  :: path !< given path
    character(len=*), intent(out), optional :: root !< root part of path without extension
    character(len=*), intent(out), optional :: ext  !< extension of given path (starting with ".")

    integer   :: lead_i, dot_i
    character(len=len_trim(path)) :: head, tail, lead

    ! only deal with tail of the path
    call path_split(path, head, tail)

    ! check if sep should be added (head not empty and not root)
    if ( (len_trim(head) > 0) .and. .not. endswith(head, sep) ) then
      lead = trim(head) // sep
    else
      lead = trim(head)
    end if

    ! ignore leading dots of the tail ("...a" has no extension)
    do lead_i = 1, len_trim(tail) + 1
      if ( lead_i > len_trim(tail) ) exit ! only dots
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
  !> \brief Return the directory name of pathname path.
  !> \details This is the first element of the pair returned by passing path to the subroutine path_split.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_dirname(path) result(dirname)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(len=*)              :: dirname !< dirname

    call path_split(path, head=dirname)

  end function path_dirname

  ! ------------------------------------------------------------------
  !> \brief Return the directory name of pathname path.
  !> \details This is the first element of the pair returned by passing path to the subroutine path_split.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_basename(path) result(basename)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(len=*)              :: basename !< basename

    call path_split(path, tail=basename)

  end function path_basename

  ! ------------------------------------------------------------------

  subroutine path_msg(msg, path, verbose, raise)
    use mo_message, only: error_message, message
    implicit none
    character(len=*), intent(in), optional :: msg
    character(len=*), intent(in), optional :: path
    logical, intent(in), optional ::  verbose
    logical, intent(in), optional ::  raise

    logical :: raise_

    raise_ = .false.
    if (present(raise)) raise_ = raise
    if (raise_) then
      call error_message(msg, trim(path), show=verbose)
    else
      call message(msg, trim(path), show=verbose)
    endif

  end subroutine path_msg

end module mo_os
