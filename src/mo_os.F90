!> \file mo_os.f90
!> \copydoc mo_os

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
  public :: path_isroot
  public :: path_splitext
  public :: path_split
  public :: path_parts
  public :: path_dirname
  public :: path_basename
  public :: path_root
  public :: path_ext
  public :: path_stem
  public :: path_as_posix
  public :: path_normpath
  public :: path_abspath
  public :: path_join

  !> \brief Join given path segments with separator if needed.
  !> \details If a segment is an absolute path, the previous ones will be ignored.
  !> \author Sebastian Müller
  !> \date Mar 2023
  interface path_join
    module procedure :: path_join_char_opt
    module procedure :: path_join_arr
  end interface

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
  !> Maximum length of a path (16 max. length components).
  integer(i4), public, save :: max_path_len = 4096_i4

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

#ifdef NAG
    call getcwd(path, errno=status_)
#else
    ! gfortran and intel can use a function
    status_ = getcwd(path)
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

#ifdef NAG
    call chdir(trim(path), errno=status_)
#else
    ! gfortran and intel can use a function
    status_ = chdir(path)
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
  !> \brief Return .true. if path is root ('/' or '//' or '///' and so on).
  !> \author Sebastian Müller
  !> \date Mar 2023
  logical function path_isroot(path)
    use mo_string_utils, only : startswith
    implicit none
    character(len=*), intent(in)  :: path !< given path

    integer   :: i

    do i=len_trim(path), 0, -1
      if (i == 0) exit ! only sep found or empty
      if (path(i:i) /= sep) exit
    end do
    path_isroot = (i == 0) .and. (len_trim(path) > 0)

  end function path_isroot

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

    integer   :: lead_i, dot_i, sep_i
    character(len=len_trim(path)) :: head, tail

    ! find last '/' and split there
    sep_i = index(trim(path), sep, back=.true.)
    head = path(1:sep_i)
    tail = path(sep_i+1:len_trim(path))

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
      if (present(root)) root = trim(head) // tail(1:dot_i-1)
    else
      ! no dot found at all or the leading dots are found
      if (present(ext)) ext = ""
      if (present(root)) root = trim(head) // trim(tail)
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
        if (i == 0) exit ! only sep found
        if (head_(i:i) /= sep) exit
      end do
      if (i == 0) i = len_trim(head_) ! all characters are '/'
      head = head_(1:i)
    endif

  end subroutine path_split

  ! ------------------------------------------------------------------
  !>\brief Splitting the path into its components.
  !>\author Sebastian Müller
  !>\date Mar 2023
  subroutine path_parts(path, parts)
    use mo_append, only : append
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(len=len_trim(path)), allocatable, intent(out) :: parts(:) !< parts of the given path

    integer   :: i
    character(len=len_trim(path)) :: temp, comp

    ! create array to join
    temp = trim(path)
    allocate(parts(0))
    ! stop if we can't further split the path
    do while (len_trim(temp) > 0)
      if (path_isroot(temp)) then
        ! POSIX allows one or two initial slashes, but treats three or more as single slash.
        if (len_trim(temp) == 2) then
          call append(parts, temp)
        else
          call append(parts, sep)
        end if
        exit
      end if
      ! get next component
      call path_split(temp, temp, comp)
      if (len_trim(comp) > 0) call append(parts, comp)
    end do

    ! reverse only if 2 or more elements
    if (size(parts) > 1) parts = [(parts(i), i = size(parts), 1, -1)]

  end subroutine path_parts

  ! ------------------------------------------------------------------
  !> \brief Return the directory name of pathname path.
  !> \details This is the first element of the pair returned by passing path to the subroutine path_split.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_dirname(path) result(dirname)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: dirname !< dirname

    character(len=len_trim(path)) :: temp

    call path_split(path, head=temp)
    dirname = trim(temp)

  end function path_dirname

  ! ------------------------------------------------------------------
  !> \brief Return the base name of pathname path.
  !> \details This is the second element of the pair returned by passing path to the subroutine path_split.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_basename(path) result(basename)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: basename !< basename

    character(len=len_trim(path)) :: temp

    call path_split(path, tail=temp)
    basename = trim(temp)

  end function path_basename

  ! ------------------------------------------------------------------
  !> \brief Return the path without its suffix.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_root(path) result(root)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: root !< root

    character(len=len_trim(path)) :: temp

    call path_splitext(path, root=temp)
    root = trim(temp)

  end function path_root

  ! ------------------------------------------------------------------
  !> \brief Return the file extension of the final path component.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_ext(path) result(ext)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: ext !< ext

    character(len=len_trim(path)) :: temp

    call path_splitext(path, ext=temp)
    ext = trim(temp)

  end function path_ext

  ! ------------------------------------------------------------------
  !> \brief Return the final path component without its suffix.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_stem(path) result(stem)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: stem !< stem

    character(len=len_trim(path)) :: temp, tail

    call path_split(path, tail=tail)
    call path_splitext(tail, root=temp)
    stem = trim(temp)

  end function path_stem

  ! ------------------------------------------------------------------
  !> \brief Return the string representation of the path with forward (/) slashes.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_as_posix(path) result(posix)
    use mo_string_utils, only : replace_text
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: posix !< posix version of the path

    posix = trim(replace_text(path, "\\", sep))

  end function path_as_posix

  ! ------------------------------------------------------------------
  !> \brief Normalize a pathname by collapsing redundant separators and up-level references.
  !> \details Normalize a pathname by collapsing redundant separators and up-level references so that
  !! A//B, A/B/, A/./B and A/foo/../B all become A/B.
  !! This string manipulation may change the meaning of a path that contains symbolic links.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_normpath(path) result(normpath)
    use mo_append, only : append
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: normpath !< normalized path

    character(len=len_trim(path)) :: temp, comp, root
    character(len=len_trim(path)), allocatable :: comps_raw(:), comps(:)
    integer :: i
    logical :: has_root ! flag to indicate an absolute path

    ! get path components
    call path_parts(path, comps_raw)
    ! return '.' for empty path
    if (size(comps_raw) == 0) then
      normpath = curdir
      return
    end if

    has_root = path_isroot(comps_raw(1))

    allocate(comps(0))
    ! care about '.' and '..'
    do i = 1, size(comps_raw)
      comp = comps_raw(i)
      if ( len_trim(comp) == 0 ) cycle ! skip empty
      if ( trim(comp) == curdir ) cycle ! skip '.'
      ! handle '..'
      if ( trim(comp) /= pardir ) then
        ! append normal component
        call append(comps, comp)
      else if (.not. has_root .and. (size(comps) == 0)) then
        ! if '..' but we can't pop anything, append
        call append(comps, comp)
      else if ( size(comps) > 0 ) then
        if (comps(size(comps)) == pardir) then
          ! if '..' but previous is also '..', append
          call append(comps, comp)
        else if (.not. path_isroot(comps(size(comps)))) then
          ! if '..' pop previous folder if it is not root
          call pop(comps)
        end if
      end if
      ! in all other cases, don't append anything
    end do

    if (size(comps) > 0) then
      normpath = path_join_arr(comps)
    else
      ! '.' if no components given
      normpath = curdir
    end if

  end function path_normpath

  ! ------------------------------------------------------------------
  !> \brief Return a normalized absolutized version of the given path.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_abspath(path) result(abspath)
    implicit none
    character(len=*), intent(in)  :: path !< given path
    character(:), allocatable     :: abspath !< stem

    character(len=max_path_len) :: cwd

    call get_cwd(cwd)
    abspath = path_normpath(path_join_char(cwd, path))

  end function path_abspath

  ! ------------------------------------------------------------------
  !> \brief Join two path segments with separator if needed.
  !> \details If the second segment is an absolute path, the first one will be ignored.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_join_char(p1, p2) result(join)
    use mo_string_utils, only : endswith
    implicit none
    character(len=*), intent(in)  :: p1, p2 ! given paths
    character(:), allocatable     :: join !< joined paths

    if (path_isabs(p2)) then
      ! if second path is absolute, first path gets ignored
      join = trim(p2)
    else
      ! check if sep should be added (p1 not empty and not ending with sep)
      if ( (len_trim(p1) > 0) .and. .not. endswith(p1, sep) ) then
        join = trim(p1) // sep // trim(p2)
      else
        join = trim(p1) // trim(p2)
      end if
    end if

  end function path_join_char

  ! ------------------------------------------------------------------
  !> \brief Join given path segments with separator if needed.
  !> \details If a segment is an absolute path, the previous ones will be ignored.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_join_char_opt(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16) result(join)
    use mo_string_utils, only : endswith
    implicit none
    character(len=*), intent(in)           :: p1 !< initial path
    character(len=*), intent(in), optional :: p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16 ! given paths
    character(:), allocatable              :: join !< joined paths

    join = p1
    if (present(p2)) join = path_join_char(join, p2)
    if (present(p3)) join = path_join_char(join, p3)
    if (present(p4)) join = path_join_char(join, p4)
    if (present(p5)) join = path_join_char(join, p5)
    if (present(p6)) join = path_join_char(join, p6)
    if (present(p7)) join = path_join_char(join, p7)
    if (present(p8)) join = path_join_char(join, p8)
    if (present(p9)) join = path_join_char(join, p9)
    if (present(p10)) join = path_join_char(join, p10)
    if (present(p11)) join = path_join_char(join, p11)
    if (present(p12)) join = path_join_char(join, p12)
    if (present(p13)) join = path_join_char(join, p13)
    if (present(p14)) join = path_join_char(join, p14)
    if (present(p15)) join = path_join_char(join, p15)
    if (present(p16)) join = path_join_char(join, p16)

  end function path_join_char_opt

  ! ------------------------------------------------------------------
  !> \brief Join given path segments with separator if needed.
  !> \details If a segment is an absolute path, the previous ones will be ignored.
  !> \author Sebastian Müller
  !> \date Mar 2023
  function path_join_arr(paths) result(join)
    implicit none
    character(len=*), dimension(:), intent(in)  :: paths !< given paths
    character(:), allocatable                   :: join !< joined paths

    integer(i4) :: i

    join = ""
    do i = 1, size(paths)
      join = path_join_char(join, paths(i))
    end do
  end function path_join_arr

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
    if (raise_) then                                    ! LCOV_EXCL_LINE
      call error_message(msg, trim(path), show=verbose) ! LCOV_EXCL_LINE
    else
      call message(msg, trim(path), show=verbose)
    endif

  end subroutine path_msg

  !> \brief character array pop
  subroutine pop(arr)
    ! TODO: move to mo_utils; add other type versions
    implicit none
    character(len=*), allocatable, intent(inout) :: arr(:)

    character(:), allocatable :: temp(:)

    if (.not.allocated(arr)) return
    if (size(arr) == 0) return

    allocate(character(len(arr(1))) :: temp(size(arr)-1))
    temp(:) = arr(1:size(arr)-1)
    call move_alloc(temp, arr)

  end subroutine pop

end module mo_os
