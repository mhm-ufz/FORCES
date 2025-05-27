!> \file    mo_cli.f90
!> \copydoc mo_cli

!> \brief   Module to parse command line arguments.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    May 2021
!> \details A simple parser for command line arguments.
!!          You can define options and then parse the given command.
!!          Option can be with or without passed values and they can be set
!!          as required.
!!
!!          The following example demonstrates the functionality:
!!          \code{.f90}
!!          program main
!!            use mo_cli, only: cli_parser
!!            implicit none
!!            type(cli_parser) :: parser
!!
!!            parser = cli_parser( &
!!              description='This program has a CLI.', &
!!              add_version_option=.true., version='1.3')
!!            call parser%add_option( &
!!              'cwd', &
!!              blank=.true., &
!!              required=.true., &
!!              help='The working directory.')
!!            call parser%add_option( &
!!              name='file', &
!!              s_name='f', &
!!              has_value=.true., &
!!              value_name='path', &
!!              default='none', &
!!              help='Your file path.')
!!            call parser%add_option('opt', help='A switch')
!!
!!            call parser%parse()
!!
!!            print*, 'file: ', parser%option_value('file')
!!            print*, 'dir: ', parser%option_value('cwd')
!!            print*, 'opt: ', parser%option_was_read('opt')
!!
!!          end program main
!!          \endcode
!!          You can call the program with:
!!          \code{.sh}
!!          $ ./prog --opt -f file.txt /dir/
!!           file: file.txt
!!           dir: /dir/
!!           opt:  T
!!          \endcode
!!          As you see, you can automatically create help and version options:
!!          \code{.sh}
!!          $ ./prog -h
!!          $ ./prog -V
!!          \endcode
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_cli

  use mo_kind, only: i4
  use mo_message, only: error_message, message

  implicit none

  private

  !> \class   option
  !> \brief   This is a container for a single command line option.
  type, public :: option
    character(:), allocatable :: help !< description of the option
    character(:), allocatable :: name !< long name (will be double hyphenated: --opt)
    character(1) :: s_name = "" !< short name (will be hyphenated: -o)
    logical :: has_s_name = .false. !< whether the option has a short name
    logical :: required = .false. !< whether the option is required
    logical :: blank = .false. !< whether the option is passed blank without hyphenated name (only latter one possible)
    logical :: was_read = .false. !< whether the option was read from command line
    logical :: has_value = .false. !< whether the option has a value
    logical :: has_default = .false. !< whether the option has a default value
    logical :: repeated = .false. !< whether the option can be read repeatedly
    integer(i4) :: read_count = 0_i4 !< number of reads (-ooo)
    character(:), allocatable :: value !< value of the option (if has one)
    character(:), allocatable :: value_name !< name of the value for the help text (default "value")
    character(:), allocatable :: default !< default value of the option (if has one)
  contains
    procedure :: print_info
    procedure :: is_given_arg
  end type option

  interface option
    procedure new_option
  end interface option

  !> \class   cli_parser
  !> \brief   This is a parser for command line arguments.
  !> \details \copydetails mo_cli
  type, public :: cli_parser
    character(:), allocatable :: prog !< Program name (default will be arg(0)).
    character(:), allocatable :: description !< help text for the cli
    character(:), allocatable :: version !< Program version
    logical :: has_help = .true. !< whether the parser cares about the help text (--help / -h)
    logical :: has_version = .false. !< whether the parser cares about the version text (--version / -V)
    logical :: has_blank_option = .false. !< whether the parser has a blank option.
    logical :: has_logger = .false. !< whether the parser should setup the logger.
    type(option), dimension(:), allocatable :: options !< defined options
  contains
    procedure :: add_option
    procedure :: get_option
    procedure :: get_option_index
    procedure :: cnt_options
    procedure :: option_was_read
    procedure :: option_read_count
    procedure :: has_option
    procedure :: get_blank_option_index
    procedure :: option_value
    procedure :: print_help
    procedure :: parse
  end type cli_parser

  interface cli_parser
    procedure new_cli_parser
  end interface cli_parser

contains

  !> \brief Create a new \ref cli_parser.
  !> \return The new \ref cli_parser.
  type(cli_parser) function new_cli_parser(prog, description, add_help_option, add_version_option, version, add_logger_options)
    use mo_os, only: path_split
    implicit none
    character(*), optional, intent(in) :: prog !< Program name (default will be arg(0))
    character(*), optional, intent(in) :: description !< help text for the cli
    logical, optional, intent(in) :: add_help_option !< whether to add a help option (--help, -h)
    logical, optional, intent(in) :: add_version_option !< whether to add a version option (--version, -V)
    character(*), optional, intent(in) :: version !< Program version
    logical, optional, intent(in) :: add_logger_options !< whether to add a logger options (--verbose, --quite, ...)

    integer(i4) :: n
    character(:), allocatable :: arg, prog_

    allocate(new_cli_parser%options(0))

    if (present(prog)) then
      new_cli_parser%prog = prog
    else
      call get_command_argument(0, length=n)
      allocate(character(n) :: arg, prog_)
      call get_command_argument(0, value=arg)
      call path_split(arg, tail=prog_)
      new_cli_parser%prog = trim(prog_)
    end if

    new_cli_parser%description = "Command line options."
    if (present(description)) new_cli_parser%description = description
    if (present(add_help_option)) new_cli_parser%has_help = add_help_option
    if (present(add_version_option)) new_cli_parser%has_version = add_version_option
    if (present(add_logger_options)) new_cli_parser%has_logger = add_logger_options

    if (new_cli_parser%has_help) call new_cli_parser%add_option( &
      name="help", s_name="h", help="Print this help message.")

    if (new_cli_parser%has_version .and. (.not. present(version))) &
      call error_message("cli_parser: when adding the version option, you need to provide a version")
    if (new_cli_parser%has_version) call new_cli_parser%add_option( &
      name="version", s_name="V", help="Print the version of the program.")
    new_cli_parser%version = ""
    if (present(version)) new_cli_parser%version = version
    ! add logging options
    if (new_cli_parser%has_logger) then
      call new_cli_parser%add_option( &
        name="verbose", s_name="v", repeated=.true., help="Increase logging verbosity level.")
      call new_cli_parser%add_option( &
        name="quiet", s_name="q", repeated=.true., help="Decrease logging verbosity level.")
      call new_cli_parser%add_option( &
        name="log-output-hostname", help="Output hostname while logging.")
      call new_cli_parser%add_option( &
        name="log-force-colors", help="Forces colors for the logger.")
      call new_cli_parser%add_option( &
        name="log-no-colors", help="Disable colors while logging.")
      call new_cli_parser%add_option( &
        name="log-no-format", help="Disable formatting while logging.")
      call new_cli_parser%add_option( &
        name="log-output-date", help="Output date while logging.")
      call new_cli_parser%add_option( &
        name="log-output-time", help="Output time while logging.")
    end if
  end function new_cli_parser

  !> \brief Create a new \ref option.
  !> \return The new \ref option.
  type(option) function new_option(name, s_name, help, has_value, value_name, default, required, blank, repeated)
    implicit none
    character(*), intent(in) :: name !< long name (will be double hyphenated: --opt)
    character(1), optional, intent(in) :: s_name !< short name (will be hyphenated: -o)
    character(*), optional, intent(in) :: help !< description of the option
    logical, optional, intent(in) :: has_value !< whether the option has a value
    character(*), optional, intent(in) :: value_name !< name of the value for the help text (default "value")
    character(*), optional, intent(in) :: default !< default value for this option
    logical, optional, intent(in) :: required !< whether the option is required
    logical, optional, intent(in) :: blank !< whether the option is passed blank without hyphenated name (only latter one possible)
    logical, optional, intent(in) :: repeated !< whether the option can be read repeatedly

    new_option%help = "No description"
    if (present(help)) new_option%help = help

    if (len(name) <= 1_i4) &
      call error_message("option: long-name needs at least 2 characters: " // name)
    new_option%name = name

    new_option%has_s_name = present(s_name)
    if (new_option%has_s_name) new_option%s_name = s_name
    if (new_option%has_s_name .and. (new_option%s_name == " ")) &
      call error_message("option: short name needs to be non empty: " // name)

    if (present(required)) new_option%required = required
    if (present(blank)) new_option%blank = blank
    if (present(has_value)) then
      new_option%has_value = has_value
      if ((.not. new_option%has_value) .and. new_option%blank) &
        call error_message("option: blank option needs a value: " // name)
    else
      new_option%has_value = new_option%blank
    end if

    new_option%value = ""
    new_option%value_name = ""
    new_option%default = ""
    if (new_option%has_value) then
      new_option%value_name = "value"
      if (present(value_name)) new_option%value_name = value_name
      if ((.not. present(value_name)) .and. new_option%blank) new_option%value_name = name
      new_option%has_default = present(default)
      if (new_option%has_default) new_option%default = default
    end if

    if ((.not. new_option%has_value) .and. new_option%required) &
      call error_message("option: option without value can't be required: " // name)

    if (new_option%has_value .and. new_option%has_default .and. new_option%required) &
      call error_message("option: option with defined default value can't be required: " // name)

    if (present(repeated)) new_option%repeated = repeated
    if (new_option%repeated .and. new_option%has_value) &
      call error_message("option: repeatedly readable options shouldn't expect a value: " // name)

  end function new_option

  !> \brief Add a new \ref option to the \ref cli_parser.
  subroutine add_option(self, name, s_name, help, has_value, value_name, default, required, blank, repeated)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< long name (will be double hyphenated: --opt)
    character(1), optional, intent(in) :: s_name !< short name (will be hyphenated: -o)
    character(*), optional, intent(in) :: help !< description of the option
    logical, optional, intent(in) :: has_value !< whether the option has a value
    character(*), optional, intent(in) :: value_name !< name of the value for the help text (default "value")
    character(*), optional, intent(in) :: default !< default value for this option
    logical, optional, intent(in) :: required !< whether the option is required
    logical, optional, intent(in) :: blank !< whether the option is passed blank without hyphenated name (only latter one possible)
    logical, optional, intent(in) :: repeated !< whether the option can be read repeatedly

    type(option), dimension(size(self%options)) :: tmp_options
    type(option) :: added_option
    integer(i4) :: i

    added_option = option(name, s_name, help, has_value, value_name, default, required, blank, repeated)
    if (added_option%blank .and. self%has_blank_option) then
      call error_message("cli_parser%add_option: only one blank option possible: " // name)
    else if (added_option%blank) then
      self%has_blank_option = .true.
    end if

    tmp_options = self%options
    do i = 1, size(tmp_options)
      if (tmp_options(i)%name == added_option%name) &
        call error_message("cli_parser%add_option: name already present: " // added_option%name)
      if (tmp_options(i)%has_s_name .and. added_option%has_s_name &
          .and. (tmp_options(i)%s_name == added_option%s_name)) &
        call error_message("cli_parser%add_option: short name already present: " // added_option%s_name)
    end do

    deallocate(self%options)
    allocate(self%options(size(tmp_options) + 1))
    self%options(1:size(tmp_options)) = tmp_options
    self%options(size(tmp_options) + 1) = added_option

  end subroutine add_option

  !> \brief Get \ref option count from the \ref cli_parser.
  !> \return Option count.
  integer(i4) function cnt_options(self)
    implicit none
    class(cli_parser), intent(inout) :: self

    cnt_options = size(self%options)

  end function cnt_options

  !> \brief check if this \ref option is the given argument.
  !> \return Truth value if the given argument is this \ref option.
  logical function is_given_arg(self, arg)
    implicit none
    class(option), intent(inout) :: self
    character(*), intent(in) :: arg

    is_given_arg = (arg == "--" // self%name) .or. (arg == "-" // self%s_name)

  end function is_given_arg

  !> \brief Get the \ref option index from \ref cli_parser by name.
  !> \return The desired \ref option index.
  integer(i4) function get_option_index(self, name, long, short, raise_error)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name of the desired option
    logical, intent(in), optional :: long !< whether to check long name (default: .true.)
    logical, intent(in), optional :: short !< whether to check short name (default: .true.)
    logical, intent(in), optional :: raise_error !< whether to raise an error if option is not found (default: .true.)

    integer(i4) :: i
    logical :: raise_error_, long_, short_

    raise_error_ = .true.
    long_ = .true.
    short_ = .true.
    if ( present(raise_error) ) raise_error_ = raise_error
    if ( present(long) ) long_ = long
    if ( present(short) ) short_ = short

    ! find the corresponding argument
    get_option_index = 0_i4
    do i = 1, self%cnt_options()
      if ((long_ .and. self%options(i)%name == name) .or. (short_ .and. self%options(i)%s_name == name)) then
        get_option_index = i
        exit
      end if
    end do

    if (get_option_index == 0_i4 .and. raise_error_) call error_message("cli_parser: unknown option: " // name)

  end function get_option_index

  !> \brief Get an \ref option from \ref cli_parser by name.
  !> \return The desired \ref option.
  type(option) function get_option(self, name)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name (long or short) of the desired option

    integer(i4) :: i

    i = self%get_option_index(name)
    get_option = self%options(i)

  end function get_option

  !> \brief Whether the \ref option was read by the \ref cli_parser given by name.
  !> \return Truth value if the given \ref option was read.
  logical function option_was_read(self, name)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name of the desired option

    type(option) :: opt

    opt = self%get_option(name)
    option_was_read = opt%was_read

  end function option_was_read

  !> \brief Read count for the \ref option in the \ref cli_parser given by name.
  !> \return Number of reads for the \ref option.
  integer(i4) function option_read_count(self, name)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name of the desired option

    type(option) :: opt

    opt = self%get_option(name)
    option_read_count = opt%read_count

  end function option_read_count

  !> \brief Whether the \ref option is defined in \ref cli_parser given by name.
  !> \return Truth value if the given \ref option was defined.
  logical function has_option(self, name)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name of the desired option

    has_option = self%get_option_index(name, raise_error=.false.) > 0

  end function has_option

  !> \brief Get the index of the blank \ref option.
  !> \return The desired \ref option index.
  integer(i4) function get_blank_option_index(self)
    implicit none
    class(cli_parser), intent(inout) :: self

    integer(i4) :: i

    if (.not. self%has_blank_option) &
      call error_message("cli_parser%get_blank_option_index: no blank option defined.")

    ! find the corresponding argument
    do i = 1, self%cnt_options()
      if (self%options(i)%blank) then
        get_blank_option_index = i
        exit
      end if
    end do

  end function get_blank_option_index

  !> \brief Get the parsed value from an \ref option by name from the \ref cli_parser.
  !> \return Value of the given \ref option.
  function option_value(self, name)
    implicit none
    class(cli_parser), intent(inout) :: self
    character(*), intent(in) :: name !< name of the desired option

    character(:), allocatable :: option_value
    type(option) :: opt

    opt = self%get_option(name)
    if (.not. opt%has_value) &
      call error_message("cli_parser%option_value: option has no value: " // name)
    option_value = opt%value

  end function option_value

  !> \brief Print info for an \ref option.
  subroutine print_info(self)
    implicit none
    class(option), intent(inout) :: self

    character(:), allocatable :: opt_str

    ! default values
    opt_str = ""
    if (self%blank) then
      opt_str = "  <" // self%value_name // ">"
    else
      opt_str = "  --" // self%name
      if (self%has_s_name) opt_str = opt_str // " / -" // self%s_name
      if (self%has_value) opt_str = opt_str // " <" // self%value_name // ">"
    end if

    call message(opt_str)
    call message("      Description: ", self%help)
    if (self%has_default) call message("      Default: ", self%default)
    if (self%repeated) call message("      Can be repeated.")
    if (self%required) call message("      (required)")

  end subroutine print_info

  !> \brief Print help message for the \ref cli_parser.
  subroutine print_help(self)
    implicit none
    class(cli_parser), intent(inout) :: self

    integer(i4) :: i
    character(:), allocatable :: blank_str

    blank_str = ""
    if (self%has_blank_option) blank_str = " <" // self%options(self%get_blank_option_index())%value_name // ">"

    call message(self%description)
    call message("")
    call message("  Usage: ", self%prog, " [options]", blank_str)
    call message("")
    call message("Options:")

    ! blank option
    if (self%has_blank_option) call self%options(self%get_blank_option_index())%print_info

    ! required
    do i = 1, self%cnt_options()
      if ((.not. self%options(i)%required) .or. self%options(i)%blank) cycle
      call message("")
      call self%options(i)%print_info
    end do

    ! optional
    do i = 1, self%cnt_options()
      if (self%options(i)%required .or. self%options(i)%blank) cycle
      call message("")
      call self%options(i)%print_info
    end do

  end subroutine print_help

  !> \brief Parse the given command line arguments with the \ref cli_parser.
  subroutine parse(self)
    use mo_logging, only: log_set_config
    implicit none
    class(cli_parser), intent(inout) :: self

    logical :: is_multi, long
    integer(i4) :: i, j, id, n
    character(:), allocatable :: arg, val, err_name, names(:)
    integer(i4), allocatable :: counts(:)

    i = 1_i4
    arg_loop: do while (i <= command_argument_count())
      call get_command_argument(i, length=n)
      if (allocated(arg)) deallocate(arg)
      allocate(character(n) :: arg)
      call get_command_argument(i, value=arg)
      ! arguments need to start with "-"
      if (.not. arg(1:1) == "-") then
        if (self%has_blank_option .and. i == command_argument_count()) then
          self%options(self%get_blank_option_index())%was_read = .true.
          self%options(self%get_blank_option_index())%value = arg
          exit arg_loop
        else
          call error_message("cli_parser%parse: unknown argument: " // arg)
        end if
      end if
      ! check for repeated values with short name (-ooo)
      call parse_arg(arg, names, counts)
      long = arg(2:2) == "-" ! after parse_arg, we know size(arg) > 1
      is_multi = sum(counts) > 1
      do j = 1, size(names)
        ! will raise an error if option not present
        id = self%get_option_index(names(j), long=long, short=.not.long)
        ! check repeatedly read options
        if ((counts(j) > 1 .or. self%options(id)%was_read) .and. .not.self%options(id)%repeated) &
          call error_message("cli_parser%parse: option given multiple times: " // self%options(id)%name)
        ! update read counts
        self%options(id)%was_read = .true.
        self%options(id)%read_count = self%options(id)%read_count + counts(j)
        ! check for value
        if (self%options(id)%has_value) then
          if ( is_multi ) &
            call error_message("cli_parser%parse: option expects a value: " // self%options(id)%name)
          if (i == command_argument_count()) &
            call error_message("cli_parser%parse: required value missing for: " // self%options(id)%name)
          call get_command_argument(i + 1, length=n)
          if (allocated(val)) deallocate(val)
          allocate(character(n) :: val)
          call get_command_argument(i + 1, value=val)
          self%options(id)%value = val
          i = i + 1
        end if
      end do
      i = i + 1
      deallocate(names, counts)
    end do arg_loop

    if (self%has_help) then
      if (self%option_was_read("help")) then
        call self%print_help()
        stop
      end if
    end if

    if (self%has_version) then
      if (self%option_was_read("version")) then
        call message(self%version)
        stop
      end if
    end if

    ! check for required parameters after help and version
    check_req: do j = 1, self%cnt_options()
      if ((.not. self%options(j)%was_read) .and. self%options(j)%has_default) then
        self%options(j)%value = self%options(j)%default
        self%options(j)%was_read = .true.
      end if
      if (self%options(j)%required .and. (.not. self%options(j)%was_read)) then
        if (self%options(j)%blank) then
          err_name = "<" // self%options(j)%value_name // ">"
        else
          err_name = "--" // self%options(j)%name
        end if
        call error_message("cli_parser%parse: required option missing: " // err_name)
      end if
    end do check_req

    ! set logger
    if ( self%has_logger ) then
      call log_set_config( &
        verbose = self%option_read_count("verbose"), &
        quiet = self%option_read_count("quiet"), &
        log_output_hostname = self%option_was_read("log-output-hostname"), &
        log_force_colors = self%option_was_read("log-force-colors"), &
        log_no_colors = self%option_was_read("log-no-colors"), &
        log_no_format = self%option_was_read("log-no-format"), &
        log_output_date = self%option_was_read("log-output-date"), &
        log_output_time = self%option_was_read("log-output-time") &
      )
    end if

  end subroutine parse

  !> \brief Parse given argument.
  !> \details Parse a given argument, that starts with an "-", to determine the involved options in case of a multi arg (-xyz).
  subroutine parse_arg(arg, names, counts)
    use mo_append, only: append
    implicit none
    character(*), intent(in) :: arg
    character(:), intent(out), allocatable :: names(:) !< names of involved options (can be multiple by using combined short names)
    integer(i4), intent(out), allocatable :: counts(:) !< counts of occurrences of each option

    character, allocatable :: s_names(:) ! needed for "append"
    integer(i4) :: i, j

    if ( arg(1:1) /= "-" ) call error_message("cli_parser%parse: invalid argument: " // arg)
    if ( len(arg) < 2 ) call error_message("cli_parser%parse: found empty argument: " // arg)

    ! check for long name (--name)
    if ( arg(2:2) == "-" ) then
      if ( len(arg) == 2 ) call error_message("cli_parser%parse: found empty argument: " // arg)
      allocate(character(len(arg)-2) :: names(1))
      names(1) = arg(3:len(arg))
      call append(counts, 1_i4)
      return
    end if

    call append(s_names, arg(2:2))
    call append(counts, 1_i4)
    do i=3, len(arg)
      ! check if name was already present in this multi option
      j = findchar(s_names, arg(i:i))
      if ( j == 0 ) then
        call append(s_names, arg(i:i))
        call append(counts, 1_i4)
      else
        counts(j) = counts(j) + 1_i4
      end if
    end do
    allocate(character(1) :: names(size(s_names)))
    names = s_names
  end subroutine parse_arg

  integer(i4) function findchar(array, chr)
    character, intent(in) :: array(:)
    character, intent(in) :: chr

    integer(i4) :: i

    findchar = 0_i4
    do i = 1, size(array)
      if (array(i) == chr) then
        findchar = i
        return
      end if
    end do

  end function findchar

end module mo_cli
