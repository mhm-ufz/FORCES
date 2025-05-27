!> \file mo_string_utils.f90
!> \copydoc mo_string_utils

!> \brief String utilities
!> \details This module provides string conversion and checking utilities.
!> \authors Matthias Cuntz, Matthias Zink, Giovanni Dalmasso, David Schaefer
!> \date Dec 2011
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_string_utils

  USE mo_kind, ONLY: i4, i8, sp, dp

  IMPLICIT NONE

  PUBLIC :: compress      ! Conversion   : 'A b C x Y z' -> 'AbCxYz'
  PUBLIC :: divide_string ! split string in substring with the help of delimiter
  PUBLIC :: equalStrings  ! compares two strings
  PUBLIC :: nonull        ! Check if string is still NULL
  PUBLIC :: num2str       ! Convert a number to a string
  PUBLIC :: separator     ! Format string: '-----...-----'
  PUBLIC :: splitString   ! splits string at given delimiter
  PUBLIC :: startswith    ! checks if string starts with a certain prefix
  PUBLIC :: endswith      ! checks if string ends with a certain suffix
  PUBLIC :: str2num       ! Converts string into an array of its numerical representation
  PUBLIC :: tolower       ! Conversion   : 'ABCXYZ' -> 'abcxyz'
  PUBLIC :: toupper       ! Conversion   : 'abcxyz' -> 'ABCXYZ'
  PUBLIC :: Replace_Text  ! replaces all text occurences in string
  PUBLIC :: replace_word  ! replaces all word occurences in string
  PUBLIC :: index_word    ! yields starting position of word in string or 0
  PUBLIC :: is_blank

  ! public :: numarray2str

  ! ------------------------------------------------------------------

  !>    \brief Convert to string.

  !>    \details Convert a number or logical to a string with an optional format.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    str = num2str(3.1415217_i4,'(F3.1)')
  !!    \endcode
  !!    See also example in test directory.

  !>    \param[in] "integer(i4/i8)/real(sp/dp)/logical :: num"  Number or logical
  !>    \param[in] "character(len=*), optional :: form"         Format string\n
  !!                                                            Defaults are:\n
  !!                                                            i4    - '(I10)'\n
  !!                                                            i8    - '(I20)'\n
  !!                                                            sp/dp - '(G32.5)'\n
  !!                                                            log   - '(L10)'
  !>    \retval "character(len=X) :: str"                       String of formatted input number or logical\n
  !!                                                            Output length X is:\n
  !!                                                            i4    - 10\n
  !!                                                            i8    - 20\n
  !!                                                            sp/dp - 32\n
  !!                                                            log   - 10

  !>    \note
  !!    Uses WRITE to write into string. Recursive write is not permitted before Fortran 2003
  !!    so that one cannot use\n
  !!    \code{.f90}
  !!    write(*,*) 'A='//num2str(a)
  !!    \endcode
  !!    Use 'call message' from mo_messages.f90
  !!    \code{.f90}
  !!    use mo_messages, only message
  !!    call message('A=', trim(num2str(a)))
  !!    \endcode
  !!    or write into another string first:
  !!    \code{.f90}
  !!    str = 'A='//num2str(a)
  !!    write(*,*) trim(str)
  !!    \endcode

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany
  INTERFACE num2str
     MODULE PROCEDURE i42str, i82str, sp2str, dp2str, log2str
  END INTERFACE num2str


  ! ------------------------------------------------------------------

  !>    \brief Convert to string.

  !>    \details Convert a array of numbers or logicals to a string.
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    str = numarray2str(num)
  !!    \endcode

  !>    \param[in] "integer(i4/i8)/real(sp/dp)/logical :: num(:)"    Array of numbers or logicals
  !>    \retval "character(len=X) :: str"                            String of formatted input number or logical\n

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany
  INTERFACE numarray2str
     MODULE PROCEDURE i4array2str
  END INTERFACE numarray2str

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------
  !> separator string (line)
  CHARACTER(len=*), PARAMETER :: separator = repeat('-',70)

  ! ------------------------------------------------------------------

CONTAINS

  !> \brief   Check for blank characters.
  !> \details Checks whether or not `c` is a blank character, namely a space and tab character.
  !> \return  Truth value if `c` is a blank.
  pure logical function is_blank(c)

    character(len=1), intent(in) :: c !< The character to test.
    integer :: ic

    ic = iachar(c)             ! TAB
    is_blank = (c == ' ') .or. (ic == int(z'09'));

  end function is_blank

  ! ------------------------------------------------------------------

  !>    \brief Remove white spaces

  !>    \details Return a copy of an input string with all whitespace (spaces and tabs) removed
  !!
  !!    \b Example
  !!
  !!    Returns 'Hallo'
  !!    \code{.f90}
  !!    noSpaces = whiteSpaces = compress('H a l l o')
  !!    \endcode

  !>     \param[in] "character(len=*) :: whiteSpaces"             String
  !>     \param[out] "integer(i4), optional :: n"                 Integer
  !>     \retval "character(len = len(whiteSpaces)) :: compress"  String where all all whitespace (spaces and tabs) are removed

  !>     \author Giovanni Dalmasso
  !>     \date Jan 2013
  !!        - modified from Paul van Delst, CIMSS/SSEC 18-Oct-1999

  function compress( whiteSpaces, n )

        use mo_kind,    only : i4

        implicit none

        character(len=*),               intent(in)  :: whiteSpaces
        integer(i4),        optional,   intent(out) :: n

        character(len(whiteSpaces))                 ::  compress

        ! Local parameters
        integer(i4),    parameter                   :: iachar_space = 32_i4
        integer(i4),    parameter                   :: iachar_tab   = 9_i4

        ! Local variables
        integer(i4)                                 :: i, j
        integer(i4)                                 :: iachar_character

        ! Setup

        ! Initialise compress
        compress = ' '
        ! Initialise counter
        j = 0_i4

        ! Loop over string
        do i = 1, len(whiteSpaces)
            ! Convert the current character to its position
            iachar_character = iachar(whiteSpaces(i:i))

            ! If the character is NOT a space ' ' or a tab '->|' copy it to the output string.
            if ( iachar_character /= iachar_space .and. iachar_character /= iachar_tab )    then
                j = j + 1
                compress(j:j) = whiteSpaces(i:i)
            end if
        end do

        ! Save the non-whitespace count
        if ( present(n) ) n = j

    end function compress

    ! replaces text
    ! e.g. replace_text('our hour', 'our', 'their') --> 'their htheir'
    function replace_text (s,text,rep)  result(outs)
      character(*)        :: s,text,rep
      character(len(s)+100) :: outs     ! provide outs with extra 100 char len
      integer             :: i, nt, nr

      outs = s ; nt = len_trim(text) ; nr = len_trim(rep)
      if (text == rep) return
      do
         i = index(outs,text(:nt)) ; if (i == 0) exit
         outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
      end do
    end function replace_text

    !> \brief replaces words in a string
    !> \details replaces proper words only, e.g. replace_word('our hour', 'our', 'their') --> 'their hour'
    !> \author Robert Schweppe
    !> \date Nov 2018
    function replace_word(s, word, rep, check_negative_number_arg)  result(outs)
      character(*) :: s, word, rep
      logical, optional  :: check_negative_number_arg
      character(len(s)+100) :: outs     ! provide outs with extra 100 char len
      integer               :: i, nt, nr

      outs = s ; nt = len_trim(word) ; nr = len_trim(rep)
      if (word == rep) return
      do
        i = index_word(outs, word(:nt), check_negative_number_arg)
        if (i == 0) exit
        outs = outs(:i-1) // rep(:nr) // outs(i+nt:)
      end do
    end function replace_word

    !> \brief find index in word
    !> \author Robert Schweppe
    !> \date Nov 2018
    function index_word(s, text, check_negative_number_arg) result(out_index)
    CHARACTER(*)       :: s
    CHARACTER(*)       :: text
    logical, optional  :: check_negative_number_arg
    integer :: out_index

    integer :: i, nw, ns, i_add
    logical :: is_begin_not_word, check_negative_number_default
    character(63), parameter :: word_chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'
    character(10), parameter :: digit_chars = '0123456789'

    check_negative_number_default = .false.
    if (present(check_negative_number_arg)) then
      check_negative_number_default = check_negative_number_arg
    end if

    nw = LEN_TRIM(text) ; ns = LEN_TRIM(s)
    out_index = 0
    i = 1
    scan_loop: DO
      ! find index of the first character of word in string that has not been scanned so far
      i_add = scan(s(i:ns), text(1:1))
      i = i + i_add - 1
      if (i_add == 0 .or. i+nw-1 > ns) then
        ! the word cannot be in string as the first char is not even contained or
        ! the word cannot be in string starting at i as it would be too long
        exit
      else if (s(i:i+nw-1) == trim(text)) then
        ! charachter matches the word
        is_begin_not_word = .true.
        if (i-1 > 0) then
          ! is the word preceded by a alphanumeric character?
          if (scan(s(i-1:i-1), word_chars) == 1) then
            is_begin_not_word = .false.
          else if (check_negative_number_default .and. &
                   scan(s(i-1:i-1), '-') == 1 .and. &
                   scan(digit_chars, text(1:1)) > 0 ) then
            is_begin_not_word = .false.
          end if
        end if
        if (is_begin_not_word) then
          ! is the word succeeded by a alphanumeric character?
          if (scan(s(i+nw:i+nw), word_chars) == 1) then
            ! word boundary end is violated, continue
            i = i + 1
          else
            ! index is found and word boundaries are checked
            out_index = i
            exit scan_loop
          end if
        else
          ! word boundary start is violated, continue
          i = i + 1
        end if
      else
        ! word does not match, continue
        i = i + 1
      end if
    END DO scan_loop

  end function index_word

  ! ------------------------------------------------------------------

  !>    \brief Divide string in substrings.

  !>    \details Divides a string in several substrings (array of strings) with the help of a user
  !!    specified delimiter.
  !!
  !!    \b Example
  !!
  !!    Divide string into 'I', 'want', 'to', ...
  !!    \code{.f90}
  !!    divide_string('I want to test this routine!', ' ', strArr(:))
  !!    \endcode

  !>    \param[in] "CHARACTER(len=*), INTENT(IN) :: string"     - string to be divided
  !>    \param[in] "CHARACTER(len=*), INTENT(IN) :: delim"      - delimiter specifying places for division
  !>    \param[out] "CHARACTER(len=*), DIMENSION(:), ALLOCATABLE,  INTENT(OUT) :: strArr"
  !!                 Array of substrings, has to be allocateable and is handed to the routine unallocated

  !>    \note
  !!    only character types allowed.\n
  !!    output array should be allocateable array, which is unallocated handed to the subroutine.
  !!    allocation is done in in devide_string.

  !>    \author Matthias Zink
  !>    \date Oct 2012

  SUBROUTINE divide_string(string, delim, strArr)

    IMPLICIT NONE

    CHARACTER(len=*)             , INTENT(IN)        :: string
    CHARACTER(len=*)             , INTENT(IN)        :: delim
    CHARACTER(len=*), DIMENSION(:), ALLOCATABLE, INTENT(OUT) :: strArr

    CHARACTER(256)                                   :: stringDummy   ! string in fisrt place but cutted in pieces
    CHARACTER(256), DIMENSION(:) , ALLOCATABLE       :: strDummyArr   ! Dummy arr until number of substrings is known
    INTEGER(i4)                                      :: pos           ! position of dilimiter
    INTEGER(i4)                                      :: nosubstr      ! number of substrings in string

    stringDummy = string

    allocate(strDummyArr(len_trim(stringDummy)))
    pos=999_i4
    nosubstr=0_i4
    ! search for substrings and theirs count
    do
       pos = index(trim(adjustl(stringDummy)), delim)
       ! exit if no more delimiter is find and save the last part of the string
       if (pos == 0_i4) then
          nosubstr = nosubstr + 1_i4
          StrDummyArr(nosubstr) = trim(stringDummy)
          exit
       end if

       nosubstr = nosubstr + 1_i4
       strDummyArr(nosubstr) = stringDummy(1:pos-1)
       stringDummy = stringDummy(pos+1:len_trim(stringDummy))
    end do
    ! hand over results to strArr
    if (nosubstr == 0_i4) then
       print*, '***WARNING: string does not contain delimiter. There are no substrings. (subroutine DIVIDE_STRING)'
       return
    else
       allocate(strArr(nosubstr))
       strArr = StrDummyArr(1:nosubstr)
    end if

    deallocate(strDummyArr)

  END SUBROUTINE divide_string

    ! ------------------------------------------------------------------

  !>    \brief Checks if two string are equal

  !>    \details Returns true if the given string arguments are equal
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    isequal = equalString(string1,string2)
  !!    \endcode

  !>    \param[in] "character(len=*) :: string1"    String
  !>    \param[in] "character(len=*) :: string2"    String
  !>    \retval "logical :: eq" Logical value if string equal

  !>    \author David Schaefer
  !>    \date Mar 2015

  function equalStrings(string1,string2)
    implicit none

    character(len=*), intent(in)     :: string1, string2
    integer(i4),      allocatable    :: array1(:), array2(:)
    integer(i4)                      :: i
    logical                          :: equalStrings

    array1 = str2num(trim(string1))
    array2 = str2num(trim(string2))
    equalStrings = .false.

    if (size(array1) == size(array2)) then
       equalStrings = .true.
       do i=1, size(array1)
          if (array1(i) /= array2(i)) then
             equalStrings = .false.
             exit
          end if
       end do
    end if

  end function equalStrings

  ! ------------------------------------------------------------------

  !>    \brief Checks if string was already used

  !>    \details Checks if string was already used, i.e. does not contain NULL character anymore.
  !!
  !!    \b Example
  !!
  !!    Trim if string is used.
  !!    \code{.f90}
  !!    if (nonull(str)) write(*,*) trim(str)
  !!    \endcode

  !>    \param[in] "character(len=*) :: str"    String
  !>    \retval "logical :: used"               .true.: string was already set; .false.: string still in initialised state

  !>    \author Matthias Cuntz
  !>    \date Jan 2012

  FUNCTION nonull(str)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(in) :: str
    LOGICAL                      :: nonull

    if (scan(str, achar(0)) == 0) then
       nonull = .true.
    else
       nonull = .false.
    end if

  END FUNCTION nonull

  ! ------------------------------------------------------------------

  !>    \brief split string at delimiter

  !>    \details Split string at delimiter an return an array of strings
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    string_parts = splitString(string,delim)
  !!    \endcode

  !>    \param[in] "character(len=*) :: string"    String
  !>    \param[in] "character(len=*) :: delim"     String
  !>    \retval "character(len=245) :: out(:)"     Array of splitted strings

  !>    \author David Schaefer
  !>    \date Mar 2015

  function splitString(string,delim) result(out)

    use mo_append, only : append
    implicit none

    character(len=*),   intent(in)        :: string
    character(len=*),   intent(in)        :: delim
    character(len=256), allocatable       :: out(:)
    integer(i4),        allocatable       :: string_array(:), delim_array(:)
    integer(i4)                           :: i, start
    !
    if (allocated(out)) deallocate(out)
    string_array = str2num(string//delim)
    delim_array = str2num(delim)
    start = 1

    do i=1, size(string_array) - size(delim_array) + 1
       if (all(string_array(i:i+size(delim_array)-1) == delim_array)) then
          call append(out, numarray2str(string_array(start:i-1)))
          start = i + size(delim_array)
       end if
    end do
    !
  end function splitString

  ! ------------------------------------------------------------------

  !>    \brief Checks if string starts with character(s)
  !>    \details Returns true if string starts with given characters, flase otherwise
  !>    \author David Schaefer
  !>    \date Mar 2015
  logical function startswith(string, start, strip)

    implicit none

    character(len=*), intent(in)     :: string !< string to check
    character(len=*), intent(in)     :: start  !< starting string
    logical, optional, intent(in)    :: strip  !< whether to strip trailing white-spaces (.false. by default)

    integer(i4) :: i
    logical :: strip_

    strip_ = .false.
    if ( present(strip) ) strip_ = strip

    if (strip_) then
      i = index(trim(string), trim(start))
    else
      i = index(string, start)
    end if

    startswith = i == 1

  end function startswith

  ! ------------------------------------------------------------------

  !>    \brief Checks if (maybe trimmed) string ends with given character(s)
  !>    \retval "endswith" if string ends with given end
  !>    \author Sebastian Müller
  !>    \date Mar 2023
  logical function endswith(string, suffix, strip)

    implicit none

    character(len=*), intent(in)     :: string !< string to check
    character(len=*), intent(in)     :: suffix !< ending string
    logical, optional, intent(in)    :: strip  !< whether to strip trailing white-spaces (.true. by default)

    integer(i4) :: i, ref
    logical :: strip_

    strip_ = .true.
    if ( present(strip) ) strip_ = strip

    if (strip_) then
      i = index(trim(string), trim(suffix), back=.true.)
      ref = len_trim(string) - len_trim(suffix) + 1_i4
    else
      i = index(string, suffix, back=.true.)
      ref = len(string) - len(suffix) + 1_i4
    end if

    endswith = i == ref

  end function endswith

  ! ------------------------------------------------------------------

  !>    \brief Convert to lower case

  !>    \details Convert all upper case letters in string to lower case letters.
  !!
  !!    \b Example
  !!
  !!    Returns 'hallo'
  !!    \code{.f90}
  !!    low = tolower('Hallo')
  !!    \endcode

  !>    \param[in] "character(len=*) :: upper"                String
  !>    \retval    "character(len=len_trim(upper)) :: low"    String where all uppercase in input is converted to lowercase

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany

  FUNCTION tolower(upper)

    IMPLICIT NONE

    CHARACTER(LEN=*)              ,INTENT(in) :: upper
    CHARACTER(LEN=LEN_TRIM(upper))            :: tolower

    INTEGER            :: i
    INTEGER ,PARAMETER :: idel = ICHAR('a')-ICHAR('A')

    DO i=1,LEN_TRIM(upper)
       IF (ICHAR(upper(i:i)) >= ICHAR('A') .AND. &
            ICHAR(upper(i:i)) <= ICHAR('Z')) THEN
          tolower(i:i) = CHAR( ICHAR(upper(i:i)) + idel )
       ELSE
          tolower(i:i) = upper(i:i)
       END IF
    END DO

  END FUNCTION tolower

  ! ------------------------------------------------------------------

  !>    \brief Convert to upper case

  !>    \details Convert all lower case letters in string to upper case letters.
  !!
  !!    \b Example
  !!
  !!    Returns 'HALLO'
  !!    \code{.f90}
  !!    up = toupper('Hallo')
  !!    \endcode

  !>    \param[in] "character(len=*) :: lower"            String
  !>    \retval "character(len=len_trim(lower)) :: up"    String where all lowercase in input is converted to uppercase

  !>    \author Matthias Cuntz
  !>    \date Dec 2011
  !!        - modified from Echam5, (C) MPI-MET, Hamburg, Germany

  FUNCTION toupper (lower)

    IMPLICIT NONE

    CHARACTER(LEN=*)              ,INTENT(in) :: lower
    CHARACTER(LEN=LEN_TRIM(lower))            :: toupper

    INTEGER            :: i
    INTEGER, PARAMETER :: idel = ICHAR('A')-ICHAR('a')

    DO i=1,LEN_TRIM(lower)
       IF (ICHAR(lower(i:i)) >= ICHAR('a') .AND. &
            ICHAR(lower(i:i)) <= ICHAR('z')) THEN
          toupper(i:i) = CHAR( ICHAR(lower(i:i)) + idel )
       ELSE
          toupper(i:i) = lower(i:i)
       END IF
    END DO

  END FUNCTION toupper


  ! -----------------------------------------------------------
  ! PRIVATE ROUTINES
  ! (no "template" documentation required)
  ! -----------------------------------------------------------

  PURE FUNCTION i42str(nn,form)
    ! returns integer nn as a string (often needed in printing messages)
    IMPLICIT NONE
    INTEGER(i4),      INTENT(IN)           :: nn
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
    CHARACTER(len=10) :: i42str

    if (present(form)) then
       write(i42str,form) nn
    else
       write(i42str,'(I10)') nn
    end if
    !i42str = adjustl(i42str)

  END FUNCTION i42str


  PURE FUNCTION i82str(nn,form)
    ! returns integer nn as a string (often needed in printing messages)
    IMPLICIT NONE
    INTEGER(i8),      INTENT(IN)           :: nn
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
    CHARACTER(len=20) :: i82str

    if (present(form)) then
       write(i82str,form) nn
    else
       write(i82str,'(I20)') nn
    end if
    !i82str = adjustl(i82str)

  END FUNCTION i82str


  PURE FUNCTION sp2str(rr,form)
    ! returns real rr as a string (often needed in printing messages)
    IMPLICIT NONE
    REAL(sp),         INTENT(IN)           :: rr
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
    CHARACTER(len=32) :: sp2str

    if (present(form)) then
       write(sp2str,form) rr
    else
       write(sp2str,'(G32.5)') rr
    end if
    !sp2str = adjustl(sp2str)

  END FUNCTION sp2str


  PURE FUNCTION dp2str(rr,form)
    ! returns real rr as a string (often needed in printing messages)
    IMPLICIT NONE
    REAL(dp),         INTENT(IN)           :: rr
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
    CHARACTER(len=32) :: dp2str

    if (present(form)) then
       write(dp2str,form) rr
    else
       write(dp2str,'(G32.5)') rr
    end if
    !dp2str = adjustl(dp2str)

  END FUNCTION dp2str


  PURE FUNCTION log2str(ll,form)
    ! returns logical ll as a string (often needed in printing messages)
    IMPLICIT NONE
    LOGICAL,          INTENT(in)           :: ll
    CHARACTER(len=*), INTENT(IN), OPTIONAL :: form
    CHARACTER(len=10) :: log2str

    if (present(form)) then
       write(log2str,form) ll
    else
       write(log2str,'(L10)') ll
    end if
    !log2str = adjustl(log2str)

  END FUNCTION log2str

  function i4array2str(arr) result(out)

    integer(i4), intent(in)     :: arr(:)
    integer(i4)                 :: ii
    character(len=size(arr))    :: out

    out = " "
    do ii=1,size(arr)
       out(ii:ii) = char(arr(ii))
    end do

  end function i4array2str

  ! ------------------------------------------------------------------

  !>    \brief Converts string into an array of its numerical representation

  !>    \details Converts string into an integer array of the numerical values of the letters
  !!
  !!    \b Example
  !!
  !!    Convert is string into numerical array of the letters
  !!    \code{.f90}
  !!    num = str2num(string)
  !!    \endcode

  !>    \param[in] "character(len=*) :: string"    String
  !>    \retval "integer  :: out(:)"               Numerical array of letters

  !>    \author David Schaefer
  !>    \date Mar 2015

  function str2num(string) result(out)

    implicit none

    character(len=*), intent(in)       :: string
    integer(i4), allocatable           :: out(:)
    integer(i4)                        :: i

    if (allocated(out)) deallocate(out)
    allocate(out(len(string)))

    do i=1,len(string)
       out(i) = ichar(string(i:i))
    end do

  end function str2num


END MODULE mo_string_utils
