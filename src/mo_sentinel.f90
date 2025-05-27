!> \file    mo_sentinel.f90
!> \copydoc mo_sentinel

!> \brief   Module to handle sentinels.
!> \details This module provides standard sentinels for all used types as well as set, get and check routines.
!!          Sentinels are used to mark variables in order to check if they were accessed by an algorithm.
!!          A common use case is to check if variables were set by reading a fortran namelist.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Sep 2022
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_sentinel

  use mo_kind,                       only : i1, i2, i4, i8, sp, dp, spc, dpc
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan

  implicit none
  private

  ! sentinel values
  public :: sentinel_i1
  public :: sentinel_i2
  public :: sentinel_i4
  public :: sentinel_i8
  public :: sentinel_sp
  public :: sentinel_dp
  public :: sentinel_spc
  public :: sentinel_dpc
  public :: sentinel_char

  ! sentinel setters
  public set_sentinel
  ! sentinel getters
  public get_sentinel
  ! sentinel checker
  public check_sentinel

  !> \brief   Set variable to sentinel value.
  !> \details This routine will set sentinal values to the given variable.
  !> \param[in] "integer/real/complex/character :: value"                   value to be set to sentinel
  interface set_sentinel
    module procedure :: set_sentinel_i1
    module procedure :: set_sentinel_i2
    module procedure :: set_sentinel_i4
    module procedure :: set_sentinel_i8
    module procedure :: set_sentinel_sp
    module procedure :: set_sentinel_dp
    module procedure :: set_sentinel_spc
    module procedure :: set_sentinel_dpc
    module procedure :: set_sentinel_char
  end interface

  !> \brief   Get sentinel values of the given kind.
  !> \details This routine will return sentinal values of the same kind and shape as the given variable.
  !> \param[in] "integer/real/complex/character :: value"                   value to determine sentinel
  !> \retval    "integer/real/complex/character :: sentinel_value"          sentinal values of the same kind and shape
  interface get_sentinel
    module procedure :: get_sentinel_i1
    module procedure :: get_sentinel_i2
    module procedure :: get_sentinel_i4
    module procedure :: get_sentinel_i8
    module procedure :: get_sentinel_sp
    module procedure :: get_sentinel_dp
    module procedure :: get_sentinel_spc
    module procedure :: get_sentinel_dpc
    module procedure :: get_sentinel_char
  end interface

  !> \brief   Check given variable to be equal to the sentinel value.
  !> \details This routine will check the given variable to be equal to the specified sentinel value.
  !> \param[in] "integer/real/complex/character :: value"                   value to be checked
  !> \retval    "logical :: is_sentinel"                                    result
  interface check_sentinel
    module procedure :: check_sentinel_i1
    module procedure :: check_sentinel_i2
    module procedure :: check_sentinel_i4
    module procedure :: check_sentinel_i8
    module procedure :: check_sentinel_sp
    module procedure :: check_sentinel_dp
    module procedure :: check_sentinel_spc
    module procedure :: check_sentinel_dpc
    module procedure :: check_sentinel_char
  end interface

  ! use maxval(empty) intrinsic to get lowest possible number for each integer kind
  integer(i1), parameter :: empty_i1(0) = 0_i1
  integer(i2), parameter :: empty_i2(0) = 0_i2
  integer(i4), parameter :: empty_i4(0) = 0_i4
  integer(i8), parameter :: empty_i8(0) = 0_i8
  character, parameter :: sent_char = achar(0) ! NULL character

contains

  !> \brief   Default sentinel value (Null character) for characters.
  !> \return  Default sentinel value (Null character) for characters.
  pure character function sentinel_char()
    sentinel_char = sent_char
  end function sentinel_char

  !> \brief   Default sentinel value (-huge()-1) for i1.
  !> \return  Default sentinel value (-huge()-1) for i1.
  pure integer(i1) function sentinel_i1()
    sentinel_i1 = maxval(empty_i1)
  end function sentinel_i1

  !> \brief   Default sentinel value (-huge()-1) for i2.
  !> \return  Default sentinel value (-huge()-1) for i2.
  pure integer(i2) function sentinel_i2()
    sentinel_i2 = maxval(empty_i2)
  end function sentinel_i2

  !> \brief   Default sentinel value (-huge()-1) for i4.
  !> \return  Default sentinel value (-huge()-1) for i4.
  pure integer(i4) function sentinel_i4()
    sentinel_i4 = maxval(empty_i4)
  end function sentinel_i4

  !> \brief   Default sentinel value (-huge()-1) for i8.
  !> \return  Default sentinel value (-huge()-1) for i8.
  pure integer(i8) function sentinel_i8()
    sentinel_i8 = maxval(empty_i8)
  end function sentinel_i8

  !> \brief   Default sentinel value (NaN) for sp.
  !> \return  Default sentinel value (NaN) for sp.
  pure real(sp) function sentinel_sp()
    sentinel_sp = ieee_value(0._sp, ieee_quiet_nan)
  end function sentinel_sp

  !> \brief   Default sentinel value (NaN) for dp.
  !> \return  Default sentinel value (NaN) for dp.
  pure real(dp) function sentinel_dp()
    sentinel_dp = ieee_value(0._dp, ieee_quiet_nan)
  end function sentinel_dp

  !> \brief   Default sentinel value (NaN) for spc.
  !> \return  Default sentinel value (NaN) for spc.
  pure complex(spc) function sentinel_spc()
    real(sp) :: sentinel
    sentinel = sentinel_sp()
    sentinel_spc = cmplx(sentinel, sentinel, spc)
  end function sentinel_spc

  !> \brief   Default sentinel value (NaN) for dpc.
  !> \return  Default sentinel value (NaN) for dpc.
  pure complex(dpc) function sentinel_dpc()
    real(dp) :: sentinel
    sentinel = sentinel_dp()
    sentinel_dpc = cmplx(sentinel, sentinel, dpc)
  end function sentinel_dpc


  !> \brief   Set sentinel value for i1.
  elemental subroutine set_sentinel_i1(value)
    integer(i1), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_i1()
  end subroutine set_sentinel_i1

  !> \brief   Set sentinel value for i2.
  elemental subroutine set_sentinel_i2(value)
    integer(i2), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_i2()
  end subroutine set_sentinel_i2

  !> \brief   Set sentinel value for i4.
  elemental subroutine set_sentinel_i4(value)
    integer(i4), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_i4()
  end subroutine set_sentinel_i4

  !> \brief   Set sentinel value for i8.
  elemental subroutine set_sentinel_i8(value)
    integer(i8), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_i8()
  end subroutine set_sentinel_i8

  !> \brief   Set sentinel value for sp.
  elemental subroutine set_sentinel_sp(value)
    real(sp), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_sp()
  end subroutine set_sentinel_sp

  !> \brief   Set sentinel value for dp.
  elemental subroutine set_sentinel_dp(value)
    real(dp), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_dp()
  end subroutine set_sentinel_dp

  !> \brief   Set sentinel value for spc.
  elemental subroutine set_sentinel_spc(value)
    complex(spc), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_spc()
  end subroutine set_sentinel_spc

  !> \brief   Set sentinel value for dpc.
  elemental subroutine set_sentinel_dpc(value)
    complex(dpc), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_dpc()
  end subroutine set_sentinel_dpc

  !> \brief   Set sentinel value for char.
  elemental subroutine set_sentinel_char(value)
    character(len=*), intent(inout) :: value !< value to be set to sentinel
    value = sentinel_char()
  end subroutine set_sentinel_char


  !> \brief   Get sentinel value for char.
  elemental character function get_sentinel_char(value)
    character(*), intent(in) :: value !< value to determine sentinel
    get_sentinel_char = sentinel_char()
  end function get_sentinel_char

  !> \brief   Get sentinel value for i1.
  elemental integer(i1) function get_sentinel_i1(value)
    integer(i1), intent(in) :: value !< value to determine sentinel
    get_sentinel_i1 = sentinel_i1()
  end function get_sentinel_i1

  !> \brief   Get sentinel value for i2.
  elemental integer(i2) function get_sentinel_i2(value)
    integer(i2), intent(in) :: value !< value to determine sentinel
    get_sentinel_i2 = sentinel_i2()
  end function get_sentinel_i2

  !> \brief   Get sentinel value for i4.
  elemental integer(i4) function get_sentinel_i4(value)
    integer(i4), intent(in) :: value !< value to determine sentinel
    get_sentinel_i4 = sentinel_i4()
  end function get_sentinel_i4

  !> \brief   Get sentinel value for i8.
  elemental integer(i8) function get_sentinel_i8(value)
    integer(i8), intent(in) :: value !< value to determine sentinel
    get_sentinel_i8 = sentinel_i8()
  end function get_sentinel_i8

  !> \brief   Get sentinel value for sp.
  elemental real(sp) function get_sentinel_sp(value)
    real(sp), intent(in) :: value !< value to determine sentinel
    get_sentinel_sp = sentinel_sp()
  end function get_sentinel_sp

  !> \brief   Get sentinel value for dp.
  elemental real(dp) function get_sentinel_dp(value)
    real(dp), intent(in) :: value !< value to determine sentinel
    get_sentinel_dp = sentinel_dp()
  end function get_sentinel_dp

  !> \brief   Get sentinel value for spc.
  elemental complex(spc) function get_sentinel_spc(value)
    complex(spc), intent(in) :: value !< value to determine sentinel
    get_sentinel_spc = sentinel_spc()
  end function get_sentinel_spc

  !> \brief   Get sentinel value for dpc.
  elemental complex(dpc) function get_sentinel_dpc(value)
    complex(dpc), intent(in) :: value !< value to determine sentinel
    get_sentinel_dpc = sentinel_dpc()
  end function get_sentinel_dpc


  !> \brief   Check for sentinel value for string.
  elemental logical function check_sentinel_char(value)
    character(*), intent(in) :: value !< value to determine sentinel
    ! if trimmed string has not exactly one char, return false
    if ( len_trim(value) /= 1 ) then
      check_sentinel_char = .false.
    else
      check_sentinel_char = value(1:1) == sent_char(1:1)
    end if
  end function check_sentinel_char

  !> \brief   Check for sentinel value for i1.
  elemental logical function check_sentinel_i1(value)
    integer(i1), intent(in) :: value !< value to determine sentinel
    integer(i1) :: sentinel
    sentinel = sentinel_i1()
    check_sentinel_i1 = sentinel == value
  end function check_sentinel_i1

  !> \brief   Check for sentinel value for i2.
  elemental logical function check_sentinel_i2(value)
    integer(i2), intent(in) :: value !< value to determine sentinel
    integer(i2) :: sentinel
    sentinel = sentinel_i2()
    check_sentinel_i2 = sentinel == value
  end function check_sentinel_i2

  !> \brief   Check for sentinel value for i4.
  elemental logical function check_sentinel_i4(value)
    integer(i4), intent(in) :: value !< value to determine sentinel
    integer(i4) :: sentinel
    sentinel = sentinel_i4()
    check_sentinel_i4 = sentinel == value
  end function check_sentinel_i4

  !> \brief   Check for sentinel value for i8.
  elemental logical function check_sentinel_i8(value)
    integer(i8), intent(in) :: value !< value to determine sentinel
    integer(i8) :: sentinel
    sentinel = sentinel_i8()
    check_sentinel_i8 = sentinel == value
  end function check_sentinel_i8

  !> \brief   Check for sentinel value for sp.
  elemental logical function check_sentinel_sp(value)
    real(sp), intent(in) :: value !< value to determine sentinel
    check_sentinel_sp = ieee_is_nan(value)
  end function check_sentinel_sp

  !> \brief   Check for sentinel value for dp.
  elemental logical function check_sentinel_dp(value)
    real(dp), intent(in) :: value !< value to determine sentinel
    check_sentinel_dp = ieee_is_nan(value)
  end function check_sentinel_dp

  !> \brief   Check for sentinel value for spc.
  elemental logical function check_sentinel_spc(value)
    complex(spc), intent(in) :: value !< value to determine sentinel
    check_sentinel_spc = ieee_is_nan(real(value)) .or. ieee_is_nan(aimag(value))
  end function check_sentinel_spc

  !> \brief   Check for sentinel value for dpc.
  elemental logical function check_sentinel_dpc(value)
    complex(dpc), intent(in) :: value !< value to determine sentinel
    check_sentinel_dpc = ieee_is_nan(real(value)) .or. ieee_is_nan(aimag(value))
  end function check_sentinel_dpc

end module mo_sentinel
