!> \file mo_forces_message.f90
!> \copydoc mo_forces_message

!> \brief FORCES diagnostics with a library-owned default scope.
!> \details Import the needed routines once at module level. Omitted scopes use 'forces',
!! independently of mo_message::def_log_scope. Explicit scopes override the library default.
!! All other options and saved settings are provided by mo_message; these wrappers own no mutable state.
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
!> \ingroup f_core
module mo_forces_message
  use mo_message, only: base_message => message, base_warn_message => warn_message, base_error_message => error_message
  implicit none
  private
  public :: message, warn_message, error_message

contains

  !> \brief Forward a message diagnostic with default scope 'forces'.
  subroutine message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, scope)
    character(len=*), optional, intent(in) :: t01 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t02 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t03 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t04 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t05 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t06 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t07 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t08 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t09 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t10 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t11 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t12 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t13 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t14 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t15 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t16 !< Optional string to concatenate
    integer, optional, intent(in) :: uni !< Plain output unit override
    character(len=*), optional, intent(in) :: advance !< Plain advancement override
    logical, optional, intent(in) :: show !< Visibility override
    character(len=*), optional, intent(in) :: scope !< Scope override; absent means 'forces'

    if (present(scope)) then
      call base_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, scope)
    else
      call base_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, scope='forces')
    end if
  end subroutine message

  !> \brief Forward a warn diagnostic with default scope 'forces'.
  subroutine warn_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, raise, scope)
    character(len=*), optional, intent(in) :: t01 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t02 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t03 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t04 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t05 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t06 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t07 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t08 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t09 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t10 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t11 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t12 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t13 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t14 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t15 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t16 !< Optional string to concatenate
    integer, optional, intent(in) :: uni !< Plain output unit override
    character(len=*), optional, intent(in) :: advance !< Plain advancement override
    logical, optional, intent(in) :: show !< Visibility override
    logical, optional, intent(in) :: raise !< Termination override; uses the base routine default if absent
    character(len=*), optional, intent(in) :: scope !< Scope override; absent means 'forces'

    if (present(scope)) then
      call base_warn_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, raise, scope)
    else
      call base_warn_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, raise, scope='forces')
    end if
  end subroutine warn_message

  !> \brief Forward a error diagnostic with default scope 'forces'.
  subroutine error_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
    uni, advance, show, raise, scope)
    character(len=*), optional, intent(in) :: t01 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t02 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t03 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t04 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t05 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t06 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t07 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t08 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t09 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t10 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t11 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t12 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t13 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t14 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t15 !< Optional string to concatenate
    character(len=*), optional, intent(in) :: t16 !< Optional string to concatenate
    integer, optional, intent(in) :: uni !< Plain output unit override
    character(len=*), optional, intent(in) :: advance !< Plain advancement override
    logical, optional, intent(in) :: show !< Visibility override
    logical, optional, intent(in) :: raise !< Termination override; uses the base routine default if absent
    character(len=*), optional, intent(in) :: scope !< Scope override; absent means 'forces'

    if (present(scope)) then
      call base_error_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, raise, scope)
    else
      call base_error_message(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15, t16, &
        uni, advance, show, raise, scope='forces')
    end if
  end subroutine error_message

end module mo_forces_message
