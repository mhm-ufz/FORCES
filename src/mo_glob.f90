!> \file    mo_glob.f90
!> \copydoc mo_glob

!> \brief   Glob pattern matching utilities.
!> \authors Sebastian Mueller
!> \date    Nov 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_glob
  implicit none
  private
  public :: glob_match

  character(len=1), public, parameter :: wildcard_any = "*" !< Wildcard character for any sequence of characters
  character(len=1), public, parameter :: wildcard_one = "?" !< Wildcard character for any single character

contains

  !> \brief Match a text against a glob pattern supporting * and ?.
  logical function glob_match(pattern, text)
    character(len=*), intent(in) :: pattern
    character(len=*), intent(in) :: text
    integer :: p, t, star, match
    integer :: plen, tlen

    plen = len_trim(pattern)
    tlen = len_trim(text)

    ! handle special cases
    if (plen == 0) then
      glob_match = (tlen == 0)
      return
    elseif (trim(pattern) == "*") then
      glob_match = .true.
      return
    elseif (trim(pattern) == "?") then
      glob_match = (tlen == 1)
      return
    end if

    if (tlen == 0) then
      glob_match = .true.
      do p = 1, plen
        if (pattern(p:p) /= wildcard_any) then
          glob_match = .false.
          return
        end if
      end do
      return
    end if

    p = 1
    t = 1
    star = 0
    match = 0

    do while (t <= tlen .or. p <= plen)
      if (p <= plen .and. pattern(min(p,plen):min(p,plen)) == wildcard_one) then
        if (t > tlen) exit
        p = p + 1
        t = t + 1
      elseif (p <= plen .and. pattern(min(p,plen):min(p,plen)) == wildcard_any) then
        star = p
        match = t
        p = p + 1
      elseif (p <= plen .and. t <= tlen .and. pattern(min(p,plen):min(p,plen)) == text(min(t,tlen):min(t,tlen))) then
        p = p + 1
        t = t + 1
      elseif (star /= 0) then
        p = star + 1
        match = match + 1
        t = match
      else
        glob_match = .false.
        return
      end if
    end do

    do while (p <= plen .and. pattern(min(p,plen):min(p,plen)) == wildcard_any)
      p = p + 1
    end do

    glob_match = (p > plen .and. t > tlen)
  end function glob_match

end module mo_glob
