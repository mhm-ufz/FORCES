!> \file    mo_io.f90
!> \copydoc mo_io

!> \brief   File reading routines.
!> \details This module provides routines to load a file into an array.
!!          This is mainly taken from the Fortran stdlib: https://github.com/fortran-lang/stdlib
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Apr 2022
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_io

  use mo_kind, only: i4, dp, sp
  use mo_message, only: error_message
  use mo_string_utils, only: is_blank

  implicit none
  private
  public :: loadtxt
  public :: number_of_columns
  public :: number_of_rows

  !> \brief   Read a file into a 2D array containing reals.
  interface loadtxt
    module procedure loadtxt_dp, loadtxt_sp
  end interface loadtxt

contains

  !> \brief   Read a file into a 2D array containing reals.
  subroutine loadtxt_dp(filename, d, skiprows, max_rows)

    !> Filename to load the array from
    character(len=*), intent(in) :: filename
    !> The array 'd' will be automatically allocated with the correct dimensions
    real(dp), allocatable, intent(out) :: d(:,:)
    !> lines to skip at the begining
    integer(i4), intent(in), optional :: skiprows
    !> Read max_rows lines of content after skiprows lines. The default is to read all the lines (negative values).
    integer(i4), intent(in), optional :: max_rows

    integer(i4) :: u,  nrow, ncol, i, skiprows_, max_rows_

    skiprows_ = 0
    if ( present(skiprows) ) skiprows_ = max(skiprows, 0)

    max_rows_ = -1
    if ( present(max_rows) ) max_rows_ = max_rows

    open(newunit=u, file=filename, action='read', position='asis', status='old', access='stream', form='formatted')

    ! determine size
    ncol = number_of_columns(u)
    nrow = number_of_rows(u)
    skiprows_ = min(skiprows_, nrow)
    if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_)) max_rows_ = nrow - skiprows_

    allocate(d(max_rows_, ncol))

    do i = 1, skiprows_
      read(u, *)
    end do

    do i = 1, max_rows_
      read(u, *) d(i, :)
    end do

    close(u)

  end subroutine loadtxt_dp

  !> \brief   Read a file into a 2D array containing reals.
  subroutine loadtxt_sp(filename, d, skiprows, max_rows)

    !> Filename to load the array from
    character(len=*), intent(in) :: filename
    !> The array 'd' will be automatically allocated with the correct dimensions
    real(sp), allocatable, intent(out) :: d(:,:)
    !> lines to skip at the begining
    integer(i4), intent(in), optional :: skiprows
    !> Read max_rows lines of content after skiprows lines. The default is to read all the lines (negative values).
    integer(i4), intent(in), optional :: max_rows

    integer(i4) :: u, nrow, ncol, i, skiprows_, max_rows_

    skiprows_ = 0
    if ( present(skiprows) ) skiprows_ = max(skiprows, 0)

    max_rows_ = -1
    if ( present(max_rows) ) max_rows_ = max_rows

    open(newunit=u, file=filename, action='read', position='asis', status='old', access='stream', form='formatted')

    ! determine size
    ncol = number_of_columns(u)
    nrow = number_of_rows(u)
    skiprows_ = min(skiprows_, nrow)
    if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_)) max_rows_ = nrow - skiprows_

    allocate(d(max_rows_, ncol))

    do i = 1, skiprows_
      read(u, *)
    end do

    do i = 1, max_rows_
      read(u, *) d(i, :)
    end do

    close(u)

  end subroutine loadtxt_sp

  !> \brief   Determine number of columns in a file. The columns are assumed to be separated by spaces or tabs.
  !> \return  Number of columns.
  integer function number_of_columns(u)

    integer(i4),intent(in) :: u !< unit of the open file
    integer(i4) :: ios
    character :: c
    logical :: lastblank

    rewind(u)
    number_of_columns = 0
    lastblank = .true.
    do
      read(u, '(a)', advance='no', iostat=ios) c
      if (ios /= 0) exit
      if (lastblank .and. .not. is_blank(c)) number_of_columns = number_of_columns + 1
      lastblank = is_blank(c)
    end do
    rewind(u)

  end function number_of_columns

  !> \brief   Determine number of rows in a file.
  !> \return  Number of rows.
  integer function number_of_rows(u)

    integer(i4), intent(in) :: u !< unit of the open file
    integer(i4) :: ios

    rewind(u)
    number_of_rows = 0
    do
      read(u, *, iostat=ios)
      if (ios /= 0) exit
      number_of_rows = number_of_rows + 1
    end do
    rewind(u)

  end function number_of_rows

end module mo_io
