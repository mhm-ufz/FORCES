!> \file    mo_grid.f90
!> \brief   \copybrief mo_grid
!> \details \copydetails mo_grid

!> \brief   Grid handling utils.
!> \details This module provides routines deal with uniform grids based on ESRI grids, also know as ascii grids.
!!          This means, the grids have a constant cell size along axis and are assumed to be 2D.
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Mar 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_grid

  use mo_kind, only: i4, dp, sp

  implicit none
  private
  ! coordsys selector
  integer(i4), public, parameter :: coordsys_cart = 0_i4 !< Cartesian coordinate system.
  integer(i4), public, parameter :: coordsys_sph_deg = 1_i4 !< Spherical coordinates in degrees.
  ! integer(i4), public, parameter :: coordsys_sph_rad = 2_i4
  !> \class   coordsys_t
  !> \brief   Supported coordniate systems (cart, sph_deg).
  type, private :: coordsys_t
    integer(i4) :: cart = 0_i4 !< Cartesian coordinate system.
    integer(i4) :: sph_deg = 1_i4 !< Spherical coordinates in degrees.
    ! integer(i4), public :: sph_rad = 2_i4
  end type coordsys_t
  type(coordsys_t), public, parameter :: coordsys = coordsys_t(0_i4, 1_i4) !< Supported coordniate systems (cart, sph_deg).
  ! align selector
  integer(i4), public, parameter :: align_ll = 0_i4 !< align in lower left corner
  integer(i4), public, parameter :: align_lr = 1_i4 !< align in lower right corner
  integer(i4), public, parameter :: align_tl = 2_i4 !< align in top left corner
  integer(i4), public, parameter :: align_tr = 3_i4 !< align in top right corner
  !> \class   align_t
  !> \brief   Supported aligning corners (ll, lr, tl, tr).
  type, private :: align_t
    integer(i4) :: ll = 0_i4 !< align in lower left corner
    integer(i4) :: lr = 1_i4 !< align in lower right corner
    integer(i4) :: tl = 2_i4 !< align in top left corner
    integer(i4) :: tr = 3_i4 !< align in top right corner
  end type align_t
  type(align_t), public, parameter :: align = align_t(0_i4, 1_i4, 2_i4, 3_i4) !< Supported aligning corners (ll, lr, tl, tr).

  !>       \brief Reads spatial data files of ASCII format.
  !>       \details Reads spatial input data, e.g. dem, aspect, flow direction.
  !>       \authors Juliane Mai
  !>       \date Jan 2013
  !>       \changelog
  !!       - Matthias Zink, Feb 2013
  !!         - added interface and routine for datatype i4
  !!       - David Schaefer, Mar 2015
  !!         - removed double allocation of temporary data
  !!       - Robert Schweppe, Jun 2018
  !!         - refactoring and reformatting
  !!       - Sebastian Müller, Mar 2024
  !!         - moving to FORCES
  !!         - remove fileunit input (use newunit)
  !!         - make data and mask optional output
  interface  read_spatial_data_ascii
    module procedure read_spatial_data_ascii_i4, read_spatial_data_ascii_dp
  end interface read_spatial_data_ascii

contains
  ! ------------------------------------------------------------------

  !>       \brief Read spatial data.
  !>       \details Read spatial data from ascii file. Data will be transposed to be in xy order with decreasing y-axis.
  !>       \authors Robert Schweppe
  !>       \date Jun 2018
  subroutine read_spatial_data_ascii_dp(path, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, data, mask)
    implicit none

    character(len = *), intent(in) :: path !< path with location
    integer(i4), intent(in) :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in) :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in) :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_cellsize !< reference cellsize
    real(dp), dimension(:, :), allocatable, intent(out), optional :: data !< data, size (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask, size (nx, ny)

    integer(i4) :: file_nrows ! number of rows of data fields (ny)
    integer(i4) :: file_ncols ! number of columns of data fields (nx)
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    real(dp), dimension(:, :), allocatable :: tmp_data ! data to be transposed, size (ny, nx)
    logical, dimension(:, :), allocatable :: tmp_mask ! mask to be transposed, size (ny, nx)
    integer(i4) :: i, j, fileunit

    ! compare headers always with reference header (intent in)
    call read_header_ascii(path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata)
    if ((file_ncols .ne. ref_ncols)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: ncols')
    if ((file_nrows .ne. ref_nrows)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: nrows')
    if ((abs(file_xllcorner - ref_xllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: xllcorner')
    if ((abs(file_yllcorner - ref_yllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: yllcorner')
    if ((abs(file_cellsize - ref_cellsize)   .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: cellsize')

    ! don't read data, if not requested (this is basically just checking grid compatibility)
    if ( .not. (present(data) .or. present(mask)) ) return

    ! allocation and initialization of matrices
    allocate(tmp_data(file_nrows, file_ncols))
    tmp_data = file_nodata

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, 6
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (tmp_data(i, j), j = 1, file_ncols)
    end do
    close(fileunit)

    if ( present(data) ) then
      ! transpose of data due to longitude-latitude ordering
      allocate(data(file_ncols, file_nrows))
      data = transpose(tmp_data)
    end if

    if ( present(mask) ) then
      allocate(tmp_mask(file_nrows, file_ncols))
      tmp_mask = .true.
      where (abs(tmp_data - file_nodata) .lt. tiny(1.0_dp))
        tmp_mask = .false.
      end where
      ! set mask .false. if nodata value appeared
      allocate(mask(file_ncols, file_nrows))
      mask = transpose(tmp_mask)
      deallocate(tmp_mask)
    end if

    deallocate(tmp_data)

  end subroutine read_spatial_data_ascii_dp

  !>       \brief Read spatial data.
  !>       \details Read spatial data from ascii file. Data will be transposed to be in xy order with decreasing y-axis.
  !>       \authors Robert Schweppe
  !>       \date Jun 2018
  subroutine read_spatial_data_ascii_i4(path, ref_ncols, ref_nrows, ref_xllcorner, ref_yllcorner, ref_cellsize, data, mask)
    implicit none

    character(len = *), intent(in) :: path !< path with location
    integer(i4), intent(in) :: ref_nrows !< reference number of rows of data fields (ny)
    integer(i4), intent(in) :: ref_ncols !< reference number of columns of data fields (nx)
    real(dp), intent(in) :: ref_xllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_yllcorner !< reference lower left corner
    real(dp), intent(in) :: ref_cellsize !< reference cellsize
    integer(i4), dimension(:, :), allocatable, intent(out), optional :: data !< data (nx, ny)
    logical, dimension(:, :), allocatable, intent(out), optional :: mask !< mask (nx, ny)

    integer(i4) :: file_nrows ! number of rows of data fields
    integer(i4) :: file_ncols ! number of columns of data fields
    real(dp) :: file_xllcorner ! file read in lower left corner
    real(dp) :: file_yllcorner ! file read in lower left corner
    real(dp) :: file_cellsize ! file read in cellsize
    real(dp) :: file_nodata ! file read in nodata value
    integer(i4), dimension(:, :), allocatable :: tmp_data ! data
    logical, dimension(:, :), allocatable :: tmp_mask ! mask
    integer(i4) :: i, j, fileunit

    ! compare headers always with reference header (intent in)
    call read_header_ascii(path, file_ncols, file_nrows, file_xllcorner, file_yllcorner, file_cellsize, file_nodata)
    if ((file_ncols .ne. ref_ncols)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: ncols')
    if ((file_nrows .ne. ref_nrows)) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: nrows')
    if ((abs(file_xllcorner - ref_xllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: xllcorner')
    if ((abs(file_yllcorner - ref_yllcorner) .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: yllcorner')
    if ((abs(file_cellsize - ref_cellsize)   .gt. tiny(1.0_dp))) &
      call error_message('read_spatial_data_ascii: header not matching with reference header: cellsize')

    ! don't read data, if not requested
    if ( .not. (present(data) .or. present(mask)) ) return

    ! allocation and initialization of matrices
    allocate(tmp_data(file_nrows, file_ncols))
    tmp_data = int(file_nodata, i4)

    ! read in
    ! recl is only a rough estimate on bytes per line in the ascii
    ! default for nag: recl=1024(byte) which is not enough for 100s of columns
    open (newunit = fileunit, file = path, action = 'read', status = 'old', recl = 48 * file_ncols)
    ! (a) skip header
    do i = 1, 6
      read(fileunit, *)
    end do
    ! (b) read data
    do i = 1, file_nrows
      read(fileunit, *) (tmp_data(i, j), j = 1, file_ncols)
    end do
    close(fileunit)

    if ( present(data) ) then
      ! transpose of data due to longitude-latitude ordering
      allocate(data(file_ncols, file_nrows))
      data = transpose(tmp_data)
    end if

    if ( present(mask) ) then
      allocate(tmp_mask(file_nrows, file_ncols))
      tmp_mask = .true.
      where (tmp_data .EQ. int(file_nodata, i4))
        tmp_mask = .false.
      end where
      ! set mask .false. if nodata value appeared
      allocate(mask(file_ncols, file_nrows))
      mask = transpose(tmp_mask)
      deallocate(tmp_mask)
    end if

    deallocate(tmp_data)

  end subroutine read_spatial_data_ascii_i4

  ! ------------------------------------------------------------------

  !>       \brief Reads header lines of ASCII files.
  !>       \details Reads header lines of ASCII files, e.g. dem, aspect, flow direction.
  !>       \authors Juliane Mai
  !>       \date Jan 2013
  subroutine read_header_ascii(path, ncols, nrows, xllcorner, yllcorner, cellsize, nodata)

    use mo_os, only : check_path_isfile
    use mo_constants, only : nodata_dp
    implicit none

    character(len = *), intent(in) :: path !< Name of file and its location
    integer(i4), intent(out) :: nrows !< number of rows (ny)
    integer(i4), intent(out) :: ncols !< number of columns (nx)
    real(dp), intent(out) :: xllcorner !< lower left corner (x)
    real(dp), intent(out) :: yllcorner !< lower left corner (y)
    real(dp), intent(out) :: cellsize !< cell size [m]
    real(dp), intent(out) :: nodata !< nodata value (default -9999.0)

    character(5) :: dummy
    integer(i4) :: io, fileunit

    !checking whether the file exists
    call check_path_isfile(path=path, raise=.true.)
    ! reading header from a file
    open (newunit = fileunit, file = path, status = 'old')
    read (fileunit, *) dummy, ncols
    read (fileunit, *) dummy, nrows
    read (fileunit, *) dummy, xllcorner
    read (fileunit, *) dummy, yllcorner
    read (fileunit, *) dummy, cellsize
    read (fileunit, *, iostat=io) dummy, nodata
    ! EOF reached (nodata not present, use default value)
    if (io < 0) nodata = nodata_dp
    close(fileunit)
  end subroutine read_header_ascii

end module mo_grid
