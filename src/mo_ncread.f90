!> \file mo_ncread.f90
!> \copydoc mo_ncread

!> \brief Reading netcdf files
!> \details Subroutines for reading arrays from nc file using the netcdf4 library.
!> \author Stephan Thober, Matthias Cuntz
!> \date Nov 2011
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_NcRead

  use mo_kind, only : i4, i8, sp, dp

  ! functions and constants of netcdf4 library
  use netcdf, only : nf90_open, nf90_get_var, nf90_close, NF90_MAX_NAME, &
          nf90_get_att, nf90_inq_varid, nf90_inquire_variable, &
          nf90_inquire_dimension, NF90_NOWRITE, &
          nf90_noerr, nf90_strerror, nf90_inquire_attribute

  implicit none

  public :: Get_NcDim    ! get the dimensions of a Variable
  public :: Get_NcDimAtt ! get the attributes of the dimensions
  public :: Get_NcVarAtt ! get attributes of a variable
  public :: Get_NcVar    ! get the data of a Variable in a nc file
  public :: NcOpen       ! Open a file and get a handle back
  public :: NcClose      ! Close a file

  ! ------------------------------------------------------------------------------

  !>    \brief Read array from NC file

  !>    \details
  !!    Reads a 2 - 5 dimensional array from a nc file given
  !!    the variable name EXACTLY as specified in the file.  If the
  !!    array is not allocated when calling, Get_NcVar will
  !!    allocate it internally. If the dimension of the actual data
  !!    is less than the ones of the array, then the dimension
  !!    lengths of the array will be filled with ones.
  !!
  !!    \b Example
  !!
  !!    See test program in directory test_mo_NcRead.
  !!
  !!    \b Literature
  !!
  !!    1.  http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html
  !!
  !>    \param[in]  "character(len=*) :: Filename"                                  Name of the nc file.
  !>    \param[in]  "character(len=*) :: VarName"                                   Name of the Variable in the nc file.
  !>    \param[in]  "real(sp/dp), dimension(:,:[,:[,:[,:]]]), allocatable :: array" Array where data will be read.
  !>    \param[in]  "integer(i4), dimension(:) :: jdate"                            Starting indeces of first value to read.
  !!                                                                                `len` is the number of dimensions of
  !!                                                                                array, default is 1, see example
  !>    \param[in]  "integer(i4), dimension(:) :: a_count"                          Same size as jdate, specifies how
  !!                                                                                many values in each dimension
  !!                                                                                is going to be read
  !>    \param[in]  "integer(i4)               :: fid"                              File handle of opened netcdf file

  !>    \note  Output array is a floating point of 2-5 dimensions.\n
  !!            NOT yet tested for different compilers than intel11.1.075
  !!            CANNOT read packed data.\n
  !!            i1 indicates, that 1 byte integer is read [type is integer(1)].

  !>    \author Stephan Thober
  !>    \date Nov 2011
  !!     -  added comments
  !>    \date Mar 2012
  !!     -  corrected dynamical read of data
  !>    \date May 2012
  !!     -  fid
  !>    \date Nov 2012
  !!     -  write out Varname, when vartype is incorrect
  !>    \date Feb 2013
  !!     -  added 1 byte integer version
  !>    \date Mar 2014
  !!     -  added subroutines for allocatable arrays
  !>    \author Matthias Cuntz
  !>    \date Jan 2012
  !!     -  unified routines for different dimensions and data types

  ! ------------------------------------------------------------------------------

  interface Get_NcVar
    module procedure Get_NcVar_0d_sp, Get_NcVar_0d_dp, Get_NcVar_1d_sp, &
            Get_NcVar_1d_dp, Get_NcVar_2d_sp, Get_NcVar_2d_dp, &
            Get_NcVar_3d_sp, Get_NcVar_3d_dp, Get_NcVar_4d_sp, &
            Get_NcVar_4d_dp, Get_NcVar_5d_sp, Get_NcVar_5d_dp, &
            Get_NcVar_0d_i4, Get_NcVar_1d_i4, Get_NcVar_2d_i4, &
            Get_NcVar_3d_i4, Get_NcVar_4d_i4, Get_NcVar_5d_i4, &
            Get_NcVar_0d_i1, Get_NcVar_1d_i1, Get_NcVar_2d_i1, &
            Get_NcVar_3d_i1, Get_NcVar_4d_i1, Get_NcVar_5d_i1
  end interface Get_NcVar

  ! ------------------------------------------------------------------------------

  private

  ! ------------------------------------------------------------------------------

contains

  ! ------------------------------------------------------------------------------

  !>    \brief Dimension of netcdf variable.

  !>    \details
  !!    Gets the dimensions of variable in a netcdf file.
  !!
  !!    \b Literature
  !!
  !!    1.  http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html
  !!
  !>    \param[in]  "character(len=*) :: Filename"            Filename of netcdf file
  !>    \param[in]  "character(len=*) :: Variable"            Variable name exactly as specified in the file
  !>    \param[in]  "logical, optional       :: PrintInfo"    If given and true, information about dimension
  !!                                                          and their lengths will be printed to standard output.
  !>    \retval     "integer(i4), dimension(5) :: Get_NcDim"  Dimension length, -1 if dimension does not exist
  !>    \param[out] "integer(i4), optional :: ndims"                    # of dimensions

  !>    \author Stephan Thober
  !>    \date Dec 2011
  !>    \author Matthias Cuntz
  !>    \date Jan 2012
  !!      - ndims
  function Get_NcDim(Filename, Variable, PrintInfo, ndims)
    !
    implicit none
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: Variable
    logical, optional, intent(in) :: PrintInfo
    integer(i4), optional, intent(out) :: ndims
    integer(i4), dimension(5) :: Get_NcDim
    !
    logical :: PrintFlag
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: NumDims ! # of dimensions
    !
    ! Open NetCDF filename
    call check(nf90_open(Filename, NF90_NOWRITE, ncid))
    !
    PrintFlag = .false.
    if (present(PrintInfo)) PrintFlag = PrintInfo
    !
    ! Inquire file and check if VarName exists in the dataset,
    ! get number of dimensions and
    ! get the length of the dimensions
    call Get_Info(Variable, ncid, varid, vartype, Get_NcDim, Info = PrintFlag, ndims = NumDims)
    if (present(ndims)) ndims = NumDims
    !
    ! close File
    call check(nf90_close(ncid))
    !
  end function Get_NcDim

  ! ------------------------------------------------------------------

  !>    \brief Name and size of variable in netcdf.

  !>    \details
  !!    Gets the name and size of the dimensions of a variable in a netcdf file
  !!
  !!    \b Example
  !!
  !!         Filename = 'test.nc'
  !!         Varname  = 'data'
  !!         call Get_NcDimAtt(Filename, Varname, DimNames, DimLen)
  !!
  !!    See example in test directory.
  !!
  !!    \b Literature
  !!
  !!    1.  http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html
  !!
  !>    \param[in]  "character(len=*),  intent(in) :: Filename"   Filename of netcdf file.
  !>    \param[in]  "character(len=*),  intent(in) :: Variable"   Variable name exactly as specified in the file.
  !>    \param[out] "integer(i4), dimension(:), allocatable, optional, intent(out) :: DimLen"    Allocatable array with the size
  !!                                                                                             of the dimensions
  !>    \retval     "character(len=*), dimension(:), allocatable, intent(out)      :: DimName"   Allocatable array with the
  !!                                                                                             names of the dimensions.

  !>    \note  DimName and DimLen are both allocated within the subroutine, so please just provide an 1 dimensional
  !!            allocatable array to the subroutine.

  !>    \author Matthias Zink
  !>    \date Oct 2012

  subroutine Get_NcDimAtt(Filename, Variable, DimName, DimLen)
    !
    implicit none
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: Variable
    character(len = *), dimension(:), allocatable, &
            intent(out) :: DimName
    integer(i4), dimension(:), allocatable, &
            optional, intent(out) :: DimLen
    !
    integer(i4), dimension(5) :: Get_NcDim
    !
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: NumDims ! # of dimensions
    integer(i4) :: dimid
    integer(i4) :: len
    !
    ! Open NetCDF filename
    call check(nf90_open(Filename, NF90_NOWRITE, ncid))
    !
    ! Inquire file and check if VarName exists in the dataset,
    ! get number of dimensions and
    ! get the length of the dimensions
    call Get_Info(Variable, ncid, varid, vartype, Get_NcDim, Info = .FALSE., ndims = NumDims)
    !
    allocate(DimName(NumDims))
    if (present(DimLen)) allocate(DimLen(NumDims))
    !
    do dimid = 1, NumDims
      call check(nf90_inquire_dimension(ncid, dimid, DimName(dimid), len))
      if (present(DimLen)) DimLen(dimid) = len
    end do
    ! close File
    call check(nf90_close(ncid))
    !
  end subroutine Get_NcDimAtt

  ! ------------------------------------------------------------------

  !>    \brief Get attribute of netcdf variable.

  !>    \details
  !!    Gets the values of an particular attribute of an variable.
  !!
  !!    \b Example
  !!
  !!         Filename = 'test.nc'
  !!         VarName  = 'data'
  !!         AttName  = 'units'
  !!         call Get_NcDimAtt(Filename, Varname, AttName, AttValues)
  !!
  !!    See example in test directory.
  !!
  !!    \b Literature
  !!
  !!    1.  http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html

  !>    \param[in]  "character(len=*), intent(in)      :: FileName"     Filename of netcdf file.
  !>    \param[in]  "character(len=*), intent(in)      :: VarName"      Variable name exactly as specified in the file.
  !>    \param[in]  "character(len=*), intent(in)      :: AttName"      Attribute name exactly as specified for the Variable.
  !>    \param[in]  "integer(i4), optional, intent(in) :: fid"          File handle of opened netcdf file
  !>    \param[in]  "integer(i4), optional, intent(in) :: dtype"        Datatype (ineteger,float) see NetCDF convention
  !!                                                                    (unidata.ucar)
  !>    \retval     "character(len=*), intent(out)     :: AttValues"    Values of the Attribute.

  !>    \note AttValues are restricted to be of character type. \n
  !!           The name of the variable (VarName) has to be known beforehand. \n
  !!           The name of the attribute (AttName) has to be known beforehand.

  !>    \author Matthias Zink
  !>    \date Oct 2012
  !>    \author Matthias Cuntz & Juliane Mai
  !>    \date Nov 2014
  !!      - correct data type detection.

  subroutine Get_NcVarAtt(FileName, VarName, AttName, AttValues, fid, dtype)
    !
    implicit none
    !
    character(len = *), intent(in) :: FileName
    character(len = *), intent(in) :: VarName
    character(len = *), intent(in) :: AttName
    character(len = *), intent(out) :: AttValues
    integer(i4), optional, intent(in) :: fid
    integer(i4), optional, intent(out) :: dtype
    !
    integer(i4) :: ncid
    integer(i4) :: varid
    !
    integer(i4) :: type
    integer(i8) :: avint
    real(dp) :: avfloat
    character(256) :: avchar
    !
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call check(nf90_inq_varid(ncid, trim(VarName), varid))
    ! get type of the attribute
    call check(nf90_inquire_attribute(ncid, varid, trim(AttName), type))
    !
    ! read attribute by type
    select case (type)
    case (1) ! 1 = NF90_BYTE
      call check(nf90_get_att(ncid, varid, trim(AttName), avint))
      write(AttValues, '(i4)')  avint
      AttValues = adjustl(trim(AttValues))
      if (present(dtype)) dtype = type
    case (2) ! NF90_CHAR
      call check(nf90_get_att(ncid, varid, trim(AttName), avchar))
      AttValues = adjustl(trim(avchar))
      if (present(dtype)) dtype = type
    case (3) ! NF90_SHORT
      call check(nf90_get_att(ncid, varid, trim(AttName), avint))
      write(AttValues, '(i6)')  avint
      AttValues = adjustl(trim(AttValues))
      if (present(dtype)) dtype = type
    case (4) ! NF90_INT
      call check(nf90_get_att(ncid, varid, trim(AttName), avint))
      write(AttValues, '(i11)')  avint
      AttValues = adjustl(trim(AttValues))
      if (present(dtype)) dtype = type
    case (5) ! NF90_FLOAT
      call check(nf90_get_att(ncid, varid, trim(AttName), avfloat))
      write(AttValues, '(E15.7)')  avfloat
      AttValues = adjustl(trim(AttValues))
      if (present(dtype)) dtype = type
    case (6) ! NF90_DOUBLE
      call check(nf90_get_att(ncid, varid, trim(AttName), avfloat))
      write(AttValues, '(E24.15)')  avfloat
      AttValues = adjustl(trim(AttValues))
      if (present(dtype)) dtype = type
    case DEFAULT
      print*, '***ERROR: mo_ncread: Mismatch in attribute datatype!'
    end select
    !
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVarAtt

  ! ------------------------------------------------------------------------------

  subroutine Get_NcVar_0d_sp(Filename, VarName, Dat, fid)
    !
    implicit none
    !
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(sp), intent(inout) :: Dat    ! array where values should be stored
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_0d_sp

  subroutine Get_NcVar_0d_dp(Filename, VarName, Dat, fid)
    !
    implicit none
    !
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), intent(inout) :: Dat    ! array where values should be stored
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_0d_dp

  subroutine Get_NcVar_1d_sp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 1
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(sp), dimension(:), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_1d_sp


  subroutine Get_NcVar_1d_dp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 1
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), dimension(:), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_1d_dp


  subroutine Get_NcVar_2d_sp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 2
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    !real(sp),    dimension(:,:), allocatable, intent(inout) :: Dat    ! array where values should be stored
    real(sp), dimension(:, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_2d_sp


  subroutine Get_NcVar_2d_dp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 2
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), dimension(:, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_2d_dp


  subroutine Get_NcVar_3d_sp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 3
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(sp), dimension(:, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i

    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if

    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_3d_sp


  subroutine Get_NcVar_3d_dp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 3
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), dimension(:, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_3d_dp


  subroutine Get_NcVar_4d_sp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 4
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(sp), dimension(:, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_4d_sp


  subroutine Get_NcVar_4d_dp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 4
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), dimension(:, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_4d_dp


  subroutine Get_NcVar_5d_sp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 5
    integer, parameter :: itype = 5 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(sp), dimension(:, :, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4), Rcount(5)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_5d_sp


  subroutine Get_NcVar_5d_dp(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 5
    integer, parameter :: itype = 6 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    real(dp), dimension(:, :, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4), Rcount(5)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) < size(shape(dat))) stop 'ERROR*** start has less values than data has dimensions. GetNcVar'
      if (size(start) > 5) stop 'ERROR*** start has dimension greater than 5. GetNcVar'
      Rstart(1 : size(start)) = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) < size(shape(dat))) stop 'ERROR*** a_count has less values than data has dimensions. GetNcVar'
      if (size(a_count) > 5) stop 'ERROR*** a_count has dimension greater than 5. GetNcVar'
      Rcount(1 : size(a_count)) = a_count
      do i = 1, idims
        if (size(Dat, i) < Rcount(i)) stop 'ERROR*** try to read more data in dimension than there is. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_5d_dp


  subroutine Get_NcVar_0d_i4(Filename, VarName, Dat, fid)
    !
    implicit none
    !
    integer, parameter :: itype = 4 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), intent(inout) :: Dat    ! array where values should be stored
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_0d_i4


  subroutine Get_NcVar_1d_i4(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 1
    integer, parameter :: itype = 4 ! 3 = single 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), dimension(:), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_1d_i4


  subroutine Get_NcVar_2d_i4(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 2
    integer, parameter :: itype = 4 ! 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), dimension(:, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_2d_i4


  subroutine Get_NcVar_3d_i4(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 3
    integer, parameter :: itype = 4 ! 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), dimension(:, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_3d_i4


  subroutine Get_NcVar_4d_i4(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 4
    integer, parameter :: itype = 4 ! 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), dimension(:, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_4d_i4


  subroutine Get_NcVar_5d_i4(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 5
    integer, parameter :: itype = 4 ! 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(i4), dimension(:, :, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4), Rcount(5)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_5d_i4

  subroutine Get_NcVar_0d_i1(Filename, VarName, Dat, fid)
    !
    implicit none
    !
    integer, parameter :: itype = 1 ! 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), intent(inout) :: Dat    ! array where values should be stored
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_0d_i1


  subroutine Get_NcVar_1d_i1(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 1
    integer, parameter :: itype = 1 ! 3 = single 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), dimension(:), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_1d_i1


  subroutine Get_NcVar_2d_i1(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 2
    integer, parameter :: itype = 1 ! 1 = 1 byte, 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), dimension(:, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_2d_i1


  subroutine Get_NcVar_3d_i1(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 3
    integer, parameter :: itype = 1 ! 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), dimension(:, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_3d_i1


  subroutine Get_NcVar_4d_i1(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 4
    integer, parameter :: itype = 1 ! 1 = 1 byte, 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), dimension(:, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_4d_i1


  subroutine Get_NcVar_5d_i1(Filename, VarName, Dat, start, a_count, fid)
    !
    implicit none
    !
    integer, parameter :: idims = 5
    integer, parameter :: itype = 1 ! 1 = 1 byte, 3 = single, 5 = float, 6 = double
    !
    character(len = *), intent(in) :: Filename
    character(len = *), intent(in) :: VarName ! Variable name
    integer(1), dimension(:, :, :, :, :), allocatable, intent(inout) :: Dat    ! array where values should be stored
    integer(i4), dimension(:), optional, intent(in) :: start
    integer(i4), dimension(:), optional, intent(in) :: a_count
    integer(i4), optional, intent(in) :: fid
    !
    integer(i4), dimension(5) :: Rstart
    integer(i4), dimension(5) :: Rcount
    integer(i4) :: ncid    ! id of input stream
    integer(i4) :: varid   ! id of variable to be read
    integer(i4) :: vartype ! type of variable
    integer(i4) :: i
    !
    ! Defaults for Options Start and Count
    Rstart = 1
    Rcount = 1
    !
    ! allocate Dat
    if (.not. allocated(Dat)) then
      if (.not. present(a_count)) then
        Rcount = Get_NcDim(Filename, Varname)
      else
        Rcount(1 : idims) = a_count(1 : idims)
      end if
      allocate(Dat(Rcount(1), Rcount(2), Rcount(3), Rcount(4), Rcount(5)))
    else
      Rcount(1 : idims) = shape(Dat)
    end if
    !
    ! Assign options Start and Count if present
    if (present(start)) then
      if (size(start) /= idims) stop 'ERROR*** size of start does not equal dimensions of data. GetNcVar'
      Rstart = start
    end if
    !
    if (present(a_count)) then
      if (size(a_count) /= idims) stop 'ERROR*** size of a_count does not equal dimensions of data. GetNcVar'
      Rcount = a_count
      do i = 1, idims
        if (size(Dat, i) /= Rcount(i)) stop 'ERROR*** size mismatch. Get_NcVar'
      end do
    end if
    !
    ! Open NetCDF filename
    if (present(fid)) then
      ncid = fid
    else
      call check(nf90_open(trim(Filename), NF90_NOWRITE, ncid))
    end if
    !
    ! Inquire file, check if VarName exists and get the id
    call Get_Info(Varname, ncid, varid, vartype)
    ! check variable type ( 5 equals float type, 6 equals double )
    if (vartype /= itype) then
      print *, 'Variable name: ', trim(Varname)
      print *, 'ERROR*** type of variable does not match argument type. subroutine Get_NcVar'
      stop
    end if
    !
    ! get values by varid
    call check(nf90_get_var(ncid, varid, Dat, Rstart, Rcount))
    !
    ! close File
    if (.not. present(fid)) call check(nf90_close(ncid))
    !
  end subroutine Get_NcVar_5d_i1
  ! ------------------------------------------------------------------------------

  !>    \brief Open netcdf file

  !>    \details Opens a netcdf file and returns file handle
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    id = NcOpen(Fname)
  !!    \endcode
  !!
  !!    \b Literature
  !!
  !!    1. http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html

  !>    \param[in] "character(len=*) :: Fname"    Filename of file to open
  !>    \param[out] "integer(i4)      :: NcOpen"  id of opened stream

  !>    \author Stephan Thober,
  !>    \date May 2012

  function NcOpen(Fname)
    !
    implicit none
    !
    character(len = *), intent(in) :: Fname
    integer(i4) :: NcOpen
    !
    call check(nf90_open(trim(Fname), NF90_NOWRITE, NcOpen))
    !
  end function NcOpen

  ! ------------------------------------------------------------------------------

  !>    \brief Closes netcdf file

  !>    \details Closes a netcdf file by file handle
  !!
  !!    \b Example
  !!
  !!    \code{.f90}
  !!    call NcClose(ncid)
  !!    \endcode
  !!
  !!    \b Literature
  !!
  !!    1. http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90.html

  !>    \param[in] "ncid" - file handle of open netcdf file

  !>    \author Stephan Thober
  !>    \date May 2012

  subroutine NcClose(ncid)
    !
    implicit none
    !
    integer(i4), intent(in) :: ncid
    !
    call check(nf90_close(ncid))
    !
  end subroutine NcClose

  ! ------------------------------------------------------------------------------
  !>    \brief Get variable info from netcdf files

  !>    \details This subroutine is PRIVATE and therefore does not exist outside of this module.
  !!
  !!    This subroutine inquires the nc file. Given the Variable name and the stream
  !!    of the nc file, this subroutine determines the variable id, the kind of the
  !!    variable and the length of the dimensions in the file.
  !!
  !!    See: http://www.unidata.ucar.edu/software/netcdf/docs/netcdf-f90/ -> NF90-INQUIRE
  !!    for detailed information.

  !>    \author Matthias Cuntz,
  !>    \date Nov 2012
  !!      - default dimension length -1

  subroutine Get_Info(Varname, ncid, varid, xtype, dl, Info, ndims)
    !
    implicit none
    !
    character(len = *), intent(in) :: Varname
    integer(i4), intent(in) :: ncid
    integer(i4), intent(out) :: varid    ! variable id of data to be read
    integer(i4), intent(out) :: xtype    ! type of the variable
    integer(i4), dimension(:), optional, intent(inout) :: dl
    logical, optional, intent(in) :: Info
    integer(i4), optional, intent(out) :: ndims    ! Number of Dimensions for specific variable
    !
    integer(i4), dimension(:), allocatable :: DimID   ! Id of dimension
    character(NF90_MAX_NAME) :: name    ! name of Variables in the file
    integer(i4) :: NumDims ! Number of Dimensions for specific variable
    integer(i4) :: n       ! loop index
    character(256) :: form    ! output format
    integer(i4) :: itmp
    !
    call check(nf90_inq_varid(ncid, Varname, varid))
    call check(nf90_inquire_variable(ncid, varid, ndims = NumDims))
    if (present(ndims)) ndims = NumDims
    !
    ! get the dimension Ids and the data type of the variable
    allocate(DimId(NumDims))
    call check(nf90_inquire_variable(ncid, varid, xtype = xtype, dimids = DimId))
    !
    if (present(dl)) then
      ! check consistency of dimensions
      if (NumDims > size(dl)) &
              stop 'ERROR*** Dimension size of Variable is greater than dims of array. subroutine Get_Info'
      ! go through dimension ids and get its length
      dl(:) = -1 ! initialise
      dimloop : do n = 1, NumDims
        call check(nf90_inquire_dimension(ncid, DimId(n), name, itmp))
        dl(n) = itmp
        if (present(info)) then
          if (info) then
            write(form, '(a12,I03.3,a1)') "(a10,i1,a4,a", len(trim(name)), ")"
            write(*, form) 'Dimension ', n, ' is ', trim(name)
            write(*, '(a14,i5.5)') 'The Length is ', dl(n)
          end if
        end if
      end do dimloop
      !
    end if
    !
  end subroutine Get_Info

  ! -----------------------------------------------------------------------------
  !  private error checking routine
  subroutine check(status)
    !
    implicit none
    !
    integer(i4), intent(in) :: status
    !
    if (status /= nf90_noerr) then
      write(*, *) trim(nf90_strerror(status))
      stop
    end if
    !
  end subroutine check

end module mo_NcRead
