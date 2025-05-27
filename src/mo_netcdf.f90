!> \file mo_netcdf.f90
!> \copydoc mo_netcdf



!> \brief NetCDF Fortran 90 interface wrapper
!> \details A wrapper around the NetCDF Fortran 90 interface.
!> \changelog
!! - David Schaefer, Jun 2015
!!   - written
!! - Matthias Cuntz, Jan 2016
!!   - compiled with PGI Fortran rev 15.9
!!   - no automatic allocation of left-hand-side
!! - Ricardo Torres, Feb 2017
!!   - add derived type NcGroup and NcAttributable. NcAttributable is the base derived type,
!!     NcGroup and NcVariable are extended from it. NcDataset is extended from NcGroup. No more
!!     duplicated routines to set attributes.
!> \authors David Schaefer
!> \date Jun 2015
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_netcdf

  use mo_kind, only: sp, dp, i1, i2, i4, i8
  use mo_utils, only: ne
  use ieee_arithmetic, only : ieee_is_nan

  use netcdf, only : &
          nf90_open, nf90_close, nf90_strerror, nf90_def_dim, nf90_def_var, &
          nf90_put_var, nf90_get_var, nf90_put_att, nf90_get_att, nf90_inq_attname, &
          nf90_inquire, nf90_inq_dimid, nf90_inquire_dimension, &
          nf90_inq_varid, nf90_inq_varids, nf90_inquire_variable, nf90_inquire_attribute, &
          nf90_inq_ncid, nf90_inq_grp_parent, nf90_inq_grpname, nf90_def_grp, &
          nf90_rename_dim, nf90_rename_var, nf90_rename_att, nf90_sync, &
          NF90_OPEN, NF90_NETCDF4, NF90_CREATE, NF90_WRITE, NF90_NOWRITE, &
          NF90_BYTE, NF90_SHORT, NF90_INT, NF90_INT64, NF90_FLOAT, NF90_DOUBLE, NF90_CHAR, &
          NF90_FILL_BYTE, NF90_FILL_SHORT, NF90_FILL_INT, NF90_FILL_FLOAT, NF90_FILL_DOUBLE, &
          NF90_NOERR, NF90_UNLIMITED, NF90_GLOBAL, NF90_SHARE, NF90_HDF5, &
          NF90_64BIT_OFFSET, NF90_CLASSIC_MODEL

  implicit none

  ! --------------------------------------------------------------------------------------
  character(10), parameter :: CF_FILL_VALUE = '_FillValue' !< CF fill value
  character(11), parameter :: CF_VALID_RANGE = 'valid_range' !< CF valid range
  character(9), parameter :: CF_VALID_MIN = 'valid_min' !< CF valid min
  character(9), parameter :: CF_VALID_MAX = 'valid_max' !< CF valid max
  integer(i4), parameter :: CF_USE_FILL_VALUE = 1_i4 !< CF use fill value
  integer(i4), parameter :: CF_USE_VALID_MIN = 2_i4 !< CF use valid min
  integer(i4), parameter :: CF_USE_VALID_MAX = 3_i4 !< CF use valid max
  integer(i4), parameter :: CF_USE_VALID_RANGE = 4_i4 !< CF use valid range
  integer(i4), parameter :: CF_USE_NAN = 5_i4 !< CF use nan

  !> \brief NetCDF base class
  type, abstract :: NcBase

    integer(i4) :: id !< object id

  contains

    procedure(getNameInterface), deferred :: getName !< object name
    procedure(getParentInterface), deferred :: getParent !< object parent

  end type NcBase

  !> \brief NetCDF attributable class
  type, abstract, extends(NcBase) :: NcAttributable

  contains

    procedure, public :: hasAttribute !< object has attribute
    procedure, public :: renameAttribute !< rename attribute
    procedure, private :: getAttributableIds
    procedure, public :: getAttributeNames

    procedure, private :: setAttribute_0d_sp
    generic, public :: setAttribute => setAttribute_0d_sp !< set attribute
    procedure, private :: getAttribute_0d_sp
    generic, public :: getAttribute => getAttribute_0d_sp !< get attribute
    procedure, private :: setAttribute_1d_sp
    generic, public :: setAttribute => setAttribute_1d_sp !< set attribute
    procedure, private :: getAttribute_1d_sp
    generic, public :: getAttribute => getAttribute_1d_sp !< get attribute
    procedure, private :: setAttribute_0d_dp
    generic, public :: setAttribute => setAttribute_0d_dp !< set attribute
    procedure, private :: getAttribute_0d_dp
    generic, public :: getAttribute => getAttribute_0d_dp !< get attribute
    procedure, private :: setAttribute_1d_dp
    generic, public :: setAttribute => setAttribute_1d_dp !< set attribute
    procedure, private :: getAttribute_1d_dp
    generic, public :: getAttribute => getAttribute_1d_dp !< get attribute
    procedure, private :: setAttribute_0d_i1
    generic, public :: setAttribute => setAttribute_0d_i1 !< set attribute
    procedure, private :: getAttribute_0d_i1
    generic, public :: getAttribute => getAttribute_0d_i1 !< get attribute
    procedure, private :: setAttribute_1d_i1
    generic, public :: setAttribute => setAttribute_1d_i1 !< set attribute
    procedure, private :: getAttribute_1d_i1
    generic, public :: getAttribute => getAttribute_1d_i1 !< get attribute
    procedure, private :: setAttribute_0d_i2
    generic, public :: setAttribute => setAttribute_0d_i2 !< set attribute
    procedure, private :: getAttribute_0d_i2
    generic, public :: getAttribute => getAttribute_0d_i2 !< get attribute
    procedure, private :: setAttribute_1d_i2
    generic, public :: setAttribute => setAttribute_1d_i2 !< set attribute
    procedure, private :: getAttribute_1d_i2
    generic, public :: getAttribute => getAttribute_1d_i2 !< get attribute
    procedure, private :: setAttribute_0d_i4
    generic, public :: setAttribute => setAttribute_0d_i4 !< set attribute
    procedure, private :: getAttribute_0d_i4
    generic, public :: getAttribute => getAttribute_0d_i4 !< get attribute
    procedure, private :: setAttribute_1d_i4
    generic, public :: setAttribute => setAttribute_1d_i4 !< set attribute
    procedure, private :: getAttribute_1d_i4
    generic, public :: getAttribute => getAttribute_1d_i4 !< get attribute
    procedure, private :: setAttribute_0d_i8
    generic, public :: setAttribute => setAttribute_0d_i8 !< set attribute
    procedure, private :: getAttribute_0d_i8
    generic, public :: getAttribute => getAttribute_0d_i8 !< get attribute
    procedure, private :: setAttribute_1d_i8
    generic, public :: setAttribute => setAttribute_1d_i8 !< set attribute
    procedure, private :: getAttribute_1d_i8
    generic, public :: getAttribute => getAttribute_1d_i8 !< get attribute
    procedure, private :: setAttribute_0d_char
    generic, public :: setAttribute => setAttribute_0d_char !< set attribute
    procedure, private :: getAttribute_0d_char
    generic, public :: getAttribute => getAttribute_0d_char !< get attribute

  end type NcAttributable

  ! --------------------------------------------------------------------------------------

  !> \brief NetCDF Group class
  type, extends(NcAttributable) :: NcGroup

  contains

    ! getter
    procedure, private :: getVariableIds
    procedure, public :: getVariables !< get variables
    procedure, public :: getUnlimitedDimension !< get unlimited dimension
    procedure, public :: getNoVariables !< get number of variables

    procedure, private :: getDimensionByName
    procedure, private :: getDimensionById

    procedure, public :: getParent => getGroupParent !< get parent
    procedure, public :: getName => getGroupName !< get name
    procedure, public :: getGroup => getGroupByName !< get group by name
    procedure, public :: getVariable => getVariableByName !< get variable by name
    generic, public :: getDimension => &
            getDimensionById, &
            getDimensionByName !< get dimension

    ! checker
    procedure, public :: hasVariable !< has variable
    procedure, public :: hasDimension !< has dimension
    procedure, public :: hasGroup !< has group
    procedure, public :: isUnlimited => isDatasetUnlimited !< is unlimited

    ! setter
    procedure, public  :: setGroup !< set group
    procedure, public  :: setDimension !< set dimension
    procedure, public  :: setCoordinate !< set coordinate
    procedure, private :: set_scrip_dimension
    procedure, private :: set_1d_coordinate_variable
    procedure, private :: setVariableWithTypes
    procedure, private :: setVariableWithNames
    procedure, private :: setVariableWithIds

    generic, public :: setVariable => &
            setVariableWithNames, &
            setVariableWithTypes, &
            setVariableWithIds !< set variable

  end type NcGroup

  interface NcGroup
    procedure newNcGroup
  end interface NcGroup

  ! --------------------------------------------------------------------------------------

  !> \brief NetCDF Dataset class
  type, extends(NcGroup) :: NcDataset

    character(256) :: fname !< Filename of the opened dataset
    character(1) :: mode  !< File open mode

  contains

    procedure, public :: sync !< sync dataset
    procedure, public :: close !< close dataset

  end type NcDataset

  interface NcDataset
    procedure newNcDataset
  end interface NcDataset

  ! --------------------------------------------------------------------------------------

  !> \brief NetCDF Dimension class
  type, extends(NcBase) :: NcDimension

    type(NcGroup) :: parent  !< The dimension's parent
  contains
    procedure, public :: renameDimension !< rename dimension
    procedure, public :: getParent => getDimensionParent !< get parent
    procedure, public :: getName => getDimensionName !< get name
    procedure, public :: getLength => getDimensionLength !< get length
    procedure, public :: isUnlimited => isUnlimitedDimension !< is unlimited
  end type NcDimension

  interface NcDimension
    procedure newNcDimension
  end interface NcDimension
  ! --------------------------------------------------------------------------------------

  !> \brief NetCDF Variable class
  type, extends(NcAttributable) :: NcVariable
    type(NcGroup) :: parent   !< The variables's parent

  contains

    procedure, public :: renameVariable !< rename
    procedure, public :: getParent => getVariableParent !< get parent
    procedure, public :: getName => getVariableName !< get name
    procedure, private :: getSlicingShape

    procedure, private :: setData_0d_sp
    generic, public :: setData => setData_0d_sp !< set data
    procedure, private :: getData_0d_sp
    generic, public :: getData => getData_0d_sp !< get data
    procedure, private :: setData_1d_sp
    generic, public :: setData => setData_1d_sp !< set data
    procedure, private :: getData_1d_sp
    generic, public :: getData => getData_1d_sp !< get data
    procedure, private :: setData_2d_sp
    generic, public :: setData => setData_2d_sp !< set data
    procedure, private :: getData_2d_sp
    generic, public :: getData => getData_2d_sp !< get data
    procedure, private :: setData_3d_sp
    generic, public :: setData => setData_3d_sp !< set data
    procedure, private :: getData_3d_sp
    generic, public :: getData => getData_3d_sp !< get data
    procedure, private :: setData_4d_sp
    generic, public :: setData => setData_4d_sp !< set data
    procedure, private :: getData_4d_sp
    generic, public :: getData => getData_4d_sp !< get data
    procedure, private :: setData_5d_sp
    generic, public :: setData => setData_5d_sp !< set data
    procedure, private :: getData_5d_sp
    generic, public :: getData => getData_5d_sp !< get data
    procedure, private :: setData_6d_sp
    generic, public :: setData => setData_6d_sp !< set data
    procedure, private :: getData_6d_sp
    generic, public :: getData => getData_6d_sp !< get data
    procedure, private :: getCFAttributes_sp
    generic, public :: getCFAttributes => getCFAttributes_sp !< get CF attributes
    procedure, private :: setVariableFillValue_sp
    generic, public :: setFillValue => setVariableFillValue_sp !< set fill value
    procedure, private :: getVariableFillValue_sp
    generic, public :: getFillValue => getVariableFillValue_sp !< get fill value
    procedure, private :: setData_0d_dp
    generic, public :: setData => setData_0d_dp !< set data
    procedure, private :: getData_0d_dp
    generic, public :: getData => getData_0d_dp !< get data
    procedure, private :: setData_1d_dp
    generic, public :: setData => setData_1d_dp !< set data
    procedure, private :: getData_1d_dp
    generic, public :: getData => getData_1d_dp !< get data
    procedure, private :: setData_2d_dp
    generic, public :: setData => setData_2d_dp !< set data
    procedure, private :: getData_2d_dp
    generic, public :: getData => getData_2d_dp !< get data
    procedure, private :: setData_3d_dp
    generic, public :: setData => setData_3d_dp !< set data
    procedure, private :: getData_3d_dp
    generic, public :: getData => getData_3d_dp !< get data
    procedure, private :: setData_4d_dp
    generic, public :: setData => setData_4d_dp !< set data
    procedure, private :: getData_4d_dp
    generic, public :: getData => getData_4d_dp !< get data
    procedure, private :: setData_5d_dp
    generic, public :: setData => setData_5d_dp !< set data
    procedure, private :: getData_5d_dp
    generic, public :: getData => getData_5d_dp !< get data
    procedure, private :: setData_6d_dp
    generic, public :: setData => setData_6d_dp !< set data
    procedure, private :: getData_6d_dp
    generic, public :: getData => getData_6d_dp !< get data
    procedure, private :: getCFAttributes_dp
    generic, public :: getCFAttributes => getCFAttributes_dp !< get CF attributes
    procedure, private :: setVariableFillValue_dp
    generic, public :: setFillValue => setVariableFillValue_dp !< set fill value
    procedure, private :: getVariableFillValue_dp
    generic, public :: getFillValue => getVariableFillValue_dp !< get fill value
    procedure, private :: setData_0d_i1
    generic, public :: setData => setData_0d_i1 !< set data
    procedure, private :: getData_0d_i1
    generic, public :: getData => getData_0d_i1 !< get data
    procedure, private :: setData_1d_i1
    generic, public :: setData => setData_1d_i1 !< set data
    procedure, private :: getData_1d_i1
    generic, public :: getData => getData_1d_i1 !< get data
    procedure, private :: setData_2d_i1
    generic, public :: setData => setData_2d_i1 !< set data
    procedure, private :: getData_2d_i1
    generic, public :: getData => getData_2d_i1 !< get data
    procedure, private :: setData_3d_i1
    generic, public :: setData => setData_3d_i1 !< set data
    procedure, private :: getData_3d_i1
    generic, public :: getData => getData_3d_i1 !< get data
    procedure, private :: setData_4d_i1
    generic, public :: setData => setData_4d_i1 !< set data
    procedure, private :: getData_4d_i1
    generic, public :: getData => getData_4d_i1 !< get data
    procedure, private :: setData_5d_i1
    generic, public :: setData => setData_5d_i1 !< set data
    procedure, private :: getData_5d_i1
    generic, public :: getData => getData_5d_i1 !< get data
    procedure, private :: setData_6d_i1
    generic, public :: setData => setData_6d_i1 !< set data
    procedure, private :: getData_6d_i1
    generic, public :: getData => getData_6d_i1 !< get data
    procedure, private :: getCFAttributes_i1
    generic, public :: getCFAttributes => getCFAttributes_i1 !< get CF attributes
    procedure, private :: setVariableFillValue_i1
    generic, public :: setFillValue => setVariableFillValue_i1 !< set fill value
    procedure, private :: getVariableFillValue_i1
    generic, public :: getFillValue => getVariableFillValue_i1 !< get fill value
    procedure, private :: setData_0d_i2
    generic, public :: setData => setData_0d_i2 !< set data
    procedure, private :: getData_0d_i2
    generic, public :: getData => getData_0d_i2 !< get data
    procedure, private :: setData_1d_i2
    generic, public :: setData => setData_1d_i2 !< set data
    procedure, private :: getData_1d_i2
    generic, public :: getData => getData_1d_i2 !< get data
    procedure, private :: setData_2d_i2
    generic, public :: setData => setData_2d_i2 !< set data
    procedure, private :: getData_2d_i2
    generic, public :: getData => getData_2d_i2 !< get data
    procedure, private :: setData_3d_i2
    generic, public :: setData => setData_3d_i2 !< set data
    procedure, private :: getData_3d_i2
    generic, public :: getData => getData_3d_i2 !< get data
    procedure, private :: setData_4d_i2
    generic, public :: setData => setData_4d_i2 !< set data
    procedure, private :: getData_4d_i2
    generic, public :: getData => getData_4d_i2 !< get data
    procedure, private :: setData_5d_i2
    generic, public :: setData => setData_5d_i2 !< set data
    procedure, private :: getData_5d_i2
    generic, public :: getData => getData_5d_i2 !< get data
    procedure, private :: setData_6d_i2
    generic, public :: setData => setData_6d_i2 !< set data
    procedure, private :: getData_6d_i2
    generic, public :: getData => getData_6d_i2 !< get data
    procedure, private :: getCFAttributes_i2
    generic, public :: getCFAttributes => getCFAttributes_i2 !< get CF attributes
    procedure, private :: setVariableFillValue_i2
    generic, public :: setFillValue => setVariableFillValue_i2 !< set fill value
    procedure, private :: getVariableFillValue_i2
    generic, public :: getFillValue => getVariableFillValue_i2 !< get fill value
    procedure, private :: setData_0d_i4
    generic, public :: setData => setData_0d_i4 !< set data
    procedure, private :: getData_0d_i4
    generic, public :: getData => getData_0d_i4 !< get data
    procedure, private :: setData_1d_i4
    generic, public :: setData => setData_1d_i4 !< set data
    procedure, private :: getData_1d_i4
    generic, public :: getData => getData_1d_i4 !< get data
    procedure, private :: setData_2d_i4
    generic, public :: setData => setData_2d_i4 !< set data
    procedure, private :: getData_2d_i4
    generic, public :: getData => getData_2d_i4 !< get data
    procedure, private :: setData_3d_i4
    generic, public :: setData => setData_3d_i4 !< set data
    procedure, private :: getData_3d_i4
    generic, public :: getData => getData_3d_i4 !< get data
    procedure, private :: setData_4d_i4
    generic, public :: setData => setData_4d_i4 !< set data
    procedure, private :: getData_4d_i4
    generic, public :: getData => getData_4d_i4 !< get data
    procedure, private :: setData_5d_i4
    generic, public :: setData => setData_5d_i4 !< set data
    procedure, private :: getData_5d_i4
    generic, public :: getData => getData_5d_i4 !< get data
    procedure, private :: setData_6d_i4
    generic, public :: setData => setData_6d_i4 !< set data
    procedure, private :: getData_6d_i4
    generic, public :: getData => getData_6d_i4 !< get data
    procedure, private :: getCFAttributes_i4
    generic, public :: getCFAttributes => getCFAttributes_i4 !< get CF attributes
    procedure, private :: setVariableFillValue_i4
    generic, public :: setFillValue => setVariableFillValue_i4 !< set fill value
    procedure, private :: getVariableFillValue_i4
    generic, public :: getFillValue => getVariableFillValue_i4 !< get fill value
    procedure, private :: setData_0d_i8
    generic, public :: setData => setData_0d_i8 !< set data
    procedure, private :: getData_0d_i8
    generic, public :: getData => getData_0d_i8 !< get data
    procedure, private :: setData_1d_i8
    generic, public :: setData => setData_1d_i8 !< set data
    procedure, private :: getData_1d_i8
    generic, public :: getData => getData_1d_i8 !< get data
    procedure, private :: setData_2d_i8
    generic, public :: setData => setData_2d_i8 !< set data
    procedure, private :: getData_2d_i8
    generic, public :: getData => getData_2d_i8 !< get data
    procedure, private :: setData_3d_i8
    generic, public :: setData => setData_3d_i8 !< set data
    procedure, private :: getData_3d_i8
    generic, public :: getData => getData_3d_i8 !< get data
    procedure, private :: setData_4d_i8
    generic, public :: setData => setData_4d_i8 !< set data
    procedure, private :: getData_4d_i8
    generic, public :: getData => getData_4d_i8 !< get data
    procedure, private :: setData_5d_i8
    generic, public :: setData => setData_5d_i8 !< set data
    procedure, private :: getData_5d_i8
    generic, public :: getData => getData_5d_i8 !< get data
    procedure, private :: setData_6d_i8
    generic, public :: setData => setData_6d_i8 !< set data
    procedure, private :: getData_6d_i8
    generic, public :: getData => getData_6d_i8 !< get data
    procedure, private :: getCFAttributes_i8
    generic, public :: getCFAttributes => getCFAttributes_i8 !< get CF attributes
    procedure, private :: setVariableFillValue_i8
    generic, public :: setFillValue => setVariableFillValue_i8 !< set fill value
    procedure, private :: getVariableFillValue_i8
    generic, public :: getFillValue => getVariableFillValue_i8 !< get fill value

    procedure, public :: getNoDimensions !< get number of dimensions

    procedure, public :: getDimensions => getVariableDimensions !< get dimensions

    procedure, public :: getRank => getVariableRank !< get rank

    procedure, public :: getShape => getVariableShape !< get shape

    procedure, public :: getDtype => getVariableDtype !< get data type

    procedure, public :: isUnlimited => isUnlimitedVariable !< is unlimited


  end type NcVariable

  interface NcVariable
    procedure newNcVariable
  end interface NcVariable
  ! --------------------------------------------------------------------------------------

  ! abstract interfaces
  interface
    !> \brief get name abstract interface
    function getNameInterface(self)
      import NcBase
      class(NcBase), intent(in) :: self
      character(len = 256) :: getNameInterface
    end function getNameInterface

    !> \brief get parent abstract interface
    function getParentInterface(self)
      import NcBase, NcGroup
      class(NcBase), intent(in) :: self
      type(NcGroup) :: getParentInterface
    end function getParentInterface
  end interface

  !> \brief NetCDF comparison operator
  interface operator (==)
    procedure equalNcBases
  end interface operator (==)

contains

  function newNcDataset(fname, fmode, cmode) result(out)
    character(*), intent(in) :: fname
    character(1), intent(in) :: fmode
    character(*), intent(inout), optional :: cmode
    integer(i4) :: status
    type(NcDataset) :: out

    select case(fmode)
    case("w")
      status = nf90_create(trim(fname), getCreationMode(cmode), out%id)
    case("r")
      status = nf90_open(trim(fname), NF90_NOWRITE, out%id)
    case("a")
      status = nf90_open(trim(fname), NF90_WRITE, out%id)
    case default
      write(*, *) "Mode argument must be in 'w','r','a' ! "
      stop 1
    end select
    call check(status, "Failed to open file: " // fname)

    out%fname = fname
    out%mode = fmode
  end function newNcDataset

  function newNcVariable(id, parent) result(out)
    integer(i4), intent(in) :: id
    type(NcGroup), intent(in) :: parent
    type(NcVariable) :: out

    out%id = id
    out%parent = parent
  end function newNcVariable

  function newNcDimension(id, parent) result(out)
    integer(i4), intent(in) :: id
    type(NcGroup), intent(in) :: parent
    type(NcDimension) :: out

    out%id = id
    out%parent = parent
  end function newNcDimension

  function newNcGroup(id) result(out)
    integer(i4), intent(in) :: id
    type(NcGroup) :: out

    out%id = id
  end function newNcGroup

  subroutine sync(self)
    class(NcDataset) :: self

    call check(nf90_sync(self%id), "Failed to sync file: " // self%fname)
  end subroutine sync

  subroutine close(self)
    class(NcDataset) :: self

    call check(nf90_close(self%id), "Failed to close file: " // self%fname)
  end subroutine close

  function setGroup(self, name)
    class(NcGroup), intent(inout) :: self
    character(*), intent(in) :: name
    integer(i4) :: id
    type(NcGroup) :: setGroup

    call check(nf90_def_grp(self%id, name, id), "Failed to create new group: " // name)
    setGroup = NcGroup(id)
  end function setGroup

  function getGroupParent(self)
    class(NcGroup), intent(in) :: self
    integer(i4) :: id
    type(NcGroup) :: getGroupParent

    call check(nf90_inq_grp_parent(self%id, id), "Failed to get parent group of: " // self%getName())
    getGroupParent = NcGroup(id)
  end function getGroupParent

  function getGroupName(self)
    class(NcGroup), intent(in) :: self
    character(256) :: getGroupName

    call check(nf90_inq_grpname(self%id, getGroupName), "Failed to inquire group name")
  end function getGroupName

  function getNoVariables(self)
    class(NcGroup), intent(in) :: self
    integer(i4) :: getNoVariables

    call check(nf90_inquire(self%id, nvariables = getNoVariables), "Failed inquire number of variables")
  end function getNoVariables

  function getDimensionParent(self)
    class(NcDimension), intent(in) :: self
    type(NcGroup) :: getDimensionParent

    getDimensionParent = self%parent
  end function getDimensionParent

  function getVariableParent(self)
    class(NcVariable), intent(in) :: self
    type(NcGroup) :: getVariableParent

    getVariableParent = self%parent
  end function getVariableParent

  function getVariableIds(self)
    class(NcGroup), intent(in) :: self
    integer(i4), dimension(:), allocatable :: getVariableIds
    integer(i4) :: tmp

    allocate(getVariableIds(self%getNoVariables()))
    call check(nf90_inq_varids(self%id, tmp, getVariableIds), "Failed to inquire variable ids")
  end function getVariableIds

  function getVariables(self)
    class(NcGroup), intent(in) :: self
    type(NcVariable), dimension(:), allocatable :: getVariables
    integer(i4), dimension(:), allocatable :: varids
    integer(i4) :: i, nvars

    nvars = self%getNoVariables()
    allocate(getVariables(nvars), varids(nvars))

    varids = self%getVariableIds()
    do i = 1, size(varids)
      getVariables(i) = NcVariable(varids(i), self)
    end do

  end function getVariables

  function getDimensionName(self)
    class(NcDimension), intent(in) :: self
    character(len = 256) :: getDimensionName

    call check(nf90_inquire_dimension(self%parent%id, self%id, name = getDimensionName), &
            "Failed to inquire dimension name")
  end function getDimensionName

  function getDimensionLength(self)
    class(NcDimension), intent(in) :: self
    integer(i4) :: getDimensionLength

    call check(nf90_inquire_dimension(self%parent%id, self%id, len = getDimensionLength), &
            "Failed to inquire dimension: " // self%getName())
  end function getDimensionLength

  function isDatasetUnlimited(self)
    class(NcGroup), intent(in) :: self
    logical :: isDatasetUnlimited
    integer(i4) :: dimid

    call check(nf90_inquire(self%id, unlimitedDimId = dimid), &
            "Failed to inquire group " // self%getName())
    isDatasetUnlimited = (dimid /= -1)
  end function isDatasetUnlimited

  function getUnlimitedDimension(self)
    class(NcGroup), intent(in) :: self
    type(NcDimension) :: getUnlimitedDimension
    integer(i4) :: dimid

    call check(nf90_inquire(self%id, unlimitedDimId = dimid), &
            "Failed to inquire group " // self%getName())

    if (dimid == -1) then
      write(*, *) "Dataset has no unlimited dimension"
      stop 1
    end if

    getUnlimitedDimension = self%getDimension(dimid)
  end function getUnlimitedDimension

  function equalNcBases(left, right) result(out)
    class(NcBase), intent(in) :: left, right
    logical :: out

    out = (left%id == right%id)
  end function equalNcBases

  function isUnlimitedDimension(self)
    class(NcDimension), intent(in) :: self
    logical :: isUnlimitedDimension

    isUnlimitedDimension = .false.
    if (self%parent%isUnlimited()) then
      isUnlimitedDimension = (self == self%parent%getUnlimitedDimension())
    end if
  end function isUnlimitedDimension

  function set_scrip_dimension(self, centersDim1, centersDim2, cornersDim1, cornersDim2, subDimSizes, units) &
          result(ncDim)
    class(NcGroup), intent(in) :: self
    real(dp)      , intent(in), dimension(:) :: centersDim1
    real(dp)      , intent(in), dimension(:) :: centersDim2
    real(dp)      , intent(in), dimension(:,:) :: cornersDim1
    real(dp)      , intent(in), dimension(:,:) :: cornersDim2
    integer(i4)   , intent(in), dimension(:) :: subDimSizes
    character(256), intent(in) :: units
    type(NcDimension) :: ncDim

    type(NcDimension)          :: cornerDim, rankDim
    type(NcVariable)           :: ncVar
    integer(i4), allocatable, dimension(:) :: imask_data

    ! set the new ncDimension (integer values and name)
    ncDim = self%setDimension('grid_size', size(centersDim1))
    cornerDim = self%setDimension('grid_corners', size(cornersDim1, 1))
    rankDim = self%setDimension('grid_rank', size(subDimSizes))
    ! here we set the reference to ncDimension for labelled ncDimension which in fact is a variable
    ncVar = self%setVariable('grid_center_lon', "f64", [ncDim])
    call ncVar%setData(centersDim1)
    call ncVar%setAttribute('units', trim(units))
    ncVar = self%setVariable('grid_center_lat', "f64", [ncDim])
    call ncVar%setData(centersDim2)
    call ncVar%setAttribute('units', trim(units))
    ncVar = self%setVariable('grid_corner_lon', "f64", [cornerDim, ncDim])
    call ncVar%setData(cornersDim1)
    call ncVar%setAttribute('units', trim(units))
    ncVar = self%setVariable('grid_corner_lat', "f64", [cornerDim, ncDim])
    call ncVar%setData(cornersDim2)
    call ncVar%setAttribute('units', trim(units))
    ncVar = self%setVariable('grid_dims', "i32", [rankDim])
    call ncVar%setData(subDimSizes)
    ! set all values to 1 (True) for mask
    ncVar = self%setVariable('grid_imask', "i32", [ncDim])
    allocate(imask_data(size(centersDim1)))
    imask_data = 1_i4
    call ncVar%setData(imask_data)
    deallocate(imask_data)
    call ncVar%setAttribute('units', 'unitless')

  end function set_scrip_dimension

  subroutine set_1D_coordinate_variable(self, name, ncDim, bounds, referenceArg, ncVar)
    class(NcGroup), intent(in) :: self
    character(*)  , intent(in) :: name
    type(NcDimension), intent(in) :: ncDim
    real(dp)      , intent(in), dimension(:) :: bounds
    integer(i4)   , intent(in), optional :: referenceArg
    type(NcVariable), intent(out) :: ncVar

    type(NcVariable) :: ncVarBounds
    type(NcDimension) :: bndsDim
    integer(i8) :: dimLength, iBound
    integer(i4) :: reference
    character(256) :: dimBoundName
    real(dp), allocatable, dimension(:, :) :: boundData

    ! init
    dimLength = size(bounds, kind=i8)
    reference = 1_i4
    if (present(referenceArg)) then
      reference = referenceArg
    end if
    ! here we set the reference to ncDimension for labelled ncDimension which in fact is a variable
    ncVar = self%setVariable(name, "f64", [ncDim])
    ! write the data based on the type of reference
    select case(reference)
    case(0_i4)
      ! set the start values
      call ncVar%setData(bounds(1_i8:dimLength - 1_i8))
    case(1_i4)
      ! set the center values
      call ncVar%setData((bounds(2_i8:dimLength) + bounds(1_i8:dimLength-1_i8)) / 2.0_dp)
    case(2_4)
      ! set the end values
      call ncVar%setData(bounds(2_i8:dimLength))
    case default
      write(*,*) "reference id for set_Dimension is ", reference, ", must be 0, 1 or 2."
      stop 1
    end select

    ! --- bounds ---
    ! create dimension name for bounds
    dimBoundName = trim(name) // "_bnds"
    ! set the attribute
    call ncVar%setAttribute('bounds', trim(dimBoundName))
    ! set the dimensions used for the bounds array
    if (self%hasDimension("bnds")) then
      ! add it to our bounds of ncDimensions for the current array
      bndsDim = self%getDimension("bnds")
    else
      bndsDim = self%setDimension("bnds", 2_i4)
    end if
    ncVarBounds = self%setVariable(dimBoundName, "f64", [bndsDim, ncDim])

    ! allocate array for data
    allocate(boundData(2_i8, dimLength-1_i8))
    do iBound = 1_i8, dimLength-1_i8
      boundData(1_i8, iBound) = bounds(iBound)
      boundData(2_i8, iBound) = bounds(iBound + 1_i8)
    end do
    call ncVarBounds%setData(boundData)
    deallocate(boundData)

  end subroutine set_1D_coordinate_variable

  function setDimension(self, name, length) result(ncDim)
    class(NcGroup), intent(in) :: self
    character(*)  , intent(in) :: name
    integer(i4), intent(in), optional :: length

    type(NcDimension) :: ncDim
    integer(i4) :: dimLength
    integer(i4) :: id

    dimLength = NF90_UNLIMITED
    if (present(length)) then
      if (length > 0_i4) then
        dimLength = length
      end if
    end if

    call check(nf90_def_dim(self%id, name, dimLength, id), &
         "Failed to create dimension: " // name)

    ncDim = NcDimension(id, self)

  end function setDimension

  function setCoordinate(self, name, length, bounds, reference, attribute_names, attribute_values, &
                        centersDim1, centersDim2, cornersDim1, cornersDim2, subDimSizes, units) result(ncDim)
    class(NcGroup), intent(in) :: self
    character(*)  , intent(in) :: name
    integer(i4), intent(in), optional :: length
    real(dp)      , intent(in), optional, dimension(:) :: bounds
    integer(i4)   , intent(in), optional :: reference
    character(*) , intent(in), optional, dimension(:) :: attribute_names
    character(*) , intent(in), optional, dimension(:) :: attribute_values
    real(dp)      , intent(in), optional, dimension(:) :: centersDim1
    real(dp)      , intent(in), optional, dimension(:) :: centersDim2
    real(dp)      , intent(in), optional, dimension(:,:) :: cornersDim1
    real(dp)      , intent(in), optional, dimension(:,:) :: cornersDim2
    integer(i4)   , intent(in), optional, dimension(:) :: subDimSizes
    character(*), intent(in), optional :: units

    type(NcDimension) :: ncDim
    type(NcVariable) :: ncVar
    integer(i4) :: iAtt

    if (present(centersDim1) .and. present(centersDim2) .and. present(cornersDim1) .and. present(cornersDim2) &
             .and. present(subDimSizes) .and. present(units)) then
      ncDim = self%set_scrip_dimension(centersDim1, centersDim2, cornersDim1, cornersDim2, subDimSizes, units)
    else
      ! set the new ncDimension (integer values and name)
      ncDim = self%setDimension(name, length)

      if (present(bounds)) then
        call self%set_1D_coordinate_variable(name, ncDim, bounds, reference, ncVar)
        ! set attributes
        ! already set attributes
        if (present(attribute_names) .and. present(attribute_values)) then
          do iAtt = 1, size(attribute_names)
            call ncVar%setAttribute(trim(attribute_names(iAtt)), &
                    trim(attribute_values(iAtt)))
          end do
        end if
      end if
    end if

  end function setCoordinate

  function hasVariable(self, name)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    logical :: hasVariable
    integer(i4) :: tmpid

    hasVariable = (nf90_inq_varid(self%id, name, tmpid) == NF90_NOERR)
  end function hasVariable

  function hasDimension(self, name)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    logical :: hasDimension
    integer(i4) :: tmpid

    hasDimension = (nf90_inq_dimid(self%id, name, tmpid) == NF90_NOERR)
  end function hasDimension

  function hasGroup(self, name)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    logical :: hasGroup
    integer(i4) :: tmpid

    hasGroup = (nf90_inq_ncid(self%id, name, tmpid) == NF90_NOERR)
  end function hasGroup

  function setVariableWithIds(self, name, dtype, dimensions, contiguous, &
          chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    character(*), intent(in) :: dtype
    integer(i4), intent(in) :: dimensions(:)
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: endianness, deflate_level, cache_size, &
            cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable) :: setVariableWithIds
    integer(i4) :: varid, status

    status = nf90_def_var(self%id, name, getDtypeFromString(dtype), dimensions, varid, contiguous, &
            chunksizes, deflate_level, shuffle, fletcher32, endianness, &
            cache_size, cache_nelems, cache_preemption)
    call check(status, "Failed to create variable: " // name)
    setVariableWithIds = NcVariable(varid, self)
  end function setVariableWithIds

  function setVariableWithNames(self, name, dtype, dimensions, contiguous, &
          chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)

    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    character(*), intent(in) :: dtype
    character(*), intent(in) :: dimensions(:)
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: endianness, deflate_level, cache_size, &
            cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable) :: setVariableWithNames
    type(NcDimension) :: dim
    integer(i4) :: i, dimids(size(dimensions))

    do i = 1, size(dimensions)
      dim = self%getDimension(dimensions(i))
      dimids(i) = dim%id
    end do

    setVariableWithNames = setVariableWithIds(self, name, dtype, dimids, contiguous, &
            chunksizes, deflate_level, shuffle, fletcher32, endianness, &
            cache_size, cache_nelems, cache_preemption)
  end function setVariableWithNames

  function setVariableWithTypes(self, name, dtype, dimensions, contiguous, &
          chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    character(*), intent(in) :: dtype
    type(NcDimension), intent(in) :: dimensions(:)
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: endianness, deflate_level, cache_size, &
            cache_nelems, cache_preemption, chunksizes(:)
    type(NcVariable) :: setVariableWithTypes
    type(NcDimension) :: dim
    integer(i4) :: i, dimids(size(dimensions))

    do i = 1, size(dimensions)
      dim = dimensions(i)
      dimids(i) = dim%id
    end do

    setVariableWithTypes = setVariableWithIds(self, name, dtype, dimids, contiguous, &
            chunksizes, deflate_level, shuffle, fletcher32, endianness, &
            cache_size, cache_nelems, cache_preemption)
  end function setVariableWithTypes

  function getDimensionById(self, id)
    class(NcGroup), intent(in) :: self
    integer(i4) :: id
    type(NcDimension) :: getDimensionById
    character(32) :: msg, name

    write(msg, *) id
    call check(nf90_inquire_dimension(self%id, id, name), &
            "Could not inquire dimension: " // msg)
    getDimensionById = NcDimension(id, self)
  end function getDimensionById

  function getDimensionByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*) :: name
    type(NcDimension) :: getDimensionByName
    integer(i4) :: id

    call check(nf90_inq_dimid(self%id, name, id), &
            "Could not inquire dimension: " // name)
    getDimensionByName = self%getDimensionById(id)
  end function getDimensionByName

  function getGroupByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    type(NcGroup) :: getGroupByName
    integer(i4) :: id

    call check(nf90_inq_ncid(self%id, name, id), &
            "Could not inquire variable: " // name)
    getGroupByName = NcGroup(id)
  end function getGroupByName

  function getVariableByName(self, name)
    class(NcGroup), intent(in) :: self
    character(*), intent(in) :: name
    type(NcVariable) :: getVariableByName
    integer(i4) :: id

    call check(nf90_inq_varid(self%id, name, id), &
            "Could not inquire variable: " // name)
    getVariableByName = NcVariable(id, self)

  end function getVariableByName

  function getVariableName(self)
    class(NcVariable), intent(in) :: self
    character(len = 256) :: getVariableName

    call check(nf90_inquire_variable(self%parent%id, self%id, name = getVariableName), &
            "Could not inquire variable name")
  end function getVariableName

  function getNoDimensions(self)
    class(NcVariable), intent(in) :: self
    integer(i4) :: getNoDimensions

    call check(nf90_inquire_variable(self%parent%id, self%id, ndims = getNoDimensions), &
            "Could not inquire variable: " // self%getName())
  end function getNoDimensions

  function getVariableDimensions(self)
    class(NcVariable), intent(in) :: self
    type(NcDimension), allocatable :: getVariableDimensions(:)
    integer(i4), allocatable :: dimids(:)
    integer(i4) :: ii, ndims

    ndims = self%getNoDimensions()
    allocate(dimids(ndims), getVariableDimensions(ndims))
    call check(nf90_inquire_variable(self%parent%id, self%id, dimids = dimids), &
            "Could not inquire variable: " // self%getName())

    do ii = 1, ndims
      getVariableDimensions (ii) = self%parent%getDimension(dimids(ii))
    end do
  end function getVariableDimensions

  function getVariableShape(self)
    class(NcVariable), intent(in) :: self
    integer(i4), allocatable :: getVariableShape(:)
    type(NcDimension), allocatable :: dims(:)
    integer(i4) :: ii, ndims

    ndims = self%getNoDimensions()
    allocate(getVariableShape(ndims), dims(ndims))

    dims = self%getDimensions()
    do ii = 1, size(dims)
      getVariableShape(ii) = dims(ii)%getLength()
    end do
  end function getVariableShape

  function getVariableRank(self)
    class(NcVariable), intent(in) :: self
    integer(i4) :: getVariableRank

    getVariableRank = size(self%getDimensions())
  end function getVariableRank

  function getVariableDtype(self)
    class(NcVariable), intent(in) :: self
    integer(i4) :: dtype
    character(4) :: getVariableDtype

    call check(nf90_inquire_variable(self%parent%id, self%id, xtype = dtype), &
            "Could not inquire variable: " // self%getName())
    getVariableDtype = getDtypeFromInteger(dtype)
  end function getVariableDtype

  function isUnlimitedVariable(self)
    class(NcVariable), intent(in) :: self
    logical :: isUnlimitedVariable
    type(NcDimension), allocatable :: dims(:)
    type(NcDimension) :: dim
    integer(i4) :: ii

    allocate(dims(self%getNoDimensions()))

    isUnlimitedVariable = .false.
    dims = self%getDimensions()

    do ii = 1, size(dims)
      dim = dims(ii)
      if (dim%isUnlimited()) then
        isUnlimitedVariable = .true.
      end if
    end do
  end function isUnlimitedVariable

  logical function hasAttribute(self, name, xtype, len, attnum)
    class(NcAttributable), intent(in) :: self
    character(*), intent(in) :: name
    integer(i4), intent(out), optional :: xtype
    integer(i4), intent(out), optional :: len
    integer(i4), intent(out), optional :: attnum
    integer(i4) :: status

    select type (self)
    class is (NcGroup)
      status = nf90_inquire_attribute(self%id, NF90_GLOBAL, name, xtype=xtype, len=len, attnum=attnum)
    class is (NcVariable)
      status = nf90_inquire_attribute(self%parent%id, self%id, name, xtype=xtype, len=len, attnum=attnum)
    end select

    hasAttribute = (status == NF90_NOERR)
  end function hasAttribute

  function getAttributeNames(self) result(attributeNames)
    class(NcAttributable), intent(in) :: self
    character(256), dimension(:), allocatable :: attributeNames

    integer(i4), parameter :: maxNames = 100_i4
    integer(i4) :: nAtts
    integer(i4) :: status

    ! assume a maximum number of 100 attributes that are checked
    allocate(attributeNames(maxNames))
    nAtts = 0_i4
    do while (nAtts < maxNames)
      select type (self)
      class is (NcGroup)
        status = nf90_inq_attname(self%id, NF90_GLOBAL, nAtts + 1_i4, attributeNames(nAtts + 1_i4))
      class is (NcVariable)
        status = nf90_inq_attname(self%parent%id, self%id, nAtts + 1_i4, attributeNames(nAtts + 1_i4))
      end select
      ! if the status is negative, exit loop, else increase counter
      if (status /= NF90_NOERR) then
        exit
      else
        nAtts = nAtts + 1_i4
      end if
    end do
    ! select only valid names
    attributeNames = attributeNames(1:nAtts)

  end function getAttributeNames



  subroutine setAttribute_0d_sp(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_sp

  subroutine getAttribute_0d_sp(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_sp

  subroutine setAttribute_1d_sp(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_sp

  subroutine getAttribute_1d_sp(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(sp), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_sp

  subroutine setAttribute_0d_dp(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_dp

  subroutine getAttribute_0d_dp(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_dp

  subroutine setAttribute_1d_dp(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_dp

  subroutine getAttribute_1d_dp(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_dp

  subroutine setAttribute_0d_i1(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i1), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_i1

  subroutine getAttribute_0d_i1(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i1), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_i1

  subroutine setAttribute_1d_i1(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i1), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_i1

  subroutine getAttribute_1d_i1(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i1), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_i1

  subroutine setAttribute_0d_i2(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i2), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_i2

  subroutine getAttribute_0d_i2(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i2), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_i2

  subroutine setAttribute_1d_i2(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i2), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_i2

  subroutine getAttribute_1d_i2(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i2), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_i2

  subroutine setAttribute_0d_i4(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i4), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_i4

  subroutine getAttribute_0d_i4(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i4), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_i4

  subroutine setAttribute_1d_i4(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i4), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_i4

  subroutine getAttribute_1d_i4(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i4), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_i4

  subroutine setAttribute_0d_i8(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_i8

  subroutine getAttribute_0d_i8(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_i8

  subroutine setAttribute_1d_i8(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(in) :: data(:)
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_1d_i8

  subroutine getAttribute_1d_i8(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    integer(i8), intent(out) :: avalue(:)
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_1d_i8

  subroutine setAttribute_0d_char(self, name, data)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: data
    integer(i4) :: ids(2)

    ids = self%getAttributableIds()
    call check(nf90_put_att(ids(1), ids(2), name, data), &
            "Failed to write attribute: " // name)

  end subroutine setAttribute_0d_char

  subroutine getAttribute_0d_char(self, name, avalue)
    class(NcAttributable), intent(in) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: avalue
    integer(i4) :: length, ids(2)

    ids = self%getAttributableIds()
    call check(nf90_inquire_attribute(ids(1), ids(2), name, len = length), &
            "Could not inquire attribute " // name)
    call check(nf90_get_att(ids(1), ids(2), name, avalue), &
            "Could not read attribute " // name)

  end subroutine getAttribute_0d_char


  function getAttributableIds(self)
    class(NcAttributable), intent(in) :: self
    integer(i4) :: getAttributableIds(2)
    select type(self)
    class is (NcGroup)
      getAttributableIds(1) = self%id
      getAttributableIds(2) = NF90_GLOBAL
    class is (NcVariable)
      getAttributableIds(1) = self%parent%id
      getAttributableIds(2) = self%id
    end select
  end function getAttributableIds

  subroutine renameAttribute(self, oldname, newname)
    class(NcAttributable), intent(inout) :: self
    character(len = *), intent(in) :: oldname, newname
    integer(i4) :: ids(2)
    ids = self%getAttributableIds()
    call check(nf90_rename_att(ids(1), ids(2), oldname, newname), "Failed to rename attribute: " // oldname)
  end subroutine renameAttribute

  subroutine renameVariable(self, name)
    class(NcVariable), intent(inout) :: self
    character(len = *), intent(in) :: name
    call check(nf90_rename_var(self%parent%id, self%id, name), "Failed to rename variable: " // self%getName())
  end subroutine renameVariable

  subroutine renameDimension(self, name)
    class(NcDimension), intent(inout) :: self
    character(len = *), intent(in) :: name
    call check(nf90_rename_dim(self%parent%id, self%id, name), "Failed to rename dimension: " // self%getName())
  end subroutine renameDimension






  subroutine getData_0d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    real(sp) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_0d_sp

  subroutine setData_0d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_sp

  subroutine getData_1d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_1d_sp

  subroutine setData_1d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_sp

  subroutine getData_2d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_2d_sp

  subroutine setData_2d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_sp

  subroutine getData_3d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_3d_sp

  subroutine setData_3d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_sp

  subroutine getData_4d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_4d_sp

  subroutine setData_4d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_sp

  subroutine getData_5d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_5d_sp

  subroutine setData_5d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_sp

  subroutine getData_6d_sp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    real(sp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_6d_sp

  subroutine setData_6d_sp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(sp), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_sp

  subroutine getVariableFillValue_sp(self, fvalue)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_FLOAT
    end if

  end subroutine getVariableFillValue_sp

  subroutine getCFAttributes_sp(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    real(sp), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    real(sp) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (ieee_is_nan(fillValue)) then
      flagMissing = CF_USE_NAN
    end if
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_sp

  subroutine setVariableFillValue_sp(self, fvalue)
    class(NcVariable), intent(inout) :: self
    real(sp), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_sp

  subroutine getData_0d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    real(dp) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_0d_dp

  subroutine setData_0d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_dp

  subroutine getData_1d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_1d_dp

  subroutine setData_1d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_dp

  subroutine getData_2d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_2d_dp

  subroutine setData_2d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_dp

  subroutine getData_3d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_3d_dp

  subroutine setData_3d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_dp

  subroutine getData_4d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_4d_dp

  subroutine setData_4d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_dp

  subroutine getData_5d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_5d_dp

  subroutine setData_5d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_dp

  subroutine getData_6d_dp(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    real(dp) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = ne(data, fillValue)
      case(CF_USE_NAN)
        mask = .not. ieee_is_nan(data)
      case(CF_USE_VALID_MIN)
        mask = data > minValue
      case(CF_USE_VALID_MAX)
        mask = data < maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data < maxValue) .and. (data > minValue)
      end select
    end if

  end subroutine getData_6d_dp

  subroutine setData_6d_dp(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    real(dp), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_dp

  subroutine getVariableFillValue_dp(self, fvalue)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_DOUBLE
    end if

  end subroutine getVariableFillValue_dp

  subroutine getCFAttributes_dp(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    real(dp), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    real(dp) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (ieee_is_nan(fillValue)) then
      flagMissing = CF_USE_NAN
    end if
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_dp

  subroutine setVariableFillValue_dp(self, fvalue)
    class(NcVariable), intent(inout) :: self
    real(dp), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_dp

  subroutine getData_0d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i1) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_0d_i1

  subroutine setData_0d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_i1

  subroutine getData_1d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_1d_i1

  subroutine setData_1d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_i1

  subroutine getData_2d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_2d_i1

  subroutine setData_2d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_i1

  subroutine getData_3d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_3d_i1

  subroutine setData_3d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_i1

  subroutine getData_4d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_4d_i1

  subroutine setData_4d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_i1

  subroutine getData_5d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_5d_i1

  subroutine setData_5d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_i1

  subroutine getData_6d_i1(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i1) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_6d_i1

  subroutine setData_6d_i1(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_i1

  subroutine getVariableFillValue_i1(self, fvalue)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_BYTE
    end if

  end subroutine getVariableFillValue_i1

  subroutine getCFAttributes_i1(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    integer(i1), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    integer(i1) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_i1

  subroutine setVariableFillValue_i1(self, fvalue)
    class(NcVariable), intent(inout) :: self
    integer(i1), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_i1

  subroutine getData_0d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i2) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_0d_i2

  subroutine setData_0d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_i2

  subroutine getData_1d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_1d_i2

  subroutine setData_1d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_i2

  subroutine getData_2d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_2d_i2

  subroutine setData_2d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_i2

  subroutine getData_3d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_3d_i2

  subroutine setData_3d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_i2

  subroutine getData_4d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_4d_i2

  subroutine setData_4d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_i2

  subroutine getData_5d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_5d_i2

  subroutine setData_5d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_i2

  subroutine getData_6d_i2(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i2) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_6d_i2

  subroutine setData_6d_i2(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_i2

  subroutine getVariableFillValue_i2(self, fvalue)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_SHORT
    end if

  end subroutine getVariableFillValue_i2

  subroutine getCFAttributes_i2(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    integer(i2), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    integer(i2) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_i2

  subroutine setVariableFillValue_i2(self, fvalue)
    class(NcVariable), intent(inout) :: self
    integer(i2), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_i2

  subroutine getData_0d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_0d_i4

  subroutine setData_0d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_i4

  subroutine getData_1d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_1d_i4

  subroutine setData_1d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_i4

  subroutine getData_2d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_2d_i4

  subroutine setData_2d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_i4

  subroutine getData_3d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_3d_i4

  subroutine setData_3d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_i4

  subroutine getData_4d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_4d_i4

  subroutine setData_4d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_i4

  subroutine getData_5d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_5d_i4

  subroutine setData_5d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_i4

  subroutine getData_6d_i4(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i4) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_6d_i4

  subroutine setData_6d_i4(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_i4

  subroutine getVariableFillValue_i4(self, fvalue)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_INT
    end if

  end subroutine getVariableFillValue_i4

  subroutine getCFAttributes_i4(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    integer(i4) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_i4

  subroutine setVariableFillValue_i4(self, fvalue)
    class(NcVariable), intent(inout) :: self
    integer(i4), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_i4

  subroutine getData_0d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i8) :: tmp(1)

    call check (nf90_get_var(self%parent%id, self%id, tmp, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    data = tmp(1)
    if (present(mask)) then
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_0d_i8

  subroutine setData_0d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_0d_i8

  subroutine getData_1d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_1d_i8

  subroutine setData_1d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_1d_i8

  subroutine getData_2d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_2d_i8

  subroutine setData_2d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_2d_i8

  subroutine getData_3d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_3d_i8

  subroutine setData_3d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_3d_i8

  subroutine getData_4d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_4d_i8

  subroutine setData_4d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_4d_i8

  subroutine getData_5d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_5d_i8

  subroutine setData_5d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_5d_i8

  subroutine getData_6d_i8(self, data, start, cnt, stride, map, mask)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out), allocatable :: data(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    logical, intent(out), allocatable, optional :: mask(:,:,:,:,:,:)

    integer(i4) :: flagMissing
    integer(i8) :: fillValue, minValue, maxValue
    integer(i4), allocatable :: slcshape(:), datashape(:)

    slcshape = self%getSlicingShape(start, cnt, stride)
    datashape = getReadShape(slcshape, size(shape(data)))
      allocate(data(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
    call check (nf90_get_var(self%parent%id, self%id, data, start, cnt, stride, map), &
            "Could not read data from variable: " // trim(self%getName()))
    if (present(mask)) then
      allocate(mask(datashape(1), datashape(2), datashape(3), datashape(4), datashape(5), datashape(6)))
      mask =.true.
      call self%getCFAttributes(minValue, maxValue, fillValue, flagMissing)
      select case(flagMissing)
      case(CF_USE_FILL_VALUE)
        mask = .not. (data == fillValue)
      case(CF_USE_VALID_MIN)
        mask = data >= minValue
      case(CF_USE_VALID_MAX)
        mask = data <= maxValue
      case(CF_USE_VALID_RANGE)
        mask = (data <= maxValue) .and. (data >= minValue)
      end select
    end if

  end subroutine getData_6d_i8

  subroutine setData_6d_i8(self, values, start, cnt, stride, map)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(in) :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    call check(nf90_put_var(self%parent%id, self%id, values, start, cnt, stride, map), &
            "Failed to write data into variable: " // trim(self%getName()))

  end subroutine setData_6d_i8

  subroutine getVariableFillValue_i8(self, fvalue)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out) :: fvalue

    if (self%hasAttribute(CF_FILL_VALUE)) then
      call self%getAttribute(CF_FILL_VALUE, fvalue)
    else
      fvalue = NF90_FILL_INT
    end if

  end subroutine getVariableFillValue_i8

  subroutine getCFAttributes_i8(self, minValue, maxValue, fillValue, flagMissing)
    class(NcVariable), intent(in) :: self
    integer(i8), intent(out) :: minValue, maxValue, fillValue
    integer(i4), intent(out) :: flagMissing

    integer(i8) :: valid_range(2)

    flagMissing = CF_USE_FILL_VALUE
    call self%getFillValue(fillValue)
    if (self%hasAttribute(CF_VALID_RANGE)) then
      flagMissing = CF_USE_VALID_RANGE
      call self%getAttribute(CF_VALID_RANGE, valid_range)
      minValue = valid_range(1)
      maxValue = valid_range(2)
    else if (self%hasAttribute(CF_VALID_MIN)) then
      flagMissing = CF_USE_VALID_MIN
      call self%getAttribute(CF_VALID_MIN, minValue)
    else if (self%hasAttribute(CF_VALID_MAX)) then
      flagMissing = CF_USE_VALID_MAX
      call self%getAttribute(CF_VALID_MAX, maxValue)
    end if

  end subroutine getCFAttributes_i8

  subroutine setVariableFillValue_i8(self, fvalue)
    class(NcVariable), intent(inout) :: self
    integer(i8), intent(in) :: fvalue

    if (.not. self%hasAttribute(CF_FILL_VALUE)) then
      call self%setAttribute(CF_FILL_VALUE, fvalue)
    end if

  end subroutine setVariableFillValue_i8


  function getSlicingShape(self, instart, incnt, instride) result(out)
    class(NcVariable), intent(in) :: self
    integer(i4), intent(in), optional :: instart(:), incnt(:), instride(:)
    integer(i4), allocatable :: out(:)

    out = self%getShape()

    if (present(incnt)) then
      out(:size(incnt)) = incnt
      ! out = incnt
    else
      if (present(instart)) then
        out(:size(instart)) = out(:size(instart)) - (instart - 1)
      end if
      if (present(instride)) then
        out(:size(instride)) = out(:size(instride)) / instride
      end if
    end if

  end function getSlicingShape

  function getReadShape(slcshape, outrank) result(out)
    integer(i4), intent(in) :: slcshape(:)
    integer(i4), intent(in) :: outrank
    integer(i4) :: naxis
    integer(i4), allocatable :: out(:)

    naxis = count(slcshape > 1)

    if (all(slcshape == 1)) then
      ! return 1-element array
      allocate(out(size(slcshape)))
      out(:) = 1
    else if (size(slcshape) == outrank) then
      ! sizes fit
      out = slcshape
    else if (naxis == outrank) then
      out = pack(slcshape, slcshape > 1)
      ! else if (naxis .lt. outrank) then
      ! would be nice...
    else
      write(*, *) "Given indices do not match output variable rank!"
      stop 1
    end if
  end function getReadShape

  function getDtypeFromString(dtype)
    integer(i4) :: getDtypeFromString
    character(*)         :: dtype

    select case(dtype)
    case("f32")
       getDtypeFromString = NF90_FLOAT
    case("f64")
       getDtypeFromString = NF90_DOUBLE
    case("i8")
       getDtypeFromString = NF90_BYTE
    case("i16")
       getDtypeFromString = NF90_SHORT
    case("i32")
       getDtypeFromString = NF90_INT
    case("i64")
       getDtypeFromString = NF90_INT64
    case("char")
       getDtypeFromString = NF90_CHAR
    case default
       write(*,*) "Datatype not understood: ", dtype
       stop 1
    end select
  end function getDtypeFromString

  function getDtypeFromInteger(dtype)
    character(4) :: getDtypeFromInteger
    integer(i4) :: dtype

    select case(dtype)
    case(NF90_FLOAT)
       getDtypeFromInteger = "f32"
    case(NF90_DOUBLE)
       getDtypeFromInteger = "f64"
    case(NF90_BYTE)
       getDtypeFromInteger = "i8"
    case(NF90_SHORT)
       getDtypeFromInteger = "i16"
    case(NF90_INT)
       getDtypeFromInteger = "i32"
    case(NF90_INT64)
       getDtypeFromInteger = "i64"
    case(NF90_CHAR)
       getDtypeFromInteger = "char"
    case default
       write(*,*) "Datatype not understood: ", dtype
       stop 1
    end select
  end function getDtypeFromInteger

  function getCreationMode(cmode)
    character(*), intent(in), optional :: cmode
    integer(i4) :: getCreationMode
    character(256) :: mode

    if (.not. (present(cmode))) then
      mode = "NETCDF4"
    else
      mode = cmode
    end if

    select case(trim(mode))
    case ("NETCDF4")
      getCreationMode = NF90_NETCDF4
    case ("SHARE")
      getCreationMode = NF90_SHARE
    case ("CLASSIC")
      getCreationMode = NF90_CLASSIC_MODEL
    case ("HDF5")
      getCreationMode = NF90_HDF5
    case ("64BIT_OFFSET")
      getCreationMode = NF90_64BIT_OFFSET
    case default
      print*, "Creation mode not understood: " // trim(mode)
      stop 1
    end select

  end function getCreationMode

  subroutine check(status, msg)
    integer(i4), intent(in) :: status
    character(*), intent(in) :: msg

    if (status /= NF90_NOERR) then
      write(*, *) msg
      write(*, *) nf90_strerror(status)
      stop 1
    end if
  end subroutine check

end module mo_netcdf