!> \file mo_netcdf_wrapper.f90
!> \copydoc mo_netcdf_wrapper



!> \brief Thin FORCES-owned NetCDF wrapper on top of netcdf-c.
!> \details This module exposes the subset of the nf90-style API used inside FORCES,
!! but implements it directly on top of the C library. Public callers should use the
!! `ncw_*` procedures and constants defined here rather than importing `netcdf`.
module mo_netcdf_wrapper

  use, intrinsic :: iso_c_binding, only : &
          c_char, c_double, c_float, c_int, c_loc, c_long_long, c_null_char, c_null_ptr, &
          c_ptr, c_ptrdiff_t, c_short, c_signed_char, c_size_t
  use mo_kind, only : i1, i2, i4, i8, sp, dp

  implicit none

  private

  integer(i4), parameter, public :: NCW_OPEN_MODE = 0_i4
  integer(i4), parameter, public :: NCW_CREATE_MODE = 0_i4
  integer(i4), parameter, public :: NCW_NOWRITE = int(z'0000', i4)
  integer(i4), parameter, public :: NCW_WRITE = int(z'0001', i4)
  integer(i4), parameter, public :: NCW_CLOBBER = int(z'0000', i4)
  integer(i4), parameter, public :: NCW_CLASSIC_MODEL = int(z'0100', i4)
  integer(i4), parameter, public :: NCW_64BIT_OFFSET = int(z'0200', i4)
  integer(i4), parameter, public :: NCW_SHARE = int(z'0800', i4)
  integer(i4), parameter, public :: NCW_NETCDF4 = int(z'1000', i4)
  integer(i4), parameter, public :: NCW_HDF5 = NCW_NETCDF4

  integer(i4), parameter, public :: NCW_BYTE = 1_i4
  integer(i4), parameter, public :: NCW_CHAR = 2_i4
  integer(i4), parameter, public :: NCW_SHORT = 3_i4
  integer(i4), parameter, public :: NCW_INT = 4_i4
  integer(i4), parameter, public :: NCW_FLOAT = 5_i4
  integer(i4), parameter, public :: NCW_DOUBLE = 6_i4
  integer(i4), parameter, public :: NCW_INT64 = 10_i4

  integer(i4), parameter, public :: NCW_NOERR = 0_i4
  integer(i4), parameter, public :: NCW_EINVAL = -36_i4
  integer(i4), parameter, public :: NCW_EEDGE = -57_i4
  integer(i4), parameter, public :: NCW_ESTRIDE = -58_i4
  integer(i4), parameter, public :: NCW_ERANGE = -60_i4
  integer(i4), parameter, public :: NCW_UNLIMITED = 0_i4
  integer(i4), parameter, public :: NCW_GLOBAL = -1_i4
  integer(i4), parameter, public :: NCW_MAX_NAME = 256_i4
  integer(i4), parameter, public :: NCW_MAX_VAR_DIMS = 1024_i4
  integer(i4), parameter, public :: NCW_FILL = 0_i4
  integer(i4), parameter, public :: NCW_ENDIAN_NATIVE = 0_i4
  integer(i4), parameter, public :: NCW_ENDIAN_LITTLE = 1_i4
  integer(i4), parameter, public :: NCW_ENDIAN_BIG = 2_i4
  integer(i4), parameter, public :: NCW_CHUNKED = 0_i4
  integer(i4), parameter, public :: NCW_NOTCONTIGUOUS = 0_i4
  integer(i4), parameter, public :: NCW_CONTIGUOUS = 1_i4
  integer(i4), parameter, public :: NCW_NOFILL = int(z'0100', i4)
  integer(i4), parameter, public :: NCW_FORMAT_CLASSIC = 1_i4
  integer(i4), parameter, public :: NCW_FORMAT_64BIT_OFFSET = 2_i4
  integer(i4), parameter, public :: NCW_FORMAT_NETCDF4 = 3_i4
  integer(i4), parameter, public :: NCW_FORMAT_NETCDF4_CLASSIC = 4_i4
  integer(i4), parameter, public :: NCW_FORMAT_CDF5 = 5_i4

  integer(i1), parameter, public :: NCW_FILL_BYTE = -127_i1
  integer(i2), parameter, public :: NCW_FILL_SHORT = -32767_i2
  integer(i4), parameter, public :: NCW_FILL_INT = -2147483647_i4
  real(sp), parameter, public :: NCW_FILL_FLOAT = 9.9692099683868690e+36_sp
  real(dp), parameter, public :: NCW_FILL_DOUBLE = 9.9692099683868690e+36_dp

  public :: ncw_abort, ncw_close, ncw_copy_att, ncw_create, ncw_def_dim, ncw_def_dim64, ncw_def_grp, ncw_def_var
  public :: ncw_def_var_fill, ncw_del_att, ncw_enddef, ncw_get_att, ncw_get_var, ncw_get_var64, ncw_inq_attname
  public :: ncw_inq_dimid, ncw_inq_format, ncw_inq_grp_parent, ncw_inq_grpname, ncw_inq_ncid, ncw_inq_path
  public :: ncw_inq_var_chunking, ncw_inq_var_deflate, ncw_inq_var_endian, ncw_inq_var_fletcher32, ncw_inq_var_fill
  public :: ncw_inq_varid, ncw_inq_varids, ncw_inquire, ncw_inquire_attribute, ncw_inquire_attribute64
  public :: ncw_inquire_dimension, ncw_inquire_dimension64, ncw_inquire_variable, ncw_open, ncw_put_att, ncw_put_var
  public :: ncw_put_var64, ncw_redef, ncw_rename_att, ncw_rename_dim, ncw_rename_grp, ncw_rename_var, ncw_set_fill
  public :: ncw_strerror, ncw_sync

  interface
    subroutine forces_nc_strerror_copy(status, buffer, buffer_len) bind(C, name="forces_nc_strerror_copy")
      import :: c_char, c_int, c_size_t
      integer(c_int), value :: status
      character(kind=c_char, len=1), intent(out) :: buffer(*)
      integer(c_size_t), value :: buffer_len
    end subroutine forces_nc_strerror_copy

    integer(c_int) function c_nc_open(path, mode, ncidp) bind(C, name="nc_open")
      import :: c_int, c_ptr
      type(c_ptr), value :: path, ncidp
      integer(c_int), value :: mode
    end function c_nc_open

    integer(c_int) function c_nc__open(path, mode, chunksizehintp, ncidp) bind(C, name="nc__open")
      import :: c_int, c_ptr
      type(c_ptr), value :: path, chunksizehintp, ncidp
      integer(c_int), value :: mode
    end function c_nc__open

    integer(c_int) function c_nc_create(path, cmode, ncidp) bind(C, name="nc_create")
      import :: c_int, c_ptr
      type(c_ptr), value :: path, ncidp
      integer(c_int), value :: cmode
    end function c_nc_create

    integer(c_int) function c_nc__create(path, cmode, initialsz, chunksizehintp, ncidp) bind(C, name="nc__create")
      import :: c_int, c_ptr, c_size_t
      type(c_ptr), value :: path, chunksizehintp, ncidp
      integer(c_int), value :: cmode
      integer(c_size_t), value :: initialsz
    end function c_nc__create

    integer(c_int) function c_nc_close(ncid) bind(C, name="nc_close")
      import :: c_int
      integer(c_int), value :: ncid
    end function c_nc_close

    integer(c_int) function c_nc_sync(ncid) bind(C, name="nc_sync")
      import :: c_int
      integer(c_int), value :: ncid
    end function c_nc_sync

    integer(c_int) function c_nc_redef(ncid) bind(C, name="nc_redef")
      import :: c_int
      integer(c_int), value :: ncid
    end function c_nc_redef

    integer(c_int) function c_nc_enddef(ncid) bind(C, name="nc_enddef")
      import :: c_int
      integer(c_int), value :: ncid
    end function c_nc_enddef

    integer(c_int) function c_nc_abort(ncid) bind(C, name="nc_abort")
      import :: c_int
      integer(c_int), value :: ncid
    end function c_nc_abort

    integer(c_int) function c_nc_set_fill(ncid, fillmode, old_modep) bind(C, name="nc_set_fill")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, fillmode
      type(c_ptr), value :: old_modep
    end function c_nc_set_fill

    integer(c_int) function c_nc_get_chunk_cache(sizep, nelemsp, preemptionp) bind(C, name="nc_get_chunk_cache")
      import :: c_int, c_ptr
      type(c_ptr), value :: sizep, nelemsp, preemptionp
    end function c_nc_get_chunk_cache

    integer(c_int) function c_nc_set_chunk_cache(size, nelems, preemption) bind(C, name="nc_set_chunk_cache")
      import :: c_float, c_int, c_size_t
      integer(c_size_t), value :: size, nelems
      real(c_float), value :: preemption
    end function c_nc_set_chunk_cache

    integer(c_int) function c_nc_inq(ncid, ndimsp, nvarsp, nattsp, unlimdimidp) bind(C, name="nc_inq")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: ndimsp, nvarsp, nattsp, unlimdimidp
    end function c_nc_inq

    integer(c_int) function c_nc_inq_path(ncid, pathlenp, path) bind(C, name="nc_inq_path")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: pathlenp, path
    end function c_nc_inq_path

    integer(c_int) function c_nc_inq_format(ncid, formatp) bind(C, name="nc_inq_format")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: formatp
    end function c_nc_inq_format

    integer(c_int) function c_nc_def_dim(ncid, name, len, idp) bind(C, name="nc_def_dim")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid
      type(c_ptr), value :: name, idp
      integer(c_size_t), value :: len
    end function c_nc_def_dim

    integer(c_int) function c_nc_inq_dim(ncid, dimid, name, lenp) bind(C, name="nc_inq_dim")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, dimid
      type(c_ptr), value :: name, lenp
    end function c_nc_inq_dim

    integer(c_int) function c_nc_inq_dimid(ncid, name, idp) bind(C, name="nc_inq_dimid")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: name, idp
    end function c_nc_inq_dimid

    integer(c_int) function c_nc_rename_dim(ncid, dimid, name) bind(C, name="nc_rename_dim")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, dimid
      type(c_ptr), value :: name
    end function c_nc_rename_dim

    integer(c_int) function c_nc_def_grp(parent_ncid, name, new_ncidp) bind(C, name="nc_def_grp")
      import :: c_int, c_ptr
      integer(c_int), value :: parent_ncid
      type(c_ptr), value :: name, new_ncidp
    end function c_nc_def_grp

    integer(c_int) function c_nc_inq_ncid(ncid, name, grp_ncidp) bind(C, name="nc_inq_ncid")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: name, grp_ncidp
    end function c_nc_inq_ncid

    integer(c_int) function c_nc_inq_grp_parent(ncid, parent_ncidp) bind(C, name="nc_inq_grp_parent")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: parent_ncidp
    end function c_nc_inq_grp_parent

    integer(c_int) function c_nc_inq_grpname(ncid, name) bind(C, name="nc_inq_grpname")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: name
    end function c_nc_inq_grpname

    integer(c_int) function c_nc_rename_grp(grpid, name) bind(C, name="nc_rename_grp")
      import :: c_int, c_ptr
      integer(c_int), value :: grpid
      type(c_ptr), value :: name
    end function c_nc_rename_grp

    integer(c_int) function c_nc_inq_varids(ncid, nvarsp, varidsp) bind(C, name="nc_inq_varids")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: nvarsp, varidsp
    end function c_nc_inq_varids

    integer(c_int) function c_nc_def_var(ncid, name, xtype, ndims, dimidsp, varidp) bind(C, name="nc_def_var")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, xtype, ndims
      type(c_ptr), value :: name, dimidsp, varidp
    end function c_nc_def_var

    integer(c_int) function c_nc_def_var_chunking(ncid, varid, storage, chunksizesp) bind(C, name="nc_def_var_chunking")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid, storage
      type(c_ptr), value :: chunksizesp
    end function c_nc_def_var_chunking

    integer(c_int) function c_nc_def_var_deflate(ncid, varid, shuffle, deflate, deflate_level) bind(C, name="nc_def_var_deflate")
      import :: c_int
      integer(c_int), value :: ncid, varid, shuffle, deflate, deflate_level
    end function c_nc_def_var_deflate

    integer(c_int) function c_nc_def_var_fletcher32(ncid, varid, fletcher32) bind(C, name="nc_def_var_fletcher32")
      import :: c_int
      integer(c_int), value :: ncid, varid, fletcher32
    end function c_nc_def_var_fletcher32

    integer(c_int) function c_nc_def_var_endian(ncid, varid, endianness) bind(C, name="nc_def_var_endian")
      import :: c_int
      integer(c_int), value :: ncid, varid, endianness
    end function c_nc_def_var_endian

    integer(c_int) function c_nc_inq_var_chunking(ncid, varid, storagep, chunksizesp) bind(C, name="nc_inq_var_chunking")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: storagep, chunksizesp
    end function c_nc_inq_var_chunking

    integer(c_int) function c_nc_inq_var_deflate(ncid, varid, shufflep, deflatep, deflate_levelp) bind(C, name="nc_inq_var_deflate")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: shufflep, deflatep, deflate_levelp
    end function c_nc_inq_var_deflate

    integer(c_int) function c_nc_inq_var_fletcher32(ncid, varid, fletcher32p) bind(C, name="nc_inq_var_fletcher32")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: fletcher32p
    end function c_nc_inq_var_fletcher32

    integer(c_int) function c_nc_def_var_fill(ncid, varid, no_fill, fill_valuep) bind(C, name="nc_def_var_fill")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid, no_fill
      type(c_ptr), value :: fill_valuep
    end function c_nc_def_var_fill

    integer(c_int) function c_nc_inq_var_fill(ncid, varid, no_fillp, fill_valuep) bind(C, name="nc_inq_var_fill")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: no_fillp, fill_valuep
    end function c_nc_inq_var_fill

    integer(c_int) function c_nc_inq_var_endian(ncid, varid, endianp) bind(C, name="nc_inq_var_endian")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: endianp
    end function c_nc_inq_var_endian

    integer(c_int) function c_nc_get_var_chunk_cache(ncid, varid, sizep, nelemsp, preemptionp) bind(C, name="nc_get_var_chunk_cache")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: sizep, nelemsp, preemptionp
    end function c_nc_get_var_chunk_cache

    integer(c_int) function c_nc_set_var_chunk_cache(ncid, varid, size, nelems, preemption) bind(C, name="nc_set_var_chunk_cache")
      import :: c_float, c_int, c_size_t
      integer(c_int), value :: ncid, varid
      integer(c_size_t), value :: size, nelems
      real(c_float), value :: preemption
    end function c_nc_set_var_chunk_cache

    integer(c_int) function c_nc_inq_var(ncid, varid, name, xtypep, ndimsp, dimidsp, nattsp) bind(C, name="nc_inq_var")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, xtypep, ndimsp, dimidsp, nattsp
    end function c_nc_inq_var

    integer(c_int) function c_nc_inq_varid(ncid, name, varidp) bind(C, name="nc_inq_varid")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid
      type(c_ptr), value :: name, varidp
    end function c_nc_inq_varid

    integer(c_int) function c_nc_rename_var(ncid, varid, name) bind(C, name="nc_rename_var")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name
    end function c_nc_rename_var

    integer(c_int) function c_nc_inq_att(ncid, varid, name, xtypep, lenp) bind(C, name="nc_inq_att")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, xtypep, lenp
    end function c_nc_inq_att

    integer(c_int) function c_nc_inq_attid(ncid, varid, name, idp) bind(C, name="nc_inq_attid")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, idp
    end function c_nc_inq_attid

    integer(c_int) function c_nc_inq_attname(ncid, varid, attnum, name) bind(C, name="nc_inq_attname")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid, attnum
      type(c_ptr), value :: name
    end function c_nc_inq_attname

    integer(c_int) function c_nc_rename_att(ncid, varid, name, newname) bind(C, name="nc_rename_att")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, newname
    end function c_nc_rename_att

    integer(c_int) function c_nc_copy_att(ncid_in, varid_in, name, ncid_out, varid_out) bind(C, name="nc_copy_att")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid_in, varid_in, ncid_out, varid_out
      type(c_ptr), value :: name
    end function c_nc_copy_att

    integer(c_int) function c_nc_del_att(ncid, varid, name) bind(C, name="nc_del_att")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name
    end function c_nc_del_att

    integer(c_int) function c_nc_put_att_text(ncid, varid, name, len, op) bind(C, name="nc_put_att_text")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_text

    integer(c_int) function c_nc_get_att_text(ncid, varid, name, ip) bind(C, name="nc_get_att_text")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_text

    integer(c_int) function c_nc_put_att_float(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_float")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_float

    integer(c_int) function c_nc_get_att_float(ncid, varid, name, ip) bind(C, name="nc_get_att_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_float

    integer(c_int) function c_nc_put_var_float(ncid, varid, op) bind(C, name="nc_put_var_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_float

    integer(c_int) function c_nc_get_var_float(ncid, varid, ip) bind(C, name="nc_get_var_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_float

    integer(c_int) function c_nc_put_vara_float(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_float

    integer(c_int) function c_nc_get_vara_float(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_float

    integer(c_int) function c_nc_put_vars_float(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_float

    integer(c_int) function c_nc_get_vars_float(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_float

    integer(c_int) function c_nc_put_varm_float(ncid, varid, startp, countp, stridep, imapp, op) bind(C, name="nc_put_varm_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_float

    integer(c_int) function c_nc_get_varm_float(ncid, varid, startp, countp, stridep, imapp, ip) bind(C, name="nc_get_varm_float")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_float
    integer(c_int) function c_nc_put_att_double(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_double")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_double

    integer(c_int) function c_nc_get_att_double(ncid, varid, name, ip) bind(C, name="nc_get_att_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_double

    integer(c_int) function c_nc_put_var_double(ncid, varid, op) bind(C, name="nc_put_var_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_double

    integer(c_int) function c_nc_get_var_double(ncid, varid, ip) bind(C, name="nc_get_var_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_double

    integer(c_int) function c_nc_put_vara_double(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_double

    integer(c_int) function c_nc_get_vara_double(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_double

    integer(c_int) function c_nc_put_vars_double(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_double

    integer(c_int) function c_nc_get_vars_double(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_double

    integer(c_int) function c_nc_put_varm_double(ncid, varid, startp, countp, stridep, imapp, op) bind(C, name="nc_put_varm_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_double

    integer(c_int) function c_nc_get_varm_double(ncid, varid, startp, countp, stridep, imapp, ip) bind(C, name="nc_get_varm_double")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_double
    integer(c_int) function c_nc_put_att_schar(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_schar")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_schar

    integer(c_int) function c_nc_get_att_schar(ncid, varid, name, ip) bind(C, name="nc_get_att_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_schar

    integer(c_int) function c_nc_put_var_schar(ncid, varid, op) bind(C, name="nc_put_var_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_schar

    integer(c_int) function c_nc_get_var_schar(ncid, varid, ip) bind(C, name="nc_get_var_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_schar

    integer(c_int) function c_nc_put_vara_schar(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_schar

    integer(c_int) function c_nc_get_vara_schar(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_schar

    integer(c_int) function c_nc_put_vars_schar(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_schar

    integer(c_int) function c_nc_get_vars_schar(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_schar

    integer(c_int) function c_nc_put_varm_schar(ncid, varid, startp, countp, stridep, imapp, op) bind(C, name="nc_put_varm_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_schar

    integer(c_int) function c_nc_get_varm_schar(ncid, varid, startp, countp, stridep, imapp, ip) bind(C, name="nc_get_varm_schar")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_schar
    integer(c_int) function c_nc_put_att_short(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_short")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_short

    integer(c_int) function c_nc_get_att_short(ncid, varid, name, ip) bind(C, name="nc_get_att_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_short

    integer(c_int) function c_nc_put_var_short(ncid, varid, op) bind(C, name="nc_put_var_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_short

    integer(c_int) function c_nc_get_var_short(ncid, varid, ip) bind(C, name="nc_get_var_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_short

    integer(c_int) function c_nc_put_vara_short(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_short

    integer(c_int) function c_nc_get_vara_short(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_short

    integer(c_int) function c_nc_put_vars_short(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_short

    integer(c_int) function c_nc_get_vars_short(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_short

    integer(c_int) function c_nc_put_varm_short(ncid, varid, startp, countp, stridep, imapp, op) bind(C, name="nc_put_varm_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_short

    integer(c_int) function c_nc_get_varm_short(ncid, varid, startp, countp, stridep, imapp, ip) bind(C, name="nc_get_varm_short")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_short
    integer(c_int) function c_nc_put_att_int(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_int")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_int

    integer(c_int) function c_nc_get_att_int(ncid, varid, name, ip) bind(C, name="nc_get_att_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_int

    integer(c_int) function c_nc_put_var_int(ncid, varid, op) bind(C, name="nc_put_var_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_int

    integer(c_int) function c_nc_get_var_int(ncid, varid, ip) bind(C, name="nc_get_var_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_int

    integer(c_int) function c_nc_put_vara_int(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_int

    integer(c_int) function c_nc_get_vara_int(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_int

    integer(c_int) function c_nc_put_vars_int(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_int

    integer(c_int) function c_nc_get_vars_int(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_int

    integer(c_int) function c_nc_put_varm_int(ncid, varid, startp, countp, stridep, imapp, op) bind(C, name="nc_put_varm_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_int

    integer(c_int) function c_nc_get_varm_int(ncid, varid, startp, countp, stridep, imapp, ip) bind(C, name="nc_get_varm_int")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_int
    integer(c_int) function c_nc_put_att_longlong(ncid, varid, name, xtype, len, op) bind(C, name="nc_put_att_longlong")
      import :: c_int, c_ptr, c_size_t
      integer(c_int), value :: ncid, varid, xtype
      type(c_ptr), value :: name, op
      integer(c_size_t), value :: len
    end function c_nc_put_att_longlong

    integer(c_int) function c_nc_get_att_longlong(ncid, varid, name, ip) bind(C, name="nc_get_att_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: name, ip
    end function c_nc_get_att_longlong

    integer(c_int) function c_nc_put_var_longlong(ncid, varid, op) bind(C, name="nc_put_var_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: op
    end function c_nc_put_var_longlong

    integer(c_int) function c_nc_get_var_longlong(ncid, varid, ip) bind(C, name="nc_get_var_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: ip
    end function c_nc_get_var_longlong

    integer(c_int) function c_nc_put_vara_longlong(ncid, varid, startp, countp, op) bind(C, name="nc_put_vara_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, op
    end function c_nc_put_vara_longlong

    integer(c_int) function c_nc_get_vara_longlong(ncid, varid, startp, countp, ip) bind(C, name="nc_get_vara_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, ip
    end function c_nc_get_vara_longlong

    integer(c_int) function c_nc_put_vars_longlong(ncid, varid, startp, countp, stridep, op) bind(C, name="nc_put_vars_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, op
    end function c_nc_put_vars_longlong

    integer(c_int) function c_nc_get_vars_longlong(ncid, varid, startp, countp, stridep, ip) bind(C, name="nc_get_vars_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, ip
    end function c_nc_get_vars_longlong

    integer(c_int) function c_nc_put_varm_longlong(ncid, varid, startp, countp, stridep, imapp, op) bind(C,&
        & name="nc_put_varm_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, op
    end function c_nc_put_varm_longlong

    integer(c_int) function c_nc_get_varm_longlong(ncid, varid, startp, countp, stridep, imapp, ip) bind(C,&
        & name="nc_get_varm_longlong")
      import :: c_int, c_ptr
      integer(c_int), value :: ncid, varid
      type(c_ptr), value :: startp, countp, stridep, imapp, ip
    end function c_nc_get_varm_longlong
  end interface

  interface ncw_put_att
    module procedure ncw_put_att_0d_sp
    module procedure ncw_put_att_1d_sp
    module procedure ncw_put_att_0d_dp
    module procedure ncw_put_att_1d_dp
    module procedure ncw_put_att_0d_i1
    module procedure ncw_put_att_1d_i1
    module procedure ncw_put_att_0d_i2
    module procedure ncw_put_att_1d_i2
    module procedure ncw_put_att_0d_i4
    module procedure ncw_put_att_1d_i4
    module procedure ncw_put_att_0d_i8
    module procedure ncw_put_att_1d_i8
    module procedure ncw_put_att_0d_char
  end interface ncw_put_att

  interface ncw_get_att
    module procedure ncw_get_att_0d_sp
    module procedure ncw_get_att_1d_sp
    module procedure ncw_get_att_0d_dp
    module procedure ncw_get_att_1d_dp
    module procedure ncw_get_att_0d_i1
    module procedure ncw_get_att_1d_i1
    module procedure ncw_get_att_0d_i2
    module procedure ncw_get_att_1d_i2
    module procedure ncw_get_att_0d_i4
    module procedure ncw_get_att_1d_i4
    module procedure ncw_get_att_0d_i8
    module procedure ncw_get_att_1d_i8
    module procedure ncw_get_att_0d_char
  end interface ncw_get_att

  interface ncw_put_var
    module procedure ncw_put_var_0d_sp
    module procedure ncw_put_var_1d_sp
    module procedure ncw_put_var_2d_sp
    module procedure ncw_put_var_3d_sp
    module procedure ncw_put_var_4d_sp
    module procedure ncw_put_var_5d_sp
    module procedure ncw_put_var_6d_sp
    module procedure ncw_put_var_0d_dp
    module procedure ncw_put_var_1d_dp
    module procedure ncw_put_var_2d_dp
    module procedure ncw_put_var_3d_dp
    module procedure ncw_put_var_4d_dp
    module procedure ncw_put_var_5d_dp
    module procedure ncw_put_var_6d_dp
    module procedure ncw_put_var_0d_i1
    module procedure ncw_put_var_1d_i1
    module procedure ncw_put_var_2d_i1
    module procedure ncw_put_var_3d_i1
    module procedure ncw_put_var_4d_i1
    module procedure ncw_put_var_5d_i1
    module procedure ncw_put_var_6d_i1
    module procedure ncw_put_var_0d_i2
    module procedure ncw_put_var_1d_i2
    module procedure ncw_put_var_2d_i2
    module procedure ncw_put_var_3d_i2
    module procedure ncw_put_var_4d_i2
    module procedure ncw_put_var_5d_i2
    module procedure ncw_put_var_6d_i2
    module procedure ncw_put_var_0d_i4
    module procedure ncw_put_var_1d_i4
    module procedure ncw_put_var_2d_i4
    module procedure ncw_put_var_3d_i4
    module procedure ncw_put_var_4d_i4
    module procedure ncw_put_var_5d_i4
    module procedure ncw_put_var_6d_i4
    module procedure ncw_put_var_0d_i8
    module procedure ncw_put_var_1d_i8
    module procedure ncw_put_var_2d_i8
    module procedure ncw_put_var_3d_i8
    module procedure ncw_put_var_4d_i8
    module procedure ncw_put_var_5d_i8
    module procedure ncw_put_var_6d_i8
  end interface ncw_put_var

  interface ncw_put_var64
    module procedure ncw_put_var64_0d_sp
    module procedure ncw_put_var64_1d_sp
    module procedure ncw_put_var64_2d_sp
    module procedure ncw_put_var64_3d_sp
    module procedure ncw_put_var64_4d_sp
    module procedure ncw_put_var64_5d_sp
    module procedure ncw_put_var64_6d_sp
    module procedure ncw_put_var64_0d_dp
    module procedure ncw_put_var64_1d_dp
    module procedure ncw_put_var64_2d_dp
    module procedure ncw_put_var64_3d_dp
    module procedure ncw_put_var64_4d_dp
    module procedure ncw_put_var64_5d_dp
    module procedure ncw_put_var64_6d_dp
    module procedure ncw_put_var64_0d_i1
    module procedure ncw_put_var64_1d_i1
    module procedure ncw_put_var64_2d_i1
    module procedure ncw_put_var64_3d_i1
    module procedure ncw_put_var64_4d_i1
    module procedure ncw_put_var64_5d_i1
    module procedure ncw_put_var64_6d_i1
    module procedure ncw_put_var64_0d_i2
    module procedure ncw_put_var64_1d_i2
    module procedure ncw_put_var64_2d_i2
    module procedure ncw_put_var64_3d_i2
    module procedure ncw_put_var64_4d_i2
    module procedure ncw_put_var64_5d_i2
    module procedure ncw_put_var64_6d_i2
    module procedure ncw_put_var64_0d_i4
    module procedure ncw_put_var64_1d_i4
    module procedure ncw_put_var64_2d_i4
    module procedure ncw_put_var64_3d_i4
    module procedure ncw_put_var64_4d_i4
    module procedure ncw_put_var64_5d_i4
    module procedure ncw_put_var64_6d_i4
    module procedure ncw_put_var64_0d_i8
    module procedure ncw_put_var64_1d_i8
    module procedure ncw_put_var64_2d_i8
    module procedure ncw_put_var64_3d_i8
    module procedure ncw_put_var64_4d_i8
    module procedure ncw_put_var64_5d_i8
    module procedure ncw_put_var64_6d_i8
  end interface ncw_put_var64

  interface ncw_get_var
    module procedure ncw_get_var_0d_sp
    module procedure ncw_get_var_1d_sp
    module procedure ncw_get_var_2d_sp
    module procedure ncw_get_var_3d_sp
    module procedure ncw_get_var_4d_sp
    module procedure ncw_get_var_5d_sp
    module procedure ncw_get_var_6d_sp
    module procedure ncw_get_var_0d_dp
    module procedure ncw_get_var_1d_dp
    module procedure ncw_get_var_2d_dp
    module procedure ncw_get_var_3d_dp
    module procedure ncw_get_var_4d_dp
    module procedure ncw_get_var_5d_dp
    module procedure ncw_get_var_6d_dp
    module procedure ncw_get_var_0d_i1
    module procedure ncw_get_var_1d_i1
    module procedure ncw_get_var_2d_i1
    module procedure ncw_get_var_3d_i1
    module procedure ncw_get_var_4d_i1
    module procedure ncw_get_var_5d_i1
    module procedure ncw_get_var_6d_i1
    module procedure ncw_get_var_0d_i2
    module procedure ncw_get_var_1d_i2
    module procedure ncw_get_var_2d_i2
    module procedure ncw_get_var_3d_i2
    module procedure ncw_get_var_4d_i2
    module procedure ncw_get_var_5d_i2
    module procedure ncw_get_var_6d_i2
    module procedure ncw_get_var_0d_i4
    module procedure ncw_get_var_1d_i4
    module procedure ncw_get_var_2d_i4
    module procedure ncw_get_var_3d_i4
    module procedure ncw_get_var_4d_i4
    module procedure ncw_get_var_5d_i4
    module procedure ncw_get_var_6d_i4
    module procedure ncw_get_var_0d_i8
    module procedure ncw_get_var_1d_i8
    module procedure ncw_get_var_2d_i8
    module procedure ncw_get_var_3d_i8
    module procedure ncw_get_var_4d_i8
    module procedure ncw_get_var_5d_i8
    module procedure ncw_get_var_6d_i8
  end interface ncw_get_var

  interface ncw_get_var64
    module procedure ncw_get_var64_0d_sp
    module procedure ncw_get_var64_1d_sp
    module procedure ncw_get_var64_2d_sp
    module procedure ncw_get_var64_3d_sp
    module procedure ncw_get_var64_4d_sp
    module procedure ncw_get_var64_5d_sp
    module procedure ncw_get_var64_6d_sp
    module procedure ncw_get_var64_0d_dp
    module procedure ncw_get_var64_1d_dp
    module procedure ncw_get_var64_2d_dp
    module procedure ncw_get_var64_3d_dp
    module procedure ncw_get_var64_4d_dp
    module procedure ncw_get_var64_5d_dp
    module procedure ncw_get_var64_6d_dp
    module procedure ncw_get_var64_0d_i1
    module procedure ncw_get_var64_1d_i1
    module procedure ncw_get_var64_2d_i1
    module procedure ncw_get_var64_3d_i1
    module procedure ncw_get_var64_4d_i1
    module procedure ncw_get_var64_5d_i1
    module procedure ncw_get_var64_6d_i1
    module procedure ncw_get_var64_0d_i2
    module procedure ncw_get_var64_1d_i2
    module procedure ncw_get_var64_2d_i2
    module procedure ncw_get_var64_3d_i2
    module procedure ncw_get_var64_4d_i2
    module procedure ncw_get_var64_5d_i2
    module procedure ncw_get_var64_6d_i2
    module procedure ncw_get_var64_0d_i4
    module procedure ncw_get_var64_1d_i4
    module procedure ncw_get_var64_2d_i4
    module procedure ncw_get_var64_3d_i4
    module procedure ncw_get_var64_4d_i4
    module procedure ncw_get_var64_5d_i4
    module procedure ncw_get_var64_6d_i4
    module procedure ncw_get_var64_0d_i8
    module procedure ncw_get_var64_1d_i8
    module procedure ncw_get_var64_2d_i8
    module procedure ncw_get_var64_3d_i8
    module procedure ncw_get_var64_4d_i8
    module procedure ncw_get_var64_5d_i8
    module procedure ncw_get_var64_6d_i8
  end interface ncw_get_var64

  interface ncw_def_var
    module procedure ncw_def_var_1d
    module procedure ncw_def_var_nd
  end interface ncw_def_var

  interface ncw_def_var_fill
    module procedure ncw_def_var_fill_sp
    module procedure ncw_def_var_fill_dp
    module procedure ncw_def_var_fill_i1
    module procedure ncw_def_var_fill_i2
    module procedure ncw_def_var_fill_i4
    module procedure ncw_def_var_fill_i8
  end interface ncw_def_var_fill

  interface ncw_inq_var_fill
    module procedure ncw_inq_var_fill_sp
    module procedure ncw_inq_var_fill_dp
    module procedure ncw_inq_var_fill_i1
    module procedure ncw_inq_var_fill_i2
    module procedure ncw_inq_var_fill_i4
    module procedure ncw_inq_var_fill_i8
  end interface ncw_inq_var_fill

contains

  function ncw_strerror(status) result(msg)
    integer(i4), intent(in) :: status
    character(len = NCW_MAX_NAME) :: msg

    character(kind = c_char, len = 1), target :: cbuf(NCW_MAX_NAME + 1_i4)

    cbuf = c_null_char
    call forces_nc_strerror_copy(int(status, c_int), cbuf, int(size(cbuf), c_size_t))
    call c_chars_to_fortran(cbuf, msg)
  end function ncw_strerror

  function ncw_open(path, mode, ncid, chunksize, cache_size, cache_nelems, cache_preemption)
    character(len = *), intent(in) :: path
    integer(i4), intent(in) :: mode
    integer(i4), intent(out) :: ncid
    integer(i4), intent(inout), optional :: chunksize
    integer(i4), intent(in), optional :: cache_size, cache_nelems, cache_preemption
    integer(i4) :: ncw_open

    character(kind = c_char, len = 1), allocatable, target :: cpath(:)
    integer(c_int), target :: c_ncid
    integer(c_size_t), target :: c_chunk
    integer(c_size_t), target :: size_in, nelems_in, size_out, nelems_out
    real(c_float), target :: preemption_in
    real(c_float) :: preemption_out
    logical :: reset_cache

    cpath = to_c_string(path)
    reset_cache = .false.
    ncid = -1_i4

    if (present(cache_size) .or. present(cache_nelems) .or. present(cache_preemption)) then
      ncw_open = int(c_nc_get_chunk_cache(c_loc(size_in), c_loc(nelems_in), c_loc(preemption_in)), i4)
      if (ncw_open /= NCW_NOERR) return
      size_out = size_in
      nelems_out = nelems_in
      preemption_out = preemption_in
      if (present(cache_size)) size_out = int(cache_size, c_size_t)
      if (present(cache_nelems)) nelems_out = int(cache_nelems, c_size_t)
      if (present(cache_preemption)) preemption_out = real(cache_preemption, c_float) / 100.0_c_float
      ncw_open = int(c_nc_set_chunk_cache(size_out, nelems_out, preemption_out), i4)
      if (ncw_open /= NCW_NOERR) return
      reset_cache = .true.
    end if

    if (present(chunksize)) then
      c_chunk = int(max(chunksize, 0_i4), c_size_t)
      ncw_open = int(c_nc__open(c_loc(cpath(1)), int(mode, c_int), c_loc(c_chunk), c_loc(c_ncid)), i4)
      if (ncw_open == NCW_NOERR) chunksize = int(min(c_chunk, int(huge(0_i4), c_size_t)), i4)
    else
      ncw_open = int(c_nc_open(c_loc(cpath(1)), int(mode, c_int), c_loc(c_ncid)), i4)
    end if

    if (ncw_open == NCW_NOERR) ncid = int(c_ncid, i4)

    if (reset_cache) then
      ncw_open = int(c_nc_set_chunk_cache(size_in, nelems_in, preemption_in), i4)
    end if
  end function ncw_open

  function ncw_create(path, cmode, ncid, initialsize, chunksize, cache_size, cache_nelems, cache_preemption)
    character(len = *), intent(in) :: path
    integer(i4), intent(in) :: cmode
    integer(i4), intent(out) :: ncid
    integer(i4), intent(in), optional :: initialsize
    integer(i4), intent(inout), optional :: chunksize
    integer(i4), intent(in), optional :: cache_size, cache_nelems, cache_preemption
    integer(i4) :: ncw_create

    character(kind = c_char, len = 1), allocatable, target :: cpath(:)
    integer(c_int), target :: c_ncid
    integer(c_size_t), target :: c_chunk
    integer(c_size_t) :: initial_size
    integer(c_size_t), target :: size_in, nelems_in, size_out, nelems_out
    real(c_float), target :: preemption_in
    real(c_float) :: preemption_out
    logical :: reset_cache

    cpath = to_c_string(path)
    reset_cache = .false.
    ncid = -1_i4
    initial_size = 0_c_size_t
    if (present(initialsize)) initial_size = int(max(initialsize, 0_i4), c_size_t)

    if (present(cache_size) .or. present(cache_nelems) .or. present(cache_preemption)) then
      ncw_create = int(c_nc_get_chunk_cache(c_loc(size_in), c_loc(nelems_in), c_loc(preemption_in)), i4)
      if (ncw_create /= NCW_NOERR) return
      size_out = size_in
      nelems_out = nelems_in
      preemption_out = preemption_in
      if (present(cache_size)) size_out = int(cache_size, c_size_t)
      if (present(cache_nelems)) nelems_out = int(cache_nelems, c_size_t)
      if (present(cache_preemption)) preemption_out = real(cache_preemption, c_float) / 100.0_c_float
      ncw_create = int(c_nc_set_chunk_cache(size_out, nelems_out, preemption_out), i4)
      if (ncw_create /= NCW_NOERR) return
      reset_cache = .true.
    end if

    if (present(initialsize) .or. present(chunksize)) then
      c_chunk = 0_c_size_t
      if (present(chunksize)) c_chunk = int(max(chunksize, 0_i4), c_size_t)
      ncw_create = int(c_nc__create(c_loc(cpath(1)), int(cmode, c_int), initial_size, c_loc(c_chunk), c_loc(c_ncid)), i4)
      if (ncw_create == NCW_NOERR .and. present(chunksize)) chunksize = int(min(c_chunk, int(huge(0_i4), c_size_t)), i4)
    else
      ncw_create = int(c_nc_create(c_loc(cpath(1)), int(cmode, c_int), c_loc(c_ncid)), i4)
    end if

    if (ncw_create == NCW_NOERR) ncid = int(c_ncid, i4)

    if (reset_cache) then
      ncw_create = int(c_nc_set_chunk_cache(size_in, nelems_in, preemption_in), i4)
    end if
  end function ncw_create

  function ncw_close(ncid)
    integer(i4), intent(in) :: ncid
    integer(i4) :: ncw_close
    ncw_close = int(c_nc_close(int(ncid, c_int)), i4)
  end function ncw_close

  function ncw_sync(ncid)
    integer(i4), intent(in) :: ncid
    integer(i4) :: ncw_sync
    ncw_sync = int(c_nc_sync(int(ncid, c_int)), i4)
  end function ncw_sync

  function ncw_redef(ncid)
    integer(i4), intent(in) :: ncid
    integer(i4) :: ncw_redef
    ncw_redef = int(c_nc_redef(int(ncid, c_int)), i4)
  end function ncw_redef

  function ncw_enddef(ncid)
    integer(i4), intent(in) :: ncid
    integer(i4) :: ncw_enddef
    ncw_enddef = int(c_nc_enddef(int(ncid, c_int)), i4)
  end function ncw_enddef

  function ncw_abort(ncid)
    integer(i4), intent(in) :: ncid
    integer(i4) :: ncw_abort
    ncw_abort = int(c_nc_abort(int(ncid, c_int)), i4)
  end function ncw_abort

  function ncw_set_fill(ncid, fillmode, old_mode)
    integer(i4), intent(in) :: ncid, fillmode
    integer(i4), intent(out), optional :: old_mode
    integer(i4) :: ncw_set_fill
    integer(c_int), target :: c_old_mode

    if (present(old_mode)) then
      ncw_set_fill = int(c_nc_set_fill(int(ncid, c_int), int(fillmode, c_int), c_loc(c_old_mode)), i4)
      if (ncw_set_fill == NCW_NOERR) old_mode = int(c_old_mode, i4)
    else
      ncw_set_fill = int(c_nc_set_fill(int(ncid, c_int), int(fillmode, c_int), c_null_ptr), i4)
    end if
  end function ncw_set_fill

  function ncw_inq_path(ncid, pathlen, path)
    integer(i4), intent(in) :: ncid
    integer(i4), intent(inout) :: pathlen
    character(len = *), intent(inout) :: path
    integer(i4) :: ncw_inq_path

    integer(c_size_t), target :: c_pathlen
    character(kind = c_char, len = 1), allocatable, target :: cpath(:)
    integer(i4) :: alloc_len

    ncw_inq_path = int(c_nc_inq_path(int(ncid, c_int), c_loc(c_pathlen), c_null_ptr), i4)
    if (ncw_inq_path /= NCW_NOERR) return

    alloc_len = int(c_pathlen, i4) + 1_i4
    allocate(cpath(max(1_i4, alloc_len)))
    cpath = c_null_char
    ncw_inq_path = int(c_nc_inq_path(int(ncid, c_int), c_loc(c_pathlen), c_loc(cpath(1))), i4)
    if (ncw_inq_path /= NCW_NOERR) return

    pathlen = int(min(c_pathlen, int(len(path), c_size_t)), i4)
    call c_chars_to_fortran(cpath, path)
  end function ncw_inq_path

  function ncw_inq_format(ncid, format_type)
    integer(i4), intent(in) :: ncid
    integer(i4), intent(out) :: format_type
    integer(i4) :: ncw_inq_format

    integer(c_int), target :: c_format

    ncw_inq_format = int(c_nc_inq_format(int(ncid, c_int), c_loc(c_format)), i4)
    if (ncw_inq_format == NCW_NOERR) format_type = int(c_format, i4)
  end function ncw_inq_format

  function ncw_inquire(ncid, ndimensions, nvariables, nattributes, unlimitedDimId, formatNum)
    integer(i4), intent(in) :: ncid
    integer(i4), intent(out), optional :: ndimensions, nvariables, nattributes, unlimitedDimId, formatNum
    integer(i4) :: ncw_inquire

    integer(c_int), target :: c_ndims, c_nvars, c_natts, c_unlim, c_format

    ncw_inquire = int(c_nc_inq(int(ncid, c_int), maybe_i4_ptr(ndimensions, c_ndims), maybe_i4_ptr(nvariables, c_nvars), &
            maybe_i4_ptr(nattributes, c_natts), maybe_i4_ptr(unlimitedDimId, c_unlim)), i4)

    if (ncw_inquire /= NCW_NOERR) return

    if (present(ndimensions)) ndimensions = int(c_ndims, i4)
    if (present(nvariables)) nvariables = int(c_nvars, i4)
    if (present(nattributes)) nattributes = int(c_natts, i4)
    if (present(unlimitedDimId)) unlimitedDimId = int(c_unlim, i4)
    if (present(formatNum)) then
      ncw_inquire = int(c_nc_inq_format(int(ncid, c_int), c_loc(c_format)), i4)
      if (ncw_inquire == NCW_NOERR) formatNum = int(c_format, i4)
    end if
  end function ncw_inquire

  function ncw_def_dim(ncid, name, len, dimid)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(in) :: name
    integer(i4), intent(in) :: len
    integer(i4), intent(out) :: dimid
    integer(i4) :: ncw_def_dim

    ncw_def_dim = ncw_def_dim64(ncid, name, int(len, i8), dimid)
  end function ncw_def_dim

  function ncw_def_dim64(ncid, name, len, dimid)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(in) :: name
    integer(i8), intent(in) :: len
    integer(i4), intent(out) :: dimid
    integer(i4) :: ncw_def_dim64

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_dimid
    integer(c_size_t) :: c_len

    cname = to_c_string(name)
    if (len <= 0_i8) then
      c_len = NCW_UNLIMITED
    else
      c_len = int(len, c_size_t)
    end if

    ncw_def_dim64 = int(c_nc_def_dim(int(ncid, c_int), c_loc(cname(1)), c_len, c_loc(c_dimid)), i4)
    if (ncw_def_dim64 == NCW_NOERR) dimid = int(c_dimid, i4)
  end function ncw_def_dim64

  function ncw_inquire_dimension(ncid, dimid, name, len)
    integer(i4), intent(in) :: ncid, dimid
    character(len = *), intent(out), optional :: name
    integer(i4), intent(out), optional :: len
    integer(i4) :: ncw_inquire_dimension

    integer(i8) :: len64

    ncw_inquire_dimension = ncw_inquire_dimension64(ncid, dimid, name, len64)
    if (ncw_inquire_dimension /= NCW_NOERR) return

    if (present(len)) then
      if (len64 > huge(len)) then
        len = huge(len)
        ncw_inquire_dimension = NCW_EINVAL
      else
        len = int(len64, i4)
      end if
    end if
  end function ncw_inquire_dimension

  function ncw_inquire_dimension64(ncid, dimid, name, len)
    integer(i4), intent(in) :: ncid, dimid
    character(len = *), intent(out), optional :: name
    integer(i8), intent(out), optional :: len
    integer(i4) :: ncw_inquire_dimension64

    character(kind = c_char, len = 1), target :: cname(NCW_MAX_NAME + 1_i4)
    integer(c_size_t), target :: c_len
    type(c_ptr) :: cname_ptr, len_ptr

    cname = c_null_char
    cname_ptr = c_null_ptr
    len_ptr = c_null_ptr
    if (present(name)) cname_ptr = c_loc(cname(1))
    if (present(len)) len_ptr = c_loc(c_len)
    ncw_inquire_dimension64 = int(c_nc_inq_dim(int(ncid, c_int), int(dimid, c_int), &
            cname_ptr, len_ptr), i4)
    if (ncw_inquire_dimension64 /= NCW_NOERR) return

    if (present(name)) call c_chars_to_fortran(cname, name)
    if (present(len)) len = int(c_len, i8)
  end function ncw_inquire_dimension64

  function ncw_inq_dimid(ncid, name, dimid)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: dimid
    integer(i4) :: ncw_inq_dimid

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_dimid

    cname = to_c_string(name)
    ncw_inq_dimid = int(c_nc_inq_dimid(int(ncid, c_int), c_loc(cname(1)), c_loc(c_dimid)), i4)
    if (ncw_inq_dimid == NCW_NOERR) dimid = int(c_dimid, i4)
  end function ncw_inq_dimid

  function ncw_rename_dim(ncid, dimid, name)
    integer(i4), intent(in) :: ncid, dimid
    character(len = *), intent(in) :: name
    integer(i4) :: ncw_rename_dim

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_rename_dim = int(c_nc_rename_dim(int(ncid, c_int), int(dimid, c_int), c_loc(cname(1))), i4)
  end function ncw_rename_dim

  function ncw_def_grp(parent_ncid, name, new_ncid)
    integer(i4), intent(in) :: parent_ncid
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: new_ncid
    integer(i4) :: ncw_def_grp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_new_ncid

    cname = to_c_string(name)
    ncw_def_grp = int(c_nc_def_grp(int(parent_ncid, c_int), c_loc(cname(1)), c_loc(c_new_ncid)), i4)
    if (ncw_def_grp == NCW_NOERR) new_ncid = int(c_new_ncid, i4)
  end function ncw_def_grp

  function ncw_inq_ncid(ncid, name, grp_ncid)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: grp_ncid
    integer(i4) :: ncw_inq_ncid

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_grp_ncid

    cname = to_c_string(name)
    ncw_inq_ncid = int(c_nc_inq_ncid(int(ncid, c_int), c_loc(cname(1)), c_loc(c_grp_ncid)), i4)
    if (ncw_inq_ncid == NCW_NOERR) grp_ncid = int(c_grp_ncid, i4)
  end function ncw_inq_ncid

  function ncw_inq_grp_parent(ncid, parent_ncid)
    integer(i4), intent(in) :: ncid
    integer(i4), intent(out) :: parent_ncid
    integer(i4) :: ncw_inq_grp_parent

    integer(c_int), target :: c_parent

    ncw_inq_grp_parent = int(c_nc_inq_grp_parent(int(ncid, c_int), c_loc(c_parent)), i4)
    if (ncw_inq_grp_parent == NCW_NOERR) parent_ncid = int(c_parent, i4)
  end function ncw_inq_grp_parent

  function ncw_inq_grpname(ncid, name)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(out) :: name
    integer(i4) :: ncw_inq_grpname

    character(kind = c_char, len = 1), target :: cname(NCW_MAX_NAME + 1_i4)

    cname = c_null_char
    ncw_inq_grpname = int(c_nc_inq_grpname(int(ncid, c_int), c_loc(cname(1))), i4)
    if (ncw_inq_grpname == NCW_NOERR) call c_chars_to_fortran(cname, name)
  end function ncw_inq_grpname

  function ncw_rename_grp(grpid, name)
    integer(i4), intent(in) :: grpid
    character(len = *), intent(in) :: name
    integer(i4) :: ncw_rename_grp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_rename_grp = int(c_nc_rename_grp(int(grpid, c_int), c_loc(cname(1))), i4)
  end function ncw_rename_grp

  function ncw_inq_varids(ncid, nvars, varids)
    integer(i4), intent(in) :: ncid
    integer(i4), intent(out) :: nvars
    integer(i4), intent(out) :: varids(:)
    integer(i4) :: ncw_inq_varids

    integer(c_int), target :: c_nvars
    integer(c_int), target :: c_varids(max(1, size(varids)))
    integer(i4) :: i

    ncw_inq_varids = int(c_nc_inq_varids(int(ncid, c_int), c_loc(c_nvars), c_loc(c_varids(1))), i4)
    if (ncw_inq_varids /= NCW_NOERR) return

    nvars = int(c_nvars, i4)
    do i = 1, min(size(varids), nvars)
      varids(i) = int(c_varids(i), i4)
    end do
  end function ncw_inq_varids

  function ncw_def_var_scalar(ncid, name, xtype, varid, contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)
    integer(i4), intent(in) :: ncid, xtype
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: varid
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: chunksizes, deflate_level, endianness, cache_size, cache_nelems, cache_preemption
    integer(i4) :: ncw_def_var_scalar
    integer(i4) :: dimids(1)
    integer(i4) :: chunk(1)
    if (present(chunksizes)) chunk(1) = chunksizes
    ncw_def_var_scalar = ncw_def_var_impl(ncid, name, xtype, dimids(1:0), varid, contiguous, chunk(1:0), deflate_level, shuffle, &
            fletcher32, endianness, cache_size, cache_nelems, cache_preemption, .false.)
  end function ncw_def_var_scalar

  function ncw_def_var_1d(ncid, name, xtype, dimids, varid, contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)
    integer(i4), intent(in) :: ncid, xtype, dimids
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: varid
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: chunksizes, deflate_level, endianness, cache_size, cache_nelems, cache_preemption
    integer(i4) :: ncw_def_var_1d
    integer(i4) :: dimids_a(1), chunk(1)
    dimids_a(1) = dimids
    if (present(chunksizes)) chunk(1) = chunksizes
    if (present(chunksizes)) then
      ncw_def_var_1d = ncw_def_var_impl(ncid, name, xtype, dimids_a, varid, contiguous, chunk, deflate_level, shuffle, fletcher32, &
              endianness, cache_size, cache_nelems, cache_preemption, .true.)
    else
      ncw_def_var_1d = ncw_def_var_impl(ncid, name, xtype, dimids_a, varid, contiguous, chunk(1:0), deflate_level, shuffle, &
              fletcher32, endianness, cache_size, cache_nelems, cache_preemption, .false.)
    end if
  end function ncw_def_var_1d

  function ncw_def_var_nd(ncid, name, xtype, dimids, varid, contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption)
    integer(i4), intent(in) :: ncid, xtype, dimids(:)
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: varid
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in), optional :: chunksizes(:), deflate_level, endianness, cache_size, cache_nelems, cache_preemption
    integer(i4) :: ncw_def_var_nd
    integer(i4) :: chunks_local(max(1, size(dimids)))
    if (present(chunksizes)) chunks_local(1:size(chunksizes)) = chunksizes
    if (present(chunksizes)) then
      ncw_def_var_nd = ncw_def_var_impl(ncid, name, xtype, dimids, varid, contiguous, chunks_local(1:size(dimids)), deflate_level, shuffle, &
              fletcher32, endianness, cache_size, cache_nelems, cache_preemption, .true.)
    else
      ncw_def_var_nd = ncw_def_var_impl(ncid, name, xtype, dimids, varid, contiguous, chunks_local(1:0), deflate_level, shuffle, &
              fletcher32, endianness, cache_size, cache_nelems, cache_preemption, .false.)
    end if
  end function ncw_def_var_nd

  function ncw_inquire_variable(ncid, varid, name, xtype, ndims, dimids, natts, contiguous, chunksizes, deflate_level, shuffle, &
          fletcher32, endianness, cache_size, cache_nelems, cache_preemption)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(out), optional :: name
    integer(i4), intent(out), optional :: xtype, ndims, natts
    integer(i4), intent(out), optional :: dimids(:), chunksizes(:), deflate_level, endianness, cache_size, cache_nelems, cache_preemption
    logical, intent(out), optional :: contiguous, shuffle, fletcher32
    integer(i4) :: ncw_inquire_variable

    character(kind = c_char, len = 1), target :: cname(NCW_MAX_NAME + 1_i4)
    integer(c_int), target :: c_xtype, c_ndims, c_natts, c_storage, c_shuffle, c_deflate, c_deflate_level, c_fletcher32, c_endianness
    integer(c_int), target :: c_dimids(NCW_MAX_VAR_DIMS)
    integer(c_size_t), target :: c_chunks(NCW_MAX_VAR_DIMS), c_cache_size, c_cache_nelems
    real(c_float), target :: c_cache_preemption
    type(c_ptr) :: dimids_ptr, name_ptr
    integer(i4) :: i, n, ndims_i4

    cname = c_null_char
    c_chunks = 0_c_size_t
    dimids_ptr = c_null_ptr
    name_ptr = c_null_ptr
    if (present(name)) name_ptr = c_loc(cname(1))
    if (present(dimids)) dimids_ptr = c_loc(c_dimids(1))
    ncw_inquire_variable = int(c_nc_inq_var(int(ncid, c_int), int(varid, c_int), name_ptr, &
            maybe_i4_ptr(xtype, c_xtype), c_loc(c_ndims), dimids_ptr, maybe_i4_ptr(natts, c_natts)), i4)
    if (ncw_inquire_variable /= NCW_NOERR) return

    ndims_i4 = int(c_ndims, i4)
    if (present(name)) call c_chars_to_fortran(cname, name)
    if (present(xtype)) xtype = int(c_xtype, i4)
    if (present(ndims)) ndims = ndims_i4
    if (present(natts)) natts = int(c_natts, i4)
    if (present(dimids)) then
      n = min(size(dimids), ndims_i4)
      do i = 1, n
        dimids(i) = int(c_dimids(ndims_i4 - i + 1), i4)
      end do
    end if
    if (present(chunksizes) .or. present(contiguous)) then
      ncw_inquire_variable = int(c_nc_inq_var_chunking(int(ncid, c_int), int(varid, c_int), c_loc(c_storage), c_loc(c_chunks(1))), i4)
      if (ncw_inquire_variable /= NCW_NOERR) return
      if (present(contiguous)) contiguous = int(c_storage, i4) /= NCW_NOTCONTIGUOUS
      if (present(chunksizes)) then
        chunksizes = 0_i4
        n = min(size(chunksizes), ndims_i4)
        do i = 1, n
          chunksizes(i) = int(min(c_chunks(ndims_i4 - i + 1), int(huge(0_i4), c_size_t)), i4)
        end do
      end if
    end if
    if (present(deflate_level) .or. present(shuffle)) then
      ncw_inquire_variable = int(c_nc_inq_var_deflate(int(ncid, c_int), int(varid, c_int), c_loc(c_shuffle), c_loc(c_deflate), &
              c_loc(c_deflate_level)), i4)
      if (ncw_inquire_variable /= NCW_NOERR) return
      if (present(deflate_level)) deflate_level = int(c_deflate_level, i4)
      if (present(shuffle)) shuffle = c_shuffle /= 0_c_int
    end if
    if (present(fletcher32)) then
      ncw_inquire_variable = int(c_nc_inq_var_fletcher32(int(ncid, c_int), int(varid, c_int), c_loc(c_fletcher32)), i4)
      if (ncw_inquire_variable /= NCW_NOERR) return
      fletcher32 = c_fletcher32 /= 0_c_int
    end if
    if (present(endianness)) then
      ncw_inquire_variable = int(c_nc_inq_var_endian(int(ncid, c_int), int(varid, c_int), c_loc(c_endianness)), i4)
      if (ncw_inquire_variable /= NCW_NOERR) return
      endianness = int(c_endianness, i4)
    end if
    if (present(cache_size) .or. present(cache_nelems) .or. present(cache_preemption)) then
      ncw_inquire_variable = int(c_nc_get_var_chunk_cache(int(ncid, c_int), int(varid, c_int), c_loc(c_cache_size), c_loc(c_cache_nelems), &
              c_loc(c_cache_preemption)), i4)
      if (ncw_inquire_variable /= NCW_NOERR) return
      if (present(cache_size)) cache_size = int(min(c_cache_size, int(huge(0_i4), c_size_t)), i4)
      if (present(cache_nelems)) cache_nelems = int(min(c_cache_nelems, int(huge(0_i4), c_size_t)), i4)
      if (present(cache_preemption)) cache_preemption = int(nint(100.0_c_float * c_cache_preemption), i4)
    end if
  end function ncw_inquire_variable

  function ncw_inq_var_chunking(ncid, varid, storage, chunksizes)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: storage
    integer(i4), intent(out) :: chunksizes(:)
    integer(i4) :: ncw_inq_var_chunking

    integer(c_int), target :: c_ndims, c_storage
    integer(c_size_t), target :: c_chunks(NCW_MAX_VAR_DIMS)
    integer(i4) :: i, n, ndims_i4

    c_chunks = 0_c_size_t
    chunksizes = 0_i4
    storage = NCW_NOTCONTIGUOUS
    ncw_inq_var_chunking = int(c_nc_inq_var(int(ncid, c_int), int(varid, c_int), c_null_ptr, c_null_ptr, c_loc(c_ndims), c_null_ptr, &
            c_null_ptr), i4)
    if (ncw_inq_var_chunking /= NCW_NOERR) return

    ncw_inq_var_chunking = int(c_nc_inq_var_chunking(int(ncid, c_int), int(varid, c_int), c_loc(c_storage), c_loc(c_chunks(1))), i4)
    if (ncw_inq_var_chunking /= NCW_NOERR) return

    storage = int(c_storage, i4)
    ndims_i4 = int(c_ndims, i4)
    n = min(size(chunksizes), ndims_i4)
    do i = 1, n
      chunksizes(i) = int(min(c_chunks(ndims_i4 - i + 1), int(huge(0_i4), c_size_t)), i4)
    end do
  end function ncw_inq_var_chunking

  function ncw_inq_var_deflate(ncid, varid, shuffle, deflate, deflate_level)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: shuffle, deflate, deflate_level
    integer(i4) :: ncw_inq_var_deflate

    integer(c_int), target :: c_shuffle, c_deflate, c_deflate_level

    ncw_inq_var_deflate = int(c_nc_inq_var_deflate(int(ncid, c_int), int(varid, c_int), c_loc(c_shuffle), c_loc(c_deflate), &
            c_loc(c_deflate_level)), i4)
    if (ncw_inq_var_deflate /= NCW_NOERR) return

    shuffle = int(c_shuffle, i4)
    deflate = int(c_deflate, i4)
    deflate_level = int(c_deflate_level, i4)
  end function ncw_inq_var_deflate

  function ncw_inq_var_fletcher32(ncid, varid, fletcher32)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: fletcher32
    integer(i4) :: ncw_inq_var_fletcher32

    integer(c_int), target :: c_fletcher32

    ncw_inq_var_fletcher32 = int(c_nc_inq_var_fletcher32(int(ncid, c_int), int(varid, c_int), c_loc(c_fletcher32)), i4)
    if (ncw_inq_var_fletcher32 == NCW_NOERR) fletcher32 = int(c_fletcher32, i4)
  end function ncw_inq_var_fletcher32

  function ncw_inq_var_endian(ncid, varid, endianness)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: endianness
    integer(i4) :: ncw_inq_var_endian

    integer(c_int), target :: c_endianness

    ncw_inq_var_endian = int(c_nc_inq_var_endian(int(ncid, c_int), int(varid, c_int), c_loc(c_endianness)), i4)
    if (ncw_inq_var_endian == NCW_NOERR) endianness = int(c_endianness, i4)
  end function ncw_inq_var_endian

  function ncw_inq_varid(ncid, name, varid)
    integer(i4), intent(in) :: ncid
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: varid
    integer(i4) :: ncw_inq_varid

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_varid

    cname = to_c_string(name)
    ncw_inq_varid = int(c_nc_inq_varid(int(ncid, c_int), c_loc(cname(1)), c_loc(c_varid)), i4)
    if (ncw_inq_varid == NCW_NOERR) varid = int(c_varid, i4)
  end function ncw_inq_varid

  function ncw_rename_var(ncid, varid, name)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4) :: ncw_rename_var

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_rename_var = int(c_nc_rename_var(int(ncid, c_int), int(varid, c_int), c_loc(cname(1))), i4)
  end function ncw_rename_var

  function ncw_inquire_attribute(ncid, varid, name, xtype, len, attnum)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(out), optional :: xtype, len, attnum
    integer(i4) :: ncw_inquire_attribute

    integer(i8) :: len64

    ncw_inquire_attribute = ncw_inquire_attribute64(ncid, varid, name, xtype, len64, attnum)
    if (ncw_inquire_attribute /= NCW_NOERR) return

    if (present(len)) then
      if (len64 > huge(len)) then
        len = huge(len)
        ncw_inquire_attribute = NCW_EINVAL
      else
        len = int(len64, i4)
      end if
    end if
  end function ncw_inquire_attribute

  function ncw_inquire_attribute64(ncid, varid, name, xtype, len, attnum)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(out), optional :: xtype, attnum
    integer(i8), intent(out), optional :: len
    integer(i4) :: ncw_inquire_attribute64

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_xtype, c_attnum
    integer(c_size_t), target :: c_len
    type(c_ptr) :: len_ptr

    cname = to_c_string(name)
    len_ptr = c_null_ptr
    if (present(len)) len_ptr = c_loc(c_len)
    ncw_inquire_attribute64 = int(c_nc_inq_att(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            maybe_i4_ptr(xtype, c_xtype), len_ptr), i4)
    if (ncw_inquire_attribute64 /= NCW_NOERR) return

    if (present(xtype)) xtype = int(c_xtype, i4)
    if (present(len)) len = int(c_len, i8)

    if (present(attnum)) then
      ncw_inquire_attribute64 = int(c_nc_inq_attid(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(c_attnum)), i4)
      if (ncw_inquire_attribute64 == NCW_NOERR) attnum = int(c_attnum, i4) + 1_i4
    end if
  end function ncw_inquire_attribute64

  function ncw_inq_attname(ncid, varid, attnum, name)
    integer(i4), intent(in) :: ncid, varid, attnum
    character(len = *), intent(out) :: name
    integer(i4) :: ncw_inq_attname

    character(kind = c_char, len = 1), target :: cname(NCW_MAX_NAME + 1_i4)

    cname = c_null_char
    ncw_inq_attname = int(c_nc_inq_attname(int(ncid, c_int), int(varid, c_int), int(attnum - 1_i4, c_int), c_loc(cname(1))), i4)
    if (ncw_inq_attname == NCW_NOERR) call c_chars_to_fortran(cname, name)
  end function ncw_inq_attname

  function ncw_rename_att(ncid, varid, name, newname)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name, newname
    integer(i4) :: ncw_rename_att

    character(kind = c_char, len = 1), allocatable, target :: cname(:), cnew(:)

    cname = to_c_string(name)
    cnew = to_c_string(newname)
    ncw_rename_att = int(c_nc_rename_att(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(cnew(1))), i4)
  end function ncw_rename_att

  function ncw_copy_att(ncid_in, varid_in, name, ncid_out, varid_out)
    integer(i4), intent(in) :: ncid_in, varid_in, ncid_out, varid_out
    character(len = *), intent(in) :: name
    integer(i4) :: ncw_copy_att

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_copy_att = int(c_nc_copy_att(int(ncid_in, c_int), int(varid_in, c_int), c_loc(cname(1)), int(ncid_out, c_int), &
            int(varid_out, c_int)), i4)
  end function ncw_copy_att

  function ncw_del_att(ncid, varid, name)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4) :: ncw_del_att

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_del_att = int(c_nc_del_att(int(ncid, c_int), int(varid, c_int), c_loc(cname(1))), i4)
  end function ncw_del_att

  function ncw_def_var_fill_sp(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    real(sp), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_sp

    ncw_def_var_fill_sp = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_sp

  function ncw_inq_var_fill_sp(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    real(sp), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_sp

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_sp = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_sp == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_sp
  function ncw_def_var_fill_dp(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    real(dp), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_dp

    ncw_def_var_fill_dp = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_dp

  function ncw_inq_var_fill_dp(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    real(dp), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_dp

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_dp = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_dp == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_dp
  function ncw_def_var_fill_i1(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    integer(i1), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_i1

    ncw_def_var_fill_i1 = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_i1

  function ncw_inq_var_fill_i1(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    integer(i1), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_i1

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_i1 = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_i1 == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_i1
  function ncw_def_var_fill_i2(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    integer(i2), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_i2

    ncw_def_var_fill_i2 = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_i2

  function ncw_inq_var_fill_i2(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    integer(i2), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_i2

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_i2 = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_i2 == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_i2
  function ncw_def_var_fill_i4(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    integer(i4), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_i4

    ncw_def_var_fill_i4 = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_i4

  function ncw_inq_var_fill_i4(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    integer(i4), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_i4

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_i4 = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_i4 == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_i4
  function ncw_def_var_fill_i8(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid, no_fill
    integer(i8), intent(in), target :: fill
    integer(i4) :: ncw_def_var_fill_i8

    ncw_def_var_fill_i8 = int(c_nc_def_var_fill(int(ncid, c_int), int(varid, c_int), int(no_fill, c_int), c_loc(fill)), i4)
  end function ncw_def_var_fill_i8

  function ncw_inq_var_fill_i8(ncid, varid, no_fill, fill)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out) :: no_fill
    integer(i8), intent(out), target :: fill
    integer(i4) :: ncw_inq_var_fill_i8

    integer(c_int), target :: c_no_fill

    ncw_inq_var_fill_i8 = int(c_nc_inq_var_fill(int(ncid, c_int), int(varid, c_int), c_loc(c_no_fill), c_loc(fill)), i4)
    if (ncw_inq_var_fill_i8 == NCW_NOERR) no_fill = int(c_no_fill, i4)
  end function ncw_inq_var_fill_i8

  function ncw_put_att_0d_sp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(sp), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_sp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_sp = int(c_nc_put_att_float(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_FLOAT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_sp

  function ncw_get_att_0d_sp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(sp), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_sp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_sp = int(c_nc_get_att_float(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_sp
  function ncw_put_att_1d_sp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(sp), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_sp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_sp = int(c_nc_put_att_float(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_FLOAT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_sp

  function ncw_get_att_1d_sp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(sp), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_sp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_sp = int(c_nc_get_att_float(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_sp
  function ncw_put_att_0d_dp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(dp), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_dp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_dp = int(c_nc_put_att_double(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_DOUBLE, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_dp

  function ncw_get_att_0d_dp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(dp), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_dp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_dp = int(c_nc_get_att_double(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_dp
  function ncw_put_att_1d_dp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(dp), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_dp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_dp = int(c_nc_put_att_double(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_DOUBLE, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_dp

  function ncw_get_att_1d_dp(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    real(dp), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_dp

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_dp = int(c_nc_get_att_double(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_dp
  function ncw_put_att_0d_i1(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i1), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_i1

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_i1 = int(c_nc_put_att_schar(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_BYTE, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_i1

  function ncw_get_att_0d_i1(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i1), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_i1

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_i1 = int(c_nc_get_att_schar(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_i1
  function ncw_put_att_1d_i1(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i1), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_i1

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_i1 = int(c_nc_put_att_schar(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_BYTE, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_i1

  function ncw_get_att_1d_i1(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i1), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_i1

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_i1 = int(c_nc_get_att_schar(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_i1
  function ncw_put_att_0d_i2(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i2), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_i2

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_i2 = int(c_nc_put_att_short(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_SHORT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_i2

  function ncw_get_att_0d_i2(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i2), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_i2

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_i2 = int(c_nc_get_att_short(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_i2
  function ncw_put_att_1d_i2(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i2), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_i2

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_i2 = int(c_nc_put_att_short(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_SHORT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_i2

  function ncw_get_att_1d_i2(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i2), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_i2

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_i2 = int(c_nc_get_att_short(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_i2
  function ncw_put_att_0d_i4(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_i4

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_i4 = int(c_nc_put_att_int(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_INT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_i4

  function ncw_get_att_0d_i4(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_i4

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_i4 = int(c_nc_get_att_int(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_i4
  function ncw_put_att_1d_i4(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_i4

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_i4 = int(c_nc_put_att_int(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_INT, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_i4

  function ncw_get_att_1d_i4(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i4), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_i4

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_i4 = int(c_nc_get_att_int(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_i4
  function ncw_put_att_0d_i8(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i8), intent(in), target :: values
    integer(i4) :: ncw_put_att_0d_i8

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = 1_c_size_t
    ncw_put_att_0d_i8 = int(c_nc_put_att_longlong(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_INT64, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_0d_i8

  function ncw_get_att_0d_i8(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i8), intent(out), target :: values
    integer(i4) :: ncw_get_att_0d_i8

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_0d_i8 = int(c_nc_get_att_longlong(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_0d_i8
  function ncw_put_att_1d_i8(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i8), intent(in), target :: values(:)
    integer(i4) :: ncw_put_att_1d_i8

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    nvals = int(size(values), c_size_t)
    ncw_put_att_1d_i8 = int(c_nc_put_att_longlong(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            int(NCW_INT64, c_int), nvals, c_loc(values)), i4)
  end function ncw_put_att_1d_i8

  function ncw_get_att_1d_i8(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    integer(i8), intent(out), target :: values(:)
    integer(i4) :: ncw_get_att_1d_i8

    character(kind = c_char, len = 1), allocatable, target :: cname(:)

    cname = to_c_string(name)
    ncw_get_att_1d_i8 = int(c_nc_get_att_longlong(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(values)), i4)
  end function ncw_get_att_1d_i8

  function ncw_put_att_0d_char(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name, values
    integer(i4) :: ncw_put_att_0d_char

    character(kind = c_char, len = 1), allocatable, target :: cname(:), cvalue(:)
    integer(c_size_t) :: nvals

    cname = to_c_string(name)
    cvalue = to_c_string(values)
    nvals = int(len_trim(values, kind=i4), c_size_t)
    ncw_put_att_0d_char = int(c_nc_put_att_text(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), &
            nvals, c_loc(cvalue(1))), i4)
  end function ncw_put_att_0d_char

  function ncw_get_att_0d_char(ncid, varid, name, values)
    integer(i4), intent(in) :: ncid, varid
    character(len = *), intent(in) :: name
    character(len = *), intent(out) :: values
    integer(i4) :: ncw_get_att_0d_char

    character(kind = c_char, len = 1), allocatable, target :: cname(:), cvalue(:)
    integer(i8) :: nvals64
    integer(i4) :: nvals

    cname = to_c_string(name)
    ncw_get_att_0d_char = ncw_inquire_attribute64(ncid, varid, name, len=nvals64)
    if (ncw_get_att_0d_char /= NCW_NOERR) return
    if (.not. ncw_fits_c_size_t(nvals64)) then
      ncw_get_att_0d_char = NCW_EINVAL
      return
    end if
    if (nvals64 > int(huge(0_i4), i8)) then
      ncw_get_att_0d_char = NCW_EINVAL
      return
    end if
    nvals = max(1_i4, int(nvals64, i4))
    allocate(cvalue(nvals))
    cvalue = c_null_char
    ncw_get_att_0d_char = int(c_nc_get_att_text(int(ncid, c_int), int(varid, c_int), c_loc(cname(1)), c_loc(cvalue(1))), i4)
    if (ncw_get_att_0d_char == NCW_NOERR) call c_chars_to_fortran(cvalue, values, stop_at_null=.false.)
  end function ncw_get_att_0d_char

  function ncw_put_var_0d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_sp

    ncw_put_var_0d_sp = ncw_put_data_i4_sp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_sp

  function ncw_put_var64_0d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_sp

    ncw_put_var64_0d_sp = ncw_put_data_i8_sp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_sp

  function ncw_get_var_0d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_sp

    ncw_get_var_0d_sp = ncw_get_data_i4_sp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_sp

  function ncw_get_var64_0d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_sp

    ncw_get_var64_0d_sp = ncw_get_data_i8_sp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_sp
  function ncw_put_var_1d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_sp

    ncw_put_var_1d_sp = ncw_put_data_i4_sp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_sp

  function ncw_put_var64_1d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_sp

    ncw_put_var64_1d_sp = ncw_put_data_i8_sp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_sp

  function ncw_get_var_1d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_sp

    ncw_get_var_1d_sp = ncw_get_data_i4_sp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_sp

  function ncw_get_var64_1d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_sp

    ncw_get_var64_1d_sp = ncw_get_data_i8_sp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_sp
  function ncw_put_var_2d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_sp

    ncw_put_var_2d_sp = ncw_put_data_i4_sp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_sp

  function ncw_put_var64_2d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_sp

    ncw_put_var64_2d_sp = ncw_put_data_i8_sp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_sp

  function ncw_get_var_2d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_sp

    ncw_get_var_2d_sp = ncw_get_data_i4_sp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_sp

  function ncw_get_var64_2d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_sp

    ncw_get_var64_2d_sp = ncw_get_data_i8_sp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_sp
  function ncw_put_var_3d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_sp

    ncw_put_var_3d_sp = ncw_put_data_i4_sp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_sp

  function ncw_put_var64_3d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_sp

    ncw_put_var64_3d_sp = ncw_put_data_i8_sp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_sp

  function ncw_get_var_3d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_sp

    ncw_get_var_3d_sp = ncw_get_data_i4_sp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_sp

  function ncw_get_var64_3d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_sp

    ncw_get_var64_3d_sp = ncw_get_data_i8_sp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_sp
  function ncw_put_var_4d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_sp

    ncw_put_var_4d_sp = ncw_put_data_i4_sp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_sp

  function ncw_put_var64_4d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_sp

    ncw_put_var64_4d_sp = ncw_put_data_i8_sp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_sp

  function ncw_get_var_4d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_sp

    ncw_get_var_4d_sp = ncw_get_data_i4_sp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_sp

  function ncw_get_var64_4d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_sp

    ncw_get_var64_4d_sp = ncw_get_data_i8_sp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_sp
  function ncw_put_var_5d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_sp

    ncw_put_var_5d_sp = ncw_put_data_i4_sp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_sp

  function ncw_put_var64_5d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_sp

    ncw_put_var64_5d_sp = ncw_put_data_i8_sp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_sp

  function ncw_get_var_5d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_sp

    ncw_get_var_5d_sp = ncw_get_data_i4_sp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_sp

  function ncw_get_var64_5d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_sp

    ncw_get_var64_5d_sp = ncw_get_data_i8_sp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_sp
  function ncw_put_var_6d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_sp

    ncw_put_var_6d_sp = ncw_put_data_i4_sp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_sp

  function ncw_put_var64_6d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_sp

    ncw_put_var64_6d_sp = ncw_put_data_i8_sp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_sp

  function ncw_get_var_6d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_sp

    ncw_get_var_6d_sp = ncw_get_data_i4_sp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_sp

  function ncw_get_var64_6d_sp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_sp

    ncw_get_var64_6d_sp = ncw_get_data_i8_sp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_sp
  function ncw_put_var_0d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_dp

    ncw_put_var_0d_dp = ncw_put_data_i4_dp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_dp

  function ncw_put_var64_0d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_dp

    ncw_put_var64_0d_dp = ncw_put_data_i8_dp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_dp

  function ncw_get_var_0d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_dp

    ncw_get_var_0d_dp = ncw_get_data_i4_dp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_dp

  function ncw_get_var64_0d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_dp

    ncw_get_var64_0d_dp = ncw_get_data_i8_dp_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_dp
  function ncw_put_var_1d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_dp

    ncw_put_var_1d_dp = ncw_put_data_i4_dp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_dp

  function ncw_put_var64_1d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_dp

    ncw_put_var64_1d_dp = ncw_put_data_i8_dp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_dp

  function ncw_get_var_1d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_dp

    ncw_get_var_1d_dp = ncw_get_data_i4_dp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_dp

  function ncw_get_var64_1d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_dp

    ncw_get_var64_1d_dp = ncw_get_data_i8_dp_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_dp
  function ncw_put_var_2d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_dp

    ncw_put_var_2d_dp = ncw_put_data_i4_dp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_dp

  function ncw_put_var64_2d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_dp

    ncw_put_var64_2d_dp = ncw_put_data_i8_dp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_dp

  function ncw_get_var_2d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_dp

    ncw_get_var_2d_dp = ncw_get_data_i4_dp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_dp

  function ncw_get_var64_2d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_dp

    ncw_get_var64_2d_dp = ncw_get_data_i8_dp_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_dp
  function ncw_put_var_3d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_dp

    ncw_put_var_3d_dp = ncw_put_data_i4_dp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_dp

  function ncw_put_var64_3d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_dp

    ncw_put_var64_3d_dp = ncw_put_data_i8_dp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_dp

  function ncw_get_var_3d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_dp

    ncw_get_var_3d_dp = ncw_get_data_i4_dp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_dp

  function ncw_get_var64_3d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_dp

    ncw_get_var64_3d_dp = ncw_get_data_i8_dp_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_dp
  function ncw_put_var_4d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_dp

    ncw_put_var_4d_dp = ncw_put_data_i4_dp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_dp

  function ncw_put_var64_4d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_dp

    ncw_put_var64_4d_dp = ncw_put_data_i8_dp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_dp

  function ncw_get_var_4d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_dp

    ncw_get_var_4d_dp = ncw_get_data_i4_dp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_dp

  function ncw_get_var64_4d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_dp

    ncw_get_var64_4d_dp = ncw_get_data_i8_dp_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_dp
  function ncw_put_var_5d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_dp

    ncw_put_var_5d_dp = ncw_put_data_i4_dp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_dp

  function ncw_put_var64_5d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_dp

    ncw_put_var64_5d_dp = ncw_put_data_i8_dp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_dp

  function ncw_get_var_5d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_dp

    ncw_get_var_5d_dp = ncw_get_data_i4_dp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_dp

  function ncw_get_var64_5d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_dp

    ncw_get_var64_5d_dp = ncw_get_data_i8_dp_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_dp
  function ncw_put_var_6d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_dp

    ncw_put_var_6d_dp = ncw_put_data_i4_dp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_dp

  function ncw_put_var64_6d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_dp

    ncw_put_var64_6d_dp = ncw_put_data_i8_dp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_dp

  function ncw_get_var_6d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_dp

    ncw_get_var_6d_dp = ncw_get_data_i4_dp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_dp

  function ncw_get_var64_6d_dp(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_dp

    ncw_get_var64_6d_dp = ncw_get_data_i8_dp_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_dp
  function ncw_put_var_0d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_i1

    ncw_put_var_0d_i1 = ncw_put_data_i4_i1_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_i1

  function ncw_put_var64_0d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_i1

    ncw_put_var64_0d_i1 = ncw_put_data_i8_i1_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_i1

  function ncw_get_var_0d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_i1

    ncw_get_var_0d_i1 = ncw_get_data_i4_i1_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_i1

  function ncw_get_var64_0d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_i1

    ncw_get_var64_0d_i1 = ncw_get_data_i8_i1_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_i1
  function ncw_put_var_1d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_i1

    ncw_put_var_1d_i1 = ncw_put_data_i4_i1_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_i1

  function ncw_put_var64_1d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_i1

    ncw_put_var64_1d_i1 = ncw_put_data_i8_i1_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_i1

  function ncw_get_var_1d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_i1

    ncw_get_var_1d_i1 = ncw_get_data_i4_i1_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_i1

  function ncw_get_var64_1d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_i1

    ncw_get_var64_1d_i1 = ncw_get_data_i8_i1_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_i1
  function ncw_put_var_2d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_i1

    ncw_put_var_2d_i1 = ncw_put_data_i4_i1_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_i1

  function ncw_put_var64_2d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_i1

    ncw_put_var64_2d_i1 = ncw_put_data_i8_i1_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_i1

  function ncw_get_var_2d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_i1

    ncw_get_var_2d_i1 = ncw_get_data_i4_i1_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_i1

  function ncw_get_var64_2d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_i1

    ncw_get_var64_2d_i1 = ncw_get_data_i8_i1_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_i1
  function ncw_put_var_3d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_i1

    ncw_put_var_3d_i1 = ncw_put_data_i4_i1_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_i1

  function ncw_put_var64_3d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_i1

    ncw_put_var64_3d_i1 = ncw_put_data_i8_i1_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_i1

  function ncw_get_var_3d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_i1

    ncw_get_var_3d_i1 = ncw_get_data_i4_i1_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_i1

  function ncw_get_var64_3d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_i1

    ncw_get_var64_3d_i1 = ncw_get_data_i8_i1_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_i1
  function ncw_put_var_4d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_i1

    ncw_put_var_4d_i1 = ncw_put_data_i4_i1_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_i1

  function ncw_put_var64_4d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_i1

    ncw_put_var64_4d_i1 = ncw_put_data_i8_i1_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_i1

  function ncw_get_var_4d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_i1

    ncw_get_var_4d_i1 = ncw_get_data_i4_i1_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_i1

  function ncw_get_var64_4d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_i1

    ncw_get_var64_4d_i1 = ncw_get_data_i8_i1_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_i1
  function ncw_put_var_5d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_i1

    ncw_put_var_5d_i1 = ncw_put_data_i4_i1_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_i1

  function ncw_put_var64_5d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_i1

    ncw_put_var64_5d_i1 = ncw_put_data_i8_i1_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_i1

  function ncw_get_var_5d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_i1

    ncw_get_var_5d_i1 = ncw_get_data_i4_i1_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_i1

  function ncw_get_var64_5d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_i1

    ncw_get_var64_5d_i1 = ncw_get_data_i8_i1_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_i1
  function ncw_put_var_6d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_i1

    ncw_put_var_6d_i1 = ncw_put_data_i4_i1_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_i1

  function ncw_put_var64_6d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_i1

    ncw_put_var64_6d_i1 = ncw_put_data_i8_i1_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_i1

  function ncw_get_var_6d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_i1

    ncw_get_var_6d_i1 = ncw_get_data_i4_i1_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_i1

  function ncw_get_var64_6d_i1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_i1

    ncw_get_var64_6d_i1 = ncw_get_data_i8_i1_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_i1
  function ncw_put_var_0d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_i2

    ncw_put_var_0d_i2 = ncw_put_data_i4_i2_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_i2

  function ncw_put_var64_0d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_i2

    ncw_put_var64_0d_i2 = ncw_put_data_i8_i2_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_i2

  function ncw_get_var_0d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_i2

    ncw_get_var_0d_i2 = ncw_get_data_i4_i2_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_i2

  function ncw_get_var64_0d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_i2

    ncw_get_var64_0d_i2 = ncw_get_data_i8_i2_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_i2
  function ncw_put_var_1d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_i2

    ncw_put_var_1d_i2 = ncw_put_data_i4_i2_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_i2

  function ncw_put_var64_1d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_i2

    ncw_put_var64_1d_i2 = ncw_put_data_i8_i2_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_i2

  function ncw_get_var_1d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_i2

    ncw_get_var_1d_i2 = ncw_get_data_i4_i2_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_i2

  function ncw_get_var64_1d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_i2

    ncw_get_var64_1d_i2 = ncw_get_data_i8_i2_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_i2
  function ncw_put_var_2d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_i2

    ncw_put_var_2d_i2 = ncw_put_data_i4_i2_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_i2

  function ncw_put_var64_2d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_i2

    ncw_put_var64_2d_i2 = ncw_put_data_i8_i2_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_i2

  function ncw_get_var_2d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_i2

    ncw_get_var_2d_i2 = ncw_get_data_i4_i2_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_i2

  function ncw_get_var64_2d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_i2

    ncw_get_var64_2d_i2 = ncw_get_data_i8_i2_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_i2
  function ncw_put_var_3d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_i2

    ncw_put_var_3d_i2 = ncw_put_data_i4_i2_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_i2

  function ncw_put_var64_3d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_i2

    ncw_put_var64_3d_i2 = ncw_put_data_i8_i2_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_i2

  function ncw_get_var_3d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_i2

    ncw_get_var_3d_i2 = ncw_get_data_i4_i2_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_i2

  function ncw_get_var64_3d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_i2

    ncw_get_var64_3d_i2 = ncw_get_data_i8_i2_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_i2
  function ncw_put_var_4d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_i2

    ncw_put_var_4d_i2 = ncw_put_data_i4_i2_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_i2

  function ncw_put_var64_4d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_i2

    ncw_put_var64_4d_i2 = ncw_put_data_i8_i2_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_i2

  function ncw_get_var_4d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_i2

    ncw_get_var_4d_i2 = ncw_get_data_i4_i2_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_i2

  function ncw_get_var64_4d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_i2

    ncw_get_var64_4d_i2 = ncw_get_data_i8_i2_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_i2
  function ncw_put_var_5d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_i2

    ncw_put_var_5d_i2 = ncw_put_data_i4_i2_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_i2

  function ncw_put_var64_5d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_i2

    ncw_put_var64_5d_i2 = ncw_put_data_i8_i2_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_i2

  function ncw_get_var_5d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_i2

    ncw_get_var_5d_i2 = ncw_get_data_i4_i2_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_i2

  function ncw_get_var64_5d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_i2

    ncw_get_var64_5d_i2 = ncw_get_data_i8_i2_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_i2
  function ncw_put_var_6d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_i2

    ncw_put_var_6d_i2 = ncw_put_data_i4_i2_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_i2

  function ncw_put_var64_6d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_i2

    ncw_put_var64_6d_i2 = ncw_put_data_i8_i2_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_i2

  function ncw_get_var_6d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_i2

    ncw_get_var_6d_i2 = ncw_get_data_i4_i2_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_i2

  function ncw_get_var64_6d_i2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_i2

    ncw_get_var64_6d_i2 = ncw_get_data_i8_i2_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_i2
  function ncw_put_var_0d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_i4

    ncw_put_var_0d_i4 = ncw_put_data_i4_i4_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_i4

  function ncw_put_var64_0d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_i4

    ncw_put_var64_0d_i4 = ncw_put_data_i8_i4_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_i4

  function ncw_get_var_0d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_i4

    ncw_get_var_0d_i4 = ncw_get_data_i4_i4_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_i4

  function ncw_get_var64_0d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_i4

    ncw_get_var64_0d_i4 = ncw_get_data_i8_i4_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_i4
  function ncw_put_var_1d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_i4

    ncw_put_var_1d_i4 = ncw_put_data_i4_i4_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_i4

  function ncw_put_var64_1d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_i4

    ncw_put_var64_1d_i4 = ncw_put_data_i8_i4_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_i4

  function ncw_get_var_1d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_i4

    ncw_get_var_1d_i4 = ncw_get_data_i4_i4_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_i4

  function ncw_get_var64_1d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_i4

    ncw_get_var64_1d_i4 = ncw_get_data_i8_i4_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_i4
  function ncw_put_var_2d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_i4

    ncw_put_var_2d_i4 = ncw_put_data_i4_i4_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_i4

  function ncw_put_var64_2d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_i4

    ncw_put_var64_2d_i4 = ncw_put_data_i8_i4_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_i4

  function ncw_get_var_2d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_i4

    ncw_get_var_2d_i4 = ncw_get_data_i4_i4_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_i4

  function ncw_get_var64_2d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_i4

    ncw_get_var64_2d_i4 = ncw_get_data_i8_i4_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_i4
  function ncw_put_var_3d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_i4

    ncw_put_var_3d_i4 = ncw_put_data_i4_i4_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_i4

  function ncw_put_var64_3d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_i4

    ncw_put_var64_3d_i4 = ncw_put_data_i8_i4_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_i4

  function ncw_get_var_3d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_i4

    ncw_get_var_3d_i4 = ncw_get_data_i4_i4_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_i4

  function ncw_get_var64_3d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_i4

    ncw_get_var64_3d_i4 = ncw_get_data_i8_i4_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_i4
  function ncw_put_var_4d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_i4

    ncw_put_var_4d_i4 = ncw_put_data_i4_i4_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_i4

  function ncw_put_var64_4d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_i4

    ncw_put_var64_4d_i4 = ncw_put_data_i8_i4_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_i4

  function ncw_get_var_4d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_i4

    ncw_get_var_4d_i4 = ncw_get_data_i4_i4_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_i4

  function ncw_get_var64_4d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_i4

    ncw_get_var64_4d_i4 = ncw_get_data_i8_i4_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_i4
  function ncw_put_var_5d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_i4

    ncw_put_var_5d_i4 = ncw_put_data_i4_i4_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_i4

  function ncw_put_var64_5d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_i4

    ncw_put_var64_5d_i4 = ncw_put_data_i8_i4_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_i4

  function ncw_get_var_5d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_i4

    ncw_get_var_5d_i4 = ncw_get_data_i4_i4_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_i4

  function ncw_get_var64_5d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_i4

    ncw_get_var64_5d_i4 = ncw_get_data_i8_i4_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_i4
  function ncw_put_var_6d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_i4

    ncw_put_var_6d_i4 = ncw_put_data_i4_i4_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_i4

  function ncw_put_var64_6d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_i4

    ncw_put_var64_6d_i4 = ncw_put_data_i8_i4_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_i4

  function ncw_get_var_6d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_i4

    ncw_get_var_6d_i4 = ncw_get_data_i4_i4_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_i4

  function ncw_get_var64_6d_i4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_i4

    ncw_get_var64_6d_i4 = ncw_get_data_i8_i4_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_i4
  function ncw_put_var_0d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_0d_i8

    ncw_put_var_0d_i8 = ncw_put_data_i4_i8_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_0d_i8

  function ncw_put_var64_0d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_0d_i8

    ncw_put_var64_0d_i8 = ncw_put_data_i8_i8_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_0d_i8

  function ncw_get_var_0d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_0d_i8

    ncw_get_var_0d_i8 = ncw_get_data_i4_i8_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_0d_i8

  function ncw_get_var64_0d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_0d_i8

    ncw_get_var64_0d_i8 = ncw_get_data_i8_i8_0(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_0d_i8
  function ncw_put_var_1d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_1d_i8

    ncw_put_var_1d_i8 = ncw_put_data_i4_i8_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_1d_i8

  function ncw_put_var64_1d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_1d_i8

    ncw_put_var64_1d_i8 = ncw_put_data_i8_i8_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_1d_i8

  function ncw_get_var_1d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_1d_i8

    ncw_get_var_1d_i8 = ncw_get_data_i4_i8_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_1d_i8

  function ncw_get_var64_1d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_1d_i8

    ncw_get_var64_1d_i8 = ncw_get_data_i8_i8_1(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_1d_i8
  function ncw_put_var_2d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_2d_i8

    ncw_put_var_2d_i8 = ncw_put_data_i4_i8_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_2d_i8

  function ncw_put_var64_2d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_2d_i8

    ncw_put_var64_2d_i8 = ncw_put_data_i8_i8_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_2d_i8

  function ncw_get_var_2d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_2d_i8

    ncw_get_var_2d_i8 = ncw_get_data_i4_i8_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_2d_i8

  function ncw_get_var64_2d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_2d_i8

    ncw_get_var64_2d_i8 = ncw_get_data_i8_i8_2(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_2d_i8
  function ncw_put_var_3d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_3d_i8

    ncw_put_var_3d_i8 = ncw_put_data_i4_i8_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_3d_i8

  function ncw_put_var64_3d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_3d_i8

    ncw_put_var64_3d_i8 = ncw_put_data_i8_i8_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_3d_i8

  function ncw_get_var_3d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_3d_i8

    ncw_get_var_3d_i8 = ncw_get_data_i4_i8_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_3d_i8

  function ncw_get_var64_3d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_3d_i8

    ncw_get_var64_3d_i8 = ncw_get_data_i8_i8_3(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_3d_i8
  function ncw_put_var_4d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_4d_i8

    ncw_put_var_4d_i8 = ncw_put_data_i4_i8_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_4d_i8

  function ncw_put_var64_4d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_4d_i8

    ncw_put_var64_4d_i8 = ncw_put_data_i8_i8_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_4d_i8

  function ncw_get_var_4d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_4d_i8

    ncw_get_var_4d_i8 = ncw_get_data_i4_i8_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_4d_i8

  function ncw_get_var64_4d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_4d_i8

    ncw_get_var64_4d_i8 = ncw_get_data_i8_i8_4(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_4d_i8
  function ncw_put_var_5d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_5d_i8

    ncw_put_var_5d_i8 = ncw_put_data_i4_i8_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_5d_i8

  function ncw_put_var64_5d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_5d_i8

    ncw_put_var64_5d_i8 = ncw_put_data_i8_i8_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_5d_i8

  function ncw_get_var_5d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_5d_i8

    ncw_get_var_5d_i8 = ncw_get_data_i4_i8_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_5d_i8

  function ncw_get_var64_5d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_5d_i8

    ncw_get_var64_5d_i8 = ncw_get_data_i8_i8_5(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_5d_i8
  function ncw_put_var_6d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var_6d_i8

    ncw_put_var_6d_i8 = ncw_put_data_i4_i8_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var_6d_i8

  function ncw_put_var64_6d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_var64_6d_i8

    ncw_put_var64_6d_i8 = ncw_put_data_i8_i8_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_put_var64_6d_i8

  function ncw_get_var_6d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var_6d_i8

    ncw_get_var_6d_i8 = ncw_get_data_i4_i8_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var_6d_i8

  function ncw_get_var64_6d_i8(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_var64_6d_i8

    ncw_get_var64_6d_i8 = ncw_get_data_i8_i8_6(ncid, varid, values, start, cnt, stride, map)
  end function ncw_get_var64_6d_i8

  function ncw_put_data_i4_sp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_0 = int(c_nc_put_var_float(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_0 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_0 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_0 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_0

  function ncw_get_data_i4_sp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_0 = int(c_nc_get_var_float(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_0 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_0 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_0 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_0
  function ncw_put_data_i4_sp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_1 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_1 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_1 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_1 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_1

  function ncw_get_data_i4_sp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_1 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_1 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_1 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_1 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_1
  function ncw_put_data_i4_sp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_2 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_2 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_2 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_2 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_2

  function ncw_get_data_i4_sp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_2 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_2 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_2 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_2 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_2
  function ncw_put_data_i4_sp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_3 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_3 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_3 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_3 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_3

  function ncw_get_data_i4_sp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_3 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_3 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_3 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_3 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_3
  function ncw_put_data_i4_sp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_4 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_4 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_4 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_4 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_4

  function ncw_get_data_i4_sp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_4 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_4 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_4 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_4 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_4
  function ncw_put_data_i4_sp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_5 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_5 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_5 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_5 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_5

  function ncw_get_data_i4_sp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_5 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_5 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_5 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_5 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_5
  function ncw_put_data_i4_sp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_sp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_sp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_sp_6 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_sp_6 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_sp_6 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_sp_6 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_sp_6

  function ncw_get_data_i4_sp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_sp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_sp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_sp_6 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_sp_6 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_sp_6 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_sp_6 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_sp_6
  function ncw_put_data_i4_dp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_0 = int(c_nc_put_var_double(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_0 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_0 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_0 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_0

  function ncw_get_data_i4_dp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_0 = int(c_nc_get_var_double(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_0 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_0 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_0 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_0
  function ncw_put_data_i4_dp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_1 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_1 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_1 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_1 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_1

  function ncw_get_data_i4_dp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_1 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_1 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_1 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_1 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_1
  function ncw_put_data_i4_dp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_2 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_2 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_2 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_2 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_2

  function ncw_get_data_i4_dp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_2 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_2 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_2 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_2 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_2
  function ncw_put_data_i4_dp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_3 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_3 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_3 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_3 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_3

  function ncw_get_data_i4_dp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_3 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_3 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_3 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_3 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_3
  function ncw_put_data_i4_dp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_4 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_4 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_4 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_4 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_4

  function ncw_get_data_i4_dp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_4 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_4 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_4 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_4 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_4
  function ncw_put_data_i4_dp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_5 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_5 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_5 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_5 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_5

  function ncw_get_data_i4_dp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_5 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_5 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_5 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_5 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_5
  function ncw_put_data_i4_dp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_dp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_dp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_dp_6 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_dp_6 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_dp_6 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_dp_6 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_dp_6

  function ncw_get_data_i4_dp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_dp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_dp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_dp_6 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_dp_6 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_dp_6 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_dp_6 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_dp_6
  function ncw_put_data_i4_i1_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_0 = int(c_nc_put_var_schar(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_0 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_0 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_0 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_0

  function ncw_get_data_i4_i1_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_0 = int(c_nc_get_var_schar(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_0 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_0 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_0 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_0
  function ncw_put_data_i4_i1_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_1 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_1 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_1 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_1 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_1

  function ncw_get_data_i4_i1_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_1 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_1 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_1 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_1 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_1
  function ncw_put_data_i4_i1_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_2 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_2 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_2 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_2 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_2

  function ncw_get_data_i4_i1_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_2 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_2 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_2 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_2 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_2
  function ncw_put_data_i4_i1_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_3 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_3 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_3 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_3 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_3

  function ncw_get_data_i4_i1_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_3 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_3 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_3 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_3 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_3
  function ncw_put_data_i4_i1_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_4 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_4 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_4 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_4 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_4

  function ncw_get_data_i4_i1_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_4 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_4 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_4 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_4 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_4
  function ncw_put_data_i4_i1_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_5 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_5 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_5 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_5 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_5

  function ncw_get_data_i4_i1_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_5 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_5 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_5 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_5 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_5
  function ncw_put_data_i4_i1_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i1_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i1_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i1_6 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i1_6 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i1_6 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i1_6 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i1_6

  function ncw_get_data_i4_i1_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i1_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i1_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i1_6 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i1_6 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i1_6 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i1_6 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i1_6
  function ncw_put_data_i4_i2_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_0 = int(c_nc_put_var_short(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_0 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_0 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_0 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_0

  function ncw_get_data_i4_i2_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_0 = int(c_nc_get_var_short(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_0 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_0 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_0 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_0
  function ncw_put_data_i4_i2_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_1 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_1 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_1 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_1 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_1

  function ncw_get_data_i4_i2_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_1 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_1 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_1 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_1 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_1
  function ncw_put_data_i4_i2_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_2 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_2 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_2 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_2 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_2

  function ncw_get_data_i4_i2_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_2 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_2 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_2 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_2 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_2
  function ncw_put_data_i4_i2_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_3 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_3 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_3 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_3 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_3

  function ncw_get_data_i4_i2_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_3 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_3 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_3 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_3 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_3
  function ncw_put_data_i4_i2_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_4 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_4 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_4 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_4 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_4

  function ncw_get_data_i4_i2_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_4 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_4 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_4 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_4 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_4
  function ncw_put_data_i4_i2_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_5 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_5 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_5 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_5 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_5

  function ncw_get_data_i4_i2_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_5 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_5 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_5 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_5 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_5
  function ncw_put_data_i4_i2_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i2_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i2_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i2_6 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i2_6 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i2_6 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i2_6 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i2_6

  function ncw_get_data_i4_i2_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i2_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i2_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i2_6 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i2_6 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i2_6 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i2_6 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i2_6
  function ncw_put_data_i4_i4_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_0 = int(c_nc_put_var_int(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_0 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_0 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_0 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_0

  function ncw_get_data_i4_i4_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_0 = int(c_nc_get_var_int(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_0 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_0 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_0 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_0
  function ncw_put_data_i4_i4_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_1 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_1 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_1 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_1 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_1

  function ncw_get_data_i4_i4_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_1 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_1 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_1 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_1 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_1
  function ncw_put_data_i4_i4_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_2 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_2 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_2 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_2 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_2

  function ncw_get_data_i4_i4_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_2 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_2 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_2 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_2 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_2
  function ncw_put_data_i4_i4_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_3 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_3 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_3 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_3 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_3

  function ncw_get_data_i4_i4_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_3 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_3 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_3 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_3 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_3
  function ncw_put_data_i4_i4_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_4 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_4 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_4 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_4 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_4

  function ncw_get_data_i4_i4_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_4 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_4 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_4 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_4 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_4
  function ncw_put_data_i4_i4_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_5 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_5 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_5 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_5 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_5

  function ncw_get_data_i4_i4_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_5 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_5 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_5 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_5 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_5
  function ncw_put_data_i4_i4_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i4_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i4_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i4_6 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i4_6 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i4_6 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i4_6 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i4_6

  function ncw_get_data_i4_i4_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i4_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i4_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i4_6 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i4_6 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i4_6 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i4_6 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i4_6
  function ncw_put_data_i4_i8_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_0 = int(c_nc_put_var_longlong(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_0 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_0 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_0 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_0

  function ncw_get_data_i4_i8_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target :: values
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_0 = int(c_nc_get_var_longlong(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_0 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_0 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_0 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_0
  function ncw_put_data_i4_i8_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_1 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_1 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_1 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_1 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_1

  function ncw_get_data_i4_i8_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_1 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_1 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_1 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_1 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_1
  function ncw_put_data_i4_i8_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_2 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_2 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_2 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_2 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_2

  function ncw_get_data_i4_i8_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_2 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_2 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_2 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_2 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_2
  function ncw_put_data_i4_i8_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_3 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_3 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_3 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_3 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_3

  function ncw_get_data_i4_i8_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_3 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_3 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_3 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_3 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_3
  function ncw_put_data_i4_i8_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_4 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_4 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_4 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_4 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_4

  function ncw_get_data_i4_i8_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_4 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_4 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_4 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_4 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_4
  function ncw_put_data_i4_i8_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_5 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_5 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_5 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_5 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_5

  function ncw_get_data_i4_i8_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_5 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_5 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_5 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_5 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_5
  function ncw_put_data_i4_i8_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i4_i8_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i4_i8_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i4_i8_6 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i4_i8_6 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i4_i8_6 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i4_i8_6 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i4_i8_6

  function ncw_get_data_i4_i8_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i4_i8_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i4(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i4(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i4_i8_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i4_i8_6 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i4_i8_6 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i4_i8_6 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i4_i8_6 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i4_i8_6
  function ncw_put_data_i8_sp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_0 = int(c_nc_put_var_float(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_0 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_0 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_0 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_0

  function ncw_get_data_i8_sp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_0 = int(c_nc_get_var_float(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_0 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_0 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_0 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_0
  function ncw_put_data_i8_sp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_1 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_1 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_1 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_1 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_1

  function ncw_get_data_i8_sp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_1 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_1 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_1 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_1 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_1
  function ncw_put_data_i8_sp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_2 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_2 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_2 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_2 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_2

  function ncw_get_data_i8_sp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_2 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_2 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_2 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_2 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_2
  function ncw_put_data_i8_sp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_3 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_3 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_3 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_3 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_3

  function ncw_get_data_i8_sp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_3 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_3 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_3 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_3 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_3
  function ncw_put_data_i8_sp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_4 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_4 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_4 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_4 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_4

  function ncw_get_data_i8_sp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_4 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_4 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_4 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_4 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_4
  function ncw_put_data_i8_sp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_5 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_5 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_5 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_5 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_5

  function ncw_get_data_i8_sp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_5 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_5 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_5 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_5 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_5
  function ncw_put_data_i8_sp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_sp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_sp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_sp_6 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_sp_6 = int(c_nc_put_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_sp_6 = int(c_nc_put_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_sp_6 = int(c_nc_put_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_sp_6

  function ncw_get_data_i8_sp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(sp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_sp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_sp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_sp_6 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_sp_6 = int(c_nc_get_varm_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_sp_6 = int(c_nc_get_vars_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_sp_6 = int(c_nc_get_vara_float(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_sp_6
  function ncw_put_data_i8_dp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_0 = int(c_nc_put_var_double(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_0 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_0 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_0 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_0

  function ncw_get_data_i8_dp_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_0 = int(c_nc_get_var_double(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_0 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_0 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_0 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_0
  function ncw_put_data_i8_dp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_1 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_1 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_1 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_1 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_1

  function ncw_get_data_i8_dp_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_1 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_1 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_1 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_1 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_1
  function ncw_put_data_i8_dp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_2 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_2 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_2 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_2 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_2

  function ncw_get_data_i8_dp_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_2 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_2 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_2 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_2 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_2
  function ncw_put_data_i8_dp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_3 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_3 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_3 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_3 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_3

  function ncw_get_data_i8_dp_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_3 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_3 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_3 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_3 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_3
  function ncw_put_data_i8_dp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_4 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_4 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_4 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_4 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_4

  function ncw_get_data_i8_dp_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_4 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_4 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_4 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_4 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_4
  function ncw_put_data_i8_dp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_5 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_5 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_5 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_5 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_5

  function ncw_get_data_i8_dp_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_5 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_5 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_5 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_5 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_5
  function ncw_put_data_i8_dp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_dp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_dp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_dp_6 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_dp_6 = int(c_nc_put_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_dp_6 = int(c_nc_put_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_dp_6 = int(c_nc_put_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_dp_6

  function ncw_get_data_i8_dp_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    real(dp), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_dp_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_dp_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_dp_6 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_dp_6 = int(c_nc_get_varm_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_dp_6 = int(c_nc_get_vars_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_dp_6 = int(c_nc_get_vara_double(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_dp_6
  function ncw_put_data_i8_i1_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_0 = int(c_nc_put_var_schar(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_0 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_0 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_0 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_0

  function ncw_get_data_i8_i1_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_0 = int(c_nc_get_var_schar(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_0 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_0 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_0 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_0
  function ncw_put_data_i8_i1_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_1 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_1 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_1 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_1 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_1

  function ncw_get_data_i8_i1_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_1 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_1 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_1 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_1 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_1
  function ncw_put_data_i8_i1_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_2 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_2 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_2 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_2 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_2

  function ncw_get_data_i8_i1_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_2 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_2 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_2 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_2 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_2
  function ncw_put_data_i8_i1_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_3 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_3 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_3 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_3 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_3

  function ncw_get_data_i8_i1_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_3 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_3 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_3 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_3 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_3
  function ncw_put_data_i8_i1_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_4 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_4 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_4 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_4 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_4

  function ncw_get_data_i8_i1_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_4 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_4 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_4 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_4 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_4
  function ncw_put_data_i8_i1_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_5 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_5 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_5 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_5 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_5

  function ncw_get_data_i8_i1_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_5 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_5 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_5 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_5 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_5
  function ncw_put_data_i8_i1_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i1_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i1_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i1_6 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i1_6 = int(c_nc_put_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i1_6 = int(c_nc_put_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i1_6 = int(c_nc_put_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i1_6

  function ncw_get_data_i8_i1_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i1), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i1_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i1_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i1_6 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i1_6 = int(c_nc_get_varm_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i1_6 = int(c_nc_get_vars_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i1_6 = int(c_nc_get_vara_schar(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i1_6
  function ncw_put_data_i8_i2_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_0 = int(c_nc_put_var_short(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_0 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_0 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_0 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_0

  function ncw_get_data_i8_i2_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_0 = int(c_nc_get_var_short(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_0 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_0 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_0 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_0
  function ncw_put_data_i8_i2_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_1 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_1 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_1 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_1 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_1

  function ncw_get_data_i8_i2_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_1 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_1 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_1 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_1 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_1
  function ncw_put_data_i8_i2_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_2 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_2 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_2 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_2 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_2

  function ncw_get_data_i8_i2_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_2 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_2 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_2 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_2 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_2
  function ncw_put_data_i8_i2_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_3 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_3 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_3 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_3 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_3

  function ncw_get_data_i8_i2_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_3 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_3 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_3 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_3 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_3
  function ncw_put_data_i8_i2_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_4 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_4 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_4 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_4 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_4

  function ncw_get_data_i8_i2_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_4 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_4 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_4 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_4 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_4
  function ncw_put_data_i8_i2_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_5 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_5 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_5 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_5 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_5

  function ncw_get_data_i8_i2_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_5 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_5 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_5 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_5 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_5
  function ncw_put_data_i8_i2_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i2_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i2_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i2_6 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i2_6 = int(c_nc_put_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i2_6 = int(c_nc_put_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i2_6 = int(c_nc_put_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i2_6

  function ncw_get_data_i8_i2_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i2), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i2_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i2_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i2_6 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i2_6 = int(c_nc_get_varm_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i2_6 = int(c_nc_get_vars_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i2_6 = int(c_nc_get_vara_short(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i2_6
  function ncw_put_data_i8_i4_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_0 = int(c_nc_put_var_int(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_0 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_0 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_0 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_0

  function ncw_get_data_i8_i4_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_0 = int(c_nc_get_var_int(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_0 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_0 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_0 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_0
  function ncw_put_data_i8_i4_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_1 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_1 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_1 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_1 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_1

  function ncw_get_data_i8_i4_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_1 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_1 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_1 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_1 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_1
  function ncw_put_data_i8_i4_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_2 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_2 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_2 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_2 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_2

  function ncw_get_data_i8_i4_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_2 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_2 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_2 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_2 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_2
  function ncw_put_data_i8_i4_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_3 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_3 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_3 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_3 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_3

  function ncw_get_data_i8_i4_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_3 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_3 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_3 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_3 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_3
  function ncw_put_data_i8_i4_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_4 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_4 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_4 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_4 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_4

  function ncw_get_data_i8_i4_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_4 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_4 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_4 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_4 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_4
  function ncw_put_data_i8_i4_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_5 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_5 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_5 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_5 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_5

  function ncw_get_data_i8_i4_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_5 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_5 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_5 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_5 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_5
  function ncw_put_data_i8_i4_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i4_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i4_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i4_6 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i4_6 = int(c_nc_put_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i4_6 = int(c_nc_put_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i4_6 = int(c_nc_put_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i4_6

  function ncw_get_data_i8_i4_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i4), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i4_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i4_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i4_6 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i4_6 = int(c_nc_get_varm_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i4_6 = int(c_nc_get_vars_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i4_6 = int(c_nc_get_vara_int(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i4_6
  function ncw_put_data_i8_i8_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_0 = int(c_nc_put_var_longlong(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_0 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_0 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_0 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_0

  function ncw_get_data_i8_i8_0(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target :: values
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_0
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(0, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_0 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_0 = int(c_nc_get_var_longlong(int(ncid, c_int), int(varid, c_int), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_0 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_0 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_0 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_0
  function ncw_put_data_i8_i8_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_1 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_1 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_1 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_1 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_1

  function ncw_get_data_i8_i8_1(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_1
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(1, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_1 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_1 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_1 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_1 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_1 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_1
  function ncw_put_data_i8_i8_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_2 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_2 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_2 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_2 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_2

  function ncw_get_data_i8_i8_2(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_2
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(2, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_2 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_2 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_2 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_2 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_2 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_2
  function ncw_put_data_i8_i8_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_3 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_3 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_3 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_3 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_3

  function ncw_get_data_i8_i8_3(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_3
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(3, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_3 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_3 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_3 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_3 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_3 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_3
  function ncw_put_data_i8_i8_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_4 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_4 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_4 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_4 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_4

  function ncw_get_data_i8_i8_4(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_4
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(4, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_4 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_4 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_4 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_4 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_4 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_4
  function ncw_put_data_i8_i8_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_5 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_5 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_5 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_5 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_5

  function ncw_get_data_i8_i8_5(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_5
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(5, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_5 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_5 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_5 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_5 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_5 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_5
  function ncw_put_data_i8_i8_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(in), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_put_data_i8_i8_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_put_data_i8_i8_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_put_data_i8_i8_6 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_put_data_i8_i8_6 = int(c_nc_put_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_put_data_i8_i8_6 = int(c_nc_put_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_put_data_i8_i8_6 = int(c_nc_put_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_put_data_i8_i8_6

  function ncw_get_data_i8_i8_6(ncid, varid, values, start, cnt, stride, map)
    integer(i4), intent(in) :: ncid, varid
    integer(i8), intent(out), target, contiguous :: values(:,:,:,:,:,:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: ncw_get_data_i8_i8_6
    integer(i4) :: slice_rank, status
    integer(c_size_t), allocatable, target :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), allocatable, target :: stride_c(:), map_c(:)
    logical :: use_slice, use_stride, use_map

    slice_rank = ncw_slice_rank_i8(6, start, cnt, stride, map)
    allocate(start_c(max(1, slice_rank)), count_c(max(1, slice_rank)), stride_c(max(1, slice_rank)), map_c(max(1, slice_rank)))
    call ncw_prepare_slicing_i8(shape(values, kind=i8), start, cnt, stride, map, start_c, count_c, stride_c, map_c, &
            use_slice, use_stride, use_map, status)
    if (status /= NCW_NOERR) then
      ncw_get_data_i8_i8_6 = status
      return
    end if
    if (.not. use_slice) then
      ncw_get_data_i8_i8_6 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    else if (use_map) then
      ncw_get_data_i8_i8_6 = int(c_nc_get_varm_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(map_c(1)), c_loc(values)), i4)
    else if (use_stride) then
      ncw_get_data_i8_i8_6 = int(c_nc_get_vars_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(stride_c(1)), c_loc(values)), i4)
    else
      ncw_get_data_i8_i8_6 = int(c_nc_get_vara_longlong(int(ncid, c_int), int(varid, c_int), c_loc(start_c(1)), &
              c_loc(count_c(1)), c_loc(values)), i4)
    end if
  end function ncw_get_data_i8_i8_6

  function ncw_def_var_impl(ncid, name, xtype, dimids, varid, contiguous, chunksizes, deflate_level, shuffle, fletcher32, endianness, &
          cache_size, cache_nelems, cache_preemption, has_chunksizes)
    integer(i4), intent(in) :: ncid, xtype, dimids(:)
    character(len = *), intent(in) :: name
    integer(i4), intent(out) :: varid
    logical, intent(in), optional :: contiguous, shuffle, fletcher32
    integer(i4), intent(in) :: chunksizes(:)
    integer(i4), intent(in), optional :: deflate_level, endianness, cache_size, cache_nelems, cache_preemption
    logical, intent(in) :: has_chunksizes
    integer(i4) :: ncw_def_var_impl

    character(kind = c_char, len = 1), allocatable, target :: cname(:)
    integer(c_int), target :: c_varid
    integer(c_int), target :: c_dimids(max(1, size(dimids)))
    integer(c_size_t), target :: c_chunks(max(1, size(dimids)))
    integer(c_size_t), target :: cache_size_in, cache_nelems_in, cache_size_out, cache_nelems_out
    real(c_float), target :: cache_preemption_in
    real(c_float) :: cache_preemption_out
    integer(i4) :: i
    logical :: want_contiguous, want_shuffle
    type(c_ptr) :: dimids_ptr

    if (present(contiguous)) then
      if (contiguous .and. has_chunksizes) then
        ncw_def_var_impl = NCW_EINVAL
        return
      end if
      if ((.not. contiguous) .and. (.not. has_chunksizes)) then
        ncw_def_var_impl = NCW_EINVAL
        return
      end if
    end if
    if (has_chunksizes .and. size(chunksizes) /= size(dimids)) then
      ncw_def_var_impl = NCW_EINVAL
      return
    end if

    cname = to_c_string(name)
    do i = 1, size(dimids)
      c_dimids(i) = int(dimids(size(dimids) - i + 1), c_int)
      if (has_chunksizes) c_chunks(i) = int(chunksizes(size(dimids) - i + 1), c_size_t)
    end do

    dimids_ptr = c_null_ptr
    if (size(dimids) > 0) dimids_ptr = c_loc(c_dimids(1))
    ncw_def_var_impl = int(c_nc_def_var(int(ncid, c_int), c_loc(cname(1)), int(xtype, c_int), int(size(dimids), c_int), &
            dimids_ptr, c_loc(c_varid)), i4)
    if (ncw_def_var_impl /= NCW_NOERR) return
    varid = int(c_varid, i4)

    want_contiguous = .false.
    if (present(contiguous)) want_contiguous = contiguous
    if (has_chunksizes) then
      ncw_def_var_impl = int(c_nc_def_var_chunking(int(ncid, c_int), int(c_varid, c_int), int(NCW_CHUNKED, c_int), c_loc(c_chunks(1))), i4)
      if (ncw_def_var_impl /= NCW_NOERR) return
    else if (want_contiguous) then
      ncw_def_var_impl = int(c_nc_def_var_chunking(int(ncid, c_int), int(c_varid, c_int), int(NCW_CONTIGUOUS, c_int), c_null_ptr), i4)
      if (ncw_def_var_impl /= NCW_NOERR) return
    end if

    want_shuffle = .false.
    if (present(shuffle)) want_shuffle = shuffle
    if (present(deflate_level)) then
      if (deflate_level > 0_i4) then
        ncw_def_var_impl = int(c_nc_def_var_deflate(int(ncid, c_int), int(c_varid, c_int), &
                int(merge(1_i4, 0_i4, want_shuffle), c_int), 1_c_int, int(deflate_level, c_int)), i4)
        if (ncw_def_var_impl /= NCW_NOERR) return
      end if
    end if

    if (present(fletcher32)) then
      if (fletcher32) then
        ncw_def_var_impl = int(c_nc_def_var_fletcher32(int(ncid, c_int), int(c_varid, c_int), 1_c_int), i4)
        if (ncw_def_var_impl /= NCW_NOERR) return
      end if
    end if

    if (present(endianness)) then
      ncw_def_var_impl = int(c_nc_def_var_endian(int(ncid, c_int), int(c_varid, c_int), int(endianness, c_int)), i4)
      if (ncw_def_var_impl /= NCW_NOERR) return
    end if

    if (present(cache_size) .or. present(cache_nelems) .or. present(cache_preemption)) then
      ncw_def_var_impl = int(c_nc_get_var_chunk_cache(int(ncid, c_int), int(c_varid, c_int), c_loc(cache_size_in), c_loc(cache_nelems_in), &
              c_loc(cache_preemption_in)), i4)
      if (ncw_def_var_impl /= NCW_NOERR) return
      cache_size_out = cache_size_in
      cache_nelems_out = cache_nelems_in
      cache_preemption_out = cache_preemption_in
      if (present(cache_size)) cache_size_out = int(cache_size, c_size_t)
      if (present(cache_nelems)) cache_nelems_out = int(cache_nelems, c_size_t)
      if (present(cache_preemption)) cache_preemption_out = real(cache_preemption, c_float) / 100.0_c_float
      ncw_def_var_impl = int(c_nc_set_var_chunk_cache(int(ncid, c_int), int(c_varid, c_int), cache_size_out, cache_nelems_out, &
              cache_preemption_out), i4)
    end if
  end function ncw_def_var_impl

  subroutine ncw_prepare_slicing_i4(shape_f, start, cnt, stride, map, start_c, count_c, stride_c, map_c, use_slice, use_stride, use_map, &
          status)
    integer(i8), intent(in) :: shape_f(:)
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(c_size_t), intent(out) :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), intent(out) :: stride_c(:), map_c(:)
    logical, intent(out) :: use_slice, use_stride, use_map
    integer(i4), intent(out) :: status

    integer(i4) :: i, j, rank_c, value_rank
    integer(i8) :: value_i8

    rank_c = size(start_c)
    value_rank = size(shape_f)
    status = NCW_NOERR
    start_c = 0_c_size_t
    count_c = 1_c_size_t
    stride_c = 1_c_ptrdiff_t
    map_c = 1_c_ptrdiff_t

    do i = 1, value_rank
      if (.not. ncw_fits_c_size_t(shape_f(i))) then
        status = NCW_ERANGE
        return
      end if
      j = rank_c - i + 1
      count_c(j) = int(shape_f(i), c_size_t)
    end do

    if (present(start)) then
      do i = 1, size(start)
        if (start(i) < 1_i4) then
          status = NCW_EINVAL
          return
        end if
        value_i8 = int(start(i) - 1_i4, i8)
        if (.not. ncw_fits_c_size_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        start_c(j) = int(value_i8, c_size_t)
      end do
    end if
    if (present(cnt)) then
      do i = 1, size(cnt)
        if (cnt(i) < 0_i4) then
          status = NCW_EINVAL
          return
        end if
        value_i8 = int(cnt(i), i8)
        if (.not. ncw_fits_c_size_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        if (.not. present(map)) then
          if (i <= value_rank) then
            if (value_i8 > shape_f(i)) then
              status = NCW_EEDGE
              return
            end if
          else if (value_i8 > 1_i8) then
            status = NCW_EEDGE
            return
          end if
        end if
        j = rank_c - i + 1
        count_c(j) = int(value_i8, c_size_t)
      end do
    end if
    if (present(stride)) then
      do i = 1, size(stride)
        if (stride(i) <= 0_i4) then
          status = NCW_ESTRIDE
          return
        end if
        value_i8 = int(stride(i), i8)
        if (.not. ncw_fits_c_ptrdiff_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        stride_c(j) = int(value_i8, c_ptrdiff_t)
      end do
    end if
    if (present(map)) then
      do i = 1, size(map)
        value_i8 = int(map(i), i8)
        if (.not. ncw_fits_c_ptrdiff_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        map_c(j) = int(value_i8, c_ptrdiff_t)
      end do
    end if

    use_stride = present(stride)
    use_map = present(map)
    use_slice = present(start) .or. present(cnt) .or. use_stride .or. use_map
  end subroutine ncw_prepare_slicing_i4

  subroutine ncw_prepare_slicing_i8(shape_f, start, cnt, stride, map, start_c, count_c, stride_c, map_c, use_slice, use_stride, use_map, &
          status)
    integer(i8), intent(in) :: shape_f(:)
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(c_size_t), intent(out) :: start_c(:), count_c(:)
    integer(c_ptrdiff_t), intent(out) :: stride_c(:), map_c(:)
    logical, intent(out) :: use_slice, use_stride, use_map
    integer(i4), intent(out) :: status

    integer(i4) :: i, j, rank_c, value_rank
    integer(i8) :: value_i8

    rank_c = size(start_c)
    value_rank = size(shape_f)
    status = NCW_NOERR
    start_c = 0_c_size_t
    count_c = 1_c_size_t
    stride_c = 1_c_ptrdiff_t
    map_c = 1_c_ptrdiff_t

    do i = 1, value_rank
      if (.not. ncw_fits_c_size_t(shape_f(i))) then
        status = NCW_ERANGE
        return
      end if
      j = rank_c - i + 1
      count_c(j) = int(shape_f(i), c_size_t)
    end do

    if (present(start)) then
      do i = 1, size(start)
        if (start(i) < 1_i8) then
          status = NCW_EINVAL
          return
        end if
        value_i8 = start(i) - 1_i8
        if (.not. ncw_fits_c_size_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        start_c(j) = int(value_i8, c_size_t)
      end do
    end if
    if (present(cnt)) then
      do i = 1, size(cnt)
        if (cnt(i) < 0_i8) then
          status = NCW_EINVAL
          return
        end if
        value_i8 = cnt(i)
        if (.not. ncw_fits_c_size_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        if (.not. present(map)) then
          if (i <= value_rank) then
            if (value_i8 > shape_f(i)) then
              status = NCW_EEDGE
              return
            end if
          else if (value_i8 > 1_i8) then
            status = NCW_EEDGE
            return
          end if
        end if
        j = rank_c - i + 1
        count_c(j) = int(value_i8, c_size_t)
      end do
    end if
    if (present(stride)) then
      do i = 1, size(stride)
        if (stride(i) <= 0_i8) then
          status = NCW_ESTRIDE
          return
        end if
        value_i8 = stride(i)
        if (.not. ncw_fits_c_ptrdiff_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        stride_c(j) = int(value_i8, c_ptrdiff_t)
      end do
    end if
    if (present(map)) then
      do i = 1, size(map)
        value_i8 = map(i)
        if (.not. ncw_fits_c_ptrdiff_t(value_i8)) then
          status = NCW_ERANGE
          return
        end if
        j = rank_c - i + 1
        map_c(j) = int(value_i8, c_ptrdiff_t)
      end do
    end if

    use_stride = present(stride)
    use_map = present(map)
    use_slice = present(start) .or. present(cnt) .or. use_stride .or. use_map
  end subroutine ncw_prepare_slicing_i8

  function ncw_slice_rank_i4(base_rank, start, cnt, stride, map) result(rank_out)
    integer(i4), intent(in) :: base_rank
    integer(i4), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: rank_out

    rank_out = base_rank
    if (present(start)) rank_out = max(rank_out, size(start))
    if (present(cnt)) rank_out = max(rank_out, size(cnt))
    if (present(stride)) rank_out = max(rank_out, size(stride))
    if (present(map)) rank_out = max(rank_out, size(map))
  end function ncw_slice_rank_i4

  function ncw_slice_rank_i8(base_rank, start, cnt, stride, map) result(rank_out)
    integer(i4), intent(in) :: base_rank
    integer(i8), intent(in), optional :: start(:), cnt(:), stride(:), map(:)
    integer(i4) :: rank_out

    rank_out = base_rank
    if (present(start)) rank_out = max(rank_out, size(start))
    if (present(cnt)) rank_out = max(rank_out, size(cnt))
    if (present(stride)) rank_out = max(rank_out, size(stride))
    if (present(map)) rank_out = max(rank_out, size(map))
  end function ncw_slice_rank_i8

  logical function ncw_fits_c_size_t(value)
    integer(i8), intent(in) :: value

    ncw_fits_c_size_t = value >= 0_i8
    if (.not. ncw_fits_c_size_t) return
    if (int(storage_size(0_c_size_t), i4) <= int(storage_size(0_i8), i4)) then
      ncw_fits_c_size_t = value <= int(huge(0_c_size_t), i8)
    end if
  end function ncw_fits_c_size_t

  logical function ncw_fits_c_ptrdiff_t(value)
    integer(i8), intent(in) :: value
    integer(i8) :: min_value, max_value

    if (int(storage_size(0_c_ptrdiff_t), i4) > int(storage_size(0_i8), i4)) then
      ncw_fits_c_ptrdiff_t = .true.
      return
    end if
    max_value = int(huge(0_c_ptrdiff_t), i8)
    min_value = -max_value - 1_i8
    ncw_fits_c_ptrdiff_t = (value >= min_value) .and. (value <= max_value)
  end function ncw_fits_c_ptrdiff_t

  function maybe_i4_ptr(arg, carg) result(ptr)
    integer(i4), intent(out), optional :: arg
    integer(c_int), target, intent(out) :: carg
    type(c_ptr) :: ptr
    if (present(arg)) then
      ptr = c_loc(carg)
    else
      ptr = c_null_ptr
    end if
  end function maybe_i4_ptr

  function to_c_string(text) result(cbuf)
    character(len = *), intent(in) :: text
    character(kind = c_char, len = 1), allocatable, target :: cbuf(:)
    integer(i4) :: i, n

    n = len_trim(text)
    allocate(cbuf(n + 1_i4))
    do i = 1, n
      cbuf(i) = char(iachar(text(i:i)), kind = c_char)
    end do
    cbuf(n + 1_i4) = c_null_char
  end function to_c_string

  subroutine c_chars_to_fortran(cbuf, text, stop_at_null)
    character(kind = c_char, len = 1), intent(in) :: cbuf(:)
    character(len = *), intent(out) :: text
    logical, intent(in), optional :: stop_at_null
    integer(i4) :: i, n
    logical :: stop_on_null

    text = ""
    stop_on_null = .true.
    if (present(stop_at_null)) stop_on_null = stop_at_null
    n = min(size(cbuf), len(text, kind = i4))
    do i = 1, n
      if (stop_on_null .and. cbuf(i) == c_null_char) exit
      text(i:i) = char(iachar(cbuf(i)))
    end do
  end subroutine c_chars_to_fortran

end module mo_netcdf_wrapper
