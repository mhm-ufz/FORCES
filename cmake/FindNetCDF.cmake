# FindNetCDF.cmake - find NetCDF libraries
# ========================================
# safely find netcdf c/fortran
# To specify a particular NetCDF library, use
#
#     cmake -DNetCDF_ROOT=/path/to/netcdf -B build
#
# or set environment variable NetCDF_ROOT=/path/to/netcdf
#
# MIT License
# -----------
#[[
  Copyright (c) 2020 - 2026 CHS Developers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
]]

# function to search for netcdf-C
function(search_netcdf_c)

  if(PkgConfig_FOUND AND NOT NetCDF_C_LIBRARY)
    pkg_search_module(pkg_conf_nc QUIET netcdf)
  endif()

  # include dirs
  find_path(NetCDF_C_INCLUDE_DIR
    NAMES netcdf.h
    HINTS ${pkg_conf_nc_INCLUDE_DIRS}
  )
  if(NOT NetCDF_C_INCLUDE_DIR)
    return()
  endif()

  # library
  find_library(NetCDF_C_LIBRARY
    NAMES netcdf
    HINTS ${pkg_conf_nc_LIBRARY_DIRS} ${pkg_conf_nc_LIBDIR}
  )
  if(NOT NetCDF_C_LIBRARY)
    return()
  endif()

  # set required variables in parent scope
  set(NetCDF_C_FOUND true PARENT_SCOPE)
  set(NetCDF_C_INCLUDE_DIR ${NetCDF_C_INCLUDE_DIR} PARENT_SCOPE)
  set(NetCDF_C_LIBRARY ${NetCDF_C_LIBRARY} PARENT_SCOPE)

endfunction(search_netcdf_c)

# function to search for netcdf-Fortran
function(search_netcdf_fortran)

  if(PkgConfig_FOUND AND NOT NetCDF_Fortran_LIBRARY)
    pkg_search_module(pkg_conf_nf QUIET netcdf-fortran netcdf)
  endif()

  # include dirs
  find_path(NetCDF_Fortran_INCLUDE_DIR
    names netcdf.mod
    HINTS ${pkg_conf_nf_INCLUDE_DIRS}
  )
  if(NOT NetCDF_Fortran_INCLUDE_DIR)
    return()
  endif()

  # library
  find_library(NetCDF_Fortran_LIBRARY
    NAMES netcdff
    HINTS ${pkg_conf_nf_LIBRARY_DIRS} ${pkg_conf_nf_LIBDIR}
  )
  if(NOT NetCDF_Fortran_LIBRARY)
    return()
  endif()

  # set required variables in parent scope
  set(NetCDF_Fortran_FOUND true PARENT_SCOPE)
  set(NetCDF_Fortran_INCLUDE_DIR ${NetCDF_Fortran_INCLUDE_DIR} PARENT_SCOPE)
  set(NetCDF_Fortran_LIBRARY ${NetCDF_Fortran_LIBRARY} PARENT_SCOPE)

endfunction(search_netcdf_fortran)

# resolve a usable filesystem path from an imported library target
function(resolve_imported_target_location imported_target out_var)

  set(_location "")
  set(_config_candidates)

  if(CMAKE_BUILD_TYPE)
    string(TOUPPER "${CMAKE_BUILD_TYPE}" _build_type)
    list(APPEND _config_candidates "${_build_type}")
  endif()

  get_target_property(_imported_configs "${imported_target}" IMPORTED_CONFIGURATIONS)
  if(_imported_configs AND NOT _imported_configs STREQUAL "_imported_configs-NOTFOUND")
    list(APPEND _config_candidates ${_imported_configs})
  endif()
  list(REMOVE_DUPLICATES _config_candidates)

  foreach(_config IN LISTS _config_candidates)
    get_target_property(_location "${imported_target}" "IMPORTED_LOCATION_${_config}")
    if(_location AND NOT _location STREQUAL "_location-NOTFOUND" AND EXISTS "${_location}")
      set(${out_var} "${_location}" PARENT_SCOPE)
      return()
    endif()
  endforeach()

  get_target_property(_location "${imported_target}" IMPORTED_LOCATION)
  if(_location AND NOT _location STREQUAL "_location-NOTFOUND" AND EXISTS "${_location}")
    set(${out_var} "${_location}" PARENT_SCOPE)
    return()
  endif()

  set(${out_var} "" PARENT_SCOPE)

endfunction(resolve_imported_target_location)

# search for PkgConfig
find_package(PkgConfig QUIET)

# keep the documented root hint working for the inner netCDF config-package lookup
if(DEFINED NetCDF_ROOT AND NOT DEFINED netCDF_ROOT)
  set(netCDF_ROOT "${NetCDF_ROOT}")
endif()
if(DEFINED ENV{NetCDF_ROOT} AND NOT DEFINED ENV{netCDF_ROOT})
  set(ENV{netCDF_ROOT} "$ENV{NetCDF_ROOT}")
endif()

# try CMake built-in finder for NetCDF-c.
set(_NetCDF_C_CONFIG_TARGET "")

find_package(netCDF CONFIG QUIET)
if(netCDF_FOUND)
  set(NetCDF_VERSION "${NetCDFVersion}")

  if(TARGET netCDF::netcdf)
    set(_NetCDF_C_CONFIG_TARGET netCDF::netcdf)

    get_target_property(NetCDF_C_INCLUDE_DIRS "${_NetCDF_C_CONFIG_TARGET}" INTERFACE_INCLUDE_DIRECTORIES)
    if(NetCDF_C_INCLUDE_DIRS STREQUAL "NetCDF_C_INCLUDE_DIRS-NOTFOUND")
      set(NetCDF_C_INCLUDE_DIRS "")
    endif()
    if(NOT NetCDF_C_INCLUDE_DIRS AND netCDF_INCLUDE_DIR)
      set(NetCDF_C_INCLUDE_DIRS "${netCDF_INCLUDE_DIR}")
    endif()
    if(NetCDF_C_INCLUDE_DIRS)
      list(REMOVE_DUPLICATES NetCDF_C_INCLUDE_DIRS)
    endif()

    if(netCDF_INCLUDE_DIR)
      set(NetCDF_C_INCLUDE_DIR "${netCDF_INCLUDE_DIR}")
    elseif(NetCDF_C_INCLUDE_DIRS)
      list(GET NetCDF_C_INCLUDE_DIRS 0 NetCDF_C_INCLUDE_DIR)
    endif()

    resolve_imported_target_location("${_NetCDF_C_CONFIG_TARGET}" NetCDF_C_LIBRARY)
    if(NOT NetCDF_C_LIBRARY)
      search_netcdf_c()
      if(NetCDF_C_INCLUDE_DIRS)
        if(netCDF_INCLUDE_DIR)
          set(NetCDF_C_INCLUDE_DIR "${netCDF_INCLUDE_DIR}")
        elseif(NOT NetCDF_C_INCLUDE_DIR)
          list(GET NetCDF_C_INCLUDE_DIRS 0 NetCDF_C_INCLUDE_DIR)
        endif()
      else()
        set(NetCDF_C_INCLUDE_DIRS "${NetCDF_C_INCLUDE_DIR}")
      endif()
    endif()

    if(NetCDF_C_LIBRARY)
      set(NetCDF_C_FOUND true)
      set(NetCDF_C_LIBRARIES "${NetCDF_C_LIBRARY}")
    else()
      unset(NetCDF_C_FOUND)
      unset(NetCDF_C_INCLUDE_DIR)
      unset(NetCDF_C_INCLUDE_DIRS)
      unset(NetCDF_C_LIBRARY)
      unset(NetCDF_C_LIBRARIES)
    endif()
  elseif(netCDF_LIBRARIES)
    set(_NetCDF_C_CONFIG_LIBRARIES "${netCDF_LIBRARIES}")
    list(GET _NetCDF_C_CONFIG_LIBRARIES 0 _NetCDF_C_CONFIG_LIBRARY)
    if(IS_ABSOLUTE "${_NetCDF_C_CONFIG_LIBRARY}" AND EXISTS "${_NetCDF_C_CONFIG_LIBRARY}")
      set(NetCDF_C_FOUND true)
      set(NetCDF_C_INCLUDE_DIR "${netCDF_INCLUDE_DIR}")
      set(NetCDF_C_INCLUDE_DIRS "${netCDF_INCLUDE_DIR}")
      set(NetCDF_C_LIBRARY "${_NetCDF_C_CONFIG_LIBRARY}")
      set(NetCDF_C_LIBRARIES "${_NetCDF_C_CONFIG_LIBRARIES}")
    endif()
  endif()
endif()

if(NOT NetCDF_C_LIBRARY)
  search_netcdf_c()
  if(NetCDF_C_LIBRARY)
    set(NetCDF_C_LIBRARIES "${NetCDF_C_LIBRARY}")
    set(NetCDF_C_INCLUDE_DIRS "${NetCDF_C_INCLUDE_DIR}")
  endif()
endif()

set(_nc_req_vars ${NetCDF_C_LIBRARY})

# search for netcdf-fortran if wanted
if(Fortran IN_LIST NetCDF_FIND_COMPONENTS)
  search_netcdf_fortran()
  list(APPEND _nc_req_vars ${NetCDF_Fortran_LIBRARY})
endif()

# hide the cached variables from cmake GUI
mark_as_advanced(
  NetCDF_C_INCLUDE_DIR
  NetCDF_Fortran_INCLUDE_DIR
  NetCDF_C_LIBRARY
  NetCDF_Fortran_LIBRARY
)

# check the requirements
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDF
  REQUIRED_VARS _nc_req_vars
  HANDLE_COMPONENTS
)

# define imported targets
if(NetCDF_FOUND)
  if(NOT TARGET NetCDF::NetCDF_C)
    if(_NetCDF_C_CONFIG_TARGET)
      add_library(NetCDF::NetCDF_C INTERFACE IMPORTED)
      set_property(TARGET NetCDF::NetCDF_C PROPERTY
        INTERFACE_LINK_LIBRARIES "${_NetCDF_C_CONFIG_TARGET}"
      )
      if(NetCDF_C_INCLUDE_DIRS)
        set_property(TARGET NetCDF::NetCDF_C PROPERTY
          INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_C_INCLUDE_DIRS}"
        )
      endif()
    else()
      add_library(NetCDF::NetCDF_C UNKNOWN IMPORTED)
      set_target_properties(NetCDF::NetCDF_C PROPERTIES
        IMPORTED_LOCATION "${NetCDF_C_LIBRARY}"
        INTERFACE_LINK_LIBRARIES "${NetCDF_C_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_C_INCLUDE_DIR}"
      )
    endif()
  endif()
  if(NetCDF_Fortran_FOUND)
    set(NetCDF_Fortran_INCLUDE_DIRS ${NetCDF_Fortran_INCLUDE_DIR})
    set(NetCDF_Fortran_LIBRARIES ${NetCDF_Fortran_LIBRARY})
    if(NOT TARGET NetCDF::NetCDF_Fortran)
      add_library(NetCDF::NetCDF_Fortran UNKNOWN IMPORTED)
      set_target_properties(NetCDF::NetCDF_Fortran PROPERTIES
        IMPORTED_LOCATION "${NetCDF_Fortran_LIBRARY}"
        INTERFACE_LINK_LIBRARIES "${NetCDF_Fortran_LIBRARY};NetCDF::NetCDF_C"
        INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_Fortran_INCLUDE_DIR}"
      )
    endif()
  endif()
endif()
