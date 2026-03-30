# FindYAC.cmake - find YAC coupler libraries
# ==========================================
# safely find YAC
# To specify a particular YAC library, use
#
#     -DYAC_ROOT=/path/to/yac
#
# or set environment variable YAC_ROOT=/path/to/yac
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

# pkg-config uses ':' on Unix-like systems and ';' on Windows.
set(_YAC_pkg_config_separator ":")
if(CMAKE_HOST_WIN32)
  set(_YAC_pkg_config_separator ";")
endif()

set(_YAC_root_hints)
if(DEFINED YAC_ROOT AND NOT YAC_ROOT STREQUAL "")
  list(APPEND _YAC_root_hints "${YAC_ROOT}")
endif()
if(DEFINED ENV{YAC_ROOT} AND NOT "$ENV{YAC_ROOT}" STREQUAL "")
  list(APPEND _YAC_root_hints "$ENV{YAC_ROOT}")
endif()
list(REMOVE_DUPLICATES _YAC_root_hints)

set(_YAC_pkg_config_paths)
foreach(_YAC_root IN LISTS _YAC_root_hints)
  foreach(_YAC_suffix lib/pkgconfig lib64/pkgconfig)
    if(IS_DIRECTORY "${_YAC_root}/${_YAC_suffix}")
      list(APPEND _YAC_pkg_config_paths "${_YAC_root}/${_YAC_suffix}")
    endif()
  endforeach()
endforeach()

if(DEFINED ENV{PKG_CONFIG_PATH})
  set(_YAC_original_pkg_config_path "$ENV{PKG_CONFIG_PATH}")
  set(_YAC_had_pkg_config_path TRUE)
else()
  set(_YAC_had_pkg_config_path FALSE)
endif()

if(_YAC_pkg_config_paths)
  list(JOIN _YAC_pkg_config_paths "${_YAC_pkg_config_separator}" _YAC_root_pkg_config_path)
  if(_YAC_had_pkg_config_path)
    set(ENV{PKG_CONFIG_PATH} "${_YAC_root_pkg_config_path}${_YAC_pkg_config_separator}$ENV{PKG_CONFIG_PATH}")
  else()
    set(ENV{PKG_CONFIG_PATH} "${_YAC_root_pkg_config_path}")
  endif()
endif()

find_package(PkgConfig QUIET)
if(PkgConfig_FOUND)
  pkg_check_modules(YAC_PKG QUIET IMPORTED_TARGET yac)
endif()

if(_YAC_had_pkg_config_path)
  set(ENV{PKG_CONFIG_PATH} "${_YAC_original_pkg_config_path}")
else()
  unset(ENV{PKG_CONFIG_PATH})
endif()

if(NOT PkgConfig_FOUND)
  set(_YAC_failure_reason "PkgConfig is required to locate YAC")
elseif(NOT YAC_PKG_FOUND)
  set(_YAC_failure_reason "pkg-config could not locate package 'yac'")
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(YAC
  REQUIRED_VARS PkgConfig_FOUND YAC_PKG_FOUND
  VERSION_VAR YAC_PKG_VERSION
  REASON_FAILURE_MESSAGE "${_YAC_failure_reason}"
  HANDLE_COMPONENTS
)

if(YAC_FOUND AND NOT TARGET YAC::YAC)
  add_library(YAC::YAC INTERFACE IMPORTED)
  set_property(TARGET YAC::YAC PROPERTY
    INTERFACE_LINK_LIBRARIES PkgConfig::YAC_PKG
  )
  if(YAC_PKG_INCLUDE_DIRS)
    set_property(TARGET YAC::YAC PROPERTY
      INTERFACE_INCLUDE_DIRECTORIES "${YAC_PKG_INCLUDE_DIRS}"
    )
  endif()
endif()
