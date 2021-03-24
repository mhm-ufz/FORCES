cmake_minimum_required(VERSION 3.12)
# check version file
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/version.txt")
	file(STRINGS "version.txt" FORCESVERSION LIMIT_COUNT 1)
else()
	set(FORCESVERSION "0.0.0-dev0") # default version
endif()

# version should be of the form (semver.org):
# - 1.2.3-dev0 (development with number)
# - 1.2.3-rc1  (release candidate with number)
# - 1.2.3      (release)
# remove possible "v" prefix and find major.minor.patch version
string(REGEX MATCH "^v?([0-9]+)" _ ${FORCESVERSION})
set(ver_major ${CMAKE_MATCH_1})
string(REGEX MATCH "^v?[0-9]+\.([0-9]+)" _ ${FORCESVERSION})
set(ver_minor ${CMAKE_MATCH_1})
string(REGEX MATCH "^v?[0-9]+\.[0-9]+\.([0-9]+)" _ ${FORCESVERSION})
set(ver_patch ${CMAKE_MATCH_1})
# find pre-release tag
string(REGEX MATCH ".*-(.+)" _ ${FORCESVERSION})
set(ver_pre ${CMAKE_MATCH_1})

# create the version string for cmake (fill up with 0)
if ("${ver_major}" STREQUAL "")
	set(ver_major 0) # default version
endif()
if ("${ver_minor}" STREQUAL "")
	set(ver_minor 0) # default version
endif()
if ("${ver_patch}" STREQUAL "")
	set(ver_patch 0) # default version
endif()
set(ver_final ${ver_major}.${ver_minor}.${ver_patch}) # full version

# check date file (if not a development version)
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/version_date.txt" AND (NOT (${ver_pre} MATCHES "^dev.*")))
	file(STRINGS "version_date.txt" FORCESDATE LIMIT_COUNT 1)
else()
	string(TIMESTAMP FORCESDATE "%Y-%m-%d") # current date
endif()

# create the project
project(FORCES
        VERSION ${ver_final}
        DESCRIPTION "FORtran library for Computational Environmental Systems"
        HOMEPAGE_URL "https://git.ufz.de/chs/lightweight_fortran_lib/"
        LANGUAGES Fortran)
message("FORCES VERSION: ${FORCES_VERSION} (from ${FORCESVERSION})")
message("FORCES DATE:    ${FORCESDATE}")

# common (default) options to cmake
# -DCMAKE_BUILD_TYPE=Release     - compile in debug or release mode
# -DBUILD_TESTING=ON             - whether to compile tests
# -DCMAKE_VERBOSE_MAKEFILE=OFF   - see all the commands
# -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT=OFF   - build the library INDEPENDENT of the module system,
# -DCMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX=64     - find extra library paths on some systems (eve.nag62)
#         so the build in the build tree works even after a module purge

# automatically enables testing
include(CTest)

# add version to pre-processor flags (qoutes need in before hand)
add_compile_definitions(FORCESVERSION='${FORCESVERSION}')
# add date to pre-processor flags (qoutes need in before hand)
add_compile_definitions(FORCESDATE='${FORCESDATE}')

# same with coverage
set(CMAKE_WITH_COVERAGE OFF CACHE STRING "build the module with gcov coverage support")

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake)

# The variable "CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT" can be set before executing cmake via a cache command:
# $cmake -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT:STRING=ON ..
# or cache file:
# $cmake -C ../CMakeCacheFiles/eve ..
# or after executing CMake editing the CMakeCache.txt, preferably with a corresponding cmake editor i.e ccmake
option(CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT "build the module INDEPENDENT of the module system, so the build in the build tree works even after a module purge")
message(STATUS "build INDEPENDENT of module system ${CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT}")

# library module specific settings
add_subdirectory(./src)
