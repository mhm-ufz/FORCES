cmake_minimum_required(VERSION 3.12)
# check version file
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
# get version and date from files (version.txt and version_date.txt)
include(version)
get_version(FORCES_VER FORCES_VER_DEV FORCES_DATE)

# create the project
project(FORCES
        VERSION ${FORCES_VER}
        DESCRIPTION "FORtran library for Computational Environmental Systems"
        HOMEPAGE_URL "https://git.ufz.de/chs/lightweight_fortran_lib/"
        LANGUAGES Fortran)

# add full version and date to pre-processor flags (qoutes need in before hand)
add_compile_definitions(FORCESVERSION='${FORCES_VER_DEV}' FORCESDATE='${FORCES_DATE}')

# common (default) options to cmake
# -DCMAKE_BUILD_TYPE=Release     - compile in debug or release mode
# -DBUILD_TESTING=ON             - whether to compile tests
# -DCMAKE_VERBOSE_MAKEFILE=OFF   - see all the commands
# -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT=OFF   - build the library INDEPENDENT of the module system,
# -DCMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX=64     - find extra library paths on some systems (eve.nag62)
#         so the build in the build tree works even after a module purge

# automatically enables testing
include(CTest)

# same with coverage
set(CMAKE_WITH_COVERAGE OFF CACHE STRING "build the module with gcov coverage support")

# The variable "CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT" can be set before executing cmake via a cache command:
# $cmake -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT:STRING=ON ..
# or cache file:
# $cmake -C ../CMakeCacheFiles/eve ..
# or after executing CMake editing the CMakeCache.txt, preferably with a corresponding cmake editor i.e ccmake
option(CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT "build the module INDEPENDENT of the module system, so the build in the build tree works even after a module purge")
message(STATUS "build INDEPENDENT of module system ${CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT}")

# library module specific settings
add_subdirectory(./src)
