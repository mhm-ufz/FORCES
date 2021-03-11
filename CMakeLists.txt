# This cmake file is not meant to be edited for a special setup.
# For special setups use cache line files or command line options, as described a few
# lines ahead concerning module INDEPENDENT builds
cmake_minimum_required(VERSION 3.5)

project(lightweight_fortran_lib Fortran)

# automatically enables testing
include(CTest)
# turn off testing in case the Project is not the main
if(NOT CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    set(BUILD_TESTING OFF)
endif()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/utils.cmake)
# The variable "CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT" can be set before executing cmake via a cache command:
# $cmake -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT:STRING=ON ..
# or cache file:
# $cmake -C ../CMakeCacheFiles/eve ..
# or after executing CMake editing the CMakeCache.txt, preferably with a corresponding cmake editor i.e ccmake
set(CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT OFF CACHE STRING "build the module INDEPENDENT of the module system, so the build in the build tree works even after a module purge")
message(STATUS "build INDEPENDENT of module system ${CMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT}")

# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-form -ffixed-line-length-132")
	set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -pedantic-errors -Wall -W -O -g -Wno-maybe-uninitialized")
	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume byterecl -fp-model=source -m64 -assume realloc_lhs")
	set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -warn all -g -debug -traceback -fp-stack-check -O0 -check all -fstack-protector-all -fstack-security-check")
	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -qoverride-limits")
	cpp_definitions("-DINTEL" "CMAKE_INTEL" "ON" "Code exchange for intel compiler dependent issues")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -colour -unsharedf95 -ideclient")
	# this effectively omits the -C=intovf flag which checks for integer overflow
	# we need to exclude that as the random number generator relies on that technique
	set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -g -nan -O0 -C -C=alias -C=dangling -strict95 -ieee=full")
	set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O4 -ieee=full")
endif()

# library module specific settings
add_subdirectory(./src)

#----------------------------------
# Get version (semantic versioning)
# C.F. semver.org
#----------------------------------
file ( STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/.VERSION" VERSION )
string( REPLACE "." ";" VERSION_LIST ${VERSION} )
list(GET VERSION_LIST 0 VERSION_MAJOR)
list(GET VERSION_LIST 1 VERSION_MINOR)
list(GET VERSION_LIST 2 VERSION_PATCH)
set(PROJECT_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")
message ( STATUS "CMake build configuration for ${PROJECT_NAME} ${PROJECT_VERSION}" )

