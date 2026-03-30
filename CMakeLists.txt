# common (default) options to cmake
# -DCMAKE_BUILD_TYPE=Release     - compile in debug or release mode
# -DFORCES_BUILD_TESTING=ON      - build FORCES pfUnit tests
# -DFORCES_WITH_NETCDF=OFF       - build without NetCDF support
# -DFORCES_WITH_OpenMP=ON        - build with OpenMP support
# -DFORCES_WITH_MPI=ON           - build with MPI support
# -DFORCES_ENABLE_NATIVE=ON      - enable host-native release tuning for local/HPC builds
# -DCMAKE_VERBOSE_MAKEFILE=OFF   - see all the commands
# -DCMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX=64     - find extra library paths on some systems (eve.nag62)
#         so the build in the build tree works even after a module purge
cmake_minimum_required(VERSION 3.18)
# additional cmake-modules
set(FORCES_CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
# get version and date from files (version.txt and version_date.txt)
include(${FORCES_CMAKE_MODULE_PATH}/version.cmake)
get_version(FORCES SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")

# create the project
project(FORCES
  VERSION ${FORCES_VER}
  DESCRIPTION "FORtran library for Computational Environmental Systems"
  HOMEPAGE_URL "https://git.ufz.de/chs/forces/"
  LANGUAGES Fortran C
)

set (LIB_NAME forces)

# CMake behaviour differs slightly when FORCES is used as a dependency.
set(FORCES_IS_TOP_LEVEL OFF)
if(DEFINED PROJECT_IS_TOP_LEVEL)
  set(FORCES_IS_TOP_LEVEL ${PROJECT_IS_TOP_LEVEL})
elseif(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  set(FORCES_IS_TOP_LEVEL ON)
endif()

# public build configuration
include(CMakeDependentOption)

option(FORCES_BUILD_TESTING "Build FORCES pfUnit tests." OFF)
cmake_dependent_option(FORCES_WITH_COVERAGE "Build with code coverage." OFF
  "FORCES_BUILD_TESTING" OFF)
option(FORCES_WITH_OPTIMIZATION "Building FORCES with optimization routines." ON)
option(FORCES_WITH_NETCDF "Building FORCES with NetCDF support." ON)
option(FORCES_ENABLE_NATIVE "Enable host-native tuning for Release builds." OFF)
option(FORCES_WITH_ISO_FORTRAN_ENV
  "Build the module with kind definitions of ISO_FORTRAN_ENV instead of ISO_C_BINDING." OFF)
option(FORCES_WITH_MPI "Build the module with MPI support." OFF)
option(FORCES_WITH_OpenMP "Build the module with OpenMP support." OFF)
set(FORCES_EXE "" CACHE FILEPATH "Path to an executable linking against forces.")
set(FORCES_TEST_GLOB "*.pf" CACHE STRING "Globbing expression for pFUnit tests to run.")
set(LOG_TRACE "AUTO" CACHE STRING "Control log trace level (AUTO (debug - ON), ON, OFF)")
set_property(CACHE LOG_TRACE PROPERTY STRINGS AUTO ON OFF)
set(LOG_DEBUG "AUTO" CACHE STRING "Control log debug level (AUTO (release - OFF), ON, OFF)")
set_property(CACHE LOG_DEBUG PROPERTY STRINGS AUTO ON OFF)

if(FORCES_IS_TOP_LEVEL)
  set(BUILD_TESTING "${FORCES_BUILD_TESTING}" CACHE BOOL "Enable testing via CTest.")
  include(CTest)
endif()

if(FORCES_WITH_COVERAGE)
  if(NOT BUILD_TESTING)
    message(FATAL_ERROR "FORCES: FORCES_WITH_COVERAGE requires BUILD_TESTING=ON.")
  endif()
  if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    message(FATAL_ERROR "FORCES: FORCES_WITH_COVERAGE requires GNU Fortran.")
  endif()
endif()

# library module specific settings
add_subdirectory(./src)

if(FORCES_EXE)
  if(IS_ABSOLUTE ${FORCES_EXE})
    set(EXE_PATH ${FORCES_EXE})
  else()
    set(EXE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${FORCES_EXE}")
  endif()
  if(EXISTS ${EXE_PATH})
    message(STATUS "FORCES: found executable: ${EXE_PATH}")
    add_executable(main ${EXE_PATH})
    target_link_libraries(main PRIVATE forces)
  else()
    message(FATAL_ERROR "FORCES: could not find executable: ${EXE_PATH}")
  endif()
endif()

# add full version and date to pre-processor flags (qoutes need in before hand)
target_compile_definitions(${LIB_NAME} PRIVATE
  FORCESVERSION='${FORCES_VER_DEV}'
  FORCESDATE='${FORCES_DATE}'
)

# prepare testing
if(FORCES_BUILD_TESTING AND BUILD_TESTING)
  message(STATUS "FORCES: build tests")
  add_subdirectory(./src/pf_tests)
  # prepare coverage
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU" AND FORCES_WITH_COVERAGE)
    include(${FORCES_CMAKE_MODULE_PATH}/CodeCoverage.cmake)
    append_coverage_compiler_flags_to_target(${LIB_NAME})
    SETUP_TARGET_FOR_COVERAGE_LCOV(
      NAME coverage
      EXECUTABLE ctest
      DEPENDENCIES ${LIB_NAME}
      EXCLUDE "src/pf_tests/*" "${CMAKE_CURRENT_BINARY_DIR}/*"
      LCOV_ARGS --no-external
      GENHTML_ARGS -t "${LIB_NAME} coverage" --html-prolog ${CMAKE_CURRENT_SOURCE_DIR}/doc/html_files/cov_header.prolog
    )
  endif()
endif()
