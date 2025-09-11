# common (default) options to cmake
# -DCMAKE_BUILD_TYPE=Release     - compile in debug or release mode
# -DBUILD_TESTING=ON             - whether to compile tests
# -DCMAKE_VERBOSE_MAKEFILE=OFF   - see all the commands
# -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT=OFF   - build the library INDEPENDENT of the module system,
# -DCMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX=64     - find extra library paths on some systems (eve.nag62)
#         so the build in the build tree works even after a module purge
cmake_minimum_required(VERSION 3.14)
# additional cmake-modules
set(FORCES_CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
# get version and date from files (version.txt and version_date.txt)
include(${FORCES_CMAKE_MODULE_PATH}/version.cmake)
get_version(FORCES_VER FORCES_VER_DEV FORCES_DATE)

# create the project
project(FORCES
  VERSION ${FORCES_VER}
  DESCRIPTION "FORtran library for Computational Environmental Systems"
  HOMEPAGE_URL "https://git.ufz.de/chs/forces/"
  LANGUAGES Fortran C
)

# library module specific settings (defined after coverage definition)
add_subdirectory(./src)
set (LIB_NAME forces)

set(FORCES_EXE "" CACHE FILEPATH "Path to an executable linking against forces.")
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
option(BUILD_TESTING "Build with (pfUnit) tests." OFF)
set(BUILD_FORCES_TESTS OFF)
if((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME OR ${LIB_NAME}_BUILD_TESTING) AND BUILD_TESTING)
  set(BUILD_FORCES_TESTS ON)
endif()
include(CTest)

# add test directories
if(BUILD_FORCES_TESTS)
  message(STATUS "${PROJECT_NAME}: build tests")
  add_subdirectory(./src/pf_tests)
endif()

# prepare coverage
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU" AND CMAKE_WITH_COVERAGE AND BUILD_FORCES_TESTS)
  include(${FORCES_CMAKE_MODULE_PATH}/CodeCoverage.cmake)
  target_compile_options(${LIB_NAME} PRIVATE -g --coverage)
  target_link_libraries(${LIB_NAME} PRIVATE gcov)
  SETUP_TARGET_FOR_COVERAGE_LCOV(
    NAME coverage
    EXECUTABLE ctest
    DEPENDENCIES ${LIB_NAME}
    EXCLUDE "src/pf_tests/*" "${CMAKE_CURRENT_BINARY_DIR}/*"
    LCOV_ARGS --no-external
    GENHTML_ARGS -t "${LIB_NAME} coverage" --html-prolog ../doc/html_files/cov_header.prolog
  )
endif()
