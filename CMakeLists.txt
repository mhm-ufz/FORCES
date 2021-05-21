# common (default) options to cmake
# -DCMAKE_BUILD_TYPE=Release     - compile in debug or release mode
# -DBUILD_TESTING=ON             - whether to compile tests
# -DCMAKE_VERBOSE_MAKEFILE=OFF   - see all the commands
# -DCMAKE_BUILD_MODULE_SYSTEM_INDEPENDENT=OFF   - build the library INDEPENDENT of the module system,
# -DCMAKE_FIND_LIBRARY_CUSTOM_LIB_SUFFIX=64     - find extra library paths on some systems (eve.nag62)
#         so the build in the build tree works even after a module purge
cmake_minimum_required(VERSION 3.14)
# additional cmake-modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake-modules)
# get version and date from files (version.txt and version_date.txt)
include(version)
get_version(FORCES_VER FORCES_VER_DEV FORCES_DATE)

# create the project
project(FORCES
  VERSION ${FORCES_VER}
  DESCRIPTION "FORtran library for Computational Environmental Systems"
  HOMEPAGE_URL "https://git.ufz.de/chs/forces/"
  LANGUAGES Fortran
)
# add full version and date to pre-processor flags (qoutes need in before hand)
add_compile_definitions(FORCESVERSION='${FORCES_VER_DEV}' FORCESDATE='${FORCES_DATE}')

# automatically enables testing
option(BUILD_TESTING "Build with (pfUnit) tests." OFF)
include(CTest)
if((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME OR ${LIB_NAME}_BUILD_TESTING) AND BUILD_TESTING)
  if(CMAKE_WITH_COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags()
    SETUP_TARGET_FOR_COVERAGE_LCOV(
      NAME coverage
      EXECUTABLE ctest -j
      DEPENDENCIES ${LIB_NAME}
      BASE_DIRECTORY "${PROJECT_SOURCE_DIR}/src/"
      EXCLUDE "pf_tests/*" "tests/*" "../build/*"
      GENHTML_ARGS -t "FORCES coverage" --html-prolog ../doc/html_files/cov_header.prolog
    )
  endif()
  # add test directories (defined after coverage definition)
  add_subdirectory(./src/pf_tests)
  add_subdirectory(./src/tests)
endif()

# library module specific settings (defined after coverage definition)
add_subdirectory(./src)
