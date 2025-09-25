set(FORCES_CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

set (LIB_NAME forces)

# add sources
file(GLOB sources ./mo_*.f90 ./mo_*.F90)
# find preprocessor (fypp)
include(${FORCES_CMAKE_MODULE_PATH}/fortranpreprocessor.cmake)
find_program(FYPP fypp)
if(NOT FYPP)
  message(WARNING "Preprocessor fypp not found!")
else()
  message(STATUS "Preprocessor fypp found!")
  # Create a list of the files to be preprocessed
  set(fppFiles mo_netcdf.fypp mo_poly.fypp mo_sentinel.fypp)
  # Custom preprocessor flags
  if(DEFINED CMAKE_MAXIMUM_RANK)
    set(fyppFlags "-DMAXRANK=${CMAKE_MAXIMUM_RANK}")
  # Fortran03 standard supports rank 15 arrays, activate here, if required
  # elseif(f03rank)
  #   set(fyppFlags)
  else()
    set(fyppFlags "-DVERSION90")
  endif()
  # Pre-process: .fpp -> .f90 via Fypp
  fypp_f90("${fyppFlags}" "${fppFiles}" fyppOutFiles)
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_poly.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_sentinel.f90$")
endif()

# option for optimization routines
option(FORCES_WITH_OPTIMIZATION "Building FORCES with optimization routines." ON)
if (FORCES_WITH_OPTIMIZATION)
  message(STATUS "FORCES: provide optimization routines")
else()
  message(STATUS "FORCES: optimization routines not wanted")
  list(FILTER sources EXCLUDE REGEX ".*mo_sce.F90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_mcmc.F90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_anneal.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_dds.F90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_errormeasures.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_likelihood.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_opt_functions.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_cost.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_optimizee.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_opt_eval_utils.f90$")
endif()

# option for NetCDF support
option(FORCES_WITH_NETCDF "Building FORCES with NetCDF support." ON)
if (FORCES_WITH_NETCDF)
  message(STATUS "FORCES: use NetCDF")
else()
  message(STATUS "FORCES: no NetCDF support wanted")
  list(FILTER fyppOutFiles EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_ncwrite.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_ncread.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_grid_io.f90$")
endif()

# create library
add_library(${LIB_NAME} ${sources} ${fyppOutFiles})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../include)

# control mo_kind
option(FORCES_WITH_ISO_FORTRAN_ENV "build the module with kind definitions of ISO_FORTRAN_ENV instead of ISO_C_BINDING" OFF)
if (FORCES_WITH_ISO_FORTRAN_ENV)
  message(STATUS "FORCES: use ISO_FORTRAN_ENV for mo_kind")
  target_compile_definitions(${LIB_NAME} PRIVATE FORCES_WITH_ISO_FORTRAN_ENV)
else()
  message(STATUS "FORCES: use ISO_C_BINDING for mo_kind")
endif()

if (FORCES_WITH_NETCDF)
  include(${FORCES_CMAKE_MODULE_PATH}/FindPackageWrapper.cmake)
  find_package_wrapper(${FORCES_CMAKE_MODULE_PATH} NetCDF COMPONENTS Fortran QUIET)
  # Sometimes NetCDF_INCLUDE_DIRS can't be found but NetCDF_Fortran is still usable
  if(NOT NetCDF_Fortran_FOUND)
    message(FATAL_ERROR "FORCES: NetCDF-Fortran not usable")
  else()
    message(STATUS "FORCES: NetCDF-Fortran usable: ${NetCDF_Fortran_LIBRARY}")
  endif()
  target_link_libraries(${LIB_NAME} PUBLIC NetCDF::NetCDF_Fortran)
  target_compile_definitions(${LIB_NAME} PRIVATE FORCES_WITH_NETCDF)
endif()

# add all compile options (MPI, OpenMP, Lapack, Coverage)
include(${FORCES_CMAKE_MODULE_PATH}/compileoptions.cmake)
if (CMAKE_WITH_MPI)
  message(STATUS "FORCES: use MPI")
  target_compile_definitions(${LIB_NAME} PRIVATE MPI)
  target_link_libraries(${LIB_NAME} PUBLIC MPI::MPI_Fortran)
endif()
if (CMAKE_WITH_OpenMP)
  message(STATUS "FORCES: use OpenMP")
  target_link_libraries(${LIB_NAME} PUBLIC OpenMP::OpenMP_Fortran)
endif()

# log settings
target_compile_definitions(${LIB_NAME} PUBLIC
  $<$<CONFIG:DEBUG>:ENABLE_LOG_TRACE>
  $<$<CONFIG:RELEASE>:DISABLE_LOG_DEBUG>
)
# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # set the preprocessor
  target_compile_definitions(${LIB_NAME} PRIVATE "GFORTRAN")
  # free line length to enable logging (only needed for gfortran)
  target_compile_options(${LIB_NAME} PRIVATE
    -ffree-line-length-none
    $<$<CONFIG:DEBUG>:-Og -g -Wall -Wextra -Wimplicit-interface -Wsurprising
      -fimplicit-none -fbacktrace -fcheck=all
      -ffpe-trap=zero,overflow,underflow -finit-real=snan -pedantic-errors>
    $<$<CONFIG:RELEASE>:-O3>
  )
  # https://www.scivision.dev/gfortran-15-external-argument-mismatch
  # AND: passed procedures with pointer attribute should be called with positional arguments
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "15.0" AND
     CMAKE_Fortran_COMPILER_VERSION VERSION_LESS          "15.2")
    target_compile_options(${LIB_NAME} PRIVATE $<$<CONFIG:DEBUG>:-Wno-external-argument-mismatch>)
  endif()
endif()
# ifort (Classic)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  target_compile_definitions(${LIB_NAME} PRIVATE "INTEL")
  target_compile_options(${LIB_NAME} PRIVATE
    -nofixed "SHELL:-assume byterecl" -standard-realloc-lhs
    $<$<CONFIG:DEBUG>:-g -fp-model=source "SHELL:-warn all" "SHELL:-check all" "SHELL:-check noarg_temp_created" -debug -traceback -fp-stack-check -O0>
    $<$<CONFIG:RELEASE>:-O3 -qoverride-limits -fp-model=precise -fprotect-parens>
  )
endif()
# ifx (IntelLLVM)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  target_compile_definitions(${LIB_NAME} PRIVATE "INTEL")
  target_compile_options(${LIB_NAME} PRIVATE
    -nofixed "SHELL:-assume byterecl" -standard-realloc-lhs
    $<$<CONFIG:DEBUG>:-g -fp-model=source "SHELL:-warn all" "SHELL:-check all" "SHELL:-check noarg_temp_created" "SHELL:-check nouninit" -traceback -O0>
    $<$<CONFIG:RELEASE>:-O3 -qoverride-limits -fp-model=precise -fprotect-parens>
  )
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  target_compile_definitions(${LIB_NAME} PRIVATE "NAG")
  target_compile_options(${LIB_NAME} PRIVATE
    -fpp -colour -unsharedf95 -ideclient
    # "-C=all" is not set, only "-C -C=alias -C=dangling" and "-ieee=full" instead of "-ieee=stop" because
    # this effectively omits the -C=intovf flag which checks for integer overflow
    # we need to exclude that as the random number generator relies on that technique
    # -ieee=full is needed for mo_utils (is_nan, is_finite etc. fails with -ieee=stop)
    $<$<CONFIG:DEBUG>:-gline -g -nan -O0 -C -C=alias -C=dangling -strict95 -ieee=full>
    $<$<CONFIG:RELEASE>:-O4 -ieee=full>
  )
endif()
