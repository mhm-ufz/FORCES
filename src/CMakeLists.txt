list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/cmake-modules)

set (LIB_NAME forces)

file(GLOB sources ./mo_*.f90 ./mo_*.F90)
# --- find preprocessor (fypp)
include(fortranpreprocessor)
find_program(FYPP fypp)
if(NOT FYPP)
  message(WARNING "Preprocessor fypp not found!")
else()
  message(STATUS "Preprocessor fypp found!")
# Create a list of the files to be preprocessed
  set(fppFiles mo_netcdf.fypp)
  # Custom preprocessor flags
  if(DEFINED CMAKE_MAXIMUM_RANK)
    set(fyppFlags "-DMAXRANK=${CMAKE_MAXIMUM_RANK}")
  elseif(f03rank)
    set(fyppFlags)
  else()
    set(fyppFlags "-DVERSION90")
  endif()
  # Pre-process: .fpp -> .f90 via Fypp
  fypp_f90("${fyppFlags}" "${fppFiles}" fyppOutFiles)
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
endif()

# add sources
add_library(${LIB_NAME} ${sources} ${fyppOutFiles})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

# control mo_kind
option(FORCES_WITH_ISO_FORTRAN_ENV "build the module with kind definitions of ISO_FORTRAN_ENV instead of ISO_C_BINDING" OFF)
if (FORCES_WITH_ISO_FORTRAN_ENV)
  message(STATUS "FORCES: use ISO_FORTRAN_ENV for mo_kind")
  target_compile_definitions(${LIB_NAME} PRIVATE FORCES_WITH_ISO_FORTRAN_ENV)
else()
  message(STATUS "FORCES: use ISO_C_BINDING for mo_kind")
endif()

# find the netcdf and netcdf-fortran libraries quietly, taken from ecmwf project
find_package(NetCDF COMPONENTS Fortran QUIET)
# Sometimes NetCDF_INCLUDE_DIRS can't be found but NetCDF_Fortran is still usable
if(NOT NetCDF_Fortran_FOUND)
  message(FATAL_ERROR "FORCES: NetCDF-Fortran not usable")
else()
  message(STATUS "FORCES: NetCDF-Fortran usable: ${NetCDF_Fortran_LIBRARY}")
endif()
target_link_libraries(${LIB_NAME} PUBLIC NetCDF::NetCDF_Fortran)

# add all compile options (MPI, OpenMP, Lapack, Coverage)
include(compileoptions)

if (CMAKE_WITH_MPI)
  target_compile_definitions(${LIB_NAME} PRIVATE MPI)
  target_link_libraries(${LIB_NAME} PUBLIC MPI::MPI_Fortran)
endif()
if (CMAKE_WITH_OpenMP)
  target_link_libraries(${LIB_NAME} PUBLIC OpenMP::OpenMP_Fortran)
endif()

# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # set the preprocessor
  target_compile_definitions(${LIB_NAME} PRIVATE "GFORTRAN")
  target_compile_options(${LIB_NAME} PRIVATE
    -ffree-form -ffixed-line-length-132
    $<$<CONFIG:DEBUG>:-fbacktrace -pedantic-errors -Wall -W -O -g -Wno-maybe-uninitialized>
    $<$<CONFIG:RELEASE>:-O3 -fno-peel-loops>
  )
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_definitions(${LIB_NAME} PRIVATE "INTEL")
  target_compile_options(${LIB_NAME} PRIVATE
    -nofixed "SHELL:-assume byterecl" "SHELL:-fp-model source" -m64 "SHELL:-assume realloc-lhs"
    # -fstack-protector-all -fstack-security-check were previously on in debug mode, still needed?
    $<$<CONFIG:DEBUG>:-g "SHELL:-warn all" "SHELL:-check all" -debug -traceback -fp-stack-check -O0>
    $<$<CONFIG:RELEASE>:-O3 -qoverride-limits>
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
