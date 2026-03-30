set(FORCES_CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

set (LIB_NAME forces)

# add sources
file(GLOB sources ./mo_*.f90 ./mo_*.F90)
# find preprocessor (fypp)
include(${FORCES_CMAKE_MODULE_PATH}/fortranpreprocessor.cmake)
find_program(FYPP fypp)
if(NOT FYPP)
  message(WARNING "FORCES: Preprocessor fypp not found!")
else()
  message(STATUS "FORCES: Preprocessor fypp found!")
  # Create a list of the files to be preprocessed
  set(fppFiles mo_netcdf_wrapper.fypp mo_netcdf.fypp mo_poly.fypp mo_sentinel.fypp mo_grid_io.fypp)
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
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf_wrapper.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_poly.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_sentinel.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_grid_io.f90$")
endif()

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

if (FORCES_WITH_NETCDF)
  message(STATUS "FORCES: use NetCDF")
else()
  message(STATUS "FORCES: no NetCDF support wanted")
  list(FILTER fyppOutFiles EXCLUDE REGEX ".*mo_netcdf_wrapper.f90$")
  list(FILTER fyppOutFiles EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER fyppOutFiles EXCLUDE REGEX ".*mo_grid_io.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf_wrapper.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_ncwrite.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_ncread.f90$")
  list(FILTER sources EXCLUDE REGEX ".*mo_grid_io.f90$")
endif()

if(FORCES_ENABLE_NATIVE)
  message(STATUS "FORCES: enable host-native Release tuning (opt-in local/HPC mode)")
else()
  message(STATUS "FORCES: use portable Release tuning")
endif()

# create library
add_library(${LIB_NAME} ${sources} ${fyppOutFiles})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../include)

if (FORCES_WITH_ISO_FORTRAN_ENV)
  message(STATUS "FORCES: use ISO_FORTRAN_ENV for mo_kind")
  target_compile_definitions(${LIB_NAME} PRIVATE FORCES_WITH_ISO_FORTRAN_ENV)
else()
  message(STATUS "FORCES: use ISO_C_BINDING for mo_kind")
endif()

if (FORCES_WITH_NETCDF)
  include(${FORCES_CMAKE_MODULE_PATH}/FindPackageWrapper.cmake)
  find_package_wrapper(${FORCES_CMAKE_MODULE_PATH} NetCDF QUIET)
  if(NOT NetCDF_C_FOUND)
    message(FATAL_ERROR "FORCES: NetCDF-C not usable")
  else()
    message(STATUS "FORCES: NetCDF-C usable: ${NetCDF_C_LIBRARY}")
  endif()
  target_sources(${LIB_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/netcdf_wrapper_c.c)
  target_link_libraries(${LIB_NAME} PUBLIC NetCDF::NetCDF_C)
  target_compile_definitions(${LIB_NAME} PRIVATE FORCES_WITH_NETCDF)
endif()

# add all compile options (MPI, OpenMP, Coverage)
if (FORCES_WITH_MPI)
  find_package(MPI REQUIRED COMPONENTS Fortran)
  if(NOT MPI_Fortran_HAVE_F08_MODULE)
      message(FATAL_ERROR "FORCES: MPI found, but the 'mpi_f08' module is not available.")
  endif()
  message(STATUS "FORCES: use MPI")
  target_compile_definitions(${LIB_NAME} PRIVATE MPI)
  target_link_libraries(${LIB_NAME} PUBLIC MPI::MPI_Fortran)
endif()
if (FORCES_WITH_OpenMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
  if(NOT OpenMP_Fortran_HAVE_OMPLIB_MODULE)
      message(FATAL_ERROR "FORCES: OpenMP found, but the 'omp_lib' module is not available.")
  endif()
  message(STATUS "FORCES: use OpenMP")
  # Hack for NAG+Cmake+OpenMP
  # See: https://gitlab.kitware.com/cmake/cmake/-/work_items/21280
  if(CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
    # Do not use OpenMP::OpenMP_Fortran for NAG.
    # Do not add -thread_safe here: FORCES intentionally uses saved/threadprivate
    # RNG state in mo_xor4096, which NAG rejects in thread-safe procedures.
    add_library(project_openmp INTERFACE)
    target_compile_options(project_openmp INTERFACE "$<$<COMPILE_LANGUAGE:Fortran>:-openmp>")
    target_link_options(project_openmp INTERFACE "$<$<LINK_LANGUAGE:Fortran>:-openmp>")
  else()
    add_library(project_openmp INTERFACE)
    target_link_libraries(project_openmp INTERFACE OpenMP::OpenMP_Fortran)
  endif()
  target_link_libraries(${LIB_NAME} PRIVATE project_openmp)
endif()

# log settings
if(LOG_TRACE STREQUAL "ON")
  target_compile_definitions(${LIB_NAME} PUBLIC ENABLE_LOG_TRACE)
elseif(LOG_TRACE STREQUAL "AUTO")
  target_compile_definitions(${LIB_NAME} PUBLIC $<$<CONFIG:DEBUG>:ENABLE_LOG_TRACE>)
endif()

if(LOG_DEBUG STREQUAL "OFF")
  target_compile_definitions(${LIB_NAME} PUBLIC DISABLE_LOG_DEBUG)
elseif(LOG_DEBUG STREQUAL "AUTO")
  target_compile_definitions(${LIB_NAME} PUBLIC $<$<CONFIG:RELEASE>:DISABLE_LOG_DEBUG>)
endif()

# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  # set the preprocessor
  target_compile_definitions(${LIB_NAME} PRIVATE "GFORTRAN")
  # Keep this PUBLIC because downstream code using mo_logging can expand
  # logging calls into lines that exceed gfortran's default free-form limit.
  target_compile_options(${LIB_NAME} PUBLIC
    "$<$<COMPILE_LANGUAGE:Fortran>:-ffree-line-length-none>"
  )
  target_compile_options(${LIB_NAME} PRIVATE
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:DEBUG>>:-Og;-g;-Wall;-Wextra;-Wimplicit-interface;-Wsurprising;-fimplicit-none;-fbacktrace;-fcheck=all,no-array-temps;-ffpe-trap=zero,overflow,underflow;-finit-real=snan;-pedantic-errors>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-O3>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-fbacktrace>"
  )
  if(FORCES_ENABLE_NATIVE)
    target_compile_options(${LIB_NAME} PRIVATE
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-march=native>"
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-march=native>"
    )
  endif()
  # https://www.scivision.dev/gfortran-15-external-argument-mismatch
  # AND: passed procedures with pointer attribute should be called with positional arguments
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "15.0" AND
     CMAKE_Fortran_COMPILER_VERSION VERSION_LESS          "15.2")
    target_compile_options(${LIB_NAME} PRIVATE
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:DEBUG>>:-Wno-external-argument-mismatch>"
    )
  endif()
endif()
# ifort (Classic)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  target_compile_definitions(${LIB_NAME} PRIVATE "INTEL")
  target_compile_options(${LIB_NAME} PRIVATE
    "$<$<COMPILE_LANGUAGE:Fortran>:-nofixed;SHELL:-assume byterecl;-standard-realloc-lhs>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:DEBUG>>:-g;-fp-model=source;SHELL:-warn all;SHELL:-check all;SHELL:-check noarg_temp_created;-debug;-traceback;-fp-stack-check;-O0>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-O3;-qoverride-limits>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-traceback>"
  )
  if(FORCES_ENABLE_NATIVE)
    target_compile_options(${LIB_NAME} PRIVATE
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-xHost>"
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-xHost>"
    )
  endif()
endif()
# ifx (IntelLLVM)
if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  target_compile_definitions(${LIB_NAME} PRIVATE "INTEL")
  target_compile_options(${LIB_NAME} PRIVATE
    "$<$<COMPILE_LANGUAGE:Fortran>:-nofixed;SHELL:-assume byterecl;-standard-realloc-lhs>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:DEBUG>>:-g;-fp-model=source;SHELL:-warn all;SHELL:-check all;SHELL:-check noarg_temp_created;SHELL:-check nouninit;-traceback;-O0>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-O3;-qoverride-limits>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-traceback>"
  )
  if(FORCES_ENABLE_NATIVE)
    target_compile_options(${LIB_NAME} PRIVATE
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-xHost>"
      "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:-xHost>"
    )
  endif()
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
  if(FORCES_ENABLE_NATIVE)
    message(STATUS "FORCES: host-native Release tuning has no effect for NAG")
  endif()
  target_compile_definitions(${LIB_NAME} PRIVATE "NAG")
  if(FORCES_WITH_OpenMP)
    # NAG's -gline traceback instrumentation destabilizes explicit OpenMP calls
    # into the recursive indmed path in mo_orderpack. Keep debug checks, but
    # disable -gline for OpenMP builds.
    set(NAG_DEBUG_FLAGS "-g;-nan;-O0;-C;-C=alias;-C=dangling;-strict95;-ieee=full")
    set(NAG_RELWITHDEBINFO_FLAGS "-ieee=full")
  else()
    set(NAG_DEBUG_FLAGS "-gline;-g;-nan;-O0;-C;-C=alias;-C=dangling;-strict95;-ieee=full")
    set(NAG_RELWITHDEBINFO_FLAGS "-gline;-ieee=full")
  endif()
  target_compile_options(${LIB_NAME} PRIVATE
    "$<$<COMPILE_LANGUAGE:Fortran>:-fpp;-colour;-unsharedf95;-ideclient>"
    # "-C=all" is not set, only "-C -C=alias -C=dangling" and "-ieee=full" instead of "-ieee=stop" because
    # this effectively omits the -C=intovf flag which checks for integer overflow
    # we need to exclude that as the random number generator relies on that technique
    # -ieee=full is needed for mo_utils (is_nan, is_finite etc. fails with -ieee=stop)
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:DEBUG>>:${NAG_DEBUG_FLAGS}>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELEASE>>:-O4;-ieee=full>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:RELWITHDEBINFO>>:${NAG_RELWITHDEBINFO_FLAGS}>"
  )
endif()
