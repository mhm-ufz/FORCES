set (LIB_NAME lightweight_fortran_lib)

include(${CMAKE_CURRENT_SOURCE_DIR}/../cmake/utils.cmake)
file(GLOB sources ./mo_*.f90)
# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
	message(WARNING "Preprocessor fypp not found!")
else()
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

	### Pre-process: .fpp -> .f90 via Fypp
	fypp_f90("${fyppFlags}" "${fppFiles}" fyppOutFiles)
	list(FILTER sources EXCLUDE REGEX ".*mo_netcdf.f90$")
endif()

add_library ( ${LIB_NAME} ${sources} ${fyppOutFiles})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# additional cmake-modules created for the purpose of finding netCDF or other libraries ly in the source_directory in
# a folder named cmake-modules. This command tells cmake to search there for Find<module>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

# find the netcdf and netcdf-fortran libraries, taken from wrf-cmake project
set (NETCDF_F90 "YES")
find_package (NetCDF REQUIRED)
target_link_libraries(${LIB_NAME} ${NETCDF_F90_LIBRARIES})
target_include_directories(${LIB_NAME} PUBLIC ${NETCDF_F90_INCLUDE_DIRS})

set(CMAKE_WITH_MPI OFF CACHE STRING "build the module with MPI, so it can be executed using mpirun")
# if cmake provides a findLIBRARY module, this gets invoked via find_package(LIBRARY)
if (CMAKE_WITH_MPI)
        # find if there is an MPI setup on the system and if so, set corresponding variables
        find_package(MPI)
        if (NOT ${MPI_Fortran_FOUND})
                message(FATAL_ERROR "MPI required but not found")
        else()
                message(STATUS "found MPI_Fortran_COMPILER ${MPI_Fortran_COMPILER}")
                # set the preprocessor
                target_compile_definitions(${LIB_NAME} PRIVATE "-MPI")
				# add the link to mpi if existing
                target_link_libraries(${LIB_NAME} ${MPI_Fortran_LIBRARIES} )
                target_include_directories(${LIB_NAME} PUBLIC ${MPI_Fortran_INCLUDE_PATH} )
        endif()
endif()

# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DGFORTRAN")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DPGI")
endif()
if(CMAKE_ABSOFT)
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DABSOFT")
endif()

# ifort and gfortran need the flag -cpp to interpret definitions like -DMRM2MHM
# the nag compiler is not able to interpret the flag -cpp but can interpret these definitions anyway
# so we check whether the compiler is able to use the flag -cpp
# for that we need the module CheckFortranCompilerFlag
include(CheckFortranCompilerFlag)
CHECK_Fortran_COMPILER_FLAG("-cpp" CPP_FLAG)
# if the flag exists, we add it to the compilation flags
if (CPP_FLAG)
	set(ADDITIONAL_GCC_FLAGS "-cpp")
endif()

set_target_properties ( ${LIB_NAME}
  PROPERTIES
  COMPILE_FLAGS "${ADDITIONAL_GCC_FLAGS}"
)
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

if(BUILD_TESTING)
  add_subdirectory(./tests)
endif()