set (LIB_NAME lightweight_fortran_lib)
file(GLOB sources ./mo_*.f90)
add_library ( ${LIB_NAME} ${sources})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

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

# set specific place where to search for the netCDF directory
set(CMAKE_NETCDF_DIR " " CACHE STRING "set set specific place where to search for the netCDF directory")
message(STATUS "search in additional directory ${CMAKE_NETCDF_DIR} for netCDF")

# additional cmake-modules created for the purpose of finding netCDF or other libraries ly in the source_directory in
# a folder named cmake-modules. This command tells cmake to search there for Find<module>.cmake files
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../cmake-modules)
set (NETCDF_F90 "YES")
# the FindNetCDFF.cmake file can be found after we added the cmake-modules folder to the CMAKE_MODULE_PATH
# the build fails, if it is not present
find_package(NetCDFF REQUIRED)
# from that module we gain the following variables:
# NETCDF_INCLUDES	: the include directory
# NETCDF_LINK_LIBRARIES : the absolute path to and with the libraries
# NETCDF_CFLAGS_OTHER	: additional compilation flags
# NETCDF_LDFLAGS_OTHER	: additional linking flags

# set compiling flags for debug and relese version
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DGFORTRAN")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DpgiFortran")
endif()
if(CMAKE_ABSOFT)
    # set the preprocessor
    target_compile_definitions(${LIB_NAME} PRIVATE "-DABSOFT")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
	target_compile_options(${LIB_NAME} PUBLIC  "-kind=byte")
endif()

set_target_properties ( ${LIB_NAME}
  PROPERTIES
  COMPILE_FLAGS "${NETCDF_CFLAGS_OTHER} ${ADDITIONAL_GCC_FLAGS}"
  LINK_FLAGS "${NETCDF_LDFLAGS_OTHER}"
)

target_link_libraries(${LIB_NAME} ${NETCDF_LINK_LIBRARIES})
target_include_directories(${LIB_NAME} PUBLIC ${NETCDF_INCLUDES})
target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_CURRENT_BINARY_DIR})

enable_testing()
add_subdirectory(./tests)