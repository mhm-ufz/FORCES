
enable_testing()

file ( COPY ./files
DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

file ( GLOB sources ./test_*.f90)
foreach ( UNIT_TEST ${sources} )
    # get the filename and use it for variable name
    get_filename_component ( test_name_ext ${UNIT_TEST} NAME )
    get_filename_component ( test_name ${UNIT_TEST} NAME_WE )
	add_executable(${test_name} ${test_name_ext})
	set_target_properties ( ${test_name}
	  PROPERTIES
	  COMPILE_FLAGS "${NETCDF_CFLAGS_OTHER} ${ADDITIONAL_GCC_FLAGS}"
	  LINK_FLAGS "${NETCDF_LDFLAGS_OTHER}"
	)
	# message(STATUS "Testing enabled for ${TEST_DIR}")
	target_link_libraries(${test_name} lightweight_fortran_lib)
	add_test(NAME ${test_name} COMMAND ${test_name})
	# set compiling flags for debug and relese version
	if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	    # set the preprocessor
	    target_compile_definitions(${test_name} PRIVATE "-DGFORTRAN")
	endif()
	if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
	    # set the preprocessor
	    target_compile_definitions(${test_name} PRIVATE "-DpgiFortran")
	endif()
	if(CMAKE_ABSOFT)
	    # set the preprocessor
	    target_compile_definitions(${test_name} PRIVATE "-DABSOFT")
	endif()
endforeach ( UNIT_TEST )
