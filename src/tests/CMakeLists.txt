
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
	  COMPILE_FLAGS "${ADDITIONAL_GCC_FLAGS}"
	)
	# message(STATUS "Testing enabled for ${TEST_DIR}")
	target_link_libraries(${test_name} lightweight_fortran_lib)
	add_test(NAME ${test_name} COMMAND ${test_name})
	# set compiling flags for debug and relese version
	# set compiling flags for debug and relese version

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
    target_compile_options(${test_name} PRIVATE
      -fpp -colour -unsharedf95 -ideclient
      # "-C=all" is not set, only "-C -C=alias -C=dangling" and "-ieee=full" instead of "-ieee=stop" because
      # this effectively omits the -C=intovf flag which checks for integer overflow
      # we need to exclude that as the random number generator relies on that technique
      # -ieee=full is needed for mo_utils (is_nan, is_finite etc. fails with -ieee=stop)
      $<$<CONFIG:DEBUG>:-g -nan -O0 -C -C=alias -C=dangling -strict95 -ieee=full>
      $<$<CONFIG:RELEASE>:-O4 -ieee=full>
    )
  endif()
	if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
	    # set the preprocessor
	    target_compile_definitions(${test_name} PRIVATE "GFORTRAN")
      target_compile_options(${test_name} PRIVATE
      -ffree-form -ffixed-line-length-132
      $<$<CONFIG:DEBUG>:-pedantic-errors -Wall -W -O -g -Wno-maybe-uninitialized>
      $<$<CONFIG:RELEASE>:-O3>
      #$<$<BOOL:${CMAKE_WITH_COVERAGE}>:-g -fprofile-arcs -ftest-coverage>
    )
	endif()
  list(APPEND testnames ${testname})
endforeach()

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  if (CMAKE_WITH_COVERAGE)
    include(CodeCoverage)
    APPEND_COVERAGE_COMPILER_FLAGS()
    SETUP_TARGET_FOR_COVERAGE_LCOV(NAME FORCES_coverage_CI
      EXECUTABLE ${testnames}
      GENHTML_ARGS -t "${LIB_NAME} coverage" --html-prolog ${CMAKE_CURRENT_SOURCE_DIR}/../doc/html_files/cov_header.prolog)
  endif()
endif()
