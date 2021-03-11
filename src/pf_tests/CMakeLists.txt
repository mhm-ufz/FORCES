enable_testing()

find_package(PFUNIT)
set (LIB_NAME lightweight_fortran_lib)

if(PFUNIT_FOUND)
	# find if there is an OpenMP setup on the system and if so, set corresponding variables
	find_package(OpenMP)
	if (NOT ${OpenMP_Fortran_FOUND})
		message(FATAL_ERROR "OpenMP required but not found")
	endif()

  set (pfunit_suffix ".pf")
  file( GLOB testsourcefiles RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *${pfunit_suffix} )
  foreach( testsourcefile ${testsourcefiles} )
    # I used a simple string replace, to cut off .pf
    string( REPLACE ${pfunit_suffix} "" testname ${testsourcefile} )
    add_pfunit_ctest (${testname}
      TEST_SOURCES ${testsourcefile}
      LINK_LIBRARIES ${LIB_NAME}
      )
    list(APPEND testnames ${testname})
  endforeach()

  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    if (CMAKE_WITH_COVERAGE)
      include(CodeCoverage)
      APPEND_COVERAGE_COMPILER_FLAGS()
      SETUP_TARGET_FOR_COVERAGE_LCOV(NAME FORCES_unit_coverage_CI
        EXECUTABLE ${testnames}
        GENHTML_ARGS -t "${LIB_NAME} unit coverage" --html-prolog ${CMAKE_CURRENT_SOURCE_DIR}/../../doc/html_files/cov_header.prolog)
    endif()
  endif()

else()
    message(STATUS "No test program found, install PFUNIT")
endif()

