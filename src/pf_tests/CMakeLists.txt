set (LIB_NAME forces)

enable_testing()

find_package(PFUNIT)

file ( COPY ./files
DESTINATION ${CMAKE_CURRENT_BINARY_DIR})


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

else()
    message(STATUS "No test program found, install PFUNIT")
endif()

