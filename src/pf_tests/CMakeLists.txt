enable_testing()

find_package(PFUNIT)

if(PFUNIT_FOUND)
	# find if there is an OpenMP setup on the system and if so, set corresponding variables
	find_package(OpenMP)
	if (NOT ${OpenMP_Fortran_FOUND})
		message(FATAL_ERROR "OpenMP required but not found")
	endif()

	add_pfunit_ctest (test_mo_os
	  TEST_SOURCES test_mo_os.pf
	  LINK_LIBRARIES lightweight_fortran_lib
	  )
else()
    message(STATUS "No test program found, install PFUNIT")
endif()

