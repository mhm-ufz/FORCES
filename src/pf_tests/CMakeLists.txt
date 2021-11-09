enable_testing()

set (LIB_NAME forces)
find_package(PFUNIT)
# copy files
file ( COPY ./files DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

if(PFUNIT_FOUND)
  set(pfunit_suffix ".pf")
  file(GLOB testsourcefiles RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.pf)
  foreach(testsourcefile ${testsourcefiles})
    get_filename_component(testname ${testsourcefile} NAME_WE)
    add_pfunit_ctest(${testname}
      TEST_SOURCES ${testsourcefile}
      LINK_LIBRARIES ${LIB_NAME})
    list(APPEND testnames ${testname})
  endforeach()
else()
  message(STATUS "No test program found, install pFUnit")
endif()
