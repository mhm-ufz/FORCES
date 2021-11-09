enable_testing()

set (LIB_NAME forces)
file ( COPY ./files DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

file ( GLOB sources ./test_*.f90)
foreach ( UNIT_TEST ${sources} )
  # get the filename and use it for variable name
  get_filename_component ( test_name_ext ${UNIT_TEST} NAME )
  get_filename_component ( test_name ${UNIT_TEST} NAME_WE )
  add_executable(${test_name} ${test_name_ext})
  target_link_libraries(${test_name} ${LIB_NAME})
  add_test(NAME ${test_name} COMMAND ${test_name})

  # set compiling flags for debug and relese version
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
      # set the preprocessor
      target_compile_definitions(${test_name} PRIVATE "GFORTRAN")
      target_compile_options(${test_name} PRIVATE
      -ffree-form -ffixed-line-length-132
      $<$<CONFIG:DEBUG>:-pedantic-errors -Wall -W -O -g -Wno-maybe-uninitialized>
      $<$<CONFIG:RELEASE>:-O3 -fno-peel-loops>
    )
  endif()
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    target_compile_definitions(${test_name} PRIVATE "INTEL")
    target_compile_options(${test_name} PRIVATE
      -nofixed "SHELL:-assume byterecl" "SHELL:-fp-model source" -m64 "SHELL:-assume realloc-lhs"
      # -fstack-protector-all -fstack-security-check were previously on in debug mode, still needed?
      $<$<CONFIG:DEBUG>:-g "SHELL:-warn all" "SHELL:-check all" -debug -traceback -fp-stack-check -O0>
      $<$<CONFIG:RELEASE>:-O3 -qoverride-limits>
    )
  endif()
  if(CMAKE_Fortran_COMPILER_ID MATCHES "NAG")
    target_compile_definitions(${test_name} PRIVATE "NAG")
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
  list(APPEND testnames ${testname})
endforeach()
