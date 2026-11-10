set (LIB_NAME forces)
# Coverage must include the pFUnit suites rather than silently skip them.
if(FORCES_WITH_COVERAGE)
  find_package(PFUNIT REQUIRED)
else()
  find_package(PFUNIT QUIET)
endif()
# copy files
file ( COPY ./files DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

if(PFUNIT_FOUND)
  set(pfunit_suffix ".pf")
  file(GLOB testsourcefiles RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} ${FORCES_TEST_GLOB})
  if (NOT FORCES_WITH_OPTIMIZATION)
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_anneal.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_dds.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_errormeasures.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_mcmc.*\\.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_sce.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_opt_eval_utils.pf$")
  endif()
  if (NOT FORCES_WITH_NETCDF)
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_netcdf.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_netcdf_wrapper.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_grid.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_grid_io.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_points.pf$")
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_mcmc_netcdf.pf$")
  else()
    list(FILTER testsourcefiles EXCLUDE REGEX ".*mo_mcmc_no_netcdf.pf$")
  endif()
  foreach(testsourcefile ${testsourcefiles})
    get_filename_component(testname ${testsourcefile} NAME_WE)
    if(testname MATCHES "^test_mo_(message|forces_message|logging)$")
      add_pfunit_ctest(${testname}
        TEST_SOURCES ${testsourcefile}
        OTHER_SOURCES logging_test_support.F90
        EXTRA_USE logging_test_support
        EXTRA_INITIALIZE initialize_logging_tests
        EXTRA_FINALIZE finalize_logging_tests
        LINK_LIBRARIES ${LIB_NAME})
      if(FORCES_WITH_MPI)
        target_compile_definitions(${testname} PRIVATE MPI)
      endif()
    else()
      add_pfunit_ctest(${testname}
        TEST_SOURCES ${testsourcefile}
        LINK_LIBRARIES ${LIB_NAME})
    endif()
    list(APPEND testnames ${testname})
  endforeach()

  if(TARGET test_mo_message OR TARGET test_mo_forces_message OR TARGET test_mo_logging)
    add_executable(logging_cases logging_cases.F90)
    target_link_libraries(logging_cases PRIVATE ${LIB_NAME})
    if(FORCES_WITH_MPI)
      target_compile_definitions(logging_cases PRIVATE MPI)
    endif()
    foreach(suite test_mo_message test_mo_forces_message test_mo_logging)
      if(TARGET ${suite})
        add_dependencies(${suite} logging_cases)
      endif()
    endforeach()
    # case | exit status | required diagnostic | forbidden diagnostic
    set(logging_cases_to_check
      "fatal-plain|1|[forces] sentinel fatal|[FATAL]"
      "fatal-filtered|1|[forces][FATAL] sentinel fatal|[mhm]"
      "warning-fatal|1|[forces][FATAL] sentinel warning|[WARN]"
      "fatal-plain-hidden|1||sentinel hidden"
      "fatal-hidden|1||sentinel hidden"
      "fatal-global-hidden|1||sentinel hidden"
      "fatal-global-visible|1|[forces][FATAL] sentinel visible|[mhm]"
      "warning-global-visible|1|[forces][FATAL] sentinel visible|[mhm]"
      "warning-hidden|1||sentinel hidden"
      "fatal-quiet|1||sentinel quiet"
      "append-shape|1|[forces]|[mhm]"
      "moment-size|1|stddev_dp: n must be at least 2|[mhm]"
      "calendar|1|Unknown selector!|[mhm]")
    foreach(logging_case IN LISTS logging_cases_to_check)
      string(REPLACE "|" ";" case_parts "${logging_case}")
      list(GET case_parts 0 case_name)
      list(GET case_parts 1 expected_exit)
      list(GET case_parts 2 expected)
      list(GET case_parts 3 forbidden)
      add_test(NAME test_mo_logging_${case_name}
        COMMAND ${CMAKE_COMMAND}
          "-DPROGRAM=$<TARGET_FILE:logging_cases>"
          "-DCASE=${case_name}"
          "-DCASE_DIRECTORY=${CMAKE_CURRENT_BINARY_DIR}/logging_case_${case_name}"
          "-DEXPECTED_EXIT=${expected_exit}"
          "-DEXPECTED=${expected}"
          "-DFORBIDDEN=${forbidden}"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/expect_logging_case.cmake")
    endforeach()
  endif()

  if("test_mo_grid.pf" IN_LIST testsourcefiles OR "test_mo_grid_scaler.pf" IN_LIST testsourcefiles)
    add_executable(grid_offset_error grid_offset_error.f90)
    target_link_libraries(grid_offset_error PRIVATE ${LIB_NAME})
    foreach(suite test_mo_grid test_mo_grid_scaler)
      if(TARGET ${suite})
        add_dependencies(${suite} grid_offset_error)
      endif()
    endforeach()
    set(offset_error_cases
      "fractional|check_offset: lower-left displacement must be an integer"
      "offset_range|check_offset: displacement is outside"
      "offset_range_negative|check_offset: displacement is outside"
      "offset_range_large|check_offset: displacement is outside"
      "offset_nonfinite|check_offset: origins, cell size and tolerance must be finite"
      "lost_coverage|active fine cell outside coarse extent"
      "uncovered_mask|fine cells outside of coarse mask"
      "unfilled_mask|coarse cells without any filling fine cells found"
      "fine_padding|derive_fine_grid: fine extent must lie inside"
      "fine_excludes|derive_fine_grid: llcorner excludes required source cells"
      "coarse_empty|derive_coarse_grid: each coarse cell must overlap"
      "coarse_excludes|derive_coarse_grid: llcorner excludes required source cells"
      "origin_fractional|check_offset: lower-left displacement must be an integer"
      "fine_origin_fractional|check_offset: lower-left displacement must be an integer"
      "origin_nonfinite|check_offset: origins, cell size and tolerance must be finite"
      "dims_zero|derive_coarse_grid: derived dimensions are out of range"
      "fine_dims_negative|derive_fine_grid: derived dimensions are out of range"
      "coarse_pad_upper|derive_coarse_grid: each coarse cell must overlap"
      "fine_pad_upper|derive_fine_grid: fine extent must lie inside"
      "coarse_excludes_no_aux|derive_coarse_grid: llcorner excludes required source cells"
      "fine_excludes_no_aux|derive_fine_grid: llcorner excludes required source cells"
      "coarse_dimension_overflow|derive_coarse_grid: derived dimensions are out of range"
      "fine_dimension_overflow|derive_fine_grid: derived dimensions are out of range"
      "mask_origin_fractional|check_offset: lower-left displacement must be an integer"
      "mask_fine_origin_fractional|check_offset: lower-left displacement must be an integer"
      "mask_origin_nonfinite|check_offset: origins, cell size and tolerance must be finite"
      "mask_origin_excludes|grid % mask_extent: origin excludes required source cells"
      "mask_fine_origin_excludes|grid % mask_extent: origin excludes required source cells"
      "mask_empty_excludes|grid % mask_extent: origin excludes required source cells"
      "mask_coarse_dimension_overflow|grid % mask_extent: derived dimensions are out of range"
      "mask_fine_dimension_overflow|grid % mask_extent: derived dimensions are out of range"
      "mask_resolution_ratio|check_factor: Two resolutions"
      "mask_resolution_nonfinite|grid % mask_extent: target resolution must be finite"
      "periodic_up_west|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_up_east|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_up_fill|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_down_west|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_down_east|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_equal_east|scaler: active target cells cross the periodic source longitude boundary"
      "periodic_global_up|scaler: periodic grids must use the same longitude interval"
      "periodic_global_down|scaler: periodic grids must use the same longitude interval"
      "periodic_global_equal|scaler: periodic grids must use the same longitude interval"
      "periodic_coarse_west|derive_coarse_grid: longitude extent exceeds periodic source"
      "periodic_coarse_east|derive_coarse_grid: longitude extent exceeds periodic source"
      "periodic_coarse_empty|derive_coarse_grid: longitude extent exceeds periodic source"
      "periodic_coarse_default|derive_coarse_grid: longitude extent exceeds periodic source"
      "periodic_mask_padding|derive_coarse_grid: longitude extent exceeds periodic source"
      "periodic_fine_west|derive_fine_grid: longitude extent exceeds periodic source"
      "periodic_fine_east|derive_fine_grid: longitude extent exceeds periodic source"
      "periodic_fine_default|derive_fine_grid: longitude extent exceeds periodic source"
      "no_overlap|active coarse cell outside fine extent"
      "coordinate_system|grids don't use the same coordinate system"
      "resolution_ratio|check_factor: Two resolutions"
      "equal_mask|coarse cells without any filling fine cells found"
      "equal_fill_geometry|active coarse cell outside fine extent"
      "equal_no_donor|no valid target result is available to fill missing cells"
      "equal_real_fraction|class fraction upscaling operator not supported for real input data"
      "equal_integer_fraction|class fraction upscaling operator not supported for integer output data"
      "fill_geometry|active fine cell outside coarse extent")
    foreach(error_case IN LISTS offset_error_cases)
      string(REPLACE "|" ";" case_parts "${error_case}")
      list(GET case_parts 0 case_name)
      list(GET case_parts 1 expected)
      add_test(NAME test_mo_grid_offset_${case_name}
        COMMAND ${CMAKE_COMMAND}
          "-DPROGRAM=$<TARGET_FILE:grid_offset_error>"
          "-DCASE=${case_name}"
          "-DEXPECTED=${expected}"
          -P "${CMAKE_CURRENT_SOURCE_DIR}/expect_grid_offset_error.cmake")
    endforeach()
  endif()
else()
  message(STATUS "FORCES: No test program found, install pFUnit")
endif()
