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
    add_pfunit_ctest(${testname}
      TEST_SOURCES ${testsourcefile}
      LINK_LIBRARIES ${LIB_NAME})
    list(APPEND testnames ${testname})
  endforeach()

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
      "no_overlap|active coarse cell outside fine extent"
      "coordinate_system|grids don't use the same coordinate system"
      "resolution_ratio|check_factor: Two resolutions"
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
