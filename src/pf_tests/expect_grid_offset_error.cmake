# A failing process alone is insufficient: verify the intended diagnostic too.
execute_process(
  COMMAND "${PROGRAM}" "${CASE}"
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE error)
if(NOT "${result}" STREQUAL "1")
  message(FATAL_ERROR "${CASE}: expected exit code 1, got ${result}\n${output}${error}")
endif()
string(FIND "${output}${error}" "${EXPECTED}" diagnostic_position)
if(diagnostic_position EQUAL -1)
  message(FATAL_ERROR "${CASE}: missing diagnostic '${EXPECTED}'\n${output}${error}")
endif()
# Multiple reports can indicate concurrent fatal errors from OpenMP workers.
string(LENGTH "${EXPECTED}" diagnostic_length)
math(EXPR diagnostic_end "${diagnostic_position} + ${diagnostic_length}")
string(SUBSTRING "${output}${error}" ${diagnostic_end} -1 remaining_output)
string(FIND "${remaining_output}" "${EXPECTED}" duplicate_position)
if(NOT duplicate_position EQUAL -1)
  message(FATAL_ERROR "${CASE}: diagnostic reported more than once\n${output}${error}")
endif()
