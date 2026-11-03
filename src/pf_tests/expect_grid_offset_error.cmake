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
