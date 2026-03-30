# fortranpreprocessor.cmake - pre-processor helpers
# =================================================
#
# MIT License
# -----------
#[[
  Copyright (c) 2020 - 2026 CHS Developers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
]]

# this code is based on from https://github.com/fortran-lang/stdlib/blob/master
# MIT License
#
# Copyright (c) 2019 Fortran stdlib developers
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Preprocesses a list of files with given preprocessor and preprocessor options
#
# Args:
#     preproc [in]: Preprocessor program
#     preprocopts [in]: Preprocessor options
#     srcext [in]: File extension of the source files
#     trgext [in]: File extension of the target files
#     srcfiles [in]: List of the source files
#     trgfiles [out]: Contains the list of the preprocessed files on exit
#
function(preprocess preproc preprocopts srcext trgext srcfiles trgfiles)

  if(NOT preproc OR "${preproc}" MATCHES "NOTFOUND")
    message(FATAL_ERROR "preprocess function called with invalid preprocessor path: '${preproc}'")
  endif()

  set(_trgfiles)
  foreach(srcfile IN LISTS srcfiles)
    string(REGEX REPLACE "\\.${srcext}$" ".${trgext}" trgfile ${srcfile})
    set(trgpath ${CMAKE_CURRENT_BINARY_DIR}/${trgfile})
    get_filename_component(trgdir ${trgpath} DIRECTORY)
    add_custom_command(
      OUTPUT ${trgpath}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${trgdir}
      COMMAND ${preproc} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile} ${trgpath}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}
      VERBATIM)
    list(APPEND _trgfiles ${trgpath})
  endforeach()
  set(${trgfiles} ${_trgfiles} PARENT_SCOPE)

endfunction()

# Preprocesses fortran files with fypp.
#
# It assumes that source files have the ".fypp" extension. Target files will be
# created the extension ".f90". The FYPP variable must contain the path to the
# fypp-preprocessor.
#
# Args:
#     fyppopts [in]: Options to pass to fypp.
#     fyppfiles [in]: Files to be processed by fypp
#     f90files [out]: List of created f90 files on exit
#
function (fypp_f90 fyppopts fyppfiles f90files)
  preprocess("${FYPP}" "${fyppopts}" "fypp" "f90" "${fyppfiles}" _f90files)
  set(${f90files} ${_f90files} PARENT_SCOPE)
endfunction()

# Preprocesses Fortran files with fypp and keeps compiler preprocessing enabled.
#
# It assumes that source files have the ".fypp" extension. Target files will be
# created with the extension ".F90". The FYPP variable must contain the path to
# the fypp-preprocessor.
#
# Args:
#     fyppopts [in]: Options to pass to fypp.
#     fyppfiles [in]: Files to be processed by fypp
#     F90files [out]: List of created F90 files on exit
#
function (fypp_f90pp fyppopts fyppfiles F90files)
  preprocess("${FYPP}" "${fyppopts}" "fypp" "F90" "${fyppfiles}" _F90files)
  set(${F90files} ${_F90files} PARENT_SCOPE)
endfunction()
