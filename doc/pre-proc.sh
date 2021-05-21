#!/bin/bash
if [[ -n "${FC}" ]]; then
  if [ "${FC}" = "nagfor" ]; then
    nagfor -fpp -F -o tempfile-doxygen.fpp $1 &>/dev/null
    cat tempfile-doxygen.fpp
    rm -f tempfile-doxygen.fpp
  else
    $FC -cpp -E $1
  fi
else
  if [[ -n "(command -v gfortran)" ]]; then
    gfortran -cpp -E $1
  fi
fi
