PROGRAM main

  ! \ls mo_*.f90 | sed -e 's/mo_/  USE mo_/' -e 's/.f90//' | \
  !    sed -e 's/USE mo_minpack/!USE mo_minpack/' -e 's/USE mo_template/!USE mo_template/' \
  !        -e 's/USE mo_nr$/!USE mo_nr/' -e 's/USE mo_pumpingtests/!USE mo_pumpingtests/' \
  !        -e 's/USE mo_qhull/!USE mo_qhull/' \
  ! make SRCPATH=~/prog/ufz/chs-svn/FORTRAN_chs_lib EXCLUDE_FILES='../FORTRAN_chs_lib/mo_minpack.f90 \
  !     ../FORTRAN_chs_lib/mo_nr.f90 ../FORTRAN_chs_lib/mo_template.f90 \
  !     ../FORTRAN_chs_lib/mo_pumpingtests.f90 ../FORTRAN_chs_lib/mo_qhull.f90'
  USE mo_anneal
  USE mo_append
  USE mo_constants
  USE mo_corr
  USE mo_dds
  USE mo_errormeasures
  USE mo_finish
  USE mo_julian
  USE mo_kind
  USE mo_linfit
  USE mo_mcmc
  USE mo_message
  USE mo_moment
  USE mo_ncread
  USE mo_ncwrite
  USE mo_netcdf
  USE mo_nml
  USE mo_orderpack
  USE mo_percentile
  USE mo_sce
  USE mo_spatialsimilarity
  USE mo_standard_score
  USE mo_string_utils
  USE mo_timer
  USE mo_utils
  USE mo_xor4096

  IMPLICIT NONE

  Write(*,*) 'I can compile.'

END PROGRAM main
