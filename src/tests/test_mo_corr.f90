PROGRAM main

  USE mo_kind,   ONLY: dp, sp
  USE mo_corr,   ONLY: autocorr

  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 1500
  REAL(dp), DIMENSION(n) :: dat1, dat2  !, dc
  REAL(sp), DIMENSION(n) :: sat1, sat2  !, sc
  INTEGER :: i  !, nout

  LOGICAL :: isgood

  Write(*,*) ''
  Write(*,*) 'Test mo_corr.f90'

  ! Double precision
  forall(i=1:n) dat1(i) = real(i,dp)
  dat2 = dat1 + sin(dat1)
  isgood = .true.
  if (abs(autocorr(dat2,0) - 1.0_dp) > epsilon(1.0_dp)) isgood =.false.
  if (isgood) then
     write(*,*) 'mo_corr double precision o.k.'
  else
     write(*,*) 'mo_corr double precision failed!'
  endif

  ! Single precision
  forall(i=1:n) sat1(i) = real(i,sp)
  sat2 = sat1 + sin(sat1)
  isgood = .true.
  if (abs(autocorr(sat2,0) - 1.0_sp) > epsilon(1.0_sp)) isgood =.false.
  if (isgood) then
     write(*,*) 'mo_corr single precision o.k.'
  else
     write(*,*) 'mo_corr single precision failed!'
  endif

END PROGRAM main
