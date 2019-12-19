PROGRAM main

  USE mo_kind,    ONLY: dp, sp
  USE mo_boxcox,  ONLY: boxcox, invboxcox

  IMPLICIT NONE

  LOGICAL :: isgood

  real(dp), dimension(20) :: dx  = (/ 4.86, 4.92, 3.37, 3.29, 4.43, 1.53, 3.76, 0.35, &
                                      1.73, 1.98, 4.26, 4.64, 2.88, 2.35, 8.35, 5.6 , &
                                      4.28,13.04, 8.94, 3.94 /)
  real(dp), dimension(20) :: dy, dyx
  real(dp)                :: dl = 2.5_dp
  real(sp), dimension(20) :: sx  = (/ 4.86, 4.92, 3.37, 3.29, 4.43, 1.53, 3.76, 0.35, &
                                      1.73, 1.98, 4.26, 4.64, 2.88, 2.35, 8.35, 5.6 , &
                                      4.28,13.04, 8.94, 3.94 /)
  real(sp), dimension(20) :: sy, syx
  real(sp)                :: sl = 2.5_sp

  Write(*,*) ''
  Write(*,*) 'Test mo_boxcox.f90'
  Write(*,*) ''

  ! Double precision
  Write(*,*) ''
  isgood = .true.
  dy  = boxcox(dx,dl)
  dyx = invboxcox(dy,dl)
  if (any(abs(dx-dyx) > 1000._dp*epsilon(1.0_dp))) isgood = .False.

  if (isgood) then
     write(*,*) 'mo_boxcox double precision o.k.'
  else
     write(*,*) 'mo_boxcox double precision failed!'
  endif

  ! Single precision
  Write(*,*) ''
  isgood = .true.
  sy  = boxcox(sx,sl)
  syx = invboxcox(sy,sl)
  if (any(abs(sx-syx) > 1000._sp*epsilon(1.0_sp))) isgood = .False.

  if (isgood) then
     write(*,*) 'mo_boxcox single precision o.k.'
  else
     write(*,*) 'mo_boxcox single precision failed!'
  endif

END PROGRAM main
