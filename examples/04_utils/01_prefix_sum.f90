!> \file    04_utils/compare_prefix_sum.f90
!> \example 04_utils/compare_prefix_sum.f90

!> \brief   Compare mo_utils prefix_sum against intrinsic cumsum with optional shift/start.
!> \details Runs small correctness cases and (with OpenMP) optional timing of large arrays.
!> \authors Sebastian Mueller
!> \date    Nov 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program compare_prefix_sum
  use mo_kind,  only: i8, dp
  use mo_utils, only: prefix_sum, cumsum
  !$ use omp_lib, only: OMP_GET_NUM_THREADS, omp_get_wtime
  implicit none
  !$ integer :: n_threads
  integer(i8), parameter :: n = 20_i8
  integer(i8) :: input(n), pref(n), shifted(n), csum(n)
  integer(i8) :: shift, start, i

  !$omp parallel
  !$ n_threads = OMP_GET_NUM_THREADS()
  !$omp end parallel
  !$ print*, 'OpenMP threads ', n_threads

  input = [(i, i=1_i8, n)]

  ! shift = 0, start = 0 -> matches intrinsic cumsum
  shift = 0_i8
  start = 0_i8
  call prefix_sum(input, pref, shift=shift, start=start, block_size=5_i8)
  csum = cumsum(input)
  call print_case('shift=0, start=0 (matches cumsum)', input, pref, csum)

  ! shift = 1, start = 1 -> delayed accumulation
  shift = 1_i8
  start = 1_i8
  ! adjust cumsum for comparison
  csum(1_i8+shift:n) = csum(1_i8:n-shift)
  csum(1_i8:shift) = 0_i8
  csum = csum + start
  call prefix_sum(input, shifted, shift=shift, start=start, block_size=5_i8)
  call print_case('shift=1, start=1 (matches adjusted cumsum)', input, shifted, csum)

  !$ call time_large_case()

contains

  subroutine print_case(title, a, p, ref)
    character(len=*), intent(in) :: title
    integer(i8), intent(in) :: a(:)
    integer(i8), intent(in) :: p(:)
    integer(i8), intent(in), optional :: ref(:)
    integer :: j

    print *, ''
    print *, trim(title)
    write(*,'(a,*(1x,i4))') 'input     :', a
    write(*,'(a,*(1x,i4))') 'prefix_sum:', p
    if (present(ref)) write(*,'(a,*(1x,i4))') 'cumsum    :', ref
    do j = 1, size(p)
      if (present(ref)) then
        if (p(j) /= ref(j)) then
          write(*,'(a,i4,a,i4,a,i4)') '  mismatch at', j, ':', p(j), '/=', ref(j)
          exit
        end if
      end if
    end do
  end subroutine print_case

  !$ subroutine time_large_case()
  !$   integer(i8), parameter :: n_big = 10000000_i8
  !$   integer(i8), allocatable :: tmp_in(:), tmp_out(:)
  !$   real(dp) :: t0, t1
  !$
  !$   allocate(tmp_in(n_big), tmp_out(n_big))
  !$   tmp_in = 1_i8
  !$
  !$   t0 = omp_get_wtime()
  !$   call prefix_sum(tmp_in, tmp_out, shift=0_i8, start=0_i8)
  !$   t1 = omp_get_wtime()
  !$   print *, ''
  !$   print *, 'timing n=', n_big, ' shift=0 start=0 : ', t1 - t0, 's'
  !$
  !$   t0 = omp_get_wtime()
  !$   call prefix_sum(tmp_in, tmp_out, shift=2_i8, start=5_i8)
  !$   t1 = omp_get_wtime()
  !$   print *, 'timing n=', n_big, ' shift=2 start=5 : ', t1 - t0, 's'
  !$
  !$   deallocate(tmp_in, tmp_out)
  !$ end subroutine time_large_case

end program compare_prefix_sum
