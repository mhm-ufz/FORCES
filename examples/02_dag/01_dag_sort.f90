!> \file    02_dag/01_dag_sort.f90
!> \copydoc dag_sort

!> \example 02_dag/01_dag_sort.f90
!> \copydoc dag_sort

!> \brief   Example for creating and sorting a DAG.
!> \details This program demonstrates how to create a directed acyclic graph (DAG) and how to sort it.
!> \authors Sebastian Mueller
!> \date    Sep 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program dag_sort
  use mo_dag, only: dag, order_t
  use mo_kind, only: i8
  implicit none
  type(dag) :: network
  type(order_t) :: ord
  integer(i8),dimension(:),allocatable :: order
  integer(i8) :: istat

  !    1 -> 2 -> 6
  !    |    |    A
  !    V    V    |
  !    3 <- 5 -> 4
  call network%init(6_i8)
  call network%set_sources(2_i8,[1_i8])      ! 2 depends on 1
  call network%set_sources(3_i8,[5_i8,1_i8]) ! 3 depends on 5 and 1
  call network%set_sources(4_i8,[5_i8])      ! 4 depends on 5
  call network%set_sources(5_i8,[2_i8])      ! 5 depends on 2
  call network%set_sources(6_i8,[2_i8,4_i8]) ! 6 depends on 2 and 4

  ! toposort
  call network%toposort(order,istat)
  print*, ""
  print*, "TOPOSORT"
  print '(a,*(1x,i2))', "order", order

  ! levelsort
  call network%levelsort(ord, istat, root=.false.)
  call ord%sort()
  print*, ""
  print*, "LEVELSORT leaf based"
  print '(a,*(1x,i2))', "order      ", ord%id
  print '(a,*(1x,i2))', "# levels   ", ord%n_levels
  print '(a,*(1x,i2))', "level start", ord%level_start
  print '(a,*(1x,i2))', "level size ", ord%level_size ! node 3 and 4 can be computed in parallel

  call network%levelsort(ord, istat, root=.true.)
  call ord%sort()
  print*, ""
  print*, "LEVELSORT root based"
  print '(a,*(1x,i2))', "order      ", ord%id
  print '(a,*(1x,i2))', "# levels   ", ord%n_levels
  print '(a,*(1x,i2))', "level start", ord%level_start
  print '(a,*(1x,i2))', "level size ", ord%level_size ! node 3 and 4 can be computed in parallel
end program dag_sort
