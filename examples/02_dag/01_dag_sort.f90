!> \file    01_dag_sort.f90
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
  call network%set_edges(2_i8,[1_i8])      ! 2 depends on 1
  call network%set_edges(3_i8,[5_i8,1_i8]) ! 3 depends on 5 and 1
  call network%set_edges(4_i8,[5_i8])      ! 4 depends on 5
  call network%set_edges(5_i8,[2_i8])      ! 5 depends on 2
  call network%set_edges(6_i8,[2_i8,4_i8]) ! 6 depends on 2 and 4

  ! toposort
  call network%toposort(order,istat)
  print*, ""
  print*, "TOPOSORT"
  print*, "order", order

  ! levelsort
  call network%levelsort(ord, istat)
  print*, ""
  print*, "LEVELSORT"
  print*, "order      ", ord%id
  print*, "# levels   ", ord%n_levels
  print*, "level start", ord%level_start
  print*, "level size ", ord%level_size ! node 3 and 4 can be computed in parallel

end program dag_sort
