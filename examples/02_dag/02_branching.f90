!> \file    02_dag/02_branching.f90

!> \example 02_dag/02_branching.f90

!> \brief   Example for using the branching DAG representation.
!> \details Demonstrates how to initialize a branching DAG with a downstream
!! linkage array and how to perform a topological sort.
!> \authors Sebastian Mueller
!> \date    Sep 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
program dag_branching
  use mo_dag, only: branching
  use mo_kind, only: i8
  implicit none
  type(branching) :: river
  integer(i8), allocatable :: order(:)
  integer(i8) :: istat
  integer(i8), parameter :: down(5) = [2_i8, 3_i8, 0_i8, 2_i8, 4_i8]

  call river%init(down)

  call river%toposort(order, istat)
  print *, 'Branching DAG toposort order: ', order

  call river%destroy()
end program dag_branching
