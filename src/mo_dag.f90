!> \file    mo_dag.f90
!> \copydoc mo_dag

!> \brief   Module providing an implementation of an directed acyclic graph (DAG).
!> \details A simple DAG implementation based on daglib (https://github.com/jacobwilliams/daglib).
!> \copyright daglib was originally released under the BSD 3-Clause license (included below).
!!
!! Copyright (c) 2018-2023, Jacob Williams
!! All rights reserved.
!!
!! Redistribution and use in source and binary forms, with or without modification,
!! are permitted provided that the following conditions are met:
!!
!! - Redistributions of source code must retain the above copyright notice, this
!!   list of conditions and the following disclaimer.
!!
!! - Redistributions in binary form must reproduce the above copyright notice, this
!!   list of conditions and the following disclaimer in the documentation and/or
!!   other materials provided with the distribution.
!!
!! - The names of its contributors may not be used to endorse or promote products
!!   derived from this software without specific prior written permission.
!!
!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
!! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
!! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
!! ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!> \version 0.1
!> \authors Jacob Williams, Sebastian Mueller
!> \date    May 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_dag
  use mo_kind, only : i8

  !> \class node
  !> \brief A node of a directed acyclic graph (DAG)
  type :: node
    !> The nodes that this node depends on (directed edges of the graph).
    !! The indices are the respective node index in the nodes list in the dag
    !! and do not necessarily match the node%tag (e.g. when nodes are removed, their IDs stay untouched)
    integer(i8),dimension(:),allocatable :: edges
    integer(i8) :: tag = 0_i8 !< node tag for external reference (will stay the same even when the DAG is modified)
    logical, private :: checking = .false.  !< used for toposort
    logical, private :: marked = .false.    !< used for toposort
  contains
    private
    generic :: set_edges => set_edge_vector, add_edge
    procedure :: set_edge_vector, add_edge
    procedure :: remove_edge
  end type node

  !> \class dag
  !> \brief A directed acyclic graph (DAG).
  !> \details A collection of nodes that are connected (have a dependency) to other nodes.
  type,public :: dag
    integer(i8) :: n = 0 !< number of nodes (size of `nodes` array)
    !> The nodes in the DAG. The index in this array is used by the edges of the nodes.
    type(node),dimension(:),allocatable :: nodes
    integer(i8), private, dimension(:), allocatable :: tag_to_id_map !< Maps node tag to array index
    integer(i8), private :: max_tag = 0_i8 !< Max tag value in current DAG (for bounds)
  contains
    procedure :: init                => dag_set_nodes
    procedure :: get_edges           => dag_get_edges
    procedure :: get_dependents      => dag_get_dependents
    procedure :: add_edge            => dag_add_edge
    procedure :: set_edges           => dag_set_edges
    procedure :: remove_edge         => dag_remove_edge
    procedure :: remove_node         => dag_remove_node
    procedure :: toposort            => dag_toposort
    procedure :: generate_dependency_matrix => dag_generate_dependency_matrix
    procedure :: destroy             => dag_destroy
    procedure :: tag_to_id
    procedure, private :: init_internal_vars !< private routine to initialize some internal variables
    procedure, private :: rebuild_tag_map
  end type dag

contains

  !> \brief Rebuild the map from node tags to array indices.
  subroutine rebuild_tag_map(this)
    class(dag), intent(inout) :: this
    integer(i8) :: i
    if (allocated(this%tag_to_id_map)) deallocate(this%tag_to_id_map)
    if (.not.allocated(this%nodes)) return
    this%max_tag = maxval(this%nodes(:)%tag)
    allocate(this%tag_to_id_map(this%max_tag), source=-1_i8) ! Initialize with invalid index
    do i = 1_i8, this%n
      this%tag_to_id_map(this%nodes(i)%tag) = i
    end do
  end subroutine rebuild_tag_map

  !> \brief Get the current array index for a given node tag.
  !> \details Returns -1 if the tag does not exist or is out of bounds.
  pure elemental function tag_to_id(this, tag) result(id)
    class(dag), intent(in) :: this
    integer(i8), intent(in) :: tag
    integer(i8) :: id
    if (.not.allocated(this%tag_to_id_map)) then
      id = -1_i8
    else if (tag < 1_i8 .or. tag > size(this%tag_to_id_map)) then
      id = -1_i8
    else
      id = this%tag_to_id_map(tag)
    end if
  end function tag_to_id

  !> \brief Destroy the `dag`.
  subroutine dag_destroy(this)
    class(dag),intent(inout) :: this
    this%n = 0_i8
    if (allocated(this%nodes)) deallocate(this%nodes)
  end subroutine dag_destroy

  !> \brief Specify the edge indices for this node
  subroutine set_edge_vector(this,edges)
    class(node),intent(inout) :: this
    integer(i8),dimension(:),intent(in) :: edges
    this%edges = edges
    call sort_ascending(this%edges)
  end subroutine set_edge_vector

  !> \brief Add an edge index for this node
  subroutine add_edge(this,e)
    class(node),intent(inout) :: this
    integer(i8),intent(in) :: e
    if (allocated(this%edges)) then
      if (.not. any(e==this%edges)) then ! don't add if already there
        this%edges = [this%edges, e]
        call sort_ascending(this%edges)
      end if
    else
      this%edges = [e]
    end if
  end subroutine add_edge

  !> \brief Remove an edge index from this node
  subroutine remove_edge(this,e)
    class(node),intent(inout) :: this
    integer(i8),intent(in) :: e
    integer(i8),dimension(1) :: idx
    integer(i8),dimension(:),allocatable :: tmp

    if (allocated(this%edges)) then
      idx = findloc(this%edges, e)
      if (idx(1)==0_i8) return
      ! the edge is in the list
      associate (i => idx(1), n => size(this%edges))
        if (n==1_i8) then
          deallocate(this%edges) ! it's the only one there
        else
          allocate(tmp(n-1_i8))
          if (i>1_i8) tmp(1_i8:i-1_i8) = this%edges(1_i8:i-1_i8)
          if (i<n) tmp(i:n-1_i8) = this%edges(i+1_i8:n)
          call move_alloc(tmp,this%edges)
        end if
      end associate
    end if

  end subroutine remove_edge

  !> \brief Remove a node from a dag. Will also remove any edges connected to it.
  !> \details This will renumber the nodes and edges internally.
  !! Note that any default integer tag generated in dag%init would then be questionable.
  subroutine dag_remove_node(this,id)
    class(dag),intent(inout) :: this
    integer(i8),intent(in) :: id !! the node to remove
    integer(i8) :: i !! counter
    type(node),dimension(:),allocatable :: tmp !! for resizing `dag%nodes`

    if (allocated(this%nodes)) then
      associate (n => size(this%nodes))
        do i = 1_i8, n
          ! first remove any edges:
          call this%nodes(i)%remove_edge(id)
          ! next, renumber the existing edges so they will be
          ! correct after id is deleted
          ! Example (removing 2): 1 [2] 3 4 ==> 1 2 3
          if (.not.allocated(this%nodes(i)%edges)) cycle
          where (this%nodes(i)%edges>id)
              this%nodes(i)%edges = this%nodes(i)%edges - 1_i8
          end where
          ! node%tag stays the same to conserve references
          ! if (this%nodes(i)%tag > id) this%nodes(i)%tag = this%nodes(i)%tag - 1_i8
        end do
        ! now, remove the node:
        allocate(tmp(n-1_i8))
        if (id>1_i8) tmp(1_i8:id-1_i8) = this%nodes(1_i8:id-1_i8)
        if (id<n) tmp(id:n-1_i8) = this%nodes(id+1_i8:n)
        call move_alloc(tmp,this%nodes)
      end associate
    end if
    this%n = size(this%nodes, kind=i8)
    if (this%n==0_i8) deallocate(this%nodes)
    call this%rebuild_tag_map()

  end subroutine dag_remove_node

  !> \brief Get the edges for the node (all of the nodes that this node depends on).
  pure function dag_get_edges(this,id) result(edges)
    class(dag),intent(in) :: this
    integer(i8),intent(in) :: id
    integer(i8),dimension(:),allocatable :: edges
    if (.not.allocated(this%nodes(id)%edges)) return
    if (id>0_i8 .and. id <= this%n) edges = this%nodes(id)%edges  ! auto LHS allocation
  end function dag_get_edges

  !> \brief Get all the nodes that depend on this node.
  pure function dag_get_dependents(this,id) result(dep)
    class(dag),intent(in) :: this
    integer(i8),intent(in) :: id
    integer(i8),dimension(:),allocatable :: dep  !! the set of all nodes that depend on given node
    integer(i8) :: i !! node counter
    if (id<1_i8 .or. id > this%n) return
    ! have to check all the nodes:
    do i=1_i8, this%n
      if (.not.allocated(this%nodes(i)%edges)) cycle
      if (.not.any(this%nodes(i)%edges == id)) cycle
      if (allocated(dep)) then
        dep = [dep, i]  ! auto LHS allocation
      else
        dep = [i]       ! auto LHS allocation
      end if
    end do
  end function dag_get_dependents

  !> \brief Set the number of nodes in the dag.
  subroutine dag_set_nodes(this, n, tags)
    use mo_message, only: error_message
    class(dag), intent(inout) :: this
    integer(i8), intent(in)   :: n !< number of nodes
    integer(i8), dimension(n), intent(in), optional :: tags !< tags of the nodes (will be their index by default)
    integer(i8) :: i !! counter
    if (n<=0_i8) call error_message('error: n must be >= 1')
    if (allocated(this%nodes)) deallocate(this%nodes)
    this%n = n
    allocate(this%nodes(n))
    if (present(tags)) then
      this%nodes%tag = tags
    else
      this%nodes%tag = [(i,i=1_i8,n)] ! default node tags from array IDs (accessing tag array as structure component)
    end if
    call this%rebuild_tag_map()
  end subroutine dag_set_nodes

  !> \brief Add an edge to a dag.
  subroutine dag_add_edge(this,id,target_id)
    class(dag),intent(inout) :: this
    integer(i8),intent(in)   :: id !< node id
    integer(i8),intent(in)   :: target_id !< the node to connect to `id`
    call this%nodes(id)%set_edges(target_id)
  end subroutine dag_add_edge

  !> \brief Set the edges for a node in a dag
  subroutine dag_set_edges(this,id,edges)
    class(dag),intent(inout)            :: this
    integer(i8),intent(in)              :: id !< node id
    integer(i8),dimension(:),intent(in) :: edges
    call this%nodes(id)%set_edges(edges)
  end subroutine dag_set_edges

  !> \brief Remove an edge from a dag.
  subroutine dag_remove_edge(this,id,target_id)
    class(dag),intent(inout) :: this
    integer(i8),intent(in)   :: id !< node id
    integer(i8),intent(in)   :: target_id !< the edge to remove
    call this%nodes(id)%remove_edge(target_id)
  end subroutine dag_remove_edge

  !> \brief Initialize the internal private variables used for graph traversal.
  subroutine init_internal_vars(this)
    class(dag),intent(inout) :: this
    integer(i8) :: i !< counter
    do i = 1_i8, this%n
      this%nodes(i)%marked = .false.
      this%nodes(i)%checking = .false.
    end do
  end subroutine init_internal_vars

  !> \brief Main toposort routine
  subroutine dag_toposort(this,order,istat)
    class(dag),intent(inout) :: this
    integer(i8),dimension(:),allocatable,intent(out) :: order !< the toposort order (contains the node%tag for correct reference)
    !> Status flag: 0 (if no errors), -1 (if circular dependency, in this case, `order` will not be allocated)
    integer(i8),intent(out) :: istat
    integer(i8) :: i,iorder

    if (this%n==0_i8) return
    ! initialize internal variables, in case
    ! we have called this routine before.
    call this%init_internal_vars()

    allocate(order(this%n))
    iorder = 0_i8  ! index in order array
    istat = 0_i8   ! no errors so far
    do i=1_i8,this%n
      if (.not. this%nodes(i)%marked) call dfs(this%nodes(i))
      if (istat==-1_i8) exit
    end do

    if (istat==-1_i8) deallocate(order)

  contains

    !> \brief depth-first graph traversal
    recursive subroutine dfs(v)
      type(node),intent(inout) :: v
      integer(i8) :: j

      if (istat==-1_i8) return
      if (v%checking) then
        ! error: circular dependency
        istat = -1_i8
      else
        if (.not. v%marked) then
          v%checking = .true.
          if (allocated(v%edges)) then
            do j=1_i8,size(v%edges)
              call dfs(this%nodes(v%edges(j)))
              if (istat==-1_i8) return
            end do
          end if
          v%checking = .false.
          v%marked = .true.
          iorder = iorder + 1_i8
          order(iorder) = v%tag
        end if
      end if
    end subroutine dfs

  end subroutine dag_toposort

  !> \brief Generate the dependency matrix for the DAG.
  !> \details This is an \(n \times n \) matrix with elements \(A_{ij}\),
  !! such that \(A_{ij}\) is true if node \(i\) depends on node \(j\).
  subroutine dag_generate_dependency_matrix(this,mat)
    class(dag),intent(in) :: this
    logical,dimension(:,:),intent(out),allocatable :: mat !< dependency matrix
    integer(i8) :: i ! node counter
    integer(i8) :: j ! edge counter
    if (this%n < 1_i8) return
    allocate(mat(this%n,this%n))
    mat = .false.
    do i=1_i8,this%n
      if (.not.allocated(this%nodes(i)%edges)) cycle
      do j = 1_i8, size(this%nodes(i)%edges)
        mat(i,this%nodes(i)%edges(j)) = .true.
      end do
    end do
  end subroutine dag_generate_dependency_matrix

  !> \brief Sorts an edge array `ivec` in increasing order by node number.
  !> \details Uses a basic recursive quicksort (with insertion sort for partitions with \(\le\) 20 elements).
  subroutine sort_ascending(ivec)
    integer(i8),dimension(:),intent(inout) :: ivec
    integer(i8),parameter :: max_size_for_insertion_sort = 20_i8 !! max size for using insertion sort.
    call quicksort(1_i8,size(ivec,kind=i8))
  contains

    !> \brief Sort the array
    recursive subroutine quicksort(ilow,ihigh)
      integer(i8),intent(in) :: ilow
      integer(i8),intent(in) :: ihigh
      integer(i8) :: ipivot !! pivot element
      integer(i8) :: i      !! counter
      integer(i8) :: j      !! counter

      if ( ihigh-ilow<=max_size_for_insertion_sort .and. ihigh>ilow ) then

        ! do insertion sort:
        do i = ilow + 1_i8,ihigh
          do j = i,ilow + 1_i8,-1_i8
            if ( ivec(j) < ivec(j-1_i8) ) then
              call swap(ivec(j),ivec(j-1_i8))
            else
              exit
            end if
          end do
        end do

      elseif ( ihigh-ilow>max_size_for_insertion_sort ) then
        ! do the normal quicksort:
        call partition(ilow,ihigh,ipivot)
        call quicksort(ilow,ipivot - 1_i8)
        call quicksort(ipivot + 1_i8,ihigh)
      end if
    end subroutine quicksort

    !> \brief Partition the array, based on the lexical ivecing comparison.
    subroutine partition(ilow,ihigh,ipivot)
      integer(i8),intent(in)  :: ilow
      integer(i8),intent(in)  :: ihigh
      integer(i8),intent(out) :: ipivot
      integer(i8) :: i,ii

      call swap(ivec(ilow),ivec((ilow+ihigh)/2_i8))
      ii = ilow
      do i = ilow + 1_i8, ihigh
        if ( ivec(i) < ivec(ilow) ) then
          ii = ii + 1_i8
          call swap(ivec(ii),ivec(i))
        end if
      end do
      call swap(ivec(ilow),ivec(ii))
      ipivot = ii
    end subroutine partition

  end subroutine sort_ascending

  !> \brief Swap two edge values.
  pure elemental subroutine swap(x,y)
    integer(i8),intent(inout) :: x
    integer(i8),intent(inout) :: y
    integer(i8) :: tmp
    tmp = x
    x = y
    y = tmp
  end subroutine swap

end module mo_dag
