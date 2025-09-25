!> \file    mo_dag.f90
!> \copydoc mo_dag

!> \brief   Module providing an implementation of an directed acyclic graph (DAG).
!> \details A simple DAG implementation based on daglib (https://github.com/jacobwilliams/daglib).
!! \par Examples
!! - \ref 01_dag_sort.f90 : \copybrief 01_dag_sort.f90
!!   \include 01_dag_sort.f90
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
  use mo_kind, only : i8, i4
  use mo_utils, only: optval, swap

  ! Control constants for depth-first traversal behavior.
  ! These values are returned by `visit()` to control DFS branching.
  integer(i8), parameter :: dfs_continue      = 0_i8  !< Continue traversal to children
  integer(i8), parameter :: dfs_skip_children = 1_i8  !< Skip children of this node
  integer(i8), parameter :: dfs_stop_all      = 2_i8  !< Stop traversal entirely

  !> \class traversal_handler
  !> \brief Abstract base type for DFS traversal handlers.
  !> \details Users should extend this type and implement the deferred `visit()` procedure.
  !! The handler can maintain state, such as visited status, counters, or data accumulators.
  type, abstract :: traversal_handler
    !> \brief Per-node visited state. Must be allocated before traversal.
    logical, allocatable :: visited(:)
  contains
    !> \brief User-defined procedure called on each visited node.
    !> \details This function should return one of:
    !! - `dfs_continue` to keep traversing from this node,
    !! - `dfs_skip_children` to skip its descendants,
    !! - `dfs_stop_all` to stop traversal completely.
    procedure(visit_if), deferred :: visit
  end type traversal_handler

  !> \brief Abstract interface for the `visit` function in a handler.
  !! \return One of `dfs_continue`, `dfs_skip_children`, `dfs_stop_all`.
  abstract interface
    function visit_if(this, id, tag) result(action)
      use mo_kind, only: i8
      import :: traversal_handler
      class(traversal_handler), intent(inout) :: this
      integer(i8), intent(in) :: id !< The index of the node being visited.
      integer(i8), intent(in) :: tag !< The tag of the node being visited.
      integer(i8) :: action
    end function
  end interface

  !> \class traversal_visit
  !> \brief Simple traversal handler to visit all dependencies.
  type, extends(traversal_handler) :: traversal_visit
  contains
    procedure :: visit => visit_simple
  end type traversal_visit

  !> \class order_t
  !> \brief Store level based ordering of a DAG.
  type :: order_t
    integer(i8), allocatable :: id(:)           !< Node indices in topological order
    integer(i8), allocatable :: level_start(:)  !< Start indices in id(:) for respective level
    integer(i8), allocatable :: level_end(:)    !< End indices in id(:) for respective level
    integer(i8), allocatable :: level_size(:)   !< Size of respective level
    integer(i8) :: n_levels                     !< Number of levels
  contains
    procedure :: reverse => order_reverse
  end type order_t

  !> \class node
  !> \brief A node of a directed acyclic graph (DAG)
  type :: node
    !> The nodes that this node depends on (directed edges of the graph).
    !! The indices are the respective node index in the nodes list in the dag
    !! and do not necessarily match the node%tag (e.g. when nodes are removed, their tags stay untouched)
    integer(i8),dimension(:),allocatable :: edges
    !> nodes that contain an edge to this node
    integer(i8),dimension(:),allocatable :: dependents
    integer(i8) :: tag = 0_i8 !< node tag for external reference (will stay the same even when the DAG is modified)
  contains
    procedure :: nedges => node_nedges
    procedure :: ndependents => node_ndependents
    generic, private :: set_edges => set_edge_vector, add_edge
    procedure, private :: set_edge_vector => node_set_edge_vector
    procedure, private :: add_edge => node_add_edge
    procedure, private :: add_dependent => node_add_dependent
  end type node

  !> \class dag
  !> \brief A directed acyclic graph (DAG).
  !> \details A collection of nodes that are connected (have a dependency) to other nodes.
  !!
  !! \par Examples
  !! - \ref 01_dag_sort.f90 : \copybrief 01_dag_sort.f90
  type,public :: dag
    integer(i8) :: n = 0 !< number of nodes (size of `nodes` array)
    !> The nodes in the DAG. The index in this array is used by the edges of the nodes.
    type(node),dimension(:),allocatable :: nodes
    integer(i8), private, dimension(:), allocatable :: tag_to_id_map !< Maps node tag to array index
    integer(i8), private :: max_tag = 0_i8 !< Max tag value in current DAG (for bounds)
  contains
    procedure :: init                       => dag_set_nodes
    procedure :: add_edge                   => dag_add_edge
    procedure :: set_edges                  => dag_set_edges
    procedure :: toposort                   => dag_toposort
    procedure :: levelsort                  => dag_levelsort
    procedure :: generate_dependency_matrix => dag_generate_dependency_matrix
    procedure :: traverse                   => dag_traverse
    procedure :: subgraph                   => dag_subgraph
    procedure :: get_dependencies           => dag_get_dependencies
    procedure :: get_dependents             => dag_get_dependents
    procedure :: destroy                    => dag_destroy
    procedure :: tag_to_id                  => dag_tag_to_id
    procedure, private :: levelsort_head    => dag_levelsort_head
    procedure, private :: levelsort_root    => dag_levelsort_root
    procedure, private :: rebuild_tag_map   => dag_rebuild_tag_map
  end type dag

contains

  !> \brief Reverse order.
  subroutine order_reverse(this)
    use mo_utils, only: flip
    class(order_t), intent(inout) :: this
    integer(i8), allocatable :: tmp(:)
    integer(i8) :: n
    call flip(this%id, idim=1_i4)
    call flip(this%level_size, idim=1_i4)
    call flip(this%level_start, idim=1_i4)
    call flip(this%level_end, idim=1_i4)
    ! numbering needs to match reversed id array
    call move_alloc(this%level_start, tmp)
    call move_alloc(this%level_end, this%level_start)
    call move_alloc(tmp, this%level_end)
    ! call swap(this%level_start, this%level_end)
    n = size(this%id)
    this%level_start = n + 1_i8 - this%level_start
    this%level_end = n + 1_i8 - this%level_end
  end subroutine

  !> \brief Simple visit function to visit all dependencies.
  function visit_simple(this, id, tag) result(action)
    class(traversal_visit), intent(inout) :: this
    integer(i8), intent(in) :: id !< The index of the node being visited.
    integer(i8), intent(in) :: tag !< The tag of the node being visited.
    integer(i8) :: action
    action = dfs_continue
  end function

  !> \brief number of edges for this node
  pure integer(i8) function node_nedges(this)
    class(node),intent(in) :: this
    if (allocated(this%edges)) then
      node_nedges = size(this%edges)
    else
      node_nedges = 0_i8
    end if
  end function node_nedges

  !> \brief number of dependents for this node
  pure integer(i8) function node_ndependents(this)
    class(node),intent(in) :: this
    if (allocated(this%dependents)) then
      node_ndependents = size(this%dependents)
    else
      node_ndependents = 0_i8
    end if
  end function node_ndependents

  !> \brief Specify the edge indices for this node
  subroutine node_set_edge_vector(this,edges)
    class(node),intent(inout) :: this
    integer(i8),dimension(:),intent(in) :: edges
    this%edges = edges
    call sort_ascending(this%edges)
  end subroutine node_set_edge_vector

  !> \brief Add an edge index for this node
  subroutine node_add_edge(this,e)
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
  end subroutine node_add_edge

  !> \brief Add a dependent index for this node
  subroutine node_add_dependent(this,d)
    class(node),intent(inout) :: this
    integer(i8),intent(in) :: d
    if (allocated(this%dependents)) then
      if (.not. any(d==this%dependents)) then ! don't add if already there
        this%dependents = [this%dependents, d]
        call sort_ascending(this%dependents)
      end if
    else
      this%dependents = [d]
    end if
  end subroutine node_add_dependent

  !> \brief Traverse graph from given starting node.
  subroutine dag_traverse(this, handler, ids, down)
    class(dag), intent(in) :: this
    class(traversal_handler), intent(inout) :: handler !< traversal handler to use
    integer(i8), dimension(:), intent(in), optional :: ids !< ids to traverse from (by default all)
    logical, intent(in), optional :: down !< traverse downstream if .true. (.false. by default to traverse upstream)

    integer(i8) :: top, dep, action, i, j
    integer(i8), allocatable :: stack(:)
    logical :: down_

    down_ = optval(down, .false.)
    allocate(stack(this%n))
    if (present(ids)) then
      top = size(ids)
      stack(1_i8:top) = [(ids(i), i=top,1_i8,-1_i8)]
    else
      top = this%n
      stack(1_i8:top) = [(i, i=top,1_i8,-1_i8)]
    end if

    do while (top > 0_i8)
      j = stack(top)
      top = top - 1_i8

      if (handler%visited(j)) cycle
      handler%visited(j) = .true.

      action = handler%visit(j, this%nodes(j)%tag)
      select case (action)
        case (dfs_continue)  ! 0
          if (down_) then
            do i = this%nodes(j)%ndependents(), 1_i8, -1_i8
              dep = this%nodes(j)%dependents(i)
              if (.not. handler%visited(dep)) then
                top = top + 1_i8
                stack(top) = dep
              end if
            end do
          else
            do i = this%nodes(j)%nedges(), 1_i8, -1_i8
              dep = this%nodes(j)%edges(i)
              if (.not. handler%visited(dep)) then
                top = top + 1_i8
                stack(top) = dep
              end if
            end do
          end if
        case (dfs_skip_children) ! 1
          cycle
        case default ! dfs_stop_all (all other)
          exit
      end select
    end do

    deallocate(stack)
  end subroutine dag_traverse

  !> \brief Construct subgraph containing given nodes and their dependencies.
  !> \details This will renumber the node ids but will conserve their tags.
  function dag_subgraph(this, ids, down) result(subgraph)
    class(dag), intent(in) :: this
    integer(i8), dimension(:), intent(in) :: ids !< ids to traverse from (by default all)
    logical, intent(in), optional :: down !< generate downstream graph if .true. (.false. by default to generate upstream graph)
    type(traversal_visit) :: handler
    type(dag) :: subgraph !< dag containing the subgraph containing given nodes and their dependencies
    integer(i8), allocatable :: idmap(:), subtags(:)
    integer(i8) :: i, j, nsub
    logical :: down_

    down_ = optval(down, .false.)
    allocate(handler%visited(this%n), source=.false.)
    call this%traverse(handler, ids, down)

    nsub = count(handler%visited)
    subtags = pack(this%nodes%tag, handler%visited)
    idmap = unpack([(i, i=1_i8, nsub)], handler%visited, 0_i8)

    call subgraph%init(nsub, subtags)
    do i = 1_i8, this%n
      if (.not.handler%visited(i)) cycle
      if (down_) then
        do j = 1_i8, this%nodes(i)%ndependents()
          call subgraph%add_edge(idmap(this%nodes(i)%dependents(j)), idmap(i))
        end do
      else
        call subgraph%set_edges(idmap(i), [(idmap(this%nodes(i)%edges(j)), j=1_i8,this%nodes(i)%nedges())])
      end if
    end do

    deallocate(handler%visited, idmap, subtags)
  end function dag_subgraph

  !> \brief Generate list of all dependencies and their sub-dependencies for a given node.
  function dag_get_dependencies(this, id) result(deps)
    class(dag), intent(in) :: this
    integer(i8), intent(in) :: id !< node id
    integer(i8), allocatable :: deps(:)
    type(traversal_visit) :: handler
    integer(i8) :: i
    if (this%nodes(id)%nedges() == 0_i8) then
      allocate(deps(0))
      return
    end if
    allocate(handler%visited(this%n), source=.false.)
    call this%traverse(handler, this%nodes(id)%edges)
    allocate(deps(count(handler%visited)))
    deps = pack([(i, i=1_i8, this%n)], handler%visited)
    deallocate(handler%visited)
  end function dag_get_dependencies

  !> \brief Generate list of all dependents and their sub-dependents for a given node.
  function dag_get_dependents(this, id) result(deps)
    class(dag), intent(in) :: this
    integer(i8), intent(in) :: id !< node id
    integer(i8), allocatable :: deps(:)
    type(traversal_visit) :: handler
    integer(i8) :: i
    if (this%nodes(id)%ndependents() == 0_i8) then
      allocate(deps(0))
      return
    end if
    allocate(handler%visited(this%n), source=.false.)
    call this%traverse(handler, this%nodes(id)%dependents, down=.true.)
    allocate(deps(count(handler%visited)))
    deps = pack([(i, i=1_i8, this%n)], handler%visited)
    deallocate(handler%visited)
  end function dag_get_dependents

  !> \brief Rebuild the map from node tags to array indices.
  subroutine dag_rebuild_tag_map(this)
    class(dag), intent(inout) :: this
    integer(i8) :: i
    if (allocated(this%tag_to_id_map)) deallocate(this%tag_to_id_map)
    if (.not.allocated(this%nodes)) return
    this%max_tag = maxval(this%nodes(:)%tag)
    allocate(this%tag_to_id_map(this%max_tag), source=-1_i8) ! Initialize with invalid index
    do i = 1_i8, this%n
      this%tag_to_id_map(this%nodes(i)%tag) = i
    end do
  end subroutine dag_rebuild_tag_map

  !> \brief Get the current array index for a given node tag.
  !> \details Returns -1 if the tag does not exist or is out of bounds.
  pure elemental function dag_tag_to_id(this, tag) result(id)
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
  end function dag_tag_to_id

  !> \brief Destroy the `dag`.
  subroutine dag_destroy(this)
    class(dag),intent(inout) :: this
    this%n = 0_i8
    if (allocated(this%nodes)) deallocate(this%nodes)
  end subroutine dag_destroy

  !> \brief Set the number of nodes in the dag.
  subroutine dag_set_nodes(this, n, tags)
    use mo_message, only: error_message
    class(dag), intent(inout) :: this
    integer(i8), intent(in)   :: n !< number of nodes
    integer(i8), dimension(n), intent(in), optional :: tags !< tags of the nodes (will be their index by default)
    integer(i8) :: i !! counter
    if (n<1_i8) call error_message('error: n must be >= 1')
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
    call this%nodes(target_id)%add_dependent(id)
  end subroutine dag_add_edge

  !> \brief Set the edges for a node in a dag
  subroutine dag_set_edges(this,id,edges)
    class(dag),intent(inout)            :: this
    integer(i8),intent(in)              :: id !< node id
    integer(i8),dimension(:),intent(in) :: edges
    integer(i8) :: i
    call this%nodes(id)%set_edges(edges)
    do i = 1_i8, this%nodes(id)%nedges()
      call this%nodes(this%nodes(id)%edges(i))%add_dependent(id)
    end do
  end subroutine dag_set_edges

  !> \brief Main toposort routine
  subroutine dag_toposort(this, order, istat)
    class(dag),intent(inout) :: this
    integer(i8),dimension(:),allocatable,intent(out) :: order !< the toposort order
    !> Status flag: 0 (if no errors), -1 (if circular dependency, in this case, `order` will not be allocated)
    integer(i8),intent(out) :: istat
    integer(i8) :: i,iorder
    logical, allocatable, dimension(:) :: checking, visited

    if (this%n==0_i8) return

    allocate(checking(this%n), source=.false.)
    allocate(visited(this%n), source=.false.)

    allocate(order(this%n))
    iorder = 0_i8  ! index in order array
    istat = 0_i8   ! no errors so far
    do i=1_i8,this%n
      if (.not. visited(i)) call dfs(i)
      if (istat==-1_i8) exit
    end do

    if (istat==-1_i8) then
      deallocate(order)
    end if

    deallocate(checking, visited)

  contains

    !> \brief depth-first graph traversal
    recursive subroutine dfs(j)
      integer(i8), intent(in) :: j
      integer(i8) :: k
      if (istat==-1_i8) return ! error: already circular
      if (checking(j)) then ! error: circular dependency
        istat = -1_i8
        return
      end if
      if ( visited(j)) return ! already touched
      checking(j) = .true.
      do k=1_i8,this%nodes(j)%nedges()
        call dfs(this%nodes(j)%edges(k))
        if (istat==-1_i8) return
      end do
      checking(j) = .false.
      visited(j) = .true.
      iorder = iorder + 1_i8
      order(iorder) = j
    end subroutine dfs

  end subroutine dag_toposort

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm
  subroutine dag_levelsort(this, order, istat, root, reverse)
    implicit none
    class(dag), intent(in) :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: root     !< levels as distance from graph roots (default: .false.)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)
    logical :: root_

    root_ = optval(root, .false.)
    if (root_) then
      call this%levelsort_root(order, istat, reverse)
    else
      call this%levelsort_head(order, istat, reverse)
    end if
  end subroutine dag_levelsort

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm
  subroutine dag_levelsort_head(this, order, istat, reverse)
    implicit none
    class(dag), intent(in) :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)

    integer(i8) :: i, j, k, m, n, count, level, added_this_level
    integer(i8), allocatable :: level_start(:), level_end(:), id(:), visit_level(:)
    logical :: ready, rev

    rev = optval(reverse, .false.)
    n = this%n ! in the worst case of a linear DAG, we get as many levels as nodes
    allocate(visit_level(n), source=0_i8)
    allocate(level_start(n))
    allocate(level_end(n))
    allocate(id(n))

    ! first scan for all dependency free nodes
    count = 0_i8
    level = 1_i8
    do i = 1_i8, n
      if (this%nodes(i)%nedges() > 0_i8) cycle
      count = count + 1_i8
      id(count) = i
      visit_level(i) = level
    end do

    level_start(level) = 1_i8
    level_end(level) = count

    do while (count < n)
      added_this_level = 0_i8
      level = level + 1_i8
      level_start(level) = count + 1_i8

      ! scan all dependents of previous level
      do i = level_start(level-1_i8), level_end(level-1_i8)
        if (.not.allocated(this%nodes(id(i))%dependents)) cycle
        do j = 1_i8, size(this%nodes(id(i))%dependents)
          k = this%nodes(id(i))%dependents(j)
          if (visit_level(k)>0_i8) cycle ! dependent already treated by earlier node in this level
          ready = .true.
          do m = 1_i8, this%nodes(k)%nedges() ! non empty since it is a dependent
            if ( visit_level(this%nodes(k)%edges(m)) == 0_i8 &
            .or. visit_level(this%nodes(k)%edges(m)) == level ) then
              ready = .false. ! some dependency not yet ready (0 - not ready, level - not added in previous levels)
              exit
            end if
          end do
          if (ready) then
            count = count + 1_i8
            id(count) = k
            visit_level(k) = level
            added_this_level = added_this_level + 1_i8
          end if
        end do
      end do

      if (added_this_level == 0_i8) exit ! cycle detected
      level_end(level) = count ! all added nodes from the new level
    end do

    if (count /= n) then ! cycle detected
      istat = -1_i8
      return
    end if

    istat = 0_i8
    call move_alloc(id, order%id)
    allocate(order%level_start(level), source=level_start(1_i8:level))
    allocate(order%level_end(level), source=level_end(1_i8:level))
    allocate(order%level_size(level))
    order%level_size = order%level_end - order%level_start + 1_i8
    order%n_levels = level
    if (rev) call order%reverse()
    deallocate(visit_level, level_start, level_end)
  end subroutine dag_levelsort_head

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm starting at roots.
  subroutine dag_levelsort_root(this, order, istat, reverse)
    implicit none
    class(dag), intent(in) :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)

    integer(i8) :: i, j, k, m, n, count, level, added_this_level
    integer(i8), allocatable :: level_start(:), level_end(:), id(:), visit_level(:)
    logical :: ready, rev

    rev = optval(reverse, .false.)
    n = this%n ! in the worst case of a linear DAG, we get as many levels as nodes
    allocate(visit_level(n), source=0_i8)
    allocate(level_start(n))
    allocate(level_end(n))
    allocate(id(n))

    ! first scan for all dependent free nodes
    count = 0_i8
    level = 1_i8
    do i = 1_i8, n
      if (this%nodes(i)%ndependents() > 0_i8) cycle
      count = count + 1_i8
      id(count) = i
      visit_level(i) = level
    end do

    level_start(level) = 1_i8
    level_end(level) = count

    do while (count < n)
      added_this_level = 0_i8
      level = level + 1_i8
      level_start(level) = count + 1_i8

      ! scan all edges of previous level
      do i = level_start(level-1_i8), level_end(level-1_i8)
        if (.not.allocated(this%nodes(id(i))%edges)) cycle
        do j = 1_i8, size(this%nodes(id(i))%edges)
          k = this%nodes(id(i))%edges(j)
          if (visit_level(k)>0_i8) cycle ! dependency already treated by earlier node in this level
          ready = .true.
          do m = 1_i8, this%nodes(k)%ndependents() ! non empty since it is a dependency
            if ( visit_level(this%nodes(k)%dependents(m)) == 0_i8 &
            .or. visit_level(this%nodes(k)%dependents(m)) == level ) then
              ready = .false. ! some dependents not yet ready (0 - not ready, level - not added in previous levels)
              exit
            end if
          end do
          if (ready) then
            count = count + 1_i8
            id(count) = k
            visit_level(k) = level
            added_this_level = added_this_level + 1_i8
          end if
        end do
      end do

      if (added_this_level == 0_i8) exit ! cycle detected
      level_end(level) = count ! all added nodes from the new level
    end do

    if (count /= n) then ! cycle detected
      istat = -1_i8
      return
    end if

    istat = 0_i8
    call move_alloc(id, order%id)
    allocate(order%level_start(level), source=level_start(1_i8:level))
    allocate(order%level_end(level), source=level_end(1_i8:level))
    allocate(order%level_size(level))
    order%level_size = order%level_end - order%level_start + 1_i8
    order%n_levels = level
    if (.not.rev) call order%reverse()
    deallocate(visit_level, level_start, level_end)
  end subroutine dag_levelsort_root

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
      do j = 1_i8, this%nodes(i)%nedges()
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

end module mo_dag
