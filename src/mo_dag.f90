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
  use mo_kind, only : i8
  use mo_utils, only: optval, swap, prefix_sum

  ! Control constants for depth-first traversal behavior.
  ! These values are returned by `visit()` to control DFS branching.
  integer(i8), parameter :: dfs_continue      = 0_i8  !< Continue traversal to children
  integer(i8), parameter :: dfs_skip_children = 1_i8  !< Skip children of this node
  integer(i8), parameter :: dfs_stop_all      = 2_i8  !< Stop traversal entirely
  integer(i8), target :: empty_int_vec(0) = [ integer(i8) :: ] !< Shared zero-length target for empty adjacency lists

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

  !> \class dag_base
  !> \brief Abstract base class shared by all DAG implementations.
  !> \details Provides the read-only traversal API that algorithms can rely on
  !! regardless of the underlying storage.
  type, abstract :: dag_base
    integer(i8) :: n_nodes = 0_i8       !< Number of nodes in the DAG
    integer(i8), allocatable :: tags(:) !< Node tags for external reference
  contains
    procedure(dag_base_count_if), deferred :: n_sources   !< Number of upstream nodes per id
    procedure(dag_base_count_if), deferred :: n_targets   !< Number of downstream nodes per id
    procedure(dag_base_view_if),  deferred :: src_view    !< Pointer view of upstream node ids
    procedure(dag_base_view_if),  deferred :: tgt_view    !< Pointer view of downstream node ids
    procedure :: traverse          => dag_base_traverse
    procedure :: toposort          => dag_base_toposort
    procedure :: levelsort         => dag_base_levelsort
    procedure :: adjacency_matrix  => dag_base_adjacency_matrix
    procedure :: sources           => dag_base_sources
    procedure :: targets           => dag_base_targets
    procedure :: dependencies      => dag_base_dependencies
    procedure :: dependents        => dag_base_dependents
    procedure :: destroy           => dag_base_destroy
  end type dag_base

  !> \brief Abstract interfaces used by \ref dag_base for adjacency access.
  abstract interface
    pure integer(i8) function dag_base_count_if(this, id)
      import :: dag_base, i8
      class(dag_base), intent(in), target :: this
      integer(i8), intent(in) :: id
    end function dag_base_count_if

    !> \brief Provide pointer view to adjacency list for node `id`.
    !! \details Callers must ensure the owning object remains a valid target
    !! for as long as they rely on the returned pointer view.
    subroutine dag_base_view_if(this, id, view)
      import :: dag_base, i8
      class(dag_base), intent(in), target :: this
      integer(i8), intent(in) :: id
      integer(i8), pointer :: view(:)
    end subroutine dag_base_view_if
  end interface

  !> \class node
  !> \brief A node of a directed acyclic graph (DAG)
  type :: node
    !> The upstream nodes that this node depends on (directed sources of the graph).
    !! The indices are the respective node index in the nodes list in the dag
    !! and do not necessarily match the dag%tags (e.g. when nodes are removed, their tags stay untouched)
    integer(i8),dimension(:),allocatable :: sources
    !> nodes that depend on this node
    integer(i8),dimension(:),allocatable :: targets
  contains
    procedure :: n_sources => node_n_sources
    procedure :: n_targets => node_n_targets
    generic, private :: set_sources => set_source_vector, add_source
    procedure, private :: set_source_vector => node_set_source_vector
    procedure, private :: add_source => node_add_source
    procedure, private :: add_target => node_add_target
  end type node

  !> \class dag
  !> \brief A directed acyclic graph (DAG).
  !> \details A collection of nodes that are connected (have a dependency) to other nodes.
  !!
  !! \par Examples
  !! - \ref 01_dag_sort.f90 : \copybrief 01_dag_sort.f90
  type,public, extends(dag_base) :: dag
    !> The nodes in the DAG. The index in this array is used by the sources of the nodes.
    type(node),dimension(:),allocatable :: nodes
    integer(i8), private, dimension(:), allocatable :: tag_to_id_map !< Maps node tag to array index
    integer(i8), private :: max_tag = 0_i8 !< Max tag value in current DAG (for bounds)
  contains
    procedure :: init                       => dag_set_nodes
    procedure :: add_source                 => dag_add_source
    procedure :: set_sources                => dag_set_sources
    procedure :: subgraph                   => dag_subgraph
    procedure :: destroy                    => dag_destroy
    procedure :: tag_to_id                  => dag_tag_to_id
    procedure :: n_sources                  => dag_n_sources
    procedure :: n_targets                  => dag_n_targets
    procedure :: src_view                   => dag_src_view
    procedure :: tgt_view                   => dag_tgt_view
    procedure, private :: rebuild_tag_map   => dag_rebuild_tag_map
    final :: dag_final
  end type dag

  !> \class branching
  !> \brief DAG specialization optimized for river-like branching structures.
  !! \details Stores a single downstream connection per node and compressed
  !! upstream adjacency using CSR-like arrays.
  type, public, extends(dag_base) :: branching
    integer(i8), allocatable :: down(:)            !< Direct downstream neighbor (0 if sink)
    integer(i8), allocatable :: sinks(:)           !< Indices of sink nodes (no downstream neighbor)
    integer(i8), allocatable :: up(:)              !< Concatenated upstream neighbors for all nodes
    integer(i8), allocatable :: n_up(:)            !< Number of upstream neighbors per node
    integer(i8), allocatable :: off_up(:)          !< Offsets into up(:) for each node
  contains
    procedure :: init            => branching_init
    procedure :: destroy         => branching_destroy
    procedure :: n_sources       => branching_n_sources
    procedure :: n_targets       => branching_n_targets
    procedure :: src_view        => branching_src_view
    procedure :: tgt_view        => branching_tgt_view
    procedure :: levelsort       => branching_levelsort
    final     :: branching_final
  end type branching

contains

  !> \brief Reverse order.
  subroutine order_reverse(this)
    use mo_utils, only: flip
    class(order_t), intent(inout) :: this
    ! integer(i8), allocatable :: tmp(:)
    integer(i8) :: n, tmp1, tmp2, i

    n = size(this%id, kind=i8)

    !$omp parallel do default(shared) private(tmp1) schedule(static)
    do i=1_i8, n/2_i8
      tmp1 = this%id(i)
      this%id(i) = this%id(n - i + 1_i8)
      this%id(n - i + 1_i8) = tmp1
    end do
    !$omp end parallel do

    !$omp parallel do default(shared) private(tmp1,tmp2) schedule(static)
    do i=1_i8, this%n_levels/2_i8
      tmp1 = this%level_size(i)
      this%level_size(i) = this%level_size(this%n_levels - i + 1_i8)
      this%level_size(this%n_levels - i + 1_i8) = tmp1
      ! flipping level start/end, swapping them and subtract it from n+1
      tmp1 = this%level_start(i)
      tmp2 = this%level_end(i)
      this%level_start(i) = n + 1_i8 - this%level_end(this%n_levels - i + 1_i8)
      this%level_end(i) = n + 1_i8 - this%level_start(this%n_levels - i + 1_i8)
      this%level_start(this%n_levels - i + 1_i8) = n + 1_i8 - tmp2
      this%level_end(this%n_levels - i + 1_i8) = n + 1_i8 - tmp1
    end do
    !$omp end parallel do

    ! handle middle level in case of odd number of levels
    if (mod(this%n_levels, 2_i8) == 1_i8) then
      i = (this%n_levels + 1_i8) / 2_i8
      tmp1 = this%level_start(i)
      tmp2 = this%level_end(i)
      this%level_start(i) = n + 1_i8 - tmp2
      this%level_end(i) = n + 1_i8 - tmp1
    end if
  end subroutine

  !> \brief Simple visit function to visit all dependencies.
  function visit_simple(this, id, tag) result(action)
    class(traversal_visit), intent(inout) :: this
    integer(i8), intent(in) :: id !< The index of the node being visited.
    integer(i8), intent(in) :: tag !< The tag of the node being visited.
    integer(i8) :: action
    action = dfs_continue
  end function

  !> \brief Traverse graph from given starting node.
  subroutine dag_base_traverse(this, handler, ids, down)
    class(dag_base), intent(in), target :: this
    class(traversal_handler), intent(inout) :: handler !< traversal handler to use
    integer(i8), dimension(:), intent(in), optional :: ids !< ids to traverse from (by default all)
    logical, intent(in), optional :: down !< traverse downstream if .true. (.false. by default to traverse upstream)

    integer(i8) :: top, dep, action, i, j
    integer(i8), allocatable :: stack(:)
    integer(i8), pointer :: neigh(:)
    logical :: down_

    if (this%n_nodes == 0_i8) return
    down_ = optval(down, .false.)
    allocate(stack(this%n_nodes))
    if (present(ids)) then
      top = size(ids, kind=i8)
      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, top
        stack(i) = ids(i)
      end do
      !$omp end parallel do
    else
      top = this%n_nodes
      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, top
        stack(i) = i
      end do
      !$omp end parallel do
    end if

    do while (top > 0_i8)
      j = stack(top)
      top = top - 1_i8

      if (handler%visited(j)) cycle
      handler%visited(j) = .true.

      action = handler%visit(j, this%tags(j))
      select case (action)
        case (dfs_continue)  ! 0
          if (down_) then
            call this%tgt_view(j, neigh)
          else
            call this%src_view(j, neigh)
          end if
          do i = size(neigh, kind=i8), 1_i8, -1_i8
            dep = neigh(i)
            if (handler%visited(dep)) cycle
            top = top + 1_i8
            stack(top) = dep
          end do
        case (dfs_skip_children) ! 1
          cycle
        case default ! dfs_stop_all (all other)
          exit
      end select
    end do

    deallocate(stack)
  end subroutine dag_base_traverse

  !> \brief Generate list of all dependencies and their sub-dependencies for a given node.
  subroutine dag_base_dependencies(this, id, deps)
    class(dag_base), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), allocatable, intent(out) :: deps(:)
    type(traversal_visit) :: handler
    integer(i8), pointer :: src(:)
    integer(i8) :: i

    if (this%n_sources(id) == 0_i8) then
      allocate(deps(0))
      return
    end if

    allocate(handler%visited(this%n_nodes), source=.false.)
    call this%src_view(id, src)
    call this%traverse(handler, src)
    allocate(deps(count(handler%visited)))
    deps = pack([(i, i=1_i8, this%n_nodes)], handler%visited)
    deallocate(handler%visited)
  end subroutine dag_base_dependencies

  !> \brief Generate list of all targets and their sub-targets for a given node.
  subroutine dag_base_dependents(this, id, deps)
    class(dag_base), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), allocatable, intent(out) :: deps(:)
    type(traversal_visit) :: handler
    integer(i8), pointer :: tgt(:)
    integer(i8) :: i

    if (this%n_targets(id) == 0_i8) then
      allocate(deps(0))
      return
    end if

    allocate(handler%visited(this%n_nodes), source=.false.)
    call this%tgt_view(id, tgt)
    call this%traverse(handler, tgt, down=.true.)
    allocate(deps(count(handler%visited)))
    deps = pack([(i, i=1_i8, this%n_nodes)], handler%visited)
    deallocate(handler%visited)
  end subroutine dag_base_dependents

  !> \brief Copy direct sources of node `id` into an allocatable array.
  subroutine dag_base_sources(this, id, vals)
    class(dag_base), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), allocatable, intent(out) :: vals(:)
    integer(i8), pointer :: view(:)
    integer(i8) :: n

    n = this%n_sources(id)
    allocate(vals(n))
    if (n == 0_i8) return
    call this%src_view(id, view)
    vals = view
  end subroutine dag_base_sources

  !> \brief Copy direct targets of node `id` into an allocatable array.
  subroutine dag_base_targets(this, id, vals)
    class(dag_base), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), allocatable, intent(out) :: vals(:)
    integer(i8), pointer :: view(:)
    integer(i8) :: n

    n = this%n_targets(id)
    allocate(vals(n))
    if (n == 0_i8) return
    call this%tgt_view(id, view)
    vals = view
  end subroutine dag_base_targets

  !> \brief Default destroy implementation shared by all DAGs.
  subroutine dag_base_destroy(this)
    class(dag_base), intent(inout) :: this
    this%n_nodes = 0_i8
    if (allocated(this%tags)) deallocate(this%tags)
  end subroutine dag_base_destroy

  !> \brief Generate the adjacency matrix for the DAG.
  !> \details This is an \(n \times n \) logical matrix with elements \(A_{ij}\)
  !! such that \(A_{ij}\) is true when node \(i\) depends on node \(j\).
  subroutine dag_base_adjacency_matrix(this, mat)
    class(dag_base), intent(in), target :: this
    logical, dimension(:,:), allocatable, intent(out) :: mat !< adjacency matrix
    integer(i8) :: i, j, ndeps
    integer(i8), pointer :: deps(:)

    allocate(mat(this%n_nodes, this%n_nodes), source=.false.)
    do i = 1_i8, this%n_nodes
      ndeps = this%n_sources(i)
      if (ndeps == 0_i8) cycle
      call this%src_view(i, deps)
      do j = 1_i8, ndeps
        mat(i, deps(j)) = .true.
      end do
    end do
  end subroutine dag_base_adjacency_matrix

  !> \brief Main toposort routine
  subroutine dag_base_toposort(this, order, istat)
    class(dag_base),intent(in), target :: this
    integer(i8),dimension(:),allocatable,intent(out) :: order !< the toposort order
    !> Status flag: 0 (if no errors), -1 (if circular dependency, in this case, `order` will not be allocated)
    integer(i8),intent(out) :: istat
    integer(i8) :: i,iorder
    logical, allocatable, dimension(:) :: checking, visited
    integer(i8), pointer :: deps(:)

    allocate(checking(this%n_nodes), source=.false.)
    allocate(visited(this%n_nodes), source=.false.)

    allocate(order(this%n_nodes))
    iorder = 0_i8  ! index in order array
    istat = 0_i8   ! no errors so far
    do i=1_i8,this%n_nodes
      if (.not. visited(i)) call dfs(i)
      if (istat==-1_i8) exit
    end do

    if (istat==-1_i8) deallocate(order)
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
      call this%src_view(j, deps)
      do k=1_i8, size(deps, kind=i8)
        call dfs(deps(k))
        if (istat==-1_i8) return
      end do
      checking(j) = .false.
      visited(j) = .true.
      iorder = iorder + 1_i8
      order(iorder) = j
    end subroutine dfs

  end subroutine dag_base_toposort

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm
  subroutine dag_base_levelsort(this, order, istat, root, reverse)
    implicit none
    class(dag_base), intent(in), target :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: root     !< levels as distance from graph roots (default: .false.)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)
    logical :: root_

    root_ = optval(root, .false.)
    if (root_) then
      call dag_base_levelsort_root(this, order, istat, reverse)
    else
      call dag_base_levelsort_head(this, order, istat, reverse)
    end if
  end subroutine dag_base_levelsort

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm
  subroutine dag_base_levelsort_head(this, order, istat, reverse)
    implicit none
    class(dag_base), intent(in), target :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)

    integer(i8) :: i, j, k, m, n, count, level, added_this_level
    integer(i8), allocatable :: level_start(:), level_end(:), id(:), visit_level(:)
    logical :: rev
    integer(i8), pointer :: neigh(:), deps(:)

    rev = optval(reverse, .false.)
    n = this%n_nodes ! in the worst case of a linear DAG, we get as many levels as nodes
    allocate(visit_level(n), source=0_i8)
    allocate(level_start(n))
    allocate(level_end(n))
    allocate(id(n))

    ! first scan for all dependency free nodes
    count = 0_i8
    level = 1_i8
    do i = 1_i8, n
      if (this%n_sources(i) > 0_i8) cycle
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

      ! scan all targets of previous level
      do i = level_start(level-1_i8), level_end(level-1_i8)
        call this%tgt_view(id(i), neigh)
        tgt_loop: do j = 1_i8, size(neigh, kind=i8)
          k = neigh(j)
          if (visit_level(k)>0_i8) cycle ! dependent already treated by earlier node in this level
          call this%src_view(k, deps)
          do m = 1_i8, size(deps, kind=i8) ! non empty since it is a dependent
            ! some dependency not yet ready (0 - not ready, level - not added in previous levels)
            if ( visit_level(deps(m)) == 0_i8 .or. visit_level(deps(m)) == level ) cycle tgt_loop
          end do
          count = count + 1_i8
          id(count) = k
          visit_level(k) = level
          added_this_level = added_this_level + 1_i8
        end do tgt_loop
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
  end subroutine dag_base_levelsort_head

  !> \brief Sorting DAG by levels for parallelization based on Kahn's algorithm starting at roots.
  subroutine dag_base_levelsort_root(this, order, istat, reverse)
    implicit none
    class(dag_base), intent(in), target :: this
    type(order_t), intent(out) :: order       !< level based order
    integer(i8), intent(out) :: istat         !< status code (0 - no error, -1 - cycle found)
    logical, intent(in), optional :: reverse  !< reverse order (default: .false.)

    integer(i8) :: i, j, k, m, n, count, level, added_this_level
    integer(i8), allocatable :: level_start(:), level_end(:), id(:), visit_level(:)
    logical :: rev
    integer(i8), pointer :: neigh(:), deps(:)

    rev = optval(reverse, .false.)
    n = this%n_nodes ! in the worst case of a linear DAG, we get as many levels as nodes
    allocate(visit_level(n), source=0_i8)
    allocate(level_start(n))
    allocate(level_end(n))
    allocate(id(n))

    ! first scan for all dependent free nodes
    count = 0_i8
    level = 1_i8
    do i = 1_i8, n
      if (this%n_targets(i) > 0_i8) cycle
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

      ! scan all sources of previous level
      do i = level_start(level-1_i8), level_end(level-1_i8)
        call this%src_view(id(i), neigh)
        src_loop: do j = 1_i8, size(neigh, kind=i8)
          k = neigh(j)
          if (visit_level(k)>0_i8) cycle ! dependency already treated by earlier node in this level
          call this%tgt_view(k, deps)
          do m = 1_i8, size(deps, kind=i8) ! non empty since it is a dependency
            ! some targets not yet ready (0 - not ready, level - not added in previous levels)
            if ( visit_level(deps(m)) == 0_i8 .or. visit_level(deps(m)) == level ) cycle src_loop
          end do
          count = count + 1_i8
          id(count) = k
          visit_level(k) = level
          added_this_level = added_this_level + 1_i8
        end do src_loop
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
  end subroutine dag_base_levelsort_root

  !> \brief number of sources for this node
  pure integer(i8) function node_n_sources(this)
    class(node),intent(in) :: this
    if (allocated(this%sources)) then
      node_n_sources = size(this%sources, kind=i8)
    else
      node_n_sources = 0_i8
    end if
  end function node_n_sources

  !> \brief number of targets for this node
  pure integer(i8) function node_n_targets(this)
    class(node),intent(in) :: this
    if (allocated(this%targets)) then
      node_n_targets = size(this%targets, kind=i8)
    else
      node_n_targets = 0_i8
    end if
  end function node_n_targets

  !> \brief Specify the source indices for this node
  subroutine node_set_source_vector(this,sources)
    class(node),intent(inout) :: this
    integer(i8),dimension(:),intent(in) :: sources
    this%sources = sources
    call sort_ascending(this%sources)
  end subroutine node_set_source_vector

  !> \brief Add a source index for this node
  subroutine node_add_source(this,e)
    class(node),intent(inout) :: this
    integer(i8),intent(in) :: e
    if (allocated(this%sources)) then
      if (.not. any(e==this%sources)) then ! don't add if already there
        this%sources = [this%sources, e]
        call sort_ascending(this%sources)
      end if
    else
      this%sources = [e]
    end if
  end subroutine node_add_source

  !> \brief Add a target index for this node
  subroutine node_add_target(this,d)
    class(node),intent(inout) :: this
    integer(i8),intent(in) :: d
    if (allocated(this%targets)) then
      if (.not. any(d==this%targets)) then ! don't add if already there
        this%targets = [this%targets, d]
        call sort_ascending(this%targets)
      end if
    else
      this%targets = [d]
    end if
  end subroutine node_add_target

  !> \brief Number of dependencies for node `id` in the dense DAG implementation.
  pure integer(i8) function dag_n_sources(this, id)
    class(dag), intent(in), target :: this
    integer(i8), intent(in) :: id
    dag_n_sources = this%nodes(id)%n_sources()
  end function dag_n_sources

  !> \brief Number of targets for node `id` in the dense DAG implementation.
  pure integer(i8) function dag_n_targets(this, id)
    class(dag), intent(in), target :: this
    integer(i8), intent(in) :: id
    dag_n_targets = this%nodes(id)%n_targets()
  end function dag_n_targets

  !> \brief Pointer view to the source list of node `id`.
  subroutine dag_src_view(this, id, view)
    class(dag), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), pointer :: view(:)
    if (allocated(this%nodes(id)%sources)) then
      view => this%nodes(id)%sources
    else
      view => empty_int_vec
    end if
  end subroutine dag_src_view

  !> \brief Pointer view to the target list of node `id`.
  subroutine dag_tgt_view(this, id, view)
    class(dag), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), pointer :: view(:)
    if (allocated(this%nodes(id)%targets)) then
      view => this%nodes(id)%targets
    else
      view => empty_int_vec
    end if
  end subroutine dag_tgt_view

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
    allocate(handler%visited(this%n_nodes), source=.false.)
    call this%traverse(handler, ids, down)

    nsub = count(handler%visited)
    subtags = pack(this%tags, handler%visited)
    idmap = unpack([(i, i=1_i8, nsub)], handler%visited, 0_i8)

    call subgraph%init(nsub, subtags)
    do i = 1_i8, this%n_nodes
      if (.not.handler%visited(i)) cycle
      if (down_) then
        do j = 1_i8, this%nodes(i)%n_targets()
          call subgraph%add_source(idmap(this%nodes(i)%targets(j)), idmap(i))
        end do
      else
        call subgraph%set_sources(idmap(i), [(idmap(this%nodes(i)%sources(j)), j=1_i8,this%nodes(i)%n_sources())])
      end if
    end do

    deallocate(handler%visited, idmap, subtags)
  end function dag_subgraph

  !> \brief Rebuild the map from node tags to array indices.
  subroutine dag_rebuild_tag_map(this)
    class(dag), intent(inout) :: this
    integer(i8) :: i
    if (allocated(this%tag_to_id_map)) deallocate(this%tag_to_id_map)
    if (.not.allocated(this%nodes)) return
    this%max_tag = maxval(this%tags)
    allocate(this%tag_to_id_map(this%max_tag), source=-1_i8) ! Initialize with invalid index
    do i = 1_i8, this%n_nodes
      this%tag_to_id_map(this%tags(i)) = i
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
    else if (tag < 1_i8 .or. tag > size(this%tag_to_id_map, kind=i8)) then
      id = -1_i8
    else
      id = this%tag_to_id_map(tag)
    end if
  end function dag_tag_to_id

  !> \brief Destroy the `dag`.
  subroutine dag_destroy(this)
    class(dag),intent(inout) :: this
    if (allocated(this%nodes)) deallocate(this%nodes)
    if (allocated(this%tag_to_id_map)) deallocate(this%tag_to_id_map)
    this%max_tag = 0_i8
    call dag_base_destroy(this)
  end subroutine dag_destroy

  !> \brief Fortran FINAL procedure to ensure cleanup when `dag` goes out of scope.
  subroutine dag_final(this)
    type(dag) :: this
    call this%destroy()
  end subroutine dag_final

  !> \brief Set the number of nodes in the dag.
  subroutine dag_set_nodes(this, n, tags)
    use mo_message, only: error_message
    class(dag), intent(inout) :: this
    integer(i8), intent(in)   :: n !< number of nodes
    integer(i8), dimension(n), intent(in), optional :: tags !< tags of the nodes (will be their index by default)
    integer(i8) :: i !! counter
    if (n<1_i8) call error_message('error: n must be >= 1')
    if (allocated(this%nodes)) deallocate(this%nodes)
    this%n_nodes = n
    allocate(this%nodes(n))
    if (present(tags)) then
      this%tags = tags
      call this%rebuild_tag_map()
    else
      this%max_tag = n
      if (allocated(this%tag_to_id_map)) deallocate(this%tag_to_id_map)
      allocate(this%tag_to_id_map(n))
      if (allocated(this%tags)) deallocate(this%tags)
      allocate(this%tags(n))
      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, n
        ! default node tags from array IDs
        this%tags(i) = i
        this%tag_to_id_map(i) = i
      end do
      !$omp end parallel do
    end if
  end subroutine dag_set_nodes

  !> \brief Add a source dependency to a dag.
  subroutine dag_add_source(this,id,target_id)
    class(dag),intent(inout) :: this
    integer(i8),intent(in)   :: id !< node id
    integer(i8),intent(in)   :: target_id !< the node to connect to `id`
    call this%nodes(id)%set_sources(target_id)
    call this%nodes(target_id)%add_target(id)
  end subroutine dag_add_source

  !> \brief Set the sources for a node in a dag
  subroutine dag_set_sources(this,id,sources)
    class(dag),intent(inout)            :: this
    integer(i8),intent(in)              :: id !< node id
    integer(i8),dimension(:),intent(in) :: sources
    integer(i8) :: i
    call this%nodes(id)%set_sources(sources)
    do i = 1_i8, this%nodes(id)%n_sources()
      call this%nodes(this%nodes(id)%sources(i))%add_target(id)
    end do
  end subroutine dag_set_sources

  !> \brief Initialize branching DAG from downstream linkage.
  subroutine branching_init(this, down, tags)
    use mo_message, only: error_message
    class(branching), intent(inout) :: this
    integer(i8), intent(in) :: down(:) !< downstream linkage (0 for sinks)
    integer(i8), intent(in), optional :: tags(:) !< node tags (by default their index)
    integer(i8) :: i, total_up, sink, idx, sink_cursor, sink_idx
    integer(i8), allocatable :: cursor(:), n_up(:), sinks(:), off_up(:), up(:)
    integer(i8) :: n, n_sinks

    call this%destroy()

    n = size(down, kind=i8)
    this%n_nodes = n

    allocate(n_up(n))
    !$omp parallel do default(none) shared(n_up,n) schedule(static)
    do i = 1_i8, n
      n_up(i) = 0_i8
    end do
    !$omp end parallel do

    n_sinks = 0_i8
    !$omp parallel do default(none) shared(down,n) reduction(+:n_sinks) schedule(static)
    do i = 1_i8, n
      if (down(i) == 0_i8) n_sinks = n_sinks + 1_i8
    end do
    !$omp end parallel do

    !$omp parallel do default(none) shared(n_up,down,n) private(sink) schedule(static)
    do i = 1_i8, n
      sink = down(i)
      if (sink == 0_i8) cycle
      !$omp atomic update
      n_up(sink) = n_up(sink) + 1_i8
    end do
    !$omp end parallel do

    allocate(sinks(n_sinks))
    sink_cursor = 0_i8
    !$omp parallel do default(none) shared(sink_cursor,sinks,down,n) private(sink_idx) schedule(static)
    do i = 1_i8, n
      if (down(i) /= 0_i8) cycle
      !$omp atomic capture
      sink_idx = sink_cursor
      sink_cursor = sink_cursor + 1_i8
      !$omp end atomic
      sinks(sink_idx + 1_i8) = i
    end do
    !$omp end parallel do

    allocate(off_up(n))
    call prefix_sum(n_up, off_up, shift=1_i8, start=1_i8)

    total_up = off_up(n) + n_up(n) - 1_i8

    allocate(up(total_up)) ! size: nodes - sinks
    if (total_up > 0_i8) then
      allocate(cursor(n))
      !$omp parallel do default(none) shared(cursor,n) schedule(static)
      do i = 1_i8, n
        cursor(i) = 0_i8
      end do
      !$omp end parallel do
      !$omp parallel do default(none) shared(down,cursor,off_up,up,n) private(sink, idx) schedule(static)
      do i = 1_i8, n
        sink = down(i)
        if (sink == 0_i8) cycle
        !$omp atomic capture
        idx = cursor(sink)
        cursor(sink) = cursor(sink) + 1_i8
        !$omp end atomic
        idx = off_up(sink) + idx
        up(idx) = i
      end do
      !$omp end parallel do
      deallocate(cursor)
    end if

    ! sort sinks and upstream lists for consistency
    if (n_sinks > 1_i8) call sort_ascending(sinks)
    !$omp parallel do default(none) shared(n_up,up,off_up,n) schedule(static)
    do i = 1_i8, n
      if (n_up(i) > 1_i8) call sort_ascending(up(off_up(i):off_up(i)+n_up(i)-1_i8))
    end do
    !$omp end parallel do

    ! it is easier for the compiler to use local variables in omp regions
    call move_alloc(n_up, this%n_up)
    call move_alloc(sinks, this%sinks)
    call move_alloc(off_up, this%off_up)
    call move_alloc(up, this%up)

    allocate(this%down(n))
    allocate(this%tags(n))
    if (present(tags)) then
      if (size(tags, kind=i8) /= n) call error_message('branching_init: tags size mismatch')
      !$omp parallel do default(none) shared(this,tags,down,n) schedule(static)
      do i = 1_i8, n
        this%tags(i) = tags(i)
        this%down(i) = down(i)
      end do
      !$omp end parallel do
    else
      !$omp parallel do default(none) shared(this,down,n) schedule(static)
      do i = 1_i8, n
        this%tags(i) = i
        this%down(i) = down(i)
      end do
      !$omp end parallel do
    end if
  end subroutine branching_init

  !> \brief Destroy branching DAG resources.
  subroutine branching_destroy(this)
    class(branching), intent(inout) :: this
    if (allocated(this%down)) deallocate(this%down)
    if (allocated(this%up)) deallocate(this%up)
    if (allocated(this%n_up)) deallocate(this%n_up)
    if (allocated(this%off_up)) deallocate(this%off_up)
    if (allocated(this%sinks)) deallocate(this%sinks)
    call dag_base_destroy(this)
  end subroutine branching_destroy

  !> \brief Number of upstream neighbors for node `id`.
  pure integer(i8) function branching_n_sources(this, id)
    class(branching), intent(in), target :: this
    integer(i8), intent(in) :: id
    branching_n_sources = this%n_up(id)
  end function branching_n_sources

  !> \brief Number of downstream neighbors for node `id` (0 or 1).
  pure integer(i8) function branching_n_targets(this, id)
    class(branching), intent(in), target :: this
    integer(i8), intent(in) :: id
    if (this%down(id) > 0_i8) then
      branching_n_targets = 1_i8
    else
      branching_n_targets = 0_i8
    end if
  end function branching_n_targets

  !> \brief Pointer view to upstream neighbors for node `id`.
  subroutine branching_src_view(this, id, view)
    class(branching), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), pointer :: view(:)
    if (this%n_up(id) > 0_i8) then
      view => this%up(this%off_up(id):this%off_up(id)+this%n_up(id)-1_i8)
    else
      view => empty_int_vec
    end if
  end subroutine branching_src_view

  !> \brief Pointer view to downstream neighbor for node `id`.
  subroutine branching_tgt_view(this, id, view)
    class(branching), intent(in), target :: this
    integer(i8), intent(in) :: id
    integer(i8), pointer :: view(:)
    if (this%down(id) > 0_i8) then
      view => this%down(id:id)
    else
      view => empty_int_vec
    end if
  end subroutine branching_tgt_view

  !> \brief Specialized levelsort for branching DAGs with optional root ordering.
  subroutine branching_levelsort(this, order, istat, root, reverse)
    use mo_utils, only : optval
    class(branching), intent(in), target :: this
    type(order_t), intent(out) :: order
    integer(i8), intent(out) :: istat
    logical, intent(in), optional :: root, reverse
    logical :: root_

    root_ = optval(root, .false.)
    if (root_) then
      call branching_levelsort_root(this, order, istat, reverse)
    else
      call dag_base_levelsort(this, order, istat, root, reverse)
    end if
  end subroutine branching_levelsort

  !> \brief Parallel root-based level ordering for branching DAGs.
  subroutine branching_levelsort_root(this, order, istat, reverse)
    use mo_utils, only : optval
    class(branching), intent(in) :: this
    type(order_t), intent(out) :: order
    integer(i8), intent(out) :: istat
    logical, intent(in), optional :: reverse

    integer(i8), allocatable :: current(:), next(:)
    integer(i8), allocatable :: level_start_tmp(:), level_end_tmp(:)
    integer(i8), allocatable :: degree(:), offsets(:)
    integer(i8), allocatable :: order_ids(:)
    integer(i8) :: n, count, level_idx, size_curr
    integer(i8) :: total_next, i

    n = this%n_nodes
    istat = 0_i8
    if (n == 0_i8) then
      order%n_levels = 0_i8
      allocate(order%id(0))
      allocate(order%level_start(0))
      allocate(order%level_end(0))
      allocate(order%level_size(0))
      return
    end if

    if (.not. allocated(this%sinks)) then
      istat = -1_i8
      return
    end if

    if (size(this%sinks, kind=i8) == 0_i8) then
      istat = -1_i8
      return
    end if

    allocate(current(size(this%sinks, kind=i8)))
    !$omp parallel do default(shared) schedule(static)
    do i = 1_i8, size(this%sinks, kind=i8)
      current(i) = this%sinks(i)
    end do
    !$omp end parallel do

    allocate(order_ids(n))
    allocate(level_start_tmp(n))
    allocate(level_end_tmp(n))

    count = 0_i8
    level_idx = 0_i8

    level_loop: do
      size_curr = size(current, kind=i8)
      if (size_curr == 0_i8) exit

      level_idx = level_idx + 1_i8
      level_start_tmp(level_idx) = count + 1_i8

      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, size_curr
        order_ids(count + i) = current(i)
      end do
      !$omp end parallel do

      count = count + size_curr
      level_end_tmp(level_idx) = count

      allocate(degree(size_curr))
      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, size_curr
        degree(i) = this%n_up(current(i))
      end do
      !$omp end parallel do

      allocate(offsets(size_curr))
      call prefix_sum(degree, offsets, shift=1_i8, start=1_i8)
      total_next = offsets(size_curr) + degree(size_curr) - 1_i8

      if (total_next == 0_i8) then
        deallocate(degree, offsets)
        exit
      end if

      allocate(next(total_next))
      !$omp parallel do default(shared) schedule(static)
      do i = 1_i8, size_curr
        if (degree(i) == 0_i8) cycle
        next(offsets(i):offsets(i)+degree(i)-1_i8) = this%up(this%off_up(current(i)):this%off_up(current(i))+degree(i)-1_i8)
      end do
      !$omp end parallel do

      deallocate(degree, offsets, current)
      call move_alloc(next, current)
    end do level_loop

    if (allocated(current)) deallocate(current)

    if (count /= n) then
      istat = -1_i8
      deallocate(order_ids, level_start_tmp, level_end_tmp)
      return
    end if

    call move_alloc(order_ids, order%id)
    order%n_levels = level_idx
    allocate(order%level_start(level_idx))
    allocate(order%level_end(level_idx))
    allocate(order%level_size(level_idx))
    !$omp parallel do default(shared) schedule(static)
    do i = 1_i8, level_idx
      order%level_start(i) = level_start_tmp(i)
      order%level_end(i) = level_end_tmp(i)
      order%level_size(i) = level_end_tmp(i) - level_start_tmp(i) + 1_i8
    end do
    !$omp end parallel do
    deallocate(level_start_tmp, level_end_tmp)

    if (.not. optval(reverse, .false.)) call order%reverse()

  end subroutine branching_levelsort_root

  !> \brief Ensure branching DAG resources are freed when going out of scope.
  subroutine branching_final(this)
    type(branching) :: this
    call this%destroy()
  end subroutine branching_final

  !> \brief Sorts an array `ivec` in increasing order.
  !> \details Uses a basic recursive quicksort (with insertion sort for partitions with <= 20 elements).
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
