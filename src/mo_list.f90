!> \file    mo_list.F90
!> \copydoc mo_list

!> \brief   Module providing a linked list.
!> \version 0.1
!> \authors Jacob Williams, Sebastian Mueller
!> \date    Apr 2025
!> \copyright flist was originally released under the BSD 3-Clause license.
!> \details A simple logging framework derived from flist (https://github.com/jacobwilliams/flist).
!!
!! A generic list implementation.
!!
!! It uses an unlimited polymorphic `class(*)` pointer variable to allow it
!! to contain any type of data. The `key` can be an integer(i4), string, or
!! any user-defined \ref key_class.
!!
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_list
  ! Copyright (c) 2015-2021, Jacob Williams
  ! All rights reserved.
  !
  ! Redistribution and use in source and binary forms, with or without modification,
  ! are permitted provided that the following conditions are met:
  !
  ! 1. Redistributions of source code must retain the above copyright notice, this
  ! list of conditions and the following disclaimer.
  !
  ! 2. Redistributions in binary form must reproduce the above copyright notice,
  ! this list of conditions and the following disclaimer in the documentation and/or
  ! other materials provided with the distribution.
  !
  ! 3. Neither the name of the copyright holder nor the names of its contributors
  ! may be used to endorse or promote products derived from this software without
  ! specific prior written permission.
  !
  ! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  ! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  ! DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ! ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  ! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  ! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ! ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  ! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  ! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  use mo_kind, only: i4
  use mo_message, only: error_message
  implicit none
  private

  !> \class key_class
  !> \brief Abstract class for list keys.
  !> \details Can be used as a key for the list.
  !! it can be extended to use any data as a key.
  !! all that is necessary is to define the == operator function.
  !! For convienence, integer(i4) or characters keys are also
  !! allowed to be used.
  type, abstract, public :: key_class
  contains
    private
    procedure(key_equal_func), deferred :: key_equal
    generic, public :: operator(==) => key_equal
  end type key_class

  !> \brief interface for equality operator for \ref key_class.
  abstract interface
    pure elemental logical function key_equal_func(item1, item2)
      import :: key_class
      implicit none
      class(key_class), intent(in) :: item1
      class(key_class), intent(in) :: item2
    end function key_equal_func
  end interface

  !> \class node
  !> \brief a node in the linked list.
  !> \details This is the container to the unlimited polymorphic `value` variable.
  type, public :: node
    private
    class(*), allocatable :: key  !< the key (can be integer(i4), string, or \ref key_class)
    class(*), pointer :: value => null()  !< the data to hold
    !> If true, value pointer is deallocated when it is removed from the list, or the list is destroyed.
    !! If false, it is only nullified.
    logical :: destroy_on_delete = .true.
    type(node), pointer :: next => null()  !< the next one in the list
    type(node), pointer :: previous => null()  !< the previous one in the list
  contains
    procedure, public :: destroy => destroy_node_data  ! deallocate value
    procedure, public :: get_data => get_node_data      ! get data from a node
  end type node

  !> \class list
  !> \brief Linked list of pointers to polymorphic types.
  type, public :: list
    private
    logical :: case_sensitive = .true. !< character key lookup is case sensitive
    integer(i4) :: count = 0      !< number of items in the list
    type(node), pointer :: head => null() !< the first item in the list
    type(node), pointer :: tail => null() !< the last item in the list
  contains
    private
    procedure, public :: add_pointer             ! add a pointer item to the list
    procedure, public :: add_clone               ! add a non-pointer item to the list
    procedure, public :: get => get_data         ! get a pointer to an item in the list
    procedure, public :: destroy => destroy_list ! destroy the list and deallocate/finalize all the data
    procedure, public :: has_key                 ! if the key is present in the list
    procedure, public :: traverse                ! traverse the list are return each key & value
    procedure, public :: remove => remove_by_key ! remove item from the list, given the key
    ! procedures that operate on nodes:
    procedure, public :: remove_by_pointer ! remove node from list, given pointer to it
    procedure, public :: get_node          ! get a pointer to a node in the list
    procedure, public :: traverse_list     ! traverse each node of the list
    !private routines:
    procedure :: keys_equal     ! for testing key string equality
    final :: list_finalizer
  end type list

  interface list
    procedure :: initialize_list !< constructor for a list
  end interface

  abstract interface
    !> \brief internal function for traversing all nodes in a list
    subroutine iterator_func(me, done)
      import :: node
      implicit none
      type(node), pointer  :: me
      logical, intent(out) :: done !< set to true to stop traversing
    end subroutine iterator_func

    !> \brief for traversing all keys in a list
    subroutine key_iterator(key, value, done)
      implicit none
      class(*), intent(in)  :: key   !< the node key
      class(*), pointer     :: value !< pointer to the node value
      logical, intent(out)  :: done  !< set to true to stop traversing
    end subroutine key_iterator
  end interface

contains

  !> \brief Returns true if the key is present in the list.
  logical function has_key(me, key)
    implicit none
    class(list), intent(inout) :: me
    class(*), intent(in)       :: key
    has_key = .false.
    ! traverse the list:
    call me%traverse_list(key_search)
  contains
    !> \brief search for the key
    subroutine key_search(p, done)
      implicit none
      type(node), pointer  :: p
      logical, intent(out) :: done !< whether key is found
      has_key = me%keys_equal(p%key, key)
      done = has_key
    end subroutine key_search
  end function has_key

  !> \brief list constructor.
  function initialize_list(case_sensitive) result(lst)
    implicit none
    logical, intent(in) :: case_sensitive !< if true, then string key searches are case sensitive.
    type(list) :: lst
    lst%case_sensitive = case_sensitive
  end function initialize_list

  !> \brief traverse list from head to tail, and call the iterator function for each node.
  subroutine traverse_list(me, iterator)
    implicit none
    class(list), intent(inout) :: me
    procedure(iterator_func)  :: iterator  !< the function to call for each node.
    type(node), pointer :: p
    logical :: done

    done = .false.
    p => me%head
    do
      if (associated(p)) then
        call iterator(p, done)
        if (done) exit
        p => p%next
      else
        exit ! done
      end if
    end do
  end subroutine traverse_list

  !> \brief traverse list from head to tail, and call the iterator function for each key.
  subroutine traverse(me, iterator)
    implicit none
    class(list), intent(inout) :: me
    procedure(key_iterator)  :: iterator  !< the function to call for each node.
    call me%traverse_list(key_iterator_wrapper)
  contains
    !> \brief for calling the user-specified key_iterator function.
    subroutine key_iterator_wrapper(my, done)
      implicit none
      type(node), pointer  :: my
      logical, intent(out) :: done !< set to true to stop traversing
      call iterator(my%key, my%value, done)
    end subroutine key_iterator_wrapper
  end subroutine traverse

  !> \brief destroy the data in the node.
  impure elemental subroutine destroy_node_data(me)
    implicit none
    class(node), intent(inout) :: me
    if (allocated(me%key)) deallocate (me%key)
    if (me%destroy_on_delete) then
      ! deallocates the pointer (and call any finalizer)
      ! (otherwise, it is up to the caller to do this)
      if (associated(me%value)) deallocate (me%value)
    end if
    nullify (me%value)
  end subroutine destroy_node_data

  !> just a wrapper for \ref destroy_list.
  impure elemental subroutine list_finalizer(me)
    implicit none
    type(list), intent(inout) :: me
    call me%destroy()
  end subroutine list_finalizer

  !> \brief destroy the list (traverses from head to tail)
  impure elemental subroutine destroy_list(me)
    implicit none
    class(list), intent(inout) :: me
    me%count = 0
    if (associated(me%head)) call destroy_node(me%head)
    nullify (me%head)
    nullify (me%tail)
  end subroutine destroy_list

  !> \brief destroy the node (and subsequent ones in the list).
  impure recursive subroutine destroy_node(me)
    implicit none
    type(node), pointer :: me
    if (associated(me)) then
      call me%destroy()
      call destroy_node(me%next)
      nullify (me%previous)
      deallocate (me)
      nullify (me)
    end if
  end subroutine destroy_node

  !> \brief Remove an item from the list (given the key).
  subroutine remove_by_key(me, key)
    implicit none
    class(list), intent(inout) :: me
    class(*), intent(in)       :: key !< node key
    type(node), pointer :: p
    call me%get_node(key, p)
    call me%remove_by_pointer(p)
  end subroutine remove_by_key

  !> \brief Remove an item from the list.
  subroutine remove_by_pointer(me, p)
    implicit none
    class(list), intent(inout) :: me
    type(node), pointer        :: p   !< the item to remove
    logical :: has_next, has_previous
    if (associated(p)) then
      call p%destroy()  ! destroy the data
      has_next = associated(p%next)
      has_previous = associated(p%previous)
      if (has_next .and. has_previous) then    !neither first nor last in a list
        p%previous%next => p%next
        p%next%previous => p%previous
      elseif (has_next .and. .not. has_previous) then    !first one in a list
        me%head => p%next
        me%head%previous => null()
      elseif (has_previous .and. .not. has_next) then    !last one in a list
        me%tail => p%previous
        me%tail%next => null()
      elseif (.not. has_previous .and. .not. has_next) then  !only one in the list
        me%head => null()
        me%tail => null()
      end if
      deallocate (p)
      nullify (p)
      me%count = me%count - 1
    end if
  end subroutine remove_by_pointer

  !> \brief Get the data from a node
  subroutine get_node_data(me, value)
    implicit none
    class(node), intent(in)       :: me
    class(*), pointer, intent(out) :: value
    if (associated(me%value)) then
      value => me%value
    else
      call error_message('error: value pointer is not associated')
    end if
  end subroutine get_node_data

  !> \brief Returns a pointer to the data stored in the list.
  subroutine get_data(me, key, value)
    implicit none
    class(list), intent(in)       :: me
    class(*), intent(in)          :: key !< key of node
    class(*), pointer, intent(out) :: value !< data value
    type(node), pointer :: p
    call me%get_node(key, p)
    if (associated(p)) then
      value => p%value
    else
      value => null()
    end if
  end subroutine get_data

  !> \brief Returns a pointer to a node in a list.
  subroutine get_node(me, key, p_node)
    implicit none
    class(list), intent(in)         :: me
    class(*), intent(in)            :: key
    type(node), pointer, intent(out) :: p_node
    type(node), pointer :: p

    nullify (p_node)
    p => me%head
    do
      if (associated(p)) then
        if (me%keys_equal(p%key, key)) then
          p_node => p
          return
        end if
        p => p%next
      else
        return !not found
      end if
    end do
  end subroutine get_node

  !> \brief Returns true if the two keys are equal.
  !> \details Allowing a key to be an integer(i4) or a character string
  !! (can be case sensitive or not), or alternately, a user-defined \ref key_class.
  pure function keys_equal(me, k1, k2)
    implicit none
    class(list), intent(in) :: me
    class(*), intent(in)    :: k1
    class(*), intent(in)    :: k2
    logical                :: keys_equal
    keys_equal = .false.
    if (same_type_as(k1, k2)) then
      select type (k1)
       class is (key_class)
        select type (k2)
         class is (key_class)
          keys_equal = k1 == k2
        end select
       type is (integer(i4))
        select type (k2)
         type is (integer(i4))
          keys_equal = k1 == k2
        end select
       type is (character(len=*))
        select type (k2)
         type is (character(len=*))
          if (me%case_sensitive) then
            keys_equal = k1 == k2
          else
            keys_equal = uppercase(k1) == uppercase(k2)
          end if
        end select
      end select
    end if
  end function keys_equal

  !> \brief Convert a string to uppercase.
  pure function uppercase(str) result(string)
    implicit none
    character(len=*), intent(in) :: str
    character(len=len(str))     :: string
    integer(i4) :: i, idx
    character(len=*), parameter :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*), parameter :: lower = 'abcdefghijklmnopqrstuvwxyz'
    string = str
    do i = 1, len_trim(str)
      idx = index(lower, str(i:i))
      if (idx > 0) string(i:i) = upper(idx:idx)
    end do
  end function uppercase

  !> \brief Add a data clone to the list.
  !> \details Add an item to the end of the list by *cloning* it.
  !! That is, using a sourced allocation: `allocate(newitem, source=value)`.
  !! A clone is made of the original value, which is not affected.
  !! The list contains only the clone, which will be deallocated (and
  !! finalized if a finalizer is present) when removed from the list.
  !!
  !! This is different from the \ref list::add_pointer routine,
  !! which takes a pointer input.
  !!
  !! This one would normally be used for basic variables and types that
  !! do not contain pointers to other variables (and are not pointed to by
  !! other variables)
  subroutine add_clone(me, key, value)
    implicit none
    class(list), intent(inout) :: me
    class(*), intent(in)       :: key
    class(*), intent(in)       :: value
    class(*), pointer :: p_value
    allocate (p_value, source=value) !make a copy
    call me%add_pointer(key, p_value, destroy_on_delete=.true.)
    nullify (p_value)
  end subroutine add_clone

  !> \brief Add a data pointer to the list.
  !> \details Add an item to the list, and associate its pointer to the input value.
  !! \note If an item with the same key is already in the list, it is removed and the new one will replace it.
  subroutine add_pointer(me, key, value, destroy_on_delete)
    implicit none
    class(list), intent(inout)   :: me
    class(*), intent(in)         :: key
    !> *value* is unlimited polymorphic, so it can
    !! be any scalar type. If the type includes
    !! pointers or other objects that must be
    !! cleaned up when it is destroyed, then it
    !! should include a finalizer.
    class(*), intent(in), pointer :: value
    !> If false, the finalizer will
    !! not be called when the item is
    !! removed from the list (the
    !! pointer will only be
    !! nullified, so the caller is
    !! responsible for cleaning it up
    !! to avoid memory leaks).
    !! The default is *True*.
    logical, intent(in), optional :: destroy_on_delete
    type(node), pointer :: p
    !only allowing integer(i4), string, or key_class keys:
    select type (key)
     type is (integer(i4))
      !ok
     type is (character(len=*))
      if (len_trim(key) < 1) call error_message('Error: key must be nonblank.')
     class is (key_class)
      !ok
     class default
      call error_message('Error: key must be an integer(i4), character string, or key_class.')
    end select
    ! if the node is already there, then remove it
    call me%get_node(key, p)
    if (associated(p)) call me%remove_by_pointer(p)
    if (associated(me%tail)) then
      allocate (me%tail%next)  !insert new item at the end
      p => me%tail%next
      p%previous => me%tail
    else
      allocate (me%head)  !first item in the list
      p => me%head
    end if
    me%tail => p
    me%count = me%count + 1
    allocate (p%key, source=key)
    p%value => value
    if (present(destroy_on_delete)) p%destroy_on_delete = destroy_on_delete
  end subroutine add_pointer

end module mo_list
