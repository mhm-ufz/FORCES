!> \file    mo_list.F90
!> \copydoc mo_list

!> \brief   Module providing a linked list.
!> \details A simple logging framework derived from flist (https://github.com/jacobwilliams/flist).
!!
!! A generic list implementation.
!!
!! It uses an unlimited polymorphic `class(*)` pointer variable to allow it
!! to contain any type of data. The `key` can be an integer(i4), string, or
!! any user-defined \ref key_class.
!!
!! Example for an integer list with a specific getter:
!! \code{.f90}
!! module mo_int_list
!!   use mo_list, only: list
!!   type, extends(list) :: integer_list
!!   contains
!!     procedure :: get_integer
!!   end type
!! contains
!!   subroutine get_integer(this, key, value)
!!     class(integer_list), intent(in) :: this
!!     class(*), intent(in) :: key
!!     integer, pointer, intent(out) :: value
!!     class(*), pointer :: p
!!     call this%get(key, p)
!!     if (associated(p)) then
!!       select type (p)
!!         type is (integer)
!!           value => p
!!         class default
!!           error stop 'list item not an integer'
!!       end select
!!     else
!!       value => null()
!!     end if
!!   end subroutine get_integer
!! end module mo_int_list
!!
!! program test_int_list
!!   use mo_int_list
!!   type(integer_list) :: ilist
!!   integer, pointer :: val
!!   call ilist%add_clone('age', 30)
!!   call ilist%add_clone('year', 1990)
!!   call ilist%get_integer('age', val)
!!   print*, 'age: ', val
!!   call ilist%get_integer('year', val)
!!   print*, 'year: ', val
!! end program test_int_list
!! \endcode
!!
!> \copyright flist was originally released under the BSD 3-Clause license (included below).
!!
!! Copyright (c) 2015-2021, Jacob Williams
!! All rights reserved.
!!
!! Redistribution and use in source and binary forms, with or without modification,
!! are permitted provided that the following conditions are met:
!!
!! 1. Redistributions of source code must retain the above copyright notice, this
!! list of conditions and the following disclaimer.
!!
!! 2. Redistributions in binary form must reproduce the above copyright notice,
!! this list of conditions and the following disclaimer in the documentation and/or
!! other materials provided with the distribution.
!!
!! 3. Neither the name of the copyright holder nor the names of its contributors
!! may be used to endorse or promote products derived from this software without
!! specific prior written permission.
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
!> \date    Apr 2025
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_list

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
    procedure(key_equal_func), deferred :: key_equal
    procedure(key_assign_func), deferred :: key_assign
    generic :: operator(==) => key_equal
    generic :: assignment(=) => key_assign
  end type key_class

  !> \brief interface for equality operator for \ref key_class.
  abstract interface
    pure elemental logical function key_equal_func(this, that)
      import :: key_class
      implicit none
      class(key_class), intent(in) :: this
      class(key_class), intent(in) :: that
    end function key_equal_func
  end interface

  !> \brief interface for assignment operator for \ref key_class.
  abstract interface
    subroutine key_assign_func(this, that)
      import :: key_class
      implicit none
      class(key_class), intent(inout) :: this
      class(key_class), intent(in) :: that
    end subroutine key_assign_func
  end interface

  !> \class item
  !> \brief an item in the linked list.
  !> \details This is the container to the unlimited polymorphic `value` variable.
  type, private :: item
    class(*), allocatable :: key  !< the key (can be integer(i4), string, or \ref key_class)
    class(*), pointer :: value => null()  !< the data to hold
    !> If true, value pointer is deallocated when it is removed from the list, or the list is destroyed.
    !! If false, it is only nullified.
    logical :: destroy_on_delete = .true.
    type(item), pointer :: next => null()  !< the next one in the list
    type(item), pointer :: previous => null()  !< the previous one in the list
  contains
    procedure :: destroy => destroy_item_data  ! deallocate value
    procedure :: get_data => get_item_data      ! get data from an item
  end type item

  !> \class list
  !> \brief Linked list of pointers to polymorphic types.
  type, public :: list
    private
    type(item), pointer :: head => null() !< the first item in the list
    type(item), pointer :: tail => null() !< the last item in the list
  contains
    private
    procedure, public :: add_target              ! add a pointer item to the list pointing to given target
    procedure, public :: add_pointer             ! add a pointer item to the list
    procedure, public :: add_clone               ! add a non-pointer item to the list
    procedure, public :: add => add_clone        ! add a copy of an item to the list
    procedure, public :: get => get_data         ! get a pointer to an item in the list
    procedure, public :: get_keys                ! get a list of keys as (int, key) pairs
    procedure, public :: keys => list_keys       ! get a list of keys as (int, key) pairs
    procedure, public :: size => list_size       ! size of the list
    procedure, public :: has_key                 ! if the key is present in the list
    procedure, public :: has => has_key          ! if the key is present in the list
    procedure, public :: remove => remove_by_key ! remove item from the list, given the key
    procedure, public :: destroy => destroy_list ! destroy the list and deallocate/finalize all the data
    procedure, public :: deepcopy                ! deep copy a list
    ! procedures that operate on items:
    procedure :: remove_by_pointer ! remove item from list, given pointer to it
    procedure :: get_item          ! get a pointer to an item in the list
    !private routines:
    procedure :: get_data                        ! get a pointer to an item in the list
    procedure :: list_copy ! copy a list
    generic, public :: assignment(=) => list_copy
    final :: list_finalizer
  end type list

  !> \class key_list
  !> \brief Linked list for key listings.
  !> \details Provides a special method \ref get_key to return valid key-values.
  type, public, extends(list) :: key_list
  contains
    procedure, public :: eqv => key_list_eqv
    generic :: operator(.eqv.) => eqv
    procedure, public :: eq => key_list_eq
    generic :: operator(==) => eq
    procedure, private :: get_string, get_integer, get_key_class
    generic, public :: get_key => get_string, get_integer, get_key_class
    procedure, public :: get_type
  end type key_list

contains

  !> \brief Get a string value from a key list.
  subroutine get_string(this, id, value)
    class(key_list), intent(in) :: this
    integer(i4), intent(in) :: id !< key id
    character(:), allocatable :: value !< key value
    class(*), pointer :: p
    call this%get(id, p)
    if (.not.associated(p)) call error_message('key_list: item not associated') ! LCOV_EXCL_LINE
    select type (p)
      type is (character(len=*))
        value = p
      class default
        call error_message("string_list: item is not a string.") ! LCOV_EXCL_LINE
    end select
  end subroutine get_string

  !> \brief Get an integer value from a key list.
  subroutine get_integer(this, id, value)
    class(key_list), intent(in) :: this
    integer(i4), intent(in) :: id !< key id
    integer(i4) :: value !< key value
    class(*), pointer :: p
    call this%get(id, p)
    if (.not.associated(p)) call error_message('key_list: item not associated') ! LCOV_EXCL_LINE
    select type (p)
      type is (integer(i4))
        value = p
      class default
        call error_message("key_list: item is not an integer.") ! LCOV_EXCL_LINE
    end select
  end subroutine get_integer

  !> \brief Get a key class value from a key list.
  subroutine get_key_class(this, id, value)
    class(key_list), intent(in) :: this
    integer(i4), intent(in) :: id !< key id
    class(key_class) :: value !< key value
    class(*), pointer :: p
    call this%get(id, p)
    if (.not.associated(p)) call error_message('key_list: item not associated') ! LCOV_EXCL_LINE
    select type (p)
      class is (key_class)
        value = p
      class default
        call error_message("key_list: item is not of key class.") ! LCOV_EXCL_LINE
    end select
  end subroutine get_key_class

  !> \brief (==) Compare key lists.
  logical function key_list_eq(this, that)
    implicit none
    class(key_list), intent(in) :: this
    class(key_list), intent(in) :: that
    integer(i4) :: cnt, i
    class(*), pointer :: p1, p2
    cnt = this%size()
    key_list_eq = .false.
    if (cnt /= that%size()) return
    do i = 1_i4, cnt
      call this%get(i, p1)
      call that%get(i, p2)
      if (.not.keys_equal(p1, p2)) return
    end do
    key_list_eq = .true.
  end function key_list_eq

  !> \brief (.eqv.) Compare key lists for equivalence (order independent).
  logical function key_list_eqv(this, that)
    implicit none
    class(key_list), intent(in) :: this
    class(key_list), intent(in) :: that
    integer(i4) :: cnt, i, j
    class(*), pointer :: p1, p2
    logical :: found
    cnt = this%size()
    key_list_eqv = .false.
    if (cnt /= that%size()) return
    do i = 1_i4, cnt
      call this%get(i, p1)
      found = .false.
      do j = 1_i4, cnt
        call that%get(j, p2)
        if (keys_equal(p1, p2)) then
          found = .true.
          exit
        end if
      end do
      if (.not.found) return
    end do
    key_list_eqv = .true.
  end function key_list_eqv

  !> \brief Returns an identifier for the key type
  !> \returns identifier as string: "str" (string), "int" (integer) or "cls" (\ref key_class)
  character(3) function get_type(this, id)
    implicit none
    class(key_list), intent(in) :: this
    integer(i4), intent(in) :: id !< key id
    class(*), pointer :: p
    call this%get(id, p)
    if (.not.associated(p)) call error_message('key_list: item not associated') ! LCOV_EXCL_LINE
    select type (p)
      type is (character(len=*))
        get_type = "str"
      type is (integer(i4))
        get_type = "int"
      class is (key_class)
        get_type = "cls"
      class default
        call error_message("key_list: item is not a valid key type.") ! LCOV_EXCL_LINE
    end select
  end function get_type

  !> \brief Returns true if the key is present in the list.
  logical function has_key(this, key)
    implicit none
    class(list), intent(inout) :: this
    class(*), intent(in)       :: key !< key
    type(item), pointer :: p
    call this%get_item(key, p)
    has_key = associated(p)
  end function has_key

  !> \brief Returns a list of keys as (int, key) pairs.
  subroutine get_keys(this, keys)
    implicit none
    class(list), intent(in)         :: this
    type(key_list), intent(out) :: keys !< keys list by indices
    type(item), pointer :: p
    integer(i4) :: cnt
    cnt = 0_i4
    p => this%head
    do
      if (associated(p)) then
        cnt = cnt + 1_i4
        call keys%add(cnt, p%key)
        p => p%next
      else
        return ! at tail
      end if
    end do
  end subroutine get_keys

  !> \brief Returns a list of keys as (int, key) pairs.
  type(key_list) function list_keys(this)
    implicit none
    class(list), intent(in) :: this
    call this%get_keys(list_keys)
  end function list_keys

  !> \brief Returns the size of the list.
  function list_size(this) result(cnt)
    implicit none
    class(list), intent(in) :: this
    type(item), pointer :: p
    integer(i4) :: cnt
    cnt = 0_i4
    p => this%head
    do
      if (associated(p)) then
        cnt = cnt + 1_i4
        p => p%next
      else
        return ! at tail
      end if
    end do
  end function list_size

  !> \brief Copy a list.
  !> \details Items that should not be destroyed on delete will be copied as pointers.
  subroutine list_copy(this, that)
    implicit none
    class(list), intent(inout) :: this
    class(list), intent(in) :: that !< shallow copy
    type(item), pointer :: p
    call this%destroy()
    p => that%head
    do
      if (associated(p)) then
        if (p%destroy_on_delete) then
          call this%add_clone(p%key, p%value)
        else
          call this%add_pointer(p%key, p%value, p%destroy_on_delete)
        end if
        p => p%next
      else
        return ! at tail
      end if
    end do
  end subroutine list_copy

  !> \brief Deep copy a list.
  !> \details All items will be cloned.
  subroutine deepcopy(this, copy)
    implicit none
    class(list), intent(in) :: this
    class(list), intent(inout) :: copy !< deep copy
    type(item), pointer :: p
    call copy%destroy()
    p => this%head
    do
      if (associated(p)) then
        call copy%add_clone(p%key, p%value)
        p => p%next
      else
        return ! at tail
      end if
    end do
  end subroutine deepcopy

  !> \brief Destroy the data in the item.
  impure elemental subroutine destroy_item_data(this)
    implicit none
    class(item), intent(inout) :: this
    if (allocated(this%key)) deallocate (this%key)
    if (this%destroy_on_delete) then
      ! deallocates the pointer (and call any finalizer)
      ! (otherwise, it is up to the caller to do this)
      if (associated(this%value)) deallocate (this%value)
    end if
    nullify (this%value)
  end subroutine destroy_item_data

  !> just a wrapper for \ref destroy_list.
  impure elemental subroutine list_finalizer(this)
    implicit none
    type(list), intent(inout) :: this
    call this%destroy()
  end subroutine list_finalizer

  !> \brief destroy the list (traverses from head to tail)
  impure elemental subroutine destroy_list(this)
    implicit none
    class(list), intent(inout) :: this
    if (associated(this%head)) call destroy_item(this%head)
    nullify (this%head)
    nullify (this%tail)
  end subroutine destroy_list

  !> \brief destroy the item (and subsequent ones in the list).
  impure recursive subroutine destroy_item(this)
    implicit none
    type(item), pointer :: this
    if (associated(this)) then
      call this%destroy()
      call destroy_item(this%next)
      nullify (this%previous)
      deallocate (this)
      nullify (this)
    end if
  end subroutine destroy_item

  !> \brief Remove an item from the list (given the key).
  subroutine remove_by_key(this, key)
    implicit none
    class(list), intent(inout) :: this
    class(*), intent(in)       :: key !< item key
    type(item), pointer :: p
    call this%get_item(key, p)
    if (.not.associated(p)) call error_message('list%remove: item not associated') ! LCOV_EXCL_LINE
    call this%remove_by_pointer(p)
  end subroutine remove_by_key

  !> \brief Remove an item from the list.
  subroutine remove_by_pointer(this, p)
    implicit none
    class(list), intent(inout) :: this
    type(item), pointer        :: p   !< the item to remove
    logical :: has_next, has_previous
    if (.not.associated(p)) return
    call p%destroy()  ! destroy the data
    has_next = associated(p%next)
    has_previous = associated(p%previous)
    if (has_next .and. has_previous) then    !neither first nor last in a list
      p%previous%next => p%next
      p%next%previous => p%previous
    elseif (has_next .and. .not. has_previous) then    !first one in a list
      this%head => p%next
      this%head%previous => null()
    elseif (has_previous .and. .not. has_next) then    !last one in a list
      this%tail => p%previous
      this%tail%next => null()
    elseif (.not. has_previous .and. .not. has_next) then  !only one in the list
      this%head => null()
      this%tail => null()
    end if
    deallocate (p)
    nullify (p)
  end subroutine remove_by_pointer

  !> \brief Get the data from an item
  subroutine get_item_data(this, value)
    implicit none
    class(item), intent(in)        :: this
    class(*), pointer, intent(out) :: value
    if (.not.associated(this%value)) call error_message('item%get: item value not associated') ! LCOV_EXCL_LINE
    value => this%value
  end subroutine get_item_data

  !> \brief Returns a pointer to the data stored in the list.
  subroutine get_data(this, key, value)
    implicit none
    class(list), intent(in)        :: this
    class(*), intent(in)           :: key !< key of item
    class(*), pointer, intent(out) :: value !< data value
    type(item), pointer :: p
    call this%get_item(key, p)
    if (.not.associated(p)) call error_message('list%get: item not associated') ! LCOV_EXCL_LINE
    call p%get_data(value)
  end subroutine get_data

  !> \brief Returns a pointer to an item in a list.
  subroutine get_item(this, key, p_item)
    implicit none
    class(list), intent(in)          :: this
    class(*), intent(in)             :: key !< key of item
    type(item), pointer, intent(out) :: p_item !< item as pointer
    type(item), pointer :: p
    nullify (p_item)
    p => this%head
    do
      if (associated(p)) then
        if (keys_equal(p%key, key)) then
          p_item => p
          return
        end if
        p => p%next
      else
        return !not found
      end if
    end do
  end subroutine get_item

  !> \brief Returns true if the two keys are equal.
  !> \details Allowing a key to be an integer(i4) or a character string
  !! (can be case sensitive or not), or alternately, a user-defined \ref key_class.
  pure function keys_equal(k1, k2)
    implicit none
    class(*), intent(in)    :: k1 !< key 1
    class(*), intent(in)    :: k2 !< key 2
    logical                 :: keys_equal
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
          keys_equal = k1 == k2
        end select
      end select
    end if
  end function keys_equal

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
  subroutine add_clone(this, key, value, replace)
    implicit none
    class(list), intent(inout) :: this
    class(*), intent(in)       :: key !< key of the new item
    class(*), intent(in)       :: value !< value to make a copy of
    !> if .true. and the key is already present, replace the existing item (default .false.)
    logical, intent(in), optional :: replace
    class(*), pointer :: p_value
    allocate (p_value, source=value) !make a copy
    call this%add_pointer(key, p_value, destroy_on_delete=.true., replace=replace)
    nullify (p_value)
  end subroutine add_clone

  !> \brief Add a data pointer to the list.
  !> \details Add an item to the list, and associate its pointer to the input value.
  !! \note If an item with the same key is already in the list, it is removed and the new one will replace it.
  subroutine add_pointer(this, key, value, destroy_on_delete, replace)
    implicit none
    class(list), intent(inout)   :: this
    class(*), intent(in)         :: key !< key of the new item
    !> *value* is unlimited polymorphic, so it can be any scalar type.
    !! If the type includes pointers or other objects that must be cleaned up when it is destroyed,
    !! then it should include a finalizer.
    class(*), intent(in), pointer :: value
    !> If false, the finalizer will not be called when the item is removed from the list
    !! (the pointer will only be nullified, so the caller is responsible for cleaning it up to avoid memory leaks).
    !! The default is *True*.
    logical, intent(in), optional :: destroy_on_delete
    !> if .true. and the key is already present, replace the existing item (default .false.)
    logical, intent(in), optional :: replace
    type(item), pointer :: p
    logical :: replace_ = .false.
    if (present(replace)) replace_ = .true.
    !only allowing integer, integer(i4), string, or key_class keys:
    select type (key)
     type is (integer(i4))
      !ok
     type is (character(len=*))
      if (len_trim(key) < 1) call error_message('Error: key must be nonblank.') ! LCOV_EXCL_LINE
     class is (key_class)
      !ok
     class default
      call error_message('Error: key must be an integer(i4), character string, or key_class.') ! LCOV_EXCL_LINE
    end select
    ! if the item is already there, then remove it
    call this%get_item(key, p)
    if (associated(p)) then
      if (.not.replace_) call error_message('Error: key already present but replace=.false.') ! LCOV_EXCL_LINE
      if (p%destroy_on_delete .and. associated(p%value)) deallocate(p%value)
      p%value => value
      p%destroy_on_delete = .true. ! reset to default
      if (present(destroy_on_delete)) p%destroy_on_delete = destroy_on_delete
      return
    end if
    ! if item not present, add it to the tail
    if (associated(this%tail)) then
      allocate (this%tail%next)  !insert new item at the end
      p => this%tail%next
      p%previous => this%tail
    else
      allocate (this%head)  !first item in the list
      p => this%head
    end if
    this%tail => p
    allocate (p%key, source=key)
    p%value => value
    if (present(destroy_on_delete)) p%destroy_on_delete = destroy_on_delete
  end subroutine add_pointer

  !> \brief Add a data target to the list.
  !> \details Add an item to the list, and associate its pointer to the input target.
  !! \note target will not be destroyed on delete
  subroutine add_target(this, key, value, replace)
    implicit none
    class(list), intent(inout)   :: this
    class(*), intent(in)         :: key !< key of the new item
    class(*), intent(in), target :: value !< target to point to
    !> if .true. and the key is already present, replace the existing item (default .false.)
    logical, intent(in), optional :: replace
    class(*), pointer :: p_value
    p_value => value
    ! target should not be destroyed on delete
    call this%add_pointer(key, p_value, destroy_on_delete=.false., replace=replace)
    nullify (p_value)
  end subroutine add_target

end module mo_list
