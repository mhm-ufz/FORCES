!> \file mo_append.f90
!> \copydoc mo_append

!> \brief Append values on existing arrays.
!> \details Provides routines to append (rows) and paste (columns) scalars, vectors,
!!          and matrixes onto existing arrays.
!> \changelog
!! - Juliane Mai,    Aug 2012
!! - Juliane Mai,    Aug 2012
!!   - character append & paste
!! - Matthias Cuntz, Jan 2013
!!   - removed 256 character restriction
!! - Matthias Cuntz, Feb 2013
!!   - logical append and paste
!> \author Juliane Mai
!> \date Aug 2012
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_append

  USE mo_kind, only: i4, i8, sp, dp

  IMPLICIT NONE

  PUBLIC :: append    !< Returns input1 appended (on first dim) with input2.  (like Unix cat)
  PUBLIC :: paste     !< Returns input1 pasted (on last dim) with input2. (like Unix paste)
  PUBLIC :: add_nodata_slice     !< Returns input1 extended (on last dim) with nodata slice.

  ! ------------------------------------------------------------------

  !>        \brief Append (rows) scalars, vectors, and matrixes onto existing array.

  !>        \details Appends one input to the rows of another, i.e. append
  !!        on the first dimension. Append variants for 3d and 4d also exists.\n
  !!        The input might be a scalar, a vector or a matrix.\n
  !!        Possibilities are:\n
  !!        1.     append scalar to vector\n
  !!        2.     append vector to vector\n
  !!        3.     append matrix to matrix
  !!
  !>        \b Example
  !!
  !!        Append input1 with input2
  !!        \code{.f90}
  !!         input1 = (/ 1.0_dp , 2.0_dp /)
  !!         input2 = 3.0_dp
  !!
  !!         call append(input1, input2)
  !!         --> input1 = (/ 1.0_dp , 2.0_dp, 3.0_dp /)
  !!        \endcode
  !!
  !!        See also test folder for a detailed example, "test/test_mo_append/".

  !>        \param[in] "input2"
  !!         Values to append. Can be `integer(i4/i8)`, `real(sp/dp)`, `logical`, or `character(len=*)`
  !!         and also scalar, `dimension(:)`, or `dimension(:,:)`.\n
  !!         Includes 3d version, where the append is always done along
  !!         the first dimension.\n
  !!         4d version is available for `real(dp)` and `logical`, where
  !!         append can be done in any component of the dimension.\n
  !!         If not scalar then the columns have to agree with `input1`.
  !>        \param[inout] "`allocatable` :: input1"
  !!         Array to be appended to. Can be `integer(i4/i8)`, `real(sp/dp)`, `logical`,
  !!         or `character(len=*)`. Must be `dimension(:)` or `dimension(:,:)`, and `allocatable`.\n
  !!         If `input2` is not scalar then it must be `size(input1,2) = size(input2,2)`.
  !>        \param[in] "optional :: fill_value"
  !!         Filler value if number of columns do not coincide. Then the largest column will
  !!         be taken and empty elements from the generated column will be filled by this value.
  !>        \param[in] "integer(i4), optional :: idim"
  !!         Applicable when appending matrix-matrix, 3d, and 4d for `logical` and `real(dp)`.
  !!         Determines along which dimension `input2` is appended to.

  !>        \note
  !!         Size of `input1` and `input2` have to fit together,
  !!         i.e. number of columns input1 = number of columns input2.

  !>        \author Juliane Mai
  !>        \date Aug 2012

  !>        \author Matthias Cuntz
  !>        \date Jan 2013
  !!          - removed 256 character restriction.
  !>        \date Feb 2013
  !!          - logical append and paste.

  !>        \author Matthias Zink
  !>        \date Feb 2015
  !!          - added optional 'fill_value' for logical append

  !>        \author Stephan Thober
  !>        \date Jan 2017
  !!          - added 3d version for append
  !>        \date Jul 2018
  !!          - added optional iDim argument for arrays larger equal 2d

  !>        \author Arya Prasetya
  !>        \date Nov 2021
  !!          -  included i4_3d in append interface

  INTERFACE append
     MODULE PROCEDURE append_i4_v_s, append_i4_v_v, append_i4_m_m, &
          append_i4_3d, append_i8_v_s, append_i8_v_v, &
          append_i8_m_m, append_i8_3d, &
          append_sp_v_s, append_sp_v_v, append_sp_m_m, &
          append_sp_3d, &
          append_dp_v_s, append_dp_v_v, append_dp_m_m, &
          append_dp_3d, append_dp_4d, &
          append_char_v_s, append_char_v_v, append_char_m_m, &
          append_char_3d, &
          append_lgt_v_s, append_lgt_v_v, append_lgt_m_m, &
          append_lgt_3d, append_lgt_4d
  END INTERFACE append

  ! ------------------------------------------------------------------

  !>        \brief Paste (columns) scalars, vectors, and matrixes onto existing array.

  !>        \details Pastes one input to the columns of another, i.e. append
  !!        on the second dimension. Paste variants for 3d and 4d also exists.\n
  !!        The input might be a scalar, a vector or a matrix.\n
  !!        Possibilities are:\n
  !!        1.     paste scalar to one-line matrix\n
  !!        2.     paste vector to a matrix\n
  !!        3.     paste matrix to matrix
  !!
  !!        \b Example
  !!
  !!        Paste input2 to input1.
  !!        \code{.f90}
  !!        input1 = (/ 1.0_dp , 2.0_dp /)
  !!        input2 = (/ 3.0_dp , 4.0_dp /)
  !!
  !!        call paste(input1, input2)
  !!        --> input1(1,:) = (/ 1.0_dp , 3.0_dp /)
  !!            input1(2,:) = (/ 2.0_dp , 4.0_dp /)
  !!        \endcode
  !!        See also test folder for a detailed example, "test/test_mo_append".

  !>        \param[in] "input2"
  !!         Values to paste. Can be `integer(i4/i8)`, `real(sp/dp)`, `logical`, or `character(len=*)`
  !!         and also scalar, `dimension(:)`, or `dimension(:,:)`.\n
  !!         Includes 3d and 4d version for `real(dp)` and `integer(i4)`, where paste is always done along
  !!         the last dimension.\n
  !!         If not scalar then the rows have to agree with `input1`.\n
  !>        \param[inout] "allocatable :: input1"
  !!         Array to be pasted to. Can be `integer(i4/i8)`, `real(sp/dp)`,
  !!         or `character(len=*)`. Must be `dimension(:)`, `dimension(:,:)`, `dimension(:,:,:)`, or
  !!         `dimension(:,:,:,:)` and `allocatable`.\n
  !!         If `input2` is not scalar then it must be `size(input1,1) = size(input2,1)`.
  !>        \param[in] "optional :: fill_value"
  !!         Filler value if number of rows do not coincide. Then the largest row will
  !!         be taken and empty elements from the generated rows will be filled by this value.

  !>        \note
  !!        Size of input1 and input2 have to fit together,
  !!        i.e. number of rows input1 = number of rows input2

  !>       \author Juliane Mai
  !>       \date Aug 2012

  !>       \author Matthias Cuntz
  !>       \date Jan 2013
  !!          - removed 256 character restriction
  !>       \date Feb 2013
  !!          - logical append and paste

  INTERFACE paste
     MODULE PROCEDURE paste_i4_m_s, paste_i4_m_v, paste_i4_m_m, &
          paste_i8_m_s, paste_i8_m_v, paste_i8_m_m, &
          paste_sp_m_s, paste_sp_m_v, paste_sp_m_m, &
          paste_dp_m_s, paste_dp_m_v, paste_dp_m_m, &
          paste_char_m_s, paste_char_m_v, paste_char_m_m, &
          paste_lgt_m_s, paste_lgt_m_v, paste_lgt_m_m, &
          paste_dp_3d, paste_dp_4d, paste_i4_3d, paste_i4_4d

  END INTERFACE paste

  ! ------------------------------------------------------------------

  !>        \brief Paste a matrix of ones times a value onto an existing matrix.

  !>        \details Pastes a matrix of uniform element and predefined number of columns
  !!        to the columns of another matrix.\n
  !!
  !!        \b Example
  !!
  !!        Add 2 columns of elements 5.0_dp.
  !!        \code{.f90}
  !!        input1(1,:) = (/ 1.0_dp , 2.0_dp /)
  !!        input1(2,:) = (/ 2.0_dp , 4.0_dp /)
  !!
  !!        call add_nodata_slice(input1, 2, 5.0_dp)
  !!        --> input1(1,:) = (/ 1.0_dp , 2.0_dp /)
  !!            input1(2,:) = (/ 2.0_dp , 4.0_dp /)
  !!            input1(3,:) = (/ 5.0_dp , 5.0_dp /)
  !!            input1(4,:) = (/ 5.0_dp , 5.0_dp /)
  !!        \endcode
  !!
  !!        See also test folder for a detailed example, "test/test_mo_append/".

  !>        \param[in,out] "allocatable :: matrix"
  !!         Matrix to be pasted to. Can be `integer(i4)` or `real(dp)`.
  !!         Must be `dimension(:,:)`, `dimension(:,:,:)`, or `dimension(:,:,:,:)` and `allocatable`.\n
  !>        \param[in] "integer(i4) :: nAdd"
  !!         Number of column to paste.\n
  !>        \param[in] "noDataValue"
  !!         Value of elements of the matrix. Can be `integer(i4)` or `real(dp)`.

  interface add_nodata_slice
    module procedure add_nodata_slice_dp_2d, add_nodata_slice_dp_3d, add_nodata_slice_dp_4d, &
            add_nodata_slice_i4_2d, add_nodata_slice_i4_3d, add_nodata_slice_i4_4d
  end interface add_nodata_slice

  ! ------------------------------------------------------------------

  PRIVATE

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  SUBROUTINE append_i4_v_s(vec1, sca2)

    implicit none

    integer(i4), dimension(:), allocatable, intent(inout)   :: vec1
    integer(i4),                            intent(in)      :: sca2

    ! local variables
    integer(i4)                             :: n1, n2
    integer(i4), dimension(:), allocatable  :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_i4_v_s

  SUBROUTINE append_i4_v_v(vec1, vec2)

    implicit none

    integer(i4), dimension(:), allocatable, intent(inout)   :: vec1
    integer(i4), dimension(:), intent(in)                   :: vec2

    ! local variables
    integer(i4)                             :: n1, n2    ! length of vectors
    integer(i4), dimension(:), allocatable  :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_i4_v_v

  SUBROUTINE append_i4_m_m(mat1, mat2, fill_value)

    implicit none

    integer(i4), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i4), dimension(:,:), intent(in)                   :: mat2
    integer(i4), optional,       intent(in)                   :: fill_value

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    integer(i4), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns

       if ((n1 /= n2) .and. .not. present(fill_value) ) then
          print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       if ( n1 == n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
       end if

       if ( n1 > n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)                = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
          mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
       end if

       if ( n1 < n2 ) then
          allocate(mat1(m1+m2,n2))
          mat1(      1:m1,      1:n1) = tmp(1:m1,:)
          mat1(      1:m1,   n1+1:n2) = fill_value
          mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
       end if

   else
       n1 = 0_i4

       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_i4_m_m

  SUBROUTINE append_i4_3d(mat1, mat2, fill_value)

    implicit none

    integer(i4), dimension(:,:,:), allocatable, intent(inout) :: mat1
    integer(i4), dimension(:,:,:), intent(in)                 :: mat2
    integer(i4), optional,         intent(in)                 :: fill_value

    ! local variables
    integer(i4)                                :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                                :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                                :: j1, j2    ! dim3 of matrixes: something else
    integer(i4), dimension(:,:,:), allocatable :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_i4_3d'

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else

       if ((n1 /= n2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1+m2,n1,j1))
       mat1(1:m1,:,:)          = tmp(1:m1,:,:)
       mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)

    else

       allocate(mat1(m2,n2,j2))
       mat1 = mat2

    end if

  END SUBROUTINE append_i4_3d

  SUBROUTINE append_i8_v_s(vec1, sca2)

    implicit none

    integer(i8), dimension(:), allocatable, intent(inout)   :: vec1
    integer(i8),                            intent(in)      :: sca2

    ! local variables
    integer(i4)                             :: n1, n2
    integer(i8), dimension(:), allocatable  :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_i8_v_s

  SUBROUTINE append_i8_v_v(vec1, vec2)

    implicit none

    integer(i8), dimension(:), allocatable, intent(inout)   :: vec1
    integer(i8), dimension(:), intent(in)                   :: vec2

    ! local variables
    integer(i4)                             :: n1, n2    ! length of vectors
    integer(i8), dimension(:), allocatable  :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_i8_v_v

  SUBROUTINE append_i8_m_m(mat1, mat2, fill_value)

    implicit none

    integer(i8), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i8), dimension(:,:), intent(in)                   :: mat2
    integer(i8), optional,       intent(in)                   :: fill_value

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    integer(i8), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns

       if ((n1 /= n2) .and. .not. present(fill_value) ) then
          print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       if ( n1 == n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
       end if

       if ( n1 > n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)                = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
          mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
       end if

       if ( n1 < n2 ) then
          allocate(mat1(m1+m2,n2))
          mat1(      1:m1,      1:n1) = tmp(1:m1,:)
          mat1(      1:m1,   n1+1:n2) = fill_value
          mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
       end if

    else
       n1 = 0_i4

       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_i8_m_m

  SUBROUTINE append_i8_3d(mat1, mat2, fill_value)

    implicit none

    integer(i8), dimension(:,:,:), allocatable, intent(inout) :: mat1
    integer(i8), dimension(:,:,:), intent(in)                 :: mat2
    integer(i8), optional,         intent(in)                 :: fill_value

    ! local variables
    integer(i4)                                :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                                :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                                :: j1, j2    ! dim3 of matrixes: something else
    integer(i8), dimension(:,:,:), allocatable :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_i8_3d'

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else

       if ((n1 /= n2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1+m2,n1,j1))
       mat1(1:m1,:,:)          = tmp(1:m1,:,:)
       mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)

    else

       allocate(mat1(m2,n2,j2))
       mat1 = mat2

    end if

  END SUBROUTINE append_i8_3d

  SUBROUTINE append_sp_v_s(vec1, sca2)

    implicit none

    real(sp), dimension(:), allocatable, intent(inout)   :: vec1
    real(sp),                            intent(in)      :: sca2

    ! local variables
    integer(i4)                             :: n1, n2
    real(sp), dimension(:), allocatable     :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_sp_v_s

  SUBROUTINE append_sp_v_v(vec1, vec2)

    implicit none

    real(sp), dimension(:), allocatable, intent(inout)   :: vec1
    real(sp), dimension(:),              intent(in)      :: vec2

    ! local variables
    integer(i4)                             :: n1, n2    ! length of vectors
    real(sp), dimension(:), allocatable     :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_sp_v_v

  SUBROUTINE append_sp_m_m(mat1, mat2, fill_value)

    implicit none

    real(sp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(sp), dimension(:,:), intent(in)                   :: mat2
    real(sp), optional,       intent(in)                   :: fill_value

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    real(sp), dimension(:,:), allocatable     :: tmp

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns

       if ((n1 /= n2) .and. .not. present(fill_value) ) then
          print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

      if ( n1 == n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
       end if

       if ( n1 > n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)                = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
          mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
       end if

       if ( n1 < n2 ) then
          allocate(mat1(m1+m2,n2))
          mat1(      1:m1,      1:n1) = tmp(1:m1,:)
          mat1(      1:m1,   n1+1:n2) = fill_value
          mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
       end if

    else
       n1 = 0_i4

       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_sp_m_m

  SUBROUTINE append_sp_3d(mat1, mat2, fill_value)

    implicit none

    real(sp), dimension(:,:,:), allocatable, intent(inout)   :: mat1
    real(sp), dimension(:,:,:), intent(in)                   :: mat2
    real(sp), optional,         intent(in)                   :: fill_value

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                               :: j1, j2    ! dim3 of matrixes: something else
    real(sp), dimension(:,:,:), allocatable   :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_sp_3d'

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else

       if ((n1 /= n2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1+m2,n1,j1))
       mat1(1:m1,:,:)          = tmp(1:m1,:,:)
       mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)

    else

       allocate(mat1(m2,n2,j2))
       mat1 = mat2

    end if

  END SUBROUTINE append_sp_3d

  SUBROUTINE append_dp_v_s(vec1, sca2)

    implicit none

    real(dp), dimension(:), allocatable, intent(inout)   :: vec1
    real(dp),                            intent(in)      :: sca2

    ! local variables
    integer(i4)                             :: n1, n2
    real(dp), dimension(:), allocatable     :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_dp_v_s

  SUBROUTINE append_dp_v_v(vec1, vec2)

    implicit none

    real(dp), dimension(:), allocatable, intent(inout)   :: vec1
    real(dp), dimension(:), intent(in)                   :: vec2

    ! local variables
    integer(i4)                             :: n1, n2    ! length of vectors
    real(dp), dimension(:), allocatable     :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save vec1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_dp_v_v

  SUBROUTINE append_dp_m_m(mat1, mat2, fill_value, idim)

    implicit none

    real(dp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(dp), dimension(:,:), intent(in)                   :: mat2
    real(dp), optional,       intent(in)                   :: fill_value
    integer(i4), optional,       intent(in)                   :: idim

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                               :: dd
    real(dp), dimension(:,:), allocatable     :: tmp

    dd = 1
    if (present(idim)) dd = idim
    if (dd > 2) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 2'
      STOP
    end if


    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns

       if (present(idim)) then
         if (dd == 1) then
           if (n1 /= n2) then
             print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
             STOP
           end if

          ! save mat1
           call move_alloc(mat1, tmp)

           allocate(mat1(m1+m2,n1))
           mat1(1:m1,:)          = tmp(1:m1,:)
           mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)

         else if (dd == 2) then

           if (m1 /= m2) then
             print*, 'append: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
             STOP
           end if

          ! save mat1
           call move_alloc(mat1, tmp)

           allocate(mat1(m1,n1 + n2))
           mat1(:,1:n1)          = tmp(:,1:n1)
           mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)

         end if

       else

         if ((n1 /= n2) .and. .not. present(fill_value) ) then
           print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         if ( n1 == n2 ) then
           allocate(mat1(m1+m2,n1))
           mat1(1:m1,:)          = tmp(1:m1,:)
           mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
         end if

         if ( n1 > n2 ) then
           allocate(mat1(m1+m2,n1))
           mat1(1:m1,:)                = tmp(1:m1,:)
           mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
           mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
         end if

         if ( n1 < n2 ) then
           allocate(mat1(m1+m2,n2))
           mat1(      1:m1,      1:n1) = tmp(1:m1,:)
           mat1(      1:m1,   n1+1:n2) = fill_value
           mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
         end if
       end if

    else
       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_dp_m_m

  SUBROUTINE append_dp_3d(mat1, mat2, fill_value, idim)

    implicit none

    real(dp), dimension(:,:,:), allocatable, intent(inout)   :: mat1
    real(dp), dimension(:,:,:), intent(in)                   :: mat2
    real(dp), optional,         intent(in)                   :: fill_value
    integer(i4), optional,      intent(in)                   :: idim

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                               :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                               :: j1, j2    ! dim3 of matrixes: something else
    integer(i4)                               :: dd        ! dimension it is appended along
    real(dp), dimension(:,:,:), allocatable   :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_dp_3d'

    dd = 1_i4
    if (present(idim)) dd = idim
    if (dd > 3) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 3'
      STOP
    end if

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else

       if (dd == 1) then
         if ((n1 /= n2) .or. (j1 /= j2) ) then
           print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1+m2,n1,j1))
         mat1(1:m1,:,:)          = tmp(1:m1,:,:)
         mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)
       else if (dd == 2) then
         if ((m1 /= m2) .or. (j1 /= j2) ) then
           print*, 'append: size mismatch: dim 1 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1 + n2,j1))
         mat1(:,1:n1,:)          = tmp(:,1:n1,:)
         mat1(:,n1+1_i4:n1+n2,:) = mat2(:,1:n2,:)
       else if (dd == 3) then
         if ((m1 /= m2) .or. (n1 /= n2) ) then
           print*, 'append: size mismatch: dim 1 and 2 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1,j1 + j2))
         mat1(:,:,1:j1) = tmp(:,:,1:j1)
         mat1(:,:,j1+1_i4:j1+j2) = mat2(:,:,1:j2)
       end if

    else

       allocate(mat1(m2,n2,j2))
       mat1 = mat2

    end if

  END SUBROUTINE append_dp_3d

  SUBROUTINE append_dp_4d(mat1, mat2, fill_value, idim)

    implicit none

    real(dp), dimension(:,:,:,:), allocatable, intent(inout)   :: mat1
    real(dp), dimension(:,:,:,:), intent(in)                   :: mat2
    real(dp), optional,         intent(in)                   :: fill_value
    integer(i4), optional,      intent(in)                   :: idim

    ! local variables
    integer(i4)           :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)           :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)           :: j1, j2    ! dim3 of matrixes: something else
    integer(i4)           :: i1, i2    ! dim4 of matrixes: something else
    integer(i4)           :: dd        ! dimension it is appended along
    real(dp), allocatable :: tmp(:,:,:,:)

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_dp_3d'

    dd = 1_i4
    if (present(idim)) dd = idim
    if (dd > 4) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 4'
      STOP
    end if

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else
    i2 = size(mat2,4)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else
       i1 = size(mat1,4)    ! something else

       if (dd == 1) then
         if ((n1 /= n2) .or. (j1 /= j2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 2, 3, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1+m2,n1,j1,i1))
         mat1(1:m1,:,:,:)          = tmp(1:m1,:,:,:)
         mat1(m1+1_i4:m1+m2,:,:,:) = mat2(1:m2,:,:,:)
       else if (dd == 2) then
         if ((m1 /= m2) .or. (j1 /= j2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 1, 3, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1 + n2,j1,i1))
         mat1(:,1:n1,:,:)          = tmp(:,1:n1,:,:)
         mat1(:,n1+1_i4:n1+n2,:,:) = mat2(:,1:n2,:,:)
       else if (dd == 3) then
         if ((m1 /= m2) .or. (n1 /= n2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 1, 2, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         allocate(tmp(m1,n1,j1,i1))
         tmp=mat1
         deallocate(mat1)

         allocate(mat1(m1,n1,j1 + j2,i1))
         mat1(:,:,1:j1,:) = tmp(:,:,1:j1,:)
         mat1(:,:,j1+1_i4:j1+j2,:) = mat2(:,:,1:j2,:)
       else if (dd == 4) then
         if ((m1 /= m2) .or. (n1 /= n2) .or. (j1 /= j2)) then
           print*, 'append: size mismatch: dim 1, 2, and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1,j1,i1 + i2))
         mat1(:,:,:,1:i1) = tmp(:,:,:,1:i1)
         mat1(:,:,:,i1+1_i4:i1+i2) = mat2(:,:,:,1:i2)
       end if

    else

       allocate(mat1(m2,n2,j2,i2))
       mat1 = mat2

    end if

  END SUBROUTINE append_dp_4d

  SUBROUTINE append_char_v_s(vec1, sca2)

    implicit none

    character(len=*), dimension(:), allocatable, intent(inout)   :: vec1
    character(len=*),                            intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: n1, n2
    character(len(vec1)), dimension(:), allocatable :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save mat1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_char_v_s

  SUBROUTINE append_char_v_v(vec1, vec2)

    character(len=*), dimension(:), allocatable, intent(inout)   :: vec1
    character(len=*), dimension(:),              intent(in)      :: vec2

    ! local variables
    integer(i4)                               :: n1, n2
    character(len(vec1)), dimension(:), allocatable :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save mat1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_char_v_v

  SUBROUTINE append_char_m_m(mat1, mat2, fill_value)

    implicit none

    character(len=*), dimension(:,:), allocatable, intent(inout)   :: mat1
    character(len=*), dimension(:,:),              intent(in)      :: mat2
    character(len=*), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                                 :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                                 :: n1, n2    ! dim2 of matrixes: columns
    character(len(mat1)), dimension(:,:), allocatable :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns

       if ((n1 /= n2) .and. .not. present(fill_value) ) then
          print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       if ( n1 == n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
       end if

       if ( n1 > n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)                = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
          mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
       end if

       if ( n1 < n2 ) then
          allocate(mat1(m1+m2,n2))
          mat1(      1:m1,      1:n1) = tmp(1:m1,:)
          mat1(      1:m1,   n1+1:n2) = fill_value
          mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
       end if

   else
       n1 = 0_i4

       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_char_m_m

  SUBROUTINE append_char_3d(mat1, mat2, fill_value)

    implicit none

    character(len=*), dimension(:,:,:), allocatable, intent(inout) :: mat1
    character(len=*), dimension(:,:,:), intent(in)                 :: mat2
    character(len=*), optional,         intent(in)                 :: fill_value

    ! local variables
    integer(i4)                                :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                                :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                                :: j1, j2    ! dim3 of matrixes: something else
    character(len(mat1)), dimension(:,:,:), allocatable :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_i8_3d'

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else

       if ((n1 /= n2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
       end if

       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1+m2,n1,j1))
       mat1(1:m1,:,:)          = tmp(1:m1,:,:)
       mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)

    else

       allocate(mat1(m2,n2,j2))
       mat1 = mat2

    end if

  END SUBROUTINE append_char_3d

  SUBROUTINE append_lgt_v_s(vec1, sca2)

    implicit none

    logical, dimension(:), allocatable, intent(inout)   :: vec1
    logical,                            intent(in)      :: sca2

    ! local variables
    integer(i4)                             :: n1, n2
    logical, dimension(:), allocatable  :: tmp

    n2 = 1_i4

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save mat1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4)       = sca2
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(1_i4) = sca2
    end if

  END SUBROUTINE append_lgt_v_s

  SUBROUTINE append_lgt_v_v(vec1, vec2)

    implicit none

    logical, dimension(:), allocatable, intent(inout)   :: vec1
    logical, dimension(:), intent(in)                   :: vec2

    ! local variables
    integer(i4)                             :: n1, n2    ! length of vectors
    logical, dimension(:), allocatable  :: tmp

    n2 = size(vec2)

    if (allocated(vec1)) then
       n1 = size(vec1)
       ! save mat1
       call move_alloc(vec1, tmp)

       allocate(vec1(n1+n2))
       vec1(1:n1)          = tmp(1:n1)
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    else
       n1 = 0_i4

       allocate(vec1(n2))
       vec1(n1+1_i4:n1+n2) = vec2(1:n2)
    end if

  END SUBROUTINE append_lgt_v_v

  SUBROUTINE append_lgt_m_m(mat1, mat2, fill_value, idim)

    implicit none

    logical, dimension(:,:), allocatable, intent(inout)   :: mat1
    logical, dimension(:,:), intent(in)                   :: mat2
    logical, optional,       intent(in)                   :: fill_value
    integer(i4), optional,   intent(in)                   :: idim

    ! local variables
    integer(i4)                           :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                           :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                           :: dd
    logical, dimension(:,:), allocatable  :: tmp

    dd = 1
    if (present(idim)) dd = idim
    if (dd > 3) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 3'
      STOP
    end if

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)    ! columns

    if (allocated(mat1)) then
      m1 = size(mat1,1)   ! rows
      n1 = size(mat1,2)   ! columns

      if (present(idim)) then
        if (dd == 1) then
          if (n1 /= n2) then
            print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
            STOP
          end if

         ! save mat1
         call move_alloc(mat1, tmp)

          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)

        else if (dd == 2) then

          if (m1 /= m2) then
            print*, 'append: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
            STOP
          end if

         ! save mat1
         call move_alloc(mat1, tmp)

          allocate(mat1(m1,n1 + n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)

        end if

      else
        if ( (n1 /= n2) .and. .not. present(fill_value) ) then
          print*, 'append: columns of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
        end if

       ! save mat1
       call move_alloc(mat1, tmp)

        if ( n1 == n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)          = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,:) = mat2(1:m2,:)
        end if

        if ( n1 > n2 ) then
          allocate(mat1(m1+m2,n1))
          mat1(1:m1,:)                = tmp(1:m1,:)
          mat1(m1+1_i4:m1+m2,   1:n2) = mat2(1:m2,:)
          mat1(m1+1_i4:m1+m2,n2+1:n1) = fill_value
        end if

        if ( n1 < n2 ) then
          allocate(mat1(m1+m2,n2))
          mat1(      1:m1,      1:n1) = tmp(1:m1,:)
          mat1(      1:m1,   n1+1:n2) = fill_value
          mat1(m1+1_i4:m1+m2,    :  ) = mat2(1:m2,:)
        end if
      end if

    else
       n1 = 0_i4

       allocate(mat1(m2,n2))
       mat1 = mat2
    end if

  END SUBROUTINE append_lgt_m_m

  SUBROUTINE append_lgt_3d(mat1, mat2, fill_value, idim)

    implicit none

    logical, dimension(:,:,:), allocatable, intent(inout) :: mat1
    logical, dimension(:,:,:), intent(in)                 :: mat2
    logical, optional,         intent(in)                 :: fill_value
    integer(i4), optional                                 :: idim

    ! local variables
    integer(i4)                                :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)                                :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)                                :: j1, j2    ! dim3 of matrixes: something else
    integer(i4)                                :: dd        ! index of dimension to append
    logical, dimension(:,:,:), allocatable :: tmp

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_i8_3d'

    dd = 1
    if (present(idim)) dd = idim
    if (dd > 3) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 3'
      STOP
    end if

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else

    if (allocated(mat1)) then
      m1 = size(mat1,1)    ! rows
      n1 = size(mat1,2)    ! columns
      j1 = size(mat1,3)    ! something else

      if (dd == 1) then
        if ((n1 /= n2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 2 and 3 of matrix1 and matrix2 are unequal : ' &
              // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
        end if

        ! save mat1
        call move_alloc(mat1, tmp)

        allocate(mat1(m1+m2,n1,j1))
        mat1(1:m1,:,:)          = tmp(1:m1,:,:)
        mat1(m1+1_i4:m1+m2,:,:) = mat2(1:m2,:,:)
      else if (dd == 2) then
        if ((m1 /= m2) .or. (j1 /= j2) ) then
          print*, 'append: size mismatch: dim 1 and 3 of matrix1 and matrix2 are unequal : ' &
              // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
        end if

        ! save mat1
        call move_alloc(mat1, tmp)

        allocate(mat1(m1,n1 + n2,j1))
        mat1(:,1:n1,:)          = tmp(:,1:n1,:)
        mat1(:,n1+1_i4:n1+n2,:) = mat2(:,1:n2,:)
      else if (dd == 3) then
        if ((m1 /= m2) .or. (n1 /= n2) ) then
          print*, 'append: size mismatch: dim 1 and 2 of matrix1 and matrix2 are unequal : ' &
              // '(',m1,',',n1,',',j1,')  and  (',m2,',',n2,',',j2,')'
          STOP
        end if

        ! save mat1
        call move_alloc(mat1, tmp)

        allocate(mat1(m1,n1,j1 + j2))
        mat1(:,:,1:j1) = tmp(:,:,1:j1)
        mat1(:,:,j1+1_i4:j1+j2) = mat2(:,:,1:j2)
      end if

    else

      allocate(mat1(m2,n2,j2))
      mat1 = mat2

    end if

  END SUBROUTINE append_lgt_3d

  SUBROUTINE append_lgt_4d(mat1, mat2, fill_value, idim)

    implicit none

    logical, dimension(:,:,:,:), allocatable, intent(inout) :: mat1
    logical, dimension(:,:,:,:), intent(in)                 :: mat2
    logical, optional,         intent(in)                   :: fill_value
    integer(i4), optional,      intent(in)                  :: idim

    ! local variables
    integer(i4)          :: m1, m2    ! dim1 of matrixes: rows
    integer(i4)          :: n1, n2    ! dim2 of matrixes: columns
    integer(i4)          :: j1, j2    ! dim3 of matrixes: something else
    integer(i4)          :: i1, i2    ! dim4 of matrixes: something else
    integer(i4)          :: dd        ! dimension it is appended along
    logical, allocatable :: tmp(:,:,:,:)

    if (present(fill_value)) print*, '***warning: fill_value is ignored in append_lgt_4d'

    dd = 1_i4
    if (present(idim)) dd = idim
    if (dd > 4) then
      print*, 'append: dd is : (',dd,')  and greater than number of dimensions : 4'
      STOP
    end if

    m2 = size(mat2,1)    ! rows
    n2 = size(mat2,2)    ! columns
    j2 = size(mat2,3)    ! something else
    i2 = size(mat2,4)    ! something else

    if (allocated(mat1)) then
       m1 = size(mat1,1)    ! rows
       n1 = size(mat1,2)    ! columns
       j1 = size(mat1,3)    ! something else
       i1 = size(mat1,4)    ! something else

       if (dd == 1) then
         if ((n1 /= n2) .or. (j1 /= j2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 2, 3, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1+m2,n1,j1,i1))
         mat1(1:m1,:,:,:)          = tmp(1:m1,:,:,:)
         mat1(m1+1_i4:m1+m2,:,:,:) = mat2(1:m2,:,:,:)
       else if (dd == 2) then
         if ((m1 /= m2) .or. (j1 /= j2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 1, 3, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1 + n2,j1,i1))
         mat1(:,1:n1,:,:)          = tmp(:,1:n1,:,:)
         mat1(:,n1+1_i4:n1+n2,:,:) = mat2(:,1:n2,:,:)
       else if (dd == 3) then
         if ((m1 /= m2) .or. (n1 /= n2) .or. (i1 /= i2)) then
           print*, 'append: size mismatch: dim 1, 2, and 4 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1,j1 + j2,i1))
         mat1(:,:,1:j1,:) = tmp(:,:,1:j1,:)
         mat1(:,:,j1+1_i4:j1+j2,:) = mat2(:,:,1:j2,:)
       else if (dd == 4) then
         if ((m1 /= m2) .or. (n1 /= n2) .or. (j1 /= j2)) then
           print*, 'append: size mismatch: dim 1, 2, and 3 of matrix1 and matrix2 are unequal : ' &
               // '(',m1,',',n1,',',j1,',',i1,')  and  (',m2,',',n2,',',j2,',',i2,')'
           STOP
         end if

         ! save mat1
         call move_alloc(mat1, tmp)

         allocate(mat1(m1,n1,j1,i1 + i2))
         mat1(:,:,:,1:i1) = tmp(:,:,:,1:i1)
         mat1(:,:,:,i1+1_i4:i1+i2) = mat2(:,:,:,1:i2)
       end if

    else

       allocate(mat1(m2,n2,j2,i2))
       mat1 = mat2

    end if

  END SUBROUTINE append_lgt_4d

  ! ------------------------------------------------------------------

  SUBROUTINE paste_i4_m_s(mat1, sca2)

    implicit none

    integer(i4), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i4),                              intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: m1    ! dim1 of matrix
    integer(i4)                               :: n1    ! dim2 of matrix
    integer(i4), dimension(:,:), allocatable  :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_i4_m_s

  SUBROUTINE paste_i4_m_v(mat1, vec2, fill_value)

    implicit none

    integer(i4), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i4), dimension(:),                intent(in)      :: vec2
    integer(i4), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4), dimension(:,:), allocatable  :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(1:m1,1:n1)       = tmp(:,1:n1)
          mat1(1:m2,n1+n2)      = vec2(1:m2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
          mat1(m2+1:m1,n1+n2)   = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(m1+1:m2,1:n1)    = fill_value
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_i4_m_v

  SUBROUTINE paste_i4_m_m(mat1, mat2, fill_value)

    implicit none

    integer(i4), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i4), dimension(:,:),              intent(in)      :: mat2
    integer(i4), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(    :  ,1:n1)          = tmp(:,1:n1)
          mat1(   1:m2,n1+1_i4:n1+n2) = mat2(:,1:n2)
          mat1(m2+1:m1,n1+1_i4:n1+n2) = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,      1:n1   ) = tmp(:,1:n1)
          mat1(m1+1:m2,      1:n1   ) = fill_value
          mat1(    :  ,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_i4_m_m

  SUBROUTINE paste_i8_m_s(mat1, sca2)

    implicit none

    integer(i8), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i8),                              intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: m1    ! dim1 of matrix
    integer(i4)                               :: n1    ! dim2 of matrix
    integer(i8), dimension(:,:), allocatable  :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_i8_m_s

  SUBROUTINE paste_i8_m_v(mat1, vec2, fill_value)

    implicit none

    integer(i8), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i8), dimension(:),                intent(in)      :: vec2
    integer(i8), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                               :: m1, m2    ! dim1 of matrixes
    integer(i4)                               :: n1, n2    ! dim2 of matrixes
    integer(i8), dimension(:,:), allocatable  :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(1:m1,1:n1)       = tmp(:,1:n1)
          mat1(1:m2,n1+n2)      = vec2(1:m2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
          mat1(m2+1:m1,n1+n2)   = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(m1+1:m2,1:n1)    = fill_value
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_i8_m_v

  SUBROUTINE paste_i8_m_m(mat1, mat2, fill_value)

    implicit none

    integer(i8), dimension(:,:), allocatable, intent(inout)   :: mat1
    integer(i8), dimension(:,:),              intent(in)      :: mat2
    integer(i8), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i8), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(    :  ,1:n1)          = tmp(:,1:n1)
          mat1(   1:m2,n1+1_i4:n1+n2) = mat2(:,1:n2)
          mat1(m2+1:m1,n1+1_i4:n1+n2) = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,      1:n1   ) = tmp(:,1:n1)
          mat1(m1+1:m2,      1:n1   ) = fill_value
          mat1(    :  ,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

   else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_i8_m_m

  SUBROUTINE paste_sp_m_s(mat1, sca2)

    implicit none

    real(sp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(sp),                              intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: m1    ! dim1 of matrix
    integer(i4)                               :: n1    ! dim2 of matrix
    real(sp), dimension(:,:), allocatable  :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_sp_m_s

  SUBROUTINE paste_sp_m_v(mat1, vec2, fill_value)

    implicit none

    real(sp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(sp), dimension(:),                intent(in)      :: vec2
    real(sp), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    real(sp), dimension(:,:), allocatable  :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(1:m1,1:n1)       = tmp(:,1:n1)
          mat1(1:m2,n1+n2)      = vec2(1:m2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
          mat1(m2+1:m1,n1+n2)   = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(m1+1:m2,1:n1)    = fill_value
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_sp_m_v

  SUBROUTINE paste_sp_m_m(mat1, mat2, fill_value)

    implicit none

    real(sp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(sp), dimension(:,:),              intent(in)      :: mat2
    real(sp), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    real(sp), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(    :  ,1:n1)          = tmp(:,1:n1)
          mat1(   1:m2,n1+1_i4:n1+n2) = mat2(:,1:n2)
          mat1(m2+1:m1,n1+1_i4:n1+n2) = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,      1:n1   ) = tmp(:,1:n1)
          mat1(m1+1:m2,      1:n1   ) = fill_value
          mat1(    :  ,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if


    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_sp_m_m

  SUBROUTINE paste_dp_m_s(mat1, sca2)

    implicit none

    real(dp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(dp),                              intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: m1    ! dim1 of matrix
    integer(i4)                               :: n1    ! dim2 of matrix
    real(dp), dimension(:,:), allocatable  :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_dp_m_s

  SUBROUTINE paste_dp_m_v(mat1, vec2, fill_value)

    implicit none

    real(dp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(dp), dimension(:),                intent(in)      :: vec2
    real(dp), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    real(dp), dimension(:,:), allocatable  :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(1:m1,1:n1)       = tmp(:,1:n1)
          mat1(1:m2,n1+n2)      = vec2(1:m2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
          mat1(m2+1:m1,n1+n2)   = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(m1+1:m2,1:n1)    = fill_value
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_dp_m_v

  SUBROUTINE paste_dp_m_m(mat1, mat2, fill_value)

    implicit none

    real(dp), dimension(:,:), allocatable, intent(inout)   :: mat1
    real(dp), dimension(:,:),              intent(in)      :: mat2
    real(dp), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    real(dp), dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(    :  ,1:n1)          = tmp(:,1:n1)
          mat1(   1:m2,n1+1_i4:n1+n2) = mat2(:,1:n2)
          mat1(m2+1:m1,n1+1_i4:n1+n2) = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,      1:n1   ) = tmp(:,1:n1)
          mat1(m1+1:m2,      1:n1   ) = fill_value
          mat1(    :  ,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_dp_m_m

  SUBROUTINE paste_dp_3d(mat1, mat2)

    real(dp), dimension(:,:,:), allocatable, intent(inout) :: mat1
    real(dp), dimension(:,:,:),              intent(in)    :: mat2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4)                             :: o1, o2    ! dim2 of matrixes
    real(dp), dimension(:,:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns
    o2 = size(mat2,3)   ! 3rd

    if (allocated(mat1)) then
      m1 = size(mat1,1)   ! rows
      n1 = size(mat1,2)   ! columns
      o1 = size(mat1,3)   ! columns
      if ( (m1 /= m2)) then
        print*, 'paste: rows of array1 and array2 are unequal : (',m1,')  and  (',m2,')'
        STOP
      else if ( (n1 /= n2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',n1,')  and  (',n2,')'
        STOP
      end if
      ! save mat1
      call move_alloc(mat1, tmp)

      allocate(mat1(m1,n1,o1+o2))
      mat1(:,:,1:o1)          = tmp(:,:,:)
      mat1(:,:,o1+1_i4:o1+o2) = mat2(:,:,:)

    else
       allocate(mat1(m2,n2,o2))
       mat1(:,:,:) = mat2(:,:,:)
    end if

  END SUBROUTINE paste_dp_3d

  SUBROUTINE paste_dp_4d(mat1, mat2)

    real(dp), dimension(:,:,:,:), allocatable, intent(inout) :: mat1
    real(dp), dimension(:,:,:,:),              intent(in)    :: mat2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4)                             :: o1, o2    ! dim3 of matrixes
    integer(i4)                             :: p1, p2    ! dim4 of matrixes
    real(dp), dimension(:,:,:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns
    o2 = size(mat2,3)   ! 3rd
    p2 = size(mat2,4)   ! 4th

    if (allocated(mat1)) then
      m1 = size(mat1,1)   ! rows
      n1 = size(mat1,2)   ! columns
      o1 = size(mat1,3)   ! 3rd
      p1 = size(mat1,4)   ! 4th
      if ( (m1 /= m2)) then
        print*, 'paste: rows of array1 and array2 are unequal : (',m1,')  and  (',m2,')'
        STOP
      else if ( (n1 /= n2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',n1,')  and  (',n2,')'
        STOP
      else if ( (o1 /= o2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',o1,')  and  (',o2,')'
        STOP
      end if
      ! save mat1
      call move_alloc(mat1, tmp)

      allocate(mat1(m1,n1,o1,p1+p2))
      mat1(:,:,:,1:p1)          = tmp(:,:,:,:)
      mat1(:,:,:,p1+1_i4:p1+p2) = mat2(:,:,:,:)

    else
       allocate(mat1(m2,n2,o2,p2))
       mat1(:,:,:,:) = mat2(:,:,:,:)
    end if

  END SUBROUTINE paste_dp_4d

  SUBROUTINE paste_i4_3d(mat1, mat2)

    integer(i4), dimension(:,:,:), allocatable, intent(inout) :: mat1
    integer(i4), dimension(:,:,:),              intent(in)    :: mat2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4)                             :: o1, o2    ! dim2 of matrixes
    integer(i4), dimension(:,:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns
    o2 = size(mat2,3)   ! 3rd

    if (allocated(mat1)) then
      m1 = size(mat1,1)   ! rows
      n1 = size(mat1,2)   ! columns
      o1 = size(mat1,3)   ! columns
      if ( (m1 /= m2)) then
        print*, 'paste: rows of array1 and array2 are unequal : (',m1,')  and  (',m2,')'
        STOP
      else if ( (n1 /= n2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',n1,')  and  (',n2,')'
        STOP
      end if
      ! save mat1
      call move_alloc(mat1, tmp)

      allocate(mat1(m1,n1,o1+o2))
      mat1(:,:,1:o1)          = tmp(:,:,:)
      mat1(:,:,o1+1_i4:o1+o2) = mat2(:,:,:)

    else
       allocate(mat1(m2,n2,o2))
       mat1(:,:,:) = mat2(:,:,:)
    end if

  END SUBROUTINE paste_i4_3d

  SUBROUTINE paste_i4_4d(mat1, mat2)

    integer(i4), dimension(:,:,:,:), allocatable, intent(inout) :: mat1
    integer(i4), dimension(:,:,:,:),              intent(in)    :: mat2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    integer(i4)                             :: o1, o2    ! dim3 of matrixes
    integer(i4)                             :: p1, p2    ! dim4 of matrixes
    integer(i4), dimension(:,:,:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns
    o2 = size(mat2,3)   ! 3rd
    p2 = size(mat2,4)   ! 4th

    if (allocated(mat1)) then
      m1 = size(mat1,1)   ! rows
      n1 = size(mat1,2)   ! columns
      o1 = size(mat1,3)   ! 3rd
      p1 = size(mat1,4)   ! 4th
      if ( (m1 /= m2)) then
        print*, 'paste: rows of array1 and array2 are unequal : (',m1,')  and  (',m2,')'
        STOP
      else if ( (n1 /= n2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',n1,')  and  (',n2,')'
        STOP
      else if ( (o1 /= o2)) then
        print*, 'paste: columns of array1 and array2 are unequal : (',o1,')  and  (',o2,')'
        STOP
      end if
      ! save mat1
      call move_alloc(mat1, tmp)

      allocate(mat1(m1,n1,o1,p1+p2))
      mat1(:,:,:,1:p1)          = tmp(:,:,:,:)
      mat1(:,:,:,p1+1_i4:p1+p2) = mat2(:,:,:,:)

    else
       allocate(mat1(m2,n2,o2,p2))
       mat1(:,:,:,:) = mat2(:,:,:,:)
    end if

  END SUBROUTINE paste_i4_4d

  SUBROUTINE paste_char_m_s(mat1, sca2)

    implicit none

    character(len=*), dimension(:,:), allocatable, intent(inout)   :: mat1
    character(len=*),                              intent(in)      :: sca2

    ! local variables
    integer(i4)                                  :: m1    ! dim1 of matrix
    integer(i4)                                  :: n1    ! dim2 of matrix
    character(len(mat1)), dimension(:,:), allocatable :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_char_m_s

  SUBROUTINE paste_char_m_v(mat1, vec2, fill_value)

    implicit none

    character(len=*), dimension(:,:), allocatable, intent(inout)   :: mat1
    character(len=*), dimension(:),                intent(in)      :: vec2
    character(len=*), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                                  :: m1, m2    ! dim1 of matrixes
    integer(i4)                                  :: n1, n2    ! dim2 of matrixes
    character(len(mat1)), dimension(:,:), allocatable :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(1:m1,1:n1)       = tmp(:,1:n1)
          mat1(1:m2,n1+n2)      = vec2(1:m2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
          mat1(m2+1:m1,n1+n2)   = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,1:n1)    = tmp(:,1:n1)
          mat1(m1+1:m2,1:n1)    = fill_value
          mat1(   1:m2,n1+n2)   = vec2(1:m2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_char_m_v

  SUBROUTINE paste_char_m_m(mat1, mat2, fill_value)

    implicit none

    character(len=*), dimension(:,:), allocatable, intent(inout)   :: mat1
    character(len=*), dimension(:,:),              intent(in)      :: mat2
    character(len=*), optional,                    intent(in)      :: fill_value

    ! local variables
    integer(i4)                                  :: m1, m2    ! dim1 of matrixes
    integer(i4)                                  :: n1, n2    ! dim2 of matrixes
    character(len(mat1)), dimension(:,:), allocatable :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if ( (m1 /= m2) .and. .not. present( fill_value ) ) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       if ( m1 == m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(:,1:n1)          = tmp(:,1:n1)
          mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

       if ( m1 > m2 ) then
          allocate(mat1(m1,n1+n2))
          mat1(    :  ,1:n1)          = tmp(:,1:n1)
          mat1(   1:m2,n1+1_i4:n1+n2) = mat2(:,1:n2)
          mat1(m2+1:m1,n1+1_i4:n1+n2) = fill_value
       end if

       if ( m1 < m2 ) then
          allocate(mat1(m2,n1+n2))
          mat1(   1:m1,      1:n1   ) = tmp(:,1:n1)
          mat1(m1+1:m2,      1:n1   ) = fill_value
          mat1(    :  ,n1+1_i4:n1+n2) = mat2(:,1:n2)
       end if

    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_char_m_m

  SUBROUTINE paste_lgt_m_s(mat1, sca2)

    implicit none

    logical, dimension(:,:), allocatable, intent(inout)   :: mat1
    logical,                              intent(in)      :: sca2

    ! local variables
    integer(i4)                               :: m1    ! dim1 of matrix
    integer(i4)                               :: n1    ! dim2 of matrix
    logical, dimension(:,:), allocatable  :: tmp

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= 1_i4) then
          print*, 'paste: scalar paste to matrix only works with one-line matrix'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(1_i4,n1+1_i4))
       mat1(1,1:n1)          = tmp(1,1:n1)
       mat1(1,n1+1_i4)       = sca2
    else
       allocate(mat1(1_i4,1_i4))
       mat1(1,1) = sca2
    end if

  END SUBROUTINE paste_lgt_m_s

  SUBROUTINE paste_lgt_m_v(mat1, vec2)

    implicit none

    logical, dimension(:,:), allocatable, intent(inout)   :: mat1
    logical, dimension(:),                intent(in)      :: vec2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    logical, dimension(:,:), allocatable  :: tmp

    m2 = size(vec2,1)   ! rows
    n2 = 1_i4           ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= m2) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1,n1+n2))
       mat1(:,1:n1)          = tmp(:,1:n1)
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(1:m2,n1+n2)      = vec2(1:m2)
    end if

  END SUBROUTINE paste_lgt_m_v

  SUBROUTINE paste_lgt_m_m(mat1, mat2)

    implicit none

    logical, dimension(:,:), allocatable, intent(inout)   :: mat1
    logical, dimension(:,:),              intent(in)      :: mat2

    ! local variables
    integer(i4)                             :: m1, m2    ! dim1 of matrixes
    integer(i4)                             :: n1, n2    ! dim2 of matrixes
    logical, dimension(:,:), allocatable  :: tmp

    m2 = size(mat2,1)   ! rows
    n2 = size(mat2,2)   ! columns

    if (allocated(mat1)) then
       m1 = size(mat1,1)   ! rows
       n1 = size(mat1,2)   ! columns
       if (m1 /= m2) then
          print*, 'paste: rows of matrix1 and matrix2 are unequal : (',m1,',',n1,')  and  (',m2,',',n2,')'
          STOP
       end if
       ! save mat1
       call move_alloc(mat1, tmp)

       allocate(mat1(m1,n1+n2))
       mat1(:,1:n1)          = tmp(:,1:n1)
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    else
       n1 = 0_i4
       m1 = m2

       allocate(mat1(m2,n2))
       mat1(:,n1+1_i4:n1+n2) = mat2(:,1:n2)
    end if

  END SUBROUTINE paste_lgt_m_m

  subroutine add_nodata_slice_dp_2d(array, nAdd, noDataValue)
    real(dp), dimension(:, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    real(dp), intent(in) :: noDataValue

    real(dp), dimension(size(array, 1), nAdd) :: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_dp_2d

  subroutine add_nodata_slice_dp_3d(array, nAdd, noDataValue)
    real(dp), dimension(:, :, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    real(dp), intent(in) :: noDataValue

    real(dp), dimension(size(array, 1), size(array, 2), nAdd) :: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_dp_3d

  subroutine add_nodata_slice_dp_4d(array, nAdd, noDataValue)
    real(dp), dimension(:, :, :, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    real(dp), intent(in) :: noDataValue

    real(dp), dimension(size(array, 1), size(array, 2), size(array, 3), nAdd):: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_dp_4d

  subroutine add_nodata_slice_i4_2d(array, nAdd, noDataValue)
    integer(i4), dimension(:, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    integer(i4), intent(in) :: noDataValue

    integer(i4), dimension(size(array, 1), nAdd) :: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_i4_2d

  subroutine add_nodata_slice_i4_3d(array, nAdd, noDataValue)
    integer(i4), dimension(:, :, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    integer(i4), intent(in) :: noDataValue

    integer(i4), dimension(size(array, 1), size(array, 2), nAdd) :: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_i4_3d

  subroutine add_nodata_slice_i4_4d(array, nAdd, noDataValue)
    integer(i4), dimension(:, :, :, :), intent(inout), allocatable :: array
    integer(i4), intent(in) :: nAdd
    integer(i4), intent(in) :: noDataValue

    integer(i4), dimension(size(array, 1), size(array, 2), size(array, 3), nAdd):: dummy

    if (nAdd > 0_i4) then
      dummy = noDataValue
      call paste(array, dummy)
    end if

  end subroutine add_nodata_slice_i4_4d

END MODULE mo_append
