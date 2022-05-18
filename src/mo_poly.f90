!>    \file mo_poly.f90
!>    \copydoc mo_poly

!>    \brief  polygon calculations.
!>    \details
!!    this module determines some topological functions working with polygons,
!!    and is part of the UFZ CHS Fortran library.
!>    \author juliane mai
!>    \date jul 2012



module mo_poly

  ! this module determines wether a 2D point lies inside, outside, or
  ! on the vertice/edge of a 2D polygon
  ! and is part of the UFZ CHS Fortran library.
  !
  ! written  juliane mai, july 2012
  ! modified maren goehler, july 2012 - area & center of mass

  ! license
  ! -------
  ! this file is part of the UFZ Fortran library.

  ! the UFZ Fortran library is free software: you can redistribute it and/or modify
  ! it under the terms of the GNU lesser general public license as published by
  ! the free software foundation, either version 3 of the license, or
  ! (at your option) any later version.

  ! the UFZ Fortran library is distributed in the hope that it will be useful,
  ! but without any warranty; without even the implied warranty of
  ! merchantability or fitness for a particular purpose. see the
  ! GNU lesser general public license for more details.

  ! you should have received a copy of the GNU lesser general public license
  ! along with the UFZ Fortran library (license).
  ! if not, see <http://www.GNU.org/licenses/>.

  ! copyright 2012 juliane mai


  use mo_kind, only: i4, sp, dp
  use mo_utils, only: eq, ge, le, ne

  implicit none

  public :: areapoly

  interface areapoly
    module procedure areapoly_sp
    module procedure areapoly_dp
  end interface

  public :: center_of_mass

  interface center_of_mass
    module procedure center_of_mass_sp
    module procedure center_of_mass_dp
  end interface

  public :: inpoly

  interface inpoly
    module procedure inpoly_sp
    module procedure inpoly_dp
  end interface

  public :: orientpoly

  interface orientpoly
    module procedure orientpoly_sp
    module procedure orientpoly_dp
  end interface

  public :: mod_pole

  interface mod_pole
    module procedure mod_pole_sp
    module procedure mod_pole_dp
  end interface

  public :: mod_shift

  interface mod_shift
    module procedure mod_shift_sp
    module procedure mod_shift_dp
  end interface


  ! ------------------------------------------------------------------

  private

  ! ------------------------------------------------------------------

contains

  ! ------------------------------------------------------------------

  !>    \brief   Area of polygon
  !>    \details Function for computing the area of a polygon (2D, convex or not).
  !>    \return  Area of polygon
  function areapoly_sp(coord) result(areapoly)
    !> coordinates of the polygon in question
    real(sp), dimension(:,:),     intent(in)     :: coord
    real(sp)                                     :: areapoly

    ! local variables
    integer(i4)                                  :: i,k          ! loop
    integer(i4)                                  :: nedges       ! number of coordinates
    real(sp)                                     :: xsum         ! for summing up
    real(sp)                                     :: ysum         ! for summing up

    xsum   = 0.0_sp
    ysum   = 0.0_sp
    nedges = size(coord,1)

    do i = 1,  nedges
       if (i == nedges) then
          k = 1_i4
       else
          k = i + 1_i4
       end if
       xsum = xsum + ( coord(i,1) * coord(k,2) )
       ysum = ysum + ( coord(i,2) * coord(k,1) )
    end do

    areapoly = 0.5_sp * (xsum - ysum)

  end function areapoly_sp

  ! ------------------------------------------------------------------
  !>    \brief    Center of mass of polygon.
  !>    \details  Function for computing the center of mass of a polygon (2D, convex or not).
  !>    \return   Center of mass of polygon.
  function center_of_mass_sp(coord) result(center_of_mass)
    !> coordinates of polygon in question
    real(sp), dimension(:,:),   intent(in)       :: coord
    real(sp), dimension(2)                       :: center_of_mass

    ! local variables
    integer(i4)                                 :: i,k       ! loop
    integer(i4)                                 :: nedges    ! number of coordinates
    real(sp)                                    :: area      ! area of the polygon
    real(sp)                                    :: xsum      ! for summing up
    real(sp)                                    :: ysum      ! for summing up

    xsum   = 0.0_sp
    ysum   = 0.0_sp
    nedges = size(coord,1)

    area = areapoly_sp(coord)

    do i = 1, nedges
       if (i == nedges ) then
          k = 1_i4
       else
          k = i + 1_i4
       end if
       ! multiply x coord by the y coord of next vertex
       xsum = xsum + ((coord(i,1) + coord(k,1)) * &
            ((coord(i,1) * coord(k,2) - coord(k,1) * coord(i,2))))

       ysum = ysum + ((coord(i,2) + coord(k,2)) * &
            ((coord(i,1) * coord(k,2) - coord(k,1) * coord(i,2))))
    end do

    center_of_mass(1) = 1.0_sp / (6.0_sp * area) * xsum
    center_of_mass(2) = 1.0_sp / (6.0_sp * area) * ysum

  end function center_of_mass_sp

  ! ------------------------------------------------------------------
  !>    \brief   Determination point of polygon.
  !>    \details Determines whether a 2D point is inside, outside or on vertex of a polygon (2D, convex or not).
  !>    \return  Whether point is inside (=1), outside (=-1) or on a vertex/edge of the polygon (=0)
  subroutine inpoly_sp(p,coord,erg)
    !> point in question
    real(sp),   dimension(2),   intent(in)   :: p
    !> coordinates of the polygon
    real(sp),   dimension(:, :), intent(in)  :: coord
    !> result:
    !!     inside:         erg =  1
    !!     outside:        erg = -1
    !!     on vertex/edge: erg =  0
    integer(i4),                intent(out)  :: erg

    ! local variables
    real(sp), dimension(size(coord,1))    :: x, y
    real(sp)                              :: lx, ly
    logical                               :: mx,my,nx,ny, test1, test2
    integer(i4)                           :: n, i, j

    n  = size(coord,1)

    do i=1,n
       x(i)=coord(i,1)-p(1)
       y(i)=coord(i,2)-p(2)
       ! check if point is equal to any coord
       if ( eq(x(i),0.0_sp) .and. eq(y(i),0.0_sp) ) then
          erg=0_i4
          return
       end if
    end do

    erg=-1_i4

    do i=1,n
       j=1+mod(i,n)
       ! vertical vertex
       if ( eq(coord(i,1),coord(j,1)) .and. eq(coord(i,1),p(1)) ) then
          ly = (p(2)-coord(j,2)) / (coord(i,2)-coord(j,2))
          if ( ge(ly,0.0_sp) .and. le(ly,1.0_sp) ) then
             erg=0_i4
             return
          end if
       end if
       ! horizontal vertex
       if ( eq(coord(i,2),coord(j,2)) .and. eq(coord(i,2),p(2)) ) then
          lx = (p(1)-coord(j,1)) / (coord(i,1)-coord(j,1))
          if ( ge(lx,0.0_sp ) .and. le(lx,1.0_sp) ) then
             erg=0_i4
             return
          end if
       end if
       !
       mx = ge(x(i),0.0_sp)
       nx = ge(x(j),0.0_sp)
       my = ge(y(i),0.0_sp)
       ny = ge(y(j),0.0_sp)

       test1 = .not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)
       test2 = .not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))

       if (.not. test1) then
          if (test2) then
             if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i)) < 0.0_sp) then
                cycle
             else
                if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i)) > 0.0_sp) then
                   erg = -erg
                   cycle
                else
                   erg = 0_i4
                   return
                end if
             end if
          else
             erg=-erg
          end if
       end if

    end do

  end subroutine inpoly_sp

  ! ------------------------------------------------------------------
  !>    \brief   Check orientation of polygon
  !>    \details Function for checking the orientation of a polygon (2D, convex or not).
  !>    \return  Boolean indicating orientation (counter-clockwise: .true., clockwise: .false.)
  function orientpoly_sp(coord) result(orientpoly)
    !> coordinates of the polygon in question
    real(sp), dimension(:,:),     intent(in)     :: coord
    integer(i4) :: orientpoly
    !> result:
    !!     inside:         erg =  1
    !!     outside:        erg = -1
    !!     on vertex/edge: erg =  0

    ! local variables
    integer(i4) :: n
    real(sp) :: sum_edges

    ! calculate sum over the edges, (x2 − x1)(y2 + y1) as in
    ! https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
    n = size(coord, 1)
    ! use a vectorized version of sum over all (x2 -x1)*(y2-y1)
    sum_edges = sum((coord(2:n, 1) - coord(1:n-1, 1)) * (coord(2:n, 2) + coord(1:n-1, 2)))
    sum_edges = sum_edges + (coord(1, 1) - coord(n, 1)) * (coord(1, 2) + coord(n, 2))
    if (eq(sum_edges, 0._sp)) then
      orientpoly = 0_i4
    else if (sum_edges < 0._sp) then
      orientpoly = -1_i4
    else
      orientpoly = 1_i4
    end if

  end function orientpoly_sp

  ! ------------------------------------------------------------------
  !>    \brief   Modify polygon so it covers pole correctly
  !>    \details Modifies a polygon (2D, convex or not) to include pole when passed to inpoly
  !!  this function is intended to modify a given polygon, so it can be represented in a Cartesian grid
  !!  the use case is a polygon (e.g. 120,80, -120,80, 0,80 covering the north pole) that is not represented on
  !!  Cartesian lat-lon grid as polygon.
  !!  The script inserts additional coordinates, so the pole is covered (e.g. 180,80, 180,90, -180,90, -180,80)
  !!  See test cases for examples.
  !>    \return  modified coordinates
  function mod_pole_sp(coord, meridian_arg) result(coord_mod)
    !> coordinates of the polygon in question
    real(sp), dimension(:,:), intent(in)     :: coord
    !> meridian that represents discontinuity, defaults to 180.0
    real(sp), intent(in), optional :: meridian_arg
    real(sp), dimension(:,:), allocatable :: coord_mod

    ! local variables
    real(sp) :: meridian
    real(sp) :: break, a
    integer(i4) :: i, j, k, n

    if (present(meridian_arg)) then
      meridian = meridian_arg
    else
      meridian = 180._sp
    end if

    n = size(coord, 1)
    ! determine location where meridian is crossed
    ! find the maximum and minimum longitudes
    i = maxloc(coord(:, 1), 1)
    j = minloc(coord(:, 1), 1)
    ! determine size of new coords array
    k = n + 2
    if (ne(coord(i, 1), meridian)) then
      k = k + 1
    end if
    if (ne(coord(j, 1), meridian * (-1._sp))) then
      k = k + 1
    end if
    allocate(coord_mod(k, 2))
    ! polygon covers a pole
    if (mod(i,n)+1 == j) then
      ! the coord pair after i is j, so longitudes are ascending, so north pole is contained
      coord_mod(1:i, :) = coord(1:i, :)
      k = i
      ! if the maxval is not meridian, we need to add point at intersection of meridian and points i and j
      if (ne(coord(i, 1), meridian)) then
        a = meridian - coord(i, 1)
        break = coord(i, 2) + a / (a + abs(meridian + coord(j, 1))) * (coord(j, 2) - coord(i, 2))
        coord_mod(k+1, :) = [meridian, break]
        k = k + 1
      end if
      ! add the points meridian,90 and meridian * -1, 90
      coord_mod(k+1:k+2, 1) = [meridian, meridian * (-1._sp)]
      coord_mod(k+1:k+2, 2) = [90._sp, 90._sp]
      k = k + 2
      ! if the minval is not meridian * -1, we need to add point at intersection of meridian and points i and j
      if (ne(coord(j, 1), meridian * (-1._sp))) then
        a = meridian - coord(i, 1)
        break = coord(i, 2) + a / (a + abs(meridian + coord(j, 1))) * (coord(j, 2) - coord(i, 2))
        coord_mod(k+1, :) = [meridian * (-1._sp), break]
        k = k + 1
      end if
      ! add the remaining coordinates
      if (j > 1) coord_mod(k+1:k+1+n-j, :) = coord(j:n, :)
    else if (mod(j,n)+1 == i) then
      ! the coord pair after j is i, so longitudes are descending, so south pole is contained
      coord_mod(1:j, :) = coord(1:j, :)
      k = j
      ! if the minval is not meridian * -1, we need to add point at intersection of meridian and points i and j
      if (ne(coord(j, 1), meridian * (-1._sp))) then
        a = abs(meridian + coord(j, 1))
        break = coord(j, 2) + a / (a + meridian - coord(i, 1)) * (coord(i, 2) - coord(j, 2))
        coord_mod(k+1, :) = [meridian * (-1._sp), break]
        k = k + 1
      end if
      ! add the points meridian * -1, -90 and meridian, -90
      coord_mod(k+1:k+2, 1) = [meridian * (-1._sp), meridian]
      coord_mod(k+1:k+2, 2) = [-90._sp, -90._sp]
      k = k + 2
      ! if the maxval is not meridian, we need to add point at intersection of meridian and points i and j
      if (ne(coord(i, 1), meridian)) then
        a = abs(meridian + coord(j, 1))
        break = coord(j, 2) + a / (a + meridian - coord(i, 1)) * (coord(i, 2) - coord(j, 2))
        coord_mod(k+1, :) = [meridian, break]
        k = k + 1
      end if
      ! add the remaining coordinates
      if (i > 1) coord_mod(k+1:k+1+n-i, :) = coord(i:n, :)
    ! else: if there are multiple locations of minval or maxval, this edge case is not covered...
    end if

  end function mod_pole_sp

  ! ------------------------------------------------------------------
  !>    \brief   Shifts the (longitude) value 180 degrees
  !>    \details Modify a coordinate value
  !>    \return  Shifted value
  elemental function mod_shift_sp(x_coord, meridian_arg) result(shifted)
    !> coordinates of the polygon in question
    real(sp), intent(in) :: x_coord
    !> meridian that represents discontinuity, defaults to 180.0
    real(sp), intent(in), optional :: meridian_arg
    real(sp) :: shifted
    real(sp) :: meridian

    ! shift values
    if (present(meridian_arg)) then
      meridian = meridian_arg
    else
      meridian = 180._sp
    end if
    shifted = sign(abs(x_coord) - meridian, x_coord * (-1._sp))

  end function mod_shift_sp

  ! ------------------------------------------------------------------
  !>    \brief   Area of polygon
  !>    \details Function for computing the area of a polygon (2D, convex or not).
  !>    \return  Area of polygon
  function areapoly_dp(coord) result(areapoly)
    !> coordinates of the polygon in question
    real(dp), dimension(:,:),     intent(in)     :: coord
    real(dp)                                     :: areapoly

    ! local variables
    integer(i4)                                  :: i,k          ! loop
    integer(i4)                                  :: nedges       ! number of coordinates
    real(dp)                                     :: xsum         ! for summing up
    real(dp)                                     :: ysum         ! for summing up

    xsum   = 0.0_dp
    ysum   = 0.0_dp
    nedges = size(coord,1)

    do i = 1,  nedges
       if (i == nedges) then
          k = 1_i4
       else
          k = i + 1_i4
       end if
       xsum = xsum + ( coord(i,1) * coord(k,2) )
       ysum = ysum + ( coord(i,2) * coord(k,1) )
    end do

    areapoly = 0.5_dp * (xsum - ysum)

  end function areapoly_dp

  ! ------------------------------------------------------------------
  !>    \brief    Center of mass of polygon.
  !>    \details  Function for computing the center of mass of a polygon (2D, convex or not).
  !>    \return   Center of mass of polygon.
  function center_of_mass_dp(coord) result(center_of_mass)
    !> coordinates of polygon in question
    real(dp), dimension(:,:),   intent(in)       :: coord
    real(dp), dimension(2)                       :: center_of_mass

    ! local variables
    integer(i4)                                 :: i,k       ! loop
    integer(i4)                                 :: nedges    ! number of coordinates
    real(dp)                                    :: area      ! area of the polygon
    real(dp)                                    :: xsum      ! for summing up
    real(dp)                                    :: ysum      ! for summing up

    xsum   = 0.0_dp
    ysum   = 0.0_dp
    nedges = size(coord,1)

    area = areapoly_dp(coord)

    do i = 1, nedges
       if (i == nedges ) then
          k = 1_i4
       else
          k = i + 1_i4
       end if
       ! multiply x coord by the y coord of next vertex
       xsum = xsum + ((coord(i,1) + coord(k,1)) * &
            ((coord(i,1) * coord(k,2) - coord(k,1) * coord(i,2))))

       ysum = ysum + ((coord(i,2) + coord(k,2)) * &
            ((coord(i,1) * coord(k,2) - coord(k,1) * coord(i,2))))
    end do

    center_of_mass(1) = 1.0_dp / (6.0_dp * area) * xsum
    center_of_mass(2) = 1.0_dp / (6.0_dp * area) * ysum

  end function center_of_mass_dp

  ! ------------------------------------------------------------------
  !>    \brief   Determination point of polygon.
  !>    \details Determines whether a 2D point is inside, outside or on vertex of a polygon (2D, convex or not).
  !>    \return  Whether point is inside (=1), outside (=-1) or on a vertex/edge of the polygon (=0)
  subroutine inpoly_dp(p,coord,erg)
    !> point in question
    real(dp),   dimension(2),   intent(in)   :: p
    !> coordinates of the polygon
    real(dp),   dimension(:, :), intent(in)  :: coord
    !> result:
    !!     inside:         erg =  1
    !!     outside:        erg = -1
    !!     on vertex/edge: erg =  0
    integer(i4),                intent(out)  :: erg

    ! local variables
    real(dp), dimension(size(coord,1))    :: x, y
    real(dp)                              :: lx, ly
    logical                               :: mx,my,nx,ny, test1, test2
    integer(i4)                           :: n, i, j

    n  = size(coord,1)

    do i=1,n
       x(i)=coord(i,1)-p(1)
       y(i)=coord(i,2)-p(2)
       ! check if point is equal to any coord
       if ( eq(x(i),0.0_dp) .and. eq(y(i),0.0_dp) ) then
          erg=0_i4
          return
       end if
    end do

    erg=-1_i4

    do i=1,n
       j=1+mod(i,n)
       ! vertical vertex
       if ( eq(coord(i,1),coord(j,1)) .and. eq(coord(i,1),p(1)) ) then
          ly = (p(2)-coord(j,2)) / (coord(i,2)-coord(j,2))
          if ( ge(ly,0.0_dp) .and. le(ly,1.0_dp) ) then
             erg=0_i4
             return
          end if
       end if
       ! horizontal vertex
       if ( eq(coord(i,2),coord(j,2)) .and. eq(coord(i,2),p(2)) ) then
          lx = (p(1)-coord(j,1)) / (coord(i,1)-coord(j,1))
          if ( ge(lx,0.0_dp ) .and. le(lx,1.0_dp) ) then
             erg=0_i4
             return
          end if
       end if
       !
       mx = ge(x(i),0.0_dp)
       nx = ge(x(j),0.0_dp)
       my = ge(y(i),0.0_dp)
       ny = ge(y(j),0.0_dp)

       test1 = .not.((my.or.ny).and.(mx.or.nx)).or.(mx.and.nx)
       test2 = .not.(my.and.ny.and.(mx.or.nx).and..not.(mx.and.nx))

       if (.not. test1) then
          if (test2) then
             if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i)) < 0.0_dp) then
                cycle
             else
                if ((y(i)*x(j)-x(i)*y(j))/(x(j)-x(i)) > 0.0_dp) then
                   erg = -erg
                   cycle
                else
                   erg = 0_i4
                   return
                end if
             end if
          else
             erg=-erg
          end if
       end if

    end do

  end subroutine inpoly_dp

  ! ------------------------------------------------------------------
  !>    \brief   Check orientation of polygon
  !>    \details Function for checking the orientation of a polygon (2D, convex or not).
  !>    \return  Boolean indicating orientation (counter-clockwise: .true., clockwise: .false.)
  function orientpoly_dp(coord) result(orientpoly)
    !> coordinates of the polygon in question
    real(dp), dimension(:,:),     intent(in)     :: coord
    integer(i4) :: orientpoly
    !> result:
    !!     inside:         erg =  1
    !!     outside:        erg = -1
    !!     on vertex/edge: erg =  0

    ! local variables
    integer(i4) :: n
    real(dp) :: sum_edges

    ! calculate sum over the edges, (x2 − x1)(y2 + y1) as in
    ! https://stackoverflow.com/questions/1165647/how-to-determine-if-a-list-of-polygon-points-are-in-clockwise-order
    n = size(coord, 1)
    ! use a vectorized version of sum over all (x2 -x1)*(y2-y1)
    sum_edges = sum((coord(2:n, 1) - coord(1:n-1, 1)) * (coord(2:n, 2) + coord(1:n-1, 2)))
    sum_edges = sum_edges + (coord(1, 1) - coord(n, 1)) * (coord(1, 2) + coord(n, 2))
    if (eq(sum_edges, 0._dp)) then
      orientpoly = 0_i4
    else if (sum_edges < 0._dp) then
      orientpoly = -1_i4
    else
      orientpoly = 1_i4
    end if

  end function orientpoly_dp

  ! ------------------------------------------------------------------
  !>    \brief   Modify polygon so it covers pole correctly
  !>    \details Modifies a polygon (2D, convex or not) to include pole when passed to inpoly
  !!  this function is intended to modify a given polygon, so it can be represented in a Cartesian grid
  !!  the use case is a polygon (e.g. 120,80, -120,80, 0,80 covering the north pole) that is not represented on
  !!  Cartesian lat-lon grid as polygon.
  !!  The script inserts additional coordinates, so the pole is covered (e.g. 180,80, 180,90, -180,90, -180,80)
  !!  See test cases for examples.
  !>    \return  modified coordinates
  function mod_pole_dp(coord, meridian_arg) result(coord_mod)
    !> coordinates of the polygon in question
    real(dp), dimension(:,:), intent(in)     :: coord
    !> meridian that represents discontinuity, defaults to 180.0
    real(dp), intent(in), optional :: meridian_arg
    real(dp), dimension(:,:), allocatable :: coord_mod

    ! local variables
    real(dp) :: meridian
    real(dp) :: break, a
    integer(i4) :: i, j, k, n

    if (present(meridian_arg)) then
      meridian = meridian_arg
    else
      meridian = 180._dp
    end if

    n = size(coord, 1)
    ! determine location where meridian is crossed
    ! find the maximum and minimum longitudes
    i = maxloc(coord(:, 1), 1)
    j = minloc(coord(:, 1), 1)
    ! determine size of new coords array
    k = n + 2
    if (ne(coord(i, 1), meridian)) then
      k = k + 1
    end if
    if (ne(coord(j, 1), meridian * (-1._dp))) then
      k = k + 1
    end if
    allocate(coord_mod(k, 2))
    ! polygon covers a pole
    if (mod(i,n)+1 == j) then
      ! the coord pair after i is j, so longitudes are ascending, so north pole is contained
      coord_mod(1:i, :) = coord(1:i, :)
      k = i
      ! if the maxval is not meridian, we need to add point at intersection of meridian and points i and j
      if (ne(coord(i, 1), meridian)) then
        a = meridian - coord(i, 1)
        break = coord(i, 2) + a / (a + abs(meridian + coord(j, 1))) * (coord(j, 2) - coord(i, 2))
        coord_mod(k+1, :) = [meridian, break]
        k = k + 1
      end if
      ! add the points meridian,90 and meridian * -1, 90
      coord_mod(k+1:k+2, 1) = [meridian, meridian * (-1._dp)]
      coord_mod(k+1:k+2, 2) = [90._dp, 90._dp]
      k = k + 2
      ! if the minval is not meridian * -1, we need to add point at intersection of meridian and points i and j
      if (ne(coord(j, 1), meridian * (-1._dp))) then
        a = meridian - coord(i, 1)
        break = coord(i, 2) + a / (a + abs(meridian + coord(j, 1))) * (coord(j, 2) - coord(i, 2))
        coord_mod(k+1, :) = [meridian * (-1._dp), break]
        k = k + 1
      end if
      ! add the remaining coordinates
      if (j > 1) coord_mod(k+1:k+1+n-j, :) = coord(j:n, :)
    else if (mod(j,n)+1 == i) then
      ! the coord pair after j is i, so longitudes are descending, so south pole is contained
      coord_mod(1:j, :) = coord(1:j, :)
      k = j
      ! if the minval is not meridian * -1, we need to add point at intersection of meridian and points i and j
      if (ne(coord(j, 1), meridian * (-1._dp))) then
        a = abs(meridian + coord(j, 1))
        break = coord(j, 2) + a / (a + meridian - coord(i, 1)) * (coord(i, 2) - coord(j, 2))
        coord_mod(k+1, :) = [meridian * (-1._dp), break]
        k = k + 1
      end if
      ! add the points meridian * -1, -90 and meridian, -90
      coord_mod(k+1:k+2, 1) = [meridian * (-1._dp), meridian]
      coord_mod(k+1:k+2, 2) = [-90._dp, -90._dp]
      k = k + 2
      ! if the maxval is not meridian, we need to add point at intersection of meridian and points i and j
      if (ne(coord(i, 1), meridian)) then
        a = abs(meridian + coord(j, 1))
        break = coord(j, 2) + a / (a + meridian - coord(i, 1)) * (coord(i, 2) - coord(j, 2))
        coord_mod(k+1, :) = [meridian, break]
        k = k + 1
      end if
      ! add the remaining coordinates
      if (i > 1) coord_mod(k+1:k+1+n-i, :) = coord(i:n, :)
    ! else: if there are multiple locations of minval or maxval, this edge case is not covered...
    end if

  end function mod_pole_dp

  ! ------------------------------------------------------------------
  !>    \brief   Shifts the (longitude) value 180 degrees
  !>    \details Modify a coordinate value
  !>    \return  Shifted value
  elemental function mod_shift_dp(x_coord, meridian_arg) result(shifted)
    !> coordinates of the polygon in question
    real(dp), intent(in) :: x_coord
    !> meridian that represents discontinuity, defaults to 180.0
    real(dp), intent(in), optional :: meridian_arg
    real(dp) :: shifted
    real(dp) :: meridian

    ! shift values
    if (present(meridian_arg)) then
      meridian = meridian_arg
    else
      meridian = 180._dp
    end if
    shifted = sign(abs(x_coord) - meridian, x_coord * (-1._dp))

  end function mod_shift_dp

  ! ------------------------------------------------------------------

end module mo_poly
