!> \file    mo_polynomial.f90
!> \copydoc mo_polynomial

!> \brief   Module to handle polynomials.
!> \details This module provides routines to deal with polynomials like evaluation, root finding or derivation.
!!
!! Polynomials of order (n-1) are represented by an array of n coefficients `p = (p_1, p_2, ..., p_n)` starting with the coefficient for the biggest exponent:
!! \code{.f90}
!! P(x) = p_1 * x^(n-1) + p_2 * x^(n-2) ... + p_n
!! \endcode
!!
!> \version 0.1
!> \authors Sebastian Mueller
!> \date    Oct 2024
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_polynomial

  use mo_kind, only : i4, dp
  use mo_utils, only : equal
  use mo_message, only : error_message

  implicit none
  private

  public :: poly_eval
  public :: poly_deriv
  public :: poly_order
  public :: poly_root
  public :: poly_root_newton
  public :: poly_root_halley
  public :: poly_root_cubic

contains

  !> \brief   Evaluate polynomial from given coefficients p(1) * x^(n-1) + ... + p(n).
  !> \return  Function value for given point.
  real(dp) function poly_eval(p, x)

    !> array with coefficients
    real(dp), intent(in)  :: p(:)
    !> point for evaluation
    real(dp), intent(in) :: x

    integer(i4) :: n, i

    n = size(p) ! should be > 0
    if (n == 0_i4) call error_message("mo_polynomial : p is an empty array.")

    ! Horner's scheme
    poly_eval = p(1)
    do i = 2_i4, n
      poly_eval = poly_eval * x + p(i)
    end do

  end function poly_eval

  !> \brief   Coefficients of the derivative of a polynomial from given coefficients p(1) * x^(n-1) + ... + p(n).
  !> \return  Coefficient array for the derivative d_p(1) * x^(n-2) + ... + d_p(n-1) with d_p(i) = (n - i) * p(i)
  function poly_deriv(p) result(d_p)

    !> array with coefficients
    real(dp), intent(in)  :: p(:)

    real(dp) :: d_p(max(size(p) - 1_i4, 1_i4))
    integer(i4) :: n, i

    n = size(p)
    if (n == 0_i4) call error_message("mo_polynomial : p is an empty array.")
    if (n == 1_i4) d_p(1) = 0.0

    do i = 1_i4, n - 1_i4
        d_p(i) = (n - i) * p(i)
    end do

  end function poly_deriv

  !> \brief   Derive polynomial order from given coefficients p(1) * x^(n-1) + ... + p(n).
  !> \return  Order of the polynomial (determined by non zero entry in coefficients). order < n.
  integer(i4) function poly_order(p)

    !> array with coefficients
    real(dp), intent(in)  :: p(:)

    integer(i4) :: n, i

    n = size(p)
    if (n == 0_i4) call error_message("mo_polynomial : p is an empty array.")

    poly_order = 0_i4
    do i = 1_i4, n
      if (abs(p(i)) > 0.0_dp) then
        poly_order = (n - i)
        return
      end if
    end do

  end function poly_order

  !> \brief   Find root of a polynomial from given coefficients p(1) * x^(n-1) + ... + p(n) with Householder methods.
  !> \return  Coefficient array for the derivative.
  function poly_root(p, x0, delta, max_n, method, raise, status) result(root)

    !> array with coefficients
    real(dp), intent(in)               :: p(:)
    !> initial guess
    real(dp), intent(in)               :: x0
    !> convergence criteria (default: 1e-2)
    real(dp), intent(in), optional     :: delta
    !> maximum number of iterations (default: 50)
    integer(i4), intent(in), optional  :: max_n
    !> method to use: 0 - will be selected by order, 1 - Newton, 2 - Halley (default ord > 1), 3 - cubic Householder
    integer(i4), intent(in), optional :: method
    !> whether to raise an error if status is not 0 (default: .true.)
    logical, intent(in), optional  :: raise
    !> status of root finding: 0 - found root, 1 - max number of iterations reached, 2 - not converging, 3 - polynomial is constant
    integer(i4), intent(out), optional :: status

    real(dp) :: root
    integer(i4) :: method_, status_
    logical :: raise_

    method_ = 0_i4
    raise_ = .true.
    if ( present(method) ) method_ = method
    if ( present(raise) ) raise_ = raise

    if ( method_ == 0_i4 ) then
      if ( poly_order(p) < 2 ) then
        method_ = 1_i4
      else
        method_ = 2_i4 ! Halley is optimal for polynomials
      end if
    end if

    select case(method_)
      case(1_i4) ! Newton
        root = poly_root_newton(p, x0, delta, max_n, status_)
      case(2_i4) ! Halley
        root = poly_root_halley(p, x0, delta, max_n, status_)
      case(3_i4) ! cubic Householder
        root = poly_root_cubic(p, x0, delta, max_n, status_)
      case default
        call error_message("mo_polynomial::poly_root : 'method' needs to be in {1,2,3}.")
    end select

    if ( present(status) ) status = status_
    if ( status_ /= 0_i4 .and. raise_ ) then
      select case(status_)
        case(1_i4)
          call error_message("mo_polynomial::poly_root : max number of iterations reached.")
        case(2_i4)
          call error_message("mo_polynomial::poly_root : stuck on plateau.")
        case(3_i4)
          call error_message("mo_polynomial::poly_root : given polynomial is constant.")
      end select
    end if

  end function poly_root

  !> \brief   Find root of a polynomial from given coefficients p(1) * x^(n-1) + ... + p(n) with Newtons method.
  !> \return  Coefficient array for the derivative.
  function poly_root_newton(p, x0, delta, max_n, status) result(root)

    !> array with coefficients
    real(dp), intent(in)               :: p(:)
    !> initial guess
    real(dp), intent(in)               :: x0
    !> convergence criteria (default: 1e-2)
    real(dp), intent(in), optional     :: delta
    !> maximum number of iterations (default: 50)
    integer(i4), intent(in), optional  :: max_n
    !> status of root finding: 0 - found root, 1 - max number of iterations reached, 2 - not converging, 3 - polynomial is constant
    integer(i4), intent(out), optional :: status

    real(dp) :: root
    real(dp) :: d_p(size(p) - 1)
    integer(i4) :: n, i, status_, max_n_
    real(dp) :: delta_, df

    delta_ = 1.0e-02
    max_n_ = 50_i4
    if ( present(delta) ) delta_ = delta
    if ( present(max_n) ) max_n_ = max_n

    root = x0
    if ( poly_order(p) < 1 ) then
      if ( present(status) ) status = 3_i4
      return
    end if

    status_ = 1_i4
    d_p = poly_deriv(p)

    ! newton iteration
    do i = 1_i4, max_n_
      ! check if current iteration is already fine
      if ( abs(poly_eval(p, root)) < delta_ ) then
        status_ = 0_i4
        exit
      end if
      ! if derivative is 0, return and set status to 2
      df = poly_eval(d_p, root)
      if (equal(df, 0.0_dp)) then
        status_ = 2_i4
        exit
      end if
      ! next value by newton method
      root = root - poly_eval(p, root) / df
    end do

    if ( present(status) ) status = status_

  end function poly_root_newton

  !> \brief   Find root of a polynomial from given coefficients p(1) * x^(n-1) + ... + p(n) with Halleys method.
  !> \return  Coefficient array for the derivative.
  function poly_root_halley(p, x0, delta, max_n, status) result(root)

    !> array with coefficients
    real(dp), intent(in)               :: p(:)
    !> initial guess
    real(dp), intent(in)               :: x0
    !> convergence criteria (default: 1e-2)
    real(dp), intent(in), optional     :: delta
    !> maximum number of iterations (default: 50)
    integer(i4), intent(in), optional  :: max_n
    !> status of root finding: 0 - found root, 1 - max number of iterations reached, 2 - not converging, 3 - polynomial is constant
    integer(i4), intent(out), optional :: status

    real(dp) :: root
    real(dp) :: d_p1(max(size(p) - 1_i4, 1_i4)), d_p2(max(size(p) - 2_i4, 1_i4))
    integer(i4) :: n, i, status_, max_n_
    real(dp) :: delta_, df1, df2, f_df

    delta_ = 1.0e-02
    max_n_ = 50_i4
    if ( present(delta) ) delta_ = delta
    if ( present(max_n) ) max_n_ = max_n

    root = x0
    if ( poly_order(p) < 1 ) then
      if ( present(status) ) status = 3_i4
      return
    end if

    status_ = 1_i4
    d_p1 = poly_deriv(p)
    d_p2 = poly_deriv(d_p1)

    ! halley iteration
    do i = 1_i4, max_n_
      ! check if current iteration is already fine
      if ( abs(poly_eval(p, root)) < delta_ ) then
        status_ = 0_i4
        exit
      end if
      ! if derivative is 0, return and set status to 2
      df1 = poly_eval(d_p1, root)
      if (equal(df1, 0.0_dp)) then
        status_ = 2_i4
        exit
      end if
      df2 = poly_eval(d_p2, root)
      f_df = poly_eval(p, root) / df1
      ! next value by halley method
      root = root - f_df / (1.0_dp - f_df * df2 / df1 * 0.5_dp)
    end do

    if ( present(status) ) status = status_

  end function poly_root_halley

  !> \brief   Find root of a polynomial from given coefficients p(1) * x^(n-1) + ... + p(n) with cubic Householder method.
  !> \return  Coefficient array for the derivative.
  function poly_root_cubic(p, x0, delta, max_n, status) result(root)

    !> array with coefficients
    real(dp), intent(in)               :: p(:)
    !> initial guess
    real(dp), intent(in)               :: x0
    !> convergence criteria (default: 1e-2)
    real(dp), intent(in), optional     :: delta
    !> maximum number of iterations (default: 50)
    integer(i4), intent(in), optional  :: max_n
    !> status of root finding: 0 - found root, 1 - max number of iterations reached, 2 - not converging, 3 - polynomial is constant
    integer(i4), intent(out), optional :: status

    real(dp) :: root
    real(dp) :: d_p1(max(size(p) - 1_i4, 1_i4)), d_p2(max(size(p) - 2_i4, 1_i4)), d_p3(max(size(p) - 3_i4, 1_i4))
    integer(i4) :: n, i, status_, max_n_
    real(dp) :: delta_, df1, df2, df3, f_df1, df2_df1

    delta_ = 1.0e-02
    max_n_ = 50_i4
    if ( present(delta) ) delta_ = delta
    if ( present(max_n) ) max_n_ = max_n

    root = x0
    if ( poly_order(p) < 1 ) then
      if ( present(status) ) status = 3_i4
      return
    end if

    status_ = 1_i4
    d_p1 = poly_deriv(p)
    d_p2 = poly_deriv(d_p1)
    d_p3 = poly_deriv(d_p2)

    ! halley iteration
    do i = 1_i4, max_n_
      ! check if current iteration is already fine
      if ( abs(poly_eval(p, root)) < delta_ ) then
        status_ = 0_i4
        exit
      end if
      ! if derivative is 0, return and set status to 2
      df1 = poly_eval(d_p1, root)
      if (equal(df1, 0.0_dp)) then
        status_ = 2_i4
        exit
      end if
      df2 = poly_eval(d_p2, root)
      df3 = poly_eval(d_p3, root)
      f_df1 = poly_eval(p, root) / df1
      df2_df1 = df2 / df1
      ! next value by cubic householder method
      root = root - f_df1 * (1.0_dp - f_df1 * df2_df1 * 0.5_dp) / (1.0_dp - df2_df1 * f_df1 + df3 / df1 * f_df1 ** 2_i4 / 6.0_dp)
    end do

    if ( present(status) ) status = status_

  end function poly_root_cubic

end module mo_polynomial
