!> \file mo_cost.f90
!> \copydoc mo_cost

!>  \brief Added for testing purposes of test_mo_anneal
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
Module mo_cost

  use mo_kind, only: sp, dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: cost_sp,  cost_dp
  PUBLIC :: cost_valid_sp,  cost_valid_dp
  PUBLIC :: range_sp, range_dp
  public :: cost_objective

CONTAINS

  !> \brief function: `f(x) = ax^3 + bx^2 + cx + d`
  FUNCTION cost_sp (paraset)

    implicit none

    REAL(SP), DIMENSION(:), INTENT(IN)  :: paraset
    REAL(SP)                            :: cost_sp
    REAL(SP), DIMENSION(6,2)            :: meas
    REAL(SP), DIMENSION(6)              :: calc

    ! function: f(x) = ax^3 + bx^2 + cx + d
    ! measurements: (0.5,5.725), (1.0, 21.7), (1.5, 49.175), (2.0, 88.9), (2.5, 141.625), (3.0, 208.1)
    ! --> a=1.0, b=20.0, c=0.2, d=0.5

    meas(:,1) = (/0.5_sp, 1.0_sp, 1.5_sp, 2.0_sp, 2.5_sp, 3.0_sp/)
    meas(:,2) = (/5.7250_sp, 21.7000_sp, 49.1750_sp, 88.9000_sp, 141.6250_sp, 208.1000_sp/)

    calc(:) = paraset(1)*meas(:,1)**3+paraset(2)*meas(:,1)**2+paraset(3)*meas(:,1)+paraset(4)

    ! MAE  Mean Absolute Error
    cost_sp = sum(abs( meas(:,2)-calc(:) ))/size(meas,1)

    RETURN
  END FUNCTION cost_sp

  !> \brief function: `f(x) = ax^3 + bx^2 + cx + d`
  FUNCTION cost_dp (paraset)

    implicit none

    REAL(DP), DIMENSION(:), INTENT(IN)  :: paraset
    REAL(DP)                            :: cost_dp
    REAL(DP), DIMENSION(6,2)            :: meas
    REAL(DP), DIMENSION(6)              :: calc

    ! function: f(x) = ax^3 + bx^2 + cx + d
    ! measurements: (0.5,5.725), (1.0, 21.7), (1.5, 49.175), (2.0, 88.9), (2.5, 141.625), (3.0, 208.1)
    ! --> a=1.0, b=20.0, c=0.2, d=0.5

    meas(:,1) = (/0.5_dp, 1.0_dp, 1.5_dp, 2.0_dp, 2.5_dp, 3.0_dp/)
    meas(:,2) = (/5.7250_dp, 21.7000_dp, 49.1750_dp, 88.9000_dp, 141.6250_dp, 208.1000_dp/)

    calc(:) = paraset(1)*meas(:,1)**3+paraset(2)*meas(:,1)**2+paraset(3)*meas(:,1)+paraset(4)

    ! MAE  Mean Absolute Error
    cost_dp = sum(abs( meas(:,2)-calc(:) ))/size(meas,1)

    RETURN
  END FUNCTION cost_dp

  !> \brief function: `f(x) = ax^3 + bx^2 + cx + d`
  FUNCTION cost_valid_sp (paraset,status_in)

    implicit none

    REAL(SP), DIMENSION(:), INTENT(IN)  :: paraset
    LOGICAL,  OPTIONAL,     INTENT(OUT) :: status_in
    REAL(SP)                            :: cost_valid_sp
    REAL(SP), DIMENSION(6,2)            :: meas
    REAL(SP), DIMENSION(6)              :: calc

    ! function: f(x) = ax^3 + bx^2 + cx + d
    ! measurements: (0.5,5.725), (1.0, 21.7), (1.5, 49.175), (2.0, 88.9), (2.5, 141.625), (3.0, 208.1)
    ! --> a=1.0, b=20.0, c=0.2, d=0.5

    meas(:,1) = (/0.5_sp, 1.0_sp, 1.5_sp, 2.0_sp, 2.5_sp, 3.0_sp/)
    meas(:,2) = (/5.7250_sp, 21.7000_sp, 49.1750_sp, 88.9000_sp, 141.6250_sp, 208.1000_sp/)

    calc(:) = paraset(1)*meas(:,1)**3+paraset(2)*meas(:,1)**2+paraset(3)*meas(:,1)+paraset(4)

    if (present(status_in)) then
       status_in = .true.
       ! Define a status .false. if calculation of "calc" was not successful
    end if

    ! MAE  Mean Absolute Error
    cost_valid_sp = sum(abs( meas(:,2)-calc(:) ))/size(meas,1)

    RETURN
  END FUNCTION cost_valid_sp

  !> \brief function: `f(x) = ax^3 + bx^2 + cx + d`
  FUNCTION cost_valid_dp (paraset,status_in)

    implicit none

    REAL(DP), DIMENSION(:), INTENT(IN)  :: paraset
    LOGICAL,  OPTIONAL,     INTENT(OUT) :: status_in
    REAL(DP)                            :: cost_valid_dp
    REAL(DP), DIMENSION(6,2)            :: meas
    REAL(DP), DIMENSION(6)              :: calc

    ! function: f(x) = ax^3 + bx^2 + cx + d
    ! measurements: (0.5,5.725), (1.0, 21.7), (1.5, 49.175), (2.0, 88.9), (2.5, 141.625), (3.0, 208.1)
    ! --> a=1.0, b=20.0, c=0.2, d=0.5

    meas(:,1) = (/0.5_dp, 1.0_dp, 1.5_dp, 2.0_dp, 2.5_dp, 3.0_dp/)
    meas(:,2) = (/5.7250_dp, 21.7000_dp, 49.1750_dp, 88.9000_dp, 141.6250_dp, 208.1000_dp/)

    calc(:) = paraset(1)*meas(:,1)**3+paraset(2)*meas(:,1)**2+paraset(3)*meas(:,1)+paraset(4)

    if (present(status_in)) then
       status_in = .true.
       ! Define a status .false. if calculation of "calc" was not successful
    end if

    ! MAE  Mean Absolute Error
    cost_valid_dp = sum(abs( meas(:,2)-calc(:) ))/size(meas,1)

    RETURN
  END FUNCTION cost_valid_dp

  !> \brief dummy range
  SUBROUTINE range_dp(paraset, iPar, rangePar)
    use mo_kind
    REAL(DP), DIMENSION(:), INTENT(IN)  :: paraset
    INTEGER(I4),            INTENT(IN)  :: iPar
    REAL(DP), DIMENSION(2), INTENT(OUT) :: rangePar

    ! Range does not depend on parameter set
    ! select case(iPar)
    !    case(1_i4)
    !       rangePar(1) =  0.0_dp
    !       rangePar(2) = 10.0_dp
    !    case(2_i4)
    !       rangePar(1) =  0.0_dp
    !       rangePar(2) = 40.0_dp
    !    case(3_i4)
    !       rangePar(1) =  0.0_dp
    !       rangePar(2) = 10.0_dp
    !    case(4_i4)
    !       rangePar(1) =  0.0_dp
    !       rangePar(2) =  5.0_dp
    ! end select

    ! Range of parameter 2 depends on value of parameter 1:
    !    parameter 2 at most 40* parameter 1 :
    !       0 <= p2 <= 40p1
    !       0 <= p1 <= 0.025p2
    select case(iPar)
    case(1_i4)
       rangePar(1) =  0.025_dp*paraset(2)
       rangePar(2) =  10.0_dp
    case(2_i4)
       rangePar(1) =  0.0_dp
       rangePar(2) =  40.0_dp*paraset(1)
    case(3_i4)
       rangePar(1) =  0.0_dp
       rangePar(2) = 10.0_dp
    case(4_i4)
       rangePar(1) =  0.0_dp
       rangePar(2) =  5.0_dp
    end select

  END SUBROUTINE range_dp

  !> \brief dummy range
  SUBROUTINE range_sp(paraset, iPar, rangePar)
    use mo_kind
    REAL(SP), DIMENSION(:), INTENT(IN)  :: paraset
    INTEGER(I4),            INTENT(IN)  :: iPar
    REAL(SP), DIMENSION(2), INTENT(OUT) :: rangePar

    ! Range does not depend on parameter set
    ! select case(iPar)
    !    case(1_i4)
    !       rangePar(1) =  0.0_sp
    !       rangePar(2) = 10.0_sp
    !    case(2_i4)
    !       rangePar(1) =  0.0_sp
    !       rangePar(2) = 40.0_sp
    !    case(3_i4)
    !       rangePar(1) =  0.0_sp
    !       rangePar(2) = 10.0_sp
    !    case(4_i4)
    !       rangePar(1) =  0.0_sp
    !       rangePar(2) =  5.0_sp
    ! end select

    ! Range of parameter 2 depends on value of parameter 1:
    !    parameter 2 at most 4* parameter 1 :
    !       0     <= p2 <= 4p1
    !       0.25p2 <= p1 <= 10.0
    select case(iPar)
    case(1_i4)
       rangePar(1) =  0.025_sp*paraset(2)
       rangePar(2) = 10.0_sp
    case(2_i4)
       rangePar(1) =  0.0_sp
       rangePar(2) =  40.0_sp*paraset(1)
    case(3_i4)
       rangePar(1) =  0.0_sp
       rangePar(2) = 10.0_sp
    case(4_i4)
       rangePar(1) =  0.0_sp
       rangePar(2) =  5.0_sp
    end select

  END SUBROUTINE range_sp

  !> \brief dummy cost objective function
  FUNCTION cost_objective(parameterset, eval, arg1, arg2, arg3)

    use mo_kind, only: dp, i4
    use mo_opt_eval_utils, only: eval_interface, sim_data_t, config_t

    implicit none

    real(dp), intent(in), dimension(:) :: parameterset
    procedure(eval_interface), INTENT(IN), pointer :: eval
    real(dp), optional, intent(in) :: arg1
    real(dp), optional, intent(out) :: arg2
    real(dp), optional, intent(out) :: arg3
    real(dp) :: cost_objective

    type(sim_data_t), dimension(:), pointer :: sim_data
    type(config_t) :: config
    REAL(DP), DIMENSION(6,2)            :: meas
    REAL(DP), DIMENSION(6)              :: calc

    config%parameters = parameterset
    allocate(sim_data(1))
    call sim_data(1)%add(name='et', ndim=2_i4)
    call eval(config, sim_data)

    ! function: f(x) = ax^3 + bx^2 + cx + d
    ! measurements: (0.5,5.725), (1.0, 21.7), (1.5, 49.175), (2.0, 88.9), (2.5, 141.625), (3.0, 208.1)
    ! --> a=1.0, b=20.0, c=0.2, d=0.5

    meas(:,1) = (/0.5_dp, 1.0_dp, 1.5_dp, 2.0_dp, 2.5_dp, 3.0_dp/)
    meas(:,2) = (/5.7250_dp, 21.7000_dp, 49.1750_dp, 88.9000_dp, 141.6250_dp, 208.1000_dp/)

    calc(:) = parameterset(1)*meas(:,1)**3+parameterset(2)*meas(:,1)**2+parameterset(3)*meas(:,1)+parameterset(4)

    ! MAE  Mean Absolute Error
    cost_objective = sum(abs( meas(:,2)-calc(:) ))/size(meas,1)

    deallocate(sim_data)

    RETURN
  END FUNCTION cost_objective


END MODULE mo_cost
