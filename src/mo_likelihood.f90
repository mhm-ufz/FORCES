!> \file mo_likelihood.f90
!> \copydoc mo_likelihood

!>  \brief Added for testing purposes of test_mo_mcmc
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_likelihood

  USE mo_kind,   only: i4, dp
  USE mo_moment, only: stddev
  use mo_opt_eval_utils, only: eval_interface, sim_data_t, config_t

  Implicit NONE

  PRIVATE

  PUBLIC :: data
  PUBLIC :: model_dp
  PUBLIC :: likelihood_dp,        loglikelihood_dp        ! "real" likelihood  (sigma is an error model or given)
  PUBLIC :: likelihood_stddev_dp, loglikelihood_stddev_dp ! "faked" likelihood (sigma is computed by obs vs model)
  PUBLIC :: setmeas

  ! -------------------------------
  !> \brief synthetic data
  !> \details Data generated with
  !!     paraset(1) = 1.0
  !!     paraset(2) = 2.0
  !!     paraset(3) = 3.0
  !! plus additive, Gaussian distributed error with mu=0.0 and sigma=0.5
  INTERFACE data
     MODULE PROCEDURE data_dp
  END INTERFACE data

  !> \brief set meas
  INTERFACE setmeas
     MODULE PROCEDURE setmeas_dp
  END INTERFACE setmeas

  REAL(DP),DIMENSION(100,2)  :: meas                   ! measurements
  REAL(DP),PARAMETER         :: stddev_global=0.5_dp   ! standard deviation of measurement error

  ! ------------------------------------------------------------------

CONTAINS

  ! -------------------------------
  !> \brief A Likelihood function: "real" likelihood  (sigma is an error model or given)
  function likelihood_dp(paraset, eval, stddev_in, stddev_new, likeli_new)
    REAL(DP), DIMENSION(:), INTENT(IN)            :: paraset          ! parameter set
    procedure(eval_interface), INTENT(IN), pointer :: eval
    REAL(DP),               INTENT(IN), optional  :: stddev_in        ! standard deviation of data
    REAL(DP),               INTENT(OUT), OPTIONAL :: stddev_new       ! standard deviation of errors using paraset
    REAL(DP),               INTENT(OUT), OPTIONAL :: likeli_new       ! likelihood using stddev_new,
    !                                                                 ! i.e. using new parameter set
    REAL(DP)                                      :: likelihood_dp

    ! local
    REAL(DP), DIMENSION(size(meas,1))   :: errors
    real(dp), pointer :: runoff(:, :)
    type(sim_data_t), dimension(:), pointer :: sim_data
    type(config_t) :: config

    config%parameters = paraset

    allocate(sim_data(1))
    call sim_data(1)%add(name="runoff", ndim=2_i4)
    call eval(config, sim_data)
    call sim_data(1)%set_pointer(name="runoff", ptr=runoff)
    errors(:) = runoff(:,1)-data()
    likelihood_dp = exp(-0.5_dp * sum( errors(:) * errors(:) / stddev_global**2 ))
    deallocate(sim_data)

  end function likelihood_dp

  ! -------------------------------
  !> \brief A Log-Likelihood function: "real" likelihood  (sigma is an error model or given)
  function loglikelihood_dp(paraset, eval, stddev_in, stddev_new, likeli_new)
    REAL(DP), DIMENSION(:), INTENT(IN)            :: paraset          ! parameter set
    procedure(eval_interface), INTENT(IN), pointer :: eval
    REAL(DP),               INTENT(IN), optional  :: stddev_in        ! standard deviation of data
    REAL(DP),               INTENT(OUT), OPTIONAL :: stddev_new       ! standard deviation of errors using paraset
    REAL(DP),               INTENT(OUT), OPTIONAL :: likeli_new       ! likelihood using stddev_new,
    !                                                                 ! i.e. using new parameter set
    REAL(DP)                                      :: loglikelihood_dp

    ! local
    REAL(DP), DIMENSION(size(meas,1))   :: errors
    real(dp), pointer :: runoff(:, :)
    type(sim_data_t), dimension(:), pointer :: sim_data
    type(config_t) :: config

    config%parameters = paraset

    allocate(sim_data(1))
    call sim_data(1)%add(name="runoff", ndim=2_i4)
    call eval(config, sim_data)
    call sim_data(1)%set_pointer(name="runoff", ptr=runoff)
    errors(:) = runoff(:,1)-data()
    loglikelihood_dp = -0.5_dp * sum( errors(:) * errors(:) / stddev_global**2 )
    deallocate(sim_data)

  end function loglikelihood_dp

  ! -------------------------------
  !> \brief A Likelihood function: "faked" likelihood (sigma is computed by obs vs model)
  function likelihood_stddev_dp(paraset, eval, stddev_in, stddev_new, likeli_new)
    REAL(DP), DIMENSION(:), INTENT(IN)            :: paraset          ! parameter set
    procedure(eval_interface), INTENT(IN), pointer :: eval
    REAL(DP),               INTENT(IN), optional  :: stddev_in        ! standard deviation of data
    REAL(DP),               INTENT(OUT), OPTIONAL :: stddev_new       ! standard deviation of errors using paraset
    REAL(DP),               INTENT(OUT), OPTIONAL :: likeli_new       ! likelihood using stddev_new,
    !                                                                 ! i.e. using new parameter set
    REAL(DP)                                      :: likelihood_stddev_dp

    ! local
    REAL(DP), DIMENSION(size(meas,1))   :: errors
    REAL(DP)                            :: stddev_err
    real(dp), pointer :: runoff(:, :)
    type(sim_data_t), dimension(:), pointer :: sim_data
    type(config_t) :: config

    config%parameters = paraset

    allocate(sim_data(1))
    call sim_data(1)%add(name="runoff", ndim=2_i4)
    call eval(config, sim_data)
    call sim_data(1)%set_pointer(name="runoff", ptr=runoff)
    errors(:) = runoff(:,1)-data()
    likelihood_stddev_dp = exp(-0.5_dp * sum( errors(:) * errors(:) / stddev_in**2 ))
    deallocate(sim_data)

    ! optional out
    stddev_err = stddev(errors)
    if (present( stddev_new )) then
       stddev_new = stddev_err
    end if
    if (present( likeli_new )) then
       likeli_new = exp(-0.5_dp * sum( errors(:) * errors(:) / stddev_err**2 ))
    end if

  end function likelihood_stddev_dp

  ! -------------------------------
  !> \brief A Log-Likelihood_stddev function: "faked" likelihood (sigma is computed by obs vs model)
  function loglikelihood_stddev_dp(paraset, eval, stddev_in, stddev_new, likeli_new)
    REAL(DP), DIMENSION(:), INTENT(IN)            :: paraset          ! parameter set
    procedure(eval_interface), INTENT(IN), pointer :: eval
    REAL(DP),               INTENT(IN), optional  :: stddev_in        ! standard deviation of data
    REAL(DP),               INTENT(OUT), OPTIONAL :: stddev_new       ! standard deviation of errors using paraset
    REAL(DP),               INTENT(OUT), OPTIONAL :: likeli_new       ! likelihood using stddev_new,
    !                                                                 ! i.e. using new parameter set
    REAL(DP)                                      :: loglikelihood_stddev_dp

    ! local
    REAL(DP), DIMENSION(size(meas,1))   :: errors
    REAL(DP)                            :: stddev_err
    real(dp), pointer :: runoff(:, :)
    type(sim_data_t), dimension(:), pointer :: sim_data
    type(config_t) :: config

    config%parameters = paraset

    allocate(sim_data(1))
    call sim_data(1)%add(name="runoff", ndim=2_i4)
    call eval(config, sim_data)
    call sim_data(1)%set_pointer(name="runoff", ptr=runoff)
    errors(:) = runoff(:,1)-data()
    loglikelihood_stddev_dp = -0.5_dp * sum( errors(:) * errors(:) / stddev_in**2 )
    deallocate(sim_data)

    ! optional out
    stddev_err = stddev(errors)
    if (present( stddev_new )) then
       stddev_new = stddev_err
    end if
    if (present( likeli_new )) then
       likeli_new = -0.5_dp * sum( errors(:) * errors(:) / stddev_err**2 )
    end if

  end function loglikelihood_stddev_dp

  ! -------------------------------
  !> \brief A Model: p1*x^2 + p2*x + p3
  subroutine model_dp(config, sim_data)

    use mo_kind, only: dp
    use mo_message, only : error_message
    !! !$ USE omp_lib,    only: OMP_GET_THREAD_NUM

    type(config_t), intent(in) :: config
    type(sim_data_t), dimension(:), pointer, optional, intent(inout) :: sim_data
    real(dp), pointer :: runoff(:, :)

    integer(i4) :: i, n
    ! for OMP
    !! !$  integer(i4)                           :: n_threads, is_thread

    n = size(meas,1)

    if (size(sim_data) /= 1) call error_message('model_dp: does not support sim_data data with more than 1 dimension.')

    if (sim_data(1)%has('runoff')) then
      call sim_data(1)%allocate(name="runoff", data_shape=(/n, 1/))
      call sim_data(1)%set_pointer(name="runoff", ptr=runoff)
    end if

    !! !$ is_thread = OMP_GET_THREAD_NUM()
    !! !$ write(*,*) 'OMP_thread: ', is_thread

    !$OMP parallel default(shared) &
    !$OMP private(i)
    !$OMP do
    do i=1, n
       !! !$ if (is_thread /= 0) write(*,*) '    OMP_thread-1: ', is_thread
       runoff(i,1) = config%parameters(1) * meas(i,1) * meas(i,1) + config%parameters(2) * meas(i,1) + config%parameters(3)
    end do
    !$OMP end do
    !$OMP end parallel

  end subroutine model_dp

  function data_dp()
    use mo_kind
    REAL(DP), DIMENSION(size(meas,1))      :: data_dp

    data_dp = meas(:,2)

  end function data_dp

  subroutine setmeas_dp()

    integer(i4) :: i

    do i=1,100
       meas(i,1) = real(i,dp)
    end do

    meas(:,2) = (/ 5.49537_dp, 10.7835_dp, 17.6394_dp, 26.8661_dp, 36.9247_dp, 50.9517_dp, 66.2058_dp, &
         82.9703_dp, 101.26_dp, 123.076_dp, 145.457_dp, 171.078_dp, 198.349_dp, 227.23_dp, 257.922_dp, &
         290.098_dp, 325.775_dp, 362.724_dp, 402.669_dp, 442.461_dp, 486.122_dp, 531.193_dp, &
         577.931_dp, 627.091_dp, 678.096_dp, 731.364_dp, 786.039_dp, 843.531_dp, 903.126_dp, &
         963.037_dp, 1025.85_dp, 1091.85_dp, 1158.47_dp, 1226.65_dp, 1298.25_dp, 1370.87_dp, &
         1444.96_dp, 1522.6_dp, 1602.6_dp, 1684.38_dp, 1765.15_dp, 1850.74_dp, 1937.85_dp, 2027.41_dp, &
         2118.44_dp, 2210.62_dp, 2306.9_dp, 2403.27_dp, 2501.83_dp, 2602.96_dp, 2705.29_dp, &
         2811.44_dp, 2917.82_dp, 3027.09_dp, 3137.64_dp, 3250.86_dp, 3366.67_dp, 3482.56_dp, &
         3602.37_dp, 3722.55_dp, 3845.8_dp, 3970.15_dp, 4098.15_dp, 4227.27_dp, 4357.77_dp, &
         4491.49_dp, 4626.23_dp, 4762.95_dp, 4901.15_dp, 5042.95_dp, 5184.86_dp, 5330.4_dp, &
         5478.11_dp, 5626.97_dp, 5776.94_dp, 5931.15_dp, 6085.9_dp, 6242.7_dp, 6402.17_dp, 6563.46_dp, &
         6726.37_dp, 6890.34_dp, 7058.4_dp, 7227.17_dp, 7397.09_dp, 7570.77_dp, 7745.95_dp, &
         7923.72_dp, 8102.21_dp, 8282.98_dp, 8465.42_dp, 8651.37_dp, 8838.43_dp, 9027.57_dp, &
         9219.21_dp, 9411.24_dp, 9606.31_dp, 9802.88_dp, 10002.3_dp, 10202.6_dp /)
  end subroutine setmeas_dp

end module mo_likelihood
