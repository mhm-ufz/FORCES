!> \file mo_anneal.f90
!> \brief \copybrief mo_anneal
!> \details \copydetails mo_anneal

!> \brief Anneal optimization of cost function.
!> \details Minimization of cost function and temperature finding of minima.
!> \changelog
!! - Juliane Mai, Mar 2012
!!   - module implementation
!! - Juliane Mai, May 2012
!!   - anneal: sp version
!! - Juliane Mai, May 2012
!!   - anneal: documentation
!! - Juliane Mai, May 2012
!!   - GetTemperature: sp and dp version
!! - Juliane Mai, Jun 2012
!!   - weighted parameter selection
!! - Juliane Mai, Aug 2012
!!   - function anneal instead of subroutine
!!   - using new module get_timeseed as default for seeding
!!   - new optional for minimization or maximization
!!   - fixed parameter ranges possible instead of interface range
!! - Juliane Mai, Nov 2012
!!   - history of achieved objective function values as optional out only in anneal but not anneal_valid
!! - Juliane Mai, Jan 2013
!!   - including DDS features in anneal, i.e. reflection at parameter boundaries,
!!     different parameter selection modes (one, all, neighborhood), and
!!     different parameter pertubation modes (flexible r=dR (anneal version) or
!!     constant r=0.2 (dds version))
!!   - remove sp versions
!!   - fixed and flexible parameter ranges are now in one function
!!     using optional arguments
!!   - undef_funcval instead of anneal_valid function
!! - Juliane Mai, Feb 2013
!!   - xor4096 optionals combined in save_state
!! - Arya Prasetya, Dec 2021
!!   - doxygen documentation anneal and get_temperature
!> \author Juliane Mai
!> \date Mar 2012
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
MODULE mo_anneal

  USE mo_kind, ONLY : i4, i8, dp
  USE mo_utils, ONLY : le, ge
  USE mo_xor4096, ONLY : get_timeseed, xor4096, xor4096g, n_save_state
  USE mo_optimization_utils, ONLY : eval_interface, objective_interface

  IMPLICIT NONE

  PUBLIC :: anneal                  ! Minimization of a cost function via Simaulated Annealing
  PUBLIC :: GetTemperature          ! Function which returns the optimal initial temperature for
  !                                 ! a given acceptance ratio and initial parameter set

  ! ------------------------------------------------------------------

  !>        \brief Optimize cost function with simulated annealing.

  !>        \details
  !!        Optimizes a user provided cost function using the Simulated Annealing strategy.
  !!
  !!        \b Example
  !!
  !!        User defined function `cost_dp` which calculates the cost function value for a
  !!        parameter set (the interface given below has to be used for this function!).
  !!
  !!        \code{.f90}
  !!        para = (/ 1.0_dp , 2.0_dp /)
  !!        prange(1,:) = (/ 0.0_dp, 10.0_dp /)
  !!        prange(2,:) = (/ 0.0_dp, 50.0_dp /)
  !!
  !!        parabest = anneal(cost_dp, para, prange)
  !!        \endcode
  !!
  !!        See also test folder for a detailed example, "test/test_mo_anneal".
  !!
  !!        \b Literature
  !!        1. S. Kirkpatrick, C. D. Gelatt, and M. P. Vecchi.
  !!           _Optimization by simulated annealing_.
  !!           Science, 220:671-680, 1983.
  !!        2. N. Metropolis, A. W. Rosenbluth, M. N. Rosenbluth, A. H. Teller, and E. Teller.
  !!           _Equation of state calculations by fast computing machines_.
  !!           J. Chem. Phys., 21:1087-1092, June 1953.
  !!        3. B. A. Tolson, and C. A. Shoemaker.
  !!           _Dynamically dimensioned search algorithm for computationally efficient watershed model calibration_.
  !!           WRR, 43(1), W01413, 2007.
  !!
  !>        \param[in] "INTERFACE                   :: eval"   Interface calculating the eval function at a given point.
  !>        \param[in] "INTERFACE                   :: cost"   Interface calculating the cost function at a given point.
  !>        \param[in]  "real(dp),    dimension(:)       :: para"            Initial parameter set.
  !>        \param[in]  "real(dp), dimension(size(para),2), optional :: prange"
  !!                                                                         Lower and upper bound per parameter.
  !>        \param[in]  "interface, optional             :: prange_func"     Interface calculating the
  !!                                                                         feasible range for a parameter
  !!                                                                         at a certain point, if ranges are variable.
  !>        \param[in]  "real(dp), optional              :: temp"            Initial temperature. \n
  !!                                                                         DEFAULT: Get_Temperature.
  !>        \param[in]  "real(dp), optional              :: DT"              Geometrical decreement of temperature. \n
  !!                                                                         0.7<DT<0.999. \n
  !!                                                                         DEFAULT: 0.9_dp.
  !>        \param[in]  "integer(i4), optional           :: nITERmax"        Maximal number of iterations. \n
  !!                                                                         Will be increased by 10% if stopping criteria of
  !!                                                                         acceptance ratio or epsilon decreement of cost
  !!                                                                         function is not fullfilled. \n
  !!                                                                         DEFAULT: 1000_i4.
  !>        \param[in]  "integer(i4), optional           :: LEN"             Length of Markov Chain. \n
  !!                                                                         DEFAULT: MAX(250_i4, size(para,1)).
  !>        \param[in]  "integer(i4), optional           :: nST"             Number of consecutive LEN steps. \n
  !!                                                                         DEFAULT: 5_i4.
  !>        \param[in]  "real(dp), optional              :: eps"             Stopping criteria of epsilon decreement of
  !!                                                                         cost function \n
  !!                                                                         DEFAULT: 0.0001_dp
  !>        \param[in]  "real(dp), optional              :: acc"             Stopping criteria for Acceptance Ratio \n
  !!                                                                         acc    <= 0.1_dp \n
  !!                                                                         DEFAULT: 0.1_dp
  !>        \param[in]  "integer(i4/i8), dimension(3), optional      :: seeds"
  !!                                                                         Seeds of random numbers used for random parameter
  !!                                                                         set generation \n
  !!                                                                         DEFAULT: dependent on current time
  !>        \param[in]  "logical, optional                           :: printflag"
  !!                                                                         If .true. detailed command line output is written \n
  !!                                                                         DEFAULT: .false.
  !>        \param[in]  "logical, dimension(size(para)), optional  :: maskpara"
  !!                                                                         maskpara(i) = .true.  --> parameter is optimized \n
  !!                                                                         maskpara(i) = .false. --> parameter is discarded
  !!                                                                                                   from optimiztaion \n
  !!                                                                         DEFAULT: .true.
  !>        \param[in]  "real(dp), dimension(size(para)), optional :: weight"
  !!                                                                         vector of weights per parameter: \n
  !!                                                                         gives the frequency of parameter to be chosen for
  !!                                                                         optimization (will be scaled to a CDF internally) \n
  !!                                                                         eg. [1,2,1] --> parameter 2 is chosen twice as
  !!                                                                         often as parameter 1 and 2 \n
  !!                                                                         DEFAULT: weight = 1.0_dp
  !>        \param[in]  "integer(i4), optional           :: changeParaMode"  which and how many param.are changed in one step \n
  !!                                                                         1 = one parameter \n
  !!                                                                         2 = all parameter \n
  !!                                                                         3 = neighborhood parameter \n
  !!                                                                         DEFAULT: 1_i4
  !>        \param[in]  "logical, optional               :: reflectionFlag"  if new parameter values are Gaussian
  !!                                                                         distributed and reflected (.true.) or
  !!                                                                         uniform in range (.false.) \n
  !!                                                                         DEFAULT: .false.
  !>        \param[in]  "logical, optional               :: pertubFlexFlag"  if pertubation of Gaussian distributed
  !!                                                                         parameter values is constant
  !!                                                                         at 0.2 (.false.) or
  !!                                                                         depends on dR (.true.) \n
  !!                                                                         DEFAULT: .true.
  !>        \param[in]  "logical, optional               :: maxit"           maximizing (.true.) or minimizing (.false.)
  !!                                                                         a function \n
  !!                                                                         DEFAULT: .false. (minimization)
  !>        \param[in]  "real(dp), optional              :: undef_funcval"   objective function value defining invalid
  !!                                                                         model output, e.g. -999.0_dp \n
  !>        \param[in]  "character(len=*) , optional     :: tmp_file"        file with temporal output
  !>        \param[out]  "real(dp), optional             :: funcbest"        minimized value of cost function
  !>        \param[out]  "real(dp), dimension(:,:), allocatable, optional   :: history"
  !!                                                                         returns a vector of achieved objective
  !!                                                                         after ith model evaluation
  !>        \retval "real(dp) :: parabest(size(para))"                       Parameter set minimizing the cost function.

  !>     \note
  !!           - Either fixed parameter range (`prange`) OR flexible parameter range (function interface `prange_func`)
  !!             has to be given in calling sequence.
  !!           - Only double precision version available.
  !!           - If single precision is needed not only dp has to be replaced by sp
  !!             but also i8 of `save_state` (random number variables) has to be replaced by i4.
  !!           - ParaChangeMode > 1 is not applied in GetTemperature.
  !!           - For Temperature estimation always only one single parameter is changed (ParaChangeMode=1)
  !!             which should give theoretically always the best estimate.
  !!           - `cost_func` and `prange_func` are user defined functions. See interface definition.

  !>    \author Luis Samaniego
  !>    \date Jan 2000
  !>    \date Mar 2003
  !!          - Re-heating

  !>    \author Juliane Mai
  !>    \date Mar 2012
  !!          - Modular version
  !>    \date May 2012
  !!          - sp version
  !!          - documentation

  ! ------------------------------------------------------------------

  INTERFACE anneal
    MODULE PROCEDURE anneal_dp
  END INTERFACE anneal

  ! ------------------------------------------------------------------

  !>        \brief Find initial temperature for simulated annealing.

  !>        \details Determines an initial temperature for Simulated Annealing achieving
  !!         certain acceptance ratio.
  !!
  !!        \b Example
  !!
  !!        User defined function 'cost_dp' which calculates the cost function value for a
  !!        parameter set (the interface given below has to be used for this function!).
  !!
  !!        \code{.f90}
  !!        para = (/ 1.0_dp , 2.0_dp /)
  !!        acc_goal   = 0.95_dp
  !!        prange(1,:) = (/ 0.0_dp, 10.0_dp /)
  !!        prange(2,:) = (/ 0.0_dp, 50.0_dp /)
  !!
  !!        temp = GetTemperature(para, cost_dp, acc_goal, prange)
  !!        \endcode
  !!
  !!        See also test folder for a detailed example, "pf_tests/test_mo_anneal".
  !!
  !!        \b Literature
  !!        1. Walid Ben-Ameur.
  !!           _Compututing the Initial Temperature of Simulated Annealing_.
  !!           Comput. Opt. and App. (2004).
  !!
  !>        \param[in] "real(dp),    dimension(:)   :: paraset"   Initial (valid) parameter set.
  !>        \param[in] "INTERFACE                   :: eval"   Interface calculating the eval function at a given point.
  !>        \param[in] "INTERFACE                   :: cost"   Interface calculating the cost function at a given point.
  !>        \param[in] "INTERFACE                   :: prange_func"   Interface for functional ranges.
  !>        \param[in] "real(dp)                    :: acc_goal"  Acceptance Ratio which has to be achieved.
  !>        \param[in] "real(dp), dimension(size(para),2), optional :: prange"
  !!                                                                         Lower and upper bound per parameter.
  !>        \param[in] "INTERFACE, optional              :: prange_func"     Interface calculating the
  !!                                                                         feasible range for a parameter
  !!                                                                         at a certain point, if ranges are variable.
  !>        \param[in] "integer(i4), optional            :: samplesize"      Number of iterations the estimation of temperature
  !!                                                                         is based on. \n
  !!                                                                         DEFAULT: Max(20_i4*n,250_i4)
  !>        \param[in] "logical, dimension(size(para)), optional    :: maskpara"
  !!                                                                         maskpara(i) = .true.  --> parameter is optimized. \n
  !!                                                                         maskpara(i) = .false. --> parameter is discarded
  !!                                                                                                   from optimiztaion. \n
  !!                                                                         DEFAULT: .true. .
  !>        \param[in] "INTEGER(I4/I8), dimension(2), optional      :: seeds"
  !!                                                                         Seeds of random numbers used for random parameter
  !!                                                                         set generation. \n
  !!                                                                         DEFAULT: dependent on current time.
  !>        \param[in] "logical, optional                           :: printflag"
  !!                                                                         If .true. detailed command line output is written.
  !!                                                                         DEFAULT: .false. .
  !>        \param[in] "real(dp), dimension(size(para,1)), optional :: weight"
  !!                                                                         Vector of weights per parameter. \n
  !!                                                                         Gives the frequency of parameter to be chosen for
  !!                                                                         optimization (will be scaled to a CDF internally). \n
  !!                                                                         eg. [1,2,1] --> parameter 2 is chosen twice as
  !!                                                                                         often as parameter 1 and 2. \n
  !!                                                                         DEFAULT: weight = 1.0_dp.
  !>        \param[in] "logical, optional                :: maxit"            Minimizing (.false.) or maximizing (.true.)
  !!                                                                          a function. \n
  !!                                                                          DEFAULT: .false. (minimization).
  !>        \param[in] "real(dp), optional               :: undef_funcval"    Objective function value defining invalid
  !!                                                                          model output, e.g. -999.0_dp.

  !>        \retval "real(dp) :: temperature"                                 Temperature achieving a certain
  !!                                                                          acceptance ratio in Simulated Annealing.

  !>        \note
  !!              - Either fixed parameter range (`prange`) OR flexible parameter range (function interface `prange_func`)
  !!                has to be given in calling sequence.
  !!              - Only double precision version available.
  !!              - If single precision is needed not only dp has to be replaced by sp
  !!                but also i8 of `save_state` (random number variables) has to be replaced by i4.
  !!              - ParaChangeMode > 1 is not applied in GetTemperature.
  !!              - For Temperature estimation always only one single parameter is changed (ParaChangeMode=1)
  !!                which should give theoretically always the best estimate.
  !!              - `cost_dp` and `prange_func` are user defined functions. See interface definition.

  !>        \author  Juliane Mai
  !>        \date May 2012

  ! ------------------------------------------------------------------

  INTERFACE GetTemperature
    MODULE PROCEDURE GetTemperature_dp
  END INTERFACE GetTemperature

  PRIVATE

  INTERFACE Generate_Neighborhood_weight
    MODULE PROCEDURE Generate_Neighborhood_weight_dp
  END INTERFACE Generate_Neighborhood_weight

  ! ------------------------------------------------------------------

CONTAINS

  FUNCTION anneal_dp(eval, cost, para, &   ! obligatory
          prange, prange_func, &   ! optional IN: exactly one of both
          temp, Dt, nITERmax, Len, nST, eps, acc, &   ! optional IN
          seeds, printflag, maskpara, weight, &   ! optional IN
          changeParaMode, reflectionFlag, &   ! optional IN
          pertubFlexFlag, maxit, undef_funcval, &   ! optional IN
          tmp_file, &   ! optional IN
          funcbest, history                          &   ! optional OUT
          ) &
          result(parabest)

    IMPLICIT NONE

    procedure(eval_interface), INTENT(IN), POINTER :: eval
    procedure(objective_interface), intent(in), pointer :: cost

    INTERFACE
      SUBROUTINE prange_func(paraset, iPar, rangePar)
        ! gives the range (min,max) of the parameter iPar at a certain parameter set paraset
        use mo_kind
        real(dp), dimension(:), INTENT(IN) :: paraset
        integer(i4), INTENT(IN) :: iPar
        real(dp), dimension(2), INTENT(OUT) :: rangePar
      END SUBROUTINE prange_func
    END INTERFACE
    optional :: prange_func

    real(dp), dimension(:), intent(in) :: para                         !< initial parameter

    real(dp), optional, dimension(size(para, 1), 2), intent(in) :: prange !< lower and upper limit per parameter
    real(dp), optional, intent(in) :: temp                                !< starting temperature (DEFAULT: Get_Temperature)
    real(dp), optional, intent(in) :: Dt                                  !< geometrical decreement, 0.7<DT<0.999 (DEFAULT: 0.9)
    integer(i4), optional, intent(in) :: nITERmax                         !< maximal number of iterations (DEFAULT: 1000)
    integer(i4), optional, intent(in) :: Len                              !< Length of Markov Chain,
                                                                          !! DEFAULT: max(250, size(para,1))
    integer(i4), optional, intent(in) :: nST                              !< Number of consecutive LEN steps! (DEFAULT: 5)
    real(dp), optional, intent(in) :: eps                                 !< epsilon decreement of cost function (DEFAULT: 0.01)
    real(dp), optional, intent(in) :: acc                                 !< Acceptance Ratio, <0.1 stopping criteria
                                                                          !! (DEFAULT: 0.1)
    INTEGER(I8), optional, intent(in) :: seeds(3)                         !< Seeds of random numbers (DEFAULT: Get_timeseed)
    logical, optional, intent(in) :: printflag                            !< If command line output is written (.true.)
                                                                          !!  (DEFAULT: .false.)
    logical, optional, dimension(size(para, 1)), intent(in) :: maskpara   !< true if parameter will be optimized
                                                                          !! false if parameter is discarded in optimization
                                                                          !! (DEFAULT: .true.)
    real(dp), optional, dimension(size(para, 1)), intent(in) :: weight    !< vector of weights per parameter
                                                                          !! gives the frequency of parameter to be
                                                                          !!  chosen for optimization (DEFAULT: uniform)
    integer(i4), optional, intent(in) :: changeParaMode                   !< which and how many param. are changed in a step
                                                                          !!  1 = one parameter 2 = all parameter
                                                                          !!  3 = neighborhood parameter (DEFAULT: 1_i4)
    logical, optional, intent(in) :: reflectionFlag                       !< if new parameter values are selected normal
                                                                          !!  distributed and reflected (.true.) or
                                                                          !!  uniform in range (.false.) (DEFAULT: .false.)
    logical, optional, intent(in) :: pertubFlexFlag                       !< if pertubation of normal distributed parameter
                                                                          !!  values is constant 0.2 (.false.) or
                                                                          !!  depends on dR (.true.) (DEFAULT: .true.)
    logical, optional, intent(in) :: maxit                                !< Maximization or minimization of function
                                                                          !! maximization = .true., minimization = .false.
                                                                          !! (DEFAULT: .false.)
    real(dp), optional, intent(in) :: undef_funcval                       !< objective function value occuring if
                                                                          !!  parameter set leads to  invalid model results,
                                                                          !! e.g. -9999.0_dp! (DEFAULT: not present)
    CHARACTER(LEN = *), optional, intent(in) :: tmp_file                  !< file for temporal output
    real(dp), optional, intent(out) :: funcbest                           !< minimized value of cost function
                                                                          !! (DEFAULT: not present)
    real(dp), optional, dimension(:, :), allocatable, intent(out) :: history !< returns a vector of achieved objective!
                                                                          !! after ith model evaluation (DEFAULT: not present)

    real(dp), dimension(size(para, 1)) :: parabest                     !< parameter set minimizing objective

    integer(i4) :: n              ! Number of parameters
    integer(i4) :: iPar, par      ! counter for parameter
    real(dp), dimension(2) :: iParRange      ! parameter's range
    real(dp) :: T_in           ! Temperature
    real(dp) :: DT_IN          ! Temperature decreement
    integer(i4) :: nITERmax_in    ! maximal number of iterations
    integer(i4) :: LEN_IN         ! Length of Markov Chain
    integer(i4) :: nST_in         ! Number of consecutive LEN steps
    real(dp) :: eps_in         ! epsilon decreement of cost function
    real(dp) :: acc_in         ! Acceptance Ratio, <0.1 stopping criteria
    INTEGER(I8) :: seeds_in(3)    ! Seeds of random numbers
    logical :: printflag_in   ! If command line output is written
    logical :: coststatus     ! Checks status of cost function value,
    !                                                          ! i.e. is parameter set is feasible
    logical, dimension(size(para, 1)) :: maskpara_in    ! true if parameter will be optimized
    integer(i4), dimension(:), allocatable :: truepara       ! indexes of parameters to be optimized
    real(dp), dimension(size(para, 1)) :: weight_in      ! CDF of parameter chosen for optimization
    real(dp), dimension(size(para, 1)) :: weightUni      ! uniform CDF of parameter chosen for optimization
    real(dp), dimension(size(para, 1)) :: weightGrad     ! linear combination of weight and weightUni
    integer(i4) :: changeParaMode_inin  ! 1=one, 2=all, 3=neighborhood
    logical :: reflectionFlag_inin  ! true=gaussian distributed and reflected,
    !                                                                ! false=uniform distributed in range
    logical :: pertubFlexFlag_inin  ! variance of normal distributed parameter values
    !                                                                ! .true. = depends on dR, .false. = constant 0.2
    real(dp) :: maxit_in             ! maximization = -1._dp, minimization = 1._dp
    real(dp) :: costbest             ! minimized value of cost function
    real(dp), dimension(:, :), allocatable :: history_out          ! best cost function value after k iterations
    real(dp), dimension(:, :), allocatable :: history_out_tmp      ! helper vector

    type paramLim
      real(dp) :: min         ! minimum value
      real(dp) :: max         ! maximum value
      real(dp) :: new         ! new state value
      real(dp) :: old         ! old state value
      real(dp) :: best        ! best value found
      real(dp) :: dMult       ! sensitivity multiplier for parameter search
    end type paramLim
    type (paramLim), dimension (size(para, 1)), target :: gamma ! Parameter

    ! for random numbers
    real(dp) :: RN1, RN2, RN3     ! Random numbers
    integer(I8), dimension(n_save_state) :: save_state_1      ! optional arguments for restarting RN stream 1
    integer(I8), dimension(n_save_state) :: save_state_2      ! optional arguments for restarting RN stream 2
    integer(I8), dimension(n_save_state) :: save_state_3      ! optional arguments for restarting RN stream 3
    ! for dds parameter selection
    logical, dimension(size(para, 1)) :: neighborhood      ! selected parameter in neighborhood
    real(dp) :: pertubationR      ! neighborhood pertubation size parameter

    ! for SA
    integer(i4) :: idummy, i
    real(dp) :: NormPhi
    real(dp) :: ac_ratio, pa
    integer(i4), dimension(:, :), allocatable :: iPos_iNeg_history      ! stores iPos and iNeg of nST last Markov Chains
    real(dp) :: fo, fn, df, fBest
    real(dp) :: rho, fInc, fbb, dr
    real(dp) :: T0, DT0
    real(dp), parameter :: small = -700._DP
    integer(i4) :: j, iter, kk
    integer(i4) :: Ipos, Ineg
    integer(i4) :: iConL, iConR, iConF
    integer(i4) :: iTotalCounter          ! includes reheating for final conditions
    integer(i4) :: iTotalCounterR         ! counter of interations in one reheating
    logical :: iStop
    logical :: ldummy

    ! CHECKING OPTIONALS

    n = size(para, 1)

    ! either range or rangfunc has to be given
    if (present(prange) .eqv. present(prange_func)) then
      stop 'anneal: Either range or prange_func has to be given'
    end if

    if (present(Dt)) then
      if ((Dt .lt. 0.7_dp) .or. (Dt .gt. 0.999_dp)) then
        stop 'Input argument DT must lie between 0.7 and 0.999'
      else
        DT_IN = Dt
      end if
    else
      DT_IN = 0.9_dp
    end if

    if (present(nITERmax)) then
      if (nITERmax .lt. 1_I4) then
        stop 'Input argument nITERmax must be greater 0'
      else
        nITERmax_in = nITERmax
      end if
    else
      nITERmax_in = 1000_i4
    end if

    if (present(Len)) then
      if (Len .lt. Max(20_i4 * n, 250_i4)) then
        print*, 'WARNING: Input argument LEN should be greater than Max(250,20*N), N=number of parameters'
        LEN_IN = Len
      else
        LEN_IN = Len
      end if
    else
      LEN_IN = Max(20_i4 * n, 250_i4)
    end if

    idummy = nITERmax_in / LEN_IN + 1_i4
    allocate(history_out(idummy, 2))

    if (present(nST)) then
      if (nST .lt. 0_i4) then
        stop 'Input argument nST must be greater than 0'
      else
        nST_in = nST
      end if
    else
      nST_in = 5_i4
    end if

    allocate(iPos_iNeg_history(nST_in, 2))
    iPos_iNeg_history = 0_i4

    if (present(eps)) then
      if (le(eps, 0.0_dp)) then
        stop 'Input argument eps must be greater than 0'
      else
        eps_in = eps
      end if
    else
      eps_in = 0.0001_dp
    end if

    if (present(acc)) then
      if (le(acc, 0.0_dp)  .or. ge(acc, 1.0_dp)) then
        stop 'Input argument acc must lie between 0.0 and 1.0'
      else
        acc_in = acc
      end if
    else
      acc_in = 0.1_dp
    end if

    if (present(seeds)) then
      seeds_in = seeds
    else
      ! Seeds depend on actual time
      call get_timeseed(seeds_in)
    end if

    if (present(printflag)) then
      printflag_in = printflag
    else
      printflag_in = .false.
    end if

    if (present(maskpara)) then
      if (count(maskpara) .eq. 0_i4) then
        stop 'Input argument maskpara: At least one element has to be true'
      else
        maskpara_in = maskpara
      end if
    else
      maskpara_in = .true.
    end if

    if (present(maxit)) then
      ldummy = maxit
      if (maxit) then
        maxit_in = -1._dp
      else
        maxit_in = 1._dp
      end if
    else
      ldummy = .false.
      maxit_in = 1._dp
    end if

    if (present(temp)) then
      if ((temp .lt. 0.0_dp)) then
        stop 'Input argument temp must be greater then zero'
      else
        T_in = temp
      end if
    else
      if (present(prange_func)) then
        if (present(undef_funcval)) then
          T_in = GetTemperature(eval, cost, para, 0.95_dp, prange_func = prange_func, &
                  maskpara = maskpara_in, samplesize = 2_i4 * LEN_IN, &
                  seeds = seeds_in(1 : 2), printflag = printflag_in, &
                  maxit = ldummy, undef_funcval = undef_funcval)
        else
          T_in = GetTemperature(eval, cost, para, 0.95_dp, prange_func = prange_func, &
                  maskpara = maskpara_in, samplesize = 2_i4 * LEN_IN, &
                  seeds = seeds_in(1 : 2), printflag = printflag_in, &
                  maxit = ldummy)
        end if
      else
        if (present(undef_funcval)) then
          T_in = GetTemperature(eval, cost, para, 0.95_dp, prange = prange, &
                  maskpara = maskpara_in, samplesize = 2_i4 * LEN_IN, &
                  seeds = seeds_in(1 : 2), printflag = printflag_in, &
                  maxit = ldummy, undef_funcval = undef_funcval)
        else
          T_in = GetTemperature(eval, cost, para, 0.95_dp, prange = prange, &
                  maskpara = maskpara_in, samplesize = 2_i4 * LEN_IN, &
                  seeds = seeds_in(1 : 2), printflag = printflag_in, &
                  maxit = ldummy)
        end if
      end if
    end if

    if (present(changeParaMode)) then
      changeParaMode_inin = changeParaMode
    else
      changeParaMode_inin = 1_i4
    end if

    if (present(reflectionFlag)) then
      reflectionFlag_inin = reflectionFlag
    else
      reflectionFlag_inin = .false.
    end if

    if (present(pertubFlexFlag)) then
      pertubFlexFlag_inin = pertubFlexFlag
    else
      pertubFlexFlag_inin = .true.
    end if

    ! Temporal file writing
    if(present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', status = 'unknown')
      write(999, *) '# settings :: general'
      write(999, *) '# nIterations    iseed'
      write(999, *) nITERmax_in, seeds_in
      write(999, *) '# settings :: anneal specific'
      write(999, *) '# sa_tmp'
      write(999, *) T_in
      write(999, *) '# iter   bestf   (bestx(j),j=1,nopt)'
      close(999)
    end if

    ! INITIALIZATION

    allocate (truepara(count(maskpara_in)))
    idummy = 0_i4
    do i = 1, N
      if (maskpara_in(i)) then
        idummy = idummy + 1_i4
        truepara(idummy) = i
      end if
    end do

    if (printflag_in) then
      print*, 'Following parameters will be optimized: ', truepara
    end if

    weight_in = 0.0_dp
    weightUni = 0.0_dp
    if (present(weight)) then
      where (maskpara_in(:))
        weight_in(:) = weight(:)
        weightUni(:) = 1.0_dp
      end where
    else
      where (maskpara_in(:))
        weight_in(:) = 1.0_dp
        weightUni(:) = 1.0_dp
      end where
    end if
    ! scaling the weights
    weight_in = weight_in / sum(weight_in)
    weightUni = weightUni / sum(weightUni)
    ! cummulating the weights
    do i = 2, n
      weight_in(i) = weight_in(i) + weight_in(i - 1)
      weightUni(i) = weightUni(i) + weightUni(i - 1)
    end do

    call xor4096 (seeds_in(1), RN1, save_state = save_state_1)
    call xor4096 (seeds_in(2), RN2, save_state = save_state_2)
    call xor4096g(seeds_in(3), RN3, save_state = save_state_3)
    seeds_in = 0_i8

    ! Start Simulated Annealing routine
    gamma(:)%dmult = 1.0_dp
    gamma(:)%new = para(:)
    gamma(:)%old = para(:)
    gamma(:)%best = para(:)
    NormPhi = -9999.9_dp
    T0 = T_in
    DT0 = DT_IN

    ! Generate and evaluate the initial solution state
    fo = cost(gamma(:)%old, eval) * maxit_in
    if (abs(fo) .lt. tiny(0.0_dp)) fo = 0.0000001_dp * maxit_in

    file_write : if (present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (n + 2) * 30)
      if (.not. ldummy) then
        write(999, *) '0', fo, gamma(:)%old
      else
        write(999, *) '0', -fo, gamma(:)%old
      end if
      close(999)
    end if file_write

    ! initialize counters /var for new SA
    iConL = 0_i4
    iConR = 1_i4
    iConF = 0_i4
    iTotalCounter = 0_i4
    iTotalCounterR = 0_i4
    fn = 0.0_DP
    ac_ratio = 1.0_DP
    iStop = .TRUE.
    iter = 0_i4
    ! restoring initial T param.
    DT_IN = DT0
    ! Storing the best solution so far
    NormPhi = fo * maxit_in
    fo = fo / NormPhi
    fBest = 1.0_dp * maxit_in

    if (printflag_in) then
      print '(A15,E15.7,A4,E15.7,A4)', ' start NSe   = ', fBest * maxit_in, '  ( ', fBest * normPhi * maxit_in, ' )  '
      print '(I8, 2I5, 4E15.7)', 1_i4, 0_i4, 0_i4, 1._dp, T_in, fo * normPhi * maxit_in, fBest * normPhi * maxit_in
    end if

    ! ****************** Stop Criterium  *******************
    ! Repeat until the % reduction of nST consecutive Markov
    ! chains (LEN) of the objective function (f) <= epsilon
    ! ******************************************************
    loopTest : do while (iStop)
      iter = iter + 1_i4
      Ipos = 0_i4
      Ineg = 0_i4
      fbb = fBest
      !LEN_IN = int( real(iter,dp)*sqrt(real(iter,dp)) / nITER + 1.5*real(N,dp),i4)
      ! Repeat LEN times with feasible solution
      j = 1
      loopLEN : do while (j .le. LEN_IN)

        iTotalCounterR = iTotalCounterR + 1_i4
        iTotalCounter = iTotalCounter + 1_i4

        ! Generate a random subsequent state and evaluate its objective function
        ! (1) Generate new parameter set
        dR = (1.0_DP - real(iTotalCounterR, dp) / real(nITERmax_in, dp))**2.0_DP
        if (.not. (ge(dR, 0.05_DP)) .and. &
                (iTotalCounterR <= int(real(nIterMax_in, dp) / 3._dp * 4_dp, i4))) then
          dR = 0.05_DP
        end if

        if (pertubFlexFlag_inin) then
          pertubationR = max(dR / 5.0_dp, 0.05_dp)
        else
          pertubationR = 0.2_dp
        end if

        ! gradual weights:
        !        acc_ratio close to 1 --> weight
        !        acc_ratio close to 0 --> weight uniform
        ! gradual weighting is linear combination of these two weights
        weightGrad(:) = weightUni(:) + (ac_ratio) * (weight_in(:) - weightUni(:))

        select case(changeParaMode_inin)
        case(1_i4)  ! only one parameter is changed
          ! (1a) Change one parameter traditionally
          call xor4096(seeds_in(2), RN2, save_state = save_state_2)
          iPar = 1_i4
          !
          do while (weightGrad(iPar) .lt. RN2)
            iPar = iPar + 1_i4
          end do
          if (present(prange_func)) then
            call prange_func(gamma(:)%old, iPar, iParRange)
          else
            iParRange(1) = prange(iPar, 1)
            iParRange(2) = prange(iPar, 2)
          end if
          gamma(iPar)%min = iParRange(1)
          gamma(iPar)%max = iParRange(2)
          if (reflectionFlag_inin) then
            call xor4096g(seeds_in(3), RN3, save_state = save_state_3)
            gamma(iPar)%new = parGen_dds_dp(gamma(iPar)%old, pertubationR, &
                    gamma(iPar)%min, gamma(iPar)%max, RN3)
          else
            call xor4096(seeds_in(2), RN2, save_state = save_state_2)
            gamma(iPar)%new = parGen_anneal_dp(gamma(iPar)%old, dR, &
                    gamma(iPar)%min, gamma(iPar)%max, RN2)
          end if
        case(2_i4)  ! all parameter are changed
          do par = 1, size(truepara)
            iPar = truepara(par)
            if (present(prange_func)) then
              call prange_func(gamma(:)%old, iPar, iParRange)
            else
              iParRange(1) = prange(iPar, 1)
              iParRange(2) = prange(iPar, 2)
            end if
            gamma(iPar)%min = iParRange(1)
            gamma(iPar)%max = iParRange(2)
            if (reflectionFlag_inin) then
              call xor4096g(seeds_in(3), RN3, save_state = save_state_3)
              gamma(iPar)%new = parGen_dds_dp(gamma(iPar)%old, pertubationR, &
                      gamma(iPar)%min, gamma(iPar)%max, RN3)
            else
              call xor4096(seeds_in(2), RN2, save_state = save_state_2)
              gamma(iPar)%new = parGen_anneal_dp(gamma(iPar)%old, dR, &
                      gamma(iPar)%min, gamma(iPar)%max, RN2)
            end if
          end do
        case(3_i4)  ! parameter in neighborhood are changed
          ! Generate new neighborhood
          call generate_neighborhood_weight_dp(truepara, weightGrad, save_state_1, &
                  iTotalCounter, nIterMax_in, neighborhood)
          !
          ! change parameter in neighborhood
          do iPar = 1, n
            if (neighborhood(iPar)) then
              ! find range of parameter
              if (present(prange_func)) then
                call prange_func(gamma(:)%old, iPar, iParRange)
              else
                iParRange(1) = prange(iPar, 1)
                iParRange(2) = prange(iPar, 2)
              end if
              gamma(iPar)%min = iParRange(1)
              gamma(iPar)%max = iParRange(2)
              !
              if (reflectionFlag_inin) then
                ! generate gaussian distributed new parameter value which is reflected if out of bound
                call xor4096g(seeds_in(3), RN3, save_state = save_state_3)
                gamma(iPar)%new = parGen_dds_dp(gamma(iPar)%old, pertubationR, &
                        gamma(iPar)%min, gamma(iPar)%max, RN3)
              else
                ! generate new parameter value uniform distributed in range (no reflection)
                call xor4096(seeds_in(2), RN2, save_state = save_state_2)
                gamma(iPar)%new = parGen_anneal_dp(gamma(iPar)%old, dR, &
                        gamma(iPar)%min, gamma(iPar)%max, RN2)
              end if
            end if
          end do
        end select

        ! (2) Calculate new objective function value
        fn = cost(gamma(:)%new, eval) * maxit_in
        coststatus = .true.
        if (present(undef_funcval)) then
          if (abs(fn * maxit_in - undef_funcval) .lt. tiny(1.0_dp)) then
            coststatus = .false.
          end if
        end if

        feasible : if (coststatus) then   ! feasible parameter set
          fn = fn / normPhi

          ! Change in cost function value
          df = fn - fo

          ! analyze change in the objective function: df
          if (df < 0.0_DP) then

            ! accept the new state
            Ipos = Ipos + 1_i4
            fo = fn
            gamma(:)%old = gamma(:)%new

            ! keep best solution
            if (fo < fBest) then
              fBest = fo
              gamma(:)%best = gamma(:)%new
            end if
          else
            if (df >  eps_in) then
              rho = -df / T_in
              if (rho < small) then
                pa = 0.0_DP
              else
                pa = EXP(rho)
              end if
              !
              call xor4096(seeds_in(1), RN1, save_state = save_state_1)
              !
              if (pa > RN1) then
                ! accept new state with certain probability
                Ineg = Ineg + 1_i4
                fo = fn
                ! save old state
                gamma(:)%old = gamma(:)%new
              end if
            end if
          end if
          j = j + 1
        else
          ! function value was not valid
          iTotalCounterR = iTotalCounterR - 1_i4
          iTotalCounter = iTotalCounter - 1_i4
        end if feasible !valid parameter set
      end do loopLEN

      ! estimate acceptance ratio
      ac_ratio = (real(Ipos, dp) + real(Ineg, dp)) / real(LEN_IN, dp)
      if (modulo(iTotalCounter, LEN_IN * nST_in) / LEN_IN .gt. 0_i4) then
        idummy = modulo(iTotalCounter, LEN_IN * nST_in) / LEN_IN
      else
        idummy = nST_in
      end if
      iPos_iNeg_history(idummy, :) = (/ iPos, iNeg /)

      ! store current best in history vector
      history_out(iTotalCounter / LEN_IN, 1) = real(iTotalCounter, dp)
      history_out(iTotalCounter / LEN_IN, 2) = maxit_in * fBest * normPhi

      file_write2 : if (present(tmp_file)) then
        open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (n + 2) * 30)
        write(999, *) iTotalCounter, maxit_in * fBest * normPhi, gamma(:)%best
        close(999)
      end if file_write2

      if (printflag_in) then
        print '(I8, 2I5, E15.7, 3E15.7)', ITotalCounter, Ipos, Ineg, ac_ratio, T_in, &
                fo * NormPhi * maxit_in, fBest * NormPhi * maxit_in
      end if

      ! Cooling schedule
      if (fbb .gt. tiny(1.0_dp)) then
        fInc = (fbb - fBest) / fbb
      else
        fInc = 1.0_dp
      end if
      if (fInc < 0.00000001_dp) then
        iConF = iConF + 1_i4
      else
        iConF = 0_i4
      end if

      if ((ac_ratio < 0.15_dp) .and. (iConF > 5_i4) .and. (iConR <= -3_i4)) then     ! - iConR  no reheating
        ! Re-heating
        if (printflag_in) then
          print *, 'Re-heating: ', iConR
        end if
        iConR = iConR + 1_i4
        T_in = T0 / 2._DP
        iter = 0_i4       ! for LEN
        iTotalCounterR = 0_i4       ! for dR
        ! start from current best
        gamma(:)%old = gamma(:)%best
      else
        ! Update Temperature (geometrical decrement)
        if (ac_ratio < 0.4_dp)  then
          DT_IN = 0.995_dp
        else
          DT_IN = DT0
        end if
        T_in = T_in * DT_IN
      end if

      ! Stop Criteria: consecutive MC with marginal decrements and acceptance ratio
      if (fInc < eps_in) then
        iConL = iConL + 1_i4
      else
        iConL = 0_i4
      end if
      if ((iConL > nST_in) .and. (ac_ratio < acc_in) .and. (iConR > 2_i4) .and. &
              (sum(iPos_iNeg_history(:, :)) .lt. 1_i4)) then
        iStop = .FALSE.
      end if
      ! a way out maximum
      if ((iTotalCounter > nITERmax_in) .and. & !(ac_ratio > acc_in) .and. &
              (sum(iPos_iNeg_history(:, :)) .ge. 1_i4)) then

        ! store achieved history so far
        allocate(history_out_tmp(size(history_out, 1), size(history_out, 2)))
        history_out_tmp = history_out
        deallocate(history_out)

        nITERmax_in = max(nITERmax_in + LEN_IN, int(real(nITERmax_in, dp) * 1.10_dp, i4))
        if (printflag_in) then
          print *, 'nITERmax changed to =', nITERmax_in
        end if

        ! increase lenght of history vecor
        idummy = nITERmax_in / LEN_IN + 1_i4
        allocate(history_out(idummy, 2))

        do j = 1, size(history_out_tmp, 1)
          history_out(j, :) = history_out_tmp(j, :)
        end do

        deallocate(history_out_tmp)

      end if
      ! STOP condition
      if (iTotalCounter > nITERmax_in) then
        iStop = .FALSE.
      end if

    end do loopTest

    ! calculate cost function again (only for check and return values)
    parabest = gamma(:)%best
    costbest = cost(parabest, eval) * maxit_in
    if (present (funcbest)) then
      funcbest = costbest * maxit_in
    end if
    fo = costbest / NormPhi

    if (printflag_in) then
      print *, '   '
      print '(A15,E15.7)', ' end cost    = ', maxit_in * fBest * normPhi
      print *, 'end parameter: '
      do kk = 1, N
        print '(A10,I3,A3, E15.7)', '    para #', kk, ' = ', gamma(kk)%best
      end do

      print *, 'Final check:    ', (fo - fBest)
    end if

    if (present(history)) then
      allocate(history(size(history_out, 1) - 1, size(history_out, 2)))
      history(:, :) = history_out(1 : size(history_out, 1) - 1, :)
    end if

    deallocate(truepara)
    deallocate(history_out)

    return

  END FUNCTION anneal_dp

  real(dp) function GetTemperature_dp(eval, cost, paraset, acc_goal, &    ! obligatory
          prange, prange_func, &    ! optional IN: exactly one of both
          samplesize, maskpara, seeds, printflag, &    ! optional IN
          weight, maxit, undef_funcval                             &    ! optional IN
          )
    use mo_kind, only : dp, i4, i8
    implicit none

    procedure(eval_interface), INTENT(IN), POINTER :: eval
    procedure(objective_interface), intent(in), pointer :: cost
    real(dp), dimension(:), intent(in) :: paraset                               !< a valid parameter set of the model
    real(dp), intent(in) :: acc_goal                                            !< acceptance ratio to achieve
    real(dp), optional, dimension(size(paraset, 1), 2), intent(in) :: prange    !< lower and upper limit per parameter
    integer(i4), optional, intent(in) :: samplesize                             !< size of random set the acc_estimate is based on.
                                                                                !! DEFAULT: Max(250, 20*Number paras)
    logical, optional, dimension(size(paraset, 1)), intent(in) :: maskpara      !< true if parameter will be optimized.
                                                                                !! false if parameter is discarded in optimization.
                                                                                !! DEFAULT: .true.
    integer(i8), optional, dimension(2), intent(in) :: seeds                    !< Seeds of random numbers. DEFAULT: time dependent.
    logical, optional, intent(in) :: printflag                                  !< .true. if detailed temperature estimation is
                                                                                !! printed. DEFAULT: .false.
    real(dp), OPTIONAL, dimension(size(paraset, 1)), INTENT(IN) :: weight       !< vector of weights per parameter gives the
                                                                                !! frequency of parameter to be chosen for
                                                                                !! optimization. DEFAULT: equal weighting
    logical, OPTIONAL, INTENT(IN) :: maxit                                      !< Maxim. or minim. of function
                                                                                !! maximization = .true., minimization = .false. .
                                                                                !! DEFAULT: .false.
    real(dp), OPTIONAL, INTENT(IN) :: undef_funcval                             !< objective function value occuring if
                                                                                !! parameter set leads to invalid model results,
                                                                                !! e.g. -999.0_dp. DEFAULT: not present

    INTERFACE
      SUBROUTINE prange_func(paraset, iPar, rangePar)
        ! gives the range (min,max) of the parameter iPar at a certain parameter set paraset
        use mo_kind
        real(dp), dimension(:), INTENT(IN) :: paraset
        integer(i4), INTENT(IN) :: iPar
        real(dp), dimension(2), INTENT(OUT) :: rangePar
      END SUBROUTINE prange_func
    END INTERFACE
    OPTIONAL :: prange_func
    !
    ! Local variables

    integer(i4) :: n
    integer(i4) :: samplesize_in
    integer(i4) :: idummy, i, j
    integer(i4) :: iPar
    real(dp), dimension(2) :: iParRange
    real(dp) :: NormPhi
    real(dp) :: fo, fn, dr
    real(dp) :: T

    logical :: coststatus     ! true if model output is valid
    logical, dimension(size(paraset, 1)) :: maskpara_in    ! true if parameter will be optimized
    integer(i4), dimension(:), ALLOCATABLE :: truepara       ! indexes of parameters to be optimized
    logical :: printflag_in   ! if detailed estimation of temperature is printed
    real(dp), dimension(size(paraset, 1)) :: weight_in      ! CDF of parameter to chose for optimization
    real(dp) :: maxit_in       ! Maximization or minimization of function:
    !                                                           ! -1 = maxim, 1 = minim

    type paramLim
      real(dp) :: min                 ! minimum value
      real(dp) :: max                 ! maximum value
      real(dp) :: new                 ! new state value
      real(dp) :: old                 ! old state value
      real(dp) :: best                ! best value found
      real(dp) :: dMult               ! sensitivity multiplier
      !                                                               ! for parameter search
    end type paramLim
    type (paramLim), dimension (size(paraset, 1)), target :: gamma    ! Parameter

    ! for random numbers
    INTEGER(I8) :: seeds_in(2)
    real(dp) :: RN1, RN2     ! Random numbers
    integer(I8), dimension(n_save_state) :: save_state_1      ! optional arguments for restarting RN stream 1
    integer(I8), dimension(n_save_state) :: save_state_2      ! optional arguments for restarting RN stream 2

    ! for initial temperature estimate
    real(dp) :: acc_estim  ! estimate of acceptance probability
    ! depends on temperature
    ! goal: find a temperature such that acc_estim ~ 0.9
    real(dp), dimension(:, :), allocatable :: Energy     ! dim(LEN,2) cost function values before (:,1) and
    !                                                   ! after (:,2) transition

    n = size(paraset, 1)

    ! either range or rangfunc has to be given
    if (present(prange) .eqv. present(prange_func)) then
      stop 'anneal: Either range or prange_func has to be given'
    end if

    if (present(samplesize)) then
      if (samplesize .lt. Max(20_i4 * n, 250_i4)) then
        !stop 'Input argument LEN must be greater than Max(250,20*N), N=number of parameters'
        print*, 'WARNING (GetTemperature): '
        print*, 'Input argument samplesize should be greater than Max(250,20*N), N=number of parameters'
        samplesize_in = samplesize
      else
        samplesize_in = samplesize
      end if
    else
      samplesize_in = Max(20_i4 * n, 250_i4)
    end if

    if (present(maskpara)) then
      if (count(maskpara) .eq. 0_i4) then
        stop 'Input argument maskpara: At least one element has to be true'
      else
        maskpara_in = maskpara
      end if
    else
      maskpara_in = .true.
    end if

    if (present(seeds)) then
      seeds_in = seeds
    else
      ! Seeds depend on actual time
      call get_timeseed(seeds_in)
      print*, 'temp: seeds(1)=', seeds_in(1)
    end if

    if (present(printflag)) then
      printflag_in = printflag
    else
      printflag_in = .false.
    end if

    if (present(maxit)) then
      if (maxit) then
        maxit_in = -1._dp
      else
        maxit_in = 1._dp
      end if
    else
      maxit_in = 1._dp
    end if

    allocate(Energy(samplesize_in, 2))

    allocate (truepara(count(maskpara_in)))
    idummy = 0_i4
    do i = 1, N
      if (maskpara_in(i)) then
        idummy = idummy + 1_i4
        truepara(idummy) = i
      end if
    end do

    weight_in = 0.0_dp
    if (present(weight)) then
      where (maskpara_in(:))
        weight_in(:) = weight(:)
      end where
    else
      where (maskpara_in(:))
        weight_in(:) = 1.0_dp
      end where
    end if
    ! scaling the weights
    weight_in = weight_in / sum(weight_in)
    ! cummulating the weights
    do i = 2, n
      weight_in(i) = weight_in(i) + weight_in(i - 1)
    end do

    ! Setting up the RNG
    ! (1) Seeds depend on actual time or on input seeds
    ! (2) Initialize the streams
    call xor4096(seeds_in(1), RN1, save_state = save_state_1)
    call xor4096(seeds_in(2), RN2, save_state = save_state_2)
    seeds_in = 0_i8
    ! (3) Now ready for calling

    gamma(:)%dmult = 1.0_dp
    gamma(:)%new = paraset(:)
    gamma(:)%old = paraset(:)
    gamma(:)%best = paraset(:)
    NormPhi = -9999.9_dp

    fo = cost(paraset, eval) * maxit_in
    if (abs(fo) .lt. tiny(0.0_dp)) fo = 0.0000001_dp * maxit_in
    NormPhi = fo
    fo = fo / NormPhi * maxit_in

    j = 1
    loopSamplesize : do while (j .le. samplesize_in)
      ! Generate a random subsequent state and evaluate its objective function
      ! (1)  Generate new parameter set
      dR = 1.0_DP

      ! (1a) Select parameter to be changed
      call xor4096(seeds_in(1), RN1, save_state = save_state_1)
      iPar = 1_i4
      do while (weight_in(iPar) .lt. RN1)
        iPar = iPar + 1_i4
      end do

      ! (1b) Generate new value of selected parameter
      if (present(prange_func)) then
        call prange_func(gamma(:)%old, iPar, iParRange)
      else
        iParRange(1) = prange(iPar, 1)
        iParRange(2) = prange(iPar, 2)
      end if
      gamma(iPar)%min = iParRange(1)
      gamma(iPar)%max = iParRange(2)
      call xor4096(seeds_in(2), RN2, save_state = save_state_2)
      gamma(iPar)%new = parGen_anneal_dp(gamma(iPar)%old, gamma(iPar)%dMult * dR, &
              gamma(iPar)%min, gamma(iPar)%max, RN2)
      !
      ! (2)  Calculate new objective function value and normalize it
      fn = cost(gamma(:)%new, eval) * maxit_in
      coststatus = .true.
      if (present(undef_funcval)) then
        if (abs(fn * maxit_in - undef_funcval) .lt. tiny(1.0_dp)) then
          coststatus = .false.
        end if
      end if

      feasible : if (coststatus) then   ! feasible parameter set
        fn = fn / normPhi
        ! Save Energy states of feasible transitions
        ! for adaption of optimal initial temperature
        ! Walid Ben-Ameur: "Comput. the Initial Temperature of Sim. Annealing"
        ! Comput. Opt. and App. 2004
        Energy(j, 2) = fn     ! E_max_t
        Energy(j, 1) = fo     ! E_min_t
        j = j + 1
      end if feasible !valid parameter set
    end do loopSamplesize

    ! estimation of the acceptance probability based on the random set ||<Samplesize>||
    ! only if actual temperature (T) equals initial temperature (temp)
    T = maxval(Energy)

    acc_estim = sum(exp(-(Energy(:, 2) / T))) / sum(exp(-(Energy(:, 1) / T)))
    if (printflag_in) then
      print*, "acc_estimate = ", acc_estim, "    ( T = ", T, " )"
    end if
    Do While ((acc_estim .lt. 1.0_dp) .and. (abs(acc_estim - acc_goal) .gt. 0.0001_dp))
      T = T * (Log(acc_estim) / Log(acc_goal))**(0.5_dp) ! **(1.0/p)  with p=1.0
      if (all(T .gt. Energy(:, 1) / 709._dp) .and. all(T .gt. Energy(:, 2) / 709._dp)) then
        acc_estim = sum(exp(-(Energy(:, 2) / T))) / sum(exp(-(Energy(:, 1) / T)))
        if (printflag_in) then
          print*, "acc_estimate = ", acc_estim, "    ( T = ", T, " )"
        end if
      else
        T = T / (Log(acc_estim) / Log(acc_goal))**(0.5_dp)
        exit
      end if
    end do
    GetTemperature_dp = T

  end function GetTemperature_dp

  !***************************************************
  !*               PRIVATE FUNCTIONS                 *
  !***************************************************

  real(dp) function parGen_anneal_dp(old, dMax, oMin, oMax, RN)
    use mo_kind, only : dp, i4
    implicit none

    real(dp), intent(IN) :: old, dMax, oMin, oMax
    real(dp), intent(IN) :: RN

    ! local variables
    real(dp) :: oi, ox, delta, dMaxScal
    integer(i4) :: iDigit, iszero             ! were intent IN before
    !
    iszero = 1_i4
    iDigit = 8_i4

    oi = oMin
    ox = oMax
    ! scaling (new)
    dMaxScal = dMax * ABS(oMax - oMin)

    if(ge(oi, ox)) then
      parGen_anneal_dp = (oi + ox) / 2.0_DP
    else
      if ((old - dMaxScal) > oi)  oi = old - dMaxScal
      if ((old + dMaxScal) < ox)  ox = old + dMaxScal
      delta = (oi + RN * (ox - oi)) - old
      if (delta > dMaxScal) delta = dMaxScal
      if (delta <-dMaxScal) delta = -dMaxScal
      parGen_anneal_dp = old + dChange_dp(delta, iDigit, isZero)
    end if

    ! Parameter is bounded between Max and Min.
    ! Correction from Kumar and Luis

    if (parGen_anneal_dp < oMin) then
      parGen_anneal_dp = oMin
    elseif(parGen_anneal_dp > oMax)then
      parGen_anneal_dp = oMax
    end if
  end function parGen_anneal_dp

  real(dp) function parGen_dds_dp(old, perturb, oMin, oMax, RN)
    ! PURPOSE:
    !    Perturb variable using a standard normal random number RN
    !    and reflecting at variable bounds if necessary
    !    Tolson et al. (2007): STEP 4
    !
    implicit none

    real(dp), intent(IN) :: old, perturb, oMin, oMax
    real(dp), intent(IN) :: RN

    ! generating new value
    parGen_dds_dp = old + perturb * (oMax - oMin) * RN

    ! reflect one time and set to boundary value
    if (parGen_dds_dp .lt. oMin) then
      parGen_dds_dp = oMin + (oMin - parGen_dds_dp)
      if (parGen_dds_dp .gt. oMax) parGen_dds_dp = oMin
    end if

    if (parGen_dds_dp .gt. oMax) then
      parGen_dds_dp = oMax - (parGen_dds_dp - oMax)
      if (parGen_dds_dp .lt. oMin) parGen_dds_dp = oMax
    end if

  end function parGen_dds_dp

  real(dp) function  dChange_dp(delta, iDigit, isZero)
    use mo_kind, only : i4, i8, dp
    implicit none

    integer(i4), intent(IN) :: iDigit, isZero
    real(dp), intent(IN) :: delta

    ! local variables
    integer(i4) :: ioszt
    integer(I8) :: iDelta

    ioszt = 10**iDigit
    iDelta = int(delta * real(ioszt, dp), i8)
    if(isZero == 1_i4) then
      if(iDelta == 0_i8) then
        if(delta < 0_i4) then
          iDelta = -1_i8
        else
          iDelta = 1_i8
        end if
      end if
    end if
    dChange_dp = real(iDelta, dp) / real(ioszt, dp)
  end function  dChange_dp

  subroutine generate_neighborhood_weight_dp(truepara, cum_weight, save_state_xor, iTotalCounter, &
          nITERmax, neighborhood)
    ! PURPOSE:
    !    generates a new neighborhood
    !    Tolson et al. (2007): STEP 3
    !
    integer(i4), dimension(:), intent(in) :: truepara
    real(dp), dimension(:), intent(in) :: cum_weight
    integer(i8), dimension(n_save_state), intent(inout) :: save_state_xor
    integer(i4), intent(in) :: iTotalCounter
    integer(i4), intent(in) :: nITERmax
    logical, dimension(size(cum_weight)), intent(out) :: neighborhood

    ! local variables
    integer(i4) :: ipar, npar
    integer(i4) :: iSize, size_neighbor
    real(dp) :: prob
    real(dp) :: rn
    real(dp) :: weight, norm
    real(dp), dimension(size(cum_weight)) :: local_cum_weight

    prob = 1.0_dp - Log(real(iTotalCounter, dp)) / Log(real(nITERmax, dp))
    neighborhood = .false.

    npar = size(cum_weight)
    local_cum_weight = cum_weight

    ! How many parameters will be selected for neighborhood?
    size_neighbor = 0_i4
    do ipar = 1, size(truepara)
      call xor4096(0_i8, rn, save_state = save_state_xor)
      if (rn < prob) then
        size_neighbor = size_neighbor + 1_i4
      end if
    end do
    ! at least one...
    size_neighbor = max(1_i4, size_neighbor)

    ! Which parameter will be used for neighborhood?
    do iSize = 1, size_neighbor
      ! (1) generate RN
      call xor4096(0_i8, rn, save_state = save_state_xor)
      !
      ! (2) find location <iPar> in cummulative distribution function
      iPar = 1_i4
      do while (local_cum_weight(iPar) .lt. rn)
        iPar = iPar + 1_i4
      end do
      !
      ! (3) add parameter to neighborhood
      neighborhood(iPar) = .true.
      !
      ! (4) recalculate cummulative distribution function
      ! (4.1) Which weight had iPar?
      if (iPar .gt. 1_i4) then
        weight = local_cum_weight(iPar) - local_cum_weight(iPar - 1)
      else
        weight = local_cum_weight(1)
      end if
      ! (4.2) Substract this weight from cummulative array starting at iPar
      local_cum_weight(iPar : nPar) = local_cum_weight(iPar : nPar) - weight
      ! (4.3) Renormalize cummulative weight to one
      if (count(neighborhood) .lt. size(truepara)) then
        norm = 1.0_dp / local_cum_weight(nPar)
      else
        norm = 1.0_dp
      end if
      local_cum_weight(:) = local_cum_weight(:) * norm
      !
    end do

  end subroutine generate_neighborhood_weight_dp

END MODULE mo_anneal
