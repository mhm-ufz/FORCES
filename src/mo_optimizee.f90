!> \file mo_optimizee.f90
!> \copydoc mo_optimizee

!> \brief Utility functions, such as interface definitions, for optimization routines.
!> \details An abstract type \ref optimizee is provided to be used with the optimization functions
!!          such as \ref mo_anneal::anneal, \ref mo_dds::dds, \ref mo_dds::mdds, \ref mo_mcmc::mcmc,
!!          \ref mo_mcmc::mcmc_stddev or \ref mo_sce::sce.
!!
!!          Three reference implementations are provided:
!!          - \ref function_optimizee : simple function optimization taking an array of parameters
!!          - \ref likelihood_optimizee : likelihood optimization using sigma, stddev and likeli
!!          - \ref eval_optimizee : optimization using an eval/objective pair (\ref eval_interface, \ref objective_interface)
!!
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_optimizee

  use mo_kind, only : dp
  use mo_message, only : error_message
  use mo_opt_eval_utils, only : eval_interface, objective_interface

  implicit none

  public :: optimizee
  public :: function_optimizee
  public :: likelihood_optimizee
  public :: eval_optimizee

  private

  !> \brief abstract type 'optimizee' to be used by optimizers
  type, abstract :: optimizee
  contains
    !> \brief evaluate the optimizee
    procedure(evaluate_interface), deferred :: evaluate
  end type optimizee

  !> \brief Abstract interface for deferred procedures
  abstract interface
    function evaluate_interface(self, parameters, sigma, stddev_new, likeli_new) result(value)
      use mo_kind, only : dp
      import :: optimizee
      class(optimizee), intent(inout) :: self
      real(dp), intent(in), dimension(:) :: parameters !< parameter set
      real(dp), intent(in), optional     :: sigma      !< likelihood: standard deviation of data
      real(dp), intent(out), optional    :: stddev_new !< likelihood: standard deviation of errors using parameters
      real(dp), intent(out), optional    :: likeli_new !< likelihood: likelihood using stddev_new
      real(dp)                           :: value      !< output value
    end function evaluate_interface
  end interface

  !> \brief Optimizee for a simple function: f(vec)
  type, extends(optimizee) :: function_optimizee
    procedure(func_interface), pointer, nopass :: func_pointer => null()  !< Pointer to the function
  contains
    procedure :: evaluate => evaluate_function
  end type function_optimizee

  !> \brief Abstract interface for the function pointer
  abstract interface
    function func_interface(parameters) result(value)
      use mo_kind, only : dp
      real(dp), dimension(:), intent(in) :: parameters !< parameter set
      real(dp)                           :: value      !< output value
    end function func_interface
  end interface

  !> \brief Optimizee for a likelihood function: f(vec, sigma, stddev_new, likeli_new)
  type, extends(optimizee) :: likelihood_optimizee
    procedure(likelihood_interface), pointer, nopass :: likelihood_pointer => null()  !< Pointer to the likelihood
  contains
    procedure :: evaluate => evaluate_likelihood
  end type likelihood_optimizee

  !> \brief Abstract interface for the likelihood pointer
  abstract interface
    function likelihood_interface(parameters, sigma, stddev_new, likeli_new) result(value)
      use mo_kind, only : dp
        real(dp), dimension(:), intent(in) :: parameters !< parameter set
        real(dp), intent(in), optional     :: sigma      !< standard deviation of data
        real(dp), intent(out), optional    :: stddev_new !< standard deviation of errors using parameters
        real(dp), intent(out), optional    :: likeli_new !< likelihood using stddev_new,
        real(dp)                           :: value      !< output value
      end function likelihood_interface
  end interface

  !> \brief Optimizee for a eval-objective pair
  type, extends(optimizee) :: eval_optimizee
    procedure(eval_interface), pointer, nopass :: eval_pointer => null()  !< Pointer to the eval
    procedure(objective_interface), pointer, nopass :: obj_pointer => null()  !< Pointer to the objective
  contains
    procedure :: evaluate => evaluate_obj_eval
  end type eval_optimizee

contains

  !> \brief Implementation of the evaluate procedure for a simple function
  function evaluate_function(self, parameters, sigma, stddev_new, likeli_new) result(value)
      class(function_optimizee), intent(inout) :: self
      real(dp), dimension(:), intent(in) :: parameters !< parameter set
      real(dp), intent(in), optional     :: sigma      !< likelihood: standard deviation of data
      real(dp), intent(out), optional    :: stddev_new !< likelihood: standard deviation of errors using parameters
      real(dp), intent(out), optional    :: likeli_new !< likelihood: likelihood using stddev_new
      real(dp)                           :: value      !< output value

      ! Ensure the function pointer is set
      if (.not. associated(self%func_pointer)) then
          call error_message("Function pointer is not set in function_optimizee!")
      end if

      ! Check optional arguments
      if (present(sigma) .or. present(stddev_new) .or. present(likeli_new)) then
        call error_message("function_optimizee doesn't support 'sigma', 'stddev_new' or 'likeli_new'!")
      end if

      ! Call the function pointer
      value = self%func_pointer(parameters)
  end function evaluate_function

  !> \brief Implementation of the evaluate procedure for a likelihood function
  function evaluate_likelihood(self, parameters, sigma, stddev_new, likeli_new) result(value)
    class(likelihood_optimizee), intent(inout) :: self
    real(DP), dimension(:), intent(in) :: parameters
    real(DP), intent(in),  optional :: sigma
    real(DP), intent(out), optional :: stddev_new
    real(DP), intent(out), optional :: likeli_new
    real(DP) :: value

    ! Ensure the likelihood function pointer is set
    if (.not. associated(self%likelihood_pointer)) then
        call error_message("Likelihood function pointer is not set in likelihood_optimizee!")
    end if

    ! Call the likelihood function pointer
    value = self%likelihood_pointer(parameters, sigma, stddev_new, likeli_new)
  end function evaluate_likelihood

  !> \brief Implementation of the evaluate procedure for a eval-objective pair
  function evaluate_obj_eval(self, parameters, sigma, stddev_new, likeli_new) result(value)
    class(eval_optimizee), intent(inout) :: self
    real(DP), dimension(:), intent(in) :: parameters
    real(DP), intent(in),  optional :: sigma
    real(DP), intent(out), optional :: stddev_new
    real(DP), intent(out), optional :: likeli_new
    real(DP) :: value

    ! Ensure the eval function pointer is set
    if (.not. associated(self%eval_pointer)) then
      call error_message("Eval function pointer is not set in eval_optimizee!")
    end if
    ! Ensure the objective function pointer is set
    if (.not. associated(self%obj_pointer)) then
      call error_message("Objective function pointer is not set in eval_optimizee!")
    end if

    ! Call the objective function pointer
    value = self%obj_pointer(parameters, self%eval_pointer, sigma, stddev_new, likeli_new)
  end function evaluate_obj_eval

end module mo_optimizee
