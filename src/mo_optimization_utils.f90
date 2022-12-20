!> \file mo_optimization_utils.f90
!> \brief \copybrief mo_optimization_utils
!> \details \copydetails mo_optimization_utils

!> \brief Utility functions, such as interface definitions, for optimization routines.
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_optimization_utils

  use mo_kind, only : dp

  implicit none

  !> \brief Interface for evaluation routine.
  abstract interface
    subroutine eval_interface(parameterset, opti_domain_indices, runoff, smOptiSim, neutronsOptiSim, etOptiSim, twsOptiSim, BFI)
      use mo_kind, only : dp, i4
      use mo_optimization_types, only : optidata_sim
      real(dp),    dimension(:), intent(in) :: parameterset
      integer(i4), dimension(:),                 optional, intent(in)  :: opti_domain_indices
      real(dp),    dimension(:, :), allocatable, optional, intent(out) :: runoff   !< dim1=time dim2=gauge
      type(optidata_sim), dimension(:), optional, intent(inout) :: smOptiSim       !< dim1=ncells, dim2=time
      type(optidata_sim), dimension(:), optional, intent(inout) :: neutronsOptiSim !< dim1=ncells, dim2=time
      type(optidata_sim), dimension(:), optional, intent(inout) :: etOptiSim       !< dim1=ncells, dim2=time
      type(optidata_sim), dimension(:), optional, intent(inout) :: twsOptiSim      !< dim1=ncells, dim2=time
      real(dp),    dimension(:), allocatable, optional, intent(out) :: BFI         !< baseflow index, dim1=domainID
    end subroutine
  end interface

  !> \brief Interface for objective function.
  interface
    function objective_interface (parameterset, eval, arg1, arg2, arg3)
      use mo_kind, only : dp
      import eval_interface
      real(dp), intent(in), dimension(:) :: parameterset !< parameter set
      procedure(eval_interface), INTENT(IN), pointer :: eval !< evaluation routine
      real(dp), optional, intent(in) :: arg1 !< optional argument 1
      real(dp), optional, intent(out) :: arg2 !< optional argument 2
      real(dp), optional, intent(out) :: arg3 !< optional argument 3

      real(dp) :: objective_interface
    end function objective_interface
  end interface

end module mo_optimization_utils
