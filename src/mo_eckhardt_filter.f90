!> \file    mo_eckhardt_filter.f90
!> \copydoc mo_eckhardt_filter

!> \brief   Eckhardt filter for baseflow index calculation.
!> \details This module provides routines for the Eckardt filter to analyse discharge time series and extract the baseflow.
!!          The filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
!> \version 0.1
!> \authors Sebastian Mueller
!> \authors Mariaines Di Dato
!> \date    Apr 2022
module mo_eckhardt_filter

  use mo_kind,       only: i4, dp
  use mo_moment,     only: mean
  use mo_percentile, only: percentile
  use mo_append,     only: append
  use mo_nelmin,     only: nelminrange

  implicit none
  private
  public :: fit_alpha
  public :: eckhardt_filter_fit
  public :: eckhardt_filter
  public :: weekly_average
  public :: BFI

  real(dp), allocatable :: temp_d7(:)
  logical, allocatable :: temp_qmin_mask(:)

contains

  !> \brief   Eckhardt filter for baseflow calculation from discharge time series with fitting.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  baseflow
  function eckhardt_filter_fit(discharge) result(baseflow)

    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)

    real(dp), allocatable :: baseflow(:)

    real(dp) :: alpha

    alpha = fit_alpha(discharge)
    baseflow = eckhardt_filter(alpha, discharge)

  end function eckhardt_filter_fit

  !> \brief   Fitted alpha parameter for the Eckhardt filter.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  alpha parameter for eckard filter
  real(dp) function fit_alpha(discharge)

    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)

    real(dp) :: alpha_out(1)
    real(dp), allocatable :: q_min(:)
    integer(i4) :: i

    temp_d7 = weekly_average(discharge)
    do i = 1, size(temp_d7), 365
      call append(q_min, minval(temp_d7(i : min(i+364, size(temp_d7)))))
    end do

    allocate(temp_qmin_mask(size(discharge)))
    temp_qmin_mask = (temp_d7 < percentile(q_min, 10.0_dp, mode_in=4))

    alpha_out = nelminrange( &
      func=func, &
      pstart=[0.9_dp], &
      prange=reshape([0._dp, 1._dp], [1, 2]) &
    )
    fit_alpha = alpha_out(1)

    deallocate(temp_qmin_mask)
    deallocate(temp_d7)

  end function fit_alpha

  !> \brief   Eckhardt filter for baseflow calculation from discharge time series.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  baseflow
  function eckhardt_filter(alpha, discharge) result(baseflow)

    !> filter parameter
    real(dp), intent(in)  :: alpha
    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)

    real(dp), allocatable :: baseflow(:)

    real(dp), allocatable :: d7(:), d7_perc(:)
    real(dp) :: BFI_max
    integer(i4) :: i

    allocate(baseflow(size(discharge)))
    allocate(d7_perc(size(discharge)))

    ! 20 percent percentile with Linear interpolation
    d7 = weekly_average(discharge)
    d7_perc(:) = percentile(d7, 20.0_dp, mode_in=4)
    BFI_max = BFI(d7_perc, discharge)

    ! Applying the equation Eq. (6) from Eckhardt (2008)
    baseflow(1) = ((1 - alpha)*BFI_max * discharge(1)) / (1 - alpha*BFI_max)
    do i = 2, size(discharge)
      baseflow(i) = ((1 - BFI_max)*alpha*baseflow(i-1) + (1 - alpha)*BFI_max*discharge(i)) / (1 - alpha*BFI_max)
    end do

  end function eckhardt_filter

  !> \brief   This function returns the 7days-averaged discharge.
  !> \return  array with weekly moving average
  function weekly_average(discharge) result(d7)

    !> array with daily discharge
    real(dp), intent(in) :: discharge(:)

    real(dp), allocatable :: d7(:)

    integer(i4) :: i

    allocate(d7(size(discharge)))

    do i = 1, size(discharge)
      d7(i) = mean(discharge(max(1,i-3) : min(size(discharge),i+3)))
    end do

  end function weekly_average

  !> \brief   Calculate the baseflow index as ratio between baseflow and discharge.
  !> \return  baseflow index
  real(dp) function BFI(baseflow, discharge)

    !> array with daily baseflow values
    real(dp), intent(in) :: baseflow(:)
    !> array with daily discharge
    real(dp), intent(in) :: discharge(:)

    BFI = sum(baseflow) / sum(discharge)

  end function BFI

  !> \brief   Target function for fitting Eckhardt filter.
  !> \return  Objective value.
  function func(pp)

    real(dp), dimension(:), intent(in) :: pp  !< alpha (single value)

    real(dp) :: func

    real(dp), allocatable :: baseflow(:)

    baseflow = eckhardt_filter(alpha=pp(1), discharge=temp_d7)
    func = mean((baseflow/temp_d7 - 1)**2, mask=temp_qmin_mask)

  end function func

end module mo_eckhardt_filter
