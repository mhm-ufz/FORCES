!> \file    mo_eckhardt_filter.f90
!> \copydoc mo_eckhardt_filter

!> \brief   Eckhardt filter for baseflow index calculation.
!> \details This module provides routines for the Eckardt filter to analyse discharge time series and extract the baseflow.
!!          The filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
!> \version 0.1
!> \authors Sebastian Mueller
!> \authors Mariaines Di Dato
!> \date    Apr 2022
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_eckhardt_filter

  use mo_kind,       only: i4, dp
  use mo_moment,     only: mean
  use mo_percentile, only: percentile
  use mo_append,     only: append
  use mo_nelmin,     only: nelminrange
  use mo_message,    only: error_message

  implicit none
  private
  public :: fit_alpha
  public :: eckhardt_filter_fit
  public :: eckhardt_filter
  public :: weekly_average
  public :: BFI

  real(dp), allocatable :: temp_d7(:)
  logical, allocatable :: temp_qmin_mask(:), temp_mask(:)

contains

  !> \brief   Eckhardt filter for baseflow calculation from discharge time series with fitting.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  baseflow
  function eckhardt_filter_fit(discharge, mask) result(baseflow)

    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)
    !> mask for daily discharge
    logical, intent(in), optional :: mask(:)

    real(dp), allocatable :: baseflow(:)

    real(dp) :: alpha

    alpha = fit_alpha(discharge, mask=mask)
    baseflow = eckhardt_filter(alpha, discharge, mask=mask)

  end function eckhardt_filter_fit

  !> \brief   Fitted alpha parameter for the Eckhardt filter.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  alpha parameter for eckard filter
  real(dp) function fit_alpha(discharge, mask)

    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)
    !> mask for daily discharge
    logical, intent(in), optional :: mask(:)

    real(dp) :: alpha_out(1)
    real(dp), allocatable :: q_min(:), dummy(:)
    logical, dimension(size(discharge)) :: mask_
    integer(i4) :: i, j

    mask_(:) = .true.
    if ( present(mask) ) mask_ = mask

    temp_d7 = weekly_average(discharge, mask=mask)
    allocate(q_min(0))
    do i = 1, size(temp_d7), 365
      j = min(i+364, size(temp_d7))
      ! only use values in mask
      ! TODO: do we need a threshold for number in mask here?
      if ( any(mask_(i : j)) ) call append(q_min, minval(temp_d7(i : j), mask=mask_(i : j)))
    end do
    if ( size(q_min) < 2 ) call error_message("Eckhardt filter: Less than 2 years of discharge observations! (min. 10 recommended)")

    allocate(temp_qmin_mask(size(discharge)))
    allocate(temp_mask(size(discharge)))
    temp_mask = mask_
    temp_qmin_mask = (temp_d7 < percentile(q_min, 10.0_dp, mode_in=4))
    temp_qmin_mask = temp_qmin_mask .and. temp_mask

    ! set values outside of mask to 1 in d7
    allocate(dummy(count(.not.mask_)))
    dummy(:) = 1.0_dp  ! [1.0_dp, i=1,count(.not.mask_)]
    temp_d7 = unpack( &
      vector=dummy, &
      mask=.not.mask_, &
      field=temp_d7 &
    )
    deallocate(dummy)

    alpha_out = nelminrange( &
      func=func, &
      pstart=[0.9_dp], &
      prange=reshape([0._dp, 1._dp], [1, 2]) &
    )
    fit_alpha = alpha_out(1)

    deallocate(temp_qmin_mask)
    deallocate(temp_mask)
    deallocate(temp_d7)

  end function fit_alpha

  !> \brief   Eckhardt filter for baseflow calculation from discharge time series.
  !> \details This filter was proposed in Eckhardt (2008, doi: 10.1016/j.jhydrol.2008.01.005)
  !> \return  baseflow
  function eckhardt_filter(alpha, discharge, mask) result(baseflow)

    !> filter parameter
    real(dp), intent(in)  :: alpha
    !> array with daily discharge
    real(dp), intent(in)  :: discharge(:)
    !> mask for daily discharge
    logical, intent(in), optional :: mask(:)

    real(dp), allocatable :: baseflow(:)

    real(dp), allocatable :: d7(:), d7_perc(:), d_temp(:), b_temp(:)
    logical, dimension(size(discharge)) :: mask_
    real(dp) :: BFI_max
    integer(i4) :: i

    mask_(:) = .true.
    if ( present(mask) ) mask_ = mask

    allocate(baseflow(size(discharge)))
    allocate(d7_perc(size(discharge)))

    ! 20 percent percentile with Linear interpolation
    d7 = weekly_average(discharge, mask=mask)
    d7_perc(:) = percentile(d7, 20.0_dp, mask=mask, mode_in=4)
    BFI_max = BFI(d7_perc, discharge, mask=mask)

    allocate(b_temp(count(mask_)))
    allocate(d_temp(count(mask_)))
    d_temp = pack(discharge, mask=mask_)

    ! Applying the equation Eq. (6) from Eckhardt (2008) (only at mask)
    b_temp(1) = ((1 - alpha)*BFI_max * d_temp(1)) / (1 - alpha*BFI_max)
    do i = 2, size(d_temp)
      b_temp(i) = ((1 - BFI_max)*alpha*b_temp(i-1) + (1 - alpha)*BFI_max*d_temp(i)) / (1 - alpha*BFI_max)
    end do
    baseflow(:) = 0.0_dp
    baseflow = unpack(vector=b_temp, mask=mask_, field=baseflow)

  end function eckhardt_filter

  !> \brief   This function returns the 7days-averaged discharge.
  !> \return  array with weekly moving average
  function weekly_average(discharge, mask) result(d7)

    !> array with daily discharge
    real(dp), intent(in) :: discharge(:)
    !> mask for daily discharge
    logical, intent(in), optional :: mask(:)

    real(dp), allocatable :: d7(:)

    logical, dimension(size(discharge)) :: mask_
    integer(i4) :: i, n, m

    mask_(:) = .true.
    if ( present(mask) ) mask_ = mask

    allocate(d7(size(discharge)))
    d7(:) = 0.0_dp

    do i = 1, size(discharge)
      n = max(1,i-3)
      m = min(size(discharge),i+3)
      ! TODO: do we need a threshold for number in mask here?
      if ( any(mask_(n : m)) ) d7(i) = mean(discharge(n : m), mask=mask_(n : m))
    end do

  end function weekly_average

  !> \brief   Calculate the baseflow index as ratio between baseflow and discharge.
  !> \return  baseflow index
  real(dp) function BFI(baseflow, discharge, mask)

    !> array with daily baseflow values
    real(dp), intent(in) :: baseflow(:)
    !> array with daily discharge
    real(dp), intent(in) :: discharge(:)
    !> mask for daily discharge
    logical, intent(in), optional :: mask(:)

    logical, dimension(size(discharge)) :: mask_

    mask_(:) = .true.
    if ( present(mask) ) mask_ = mask

    BFI = sum(baseflow, mask=mask_) / sum(discharge, mask=mask_)

  end function BFI

  !> \brief   Target function for fitting Eckhardt filter.
  !> \return  Objective value.
  function func(pp)

    real(dp), dimension(:), intent(in) :: pp  !< alpha (single value)

    real(dp) :: func

    real(dp), allocatable :: baseflow(:)

    baseflow = eckhardt_filter(alpha=pp(1), discharge=temp_d7, mask=temp_mask)
    func = mean((baseflow/temp_d7 - 1)**2, mask=temp_qmin_mask)

  end function func

end module mo_eckhardt_filter
