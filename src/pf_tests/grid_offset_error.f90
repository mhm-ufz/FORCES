!> Small subprocess driver for terminating grid-offset validation paths.
program grid_offset_error
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use mo_kind, only: i4, dp
  use mo_grid, only: grid_t, cartesian, spherical
  use mo_grid_helper, only: check_offset
  use mo_grid_scaler, only: scaler_t
  implicit none
  type(grid_t), target :: fine, coarse
  type(scaler_t) :: scaler
  integer(i4) :: offset(2)
  logical :: fine_mask(4,4), coarse_mask(2,2)
  character(len=40) :: scenario

  call get_command_argument(1, scenario)
  call fine%init(4_i4, 4_i4, cellsize=1.0_dp, coordsys=cartesian)
  call coarse%init(2_i4, 2_i4, cellsize=2.0_dp, coordsys=cartesian)
  select case (trim(scenario))
  case ('fractional')
    coarse%xllcorner = 0.5_dp
    offset = fine%get_offset(coarse)
  case ('offset_range')
    call check_offset([0.0_dp, 0.0_dp], [real(huge(0_i4), dp) + 1.0_dp, 0.0_dp], 1.0_dp, offset)
  case ('offset_range_negative')
    call check_offset([0.0_dp, 0.0_dp], [-real(huge(0_i4), dp) - 2.0_dp, 0.0_dp], 1.0_dp, offset)
  case ('offset_range_large')
    call check_offset([0.0_dp, 0.0_dp], [huge(0.0_dp), 0.0_dp], 1.0_dp, offset)
  case ('offset_nonfinite')
    call check_offset([0.0_dp, 0.0_dp], [ieee_value(0.0_dp, ieee_quiet_nan), 0.0_dp], 1.0_dp, offset)
  case ('lost_coverage')
    fine%xllcorner = -1.0_dp
    call fine%check_is_covered_by(coarse)
  case ('uncovered_mask')
    coarse_mask = .false.
    call coarse%init(2_i4, 2_i4, cellsize=2.0_dp, coordsys=cartesian, mask=coarse_mask)
    call fine%check_is_covered_by(coarse)
  case ('unfilled_mask')
    fine_mask = .false.
    call fine%init(4_i4, 4_i4, cellsize=1.0_dp, coordsys=cartesian, mask=fine_mask)
    call coarse%check_is_filled_by(fine)
  case ('fine_padding')
    fine = coarse%derive_fine_grid(1.0_dp, offset=[1_i4, 0_i4])
  case ('fine_excludes')
    fine = coarse%derive_fine_grid(1.0_dp, offset=[-1_i4, 0_i4])
  case ('coarse_empty')
    coarse = fine%derive_coarse_grid(2.0_dp, offset=[-2_i4, 0_i4])
  case ('coarse_excludes')
    coarse = fine%derive_coarse_grid(2.0_dp, offset=[1_i4, 0_i4])
  case ('no_overlap')
    coarse%xllcorner = 10.0_dp
    call coarse%check_is_filled_by(fine, check_mask=.false.)
  case ('coordinate_system')
    coarse%coordsys = spherical
    offset = fine%get_offset(coarse)
  case ('resolution_ratio')
    coarse%cellsize = 1.5_dp
    offset = fine%get_offset(coarse)
  case ('fill_geometry')
    fine%xllcorner = -1.0_dp
    call scaler%init(coarse, fine, fill_missing=.true.)
  case default
    error stop 2
  end select
  ! Reaching here is a validation failure: the CTest wrapper requires exit code 1.
end program grid_offset_error
