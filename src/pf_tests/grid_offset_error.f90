!> Small subprocess driver for terminating grid-offset validation paths.
program grid_offset_error
  use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use mo_kind, only: i4, dp
  use mo_grid, only: grid_t, cartesian, spherical
  use mo_grid_helper, only: check_offset
  use mo_grid_scaler, only: scaler_t, up_fraction
  implicit none
  type(grid_t), target :: fine, coarse
  type(scaler_t) :: scaler
  integer(i4) :: offset(2), dims(2)
  real(dp) :: corner(2)
  logical :: fine_mask(4,4), coarse_mask(2,2), periodic_mask(12,2)
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
    fine = coarse%derive_fine_grid(1.0_dp, llcorner=[-1.0_dp,0.0_dp])
  case ('fine_excludes')
    fine = coarse%derive_fine_grid(1.0_dp, llcorner=[1.0_dp,0.0_dp])
  case ('coarse_empty')
    coarse = fine%derive_coarse_grid(2.0_dp, llcorner=[-2.0_dp,0.0_dp])
  case ('coarse_excludes')
    coarse = fine%derive_coarse_grid(2.0_dp, llcorner=[1.0_dp,0.0_dp])
  case ('origin_fractional')
    coarse = fine%derive_grid(target_resolution=2.0_dp, llcorner=[0.5_dp,0.0_dp], estimate_aux=.false.)
  case ('fine_origin_fractional')
    call coarse%gen_fine_grid(fine, 1.0_dp, llcorner=[0.5_dp,0.0_dp], estimate_aux=.false.)
  case ('origin_nonfinite')
    coarse = fine%derive_coarse_grid(2.0_dp, &
      llcorner=[ieee_value(0.0_dp,ieee_quiet_nan),0.0_dp], estimate_aux=.false.)
  case ('dims_zero')
    coarse = fine%derive_coarse_grid(2.0_dp, dims=[0_i4,2_i4], estimate_aux=.false.)
  case ('fine_dims_negative')
    fine = coarse%derive_fine_grid(1.0_dp, dims=[2_i4,-1_i4], estimate_aux=.false.)
  case ('coarse_pad_upper')
    coarse = fine%derive_coarse_grid(2.0_dp, dims=[3_i4,2_i4])
  case ('fine_pad_upper')
    fine = coarse%derive_fine_grid(1.0_dp, dims=[4_i4,5_i4])
  case ('coarse_excludes_no_aux')
    coarse = fine%derive_coarse_grid(2.0_dp, llcorner=[1.0_dp,0.0_dp], estimate_aux=.false.)
  case ('fine_excludes_no_aux')
    fine = coarse%derive_fine_grid(1.0_dp, llcorner=[1.0_dp,0.0_dp], estimate_aux=.false.)
  case ('coarse_dimension_overflow')
    coarse = fine%derive_coarse_grid(1.0_dp, llcorner=[-real(huge(0_i4),dp),0.0_dp], estimate_aux=.false.)
  case ('fine_dimension_overflow')
    fine = coarse%derive_fine_grid(1.0_dp, llcorner=[-real(huge(0_i4),dp),0.0_dp], estimate_aux=.false.)
  case ('mask_origin_fractional')
    call fine%mask_extent(2.0_dp, dims, origin=[0.5_dp,0.0_dp])
  case ('mask_fine_origin_fractional')
    call coarse%mask_extent(1.0_dp, dims, origin=[0.5_dp,0.0_dp])
  case ('mask_origin_nonfinite')
    call fine%mask_extent(2.0_dp, dims, origin=[ieee_value(0.0_dp,ieee_quiet_nan),0.0_dp])
  case ('mask_origin_excludes')
    call fine%mask_extent(2.0_dp, dims, origin=[1.0_dp,0.0_dp])
  case ('mask_fine_origin_excludes')
    call coarse%mask_extent(1.0_dp, dims, origin=[0.0_dp,1.0_dp])
  case ('mask_empty_excludes')
    fine_mask = .false.
    call fine%init(4_i4, 4_i4, cellsize=1.0_dp, mask=fine_mask)
    call fine%mask_extent(2.0_dp, dims, origin=[1.0_dp,0.0_dp])
  case ('mask_coarse_dimension_overflow')
    call fine%mask_extent(1.0_dp, dims, origin=[-real(huge(0_i4),dp),0.0_dp])
  case ('mask_fine_dimension_overflow')
    call coarse%mask_extent(1.0_dp, dims, origin=[-real(huge(0_i4),dp),0.0_dp])
  case ('mask_resolution_ratio')
    call fine%mask_extent(1.5_dp, dims)
  case ('mask_resolution_nonfinite')
    call fine%mask_extent(ieee_value(0.0_dp,ieee_quiet_nan), dims)
  case ('periodic_up_west', 'periodic_up_east', 'periodic_up_fill')
    call fine%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    call coarse%init(1_i4, 1_i4, xllcorner=330.0_dp, yllcorner=-30.0_dp, cellsize=60.0_dp, coordsys=spherical)
    if (scenario == 'periodic_up_west') coarse%xllcorner = -30.0_dp
    call scaler%init(fine, coarse, fill_missing=scenario == 'periodic_up_fill')
  case ('periodic_down_west', 'periodic_down_east')
    call coarse%init(6_i4, 1_i4, yllcorner=-30.0_dp, cellsize=60.0_dp, coordsys=spherical)
    call fine%init(1_i4, 1_i4, xllcorner=360.0_dp, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    if (scenario == 'periodic_down_west') fine%xllcorner = -30.0_dp
    call scaler%init(coarse, fine)
  case ('periodic_equal_east')
    call coarse%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    call fine%init(1_i4, 1_i4, xllcorner=360.0_dp, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    call scaler%init(coarse, fine)
  case ('periodic_global_up', 'periodic_global_down', 'periodic_global_equal')
    call fine%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    call coarse%init(6_i4, 1_i4, xllcorner=30.0_dp, yllcorner=-30.0_dp, cellsize=60.0_dp, coordsys=spherical)
    if (scenario == 'periodic_global_equal') &
      call coarse%init(12_i4, 2_i4, xllcorner=30.0_dp, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    if (scenario == 'periodic_global_down') then
      call scaler%init(coarse, fine)
    else
      call scaler%init(fine, coarse)
    end if
  case ('periodic_coarse_west', 'periodic_coarse_east', 'periodic_coarse_empty')
    periodic_mask = scenario /= 'periodic_coarse_empty'
    call fine%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical, mask=periodic_mask)
    if (scenario == 'periodic_coarse_west') then
      coarse = fine%derive_coarse_grid(60.0_dp, llcorner=[-30.0_dp,-30.0_dp], dims=[1_i4,1_i4], estimate_aux=.false.)
    else
      coarse = fine%derive_coarse_grid(60.0_dp, llcorner=[330.0_dp,-30.0_dp], dims=[1_i4,1_i4], estimate_aux=.false.)
    end if
  case ('periodic_fine_west', 'periodic_fine_east')
    call coarse%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    if (scenario == 'periodic_fine_west') then
      fine = coarse%derive_fine_grid(15.0_dp, llcorner=[-15.0_dp,-30.0_dp], dims=[2_i4,2_i4], estimate_aux=.false.)
    else
      fine = coarse%derive_fine_grid(15.0_dp, llcorner=[345.0_dp,-30.0_dp], dims=[2_i4,2_i4], estimate_aux=.false.)
    end if
  case ('periodic_coarse_default')
    call fine%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    coarse = fine%derive_coarse_grid(150.0_dp, estimate_aux=.false.)
  case ('periodic_fine_default')
    call coarse%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical)
    fine = coarse%derive_fine_grid(15.0_dp, llcorner=[-15.0_dp,-30.0_dp], estimate_aux=.false.)
  case ('periodic_mask_padding')
    periodic_mask = .false.
    periodic_mask(12,:) = .true.
    call fine%init(12_i4, 2_i4, yllcorner=-30.0_dp, cellsize=30.0_dp, coordsys=spherical, mask=periodic_mask)
    call fine%mask_extent(60.0_dp, dims, llcorner=corner)
    coarse = fine%derive_coarse_grid(60.0_dp, dims=dims, llcorner=corner, estimate_aux=.false.)
  case ('no_overlap')
    coarse%xllcorner = 10.0_dp
    call coarse%check_is_filled_by(fine, check_mask=.false.)
  case ('coordinate_system')
    coarse%coordsys = spherical
    offset = fine%get_offset(coarse)
  case ('resolution_ratio')
    coarse%cellsize = 1.5_dp
    offset = fine%get_offset(coarse)
  case ('equal_mask')
    fine_mask = .true.
    fine_mask(2,2) = .false.
    call fine%init(4_i4,4_i4,cellsize=1.0_dp,mask=fine_mask)
    call coarse%init(4_i4,4_i4,cellsize=1.0_dp)
    call scaler%init(fine,coarse)
  case ('equal_fill_geometry')
    call coarse%init(4_i4,4_i4,cellsize=1.0_dp,xllcorner=1.0_dp)
    call scaler%init(fine,coarse,fill_missing=.true.)
  case ('equal_no_donor')
    fine_mask = .false.
    call fine%init(4_i4,4_i4,cellsize=1.0_dp,mask=fine_mask)
    call coarse%init(4_i4,4_i4,cellsize=1.0_dp)
    call scaler%init(fine,coarse,fill_missing=.true.)
  case ('equal_real_fraction')
    call scaler%init(fine,fine)
    block
      real(dp) :: result(16)
      call scaler%execute(spread(1.0_dp,1,16),result,upscaling_operator=up_fraction)
    end block
  case ('equal_integer_fraction')
    call scaler%init(fine,fine)
    block
      integer(i4) :: result(16)
      call scaler%execute(spread(1_i4,1,16),result,upscaling_operator=up_fraction)
    end block
  case ('fill_geometry')
    fine%xllcorner = -1.0_dp
    call scaler%init(coarse, fine, fill_missing=.true.)
  case default
    error stop 2
  end select
  ! Reaching here is a validation failure: the CTest wrapper requires exit code 1.
end program grid_offset_error
