!> \file mo_dds.f90
!> \copydoc mo_dds

!> \brief Dynamically Dimensioned Search (DDS)
!> \details This module provides routines for Dynamically Dimensioned Search (DDS)
!! of Tolson and Shoemaker (2007). It searches the minimum or maximum of a user-specified function,
!! using an n-dimensional continuous global optimization algorithm (DDS).
!> \authors Bryan Tolson, modified by Rohini Kumar, Matthias Cuntz and Juliane Mai.
!> \date Jul 2012
!> \copyright Copyright 2005-\today, the CHS Developers, Sabine Attinger: All rights reserved.
!! FORCES is released under the LGPLv3+ license \license_note
module mo_dds

  use mo_optimizee, only : optimizee

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: DDS    ! Dynamically Dimensioned Search (DDS)
  PUBLIC :: MDDS   ! Modified Dynamically Dimensioned Search (DDS)

  ! ------------------------------------------------------------------

CONTAINS

  ! ------------------------------------------------------------------

  !>        \brief DDS

  !>        \details Searches Minimum or Maximum of a user-specified function using
  !!        Dynamically Dimensioned Search (DDS).\n
  !!        DDS is an n-dimensional continuous global optimization algorithm.
  !!        It is coded as a minimizer but one can give maxit=True in a maximization problem,
  !!        so that the algorithm minimizes the negative of the objective function F=(-1*F).
  !!
  !!        The function to be minimized is the first argument of DDS and must be defined as \n
  !!        \code{.f90}
  !!        use mo_optimizee, only: optimizee
  !!        type, extends(optimizee) :: your_optimizee
  !!        contains
  !!          ! to be implemented with signature: your_evaluate(self, parameters, sigma, stddev_new, likeli_new)
  !!          procedure :: evaluate => your_evaluate
  !!        end type function_optimizee
  !!        \endcode
  !!
  !!        \b Example
  !!
  !!        \code{.f90}
  !!        use mo_opt_functions, only: griewank
  !!        use mo_optimizee, only: function_optimizee
  !!        dv_range(:,1) = (/ -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0 /)
  !!        dv_range(:,2) = (/ 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0 /)
  !!        dv_ini        = (/ -.226265E+01, -.130187E+01, -.151219E+01, 0.133983E+00, 0.988159E+00, &
  !!                           -.495074E+01, -.126574E+02, 0.572684E+00, 0.303864E+01, 0.343031E+01 /)
  !!        objective%func_pointer => griewank
  !!        dv_opt = DDS(objective, dv_ini, dv_range)
  !!        \endcode
  !!
  !!        See also example in test directory.
  !!
  !!        \b Literature
  !!        1. Tolson, B. A., and C. A. Shoemaker (2007)
  !!            _Dynamically dimensioned search algorithm for computationally efficient watershed
  !!            model calibration_, Water Resour. Res., 43, W01413, doi:10.1029/2005WR004723.
  !!
  !>        \param[inout] "class(optimizee) :: objective"               Objective to search the optimum
  !>        \param[in] "real(dp) :: pini(:)"                 inital value of decision variables
  !>        \param[in] "real(dp) :: prange(size(pini),2)"    Min/max range of decision variables
  !>        \param[in] "real(dp), optional           :: r"                 DDS perturbation parameter\n
  !!                                                                       (default: 0.2)
  !>        \param[in] "integer(i8), optional        :: seed"              User seed to initialise the random number generator
  !!                                                                       (default: None)
  !>        \param[in] "integer(i8), optional        :: maxiter"           Maximum number of iteration or function evaluation
  !!                                                                       (default: 1000)
  !>        \param[in] "logical, optional            :: maxit"             Maximization (.True.) or
  !!                                                                       minimization (.False.) of function
  !!                                                                       (default: .False.)
  !>        \param[in] "logical, optional            :: mask(size(pini))"  parameter to be optimized (true or false)
  !!                                                                       (default: .True.)
  !>        \param[in]  "character(len=*) , optional :: tmp_file"          file with temporal output
  !>        \param[out] "real(dp), optional              :: funcbest"    the best value of the function.
  !>        \param[out] "real(dp), optional, allocatable :: history(:)"  the history of best function values,
  !!                                                                     history(maxiter)=funcbest\n
  !!                                          allocatable only to be in correspondance with other optimization routines
  !>        \retval "real(dp) :: DDS"   The parameters of the point which is estimated to minimize the function.

  !>        \author Bryan Tolson
  !>        \date Feb 2007

  !>        \author Rohini Kumar
  !>        \date Feb 2008

  !>        \author Matthias Cuntz & Juliane Mai
  !>        \date Jul 2012
  !!          - module

  !>        \author Juliane Mai
  !>        \date Aug 2012
  !!          - optional argument funcbest added
  !>        \date Nov 2012
  !!          - masked parameter
  !>        \date Dec 2012
  !!          - history output

  !>        \author Pallav Kumar Shrestha
  !>        \date Jun 2019
  !!          - Added "dds_results.out.current" output file with current iteration (non-updated) parameters

#ifdef MPI
  function DDS(objective, pini, prange, r, seed, maxiter, maxit, mask, tmp_file, comm, funcbest, history)
#else
  function DDS(objective, pini, prange, r, seed, maxiter, maxit, mask, tmp_file, funcbest, history)
#endif

    use mo_kind, only : i4, i8, dp
    use mo_xor4096, only : xor4096, xor4096g
#ifdef MPI
    use mpi_f08
#endif

    implicit none
    class(optimizee), intent(inout) :: objective

    real(dp), dimension(:), intent(in) :: pini     ! inital value of decision variables
    real(dp), dimension(:, :), intent(in) :: prange   ! Min/max values of decision variables
    real(dp), optional, intent(in) :: r        ! DDS perturbation parameter (-> 0.2 by default)
    integer(i8), optional, intent(in) :: seed     ! User seed to initialise the random number generator
    integer(i8), optional, intent(in) :: maxiter  ! Maximum number of iteration or function evaluation
    logical, optional, intent(in) :: maxit    ! Maximization or minimization of function
    logical, dimension(:), optional, intent(in) :: mask     ! parameter to be optimized (true or false)
    character(len = *), optional, intent(in) :: tmp_file    ! file for temporal output
#ifdef MPI
    type(MPI_Comm), optional, intent(in)  :: comm                ! MPI communicator
#endif
    real(dp), optional, intent(out) :: funcbest ! Best value of the function.
    real(dp), dimension(:), optional, intent(out), &
            allocatable :: history  ! History of objective function values
    real(dp), dimension(size(pini)) :: DDS      ! Best value of decision variables

    ! Local variables
    integer(i4) :: pnum                   ! Total number of decision variables
    integer(i8) :: iseed                  ! User given seed
    integer(i8) :: imaxiter               ! Maximum number of iteration or function evaluation
    real(dp) :: ir                     ! DDS perturbation parameter
    real(dp) :: imaxit                 ! Maximization or minimization of function
    real(dp) :: of_new, of_best        ! intermediate results
    real(dp) :: Pn, new_value          ! intermediate results
    real(dp), dimension(size(pini)) :: pnew                   ! Test value of decision variables
    real(dp) :: ranval                 ! random value
    integer(i8) :: i                      ! maxiter=i8
    integer(i4) :: j, dvn_count, dv       ! pnum=i4
    integer(i4) :: idummy                 ! dummy vaiable
    integer, dimension(8) :: sdate                  ! date_and_time return
    logical, dimension(size(pini)) :: maske                  ! parameter to be optimized (true or false)
    integer(i4), dimension(:), allocatable :: truepara               ! parameter to be optimized (their indexes)
#ifdef MPI
    integer(i4) :: ierror
    logical :: do_obj_loop
    integer(i4) :: iproc, nproc
#endif

    ! Check input
    pnum = size(pini)
    if (size(prange, 1) /= pnum) stop 'Error DDS: size(prange,1) /= size(pini)'
    if (size(prange, 2) /= 2)    stop 'Error DDS: size(prange,2) /= 2'
    ! r Perturbation parameter
    ir = 0.2_dp
    if (present(r)) ir = r
    if (ir <= 0.0_dp .or. ir > 1.0_dp) stop 'Error DDS: DDS perturbation parameter (0.0, 1.0]'
    ! max. iteration
    imaxiter = 1000
    if (present(maxiter)) imaxiter = maxiter
    if (imaxiter < 6) stop 'Error DDS: max function evals must be minimum 6'
    ! history output
    if (present(history)) then
      allocate(history(imaxiter))
    end if
    ! Min or max objective function
    imaxit = 1.0_dp
    if (present(maxit)) then
      if (maxit) imaxit = -1.0_dp
    end if
    ! Given seed
    iseed = 0
    if (present(seed)) iseed = seed
    iseed = max(iseed, 0_i8)

    if (present(mask)) then
      if (count(mask) .eq. 0_i4) then
        stop 'Input argument mask: At least one element has to be true'
      else
        maske = mask
      end if
    else
      maske = .true.
    end if

    allocate (truepara(count(maske)))
    idummy = 0_i4
    do j = 1, size(pini, 1)
      if (maske(j)) then
        idummy = idummy + 1_i4
        truepara(idummy) = j
      end if
    end do

    ! Seed random numbers
    if (iseed == 0) then
      call date_and_time(values = sdate)
      iseed = sdate(1) * 31536000000_i8 + sdate(2) * 2592000000_i8 + sdate(3) * 86400000_i8 + &
              sdate(5) * 3600000_i8 + sdate(6) * 60000_i8 + sdate(7) * 1000_i8 + sdate(8)
      call xor4096(iseed, ranval)
      call xor4096g(iseed, ranval)
    else
      call xor4096(iseed, ranval)
      call xor4096g(iseed, ranval)
    end if

    ! Temporal file writing
    if(present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', status = 'unknown')
      write(999, *) '# settings :: general'
      write(999, *) '# nIterations    iseed'
      write(999, *) imaxiter, iseed
      write(999, *) '# settings :: dds specific'
      write(999, *) '# dds_r'
      write(999, *) ir
      write(999, *) '# iter   bestf   (bestx(j),j=1,nopt)'
      close(999)

      ! Second file writing with corresponding parameter set for each iteration rather than the current best
      open(unit = 998, file = trim(adjustl( tmp_file )) // ".current" , action = 'write', status = 'unknown')
      write(998, *) '# settings :: general'
      write(998, *) '# nIterations    iseed'
      write(998, *) imaxiter, iseed
      write(998, *) '# settings :: dds specific'
      write(998, *) '# dds_r'
      write(998, *) ir
      write(998, *) '# iter   bestf   (bestx(j),j=1,nopt)'
      close(998)
    end if

    ! Evaluate initial solution and return objective function value
    ! and Initialise the other variables (e.g. of_best)
    ! imaxit is 1.0 for MIN problems, -1 for MAX problems
    DDS = pini
    print *, imaxit
#ifdef MPI
    call MPI_Comm_size(comm, nproc, ierror)
    do iproc = 1, nproc-1
      do_obj_loop = .true.
      call MPI_Send(do_obj_loop,1, MPI_LOGICAL,iproc,0,comm,ierror)
    end do
#endif
    of_new = imaxit * objective%evaluate(pini)
    of_best = of_new
    if (present(history)) history(1) = of_new

    file_write : if (present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (pnum + 2) * 30)
      open(unit = 998, file = trim(adjustl( tmp_file )) // ".current" , action = 'write', position = 'append', &
        recl = (pnum + 2) * 30)
      if (imaxit .lt. 0.0_dp) then
        ! Maximize
        write(999, *) '0', -of_best, pini
        write(998, *) '0', -of_best, pini
      else
        ! Minimize
        write(999, *) '0', of_best, pini
        write(998, *) '0', of_best, pini
      end if
      close(999)
      close(998)
    end if file_write

    ! Code below is now the DDS algorithm as presented in Figure 1 of Tolson and Shoemaker (2007)

    do i = 1, imaxiter - 1
      ! Determine Decision Variable (DV) selected for perturbation:
      Pn = 1.0_dp - log(real(i, dp)) / log(real(imaxiter - 1, dp)) ! probability each DV selected
      dvn_count = 0                                                 ! counter for how many DVs selected for perturbation
      pnew = DDS                                               ! define pnew initially as best current solution

      ! Step 3 of Fig 1 of Tolson and Shoemaker (2007)
      do j = 1, size(truepara) !pnum
        call xor4096(0_i8, ranval)                           ! selects next uniform random number in sequence
        ! Step 4 of Fig 1 of Tolson and Shoemaker (2007)
        if (ranval < Pn) then                               ! jth DV selected for perturbation
          dvn_count = dvn_count + 1
          ! call 1-D perturbation function to get new DV value (new_value)
          call neigh_value(DDS(truepara(j)), prange(truepara(j), 1), prange(truepara(j), 2), ir, new_value)
          pnew(truepara(j)) = new_value
        end if
      end do

      ! Step 3 of Fig 1 of Tolson and Shoemaker (2007) in case {N} empty
      if (dvn_count == 0) then                               ! no DVs selected at random, so select one
        call xor4096(0_i8, ranval)                           ! selects next uniform random number in sequence
        dv = truepara(int((ranval * real(size(truepara), dp)) + 1.0_dp, i4))  ! index for one DV
        ! call 1-D perturbation function to get new DV value (new_value):
        call neigh_value(DDS(dv), prange(dv, 1), prange(dv, 2), ir, new_value)
        pnew(dv) = new_value                                ! change relevant DV value in stest
      end if

      ! Step 5 of Fig 1 of Tolson and Shoemaker (2007)
      ! Evaluate obj function value for test
#ifdef MPI
      call MPI_Comm_size(comm, nproc, ierror)
      do iproc = 1, nproc-1
        do_obj_loop = .true.
        call MPI_Send(do_obj_loop,1, MPI_LOGICAL,iproc,0,comm,ierror)
      end do
#endif
      of_new = imaxit * objective%evaluate(pnew)                       ! imaxit handles min(=1) and max(=-1) problems
      ! update current best solution
      if (of_new <= of_best) then
        of_best = of_new
        DDS = pnew
      end if
      if (present(history)) history(i + 1) = of_best

      file_write2 : if (present(tmp_file)) then
        open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (pnum + 2) * 30)
        open(unit = 998, file = trim(adjustl( tmp_file )) // ".current" , action = 'write', position = 'append', &
          recl = (pnum + 2) * 30)
        if (imaxit .lt. 0.0_dp) then
          ! Maximize
          write(999, *) i, -of_best, dds
          write(998, *) i, -of_new, pnew
        else
          ! Minimize
          write(999, *) i, of_best, dds
          write(998, *) i, of_new, pnew
        end if
        close(999)
        close(998)
      end if file_write2

    end do
    if (present(funcbest)) funcbest = of_best
#ifdef MPI
    call MPI_Comm_size(comm, nproc, ierror)
    do iproc = 1, nproc-1
      do_obj_loop = .false.
      call MPI_Send(do_obj_loop,1, MPI_LOGICAL,iproc,0,comm,ierror)
    end do
#endif

  end function DDS

  ! ------------------------------------------------------------------

  !>        \brief MDDS


  !>        \details Searches Minimum or Maximum of a user-specified function using the
  !!        Modified Dynamically Dimensioned Search (DDS).\n
  !!        DDS is an n-dimensional continuous global optimization algorithm.
  !!        It is coded as a minimizer but one can give maxit=True in a maximization problem,
  !!        so that the algorithm minimizes the negative of the objective function F=(-1*F).\n
  !!        The function to be minimized is the first argument of DDS and must be defined as
  !!        \code{.f90}
  !!        use mo_optimizee, only: optimizee
  !!        type, extends(optimizee) :: your_optimizee
  !!        contains
  !!          ! to be implemented with signature: your_evaluate(self, parameters, sigma, stddev_new, likeli_new)
  !!          procedure :: evaluate => your_evaluate
  !!        end type function_optimizee
  !!        \endcode
  !!
  !!       MDDS extents normal DDS by a continuous reduction of the DDS pertubation parameter r from 0.3 to 0.05,
  !!       and by allowing a larger function value with a certain probablity.

  !>        \param[inout] "class(optimizee) :: objective"               Objective to search the optimum
  !>        \param[in] "real(dp) :: pini(:)"                 inital value of decision variables
  !>        \param[in] "real(dp) :: prange(size(pini),2)"    Min/max range of decision variables
  !>        \param[in] "integer(i8), optional        :: seed"              User seed to initialise the random number generator
  !!                                                                       (default: None)
  !>        \param[in] "integer(i8), optional        :: maxiter"           Maximum number of iteration or function evaluation
  !!                                                                       (default: 1000)
  !>        \param[in] "logical, optional            :: maxit"             Maximization (.True.) or
  !!                                                                       minimization (.False.) of function
  !!                                                                       (default: .False.)
  !>        \param[in] "logical, optional            :: mask(size(pini))"  parameter to be optimized (true or false)
  !!                                                                       (default: .True.)
  !>        \param[in]  "character(len=*) , optional :: tmp_file"          file with temporal output
  !>        \param[out] "real(dp), optional              :: funcbest"    the best value of the function.
  !>        \param[out] "real(dp), optional, allocatable :: history(:)"  the history of best function values,
  !!                                                                      history(maxiter)=funcbest\n
  !!                                          allocatable only to be in correspondance with other optimization routines
  !>        \return real(dp) :: MDDS  &mdash;  The parameters of the point which is estimated to minimize the function.

  !>     ## Restrictions
  !!         None.

  !>     ## Example
  !!
  !!        \code{.f90}
  !!        use mo_opt_functions, only: griewank
  !!        use mo_optimizee, only: function_optimizee
  !!        dv_range(:,1) = (/ -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0, -600.0 /)
  !!        dv_range(:,2) = (/ 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0 /)
  !!        dv_ini        = (/ -.226265E+01, -.130187E+01, -.151219E+01, 0.133983E+00, 0.988159E+00, &
  !!                           -.495074E+01, -.126574E+02, 0.572684E+00, 0.303864E+01, 0.343031E+01 /)
  !!        objective%func_pointer => griewank
  !!        dv_opt = MDDS(objective, dv_ini, dv_range)
  !!        \endcode
  !!
  !!     See also example in test directory.

  !>     ## Literature
  !!
  !!     1. Tolson, B. A., and C. A. Shoemaker (2007),
  !!         _Dynamically dimensioned search algorithm for computationally efficient watershed
  !!         model calibration_, Water Resour. Res., 43, W01413, doi:10.1029/2005WR004723.
  !!     2. Huang X-L and Xiong J (2010),
  !!         _Parameter Optimization of Multi-tank Model with Modified Dynamically Dimensioned
  !!         Search Algorithm_, Proceedings of the Third International Symposium on Computer
  !!         Science and Computational Technology(ISCSCT ''10), Jiaozuo, P. R. China,
  !!         14-15 August 2010, pp. 283-288\n

  !>        \author Written Matthias Cuntz and Juliane Mai
  !>        \date Aug 2012
  !         Modified, Juliane Mai,                  Nov 2012 - masked parameter
  !                   Juliane Mai,                  Dec 2012 - history output

  function MDDS(objective, pini, prange, seed, maxiter, maxit, mask, tmp_file, funcbest, history)

    use mo_kind, only : i4, i8, dp
    use mo_xor4096, only : xor4096, xor4096g

    implicit none

    class(optimizee), intent(inout) :: objective
    real(dp), dimension(:), intent(in) :: pini     ! inital value of decision variables
    real(dp), dimension(:, :), intent(in) :: prange   ! Min/max values of decision variables
    integer(i8), optional, intent(in) :: seed     ! User seed to initialise the random number generator
    integer(i8), optional, intent(in) :: maxiter  ! Maximum number of iteration or function evaluation
    logical, optional, intent(in) :: maxit    ! Maximization or minimization of function
    logical, dimension(:), optional, intent(in) :: mask     ! parameter to be optimized (true or false)
    character(len = *), optional, intent(in) :: tmp_file ! file for temporal output
    real(dp), optional, intent(out) :: funcbest ! Best value of the function.
    real(dp), dimension(:), optional, intent(out), &
            allocatable :: history  ! History of objective function values
    real(dp), dimension(size(pini)) :: MDDS     ! Best value of decision variables

    ! Local variables
    integer(i4) :: pnum                   ! Total number of decision variables
    integer(i8) :: iseed                  ! User given seed
    integer(i8) :: imaxiter               ! Maximum number of iteration or function evaluation
    real(dp) :: ir                     ! MDDS perturbation parameter
    real(dp) :: imaxit                 ! Maximization or minimization of function
    real(dp) :: of_new, of_best        ! intermediate results
    real(dp) :: Pn, new_value          ! intermediate results
    real(dp), dimension(size(pini)) :: pnew                   ! Test value of decision variables
    real(dp) :: ranval                 ! random value
    integer(i8) :: i                      ! maxiter=i8
    integer(i4) :: j, dvn_count, dv       ! pnum=i4
    integer(i4) :: idummy                 ! dummy vaiable
    integer, dimension(8) :: sdate                  ! date_and_time return
    logical, dimension(size(pini)) :: maske                  ! parameter to be optimized (true or false)
    integer(i4), dimension(:), allocatable :: truepara               ! parameter to be optimized (their indexes)


    ! Check input
    pnum = size(pini)
    if (size(prange, 1) /= pnum) stop 'Error MDDS: size(prange,1) /= size(pini)'
    if (size(prange, 2) /= 2)    stop 'Error MDDS: size(prange,2) /= 2'
    ! max. iteration
    imaxiter = 1000
    if (present(maxiter)) imaxiter = maxiter
    if (imaxiter < 6) stop 'Error MDDS: max function evals must be minimum 6'
    ! history output
    if (present(history)) then
      allocate(history(imaxiter))
    end if
    ! Min or max objective function
    imaxit = 1.0_dp
    if (present(maxit)) then
      if (maxit) imaxit = -1.0_dp
    end if
    ! Given seed
    iseed = 0
    if (present(seed)) iseed = seed
    iseed = max(iseed, 0_i8)

    ! Seed random numbers
    if (iseed == 0) then
      call date_and_time(values = sdate)
      iseed = sdate(1) * 31536000000_i8 + sdate(2) * 2592000000_i8 + sdate(3) * 86400000_i8 + &
              sdate(5) * 3600000_i8 + sdate(6) * 60000_i8 + sdate(7) * 1000_i8 + sdate(8)
      call xor4096(iseed, ranval)
      call xor4096g(iseed, ranval)
    else
      call xor4096(iseed, ranval)
      call xor4096g(iseed, ranval)
    end if

    ! Masked parameters
    if (present(mask)) then
      if (count(mask) .eq. 0_i4) then
        stop 'Input argument mask: At least one element has to be true'
      else
        maske = mask
      end if
    else
      maske = .true.
    end if

    allocate (truepara(count(maske)))
    idummy = 0_i4
    do j = 1, size(pini, 1)
      if (maske(j)) then
        idummy = idummy + 1_i4
        truepara(idummy) = j
      end if
    end do

    ! Temporal file writing
    if(present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', status = 'unknown')
      write(999, *) '# settings :: general'
      write(999, *) '# nIterations    iseed'
      write(999, *) imaxiter, iseed
      write(999, *) '# settings :: mdds specific'
      write(999, *) '# None'
      write(999, *) ''
      write(999, *) '# iter   bestf   (bestx(j),j=1,nopt)'
      close(999)
    end if

    ! Evaluate initial solution and return objective function value
    ! and Initialise the other variables (e.g. of_best)
    ! imaxit is 1.0 for MIN problems, -1 for MAX problems
    MDDS = pini
    of_new = imaxit * objective%evaluate(pini)
    of_best = of_new
    if (present(history)) history(1) = of_new

    file_write : if (present(tmp_file)) then
      open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (pnum + 2) * 30)
      if (imaxit .lt. 0.0_dp) then
        ! Maximize
        write(999, *) '0', -of_best, mdds
      else
        ! Minimize
        write(999, *) '0', of_best, mdds
      end if
      close(999)
    end if file_write

    ! Code below is now the MDDS algorithm as presented in Figure 1 of Tolson and Shoemaker (2007)

    do i = 1, imaxiter - 1
      ! Determine Decision Variable (DV) selected for perturbation:
      Pn = 1.0_dp - log(real(i, dp)) / log(real(imaxiter - 1, dp)) ! probability each DV selected
      dvn_count = 0                                                 ! counter for how many DVs selected for perturbation
      pnew = MDDS                                               ! define pnew initially as best current solution
      ! Modifications by Huang et al. (2010)
      Pn = max(Pn, 0.05_dp)
      ir = max(min(0.3_dp, Pn), 0.05_dp)

      ! Step 3 of Fig 1 of Tolson and Shoemaker (2007)
      do j = 1, pnum
        call xor4096(0_i8, ranval)                           ! selects next uniform random number in sequence
        ! Step 4 of Fig 1 of Tolson and Shoemaker (2007)
        if (ranval < Pn) then                               ! jth DV selected for perturbation
          dvn_count = dvn_count + 1
          ! call 1-D perturbation function to get new DV value (new_value)
          call neigh_value(MDDS(truepara(j)), prange(truepara(j), 1), prange(truepara(j), 2), ir, new_value)
          pnew(truepara(j)) = new_value
        end if
      end do

      ! Step 3 of Fig 1 of Tolson and Shoemaker (2007) in case {N} empty
      if (dvn_count == 0) then                               ! no DVs selected at random, so select one
        call xor4096(0_i8, ranval)                           ! selects next uniform random number in sequence
        dv = truepara(int((ranval * real(size(truepara), dp)) + 1.0_dp, i4))  ! index for one DV
        ! call 1-D perturbation function to get new DV value (new_value):
        call neigh_value(MDDS(dv), prange(dv, 1), prange(dv, 2), ir, new_value)
        pnew(dv) = new_value                                ! change relevant DV value in stest
      end if

      ! Step 5 of Fig 1 of Tolson and Shoemaker (2007)
      ! Evaluate obj function value for test
      of_new = imaxit * objective%evaluate(pnew)                       ! imaxit handles min(=1) and max(=-1) problems
      ! update current best solution
      if (of_new <= of_best) then
        of_best = of_new
        MDDS = pnew
      else ! Modifications by Huang et al. (2010)
        call xor4096(0_i8, ranval)
        if (exp(-(of_new - of_best) / of_best) > (1.0_dp - ranval * Pn)) then
          of_best = of_new
          MDDS = pnew
        end if
      end if
      if (present(history)) then
        if (present(maxit)) then
          if (maxit) then
            history(i + 1) = max(history(i), of_best)
          else
            history(i + 1) = min(history(i), of_best)
          end if
        else
          history(i + 1) = min(history(i), of_best)
        end if
      end if

      file_write2 : if (present(tmp_file)) then
        open(unit = 999, file = trim(adjustl(tmp_file)), action = 'write', position = 'append', recl = (pnum + 2) * 30)
        if (imaxit .lt. 0.0_dp) then
          ! Maximize
          write(999, *) i, -of_best, mdds
        else
          ! Minimize
          write(999, *) i, of_best, mdds
        end if
        close(999)
      end if file_write2

    end do
    if (present(funcbest)) funcbest = of_best

  end function MDDS

  ! ------------------------------------------------------------------

  ! Purpose is to generate a neighboring decision variable value for a single
  !  decision variable value being perturbed by the DDS optimization algorithm.
  !  New DV value respects the upper and lower DV bounds.
  !  Coded by Bryan Tolson, Nov 2005.

  ! I/O variable definitions:
  !  x_cur     - current decision variable (DV) value
  !  x_min     - min DV value
  !  x_max     - max DV value
  !  r         - the neighborhood perturbation factor
  !  new_value - new DV variable value (within specified min and max)

  subroutine neigh_value(x_cur, x_min, x_max, r, new_value)

    use mo_kind, only : i8, dp
    use mo_xor4096, only : xor4096g

    implicit none

    real(dp), intent(in) :: x_cur, x_min, x_max, r
    real(dp), intent(out) :: new_value
    real(dp) :: x_range
    real(dp) :: zvalue

    x_range = x_max - x_min

    ! generate a standard normal random variate (zvalue)
    call xor4096g(0_i8, zvalue)

    ! calculate new decision variable value:
    new_value = x_cur + zvalue * r * x_range

    !  check new value is within bounds. If not, bounds are reflecting.
    if (new_value < x_min) then
      new_value = x_min + (x_min - new_value)
      if (new_value > x_max) then
        ! if reflection goes past x_max then value should be x_min since
        ! without reflection the approach goes way past lower bound.
        ! This keeps x close to lower bound when x_cur is close to lower bound
        ! Practically speaking, this should never happen with r values <0.3.
        new_value = x_min
      end if
    else if (new_value > x_max) then
      new_value = x_max - (new_value - x_max)
      if (new_value < x_min) then
        ! if reflection goes past x_min then value should be x_max for same reasons as above.
        new_value = x_max
      end if
    end if

  end subroutine neigh_value

  ! ------------------------------------------------------------------

end module mo_dds
