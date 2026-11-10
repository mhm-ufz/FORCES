! Shared fixtures for message/logging suites; each executable owns its logger configuration.
module logging_test_support
  use mo_message, only: use_log, show_msg, show_warn, show_err, unit_msg, unit_warn, unit_err, def_log_scope
  use mo_logging, only: log_level, log_unit, log_unit_error, log_set_config
#ifdef MPI
  use mpi, only: MPI_Initialized, MPI_Init, MPI_Finalize
#endif
  implicit none
  private
  public :: save_message_config, restore_message_config, initialize_logging_tests, finalize_logging_tests
  logical, save :: saved_flags(4), saved_scope_allocated
  integer, save :: saved_units(5), saved_level
  character(:), allocatable, save :: saved_scope
#ifdef MPI
  logical, save :: owns_mpi
#endif
contains
  subroutine initialize_logging_tests()
#ifdef MPI
    logical :: initialized
    integer :: ierr
    call MPI_Initialized(initialized, ierr)
    owns_mpi = .not. initialized
    if (owns_mpi) call MPI_Init(ierr)
#endif
  end subroutine initialize_logging_tests

  subroutine finalize_logging_tests()
#ifdef MPI
    integer :: ierr
    if (owns_mpi) call MPI_Finalize(ierr)
#endif
  end subroutine finalize_logging_tests

  subroutine save_message_config()
    saved_flags = [use_log, show_msg, show_warn, show_err]
    saved_units = [unit_msg, unit_warn, unit_err, log_unit, log_unit_error]
    saved_level = log_level
    saved_scope_allocated = allocated(def_log_scope)
    if (allocated(saved_scope)) deallocate(saved_scope)
    if (saved_scope_allocated) saved_scope = def_log_scope
    use_log = .false.
    show_msg = .true.
    show_warn = .true.
    show_err = .true.
    if (allocated(def_log_scope)) deallocate(def_log_scope)
    call log_set_config(log_output_date=.false., log_output_time=.false., log_output_line=.false., log_scope_filter='')
  end subroutine save_message_config

  subroutine restore_message_config()
    use_log = saved_flags(1)
    show_msg = saved_flags(2)
    show_warn = saved_flags(3)
    show_err = saved_flags(4)
    unit_msg = saved_units(1)
    unit_warn = saved_units(2)
    unit_err = saved_units(3)
    log_unit = saved_units(4)
    log_unit_error = saved_units(5)
    log_level = saved_level
    if (allocated(def_log_scope)) deallocate(def_log_scope)
    if (saved_scope_allocated) def_log_scope = saved_scope
    if (allocated(saved_scope)) deallocate(saved_scope)
    call log_set_config(log_output_date=.false., log_output_time=.false., log_output_line=.false., log_scope_filter='')
  end subroutine restore_message_config
end module logging_test_support
