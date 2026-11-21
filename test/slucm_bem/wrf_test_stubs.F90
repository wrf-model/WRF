! Infrastructure-only substitutes for the standalone physics regression.
! Actual module_model_constants, module_sf_slucm_bem, module_sf_urban and
! URBPARM tables are compiled without replacing any physics routine.
module module_wrf_error
  use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
  implicit none
contains
  subroutine wrf_error_fatal(message)
    character(len=*), intent(in) :: message
    write(error_unit,'(A)') 'WRF_FATAL: '//trim(message)
    flush(error_unit)
    error stop 1
  end subroutine wrf_error_fatal
  subroutine wrf_message(message)
    character(len=*), intent(in) :: message
    write(output_unit,'(A)') trim(message)
  end subroutine wrf_message
end module module_wrf_error

! urban_var_init is linked but not exercised by this single-column harness.
! The test explicitly initializes LCZ_SWITCH_kike after urban_param_init.
subroutine nl_get_use_wudapt_lcz(domain_id, value)
  implicit none
  integer, intent(in) :: domain_id
  integer, intent(out) :: value
  value = 0
end subroutine nl_get_use_wudapt_lcz
