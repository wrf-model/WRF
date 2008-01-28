subroutine da_check_max_iv_ssmi_rv(iv, it, itpw, itpwf, ispeed, ispeedf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it ! Outer loop 
   integer,       intent(inout) :: itpw, itpwf, ispeed, ispeedf

   logical :: failed
   integer :: n

   if (trace_use) call da_trace_entry("da_check_max_iv_ssmi_rv")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMI_rv_TPW   = ',max_error_pw, &
         'Error max test ratio for SSMI_rv_SPEED = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(ssmi_rv)%n1,iv%info(ssmi_rv)%n2
      call da_max_error_qc (it, iv%info(ssmi_rv), n, iv%ssmi_rv(n)%tpw, &
         max_error_pw, itpw, itpwf, check_max_iv_unit, 'TPW  ', failed, check_max_iv_print)
      call da_max_error_qc (it, iv%info(ssmi_rv), n, iv%ssmi_rv(n)%speed, &
         max_error_uv, ispeed, ispeedf, check_max_iv_unit, 'SPEED', failed, check_max_iv_print)
   end do

   if (trace_use) call da_trace_exit("da_check_max_iv_ssmi_rv")

end subroutine da_check_max_iv_ssmi_rv


