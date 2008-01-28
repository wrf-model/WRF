subroutine da_check_max_iv_qscat(iv, it, itu, itvv, ituf, itvvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it
   integer,       intent(inout) :: itu, itvv, ituf, itvvf

   logical :: failed
   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_qscat")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Qscat_U   = ',max_error_uv, &
         'Error max test ratio for Qscat_V   = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      call da_max_error_qc(it, iv%info(qscat), n, iv%qscat(n)%u, max_error_uv, itu, ituf, check_max_iv_unit, &
         'U    ', failed, check_max_iv_print)
      call da_max_error_qc (it, iv%info(qscat), n, iv%qscat(n)%v, max_error_uv, itvv, itvvf, check_max_iv_unit, &
         'V    ', failed, check_max_iv_print)                           
   end do

   !---------------------------------------------------------------------------
   ! [3.0] Output test failed results:
   !---------------------------------------------------------------------------
 
   if (rootproc .and. check_max_iv_print .and. failed) then
      write (unit = check_max_iv_unit, fmt = '(/,10(2(A,I6),/))') & 
         'Number of failed qscat u-wind observations:       ', &
         ituf,  ' on ',itu,   &
         'Number of failed qscat v-wind observations:       ', &
         itvvf,  ' on ',itvv
   end if
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_qscat")

end subroutine da_check_max_iv_qscat


