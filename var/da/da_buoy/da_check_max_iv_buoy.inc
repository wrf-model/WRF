subroutine da_check_max_iv_buoy(iv, it, itu,ituf,itvv,itvvf,itp,itpf,itt,ittf,itqv,itqvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration 
   integer,       intent(inout) :: itu,ituf,itvv,itvvf,itp,itpf,itt
   integer,       intent(inout) :: ittf,itqv,itqvf

   logical :: failed 
   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_buoy")       

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Buoy_U   = ',max_error_uv, &
         'Error max test ratio for Buoy_V   = ',max_error_uv, &
         'Error max test ratio for Buoy_P   = ',max_error_p,  &
         'Error max test ratio for Buoy_T   = ',max_error_t,  &
         'Error max test ratio for Buoy_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(buoy)%n1,iv%info(buoy)%n2
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%u, max_error_uv, itu, ituf, check_max_iv_unit, 'U    ', failed, check_max_iv_print)
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%v, max_error_uv, itvv, itvvf, check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%p, max_error_p, itp, itpf, check_max_iv_unit, 'Press', failed, check_max_iv_print)         
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%t, max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)
      ! Mixing ratio, same ratio as relative humidity
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%q, max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', failed, check_max_iv_print)
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_buoy")       

end subroutine da_check_max_iv_buoy


