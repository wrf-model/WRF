subroutine da_check_max_iv_sound(iv, it, itu,ituf,itv,itvf,itt,ittf,itqv,itqvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: itu,ituf,itv,itvf,itt,ittf,itqv,itqvf

   integer :: k,n
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_sound")

   !--------------------------------------------------------------------------- 
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Sound_U   = ',max_error_uv, &
         'Error max test ratio for Sound_V   = ',max_error_uv, &
         'Error max test ratio for Sound_T   = ',max_error_t,  &
         'Error max test ratio for Sound_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n = iv%info(sound)%n1,iv%info(sound)%n2
      do k = 1, iv%info(sound)%levels(n)
         call da_max_error_qc (it, iv%info(sound), n, iv%sound(n)%u(k), max_error_uv, itu, ituf, &
            check_max_iv_unit, 'U    ', failed, check_max_iv_print)
         call da_max_error_qc (it, iv%info(sound), n, iv%sound(n)%v(k), max_error_uv, itv, itvf, &
            check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
         call da_max_error_qc (it, iv%info(sound), n, iv%sound(n)%t(k), max_error_t,  itt, ittf, &
            check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)                           
         call da_max_error_qc (it, iv%info(sound), n, iv%sound(n)%q(k), max_error_q,  itqv, itqvf, &
            check_max_iv_unit, 'QV   ', failed, check_max_iv_print)                           
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_sound")

end subroutine da_check_max_iv_sound


