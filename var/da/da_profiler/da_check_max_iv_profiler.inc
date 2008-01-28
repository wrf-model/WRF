subroutine da_check_max_iv_profiler(iv, it, itu,ituf,itv,itvf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration 
   integer,       intent(inout) :: itu,ituf,itv,itvf

   integer :: k,n
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_profiler")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for Profiler_U   = ',max_error_uv, &
         'Error max test ratio for Profiler_V   = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n = iv%info(profiler)%n1,iv%info(profiler)%n2
      do k = 1, iv%info(profiler)%levels(n)
         call da_max_error_qc (it, iv%info(profiler), n, iv%profiler(n)%u(k), max_error_uv, itu, ituf, &
            check_max_iv_unit, 'U    ', failed, check_max_iv_print)
         call da_max_error_qc (it, iv%info(profiler), n, iv%profiler(n)%v(k), max_error_uv, itv, itvf, &
            check_max_iv_unit, 'V    ', failed, check_max_iv_print)                         
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_profiler")

end subroutine da_check_max_iv_profiler


