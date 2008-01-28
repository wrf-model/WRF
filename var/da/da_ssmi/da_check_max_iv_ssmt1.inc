subroutine da_check_max_iv_ssmt1(iv, it, itt,ittf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    ::  it      ! outer iteration
   integer,       intent(inout) ::  itt, ittf           

   integer ::  k,n
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_ssmt1")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMT1_T   = ',max_error_t
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(ssmt1)%n1,iv%info(ssmt1)%n2
      do k = 1, iv%info(ssmt1)%levels(n)
         ! Temperature
         call da_max_error_qc (it, iv%info(ssmt1), n, iv%ssmt1(n)%t(k),&
            max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', failed, check_max_iv_print)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_ssmt1")

end subroutine da_check_max_iv_ssmt1


