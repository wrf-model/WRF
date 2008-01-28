subroutine da_check_max_iv_ssmt2(iv, it, irh,irhf)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,           intent(in)    :: it     ! outer iteration
   integer,           intent(inout) :: irh, irhf

   integer :: k,n
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_ssmt2")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for SSMT2_RH  = ',max_error_rh
   end if

   !---------------------------------------------------------------------------
   !  [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(ssmt2)%n1,iv%info(ssmt2)%n2
      do k = 1, iv%info(ssmt2)%levels(n)
         ! Temperature
         call da_max_error_qc (it, iv%info(ssmt2), n, iv%ssmt2(n)%rh(k),&
            max_error_t, irh, irhf, check_max_iv_unit, 'RH  ', failed, check_max_iv_print)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_ssmt2")

end subroutine da_check_max_iv_ssmt2


