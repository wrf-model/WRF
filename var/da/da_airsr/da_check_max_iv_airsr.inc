subroutine da_check_max_iv_airsr(iv, it,itt,ittf,itqv,itqvf)

   !-------------------------------------------------------------------------
   ! Purpose: Applies max error check on AIRS retrievals
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: itt,ittf,itqv,itqvf

   integer :: k,n
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_airsr")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for AIRS_T   = ',max_error_t,  &
         'Error max test ratio for AIRS_QV  = ',max_error_q
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(airsr)%n1,iv%info(airsr)%n2
      do k = 1, iv%info(airsr)%levels(n)
         ! Temperature
         if (iv%airsr(n)%t(k)%qc >= obs_qc_pointer) then
            call da_max_error_qc (it, iv%info(airsr), n, iv%airsr(n)%t(k),&
               max_error_t, itt, ittf, check_max_iv_unit, 'TEMP ', &
                            failed, check_max_iv_print)

         end if
         ! Mixing ratio, same ratio as relative humidity
         if (iv%airsr(n)%q(k)%qc >= obs_qc_pointer) then
            call da_max_error_qc  (it, iv%info(airsr), n, iv%airsr(n)%q(k), &
               max_error_q, itqv, itqvf, check_max_iv_unit, 'QV   ', &
                            failed, check_max_iv_print)
         end if
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_airsr")

end subroutine da_check_max_iv_airsr


