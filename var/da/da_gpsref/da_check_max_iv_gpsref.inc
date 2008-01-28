subroutine da_check_max_iv_gpsref(iv,it, pref, fref)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it       ! External iteration.
   integer,       intent(inout) :: pref, fref

   integer                           :: k,n
   logical                           :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpsref")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'

      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for GPSRef = ', max_error_ref
   end if

   !----------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   failed = .false.

   do n = iv%info(gpsref)%n1,iv%info(gpsref)%n2
      do k = 1, iv%info(gpsref)%levels(n)
         call da_max_error_qc(it, iv%info(gpsref), n, iv%gpsref(n)%ref(k), &
            max_error_ref, pref, fref, check_max_iv_unit, &
            'GPSRef    ', failed, check_max_iv_print)
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpsref")

end subroutine da_check_max_iv_gpsref


