subroutine da_check_max_iv_gpseph(iv,it)

   !-----------------------------------------------------------------------
   ! Purpose: max_iv check for gpseph obs
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it       ! External iteration.

   integer                      :: k,n
   logical                      :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpseph")

   !----------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !----------------------------------------------------------------------------

   do n = iv%info(gpseph)%n1,iv%info(gpseph)%n2
      if ( iv%info(gpseph)%levels(n) < 1 ) cycle
      do k = 1, iv%info(gpseph)%levels(n)
         failed=.false.
         if ( iv%gpseph(n)%eph(k)%qc >= obs_qc_pointer )  then
            call da_max_error_qc(it, iv%info(gpseph), n, iv%gpseph(n)%eph(k), max_error_eph, failed)
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpseph")

end subroutine da_check_max_iv_gpseph
