subroutine da_check_xtoy_adjoint_gpseph(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: calculate gpseph <y, y>
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs

   integer :: n, k          ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_gpseph")

   do n=1, iv%info(gpseph)%nlocal
      if (iv%info(gpseph)%proc_domain(1,n)) then
      if ( iv%info(gpseph)%levels(n) < 1 ) cycle
      !do k=1, iv%info(gpseph)%levels(n)
      do k=iv%gpseph(n)%level1, iv%gpseph(n)%level2
      if ( y%gpseph(n)%eph(k) /= missing_r ) then
         adjtest_lhs = adjtest_lhs + (y%gpseph(n)%eph(k) / typical_eph_rms)**2
      endif
      end do
      end if

      !do k=1, iv%info(gpseph)%levels(n)
      do k=iv%gpseph(n)%level1, iv%gpseph(n)%level2
      ! Because nonlocal things are calculated globally, so we have to exclude the obs. not on tile.
      if (.not. iv%info(gpseph)%proc_domain(1,n)) y%gpseph(n)%eph(k) = 0.0
      if ( y%gpseph(n)%eph(k) /= missing_r ) then
         pertile_lhs = pertile_lhs + (y%gpseph(n)%eph(k) / typical_eph_rms)**2
         y%gpseph(n)%eph(k) = y%gpseph(n)%eph(k) / typical_eph_rms ** 2
      endif
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_gpseph")

end subroutine da_check_xtoy_adjoint_gpseph
