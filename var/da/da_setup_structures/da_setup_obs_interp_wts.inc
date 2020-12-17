subroutine da_setup_obs_interp_wts (iv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)  :: iv         ! Innovation vector (O-B).

   integer                        :: i         ! Loop counter.

   if (trace_use) call da_trace_entry("da_setup_obs_interp_wts")

   do i=1,num_ob_indexes
      if (i /= radiance .and. iv%info(i)%nlocal > 0) then ! i=22 is radiance , should be excluded here
         if ( ob_format == ob_format_ascii ) then
            call da_store_obs_grid_info (iv%info(i))
         else if ( ob_format == ob_format_bufr ) then
            call da_store_obs_grid_info (iv%info(i))
         end if
      end if
   end do

   if ( use_gpsephobs .and. gpseph_loadbalance ) then
      do i=1,iv%info(gpseph)%nlocal
         iv%info(gpseph)%proc_domain(:,i) = .true.
      end do
   endif

   do i = 1, iv % num_inst
      if (iv % instid(i) % num_rad < 1) cycle
      call da_store_obs_grid_info_rad (iv%instid(i)%info)
   end do

   if (trace_use) call da_trace_exit("da_setup_obs_interp_wts")

end subroutine da_setup_obs_interp_wts


