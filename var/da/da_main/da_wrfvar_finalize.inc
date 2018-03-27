subroutine da_wrfvar_finalize

   !-------------------------------------------------------------------------
   ! Purpose: Tidy up at the end
   !-------------------------------------------------------------------------

   implicit none

   integer               :: i
   type(domain), pointer :: grid

   if (trace_use) call da_trace_entry ("da_wrfvar_finalize")

#ifdef VAR4D
   if ( var4d_lbc ) then
      if (ALLOCATED(u6_2)) deallocate (u6_2)
      if (ALLOCATED(v6_2)) deallocate (v6_2)
      if (ALLOCATED(w6_2)) deallocate (w6_2)
      if (ALLOCATED(t6_2)) deallocate (t6_2)
      if (ALLOCATED(ph6_2)) deallocate (ph6_2)
      if (ALLOCATED(mu6_2)) deallocate (mu6_2)
      if (ALLOCATED(moist6)) deallocate (moist6)
      if (ALLOCATED(p6)) deallocate (p6)
      if (ALLOCATED(psfc6)) deallocate (psfc6)
   endif
#endif

!  grid => head_grid
   grid => input_grid

   if ( .not. anal_type_hybrid_dual_res ) then
      call dealloc_space_domain(grid%id)
   else
      !=== deallocate fine grid
      ! subroutine dealloc_space_domain checks for head_grid
      ! when anal_type_hybrid_dual_res, input_grid is another_grid
      ! dealloc_space_domain does not work in this case
      call domain_destroy(grid)
      !=== deallocate coarse grid
      grid => ensemble_grid
      call dealloc_space_domain(grid%id)
   end if

#ifdef VAR4D
   if ( var4d_lbc ) then
      if (ALLOCATED(u6_2)) deallocate (u6_2)
      if (ALLOCATED(v6_2)) deallocate (v6_2)
      if (ALLOCATED(w6_2)) deallocate (w6_2)
      if (ALLOCATED(t6_2)) deallocate (t6_2)
      if (ALLOCATED(ph6_2)) deallocate (ph6_2)
      if (ALLOCATED(mu6_2)) deallocate (mu6_2)
      if (ALLOCATED(moist6)) deallocate (moist6)
      if (ALLOCATED(p6)) deallocate (p6)
      if (ALLOCATED(psfc6)) deallocate (psfc6)
   endif
#endif

#if defined(RTTOV) || defined(CRTM)
   if (allocated(num_tovs_before)) deallocate (num_tovs_before)
   if (allocated(num_tovs_after))  deallocate (num_tovs_after)
   if (allocated(tovs_copy_count)) deallocate (tovs_copy_count)
   if (allocated(tovs_send_pe))    deallocate (tovs_send_pe)
   if (allocated(tovs_recv_pe))    deallocate (tovs_recv_pe)
   if (allocated(tovs_send_start)) deallocate (tovs_send_start)
   if (allocated(tovs_send_count)) deallocate (tovs_send_count)
   if (allocated(tovs_recv_start)) deallocate (tovs_recv_start)
#endif

   if (rootproc) then
      close (cost_unit)
      close (grad_unit)
      if (.not. print_detail_outerloop) then
         close (stats_unit)
         call da_free_unit (stats_unit)
      end if
      close (jo_unit)
      close (check_max_iv_unit)
      close (check_buddy_unit)
      call da_free_unit (cost_unit)
      call da_free_unit (grad_unit)
      call da_free_unit (jo_unit)
      call da_free_unit (check_max_iv_unit)
      call da_free_unit (check_buddy_unit )
   end if

#ifdef CRTM
   if (use_rad .and. rtm_option == rtm_option_crtm) then
      ierr = CRTM_Destroy(ChannelInfo)
      deallocate(Sensor_Descriptor)
   end if
#endif

   if ( use_gpsephobs ) then
      call da_gpseph_final ( )
   endif

   do i=unit_start,unit_end
      if (unit_used(i)) then
         write(unit=stderr,FMT=*) "unit",i,"still used"
      end if
   end do

   if (trace_use) call da_trace_exit ("da_wrfvar_finalize")

end subroutine da_wrfvar_finalize


