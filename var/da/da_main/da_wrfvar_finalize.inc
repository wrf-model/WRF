subroutine da_wrfvar_finalize

   !-------------------------------------------------------------------------
   ! Purpose: Tidy up at the end
   !-------------------------------------------------------------------------

   implicit none

   integer               :: i
   type(domain), pointer :: grid

   if (trace_use) call da_trace_entry ("da_wrfvar_finalize")

   ! output wrfvar analysis

   if ((config_flags%real_data_init_type == 1) .or. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_output (head_grid, config_flags)
      call med_shutdown_io (head_grid, config_flags)
   end if

   grid => head_grid

   deallocate (grid%parents)
   deallocate (grid%nests)
   deallocate (grid%domain_clock)
   deallocate (grid%alarms)
   deallocate (grid%alarms_created)

   deallocate (grid%i_start)
   deallocate (grid%i_end)
   deallocate (grid%j_start)
   deallocate (grid%j_end)

#include "deallocs.inc"

   deallocate (grid)

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

   do i=unit_start,unit_end
      if (unit_used(i)) then
         write(unit=stderr,FMT=*) "unit",i,"still used"
      end if
   end do

   if (trace_use) call da_trace_exit ("da_wrfvar_finalize")

end subroutine da_wrfvar_finalize


