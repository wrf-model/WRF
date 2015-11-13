subroutine da_transfer_xatoanalysis(it, xbx, grid, config_flags)

   !---------------------------------------------------------------------------
   ! Purpose: Transfer xb and xa (increments) to analysis.
   !---------------------------------------------------------------------------

   implicit none

   integer,         intent(in)    :: it    ! outer-loop index
   type (xbx_type), intent(out)   :: xbx    ! Header & non-gridded vars.
   type(domain),    intent(inout) :: grid

   type (grid_config_rec_type), intent(inout) :: config_flags

   character*4 filnam
   character(len=256) ::  timestr

   if (trace_use) call da_trace_entry("da_transfer_xatoanalysis")

   !---------------------------------------------------------------------------
   ! Write out analysis in differing formats:
   !---------------------------------------------------------------------------      

   if (fg_format == fg_format_wrf_arw_regional) then
      if (write_increments .and. var4d) then
         write(unit=filnam,fmt='(a3,i1)') 'inc',it
         call domain_clock_get( grid, current_timestr=timestr )
         call da_transfer_xatowrftl(grid, config_flags, filnam, timestr)
      end if

      call da_transfer_xatowrf(grid, config_flags)

      if (it < max_ext_its) then
         call da_transfer_wrftoxb(xbx, grid, config_flags)
      end if
   else if (fg_format == fg_format_wrf_arw_global) then
      if( var4d) then
      write(unit=message(1),fmt='(A,I5)') &
         "var4d is not possible with Global WRF fg_format = ",fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
      else
       call da_transfer_xatowrf(grid, config_flags)
       if (it < max_ext_its)call da_transfer_wrftoxb(xbx, grid, config_flags)
      end if
   else if (fg_format == fg_format_wrf_nmm_regional) then
      call da_transfer_xatowrf_nmm_regional(grid)
      if (it < max_ext_its) then
         if (var4d) then
      write(unit=message(1),fmt='(A,I5)') &
         "var4d is not possible for fg_format = ",fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
         end if

         call da_transfer_wrf_nmm_regional_toxb(xbx, grid)
      end if
   else if (fg_format == fg_format_kma_global) then
      call da_transfer_xatokma(grid)
      if (it < max_ext_its) then
         call da_transfer_kmatoxb(xbx, grid)
      end if
   end if

   if (trace_use) call da_trace_exit("da_transfer_xatoanalysis")

end subroutine da_transfer_xatoanalysis


