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

   if (trace_use) call da_trace_entry("da_transfer_xatoanalysis")

   !---------------------------------------------------------------------------
   ! Write out analysis in differing formats:
   !---------------------------------------------------------------------------      

   if (fg_format == fg_format_wrf) then

      call da_transfer_xatowrf(grid)

      if (it < max_ext_its) then

         call da_transfer_wrftoxb(xbx, grid)
      end if
   else if (fg_format == fg_format_kma_global) then
      call da_transfer_xatokma(grid)
      if (it < max_ext_its) then
         call da_transfer_kmatoxb(xbx, grid)
      end if
   end if

   if (trace_use) call da_trace_exit("da_transfer_xatoanalysis")

end subroutine da_transfer_xatoanalysis


