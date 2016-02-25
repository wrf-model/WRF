subroutine da_read_basicstates ( xbx, grid, config_flags, timestr, fgat )

   !-------------------------------------------------------------------------
   ! Purpose: Read basic state at time = timestr
   !
   ! History: 10/01/2010  Creation (Xin Zhang )
   !
   !-------------------------------------------------------------------------

   implicit none

   character(len=256),         intent(in)    ::   timestr
   type(domain),               intent(inout) ::   grid
   type(xbx_type),             intent(inout) ::   xbx
   type(grid_config_rec_type), intent(inout) ::   config_flags
   character(len=120), intent(in), optional  ::   fgat

#ifdef VAR4D
   if ( grid%var4d ) then
      if ( grid%trajectory_io ) then
         call input_nl_xtraj (timestr)
         call da_swap_xtraj (grid)
      else
         config_flags%auxinput6_inname = "auxhist6_d<domain>_<date>"
         call med_auxinput_in ( grid, auxinput6_only, config_flags )
      endif
      call da_transfer_wrftoxb(xbx, grid, config_flags)
   endif
#endif
! Following codes are for FGAT
   if ( .not. grid%var4d .and. grid%num_fgat_time > 1 .and. present(fgat) ) then
      call da_med_initialdata_input (grid, config_flags, trim(fgat))
      call da_transfer_wrftoxb(xbx, grid, config_flags)
   end if
   return

end subroutine da_read_basicstates

