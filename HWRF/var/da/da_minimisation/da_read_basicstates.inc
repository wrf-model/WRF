subroutine da_read_basicstates ( xbx, grid, config_flags, timestr )

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
   type(grid_config_rec_type), intent(inout)    ::   config_flags

#ifdef VAR4D
   if ( grid%trajectory_io ) then
      call input_nl_xtraj (timestr)
      call da_swap_xtraj (grid)
   else
      config_flags%auxinput6_inname = "auxhist6_d<domain>_<date>"
      call med_auxinput_in ( grid, auxinput6_only, config_flags )
   endif
   call da_transfer_wrftoxb(xbx, grid, config_flags)
#endif
   return

end subroutine da_read_basicstates

