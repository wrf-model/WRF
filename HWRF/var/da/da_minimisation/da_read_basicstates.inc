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
   type(grid_config_rec_type), intent(in)    ::   config_flags

#ifdef VAR4D
   call input_nl_xtraj (timestr)
   call da_swap_xtraj (grid)
   call da_setup_firstguess( xbx, grid, config_flags)
#endif
   return

end subroutine da_read_basicstates

