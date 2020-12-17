subroutine da_wrfvar_run()

   !-------------------------------------------------------------------------
   ! Purpose: run wrfvar
   !-------------------------------------------------------------------------

   implicit none

   if (trace_use) call da_trace_entry("da_wrfvar_run")

   call da_wrfvar_interface (input_grid, config_flags)

   if (trace_use) call da_trace_exit("da_wrfvar_run")

end subroutine da_wrfvar_run


