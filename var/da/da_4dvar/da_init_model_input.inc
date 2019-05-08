subroutine da_init_model_input

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   if (trace_use_dull) call da_trace_entry("da_init_model_input")

   head_grid%itimestep = 0

   config_flags%input_inname = "fg"

   call med_initialdata_input (head_grid, config_flags)

   config_flags%input_inname = "wrfinput_d<domain>"

   if (trace_use_dull) call da_trace_exit("da_init_model_input")

end subroutine da_init_model_input
