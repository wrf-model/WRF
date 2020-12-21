subroutine da_init_model_output

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   if (trace_use_dull) call da_trace_entry("da_init_model_output")

   config_flags%input_outname = "fg"
   config_flags%write_input = .true.

   call med_filter_out (head_grid, config_flags)

   config_flags%input_outname = "wrfinput_d<domain>"
   config_flags%write_input = .false.

   if (trace_use_dull) call da_trace_exit("da_init_model_output")

end subroutine da_init_model_output
