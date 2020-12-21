subroutine da_med_init_fgc

!   call med_initialdata_input (head_grid, config_flags, 'fg')
   head_grid%itimestep = 0
   config_flags%input_inname = "fg"
   call med_initialdata_input (head_grid, config_flags)
   config_flags%input_inname = "wrfinput_d<domain>"

end subroutine da_med_init_fgc
