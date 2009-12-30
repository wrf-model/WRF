module da_wrfvar_io

   use module_configure, only : grid_config_rec_type
   use module_date_time, only : geth_julgmt,current_date, start_date
   use module_domain, only : domain
   use module_io_domain, only : open_r_dataset,close_dataset, &
      input_input, open_w_dataset,output_input

   use da_control, only : trace_use,ierr
   use da_reporting, only : da_error, message, da_message
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace

contains

#include "da_med_initialdata_input.inc"
#include "da_med_initialdata_output.inc"

end module da_wrfvar_io
