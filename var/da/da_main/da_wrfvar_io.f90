module da_wrfvar_io

   use module_configure, only : grid_config_rec_type, model_config_rec, &
      model_to_grid_config_rec
   use module_date_time, only : get_julgmt, geth_julgmt, current_date, start_date
   use module_domain, only : domain, get_ijk_from_grid
   use module_io_domain, only : open_r_dataset,close_dataset, &
      input_input, open_w_dataset,output_input, &
      input_boundary, output_boundary, output_auxhist4, &
      input_auxhist6, input_auxhist4
   use module_io, only: wrf_get_dom_ti_integer
   use module_state_description, only : p_qv

   use da_control, only : trace_use, ierr, var4d, var4d_lbc, num_fgat_time, rootproc
   use da_control, only : cloud_cv_options
   use da_reporting, only : da_error, message, da_message
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace

   use, intrinsic :: iso_c_binding,                       &
                     ONLY: c_int32_t, C_CHAR, C_NULL_CHAR


#ifdef VAR4D
   use da_4dvar, only : model_grid
#endif

contains

#include "da_med_initialdata_input.inc"
#include "da_med_initialdata_output.inc"
#include "da_med_initialdata_output_lbc.inc"
#include "da_update_firstguess.inc"

end module da_wrfvar_io
