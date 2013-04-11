module da_tools_serial
   
   !---------------------------------------------------------------------------
   ! Purpose: Contains serial tools, useable by gen_be which doesn't like parallel
   !---------------------------------------------------------------------------
      
   use da_control, only : unit_used, unit_end, unit_start, stdout, num_fft_factors, pi
   use da_reporting, only : da_error

   implicit none

contains

#include "da_get_unit.inc"
#include "da_free_unit.inc"
#include "da_change_date.inc"
#include "da_array_print.inc"
#include "da_advance_cymdh.inc"
#include "da_advance_time.inc"
#include "da_find_fft_factors.inc"
#include "da_find_fft_trig_funcs.inc"

end module da_tools_serial

