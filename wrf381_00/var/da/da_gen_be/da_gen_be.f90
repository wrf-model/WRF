module da_gen_be

   !---------------------------------------------------------------------------- 
   ! Purpose: Collection of routines required by gen_be BE stats calculation 
   ! code.
   ! 
   !  Update: Multivariate BE option (cv_options=6)
   !          Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
   !          
   !  Note: Please acknowledge author/institute in work that uses this code.
   !------------------------------------------------------------------------


   !----------------------------------------------------------------------------

   use da_control, only : stdout,vertical_ip, t0,es_beta,es_alpha, &
      es_gamma,kappa,rd_over_rv,rd_over_rv1,t_kelvin, gravity, &
      filename_len,vertical_ip_0, trace_use, trace_use_dull, cv_options, use_rf, do_normalize
   use da_reporting, only : da_error, da_warning, da_message, message
   use da_tools_serial, only : da_get_unit, da_free_unit, da_array_print
   use da_lapack, only : dsyev
   use da_wavelet, only: lf,namw,nb,nij,ws,wsd

   implicit none

   real, parameter :: base_pres = 100000.0 ! Hardwired - link to namelist later.
               
contains

! Stubs to avoid picking up vast ammounts of unnecessary stuff from wrfvar

subroutine da_trace_entry(name, message, messages, maxnocalls)    
   implicit none
   character (len=*),           intent(in) :: name         
   character (len=*), optional, intent(in) :: message      
   character (len=*), optional, intent(in) :: messages(:)  
   integer, optional,           intent(in) :: maxnocalls   
end subroutine da_trace_entry

subroutine da_trace_exit(name, message, messages, maxnocalls)
   implicit none
   character (len=*), intent(in)           :: name         
   character (len=*), optional, intent(in) :: message      
   character (len=*), optional, intent(in) :: messages(:)  
   integer, optional, intent(in)           :: maxnocalls  
end subroutine da_trace_exit

#include "da_create_bins.inc"
#include "da_filter_regcoeffs.inc"
#include "da_get_field.inc"
#include "da_get_height.inc"
#include "da_get_trh.inc"
#include "da_print_be_stats_h_global.inc"
#include "da_print_be_stats_h_regional.inc"
#include "da_print_be_stats_p.inc"
#include "da_print_be_stats_v.inc"
#include "da_readwrite_be_stage1.inc"
#include "da_readwrite_be_stage2.inc"
#include "da_readwrite_be_stage3.inc"
#include "da_readwrite_be_stage4.inc"
#include "da_stage0_initialize.inc"

   ! Files from other modules:
#include "da_transform_vptovv.inc"
#include "da_eof_decomposition.inc"
#include "da_eof_decomposition_test.inc"
#include "da_perform_2drf.inc"
#include "da_recursive_filter_1d.inc"

end module da_gen_be

subroutine wrf_abort
   stop
end subroutine wrf_abort

   LOGICAL FUNCTION wrf_dm_on_monitor()
      IMPLICIT NONE
      wrf_dm_on_monitor = .TRUE.
      RETURN
   END FUNCTION wrf_dm_on_monitor

