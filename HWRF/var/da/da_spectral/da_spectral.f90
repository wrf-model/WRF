module da_spectral

#ifdef DM_PARALLEL
!  use mpi, only : mpi_sum
#endif

   use da_control, only : trace_use, its,ite,jts,jte, &
      da_zero_complex, jds,jde,comm,ierr,test_transforms, stdout, pi, &
      gaussian_lats, earth_radius, alpha_corr_type, alpha_corr_scale, &
      alpha_corr_unit1, alpha_corr_unit2, filename_len, num_alpha_corr_types, &
      earth_radius, alpha_corr_type_exp, alpha_corr_type_soar, &
      alpha_corr_type_gaussian, trace_use
#ifdef FFTPACK
   use da_control, only : ide
#endif
   use da_define_structures, only : xbx_type
#ifdef DM_PARALLEL
   use da_par_util1, only : true_mpi_complex
#endif
   use da_reporting, only : da_error, message
   use da_tools_serial, only : da_get_unit, da_free_unit
   use da_tracing, only : da_trace_entry, da_trace_exit

   !-----------------------------------------------------------------------
   ! Contains all necessary routines to perform global spectral transform
   ! (based on Fourier and Legendre decompositions). 
   !-----------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

contains

#include "da_asslegpol.inc"
#include "da_calc_power_spectrum.inc"
#include "da_get_gausslats.inc"
#include "da_get_reglats.inc"
#include "da_initialize_h.inc"
#include "da_legtra_inv.inc"
#include "da_legtra.inc"
#include "da_setlegpol_test.inc"
#include "da_setlegpol.inc"
#include "da_test_spectral.inc"
#include "da_vtovv_spectral.inc"
#include "da_vv_to_v_spectral.inc"
#include "da_vtovv_spectral_adj.inc"
#include "da_legtra_inv_adj.inc"
#include "da_apply_power.inc"

end module da_spectral
