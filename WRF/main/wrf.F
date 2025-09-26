!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_wrf_top, only : wrf_init, wrf_dfi, wrf_run, wrf_finalize
#if ( WRFPLUS == 1 )
   USE module_wrf_top, only : config_flags, wrf_adtl_check, &
                              wrf_run_tl_standalone, wrf_run_ad_standalone
   USE module_state_description, only : dyn_em, dyn_em_tl, dyn_em_ad
#endif

!<DESCRIPTION>
! Main program of WRF model.  Responsible for starting up the model, reading in (and
! broadcasting for distributed memory) configuration data, defining and initializing
! the top-level domain, either from initial or restart data, setting up time-keeping, and
! then calling the <a href=integrate.html>integrate</a> routine to advance the domain
! to the ending time of the simulation. After the integration is completed, the model
! is properly shut down.
!
!</DESCRIPTION>

   IMPLICIT NONE

#ifdef _OPENMP
  CALL setfeenv()
#endif

  ! Set up WRF model.  
  CALL wrf_init

  ! Run digital filter initialization if requested.
  CALL wrf_dfi

#if ( WRFPLUS == 1 )
  ! Run adjoint check and tangent linear check if requested.
  CALL wrf_adtl_check
#endif

  ! WRF model time-stepping.  Calls integrate().  
#if ( WRFPLUS == 1 )
  IF  ( config_flags%dyn_opt .EQ. dyn_em ) &
#endif
  CALL wrf_run

#if ( WRFPLUS == 1 )
  ! WRF model time-stepping.  Calls integrate().  
  IF  ( config_flags%dyn_opt .EQ. dyn_em_tl .and. config_flags%tl_standalone ) &
  CALL wrf_run_tl_standalone

  ! WRF model time-stepping.  Calls integrate().  
  IF  ( config_flags%dyn_opt .EQ. dyn_em_ad ) &
  CALL wrf_run_ad_standalone
#endif

  ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
  CALL wrf_finalize

END PROGRAM wrf


