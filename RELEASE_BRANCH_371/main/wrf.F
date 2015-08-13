!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_wrf_top, only : wrf_init, wrf_dfi, wrf_run, wrf_finalize

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

  ! WRF model time-stepping.  Calls integrate().  
  CALL wrf_run

  ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
  CALL wrf_finalize

END PROGRAM wrf


