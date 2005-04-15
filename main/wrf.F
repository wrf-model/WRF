!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf

   USE module_wrf_esmf_super

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
! TBH:  Get rid of this #ifdef (leaving nothing behind) as soon 
! TBH:  as wrf_ESMFApp.F is working!  
#ifdef WRF_ESMF_MOD
   TYPE(ESMF_VM) :: vm
   INTEGER :: rc
#endif

   ! Initialize WRF model:  phase one includes everything that must be done 
   ! before ESMF_Initialize() is called.  This calls MPI_INIT() for DM 
   ! parallel runs.  
   CALL init_modules(1)

   ! Initialize ESMF.  This variant of ESMF_Initialize() does not call 
   ! MPI_INIT() for DM parallel runs.  
! TBH:  Get rid of this #ifdef (leaving only the #else branch behind) as soon 
! TBH:  as wrf_ESMFApp.F is working!  
#ifdef WRF_ESMF_MOD
   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )
#else
   CALL ESMF_Initialize
#endif

   ! Initialize WRF model:  phase two includes everything that must be done 
   ! after ESMF_Initialize() is called.  
   CALL wrf_init

   ! WRF model time-stepping.  Calls integrate().  
   CALL wrf_run

   ! Clean up after ESMF.  This variant of ESMF_Finalize() does not call 
   ! MPI_FINALIZE() for DM parallel runs.  
! TBH:  Get rid of this #ifndef (leaving only the enclosed call behind) as soon 
! TBH:  as wrf_ESMFApp.F is working!  
#ifndef WRF_ESMF_MOD
   CALL ESMF_Finalize
#endif

   ! WRF model clean-up.  This calls MPI_FINALIZE() for DM parallel runs.  
   CALL wrf_finalize

! TBH:  Get rid of this #ifdef (leaving nothing behind) as soon 
! TBH:  as wrf_ESMFApp.F is working!  
#ifdef WRF_ESMF_MOD
   CALL ESMF_Finalize( rc=rc )
#endif

END PROGRAM wrf


