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

   TYPE(ESMF_GridComp) :: gcomp
   TYPE(ESMF_State)    :: importState, exportState
   TYPE(ESMF_Clock)    :: clock
   TYPE(ESMF_VM)       :: vm   
   INTEGER :: rc

   ! This call includes everything that must be done before ESMF_Initialize() 
   ! is called.  
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )

   CALL wrf_init( gcomp, importState, exportState, clock, rc )

   CALL wrf_run( gcomp, importState, exportState, clock, rc )

   CALL wrf_finalize( gcomp, importState, exportState, clock, rc )

   CALL ESMF_Finalize( rc=rc )

END PROGRAM wrf


