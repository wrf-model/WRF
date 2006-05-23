!WRF:DRIVER_LAYER:MAIN
!

!<DESCRIPTION>
! Stand-alone ESMF Application Wrapper for WRF model.  This file contains the 
! main program and creates a top level ESMF Gridded Component.  
!
! This source file is only built when ESMF coupling is used.  
!
!</DESCRIPTION>


PROGRAM wrf_ESMFApp

!<DESCRIPTION>
! Stand-alone ESMF Application Wrapper for WRF model.  This is the main 
! program that creates a top level ESMF Gridded Component.  
!
!</DESCRIPTION>
                                                                                                      
   ! WRF registration routine
   USE module_wrf_setservices, ONLY: WRF_register
   ! ESMF module, defines all ESMF data types and procedures
   USE ESMF_Mod
   ! Not-yet-implemented ESMF features
   USE module_esmf_extensions
   ! Component-independent utilities
   USE module_metadatautils, ONLY: GetTimesFromStates

   IMPLICIT NONE

   ! Local variables

   ! Components
   TYPE(ESMF_GridComp) :: WRFcompGridded   ! WRF

   ! State, Virtual Machine, and DELayout
   TYPE(ESMF_VM) :: vm
   TYPE(ESMF_State) :: importState, exportState

   ! A clock, some times, and a time step
   TYPE(ESMF_Clock) :: driverClock
   TYPE(ESMF_Time) :: startTime
   TYPE(ESMF_Time) :: stopTime
   TYPE(ESMF_TimeInterval) :: couplingInterval

   ! Return codes for error checks
   INTEGER :: rc

   ! Warn users that this is not yet ready for general use.  
   PRINT *, '                      W A R N I N G                          '
   PRINT *, '  ESMF COUPLING CAPABILITY IS EXPERIMENTAL AND UNSUPPORTED   '
   PRINT *, '                 IN THIS VERSION OF WRF                      '
   PRINT *, '          U S E   A T   Y O U R   O W N   R I S K            '

   ! This call includes everything that must be done before ESMF_Initialize()
   ! is called.
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   ! Initialize ESMF, get the default Global VM, and set
   ! the default calendar to be Gregorian.
   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_Initialize failed' )
   ENDIF
   CALL ESMF_SetInitialized()   ! eliminate this once ESMF does it internally
!TBH:  these cause hangs on bluesky, PET* files never get written...  
!TBH:   CALL ESMF_LogSet( maxElements=1, verbose=ESMF_TRUE, flush=ESMF_TRUE, rc=rc )
!TBH:   CALL ESMF_LogSet( maxElements=1, rc=rc )
!TBH:   IF ( rc /= ESMF_SUCCESS ) THEN
!TBH:     CALL wrf_error_fatal( 'ESMF_LogSet failed' )
!TBH:   ENDIF

   ! Create the top level Gridded Component, passing in the default VM.
   WRFcompGridded = ESMF_GridCompCreate(vm, "WRF Model", rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompCreate failed' )
   ENDIF

   ! Create empty import and export states
   importState = ESMF_StateCreate("WRF Import State", statetype=ESMF_STATE_IMPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_StateCreate(importState) failed' )
   ENDIF
   exportState = ESMF_StateCreate("WRF Export State", statetype=ESMF_STATE_EXPORT, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_StateCreate(exportState) failed' )
   ENDIF

   ! Create top-level clock.  There is no way to create an "empty" clock, so 
   ! stuff in bogus values for start time, stop time, and time step and fix 
   ! them after "WRF Init" returns.  
   CALL ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                     h=0, m=0, s=0, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_TimeSet(startTime) failed' )
   ENDIF
   CALL ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                     h=12, m=0, s=0, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_TimeSet(stopTime) failed' )
   ENDIF
   CALL ESMF_TimeIntervalSet(couplingInterval, s=2, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_TimeIntervalSet(couplingInterval) failed' )
   ENDIF
   driverClock = ESMF_ClockCreate(timeStep=couplingInterval, startTime=startTime, &
                                  stopTime=stopTime, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_ClockCreate failed' )
   ENDIF

   ! Register the top level Gridded Component
   CALL ESMF_GridCompSetServices(WRFcompGridded, WRF_register, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompSetServices(WRFcompGridded) failed' )
   ENDIF

   ! Init, Run, and Finalize section
   ! Phase 1 init returns WRF time and decomposition information as 
   ! exportState metadata.  
   CALL ESMF_GridCompInitialize(WRFcompGridded, importState, exportState, &
                                driverClock, phase=1, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompInitialize(WRFcompGridded phase 1) failed' )
   ENDIF

   ! For now, use settings from WRF component intialization to set up 
   ! top-level clock.  Per suggestion from ESMF Core team, these are passed 
   ! back from "WRF init" as attributes on exportState.  
   CALL GetTimesFromStates( exportState, startTime, stopTime, couplingInterval )
   ! update driver clock
   CALL ESMF_ClockDestroy(driverClock, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_ClockDestroy failed' )
   ENDIF
   driverClock = ESMF_ClockCreate(timeStep=couplingInterval, startTime=startTime, &
                                  stopTime=stopTime, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_ClockCreate(driverClock) failed' )
   ENDIF
   CALL wrf_clockprint ( 150, driverClock, 'driverClock before phase 2 WRF init' )

   ! Phase 2 init sets up WRF importState and exportState.  
   CALL ESMF_GridCompInitialize(WRFcompGridded, importState, exportState, &
                                driverClock, phase=2, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompInitialize(WRFcompGridded phase 2) failed' )
   ENDIF

   CALL wrf_debug ( 150, 'wrf_ESMFApp:  begin time stepping...' )
   ! main time-stepping loop
   DO WHILE ( .NOT. ESMF_ClockIsStopTime(driverClock, rc) )

     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal( 'ESMF_ClockIsStopTime failed' )
     ENDIF

     ! Run WRF
     CALL wrf_debug ( 150, 'wrf_ESMFApp:  calling ESMF_GridCompRun(WRFcompGridded)...' )
     CALL ESMF_GridCompRun(WRFcompGridded, importState, exportState, &
                           driverClock, rc=rc)
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal( 'ESMF_GridCompRun failed' )
     ENDIF
     CALL wrf_debug ( 150, 'wrf_ESMFApp:  back from ESMF_GridCompRun(WRFcompGridded)...' )

     ! advance clock to next coupling time step
     CALL ESMF_ClockAdvance( driverClock, rc=rc )
     IF ( rc /= ESMF_SUCCESS ) THEN
       CALL wrf_error_fatal( 'ESMF_ClockAdvance failed' )
     ENDIF
     CALL wrf_clockprint ( 150, driverClock, 'driverClock after ESMF_ClockAdvance' )

   ENDDO
   CALL wrf_debug ( 150, 'wrf_ESMFApp:  done time stepping...' )

   CALL wrf_debug ( 150, 'wrf_ESMFApp:  calling ESMF_GridCompFinalize(WRFcompGridded)...' )
   ! clean up WRF
   CALL ESMF_GridCompFinalize(WRFcompGridded, importState, exportState, &
                              driverClock, rc=rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompFinalize failed' )
   ENDIF
   CALL wrf_debug ( 150, 'wrf_ESMFApp:  back from ESMF_GridCompFinalize(WRFcompGridded)...' )
 
   ! Clean up

   CALL wrf_debug ( 150, 'wrf_ESMFApp:  cleaning up ESMF objects...' )
   CALL ESMF_GridCompDestroy(WRFcompGridded, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_GridCompDestroy failed' )
   ENDIF
   CALL ESMF_StateDestroy(importState, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_StateDestroy(importState) failed' )
   ENDIF
   CALL ESMF_StateDestroy(exportState, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_StateDestroy(exportState) failed' )
   ENDIF
   CALL ESMF_ClockDestroy(driverClock, rc)
   IF ( rc /= ESMF_SUCCESS ) THEN
     CALL wrf_error_fatal( 'ESMF_Destroy(driverClock) failed' )
   ENDIF

   CALL wrf_debug ( 150, 'wrf_ESMFApp:  calling ESMF_Finalize()...' )
   CALL ESMF_Finalize( rc=rc )
   CALL wrf_debug ( 150, 'wrf_ESMFApp:  back from ESMF_Finalize()...' )

END PROGRAM wrf_ESMFApp


