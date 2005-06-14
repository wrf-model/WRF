!WRF:DRIVER_LAYER:MAIN
!

!<DESCRIPTION>
! Stand-alone ESMF Application Wrapper for WRF model.  This file contains the 
! main program and creates a top level ESMF Gridded Component.  
!
! This source file is only built when ESMF coupling is used.  
!
!</DESCRIPTION>


MODULE module_wrf_component_top
!<DESCRIPTION>
! This module defines wrf_component_init(), wrf_component_run(), and 
! wrf_component_finalize() routines that are called when WRF is run as an 
! ESMF component.  
!</DESCRIPTION>

   USE module_wrf_top
   USE ESMF_Mod
   USE module_esmf_extensions


   IMPLICIT NONE


CONTAINS


   SUBROUTINE wrf_component_init( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component init routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! Call WRF "init" routine
     ! Call WRF "init" routine, suppressing call to init_modules(1) since 
     ! it was already done by the AppDriver.  
     CALL wrf_init( no_init1=.TRUE. )

!$$$here...  TBH:  Need to connect head_grid%start_subtime and head_grid%stop_subtime to 
!$$$here...  TBH:  clock when building with ESMF library...  
!$$$here...  maybe call wrf_clock_override() or something...  
     head_grid%start_subtime = head_grid%start_time
     head_grid%stop_subtime = head_grid%stop_time

     rc = ESMF_SUCCESS

   END SUBROUTINE wrf_component_init



   SUBROUTINE wrf_component_run( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component run routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! Call WRF "run" routine
     CALL wrf_run( )

     rc = ESMF_SUCCESS

   END SUBROUTINE wrf_component_run



   SUBROUTINE wrf_component_finalize( gcomp, importState, exportState, clock, rc )
     TYPE(ESMF_GridComp), TARGET, INTENT(INOUT) :: gcomp
     TYPE(ESMF_State),    TARGET, INTENT(INOUT) :: importState, exportState
     TYPE(ESMF_Clock),    TARGET, INTENT(INOUT) :: clock
     INTEGER,                     INTENT(  OUT) :: rc
!<DESCRIPTION>
!     WRF component finalize routine.
!
!     The arguments are:
!       gcomp           Component
!       importState     Importstate
!       exportState     Exportstate
!       clock           External clock
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     ! Local variables
     TYPE(ESMF_GridComp), POINTER :: p_gcomp
     TYPE(ESMF_State),    POINTER :: p_importState
     TYPE(ESMF_State),    POINTER :: p_exportState
     TYPE(ESMF_Clock),    POINTER :: p_clock
     p_gcomp => gcomp
     p_importState => importState
     p_exportState => exportState
     p_clock => clock
     ! NOTE:  It will be possible to remove this call once ESMF supports 
     !        interfaces ESMF_ClockGetCurrent(), ESMF_ImportStateGetCurrent(), 
     !        ESMF_ExportStateGetCurrent(), and ESMF_GridCompGetCurrent().  
     CALL ESMF_SetCurrent( gcomp=p_gcomp, importState=p_importState, &
                           exportState=p_exportState, clock=p_clock )

     ! Call WRF "finalize" routine, suppressing call to MPI_FINALIZE so 
     ! ESMF can do it (if needed) during ESMF_Finalize().  
     CALL wrf_finalize( no_shutdown=.TRUE. )

     rc = ESMF_SUCCESS

   END SUBROUTINE wrf_component_finalize


END MODULE module_wrf_component_top




MODULE module_wrf_setservices
!<DESCRIPTION>
! This module defines WRF "Set Services" method wrf_register() 
! used for ESMF coupling.  
!</DESCRIPTION>

   USE module_wrf_component_top, ONLY: wrf_component_init, wrf_component_run, &
                                       wrf_component_finalize
   USE ESMF_Mod

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry point for ESMF_GridCompSetServices()
   PUBLIC WRF_register


CONTAINS


   SUBROUTINE wrf_register(gcomp, rc)
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     INTEGER, INTENT(OUT) :: rc
     INTEGER :: finalrc
!
!<DESCRIPTION>
!     WRF_register - Externally visible registration routine
!
!     User-supplied SetServices routine.
!     The Register routine sets the subroutines to be called
!     as the init, run, and finalize routines.  Note that these are
!     private to the module.
!
!     The arguments are:
!       gcomp           Component
!       rc              Return code; equals ESMF_SUCCESS if there are no errors,
!                       otherwise ESMF_FAILURE.
!</DESCRIPTION>

     finalrc = ESMF_SUCCESS
     ! Register the callback routines.
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     wrf_component_init, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  ESMF_GridCompSetEntryPoint(wrf_component_init) failed with rc = ',rc
        finalrc = rc
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
                                     wrf_component_run, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  ESMF_GridCompSetEntryPoint(wrf_component_run) failed with rc = ',rc
        finalrc = rc
     ENDIF
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
                                     wrf_component_finalize, ESMF_SINGLEPHASE, rc)
     IF ( rc /= ESMF_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  ESMF_GridCompSetEntryPoint(wrf_component_finalize) failed with rc = ',rc
        finalrc = rc
     ENDIF
     print *, "WRF:  Registered Initialize, Run, and Finalize routines"

     rc = finalrc

   END SUBROUTINE wrf_register

END MODULE module_wrf_setservices






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

   IMPLICIT NONE

   ! Local variables

   ! Components
   TYPE(ESMF_GridComp) :: compGridded

   ! State, Virtual Machine, and DELayout
   TYPE(ESMF_VM) :: vm
   TYPE(ESMF_State) :: importState, exportState

   ! A clock, some times, and a time step
   TYPE(ESMF_Clock) :: driverClock
   TYPE(ESMF_Time) :: startTime
   TYPE(ESMF_Time) :: stopTime
   TYPE(ESMF_TimeInterval) :: timeStep

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

!$$$ TBH:  replace "print" statements!!

   ! Create the top level Gridded Component, passing in the default VM.
   compGridded = ESMF_GridCompCreate(vm, "WRF Model", rc=rc)
   PRINT *, "ESMF_GridCompCreate returned rc=", rc

   ! Create empty import and export states
   importState = ESMF_StateCreate("WRF Import State", rc=rc)
   PRINT *, "ESMF_StateCreate(WRF Import State) returned rc=", rc
   exportState = ESMF_StateCreate("WRF Export State", rc=rc)
   PRINT *, "ESMF_StateCreate(WRF Export State) returned rc=", rc

   ! Create "dummy" clock (for now)
!$$$here...  this is completely bogus - fix it!!  
   CALL ESMF_TimeSet(startTime, yy=2000, mm=1, dd=1, &
                     h=0, m=0, s=0, rc=rc)
   CALL ESMF_TimeSet(stopTime, yy=2000, mm=1, dd=1, &
                     h=12, m=0, s=0, rc=rc)
   CALL ESMF_TimeIntervalSet(timeStep, s=2, rc=rc)
   driverClock = ESMF_ClockCreate(timeStep=timeStep, startTime=startTime, &
                                  stopTime=stopTime, rc=rc)


   ! Register the top level Gridded Component
   CALL ESMF_GridCompSetServices(compGridded, WRF_register, rc)
   PRINT *, "ESMF_GridCompSetServices returned rc=", rc

   ! Init, Run, and Finalize section
   CALL ESMF_GridCompInitialize(compGridded, importState, exportState, &
                                driverClock, rc=rc)
   PRINT *, "ESMF_GridCompInitialize returned rc=", rc

   CALL ESMF_GridCompRun(compGridded, importState, exportState, &
                         driverClock, rc=rc)
   PRINT *, "ESMF_GridCompRun returned rc=", rc

   CALL ESMF_GridCompFinalize(compGridded, importState, exportState, &
                              driverClock, rc=rc)
   PRINT *, "ESMF_GridCompFinalize returned rc=", rc
 
 
   ! Clean up

   CALL ESMF_GridCompDestroy(compGridded, rc)
   PRINT *, "ESMF_GridCompDestroy returned rc=", rc
   CALL ESMF_StateDestroy(importState, rc)
   PRINT *, "ESMF_StateDestroy(importState) returned rc=", rc
   CALL ESMF_StateDestroy(exportState, rc)
   PRINT *, "ESMF_StateDestroy(exportState) returned rc=", rc
   CALL ESMF_ClockDestroy(driverClock, rc)
   PRINT *, "ESMF_ClockDestroy returned rc=", rc

   CALL ESMF_Finalize( rc=rc )

END PROGRAM wrf_ESMFApp


