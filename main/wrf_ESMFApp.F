!WRF:DRIVER_LAYER:MAIN
!

PROGRAM wrf_ESMFApp

!<DESCRIPTION>
! Stand-alone ESMF Application Wrapper for WRF model.  This file contains the 
! main program, and creates a top level ESMF Gridded Component to contain any 
! other Components.  
!
!</DESCRIPTION>
                                                                                                      
   ! WRF registration routine
   USE module_wrf_setservices, ONLY: WRF_register
   ! ESMF module, defines all ESMF data types and procedures
   USE WRF_ESMF_MOD

   IMPLICIT NONE

   ! Local variables

   ! Components
   TYPE(ESMF_GridComp) :: compGridded

   ! State, Virtual Machine, and DELayout
   TYPE(ESMF_VM) :: vm
   TYPE(ESMF_State) :: dummystate

   ! A clock, a calendar, and timesteps
   TYPE(ESMF_Clock) :: dummyclock

   ! Return codes for error checks
   INTEGER :: rc
        

   ! This call includes everything that must be done before ESMF_Initialize()
   ! is called.
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   ! Initialize ESMF, get the default Global VM, and set
   ! the default calendar to be Gregorian.
   CALL ESMF_Initialize( vm=vm, defaultCalendar=ESMF_CAL_GREGORIAN, rc=rc )

! TBH:  replace "print" statements!!

   ! Create the top level Gridded Component, passing in the default VM.
   compGridded = ESMF_GridCompCreate(vm, "WRF Model", rc=rc)
   PRINT *, "ESMF_GridCompCreate returned rc=", rc

   ! Register the top level Gridded Component
   CALL ESMF_GridCompSetServices(compGridded, WRF_register, rc)
   PRINT *, "ESMF_GridCompSetServices returned rc=", rc

   ! Init, Run, and Finalize section
   CALL ESMF_GridCompInitialize(compGridded, dummystate, dummystate, &
                                dummyclock, rc=rc)
   PRINT *, "ESMF_GridCompInitialize returned rc=", rc

   CALL ESMF_GridCompRun(compGridded, dummystate, dummystate, dummyclock, rc=rc)
   PRINT *, "ESMF_GridCompRun returned rc=", rc

   CALL ESMF_GridCompFinalize(compGridded, dummystate, dummystate, dummyclock, rc=rc)
   PRINT *, "ESMF_GridCompFinalize returned rc=", rc
 
 
   ! Clean up

   CALL ESMF_GridCompDestroy(compGridded, rc)
   PRINT *, "ESMF_GridCompDestroy returned rc=", rc

   CALL ESMF_Finalize( rc=rc )

END PROGRAM wrf_ESMFApp


