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
   USE module_comp

   IMPLICIT NONE

   ! Local variables

   ! Components
   TYPE(WRF_COMP_GridComp) :: compGridded

   ! State, Virtual Machine, and DELayout
   TYPE(WRF_COMP_VM) :: vm
   TYPE(WRF_COMP_State) :: dummystate

   ! A clock, a calendar, and timesteps
   TYPE(WRF_COMP_Clock) :: dummyclock

   ! Return codes for error checks
   INTEGER :: rc
        

   ! This call includes everything that must be done before WRF_COMP_Initialize()
   ! is called.
   CALL init_modules(1)   ! Phase 1 returns after MPI_INIT() (if it is called)

   ! Initialize ESMF, get the default Global VM, and set
   ! the default calendar to be Gregorian.
   CALL WRF_COMP_Initialize( vm=vm, defaultCalendar=WRF_COMP_CAL_GREGORIAN, rc=rc )

! TBH:  replace "print" statements!!

   ! Create the top level Gridded Component, passing in the default VM.
   compGridded = WRF_COMP_GridCompCreate(vm, "WRF Model", rc=rc)
   PRINT *, "WRF_COMP_GridCompCreate returned rc=", rc

   ! Register the top level Gridded Component
   CALL WRF_COMP_GridCompSetServices(compGridded, WRF_register, rc)
   PRINT *, "WRF_COMP_GridCompSetServices returned rc=", rc

   ! Init, Run, and Finalize section
   CALL WRF_COMP_GridCompInitialize(compGridded, dummystate, dummystate, &
                                dummyclock, rc=rc)
   PRINT *, "WRF_COMP_GridCompInitialize returned rc=", rc

   CALL WRF_COMP_GridCompRun(compGridded, dummystate, dummystate, dummyclock, rc=rc)
   PRINT *, "WRF_COMP_GridCompRun returned rc=", rc

   CALL WRF_COMP_GridCompFinalize(compGridded, dummystate, dummystate, dummyclock, rc=rc)
   PRINT *, "WRF_COMP_GridCompFinalize returned rc=", rc
 
 
   ! Clean up

   CALL WRF_COMP_GridCompDestroy(compGridded, rc)
   PRINT *, "WRF_COMP_GridCompDestroy returned rc=", rc

   CALL WRF_COMP_Finalize( rc=rc )

END PROGRAM wrf_ESMFApp


