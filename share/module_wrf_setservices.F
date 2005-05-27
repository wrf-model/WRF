!WRF:DRIVER_LAYER:COMP
!

MODULE module_wrf_setservices
!<DESCRIPTION>
! This module defines WRF "Set Services" method wrf_register().  
! This module is only built if ESMF is used for coupling.  
!</DESCRIPTION>

#ifdef ESMF_HACK

   USE module_wrf_comp_super, ONLY: wrf_init, wrf_run, wrf_finalize
   USE module_comp

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry point for WRF_COMP_GridCompSetServices()
   PUBLIC WRF_register


CONTAINS


   SUBROUTINE wrf_register(gcomp, rc)
     TYPE(WRF_COMP_GridComp), INTENT(INOUT) :: gcomp
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
!       rc              Return code; equals WRF_COMP_SUCCESS if there are no errors,
!                       otherwise WRF_COMP_FAILURE.
!</DESCRIPTION>

     finalrc = WRF_COMP_SUCCESS
     ! Register the callback routines.
     call WRF_COMP_GridCompSetEntryPoint(gcomp, WRF_COMP_SETINIT, &
                                     wrf_init, WRF_COMP_SINGLEPHASE, rc)
     IF ( rc /= WRF_COMP_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  WRF_COMP_GridCompSetEntryPoint(wrf_init) failed with rc = ',rc
        finalrc = rc
     ENDIF
     call WRF_COMP_GridCompSetEntryPoint(gcomp, WRF_COMP_SETRUN, &
                                     wrf_run, WRF_COMP_SINGLEPHASE, rc)
     IF ( rc /= WRF_COMP_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  WRF_COMP_GridCompSetEntryPoint(wrf_run) failed with rc = ',rc
        finalrc = rc
     ENDIF
     call WRF_COMP_GridCompSetEntryPoint(gcomp, WRF_COMP_SETFINAL, &
                                     wrf_finalize, WRF_COMP_SINGLEPHASE, rc)
     IF ( rc /= WRF_COMP_SUCCESS) THEN
        ! TBH:  replace "print"
        print *,'ERROR:  WRF_COMP_GridCompSetEntryPoint(wrf_finalize) failed with rc = ',rc
        finalrc = rc
     ENDIF
     print *, "WRF:  Registered Initialize, Run, and Finalize routines"

     rc = finalrc

   END SUBROUTINE wrf_register

#endif

END MODULE module_wrf_setservices


