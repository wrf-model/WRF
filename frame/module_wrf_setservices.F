!WRF:DRIVER_LAYER:ESMF
!

MODULE module_wrf_setservices
!<DESCRIPTION>
! This module defines WRF ESMF "Set Services" method wrf_register().  
! This module is only built if the ESMF superstructure is used.  
!</DESCRIPTION>

   USE WRF_ESMF_MOD
   USE module_wrf_esmf_super, ONLY: wrf_init, wrf_run, wrf_finalize

   IMPLICIT NONE

   ! everything is private by default
   PRIVATE

   ! Public entry point for ESMF_GridCompSetServices()
   PUBLIC WRF_register


CONTAINS

   SUBROUTINE wrf_register(gcomp, rc)
     TYPE(ESMF_GridComp), INTENT(INOUT) :: gcomp
     INTEGER, INTENT(OUT) :: rc
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

     ! Register the callback routines.

     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETINIT, &
                                     wrf_init, ESMF_SINGLEPHASE, rc)
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETRUN, &
                                     wrf_run, ESMF_SINGLEPHASE, rc)
     call ESMF_GridCompSetEntryPoint(gcomp, ESMF_SETFINAL, &
                                     wrf_finalize, ESMF_SINGLEPHASE, rc)
     print *, "WRF-ESMF:  Registered Initialize, Run, and Finalize routines"

   END SUBROUTINE wrf_register


END MODULE module_wrf_setservices


