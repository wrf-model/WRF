!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_refl
CONTAINS

#if (NMM_CORE==1)
   SUBROUTINE diagnostic_output_calc_refl(                            &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      its,ite, jts,jte, kts,kte,                      & ! tile dims
                      diagflag,                                       &
                      refd_max,refl_10cm                              &
                                                                     )
!----------------------------------------------------------------------


   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAGFLAG      logical flag to indicate if this is a history output time
!-- REF_MAX       max derived radar reflectivity
!-- REFL_10CM     model computed 3D reflectivity
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      its,ite, jts,jte, kts,kte

   LOGICAL,   INTENT(IN   )    ::   diagflag


   INTEGER :: i,j,k

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                                      refl_10cm

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
                                                       refd_max

       DO j=jts,jte
       DO i=its,ite
         refd_max(i,j)    = -35.
       ENDDO
       ENDDO

     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite

! Calculate the max radar reflectivity between output times

       IF ( refl_10cm(i,k,j) .GT. refd_max(i,j) ) THEN
         refd_max(i,j) = refl_10cm(i,k,j)
       ENDIF
     ENDDO
     ENDDO
     ENDDO
!  !$OMP END PARALLEL DO


   END SUBROUTINE diagnostic_output_calc_refl
#endif



END MODULE module_diag_refl
