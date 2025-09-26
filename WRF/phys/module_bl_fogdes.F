MODULE module_bl_fogdes

  USE module_model_constants
  USE module_bl_mynn, only: qcgmin, gno, gpw

!-------------------------------------------------------------------
  IMPLICIT NONE
!-------------------------------------------------------------------

CONTAINS

  SUBROUTINE bl_fogdes(&
               vdfg,qc_curr,dtbl,rho,dz8w,grav_settling,dqc,       &
               ids,ide, jds,jde, kds,kde,                          &
               ims,ime, jms,jme, kms,kme,                          &
               its,ite, jts,jte, kts,kte                           &
                                                                   )

!  This module was written by Joseph Olson (CIRES-NOAA/GSD/AMB) to allow
!  gravitational settling of cloud droplets in the atmosphere for all 
!  PBL schemes (when grav_settling > 0). Previously, this option was only 
!  available for the MYNN PBL scheme.
!
!  This module is a companion to module_sf_fogdes, which calulcates the 
!  (fog) deposition onto the surface, so it uses a consistent formulation
!  at k=1. Currently, it uses a simple form taken from Dyunkerke (1991)
!  and Dyunkerke and Driedonks (1988), but uses a lower settling 
!  velocity coefficient (gno = 1.0 instead of 4.6).
!
!    settling velocity:            Vd = gno*(qc)**(2/3)
!    cloud water flux:  gflux = Vd*qc = gno*(qc)**(5/3)
!
!  This form assumes a constant number concentration: 10**8 /m**3 for
!  gno = 4.6 and approx .2*10**8 /m**3 for gno = 1.0.
!
! References:
!
! Dyunkerke, P.G. (1991), Radiation fog: a comparison of model simulations
!     with detailed observations, Mon. Wea. Rev., 119, 324-341.
! Nakanishi, Mikio (2000), Large-eddy simulation of radiation fog,
!     Boundary Layer Meteorology, 94, 461-493. 
!
!======================================================================
! Definitions
!-----------
!-- vdfg          deposition velocity of fog (m/s)
!-- qc_curr       cloud water mixing ratio (kg/kg)
!-- dqc           cloud water mixing ratio tendency
!-- dtbl          timestep (s)
!-- rho           density of the air (kg/m^3)
!-- dp_fog        mean fog droplet diameter (m)
!-- dz8w          dz between full levels (m)
!-- grav_settling flag for fog deposition at the lowest atmos layer
!           = 2   FogDES scheme
!           = 1   use Duynkerke (1991) - same as in atmos (above k = 1)
!           = 0   No gravitational settling
!-- lwc           cloud liquid water content (kg/m^3)
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
!******************************************************************
!------------------------------------------------------------------

   INTEGER, INTENT(IN)                       :: ims,ime,jms,jme,kms,kme &
                                               ,its,ite,jts,jte,kts,kte &
                                               ,ids,ide,jds,jde,kds,kde

   INTEGER, INTENT(IN)                       :: grav_settling

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN),OPTIONAL    :: qc_curr
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN)             :: rho
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(IN   )          :: dz8w

   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(IN),OPTIONAL    :: vdfg

   REAL, INTENT(INOUT),OPTIONAL                               :: dtbl

!JOE-added for Dyunkerke(1991) & Dyunkerke and Driedonks (1988)
!    gravitational settling above the surface (creates qc tendency).
   REAL,parameter :: gpw2=0.66666666666667
   REAL :: gfluxp,gfluxm
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
                                       INTENT(INOUT),OPTIONAL :: dqc
!JOE-end

! Local variables
   INTEGER :: i,j,k,grav_settling2
!------------------------------------------------------------------

  grav_settling2 = MIN(REAL(grav_settling), 1.)

   DO j=jts,jte
     DO i=its,ite

       !!====================================================
       !! Calculate gravitational settling in the atmosphere.
       !! This uses Dyunkerke (referenced above). Note that 
       !! only the cloud mixing ratio is settled, not the
       !! number concentration. 
       !!====================================================

       k=kts

       IF (qc_curr(i,k,j) > qcgmin) THEN
          gfluxm=grav_settling2*qc_curr(i,k,j)*vdfg(i,j)
       ELSE
          gfluxm=0.
       ENDIF

       IF (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)) > qcgmin) THEN
          gfluxp=grav_settling2*gno* &
                & (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)))**gpw
       ELSE
          gfluxp=0.
       ENDIF

       dqc(i,k,j)=dqc(i,k,j) + (gfluxp - gfluxm)/dz8w(i,kts,j)    !*dtbl

       !print*,"in bl_fogdes: i,j=",i,j
       !print*,"vdfg=",vdfg(i,j)," qc=",qc_curr(i,k,j)," dtbl=",dtbl
       !print*,"dqc=",dqc(i,k,j)," gfluxm=",gfluxm," gfluxp=",gfluxp

       DO k=kts+1,kte-1

          IF (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)) > qcgmin) THEN
             gfluxp=grav_settling2*gno* &
                   & (.5*(qc_curr(i,k+1,j)+qc_curr(i,k,j)))**gpw
          ELSE
             gfluxp=0.
          ENDIF

          IF (.5*(qc_curr(i,k-1,j)+qc_curr(i,k,j)) > qcgmin) THEN
             gfluxm=grav_settling2*gno* &
                   & (.5*(qc_curr(i,k-1,j)+qc_curr(i,k,j)))**gpw
          ELSE
             gfluxm=0.
          ENDIF

          dqc(i,k,j)= dqc(i,k,j) + (gfluxp - gfluxm)/dz8w(i,k,j)  !*dtbl

       ENDDO

      ! dqc(i,kte,j)=0.

     ENDDO
   ENDDO

  END SUBROUTINE bl_fogdes

! ==================================================================

END MODULE module_bl_fogdes
