!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_sf_ocean_driver

CONTAINS

!----------------------------------------------------------------
   SUBROUTINE OCEAN_DRIVER(tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy, &
                      tmoml,f,g,oml_gamma,                         &
                      XLAND,HFX,LH,TSK,GSW,GLW,EMISS,              &
                      DELTSM,STBOLT,OML_RELAXATION_TIME,           &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      its,ite, jts,jte, kts,kte,                   &
                      sf_ocean_physics,okms, okme,                 & !cyl
                      om_tmp,om_s,om_u, om_v, om_depth, om_ml,     & !cyl
                      om_lat, om_lon,                              & !cyl
                      QFX,                                         & !cyl 
                      rdx, rdy, msfu, msfv, msft,xtime,om_tini,om_sini,id,omdt, & !cyl
                      itimestep)

!----------------------------------------------------------------
#if ( EM_CORE==1)
   USE module_state_description, ONLY : OMLSCHEME , PWP3DSCHEME
   USE module_sf_oml
   USE module_sf_3dpwp
#endif

   IMPLICIT NONE
!----------------------------------------------------------------
!
!  SUBROUTINE OCEANML CALCULATES THE SEA SURFACE TEMPERATURE (TSK)
!  FROM A SIMPLE OCEAN MIXED LAYER MODEL BASED ON
!  (Pollard, Rhines and Thompson (1973).
!
!-- TML         ocean mixed layer temperature (K)
!-- T0ML        ocean mixed layer temperature (K) at initial time
!-- TMOML       top 200 m ocean mean temperature (K) at initial time
!-- HML         ocean mixed layer depth (m)
!-- H0ML        ocean mixed layer depth (m) at initial time
!-- HUML        ocean mixed layer u component of wind
!-- HVML        ocean mixed layer v component of wind
!-- OML_GAMMA   deep water lapse rate (K m-1)
!-- OML_RELAXATION_TIME    Time scale (s) to relax TML to T0ML, H to H0,
!                          HUML and HVML to 0; value <=0 means no relaxation
!-- UAIR,VAIR   lowest model level wind component
!-- UST         frictional velocity
!-- HFX         upward heat flux at the surface (W/m^2)
!-- LH          latent heat flux at the surface (W/m^2)
!-- TSK         surface temperature (K)
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- EMISS       emissivity of the surface
!-- XLAND       land mask (1 for land, 2 for water)
!-- STBOLT      Stefan-Boltzmann constant (W/m^2/K^4)
!-- F           Coriolis parameter
!-- DT          time step (second)
!-- G           acceleration due to gravity

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   REAL,     INTENT(IN   )   ::     DELTSM, STBOLT

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                          EMISS, &
                                                         XLAND, &
                                                           GSW, &
                                                           GLW, &
                                                           HFX, &
                                                            LH

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::     &
                                    TML,T0ML,HML,H0ML,HUML,HVML

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::     &
                                             U_PHY,V_PHY

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) ::     &
                                             UST, F, TMOML

   REAL,    INTENT(IN   )   ::     G
   REAL,    INTENT(IN   )   ::     OML_GAMMA, OML_RELAXATION_TIME

! LOCAL VARS

   INTEGER ::  I,J

! variables for subrouting Dpwp

  INTEGER, OPTIONAL, INTENT(IN )::  sf_ocean_physics
  integer :: okms, okme
  real, dimension(ims:ime, okms:okme, jms:jme), INTENT(INOUT):: OM_TMP,OM_S,OM_U,OM_V,OM_DEPTH
  real, dimension(ims:ime, okms:okme, jms:jme):: om_density 
  real, dimension(ims:ime, okms:okme, jms:jme), INTENT(IN):: OM_TINI,OM_SINI
  real, dimension(ims:ime, jms:jme),INTENT(INOUT):: OM_ML, OM_LAT, OM_LON
  REAL, INTENT(IN   ) :: rdx, rdy,xtime,omdt
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: msfu, msfv, msft,qfx
  INTEGER , INTENT(IN)        :: id,itimestep
  integer :: stepom

#if ( EM_CORE==1)
! calculate the steps of om

  stepom=nint(omdt*60/deltsm)
  stepom = max(stepom,1) 

! cyl:OML1D or DPWP

if ( sf_ocean_physics .eq. OMLSCHEME ) then

   DO J=jts,jte

         DO i=its,ite
            IF (XLAND(I,J).GT.1.5) THEN
               CALL OML1D(I,J,TML(i,j),T0ML(i,j),HML(i,j),H0ML(i,j),           &
                          HUML(i,j),HVML(i,j),TSK(i,j),HFX(i,j),               &
                          LH(i,j),GSW(i,j),GLW(i,j),TMOML(i,j),                &
                          U_PHY(i,kts,j),V_PHY(i,kts,j),UST(i,j),F(i,j),       &
                          EMISS(i,j),STBOLT,G,DELTSM,OML_GAMMA,                &
                          OML_RELAXATION_TIME,                                 &
                          ids,ide, jds,jde, kds,kde,                           &
                          ims,ime, jms,jme, kms,kme,                           &
                          its,ite, jts,jte, kts,kte                            )
            ENDIF
         ENDDO

   ENDDO

! call 3DPWP

elseif ( sf_ocean_physics .eq. PWP3DSCHEME ) then 
       call wrf_debug ( 100, 'call 3DPWP' )
       if ( itimestep .eq. 1 .or. mod(itimestep, stepom) .eq. 0 ) then
         ! run 3DPWP only when the grid resolution larger than 3.0 km
          print*,'dx',1.0/rdx
          if ( 1.0/rdx .ge. 3000.0 .and. 1.0/rdy .ge. 3000.0 ) then  
             call DPWP(ims,ime, jms,jme, kms,kme,its,ite, jts,jte, kts,kte, &
                    ids,ide, jds,jde, kds,kde,okms, okme,                   &
                    om_tmp,om_s,om_u, om_v, om_density, om_depth, om_ml,    &
                    om_lat, om_lon,                                         &
                    HFX, QFX, GSW, GLW, UST, U_PHY, V_PHY,                  &
                    STBOLT, DELTSM, TSK, LH, XLAND,                         &
                    rdx, rdy, msfu, msfv, msft,xtime,om_tini,om_sini,id,omdt)
          else
              print*,'Domain',id,' no ocean'
              do  i = its, ite
                  do  j = jts, jte
                      if (XLAND(i,j).GE.1.5)then
                         TSK(i,j) = om_tmp(i, 1, j)
                      endif
                  enddo
              enddo
 
          endif 
       endif
endif 
#endif

   END SUBROUTINE OCEAN_DRIVER

!----------------------------------------------------------------

END MODULE module_sf_ocean_driver
