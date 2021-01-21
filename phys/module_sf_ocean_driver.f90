

MODULE module_sf_ocean_driver

CONTAINS


   SUBROUTINE OCEAN_DRIVER(tml,t0ml,hml,h0ml,huml,hvml,ust,u_phy,v_phy, &
                      tmoml,f,g,oml_gamma,                         &
                      XLAND,HFX,LH,TSK,GSW,GLW,EMISS,              &
                      DELTSM,STBOLT,OML_RELAXATION_TIME,           &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      its,ite, jts,jte, kts,kte,                   &
                      sf_ocean_physics,okms, okme,                 & 
                      om_tmp,om_s,om_u, om_v, om_depth, om_ml,     & 
                      om_lat, om_lon,                              & 
                      QFX,                                         & 
                      rdx, rdy, msfu, msfv, msft,xtime,om_tini,om_sini,id,omdt, & 
                      itimestep)


   USE module_state_description, ONLY : OMLSCHEME , PWP3DSCHEME
   USE module_sf_oml
   USE module_sf_3dpwp

   IMPLICIT NONE






























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



   INTEGER ::  I,J



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



  stepom=nint(omdt*60/deltsm)
  stepom = max(stepom,1) 



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



elseif ( sf_ocean_physics .eq. PWP3DSCHEME ) then 
       call wrf_debug ( 100, 'call 3DPWP' )
       if ( itimestep .eq. 1 .or. mod(itimestep, stepom) .eq. 0 ) then
         
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

   END SUBROUTINE OCEAN_DRIVER



END MODULE module_sf_ocean_driver
