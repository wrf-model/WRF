!WRF:MODEL_LAYER:PHYSICS
!
SUBROUTINE microphysics_driver(th_phy, moist_new, moist_old, w,       &
                               rho, pi_phy, p_phy, RAINNC, RAINNCV,   &
                               z, ht, dz8w, p8w, dt,dx,dy,            &
!#$                            config_flags, n_moist,                 &
                               MP_SCHEME,    n_moist,                 &
                               warm_rain,                             &
                               XLAND,itimestep,                       &
                               F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,      &
                               LOWLYR,                                &
                               ids,ide, jds,jde, kds,kde,             & 
                               ims,ime, jms,jme, kms,kme,             &
                               its,ite, jts,jte, kts,kte              )
!----------------------------------------------------------------------
! Framework
!#$USE module_state_description
! Model Layer
!#$USE module_bc
   USE module_model_constants
!#$USE module_wrf_error

! *** add new modules of schemes here

!#$USE module_mp_kessler
!#$USE module_mp_lin
!#$USE module_mp_ncloud3
!#$USE module_mp_ncloud5
!#$USE module_mp_eta
   USE module_mp_etanew
    
   use module_mpp
!----------------------------------------------------------------------
   ! This driver calls subroutines for the microphys.
   !
   ! Schemes
   !
   ! 1. Kessler scheme
   ! 2. Lin et al. (1983), Ruttledge and Hibbs (1984)
   ! 3. NCEP cloud3, Hong et al. (1998) with some mod, Dudhia (1989)
   ! 4. NCEP cloud5, Hong et al. (1998) with some mod, Ruttledge and Hobbs (1984)
   ! 
!----------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------  
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------  
! In WRF, kms (smallest number) is the bottom level and kme (largest 
! number) is the top level.  In your scheme, if 1 is at the top level, 
! then you have to reverse the order in the k direction.
!                 
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         .
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Definitions
!-----------
! Rho_d      dry density (kg/m^3)
! Theta_m    moist potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!----------------------------------------------------------------------
!-- th_phy        potential temperature    (K)
!-- moist_new     updated moisture array   (kg/kg)
!-- moist_old     Old moisture array       (kg/kg)
!-- rho           density of air           (kg/m^3)
!-- pi            exner function           (dimensionless)
!-- p             pressure                 (Pa)
!-- RAINNC        grid scale precipitation (mm)
!-- RAINNCV       one time step grid scale precipitation (mm/step)
!-- z             Height above sea level   (m)
!-- dt            Time step              (s)
!-- config_flags  flag for configuration      ! change ---  ?????   
!-- n_moist       number of water substances   (integer)
!-- G             acceleration due to gravity  (m/s^2)
!-- CP            heat capacity at constant pressure for dry air (J/kg/K)
!-- R_d           gas constant for dry air (J/kg/K)
!-- R_v           gas constant for water vapor (J/kg/K)
!-- XLS           latent heat of sublimation   (J/kg)
!-- XLV           latent heat of vaporization  (J/kg)
!-- XLF           latent heat of melting       (J/kg)
!-- rhowater      water density                      (kg/m^3)
!-- rhosnow       snow density               (kg/m^3)
!-- F_ICE_PHY     Fraction of ice.
!-- F_RAIN_PHY    Fraction of rain.
!-- F_RIMEF_PHY   Mass ratio of rimed ice (rime factor)
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QS          species index for snow
!-- P_QG          species index for graupel
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

!#$TYPE(grid_config_rec_type),    INTENT(IN   )    :: config_flags
!
   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    ::       its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   )    ::               n_moist,itimestep
   LOGICAL,      INTENT(IN   )    ::   warm_rain
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT) ::                                     th_phy
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ),           &
         INTENT(INOUT) ::                                  moist_new

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                          z, &
                                                                 rho, &
                                                                dz8w, &
                                                                   w, &
                                                                 p8w, &
                                                              pi_phy, &
                                                               p_phy
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, n_moist ),           &
         INTENT(IN   ) ::                                  moist_old
!
   REAL, DIMENSION( ims:ime , jms:jme ),  INTENT(INOUT) ::    RAINNC, &
                                                              RAINNCV

   REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY

!

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: ht,XLAND

   REAL, INTENT(IN   ) :: dt,dx,dy

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR
 
   CHARACTER(20),INTENT(IN) :: MP_SCHEME
 
! LOCAL  VAR

   INTEGER :: i,j,k

   INTEGER,PARAMETER :: P_QV=1,P_QC=2

   CHARACTER(20),PARAMETER :: KESSLERSCHEME='KESSLERSCHEME'
   CHARACTER(20),PARAMETER :: LINSCHEME='LINSCHEME'
   CHARACTER(20),PARAMETER :: NCEPCLOUD3='NCEPCLOUD3'
   CHARACTER(20),PARAMETER :: NCEPCLOUD5='NCEPCLOUD5'
   CHARACTER(20),PARAMETER :: ETAMPSCHEME='ETAMPSCHEME'
   CHARACTER(20),PARAMETER :: ETAMPNEW='ETAMPNEW'

!---------------------------------------------------------------------
!  check for microphysics type.  We need a clean way to 
!  specify these things!
!---------------------------------------------------------------------

!#$if (config_flags%mp_physics .eq. 0) return

!-----------
!#$IF ( n_moist >= PARAM_FIRST_SCALAR ) THEN

!#$  micro_select: SELECT CASE(config_flags%mp_physics)
     micro_select: SELECT CASE(MP_SCHEME)               

!#$     CASE (KESSLERSCHEME)
        CASE (KESSLERSCHEME)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling kessler' )
!#$          CALL kessler( th_phy,                       &
!#$                  moist_new(ims,kms,jms,P_QV),        &
!#$                  moist_new(ims,kms,jms,P_QC),        &
!#$                  moist_old(ims,kms,jms,P_QC),        &
!#$                  moist_new(ims,kms,jms,P_QR),        &
!#$                  moist_old(ims,kms,jms,P_QR),        &
!#$                  rho, pi_phy, RAINNC,                &
!#$                  RAINNCV, dt, z, cp,                 &
!#$                  EP_2,SVP1,SVP2,SVP3,SVPT0,rhowater, &
!#$                  dz8w,                               &
!#$                  ids,ide, jds,jde, kds,kde,          &
!#$                  ims,ime, jms,jme, kms,kme,          &
!#$                  its,ite, jts,jte, kts,kte           )

        CASE (LINSCHEME)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling lin_et_al' )
!#$          CALL lin_et_al( th_phy,                     &
!#$                  moist_new(ims,kms,jms,P_QV),        &
!#$                  moist_new(ims,kms,jms,P_QC),        &
!#$                  moist_new(ims,kms,jms,P_QR),        &
!#$                  moist_new(ims,kms,jms,P_QI),        &
!#$                  moist_new(ims,kms,jms,P_QS),        &
!#$                  moist_new(ims,kms,jms,P_QG),        &
!#$                  moist_old(ims,kms,jms,P_QR),        &
!#$                  moist_old(ims,kms,jms,P_QS),        &
!#$                  moist_old(ims,kms,jms,P_QG),        &
!#$                  rho, pi_phy, p_phy, RAINNC,         &
!#$                  RAINNCV,dt, z,                      &
!#$                  ht, dz8w, G, cp, R_d, R_v,          &
!#$                  XLS, XLV, XLF, rhowater, rhosnow,   &
!#$                  EP_2,SVP1,SVP2,SVP3,SVPT0,          &
!#$                  P_QI, P_QS, P_QG,                   &
!#$                  PARAM_FIRST_SCALAR,                 &
!#$                  ids,ide, jds,jde, kds,kde,          &
!#$                  ims,ime, jms,jme, kms,kme,          & 
!#$                  its,ite, jts,jte, kts,kte           )

        CASE (NCEPCLOUD3)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling ncloud3' )
!#$          CALL ncloud3(th_phy,                        &
!#$                  moist_new(ims,kms,jms,P_QV),        &
!#$                  moist_new(ims,kms,jms,P_QC),        &
!#$                  moist_new(ims,kms,jms,P_QR),        &
!#$                  w, rho, pi_phy, p_phy, dz8w, RAINNC,&
!#$                  RAINNCV,dt,g,cp,cv,r_d,r_v,SVPT0,   &
!#$                  ep_1, ep_2, epsilon,                &
!#$                  XLS, XLV, XLF, rhoair0, rhowater,   &
!#$                  cliq,cice,psat,                     &
!#$                  ids,ide, jds,jde, kds,kde,          &
!#$                  ims,ime, jms,jme, kms,kme,          &
!#$                  its,ite, jts,jte, kts,kte           )

        CASE (NCEPCLOUD5)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling ncloud5' )
!#$          CALL  ncloud5(th_phy,                       &
!#$                  moist_new(ims,kms,jms,P_QV),        &
!#$                  moist_new(ims,kms,jms,P_QC),        &
!#$                  moist_new(ims,kms,jms,P_QR),        &
!#$                  moist_new(ims,kms,jms,P_QI),        &
!#$                  moist_new(ims,kms,jms,P_QS),        &
!#$                  w, rho, pi_phy, p_phy, dz8w, RAINNC,&
!#$                  RAINNCV,dt,g,cp,cv,r_d,r_v,SVPT0,   &
!#$                  ep_1, ep_2, epsilon,                &
!#$                  XLS, XLV, XLF, rhoair0, rhowater,   &
!#$                  cliq,cice,psat,                     &
!#$                  ids,ide, jds,jde, kds,kde,          &
!#$                  ims,ime, jms,jme, kms,kme,          &
!#$                  its,ite, jts,jte, kts,kte           )


        CASE (ETAMPSCHEME)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling etampshcheme' )
!#$          CALL ETAMP(itimestep,DT,XLAND,pi_phy,p8w,p_phy,th_phy,  &
!#$                  moist_new(ims,kms,jms,P_QV),        &
!#$                  moist_new(ims,kms,jms,P_QC),        &
!#$                  RAINNC,RAINNCV,T0ETA,Q0ETA,P0ETA,   &
!#$                  ids,ide, jds,jde, kds,kde,          &
!#$                  ims,ime, jms,jme, kms,kme,          &
!#$                  its,ite, jts,jte, kts,kte           )

!#$          IF (config_flags%cu_physics .eq. KFSCHEME) then
!#$             print*,'Please chose another convective scheme (BMJ)'
!#$             print*,'KF scheme has qr tendency, but eta mp has no qr field.'
!#$             stop
!#$          ENDIF

        CASE (ETAMPNEW)
!#$          CALL wrf_debug ( 200 , 'microphysics_driver: calling etampnew')

             CALL ETAMP_NEW(itimestep,DT,DX,DY,RAINNC,RAINNCV,&
                            p8w,p_phy,pi_phy,th_phy,          &
                            moist_new(ims,kms,jms,P_QV),      &
                            moist_new(ims,kms,jms,P_QC),      &          
                            LOWLYR,                           &
                            F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                            ids,ide, jds,jde, kds,kde,        &
                            ims,ime, jms,jme, kms,kme,        &
                            its,ite, jts,jte, kts,kte         )

      CASE DEFAULT 

!#$      WRITE( wrf_err_message , * ) 'microphysics_driver: no microphysics for n_moist = ',n_moist
!#$      CALL wrf_error_fatal ( wrf_err_message )

      END SELECT micro_select 

!#$ELSE

!#$   WRITE( wrf_err_message , * ) 'microphysics_driver: in microphysics for n_moist = ',n_moist
!#$   CALL wrf_error_fatal ( wrf_err_message )

!#$ENDIF

!#$CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )

   RETURN

   END SUBROUTINE microphysics_driver

