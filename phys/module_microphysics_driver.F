!WRF:MODEL_LAYER:PHYSICS
!
MODULE module_microphysics_driver
CONTAINS

SUBROUTINE microphysics_driver(th_phy, moist_new, moist_old, w,        &
                               rho, pi_phy, p_phy, RAINNC, RAINNCV,    &
                               z, ht, dz8w, p8w, dt,dx,dy,             &
                               config_flags, spec_zone, n_moist,       &
                               warm_rain,                              &
                               XLAND,itimestep,                        &
                               F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                               LOWLYR,                                 &
                               ids,ide, jds,jde, kds,kde,              & 
                               ims,ime, jms,jme, kms,kme,              &
                               i_start,i_end,j_start,j_end,kts,kte,num_tiles   )
! Framework
   USE module_state_description
! Model Layer
   USE module_bc
   USE module_model_constants
   USE module_wrf_error

! *** add new modules of schemes here

   USE module_mp_kessler
   USE module_mp_lin
   USE module_mp_ncloud3
   USE module_mp_ncloud5
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
    
!----------------------------------------------------------------------
   ! This driver calls subroutines for the microphys.
   !
   ! Schemes
   !
   ! Kessler scheme
   ! Lin et al. (1983), Rutledge and Hobbs (1984)
   ! WRF Single-Moment 3-class, Hong, Dudhia and Chen (2004)
   ! WRF Single-Moment 5-class, Hong, Dudhia and Chen (2004)
   ! WRF Single-Moment 6-class, Lim and Hong (2003 WRF workshop)
   ! Eta Grid-scale Cloud and Precipitation scheme (EGCP01, Ferrier)
   ! NCEP cloud3, Hong et al. (1998) with some mod, Dudhia (1989)
   ! NCEP cloud5, Hong et al. (1998) with some mod, Rutledge and Hobbs (1984)
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
!!!-- SR            one time step mass ratio of snow to total precip
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
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!======================================================================

   TYPE(grid_config_rec_type),    INTENT(IN   )    :: config_flags
!
   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
   INTEGER,      INTENT(IN   )    ::                         kts,kte
   INTEGER,      INTENT(IN   )    ::     n_moist,itimestep,num_tiles,spec_zone
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

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
!

   REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY

!

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: ht,XLAND

   REAL, INTENT(IN   ) :: dt,dx,dy

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR

 
! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij,sz

!---------------------------------------------------------------------
!  check for microphysics type.  We need a clean way to 
!  specify these things!
!---------------------------------------------------------------------

   if (config_flags%mp_physics .eq. 0) return
   IF( config_flags%specified .or. config_flags%nested ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte )

   DO ij = 1 , num_tiles

       its = max(i_start(ij),ids+sz)
       ite = min(i_end(ij),ide-1-sz)
       jts = max(j_start(ij),jds+sz)
       jte = min(j_end(ij),jde-1-sz)

!-----------
   IF ( n_moist >= PARAM_FIRST_SCALAR ) THEN

     micro_select: SELECT CASE(config_flags%mp_physics)

        CASE (KESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling kessler' )
             CALL kessler( th_phy,                       &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_old(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     moist_old(ims,kms,jms,P_QR),        &
                     rho, pi_phy, RAINNC,                &
                     RAINNCV, dt, z, xlv, cp,            &
                     EP_2,SVP1,SVP2,SVP3,SVPT0,rhowater, &
                     dz8w,                               &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )

        CASE (LINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling lin_et_al' )
             CALL lin_et_al( th_phy,                     &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     moist_new(ims,kms,jms,P_QI),        &
                     moist_new(ims,kms,jms,P_QS),        &
                     moist_new(ims,kms,jms,P_QG),        &
                     moist_old(ims,kms,jms,P_QR),        &
                     moist_old(ims,kms,jms,P_QS),        &
                     moist_old(ims,kms,jms,P_QG),        &
                     rho, pi_phy, p_phy, RAINNC,         &
                     RAINNCV,dt, z,                      &
                     ht, dz8w, G, cp, R_d, R_v,          &
                     XLS, XLV, XLF, rhowater, rhosnow,   &
                     EP_2,SVP1,SVP2,SVP3,SVPT0,          &
                     P_QI, P_QS, P_QG,                   &
                     PARAM_FIRST_SCALAR,                 &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          & 
                     its,ite, jts,jte, kts,kte           )

        CASE (WSM3SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm3' )
             CALL wsm3(th_phy,                           &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     w, rho, pi_phy, p_phy, dz8w, RAINNC,&
                     RAINNCV,dt,g,cp,cpv,r_d,r_v,SVPT0,  &
                     ep_1, ep_2, epsilon,                &
                     XLS, XLV, XLF, rhoair0, rhowater,   &
                     cliq,cice,psat,                     &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )

        CASE (WSM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm5' )
             CALL  wsm5(th_phy,                          &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     moist_new(ims,kms,jms,P_QI),        &
                     moist_new(ims,kms,jms,P_QS),        &
                     w, rho, pi_phy, p_phy, dz8w, RAINNC,&
                     RAINNCV,dt,g,cp,cpv,r_d,r_v,SVPT0,  &
                     ep_1, ep_2, epsilon,                &
                     XLS, XLV, XLF, rhoair0, rhowater,   &
                     cliq,cice,psat,                     &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )

        CASE (WSM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm6' )
             CALL  wsm6(th_phy,                          &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     moist_new(ims,kms,jms,P_QI),        &
                     moist_new(ims,kms,jms,P_QS),        &
                     moist_new(ims,kms,jms,P_QG),        &
                     w, rho, pi_phy, p_phy, dz8w, RAINNC,&
                     RAINNCV,dt,g,cp,cpv,r_d,r_v,SVPT0,  &
                     ep_1, ep_2, epsilon,                &
                     XLS, XLV, XLF, rhoair0, rhowater,   &
                     cliq,cice,psat,                     &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )

        CASE (ETAMPNEW)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

               CALL ETAMP_NEW(itimestep,DT,DX,DY,RAINNC,RAINNCV,     &
                              dz8w,rho,p_phy,pi_phy,th_phy,          &
                              moist_new(ims,kms,jms,P_QV),           &
                              moist_new(ims,kms,jms,P_QC),           &
                              LOWLYR,                                &
                              F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,      &
                              ids,ide, jds,jde, kds,kde,             &
                              ims,ime, jms,jme, kms,kme,             &
                              its,ite, jts,jte, kts,kte             )

        CASE (NCEPCLOUD3)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling ncloud3' )
             CALL ncloud3(th_phy,                        &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     w, rho, pi_phy, p_phy, dz8w, RAINNC,&
                     RAINNCV,dt,g,cp,cpv,r_d,r_v,SVPT0,  &
                     ep_1, ep_2, epsilon,                &
                     XLS, XLV, XLF, rhoair0, rhowater,   &
                     cliq,cice,psat,                     &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )

        CASE (NCEPCLOUD5)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling ncloud5' )
             CALL  ncloud5(th_phy,                       &
                     moist_new(ims,kms,jms,P_QV),        &
                     moist_new(ims,kms,jms,P_QC),        &
                     moist_new(ims,kms,jms,P_QR),        &
                     moist_new(ims,kms,jms,P_QI),        &
                     moist_new(ims,kms,jms,P_QS),        &
                     w, rho, pi_phy, p_phy, dz8w, RAINNC,&
                     RAINNCV,dt,g,cp,cpv,r_d,r_v,SVPT0,  &
                     ep_1, ep_2, epsilon,                &
                     XLS, XLV, XLF, rhoair0, rhowater,   &
                     cliq,cice,psat,                     &
                     ids,ide, jds,jde, kds,kde,          &
                     ims,ime, jms,jme, kms,kme,          &
                     its,ite, jts,jte, kts,kte           )


      CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', config_flags%mp_physics
         CALL wrf_error_fatal ( wrf_err_message )

      END SELECT micro_select 

   ELSE

      WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', config_flags%mp_physics
      CALL wrf_error_fatal ( wrf_err_message )

   ENDIF

   ENDDO

   CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )

   RETURN

   END SUBROUTINE microphysics_driver

END MODULE module_microphysics_driver

