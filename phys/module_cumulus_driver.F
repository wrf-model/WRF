!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(itimestep,dt,DX,n_moist,                 &
                     RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,             &
                     RQICUTEN,RQSCUTEN,RAINC,RAINCV,NCA,              &
                     RTHRATEN,RTHBLTEN,RQVBLTEN,                      &
                     RTHFTEN,RQVFTEN,                                 &
                     u_phy,v_phy,th_phy,t_phy,w,moist,                &
                     dz8w,p8w,p_phy,pi_phy,                           &
                     config_flags,W0AVG,rho_phy,STEPCU,               &
                     CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,warm_rain,       &
                     apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,     &
                     apr_capme,apr_capmi,                             &
                     HTOP,HBOT,KPBL,                                  &  
                     MASS_FLUX,XF_ENS,PR_ENS,ht,                      &
                     ensdim,maxiens,maxens,maxens2,maxens3,           &
                     ids,ide, jds,jde, kds,kde,                       &
                     ims,ime, jms,jme, kms,kme,                       &
                     i_start,i_end,j_start,j_end,kts,kte,num_tiles    )
!----------------------------------------------------------------------
   USE module_bc
   USE module_state_description
   USE module_model_constants

! *** add new modules of schemes here

   USE module_cu_kf
   USE module_cu_bmj
   USE module_cu_kfeta
   USE module_cu_gd

   !  This driver calls subroutines for the cumulus parameterizations.
   !
   !  1. Kain & Fritsch (1993)
   !  2. Betts-Miller-Janjic (Janjic, 1994)
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
!-----------------------------------------------------------------
!-- DT            time step (second)
!-- itimestep     number of time step (integer)   
!-- DX            horizontal space interval (m)
!-- n_moist       number of moisture species
!-- rr            dry air density (kg/m^3)
!
!-- RTHCUTEN      Theta tendency due to 
!                 cumulus scheme precipitation (K/s)
!-- RQVCUTEN      Qv tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQRCUTEN      Qr tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQCCUTEN      Qc tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQSCUTEN      Qs tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!-- RQICUTEN      Qi tendency due to 
!                 cumulus scheme precipitation (kg/kg/s)
!
!-- RAINC         accumulated total cumulus scheme precipitation (mm)
!-- RAINCV        cumulus scheme precipitation (mm)
!-- NCA           counter of the cloud relaxation 
!                 time in KF cumulus scheme (integer)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- t_phy         temperature (K)
!-- w             vertical velocity (m/s)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- dz8w          dz between full levels (m)
!-- p8w           pressure at full levels (Pa)  
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!                 points (dimensionless)
!-- RTHRATEN      radiative temp forcing for Grell-Devenyi scheme
!-- RTHBLTEN      PBL temp forcing for Grell-Devenyi scheme
!-- RQVBLTEN      PBL moisture forcing for Grell-Devenyi scheme
!-- RTHFTEN
!-- RQVFTEN
!-- MASS_FLUX
!-- XF_ENS
!-- PR_ENS
!-- warm_rain
!-- CU_ACT_FLAG
!-- config_flags  
!-- W0AVG         average vertical velocity, (for KF scheme) (m/s)
!-- rho_phy       density (kg/m^3)
!-- CLDEFI        precipitation efficiency (for BMJ scheme) (dimensionless)
!-- STEPCU        # of fundamental timesteps between convection calls
!-- XLAND         land-sea mask (1.0 for land; 2.0 for water)
!-- LOWLYR        index of lowest model layer above the ground
!-- XLV0          latent heat of vaporization constant 
!                 used in temperature dependent formula (J/kg)
!-- XLV1          latent heat of vaporization constant 
!                 used in temperature dependent formula (J/kg/K)
!-- XLS0          latent heat of sublimation constant 
!                 used in temperature dependent formula (J/kg)
!-- XLS1          latent heat of sublimation constant
!                 used in temperature dependent formula (J/kg/K)
!-- R_d           gas constant for dry air ( 287. J/kg/K)
!-- R_v           gas constant for water vapor (461 J/k/kg)
!-- Cp            specific heat at constant pressure (1004 J/k/kg)
!-- rvovrd        R_v divided by R_d (dimensionless)
!-- G             acceleration due to gravity (m/s^2)
!-- EP_1          constant for virtual temperature 
!                 (R_v/R_d - 1) (dimensionless)
!-- pi_phy        the exner function, (p/p0)**(R/Cp) (none unit)
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
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!-- HBOT          index of lowest model layer with convection
!-- HTOP          index of highest model layer with convection
!-- LBOT          index of lowest model layer with convection
!-- LTOP          index of highest model layer with convection
!-- KPBL          layer index of the PBL
!
!======================================================================

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      n_moist, itimestep, num_tiles
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                           ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                    dz8w, &
                                                            p8w, &
                                                          p_phy, &
                                                         pi_phy, &
                                                          u_phy, &
                                                          v_phy, &
                                                         th_phy, &
                                                          t_phy, &
                                                        rho_phy, &
                                                              w
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ),        &
         INTENT(IN ) ::                                   moist

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                              RTHFTEN, &
                                                        RQVFTEN
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN   )  ::                             RTHRATEN, &
                                                       RTHBLTEN, &
                                                       RQVBLTEN

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                             RTHCUTEN, &
                                                       RQVCUTEN, &
                                                       RQCCUTEN, &
                                                       RQRCUTEN, &
                                                       RQICUTEN, &
                                                       RQSCUTEN, &
                                                          W0AVG

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::   HT,XLAND

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC, &
                                                         RAINCV, &
                                                            NCA, & 
                                                      MASS_FLUX, & 
                                                         CLDEFI 
 
   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    INTENT(INOUT) ::                        HTOP,& 
               apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,      &
               apr_capme,apr_capmi,                              &
                                                           HBOT 
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL

   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   REAL,  INTENT(IN   ) :: DT, DX

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij

!-----------------------------------------------------------------

   IF (config_flags%cu_physics .eq. 0) return

! DON'T JUDGE TIME STEP HERE, SINCE KF NEEDS ACCUMULATED W FIELD.
! DO IT INSIDE THE INDIVIDUAL CUMULUS SCHEME

! SET START AND END POINTS FOR TILES
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte)

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)


   cps_select: SELECT CASE(config_flags%cu_physics)

     CASE (KFSCHEME)
          CALL wrf_debug(100,'in kfcps')
          CALL KFCPS(DT,itimestep,DX,                           &
               rho_phy,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
               RQICUTEN,RQSCUTEN,RAINCV,NCA,                    &
               u_phy,v_phy,th_phy,t_phy,w,                      &
               moist(ims,kms,jms,P_QV),                         &
               dz8w,p_phy,pi_phy,                               &
               W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R_d,G,EP_1,         &            
               EP_2,SVP1,SVP2,SVP3,SVPT0,                       &
               P_QR,P_QI,P_QS,PARAM_FIRST_SCALAR,STEPCU,        &
               CU_ACT_FLAG,warm_rain,                           & 
               ids,ide, jds,jde, kds,kde,                       &
               ims,ime, jms,jme, kms,kme,                       &
               its,ite, jts,jte, kts,kte                        )
     CASE (BMJSCHEME)
          CALL wrf_debug(100,'in bmj_cps')
            CALL BMJDRV(DT,itimestep,STEPCU,                      &
                 RTHCUTEN,RQVCUTEN,                               &
                 RAINCV,HTOP,HBOT,KPBL,                           &
                 th_phy,t_phy,                                    &
                 moist(ims,kms,jms,P_QV),                         &
                 p8w,p_phy,pi_phy,rho_phy,dz8w,                   &
                 CP,R_d,XLV,g,SVPT0,EP_1,                         &
                 CLDEFI,LOWLYR,XLAND,CU_ACT_FLAG,                 &
                 ids,ide, jds,jde, kds,kde,                       &
                 ims,ime, jms,jme, kms,kme,                       &
                 its,ite, jts,jte, kts,kte                        )
     CASE (KFETASCHEME)
          CALL wrf_debug(100,'in kf_eta_cps')
          CALL KF_ETA_CPS(DT,itimestep,DX,                      &
               rho_phy,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
               RQICUTEN,RQSCUTEN,RAINCV,NCA,                    &
               u_phy,v_phy,th_phy,t_phy,w,                      &
               moist(ims,kms,jms,P_QV),                         &
               dz8w,p_phy,pi_phy,                               &
               W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R_d,G,EP_1,         &
               EP_2,SVP1,SVP2,SVP3,SVPT0,                       &
               P_QR,P_QI,P_QS,PARAM_FIRST_SCALAR,STEPCU,        &
               CU_ACT_FLAG,warm_rain,                           &
               ids,ide, jds,jde, kds,kde,                       &
               ims,ime, jms,jme, kms,kme,                       &
               its,ite, jts,jte, kts,kte                        )
     CASE (GDSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GRELLDRV(DT,itimestep,DX,                        &
               rho_phy,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,     &
               RAINCV,RTHFTEN,RQVFTEN,                          &
               RTHRATEN,RTHBLTEN,RQVBLTEN,                      &
               u_phy,v_phy,t_phy,w,                             &
               moist(ims,kms,jms,P_QV),                         &
               dz8w,p_phy,pi_phy,                               &
               XLV0,CP,G,R_v,                                   &
               P_QC,P_QI,P_QS,PARAM_FIRST_SCALAR,STEPCU,        &
               CU_ACT_FLAG,warm_rain,                           &
               apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma,     &
               apr_capme,apr_capmi,                             &
               MASS_FLUX,XF_ENS,PR_ENS,ht,                      &
               ensdim,maxiens,maxens,maxens2,maxens3,           &
               ids,ide, jds,jde, kds,kde,                       &
               ims,ime, jms,jme, kms,kme,                       &
               its,ite, jts,jte, kts,kte                        )

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', config_flags%cu_physics
         CALL wrf_error_fatal ( wrf_err_message )

   END SELECT cps_select

      ENDDO

   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
