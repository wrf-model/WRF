!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_cumulus_driver
CONTAINS
   SUBROUTINE cumulus_driver(grid                                     &
                 ! Order dependent args for domain, mem, and tile dims
                     ,ids,ide, jds,jde, kds,kde                       &
                     ,ims,ime, jms,jme, kms,kme                       &
                     ,ips,ipe, jps,jpe, kps,kpe                       &
                     ,i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                 ! Order independent args (use VAR= in call)
                 ! --Prognostic
                     ,u,v,th,t,w                                      &
                     ,p,pi,rho                                        &
                 ! --Other arguments
                     ,itimestep,dt,dx,cudt,curr_secs,adapt_step_flag  &
                     ,cudtacttime                                     & 
                     ,rainc,raincv,pratec,nca                         &
                     ,cldfra_dp,cldfra_sh,w_up                        & !ckay for subgrid cloud
                     ,udr_kf,ddr_kf,uer_kf,der_kf,timec_kf,kf_edrates & !kf_edrates
                     ,QC_CU,QI_CU                                     &
                     ,z,z_at_w,dz8w,mavail,pblh,p8w,psfc,tsk          &
                     ,tke_pbl, ust                                    &
                     ,ZOL,WSTAR                                       &  !ckay
                     ,forcet,forceq,w0avg,stepcu,gsw                  &
                     ,cldefi,lowlyr,xland,cu_act_flag,warm_rain       &
                     ,hfx,qfx,cldfra,cldfra_mp_all,tpert2d            &
                     ,htop,hbot,kpbl,ht                               &
                     ,ensdim,maxiens,maxens,maxens2,maxens3           &
                     ,shall                                           & !CuP, wig 18-Sep-2006
#if (EM_CORE == 1)
                     !BSINGH - For WRFCuP Scheme
                     ,akpbl, br,regime,t2,q2                            & !CuP, wig 3-Aug-2006
                     ,slopeSfc, slopeEZ, sigmaSfc, sigmaEZ            & !CuP, wig 7-Aug-2006
                     ,cupflag, cldfra_cup, cldfratend_cup             & !CuP, wig 18-Sep-2006
                     ,taucloud, tactive                               & !CuP, wig 18-Sep-2006
                     ,activeFrac                                      & !CuP, lkb 4-May-2010
                     ,tstar, lnterms, lnint                           & !CuP, wig 4-Oct-2006
                     ,numBins, thBinSize, rBinSize                    & !CUP, lkb 4-Nov-2009
                     ,minDeepFreq, minShallowFreq                     & !CUP, lkb 4-Nov-2009
                     ,wCloudBase                                      & !CuP, lkb 29-April-2010                
                     ,wact_cup                                        & !CuP, rce 10-may-2012
                     ,wulcl_cup                                       & !CuP, rce 10-may-2012
                     ,wup_cup                                         & !CuP, rce 15-mar-2013 !BSINGH (12/06/2013)
                     ,qc_ic_cup                                       & !CuP, rce 10-may-2012
                     ,qndrop_ic_cup                                   & !CuP, rce 10-may-2012
                     ,qc_iu_cup                                       & !CuP, rce 10-may-2012
                     ,fcvt_qc_to_pr_cup                               & !CuP, rce 10-may-2012
                     ,fcvt_qc_to_qi_cup                               & !CuP, rce 10-may-2012
                     ,fcvt_qi_to_pr_cup                               & !CuP, rce 10-may-2012
                     ,mfup_cup                                        & !CuP, rce 10-may-2012
                     ,mfup_ent_cup                                    & !CuP, rce 10-may-2012
                     ,mfdn_cup                                        & !CuP, rce 10-may-2012
                     ,mfdn_ent_cup                                    & !CuP, rce 10-may-2012
                     ,updfra_cup                                      & !CuP, rce 10-may-2012
                     ,tcloud_cup                                      & !CuP, rce 10-may-2012
                     !BSINGH -ENDS
#endif
                     ,periodic_x,periodic_y                           &
                     ,is_CAMMGMP_used                                 & 
                     ,evapcdp3d,icwmrdp3d,rprddp3d                    & !Balwinder.Singh@pnnl.gov: Used for CAM's wet scavenging
                 ! Package selection variables
                     ,cu_physics, bl_pbl_physics, sf_sfclay_physics   &
#if (EM_CORE == 1)
                     !BSINGH - For WRFCuP Scheme
                     ,shcu_aerosols_opt                               & !CuP, rce 10-may-2012
                     ,chem_opt                                        & !CuP, rce 10-may-2012
                     !BSINGH - ENDS
#endif
                 ! Optional moisture tracers 
                     ,qv_curr, qc_curr, qr_curr                       &
                     ,qi_curr, qs_curr, qg_curr                       & 
                     ,qv_prev, qc_prev, qr_prev                       & 
                     ,qi_prev, qs_prev, qg_prev                       &
                 ! Optional arguments for GD scheme
                     ,apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma     &
                     ,apr_capme,apr_capmi,edt_out,clos_choice         &
                     ,mass_flux,xf_ens,pr_ens,cugd_avedx,imomentum    &
                     ,ishallow,cugd_tten,cugd_qvten,cugd_qcten        &
                     ,cugd_ttens,cugd_qvtens                          &
                     ,gd_cloud,gd_cloud2                              &
                 ! Optional output arguments for CAMZM scheme
                     ,cape, zmmu, zmmd, zmdt, zmdq, dlf, rliq         &
                     ,pconvb, pconvt                                  &
                     ,evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc    &
                     ,zmflxsnw, zmntprpd, zmntsnpd, zmeiheat          &
                     ,cmfmc, cmfmcdzm, preccdzm, precz                &
                     ,zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd    &
                     ,zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq  &
                     ,dp3d, du3d, ed3d, eu3d, md3d, mu3d, dsubcld2d   &
                     ,ideep2d, jt2d, maxg2d, lengath2d                &
                     ,k22_shallow,kbcon_shallow,ktop_shallow,xmb_shallow &
                     ,ktop_deep &
                 ! Optional arguments for SAS scheme
                     ,pgcon,sas_mass_flux                             &
                     ,pert_sas,ens_random_seed                        &
                     ,ens_sasamp                                      &
                     ,shalconv,shal_pgcon                             &
                     ,HPBL2D,EVAP2D,HEAT2D                            &     !Kwon for SAS2010 shallow convection
                 ! Optional arguments for NSAS scheme
                     ,mp_physics                                      &
                 ! Optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                      &
                     ,rqicuten,rqscuten,rqgcuten                      &
                     ,rqcncuten,rqincuten                             &
                     ,rqvblten,rqvften                                &
                     ,rucuten,rvcuten                                 &
                     ,rthcuten,rthraten,rthblten,rthften              &
                     ,mommix,store_rand                               &
! Optional variables for tiedtke scheme - add by ZCX&YQW 
                     ,znu                                             &
                 ! Optional moisture tracer flags 
                     ,f_qv,f_qc,f_qr                                  &
                     ,f_qi,f_qs,f_qg                                  &
                     ,CFU1,CFD1,DFU1,EFU1,DFD1,EFD1,f_flux            &
                 ! Optional trigger function activation variable
                     ,kfeta_trigger                                   &
                     ,nsas_dx_factor                                  &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option      --- 3.2 CLEANUP TODO -- THESE SHOULD BE OPTIONAL, NOT #IF/#ENDIF
                     ,do_capsuppress                                  &
#endif                                 
                                                                      )
!----------------------------------------------------------------------
   USE module_model_constants
   USE module_state_description, ONLY:     KFSCHEME,BMJSCHEME         &
                                          ,KFETASCHEME,GDSCHEME       &
                                          ,G3SCHEME,GFSCHEME          &
                                          ,P_QC,P_QI,Param_FIRST_SCALAR &
                                          ,CAMZMSCHEME, SASSCHEME     &
                                          ,OSASSCHEME,MESO_SAS        &  !Kwon
                                          ,NSASSCHEME                 &
#if (EM_CORE == 1)
                                          ,MSKFSCHEME                 &
                                          ,CAMMGMPSCHEME              &
                                          ,KFCUPSCHEME                & !CuP, wig 3-Aug-2006  !BSINGH - For WRFCuP scheme
                                          ,num_chem                   & !CuP, rce 10-may-2012 !BSINGH - For WRFCuP scheme
#endif
                                          ,TIEDTKESCHEME              &
                                          ,NTIEDTKESCHEME             


! *** add new modules of schemes here

   USE module_cu_kf     ,  ONLY : kfcps
   USE module_cu_bmj    ,  ONLY : bmjdrv
#ifdef DM_PARALLEL
   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks 
#if (EM_CORE == 1)
   USE module_comm_dm   ,  ONLY : halo_cup_g3_in_sub, halo_cup_g3_out_sub
#endif
#endif
   USE module_domain    , ONLY: domain
   USE module_cu_kfeta  , ONLY : kf_eta_cps
#if (EM_CORE==1)
   USE module_cu_mskf   , ONLY : mskf_cps
#endif
   USE module_cu_gd     , ONLY : grelldrv
   USE module_cu_gf     , ONLY : gfdrv
   USE module_cu_g3     , ONLY : g3drv,conv_grell_spread3d
   USE module_cu_sas    , ONLY : cu_sas
#if (EM_CORE == 1)
   USE module_cu_kfcup  , ONLY : KF_CUP_CPS !wig, 3-Aug-2006 !BSINGH - For WRFCuP scheme 
#endif
   USE module_cu_osas   , ONLY : cu_osas
   USE module_cu_mesosas, ONLY : cu_meso_sas
   USE module_cu_camzm_driver, ONLY : camzm_driver
   USE module_cu_tiedtke, ONLY : cu_tiedtke
   USE module_cu_ntiedtke,ONLY : cu_ntiedtke
   USE module_cu_nsas   , ONLY : cu_nsas
   USE module_wrf_error , ONLY : wrf_err_message

   !  This driver calls subroutines for the cumulus parameterizations.
   !
   !  1. Kain & Fritsch (1993)
   !  2. Betts-Miller-Janjic (Janjic, 1994)
   !  3. Grell-Devenyi (Grell and Devenyi, 2002)
   !  4. Simplified Arakawa-Schubert scheme (NCEP)
   !     (adapted by Zhang and Wang to work with ARW in V3.3)
   !  5. Grell 3D ensemble scheme
   !  6. Modified Tiedtke scheme (Zhang and Wang 2010)
   ! 14. New simplified Arakawa-Schubert scheme (NCEP, YSU)
   ! 16. New Tiedtke scheme (Bechtold et al. 2004, 2008, 2014)
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
!-- CUDT          cumulus time step (minute)
!-- curr_secs     current forecast time (seconds)
!-- itimestep     number of time step (integer)   
!-- DX            horizontal space interval (m)
!-- rr            dry air density (kg/m^3)
!
!-- RUCUTEN       Zonal wind tendency due to 
!                 cumulus scheme precipitation (m/s/s)
!-- RVCUTEN       Meridional wind tendency due to 
!                 cumulus scheme precipitation (m/s/s)
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
!-- RAINCV        time-step cumulus scheme precipitation (mm)
!-- PRATEC        precipitiation rate from cumulus scheme (mm/s)
!-- NCA           counter of the cloud relaxation 
!                 time in KF cumulus scheme (integer)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- t_phy         temperature (K)
!-- tsk           skin temperature (K)
!-- tke_pbl       turbulent kinetic energy from PBL scheme (m2/s2)
!-- ust           u* in similarity theory (m/s)
!-- w             vertical velocity (m/s)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- z             height above sea level at middle of layers (m)
!-- z_at_w        height above sea level at layer interfaces (m)
!-- dz8w          dz between full levels (m)
!-- pblh          planetary boundary layer height (m)
!-- mavail        soil moisture availability
!-- p8w           pressure at full levels (Pa)
!-- psfc          surface pressure (Pa)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!                 points (dimensionless)
!-- hfx           upward heat flux at surface (W/m2)
!-- qfx           upward moisture flux at surface (kg/m2/s)
!-- RTHRATEN      radiative temp forcing for Grell-Devenyi scheme
!-- RTHBLTEN      PBL temp forcing for Grell-Devenyi scheme
!-- RQVBLTEN      PBL moisture forcing for Grell-Devenyi scheme
!-- RTHFTEN
!-- RQVFTEN
!-- MASS_FLUX
!-- XF_ENS
!-- PR_ENS
!-- warm_rain
!-- cldfra        cloud fraction
!-- cldfra_mp_all cloud fraction
!-- CU_ACT_FLAG
!-- W0AVG         average vertical velocity, (for KF scheme) (m/s)
!-- kfeta_trigger namelist for KF trigger (=1, default; =2, moisture-advection-dependent trigger)
!-- nsas_dx_factor namelist for NSAS deep scheme to have some dependency on grid sizes
!-- rho           density (kg/m^3)
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
!-- evapcdp3d     Evaporation of deep convective precipitation (kg/kg/s)
!-- icwmrdp3d     Deep Convection in-cloud water mixing ratio (kg/m2)
!-- rprddp3d      dq/dt due to deep convective rainout (kg/kg/s)
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
!-- periodic_x    T/F this is using periodic lateral boundaries in the X direction
!-- periodic_y    T/F this is using periodic lateral boundaries in the Y-direction
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                                        kts,kte, &
                                      itimestep, num_tiles
   LOGICAL periodic_x, periodic_y
   TYPE(domain) , INTENT(INOUT)          :: grid
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::                             &
                  ensdim,maxiens,maxens,maxens2,maxens3

   INTEGER, OPTIONAL,     INTENT(IN   )    ::                    &
                   cugd_avedx,clos_choice,bl_pbl_physics,sf_sfclay_physics

#if (EM_CORE == 1)
   !BSINGH - For WRFCuP scheme
   INTEGER, OPTIONAL,     INTENT(IN   )    ::                    &
        shcu_aerosols_opt    !CuP, rce 10-may-2012
   !BSINGH - ENDS
#endif


   INTEGER,      INTENT(IN   )    ::                 cu_physics  
   INTEGER,      INTENT(IN   )    ::   STEPCU
   LOGICAL,      INTENT(IN   )    ::   warm_rain
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used !BSINGH:01/31/2013: Added for CAMZM

   REAL, INTENT(IN), OPTIONAL :: pgcon,shal_pgcon,sas_mass_flux
   logical,intent(in),optional  :: pert_sas
   integer,intent(in),optional  :: ens_random_seed
   real,intent(in),optional     :: ens_sasamp
   INTEGER, INTENT(IN), OPTIONAL :: shalconv

   INTEGER,DIMENSION( ims:ime, jms:jme ),                        &
           INTENT(IN ) ::                                LOWLYR

   !BSINGH - For WRFCuP scheme
   REAL,DIMENSION( ims:ime, jms:jme ),                           &
           INTENT(INOUT) ::                               shall    !CuP, wig 18-Sep-2006
#if (EM_CORE == 1)
   !BSINGH - For WRFCuP scheme
   REAL,DIMENSION( ims:ime, jms:jme ),                           &
           INTENT(INOUT) ::                            taucloud, & !CuP, wig 1-Oct-2006
                                                        tactive, & !CuP, wig 1-Oct-2006
                                                     activeFrac, & !CuP, lkb 5-May-2010
                                                     wCloudBase    !CuP, lkb 29-April-2010  
   !BSINGH - ENDS
#endif


   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                          &
                                                              z  &
                                                      ,    dz8w  &
                                                      ,     p8w  &
                                                      ,       p  &
                                                      ,      pi  &
                                                      ,       u  &
                                                      ,       v  &
                                                      ,      th  &
                                                      ,       t  &
                                                      ,     rho    ! ckay
!ckay
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INout ) ::                                    w

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT ), OPTIONAL ::  evapcdp3d, icwmrdp3d, rprddp3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ),OPTIONAL ::                         z_at_w  &
                                                      ,  cldfra  &
                                               ,  cldfra_mp_all  &
                                                      , tke_pbl  

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                                       &
                                                          W0AVG 
!ckay
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                            cldfra_dp  &
                                                    , cldfra_sh

!kf_edrates
   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT)  ::                            udr_kf     &
                                                    , ddr_kf     &
                                                    , uer_kf     &
                                                    , der_kf

   REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
          timec_kf

   INTEGER, INTENT(IN   )        ::   kf_edrates

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT ),OPTIONAL ::                        w_up

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
          GSW,HT,XLAND
#if (EM_CORE == 1)
   !BSINGH - For WRFCuP scheme
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                             br, & !CuP, wig 3-Aug-2006
                                                         regime, & !CuP, wig 3-Aug-2006
                                                             t2, & !CuP, wig 3-Aug-2006
                                                             q2    !CuP, wig 3-Aug-2006

   !BSINGH - ENDS
#endif


   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                RAINC  &
                                                    ,    RAINCV  &
                                                    ,       NCA  & 
                                                    ,      HTOP  & 
                                                    ,      HBOT  &
                                                    ,    CLDEFI
#if (EM_CORE == 1)
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: akpbl    !CuP, wig 6-Oct-2006 testing !BSINGH - For WRFCuP scheme
#endif
 
   REAL, DIMENSION( kms:kme ), OPTIONAL, INTENT(IN   ) ::        &
                                                            znu

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT),OPTIONAL :: &
        PRATEC,MAVAIL,PBLH,PSFC,TSK,TPERT2D,UST,HFX,QFX

!ckay
   REAL, DIMENSION( ims:ime , jms:jme ),                         &
         OPTIONAL, INTENT(INOUT) ::                         ZOL  &
                                                    ,     WSTAR

   REAL, DIMENSION( ims:ime , jms:jme ) :: tmppratec
                                                    
   INTEGER, DIMENSION( ims:ime , jms:jme ),                      &
                    INTENT(IN) ::                          KPBL

   LOGICAL, DIMENSION( ims:ime , jms:jme ),                      &
          INTENT(INOUT) :: CU_ACT_FLAG

   INTEGER, INTENT(IN   ), OPTIONAL        ::   kfeta_trigger
   INTEGER, INTENT(IN   ), OPTIONAL        ::   nsas_dx_factor

   REAL,  INTENT(IN   ) :: DT, DX
   INTEGER,      INTENT(IN   ),OPTIONAL    ::                             &
                   ips,ipe, jps,jpe, kps,kpe,imomentum,ishallow
   REAL,  INTENT(IN   ),OPTIONAL :: CUDT
   REAL,  INTENT(IN   ),OPTIONAL :: CURR_SECS
   LOGICAL,INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,  INTENT(INOUT ),OPTIONAL :: cudtacttime                 
   REAL   :: cudt_pass, curr_secs_pass,cudtacttime_pass          
   LOGICAL :: adapt_step_flag_pass

   INTEGER,      INTENT(IN   ), OPTIONAL    ::   mp_physics
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN) ::  STORE_RAND
!
   REAL, OPTIONAL,  INTENT(INOUT) :: mommix

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,               &    !Kwon for sas2010 shallow convection
                    INTENT(INOUT) ::  HPBL2D, EVAP2D, HEAT2D
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                   INTENT(INOUT) :: rucuten,rvcuten
!
! optional arguments
!
   INTEGER, DIMENSION( ims:ime, jms:jme ),                       &
         OPTIONAL, INTENT(INOUT) ::                              &
     k22_shallow,kbcon_shallow,ktop_shallow,ideep2d,jt2d,maxg2d, &
     lengath2d 
   INTEGER, DIMENSION( ims:ime, jms:jme ),                       &
         OPTIONAL, INTENT(  OUT) :: ktop_deep
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  & 
                     ,qv_prev, qc_prev, qr_prev                  & 
                     ,qi_prev, qs_prev, qg_prev                  &
                      ! optional moisture and other tendencies
                     ,rqvcuten,rqccuten,rqrcuten                 &
                     ,rqicuten,rqscuten,rqgcuten                 &
                     ,rqcncuten,rqincuten                        &
                     ,rqvblten,rqvften                           &
                     ,rthraten,rthblten                          &
                     ,cugd_tten,cugd_qvten,cugd_qcten            &
                     ,cugd_ttens,cugd_qvtens                     &
                     ,forcet, forceq                             &
                     ,rthften,rthcuten

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                    OPTIONAL,                                    &
                    INTENT(INOUT) ::                             &
                apr_gr,apr_w,apr_mc,apr_st,apr_as,apr_capma      &
               ,apr_capme,apr_capmi,edt_out,xmb_shallow          &
                                                    , MASS_FLUX  &
               ,cape, pconvb, pconvt, preccdzm, precz, rliq      &
               ,dsubcld2d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                  GD_CLOUD,GD_CLOUD2,                            &
                  zmmd, zmmu, zmdt, zmdq, dlf,                   &
                  evaptzm, fzsntzm, evsntzm, evapqzm, zmflxprc,  &
                  zmflxsnw, zmntprpd, zmntsnpd, zmeiheat,        &
                  cmfmc, cmfmcdzm,                               &
                  zmmtu, zmmtv, zmupgu, zmupgd, zmvpgu, zmvpgd,  &
                  zmicuu, zmicud, zmicvu, zmicvd, zmdice, zmdliq,&
                  dp3d, du3d, ed3d, eu3d, md3d, mu3d

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
                  INTENT(INOUT) ::                               &
                  QC_CU,QI_CU
                  
   REAL, DIMENSION( ims:ime , jms:jme , 1:ensdim ),              &
          OPTIONAL,                                              &
          INTENT(INOUT) ::                       XF_ENS, PR_ENS


#if (EM_CORE == 1)
!BSINGH - For WRFCuP Scheme
  REAL, DIMENSION( ims:ime , jms:jme ),                         &
         INTENT(OUT) ::                       slopeSfc, slopeEZ, & !CuP, wig 7-Aug-2006
                                              sigmaSfc, SigmaEZ, & !CuP, wig 7-Aug-2006
                                                          tstar, & !CuP, wig 4-Oct-2006
                                                          lnint    !CuP, wig 4-Oct-2006

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                &
         INTENT(OUT) ::                                          &
                                                        lnterms, & !CuP, wig 4-Oct-2006
                                                     cldfra_cup, & !CuP, wig 18-Sep-2006
                                                 cldfratend_cup    !CuP, wig 18-Sep-2006

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                             wact_cup, & !CuP, rce 10-may-2012
                                                      wulcl_cup, & !CuP, rce 10-may-2012
                                                     tcloud_cup    !CuP, rce 10-may-2012

   REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),                &
         INTENT(INOUT) ::                                        &
                                                        wup_cup, & !CuP, rce 15-mar-2013 !BSINGH (12/06/2013)
                                                      qc_ic_cup, & !CuP, rce 10-may-2012
                                                  qndrop_ic_cup, & !CuP, rce 10-may-2012
                                                      qc_iu_cup, & !CuP, rce 10-may-2012
                                              fcvt_qc_to_pr_cup, & !CuP, rce 10-may-2012
                                              fcvt_qc_to_qi_cup, & !CuP, rce 10-may-2012
                                              fcvt_qi_to_pr_cup, & !CuP, rce 10-may-2012
                                                       mfup_cup, & !CuP, rce 10-may-2012
                                                   mfup_ent_cup, & !CuP, rce 10-may-2012
                                                       mfdn_cup, & !CuP, rce 10-may-2012
                                                   mfdn_ent_cup, & !CuP, rce 10-may-2012
                                                     updfra_cup    !CuP, rce 10-may-2012

   INTEGER, OPTIONAL, INTENT(IN) ::                    chem_opt    !CuP, rce 10-may-2012

   LOGICAL, DIMENSION( ims:ime, jms:jme ),                       &
            INTENT(OUT) ::                              cupflag    !CuP, wig 9-Oct-2006

   REAL,  INTENT(IN   ) :: thBinSize, rBinSize
   INTEGER, INTENT(IN ) :: numBins 
   REAL,  INTENT(IN   ) :: minDeepFreq, minShallowFreq 
!BSINGH - ENDS
#endif


   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   CFU1,        &
                                                   CFD1,        &
                                                   DFU1,        &
                                                   EFU1,        &
                                                   DFD1,        &
                                                   EFD1

!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, INTENT(IN), OPTIONAL ::                             &
                                                      f_qv      &
                                                     ,f_qc      &
                                                     ,f_qr      &
                                                     ,f_qi      &
                                                     ,f_qs      &
                                                     ,f_qg
   LOGICAL, INTENT(IN), OPTIONAL ::                   f_flux

#if ( WRF_DFI_RADAR == 1 )
!
!  option of cap suppress: 
!        do_capsuppress = 1   do
!        do_capsuppress = other   don't
!
!
   INTEGER,      INTENT(IN   ) ,OPTIONAL   :: do_capsuppress
   REAL, DIMENSION( ims:ime, jms:jme ) :: cap_suppress_loc
#endif

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij,trigger_kf,dx_factor_nsas
   logical :: l_flux
   LOGICAL :: decided , run_param , doing_adapt_dt


!-----------------------------------------------------------------

   l_flux=.FALSE.
   if (present(f_flux)) l_flux=f_flux
   if (.not. PRESENT(CURR_SECS)) then
      curr_secs_pass = -1
   else 
      curr_secs_pass = curr_secs
   endif

   if (.not. PRESENT(CUDT)) then
      cudt_pass = -1
      cudtacttime_pass = -1
   else
      cudt_pass = cudt
      cudtacttime_pass = cudtacttime
   endif

   if (.not. PRESENT(adapt_step_flag)) then
      adapt_step_flag_pass = .false.
   else
      adapt_step_flag_pass = adapt_step_flag
   endif

   ! Initialize tmppratec to pratec

   if ( PRESENT ( pratec ) ) then
      tmppratec(:,:) = pratec(:,:)
   else
      tmppratec(:,:) = 0.
   end if

   if (.not. PRESENT(kfeta_trigger)) then
      trigger_kf = 1
   else
      trigger_kf = kfeta_trigger
   endif

   if (.not. PRESENT(nsas_dx_factor)) then
      dx_factor_nsas = 0
   else
      dx_factor_nsas = nsas_dx_factor
   endif

   IF (cu_physics .eq. 0) return

!  Initialization for adaptive time step.

   IF ( adapt_step_flag_pass ) THEN
      doing_adapt_dt = .TRUE.
      IF ( cudtacttime_pass .EQ. 0. ) THEN
         cudtacttime_pass = curr_secs_pass + cudt_pass*60.
      END IF
   ELSE
      doing_adapt_dt = .FALSE.
   END IF

!  Do we run through this scheme or not?

!    Test 1:  If this is the initial model time, then yes.
!                ITIMESTEP=1
!    Test 2:  If the user asked for the cumulus to be run every time step, then yes.
!                CUDT=0 or STEPCU=1
!    Test 3:  If not adaptive dt, and this is on the requested cumulus frequency, then yes.
!                MOD(ITIMESTEP,STEPCU)=0
!    Test 4:  If using adaptive dt and the current time is past the last requested activate cumulus time, then yes.
!                CURR_SECS >= CUDTACTTIME

!  If we do run through the scheme, we set the flag run_param to TRUE and we set the decided flag
!  to TRUE.  The decided flag says that one of these tests was able to say "yes", run the scheme.
!  We only proceed to other tests if the previous tests all have left decided as FALSE.

!  If we set run_param to TRUE and this is adaptive time stepping, we set the time to the next
!  cumulus run.

   decided = .FALSE.
   run_param = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( cudt_pass .EQ. 0. ) .OR. ( stepcu .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepcu) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs_pass .GE. cudtacttime_pass ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      cudtacttime_pass = curr_secs_pass + cudt_pass*60
   END IF

   IF ( run_param ) THEN
!print *,'calling CU scheme'
   ELSE
!print *,'NOT calling CU scheme'
      RETURN
   END IF

#if  ( EM_CORE == 1 )
      if(cu_physics .eq. 5 .or. cu_physics .eq. 16) then
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij,i,j,k,its,ite,jts,jte )

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)
        do j=jts,min(jte,jde-1)
        do k=kts,kte
        do i=its,min(ite,ide-1)
           RTHFTEN(i,k,j)=(RTHFTEN(i,k,j)+RTHRATEN(i,k,j) &
                               +RTHBLTEN(i,k,j))*pi(i,k,j)
           RQVFTEN(i,k,j)=RQVFTEN(i,k,j)+RQVBLTEN(i,k,j)
       enddo
       enddo
       enddo
      ENDDO
      !$OMP END PARALLEL DO
      endif

      IF ( cu_physics == G3SCHEME .OR.  cu_physics == GFSCHEME .OR.     &
           cu_physics == KFETASCHEME .OR. cu_physics == MSKFSCHEME ) THEN
#ifdef DM_PARALLEL
#include "HALO_CUP_G3_IN.inc"
#endif
      ENDIF
#endif


! DON'T JUDGE TIME STEP HERE, SINCE KF NEEDS ACCUMULATED W FIELD.
! DO IT INSIDE THE INDIVIDUAL CUMULUS SCHEME

! SET START AND END POINTS FOR TILES
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)

      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)
        
#if (EM_CORE == 1)
        !BSINGH - For WRFCuP scheme
        !wig, beg: testing for kpbl to get output to wrfout since kpbl won't output as an integer
        do j=jts,jte
           do i=its,ite
              akpbl(i,j) = kpbl(i,j)
           end do
        end do
        !wig, end.
        !BSINGH - ENDS
#endif


   cps_select: SELECT CASE(cu_physics)

     CASE (KFSCHEME)
          CALL wrf_debug(100,'in kfcps')

          CALL KFCPS(                                           &
              ! order independent arguments
                DT=dt ,KTAU=itimestep ,DX=dx , CUDT=cudt_pass   &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RHO=rho                                         &
               ,U=u ,V=v ,TH=th ,T=t ,W=w                       &
               ,PCPS=p ,PI=pi                                   &
               ,XLV0=xlv0 ,XLV1=xlv1 ,XLS0=xls0 ,XLS1=xls1      &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,W0AVG=w0avg                                     &
               ,CP=cp ,R=r_d ,G=g ,EP1=ep_1 ,EP2=ep_2           &
               ,SVP1=svp1 ,SVP2=svp2 ,SVP3=svp3 ,SVPT0=svpt0    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,WARM_RAIN=warm_rain                             &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
               ,RQCCUTEN=rqccuten ,RQRCUTEN=rqrcuten            &
               ,RQICUTEN=rqicuten ,RQSCUTEN=rqscuten            &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,UDR_KF=udr_kf,DDR_KF=ddr_kf                     & !kf_edrates
               ,UER_KF=uer_kf,DER_KF=der_kf                     & 
               ,TIMEC_KF=timec_kf,KF_EDRATES=kf_edrates         & 
                                                                )

     CASE (BMJSCHEME)
          CALL wrf_debug(100,'in bmj_cps')
          CALL BMJDRV(                                          &
                TH=th,T=T ,RAINCV=raincv, PRATEC=tmppratec      &
               ,RHO=rho                                         &
               ,DT=dt ,ITIMESTEP=itimestep ,STEPCU=stepcu       &
               ,CUTOP=htop, CUBOT=hbot, KPBL=kpbl               &
               ,DZ8W=dz8w ,PINT=p8w, PMID=p, PI=pi              &
               ,CP=cp ,R=r_d ,ELWV=xlv ,ELIV=xls ,G=g           &
               ,TFRZ=svpt0 ,D608=ep_1 ,CLDEFI=cldefi            &
               ,LOWLYR=lowlyr ,XLAND=xland                      &
               ,CU_ACT_FLAG=cu_act_flag                         &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
               ,RTHCUTEN=rthcuten ,RQVCUTEN=rqvcuten            &
                                                                )

     CASE (KFETASCHEME)
          CALL wrf_debug(100,'in kf_eta_cps')
          CALL KF_ETA_CPS(                                      &
                U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho              &
               ,CUDT=cudt_pass                                  &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,WARM_RAIN=warm_rain    &
               ,QV=qv_curr                                      &
               ,SHALL=shall                                     & !BSINGH - For WRFCuP scheme added "shall"
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,trigger=trigger_kf                              &
              ! optionals
               ,RTHCUTEN=rthcuten                               &
               ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten            &
               ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten            &
               ,RQSCUTEN=rqscuten, RQVFTEN=RQVFTEN              &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CLDFRA_DP_KF=cldfra_dp                          & ! ckay for sub-grid cloud
               ,CLDFRA_SH_KF=cldfra_sh                          &
               ,QC_KF=QC_CU,QI_KF=QI_CU                         &
               ,UDR_KF=udr_kf,DDR_KF=ddr_kf                     & !kf_edrates
               ,UER_KF=uer_kf,DER_KF=der_kf                     & 
               ,TIMEC_KF=timec_kf,KF_EDRATES=kf_edrates         ) 

#if (EM_CORE==1)
     CASE (MSKFSCHEME)
          CALL wrf_debug(100,'in mskf_cps')
          CALL MSKF_CPS(                                        &
                U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho              &
               ,CUDT=cudt_pass                                  &
               ,ADAPT_STEP_FLAG=adapt_step_flag_pass            &
               ,RAINCV=raincv, PRATEC=tmppratec, NCA=nca        &
               ,DZ8W=dz8w                                       &
               ,PCPS=p, PI=pi ,W0AVG=W0AVG                      &
               ,CUTOP=HTOP,CUBOT=HBOT                           &
               ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1      &
               ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2           &
               ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0    &
               ,DT=dt ,KTAU=itimestep ,DX=dx                    &
               ,STEPCU=stepcu                                   &
               ,CU_ACT_FLAG=cu_act_flag ,WARM_RAIN=warm_rain    &
               ,QV=qv_curr                                      &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,trigger=trigger_kf                              &
              ! optionals
               ,RTHCUTEN=rthcuten                               &
               ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten            &
               ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten            &
               ,RQSCUTEN=rqscuten, RQVFTEN=RQVFTEN              &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CLDFRA_DP_KF=cldfra_dp                          & ! ckay for sub-grid cloud
               ,CLDFRA_SH_KF=cldfra_sh                          &
               ,W_UP=w_up                                       & ! ckay
               ,QC_KF=QC_CU,QI_KF=QI_CU                         &
               ,UDR_KF=udr_kf,DDR_KF=ddr_kf                     & !kf_edrates
               ,UER_KF=uer_kf,DER_KF=der_kf                     & 
               ,TIMEC_KF=timec_kf,KF_EDRATES=kf_edrates         & 
               ,ZOL=zol,WSTAR=wstar,UST=ust,PBLH=pblh           )  !ckay
#endif

     CASE (GDSCHEME)
          CALL wrf_debug(100,'in grelldrv')
          CALL GRELLDRV(                                        &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi ,Q=qv_curr ,RAINCV=raincv             &
               ,DZ8W=dz8w,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v     &
               ,PRATEC=tmppratec                                &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as                     &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw                             &
               ,GDC=gd_cloud,GDC2=gd_cloud2 &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3                 &
               ,htop=htop,hbot=hbot                             &
               ,ktop_deep=ktop_deep                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              ! optionals
#if (NMM_CORE == 1 )
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
#else
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
#endif
               ,RTHRATEN=RTHRATEN,RTHBLTEN=RTHBLTEN             &
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,RQVBLTEN=RQVBLTEN                               &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
               ,CFU1=CFU1,CFD1=CFD1,DFU1=DFU1,EFU1=EFU1         &
               ,DFD1=DFD1,EFD1=EFD1,f_flux=l_flux               )
          CALL wrf_debug(200,'back from grelldrv')

     CASE (SASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal ( wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_sas')
          CALL CU_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,MOMMIX=MOMMIX  &
               ,pgcon=pgcon,sas_mass_flux=sas_mass_flux         &
               ,pert_sas=pert_sas,ens_random_seed=ens_random_seed   &
               ,ens_sasamp=ens_sasamp &
               ,shalconv=shalconv,shal_pgcon=shal_pgcon         &
               ,hpbl2d=hpbl2d,evap2d=evap2d,heat2d=heat2d       &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (MESO_SAS)                  !Kwon

          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The meso SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal ( wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_mesosas')
          CALL CU_MESO_SAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,MOMMIX=MOMMIX  &
               ,pgcon=pgcon,sas_mass_flux=sas_mass_flux         &
               ,shalconv=shalconv,shal_pgcon=shal_pgcon         &
               ,hpbl2d=hpbl2d,evap2d=evap2d,heat2d=heat2d       &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (OSASSCHEME)
                                                                                                                                           
          IF ( adapt_step_flag_pass ) THEN
            WRITE( wrf_err_message , * ) 'The SAS cumulus option will not work properly with an adaptive time step'
            CALL wrf_error_fatal ( wrf_err_message )
          END IF
          CALL wrf_debug(100,'in cu_osas')
          CALL CU_OSAS(                                          &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN=RUCUTEN, RVCUTEN=RVCUTEN                &
               ,RAINCV=RAINCV,PRATEC=tmpPRATEC,HTOP=HTOP,HBOT=HBOT &
               ,U3D=u,V3D=v,W=w,T3D=t                           &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,PI3D=pi,RHO3D=rho                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,P_QC=p_qc                                       &
               ,store_rand=store_rand  &
               ,MOMMIX=MOMMIX  &
               ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
     CASE (G3SCHEME)
          CALL wrf_debug(100,'in grelldrv')
#if ( WRF_DFI_RADAR == 1 )
          if (do_capsuppress == 1) then
            WRITE( wrf_err_message , * ) 'G3 do CAP suppress',its,jts,min( jte,jde-1 ),min( ite,ide-1 ),kte
            CALL wrf_debug(200, wrf_err_message)
            DO j = jts, min( jte,jde-1 )
            DO i = its, min( ite,ide-1 )
               cap_suppress_loc(i,j) = grid%dfi_tten_rad(i,kte,j)
            ENDDO
            ENDDO
         endif
#endif
          CALL G3DRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,XF_ENS=xf_ens,PR_ENS=pr_ens,HT=ht               &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2,kpbl=kpbl           &
               ,k22_shallow=k22_shallow                         &
               ,kbcon_shallow=kbcon_shallow                     &
               ,ktop_shallow=ktop_shallow                       &
               ,xmb_shallow=xmb_shallow                         &
               ,ktop_deep=ktop_deep                             &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum,ishallow_g3=ishallow        &
               ,ENSDIM=ensdim,MAXIENS=maxiens,MAXENS=maxens     &
               ,MAXENS2=maxens2,MAXENS3=maxens3,ichoice=clos_choice &
               ,htop=htop,hbot=hbot                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              ! optionals
#if (NMM_CORE == 1 )
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
#else
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,rqvblten=rqvblten,rthblten=rthblten             &
#endif
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option
              ,do_capsuppress=do_capsuppress                    &
              ,cap_suppress_loc=cap_suppress_loc                &
#endif                                 
                                                                )
     CASE (GFSCHEME)
          CALL wrf_debug(100,'in grelldrv')
#if ( WRF_DFI_RADAR == 1 )
          if (do_capsuppress == 1) then
            WRITE( wrf_err_message , * ) 'G3 do CAP suppress',its,jts,min( jte,jde-1 ),min( ite,ide-1 ),kte
            CALL wrf_debug(200, wrf_err_message)
            DO j = jts, min( jte,jde-1 )
            DO i = its, min( ite,ide-1 )
               cap_suppress_loc(i,j) = grid%dfi_tten_rad(i,kte,j)
            ENDDO
            ENDDO
         endif
#endif
          CALL GFDRV(                                           &
                DT=dt, ITIMESTEP=itimestep, DX=dx               &
               ,U=u,V=v,T=t,W=w ,RHO=rho                        &
               ,P=p,PI=pi,Q=qv_curr,RAINCV=raincv               &
               ,DZ8W=dz8w ,P8W=p8w,XLV=xlv,CP=cp,G=g,R_V=r_v    &
               ,APR_GR=apr_gr,APR_W=apr_w,APR_MC=apr_mc         &
               ,APR_ST=apr_st,APR_AS=apr_as,PRATEC=tmppratec    &
               ,APR_CAPMA=apr_capma,APR_CAPME=apr_capme         &
               ,APR_CAPMI=apr_capmi,MASS_FLUX=mass_flux         &
               ,HT=ht,qfx=qfx,hfx=hfx                           &
               ,xland=xland,gsw=gsw,edt_out=edt_out             &
               ,GDC=gd_cloud,GDC2=gd_cloud2,kpbl=kpbl           &
               ,k22_shallow=k22_shallow                         &
               ,kbcon_shallow=kbcon_shallow                     &
               ,ktop_shallow=ktop_shallow                       &
               ,ktop_deep=ktop_deep                             &
               ,xmb_shallow=xmb_shallow                         &
               ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten       &
               ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens   &
               ,cugd_qcten=cugd_qcten,cugd_avedx=cugd_avedx     &
               ,imomentum=imomentum,ishallow_g3=ishallow        &
               ,ichoice=clos_choice                             &
               ,htop=htop,hbot=hbot                             &
               ,CU_ACT_FLAG=CU_ACT_FLAG,warm_rain=warm_rain     &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
!              ,IPS=ips,IPE=ipe,JPS=jps,JPE=jpe,KPS=kps,KPE=kpe &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
               ,PERIODIC_X=periodic_x,PERIODIC_Y=periodic_y     &
              ! optionals
#if (NMM_CORE == 1 )
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=forcet               &
               ,RTHRATEN=RTHRATEN                               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=forceq               &
#else
               ,RTHCUTEN=RTHCUTEN ,RTHFTEN=RTHFTEN              &
               ,RTHRATEN=RTHRATEN                               &
               ,RQICUTEN=RQICUTEN ,RQVFTEN=RQVFTEN              &
               ,rqvblten=rqvblten,rthblten=rthblten             &
#endif
               ,RQVCUTEN=RQVCUTEN,RQCCUTEN=RQCCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
#if ( WRF_DFI_RADAR == 1 )
                 ! Optional CAP suppress option
              ,do_capsuppress=do_capsuppress                    &
              ,cap_suppress_loc=cap_suppress_loc                &
#endif                                 
                                                                )
     CASE (CAMZMSCHEME)
          IF (PRESENT(z_at_w) .AND. PRESENT(mavail)                 &
                 .AND. PRESENT(pblh) .AND. PRESENT(psfc).AND.PRESENT(RQCNCUTEN))THEN
          CALL wrf_debug(100,'in camzm_cps')
      IF(.not.f_qi)THEN
         WRITE( wrf_err_message , * ) 'This cumulus option requires ice microphysics option: f_qi = ', f_qi
         CALL wrf_error_fatal ( wrf_err_message )
      ENDIF
          CALL CAMZM_DRIVER(                                        &
                IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde     &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme     &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte     &
               ,ITIMESTEP=itimestep, BL_PBL_PHYSICS=bl_pbl_physics  &
               ,SF_SFCLAY_PHYSICS=sf_sfclay_physics                 &
               ,TH=th, T_PHY=t, TSK=tsk, TKE_PBL=tke_pbl            &
               ,UST=ust, QV=qv_curr, QC=qc_curr, QI=qi_curr         &
               ,MAVAIL=mavail,KPBL=kpbl, PBLH=pblh, XLAND=xland     & 
               ,Z=z, Z_AT_W=z_at_w                                  &
               ,DZ8W=dz8w, HT=ht                                    &
               ,P=p, P8W=p8w, PI_PHY=pi, PSFC=psfc                  &
               ,U_PHY=u, V_PHY=v, HFX=hfx, QFX=qfx, CLDFRA=cldfra   &
               ,CLDFRA_MP_ALL=cldfra_mp_all                         &
               ,IS_CAMMGMP_USED=is_CAMMGMP_used                     &
               ,TPERT_CAMUWPBL=tpert2d                              &
               ,DX=dx, DT=dt, STEPCU=stepcu, CUDT=cudt              &
               ,CURR_SECS=curr_secs                                 &
               ,ADAPT_STEP_FLAG=adapt_step_flag                     &
               ,CUDTACTTIME=cudtacttime_pass                        & 
               ,CAPE_OUT=cape                                       &
               ,MU_OUT=zmmu, MD_OUT=zmmd                            &
               ,ZMDT=zmdt, ZMDQ=zmdq, DLF_OUT=dlf, RLIQ_OUT=rliq    &
               ,PCONVT=pconvt, PCONVB=pconvb, CUBOT=hbot, CUTOP=htop&
               ,RAINCV=raincv, PRATEC=tmppratec                     &
               ,RUCUTEN=rucuten, RVCUTEN=rvcuten                    &
               ,RTHCUTEN=rthcuten, RQVCUTEN=rqvcuten                &
               ,RQCCUTEN=rqccuten, RQICUTEN=rqicuten                &
               ,RQCNCUTEN=rqcncuten, RQINCUTEN=rqincuten            &
               ,EVAPTZM=evaptzm, FZSNTZM=fzsntzm, EVSNTZM=evsntzm   &
               ,EVAPQZM=evapqzm, ZMFLXPRC=zmflxprc                  &
               ,ZMFLXSNW=zmflxsnw, ZMNTPRPD=zmntprpd                &
               ,ZMNTSNPD=zmntsnpd, ZMEIHEAT=zmeiheat                &
               ,CMFMC=cmfmc, CMFMCDZM=cmfmcdzm                      &
               ,PRECCDZM=preccdzm, PRECZ=precz                      &
               ,ZMMTU=zmmtu, ZMMTV=zmmtv, ZMUPGU=zmupgu             &
               ,ZMUPGD=zmupgd, ZMVPGU=zmvpgu, ZMVPGD=zmvpgd         &
               ,ZMICUU=zmicuu, ZMICUD=zmicud, ZMICVU=zmicvu         &
               ,ZMICVD=zmicvd, ZMDICE=zmdice, ZMDLIQ=zmdliq         &
               ,EVAPCDP3D=evapcdp3d, ICWMRDP3D=icwmrdp3d            &
               ,RPRDDP3D=rprddp3d,DP3D=dp3d, DU3D=du3d, ED3D=ed3d   &
               ,EU3D=eu3d, MD3D=md3d, MU3D=mu3d,DSUBCLD2D=dsubcld2d &
               ,IDEEP2D=ideep2d, JT2D=jt2d, MAXG2D=maxg2d           &
               ,LENGATH2D=lengath2d                                 )
        ELSE
             WRITE( wrf_err_message , * ) 'Insufficient arguments to call CAMZM cu scheme'
             CALL wrf_error_fatal ( wrf_err_message )
          ENDIF

! TIEDTKE SCHEME - ZCX&YQW (U of Hawaii)
     CASE (TIEDTKESCHEME)

        IF ( PRESENT ( QFX ) .AND. PRESENT( ZNU ) ) THEN

          CALL wrf_debug(100,'in cu_tiedtke')
          CALL CU_TIEDTKE(                                      &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU         &
               ,RAINCV=RAINCV,PRATEC=tmppratec,QFX=qfx,ZNU=znu  &
               ,U3D=u,V3D=v,W=w,T3D=t,PI3D=pi,RHO3D=rho         &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,QVPBLTEN=RQVBLTEN                               &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND            &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
#if (NMM_CORE == 1 )
               ,QVFTEN=FORCEQ                                   &
#else
               ,QVFTEN=RQVFTEN                                  &
#endif
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN = RUCUTEN,RVCUTEN = RVCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                             )
        ELSE
          CALL wrf_error_fatal('Lacking arguments for CU_TIEDTKE in cumulus driver')
        ENDIF

! NEW TIEDTKE SCHEME - ZCX&YQW (U of Hawaii)
     CASE (NTIEDTKESCHEME)

        IF ( PRESENT ( QFX ) .AND. PRESENT( HFX )) THEN

          CALL wrf_debug(100,'in cu_ntiedtke')
          CALL CU_NTIEDTKE(                                      &
                DT=dt,ITIMESTEP=itimestep,STEPCU=STEPCU,HFX=hfx  &
               ,RAINCV=RAINCV,PRATEC=tmppratec,QFX=qfx          &
               ,U3D=u,V3D=v,W=w,T3D=t,PI3D=pi,RHO3D=rho         &
               ,QV3D=QV_CURR,QC3D=QC_CURR,QI3D=QI_CURR          &
               ,DZ8W=dz8w,PCPS=p,P8W=p8w,XLAND=XLAND,DX=dx      &
               ,CU_ACT_FLAG=CU_ACT_FLAG                         &
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
              ! optionals
#if (NMM_CORE == 1 )
               ,QVFTEN=FORCEQ, THFTEN=FORCET                    &
#else
               ,QVFTEN=RQVFTEN,THFTEN=RTHFTEN                   &
#endif
               ,RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN             &
               ,RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN             &
               ,RUCUTEN = RUCUTEN,RVCUTEN = RVCUTEN             &
               ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                   &
               ,F_QI=f_qi,F_QS=f_qs                             &
                                                             )
        ELSE
          CALL wrf_error_fatal('Lacking arguments for CU_NTIEDTKE in cumulus driver')
        ENDIF

! New GFS SAS SCHEME - (Yonsei Univ., South Korea)
     CASE (NSASSCHEME)
        IF ( PRESENT ( QFX ) .AND. PRESENT( HFX ) ) THEN
          CALL wrf_debug(100,'in nsas_cps')
          CALL CU_NSAS(                                         &
                DT=dt,DX=dx,P3DI=p8w,P3D=p,PI3D=pi,             &
                QC3D=QC_CURR,QI3D=QI_CURR,RHO3D=rho,            &
                ITIMESTEP=itimestep,STEPCU=STEPCU,              &
                HBOT=HBOT,HTOP=HTOP,                            &
                CU_ACT_FLAG=CU_ACT_FLAG,                        &
                RTHCUTEN=RTHCUTEN,RQVCUTEN=RQVCUTEN,            &
                RQCCUTEN=RQCCUTEN,RQICUTEN=RQICUTEN,            &
                RUCUTEN=RUCUTEN,RVCUTEN=RVCUTEN,                &
                QV3D=QV_CURR,T3D=t,                             &
                RAINCV=RAINCV,PRATEC=tmpPRATEC,                 &
                XLAND=XLAND,DZ8W=dz8w,W=w,U3D=u,V3D=v,          &
                HPBL=pblh,HFX=hfx,QFX=qfx,                      & 
                MP_PHYSICS=mp_physics,                          & 
                DX_FACTOR_NSAS=dx_factor_nsas,                  & 
                pgcon=pgcon,                                    &
                P_QC=p_qc,P_QI=p_qi,                            &
                P_FIRST_SCALAR=param_first_scalar               &
               ,CP=cp,CLIQ=cliq,CPV=cpv,G=g,XLV=xlv,R_D=r_d     &
               ,R_V=r_v,EP_1=ep_1,EP_2=EP_2                     &
               ,CICE=cice,XLS=xls,PSAT=psat                     &
               ,F_QI=f_qi,F_QC=f_qc                             & 
               ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde &
               ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme &
               ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte &
                                                                )
        ELSE
          CALL wrf_error_fatal('Lacking arguments for CU_NSAS in cumulus driver')
        ENDIF
#if (EM_CORE == 1)
        !BSINGH -  For WRFCuP scheme
     CASE (KFCUPSCHEME)
        
        CALL wrf_debug(100,'in cu_kfcup')        
        CALL KF_CUP_CPS( GRID_ID=grid%grid_id                        & !rce 10-may-2012 add grid%id
             ,U=u ,V=v ,TH=th ,T=t ,W=w ,RHO=rho                &
             ,RAINCV=raincv,NCA=nca ,DZ8W=dz8w,XLAND=xland      & !LD add PRATEC 21-Apr-2011
             ,PCPS=p, PI=pi ,W0AVG=W0AVG                        &
             ,CUTOP=HTOP,CUBOT=HBOT                             &
             ,XLV0=XLV0 ,XLV1=XLV1 ,XLS0=XLS0 ,XLS1=XLS1        &
             ,CP=CP ,R=R_d ,G=G ,EP1=EP_1 ,EP2=EP_2             &
             ,SVP1=SVP1 ,SVP2=SVP2 ,SVP3=SVP3 ,SVPT0=SVPT0      &
             ,DT=dt ,KTAU=itimestep ,DX=dx                      &
             ,STEPCU=stepcu                                     &
             ,CU_ACT_FLAG=cu_act_flag ,warm_rain=warm_rain      &
             ,QV=qv_curr                                        &
             ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde   &
             ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme   &
             ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte   &
             ,PSFC=psfc,Z=z,Z_AT_W=z_at_w,HT=ht,TSK=tsk         & !CuP, wig 5-Oct-2006
             ,HFX=hfx,QFX=qfx                                   & !CuP, wig, 24-Aug-2006
             ,MAVAIL=mavail,SF_SFCLAY_PHYSICS=sf_sfclay_physics & !CuP, wig, 24-Aug-2006
             ,BR=br,REGIME=regime,PBLH=pblh,KPBL=kpbl           & !CuP, wig, 24-Aug-2006
             ,T2=t2,Q2=q2                                       & !CuP, wig, 24-Aug-2006
             ,SLOPESFC=slopeSfc,SLOPEEZ=slopeEZ                 & !CuP, wig, 24-Aug-2006
             ,SIGMASFC=sigmasfc,SIGMAEZ=sigmaEZ                 & !CuP, wig, 24-Aug-2006
             ,CUPFLAG=cupflag,CLDFRA_CUP=cldfra_cup             & !CuP, wig, 18-Sep-2006
             ,CLDFRATEND_CUP=cldfratend_cup                     & !CuP, wig, 18-Sep-2006
             ,SHALL=shall,TAUCLOUD=taucloud,TACTIVE=tactive     & !CuP, wig, 18-Sep-2006
             ,ACTIVEFRAC=activeFrac                             & !CuP, lkb, 5-May-2010
             ,TSTAR=tstar, LNTERMS=lnterms                      & !CuP, wig 4-Oct-2006
             ,LNINT=lnint                                       & !CuP, wig 4-Oct-2006
             ,NUMBINS=numBins                                   & !CuP, lkb 4-Nov-2009
             ,THBINSIZE=thBinSize, RBINSIZE=rBinSize            & !CuP, lkb 4-Nov-2009
             ,MINDEEPFREQ=minDeepFreq, MINSHALLOWFREQ=minShallowFreq  & !CuP, lkb 4-Nov-2009
             ,WCLOUDBASE=wCloudBase                             & !CuP, lkb 29-April-2010
             ,WACT_CUP=wact_cup                                 & !CuP, rce 10-may-2012
             ,WULCL_CUP=wulcl_cup                               & !CuP, rce 10-may-2012
             ,WUP_CUP=wup_cup                                   & !CuP, rce 15-mar-2013 !BSINGH (12/06/2013)
             ,QC_IC_CUP=qc_ic_cup                               & !CuP, rce 10-may-2012
             ,QNDROP_IC_CUP=qndrop_ic_cup                       & !CuP, rce 10-may-2012
             ,QC_IU_CUP=qc_iu_cup                               & !CuP, rce 10-may-2012
             ,FCVT_QC_TO_PR_CUP=fcvt_qc_to_pr_cup               & !CuP, rce 10-may-2012
             ,FCVT_QC_TO_QI_CUP=fcvt_qc_to_qi_cup               & !CuP, rce 10-may-2012
             ,FCVT_QI_TO_PR_CUP=fcvt_qi_to_pr_cup               & !CuP, rce 10-may-2012
             ,MFUP_CUP=mfup_cup                                 & !CuP, rce 10-may-2012
             ,MFUP_ENT_CUP=mfup_ent_cup                         & !CuP, rce 10-may-2012
             ,MFDN_CUP=mfdn_cup                                 & !CuP, rce 10-may-2012
             ,MFDN_ENT_CUP=mfdn_ent_cup                         & !CuP, rce 10-may-2012
             ,UPDFRA_CUP=updfra_cup                             & !CuP, rce 10-may-2012
             ,TCLOUD_CUP=tcloud_cup                             & !CuP, rce 10-may-2012
             ,SHCU_AEROSOLS_OPT=shcu_aerosols_opt               & !CuP, rce 10-may-2012
#if ( WRF_CHEM == 1 )
             ,CHEM_OPT=chem_opt                                 & !CuP, rce 10-may-2012
             ,CHEM=grid%chem                                    & !CuP, rce 10-may-2012
#endif
             ! optionals
             ,RTHCUTEN=rthcuten                                 &
             ,RQVCUTEN=rqvcuten ,RQCCUTEN=rqccuten              &
             ,RQRCUTEN=rqrcuten ,RQICUTEN=rqicuten              &
             ,RQSCUTEN=rqscuten                                 &
             ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
             ,F_QI=f_qi,F_QS=f_qs                               &
             )
        
        
        DO J = jts,jte
           DO I=its,ite
              tmppratec(i,j)=raincv(i,j)/DT      !!!!!byang, move the pratec after calling cup
           end do
        end do
        !BSINGH -ENDS
#endif

     CASE DEFAULT 

         WRITE( wrf_err_message , * ) 'The cumulus option does not exist: cu_physics = ', cu_physics
         CALL wrf_error_fatal ( wrf_err_message )

   END SELECT cps_select

      ENDDO
      !$OMP END PARALLEL DO
#if  ( EM_CORE == 1 )
   IF(cu_physics .eq. 5 )then
#ifdef DM_PARALLEL
#     include "HALO_CUP_G3_OUT.inc"
#endif
      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij ,its,ite,jts,jte, i,j,k)
      DO ij = 1 , num_tiles
        its = i_start(ij)
        ite = i_end(ij)
        jts = j_start(ij)
        jte = j_end(ij)

        call conv_grell_spread3d(rthcuten=rthcuten,rqvcuten=rqvcuten                &
     &            ,rqccuten=rqccuten,raincv=raincv,cugd_avedx=cugd_avedx            &
     &            ,cugd_tten=cugd_tten,cugd_qvten=cugd_qvten,rqicuten=rqicuten      &
     &            ,cugd_ttens=cugd_ttens,cugd_qvtens=cugd_qvtens                    &
     &            ,cugd_qcten=cugd_qcten,pi_phy=pi,moist_qv=qv_curr                 &
     &            ,PRATEC=tmppratec,dt=dt,num_tiles=num_tiles                       &
     &            ,imomentum=imomentum                             &
     &            ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,F_QI=F_QI,F_QS=F_QS                &
     &            ,ids=IDS,ide=IDE, jds=JDS,jde=JDE, kds=KDS,kde=KDE                &
     &            ,ips=IPS,ipe=IPE, jps=JPS,jpe=JPE, kps=KPS,kpe=KPE                &
     &            ,ims=IMS,ime=IME, jms=JMS,jme=JME, kms=KMS,kme=KME                &
     &            ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte)
      ENDDO
      !$OMP END PARALLEL DO
   endif
#endif


   ! Copy pratec back to output array, if necessary.

   if (PRESENT(PRATEC)) then
      pratec(:,:) = tmppratec(:,:)
   endif

   ! Copy cudtacttime back if necessary

   if ( PRESENT(CUDTACTTIME) ) then
      cudtacttime = cudtacttime_pass
   end if

   CALL wrf_debug(200,'returning from cumulus_driver')

   END SUBROUTINE cumulus_driver

END MODULE module_cumulus_driver
