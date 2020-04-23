!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_pbl_driver
CONTAINS

!------------------------------------------------------------------
   SUBROUTINE pbl_driver(                                          &
                  itimestep,dt,u_frame,v_frame                     &
                 ,bldt,curr_secs,adapt_step_flag                   &
                 ,bldtacttime                                      & 
                 ,rublten,rvblten,rthblten                         &
                 ,tsk,xland,znt                                    &
#if (HWRF==1)
                 ,msang,scurx,scury,iwavecpl,lcurr_sf              &
                 ,mznt                                             &
#endif
                 ,ht                                               &   
                 ,ust,pblh,hfx,qfx,grdflx,hpbl_hold                &
                 ,u_phy,v_phy,w,th_phy,rho                         &
                 ,p_phy,pi_phy,p8w,t_phy,dz8w,z                    &
                 ,exch_h,exch_m,akhs,akms                          &
                 ,thz0,qz0,uz0,vz0,qsfc,f                          &
                 ,lowlyr,u10,v10,uoce,voce,t2                      &
                 ,psim,psih,fm,fhh,gz1oz0, wspd,br,chklowq         &
                 ,bl_pbl_physics, ra_lw_physics, dx, dy            &
                 ,dx2d ,area2d                                     &
                 ,stepbl,warm_rain                                 &
                 ,kpbl,mixht,ct,lh,snow,xice                       &
                 ,znu, znw, mut, p_top                             &
! paj
                 ,ctopo,ctopo2,windfarm_opt,power                  &
                 ,ysu_topdown_pblmix                               &
                 ,shinhong_tke_diag                                &
              ! OPTIONAL for TEMF scheme
                 ,te_temf,km_temf,kh_temf                     &
                 ,shf_temf,qf_temf,uw_temf,vw_temf                &
                 ,hd_temf,lcl_temf,hct_temf                       &
                 ,wupd_temf,mf_temf,thup_temf,qtup_temf,qlup_temf &
                 ,exch_temf,cf3d_temf,cfm_temf                    &
                 ,flhc,flqc                                        &
              ! MYNN
                 ,qke,Sh3d                                         &
                 ,qke_adv,bl_mynn_tkeadvect                        & !ACF for QKE advection
                 ,tsq,qsq,cov,rmol,ch,qcg,grav_settling            & 
                 ,dqke,qWT,qSHEAR,qBUOY,qDISS,bl_mynn_tkebudget    &
                 ,bl_mynn_cloudpdf                                 &
                 ,bl_mynn_mixlength                                &
                 ,icloud_bl,qc_bl,qi_bl,cldfra_bl                  &
                 ,bl_mynn_edmf,bl_mynn_edmf_mom,bl_mynn_edmf_tke   &
                 ,bl_mynn_mixscalars,bl_mynn_output                &
                 ,bl_mynn_cloudmix,bl_mynn_mixqt                   &
                 ,edmf_a,edmf_w,edmf_thl                           &
                 ,edmf_qt,edmf_ent,edmf_qc                         &
                 ,sub_thl3D,sub_sqv3D                              &
                 ,det_thl3D,det_sqv3D                              &
                 ,vdfg                                             &
                 ,nupdraft,maxMF,ktop_plume                        &
                 ,spp_pbl,pattern_spp_pbl                          &
                 ,restart,cycling                                  &
#if (NMM_CORE==1)
                 ,DISHEAT                                          &
                 ,HPBL2D, EVAP2D, HEAT2D, RC2D                     &   !Kwon FOR SHAL. CON.
#if ( HWRF == 1 )
                 ,VAR_RIC                                          &   !Kwon for Variable Ric
#endif
                 ,DKU3D,DKT3D                                      &
#endif
#if (HWRF==1)
                 ,coef_ric_l,coef_ric_s,gfs_alpha                  &
                 ,pert_pbl, ens_random_seed, ens_pblamp            &
#endif
                 ,ids,ide, jds,jde, kds,kde                        &
                 ,ims,ime, jms,jme, kms,kme                        &
                 ,i_start,i_end, j_start,j_end, kts,kte, num_tiles &
             ! Optional
                 ,hol, mol, regime                                 &
             ! Optional gravity-wave drag             
                 ,gwd_opt                                          &
                 ,dtaux3d,dtauy3d                                  &
                 ,dusfcg,dvsfcg,var2d,oc12d                        &
                 ,oa1,oa2,oa3,oa4,ol1,ol2,ol3,ol4                  &
                 ,sina,cosa                                        &
             !  Optional moisture tracers
                 ,qv_curr, qc_curr, qr_curr                        &
                 ,qi_curr, qs_curr, qg_curr                        &
                 ,rqvblten,rqcblten,rqiblten                       &
                 ,rqrblten,rqsblten,rqgblten                       &
             !  Optional aerosol
                 ,qnwfa_curr,f_qnwfa                               &
                 ,qnifa_curr,f_qnifa                               &
             !  Optional moisture tracer flags
                 ,f_qv,f_qc,f_qr                                   &
                 ,f_qi,f_qs,f_qg                                   &
! variables added for BEP
               ,frc_urb2d                                  &
               ,a_u_bep,a_v_bep,a_t_bep,a_q_bep            &
               ,b_u_bep,b_v_bep,b_t_bep,b_q_bep            &
               ,sf_bep,vl_bep                              &
               ,sf_sfclay_physics,sf_urban_physics         &
               ,tke_pbl,el_pbl                             &
#if (NMM_CORE != 1)
               ,wu_tur,wv_tur,wt_tur,wq_tur &
#endif
! variables  for GBM PBL
               ,exch_tke, rthraten                         &
               ,a_e_bep,b_e_bep,dlg_bep,dl_u_bep           &
               ,mfshconv, massflux_EDKF, entr_EDKF, detr_EDKF    & 
               ,thl_up, thv_up, rt_up ,rv_up, rc_up, u_up, v_up    &
               ,frac_up,rc_mf                                      & 
! Wind Turbine Parameterizations
               ,phb,xlat_u,xlong_u,xlat_v,xlong_v,id                 &
! variables required for camuwpbl scheme
               , z_at_w,cldfra_old_mp,cldfra, rthratenlw             &
               , tauresx2d,tauresy2d                                 &
               , tpert2d,qpert2d,wpert2d,wsedl3d                     &
               , turbtype3d,smaw3d                                   &
               , fnm,fnp                                             &
! variables required for camuwpbl scheme (optional)               
               ,qnc_curr,f_qnc,qni_curr,f_qni,rqniblten              &
               ,IS_CAMMGMP_USED                                      &
! for grims shallow convection with ysupbl/MYNN
               , wstar,delta                                         &
! for pbl mixing of scalars and tracers
     &        ,scalar,scalar_tend,num_scalar                         &
     &        ,tracer,tracer_tend,num_tracer                         &
     &        ,scalar_pblmix,tracer_pblmix                           &
#if (WRF_CHEM == 1)
               ,chem,vd,nchem,kdvel,ndvel,num_vert_mix             &
#endif
!
! FASDAS
!
              ,QNORM, fasdas           &
!
! END FASDAS
!
                                                                     )
       
!------------------------------------------------------------------
#if (EM_CORE==1)
   USE module_state_description, ONLY :                            &
                   YSUSCHEME,MRFSCHEME,GFSSCHEME,MYJPBLSCHEME,ACMPBLSCHEME,&
                   QNSEPBLSCHEME,MYNNPBLSCHEME2,MYNNPBLSCHEME3,BOULACSCHEME,&
                   CAMUWPBLSCHEME,BEPSCHEME,BEP_BEMSCHEME,MYJSFCSCHEME, &
                   FITCHSCHEME,SHINHONGSCHEME,                       &
                   TEMFPBLSCHEME,GBMPBLSCHEME,                       &
                   CAMMGMPSCHEME,p_qi,p_qni,p_qnc,param_first_scalar,& !CAMMGMPSCHEME, p_qni,p_qnc is used for camuwpbl scheme
                   p_qnwfa,p_qnifa
#if ( WRFPLUS == 1 )
   USE module_state_description, ONLY : SURFDRAGSCHEME
#endif
#else
   USE module_state_description, ONLY :                            &
                   YSUSCHEME,MRFSCHEME,GFSSCHEME,MYJPBLSCHEME,ACMPBLSCHEME &
                   , QNSEPBLSCHEME, p_qi,param_first_scalar &
                   , TEMFPBLSCHEME, GFSEDMFSCHEME           &
                   , CAMUWPBLSCHEME                                       &
                   , FITCHSCHEME, SHINHONGSCHEME                          &
                   , GBMPBLSCHEME, MYJSFCSCHEME
#endif

   USE module_model_constants

! *** add new modules of schemes here

   USE module_bl_myjpbl
   USE module_bl_qnsepbl
   USE module_bl_ysu
   USE module_bl_shinhong
   USE module_bl_mrf
   USE module_bl_gfs
#if (HWRF==1)
   USE module_bl_gfsedmf, only: bl_gfsedmf
#endif
   USE module_bl_acm
   USE module_bl_gwdo
   USE module_bl_myjurb
   USE module_bl_boulac
#if ( WRFPLUS == 1 )
   USE module_bl_surface_drag
#endif
   USE module_bl_camuwpbl_driver, only:CAMUWPBL
   USE module_bl_temf
   USE module_bl_mfshconvpbl
   USE module_bl_gbmpbl
#if (EM_CORE==1)
   USE module_bl_mynn
   USE module_bl_fogdes
   USE module_wind_fitch
#endif

   !  This driver calls subroutines for the PBL parameterizations.
   !
   !  pbl scheme:
   !      1. ysupbl
   !      2. myjpbl
   !      4. qnsepbl
   !      5. mynnpbl2
   !      6. mynnpbl3
   !      7. acmpbl
   !      8. boulacpbl
   !      9. camuwpbl
   !      10. temfpbl
   !      11. shinhongpbl
   !      98. surface_drag
   !      99. mrfpbl
   !      12. gbmpbl
   !
!------------------------------------------------------------------
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
! QNC        cloud Liq number concentration (#/kg) !For CAMUWPBL scheme
! QNI        cloud ice number concentration (#/kg) !For CAMUWPBL scheme
!-----------------------------------------------------------------
!-- RUBLTEN       U tendency due to 
!                 PBL parameterization (m/s^2)
!-- RVBLTEN       V tendency due to 
!                 PBL parameterization (m/s^2)
!-- RTHBLTEN      Theta tendency due to 
!                 PBL parameterization (K/s)
!-- RQVBLTEN      Qv tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- RQCBLTEN      Qc tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- RQIBLTEN      Qi tendency due to 
!                 PBL parameterization (kg/kg/s)
!-- RQNIBLTEN     Qni tendency due to 
!                 PBL parameterization (#/kg/s) !For CAMUWPBL scheme
!-- id            WRF grid id  (optional, only needed by turbine drag schemes)
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           downward short wave flux at ground surface (W/m^2)
!-- EMISS         surface emissivity (between 0 and 1)
!-- TSK           surface temperature (K)
!-- TMN           soil temperature at lower boundary (K)
!-- XLAND         land mask (1 for land, 2 for water)
!-- ZNT           thermal roughness length (m)
!-- MZNT          momentum roughness length (m)
!-- MAVAIL        surface moisture availability (between 0 and 1)
!-- UST           u* in similarity theory (m/s)
!-- MOL           T* (similarity theory) (K)
!-- HOL           PBL height over Monin-Obukhov length
!-- PBLH          PBL height (m)
!-- CAPG          heat capacity for soil (J/K/m^3)
!-- THC           thermal inertia (Cal/cm/K/s^0.5)
!-- SNOWC         flag indicating snow coverage (1 for snow cover)
!-- HFX           upward heat flux at the surface (W/m^2)
!-- QFX           upward moisture flux at the surface (kg/m^2/s)
!-- REGIME        flag indicating PBL regime (stable, unstable, etc.)
!-- exch_m        exchange coefficient for momentum, m^2/s
!-- exch_h        exchange coefficient for heat, K m/s 
!-- exch_tke      exchange coeff. for TKE [enhanced], m^2/s (gbmpbl scheme)
!-- rthraten      tendency from radiation, used in GBM PBL scheme
!-- akhs          sfc exchange coefficient of heat/moisture from MYJ
!-- akms          sfc exchange coefficient of momentum from MYJ
!-- tke_pbl       turbulence kinetic energy from PBL schemes (m^2/s^2)
!-- el_pbl        length scale from PBL schemes (m)
!-- wu_tur        turbulent flux of momentum (x) (m^2/s^2)
!-- wv_tur        turbulent flux of momentum (y) (m^2/s^2)
!-- wt_tur        turbulent flux of potential temperature  (K m/s)
!-- wq_tur        turbulent flux of water vapor  (- m/s)
!-- te_temf       Total energy from TEMF BL scheme
!-- km_temf       Exchange coefficient for momentum from TEMF BL scheme
!-- kh_temf       Exchange coefficient for heat from TEMF BL scheme
!-- shf_temf      Sensible heat flux from TEMF BL scheme
!-- qf_temf       Water vapor flux from TEMF BL scheme
!-- uw_temf       Momentum flux in U direction from TEMF BL scheme
!-- vw_temf       Momentum flux in V direction from TEMF BL scheme
!-- wupd_temf     Updraft velocity from TEMF BL scheme
!-- mf_temf       Mass flux from TEMF BL scheme
!-- thup_temf     Updraft thetal from TEMF BL scheme
!-- qtup_temf     Updraft qt from TEMF BL scheme
!-- qlup_temf     Updraft ql from TEMF BL scheme
!-- cf3d_temf     3D cloud fraction from TEMF PBL
!-- cfm_temf      Column cloud fraction from TEMF PBL
!-- exch_temf     Surface exchange coefficient (as for moisture) from TEMF surface layer scheme
!-- flhc          Surface exchange coefficient for heat (for TEMF)
!-- flqc          Surface exchange coefficient for moisture (for TEMF)
!-- thz0          potential temperature at roughness length (K)
!-- uz0           u wind component at roughness length (m/s)
!-- vz0           v wind component at roughness length (m/s)
!-- qsfc          specific humidity at lower boundary (kg/kg)
!-- UOCE          sea surface zonal currents (m s-1)
!-- VOCE          sea surface meridional currents (m s-1)
!-- th2           diagnostic 2-m theta from surface layer and lsm
!-- t2            diagnostic 2-m temperature from surface layer and lsm
!-- q2            diagnostic 2-m mixing ratio from surface layer and lsm
!-- lowlyr        index of lowest model layer above ground
!-- rr            dry air density (kg/m^3)
!-- u_phy         u-velocity interpolated to theta points (m/s)
!-- v_phy         v-velocity interpolated to theta points (m/s)
!-- th_phy        potential temperature (K)
!-- p_phy         pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- p8w           pressure at full levels (Pa)
!-- t_phy         temperature (K)
!-- dz8w          dz between full levels (m)
!-- z             height above sea level (m)
!-- DX            horizontal space interval (m)
!-- DT            time step (second)
!-- n_moist       number of moisture species
!-- PSFC          pressure at the surface (Pa)
!-- TSLB          
!-- ZS
!-- DZS
!-- num_soil_layers number of soil layer
!-- IFSNOW      ifsnow=1 for snow-cover effects
!-- z_at_w      Height above sea level at layer interfaces (m) 
!-- cldfra      Cloud fraction [unitless]
!-- cldfra_old_mp      Cloud fraction [unitless]
!-- rthratenlw  Tendency for LW ( K/s)
!-- tauresx2d   X-COMP OF RESIDUAL STRESS(m^2/s^2)
!-- tauresy2d   Y-COMP OF RESIDUAL STRESS(m^2/s^2)
!-- tpert2d     Convective temperature excess (K)
!-- qpert2d     Convective humidity excess (kg/kg)
!-- wpert2d     Turbulent velocity excess (m/s)
!-- wsedl3d     Sedimentation velocity of stratiform liquid cloud droplet (m/s)
!-- turbtype3d  Turbulent interface types [ no unit ]  
!-- smaw3d      Normalized Galperin instability function for momentum  ( 0<= <=4.964 and 1 at neutral ) [no units]
!
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QNC         species index for cloud liq number concentration !For CAMUWPBL scheme
!-- P_QNI         species index for cloud ice number concentration !For CAMUWPBL scheme
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
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!******************************************************************
!------------------------------------------------------------------ 
!


   INTEGER,    INTENT(IN   )    ::     bl_pbl_physics, ra_lw_physics,sf_sfclay_physics,sf_urban_physics,windfarm_opt
   INTEGER,    INTENT(IN   )    ::     ysu_topdown_pblmix
   INTEGER,    OPTIONAL,  INTENT(IN   )    ::     shinhong_tke_diag
   INTEGER,    OPTIONAL,  INTENT(IN   )    ::     scalar_pblmix, tracer_pblmix

   INTEGER,    INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       kts,kte, num_tiles
   INTEGER,    INTENT(IN   )    ::     num_scalar, num_tracer

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
  &                                    i_start,i_end,j_start,j_end

   INTEGER,    INTENT(IN   )    ::     itimestep,STEPBL
   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                        LOWLYR
!
   LOGICAL,      INTENT(IN   )    ::   warm_rain
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used !BSINGH:01/31/2013: Added for CAMUWPBL
   LOGICAL,    OPTIONAL,   INTENT(IN   )    ::   restart,cycling !used by mynn
#if (HWRF==1)
   INTEGER,      INTENT(IN   )    ::   iwavecpl
   LOGICAL,      INTENT(IN   )    ::   lcurr_sf
#endif
#if (NMM_CORE==1)
   LOGICAL,      INTENT(IN   )    ::   DISHEAT ! (for HWRF)
   REAL, DIMENSION( ims:ime , jms:jme ),                         &
                   INTENT(OUT) ::  HPBL2D, EVAP2D, HEAT2D, RC2D      !Kwon for Shallow Convection

   REAL, DIMENSION( ims:ime , jms:jme , kms:kme ),                         &
                   INTENT(OUT) ::  DKU3D,DKT3D                      !Kwon duffusivity
#endif
#if ( HWRF == 1 )
   REAL,       INTENT(IN)         ::   VAR_RIC                       !Kwon for variable Ric
   integer,intent(in) :: ens_random_seed
   real,intent(in) :: ens_pblamp
   logical,intent(in) :: pert_pbl
   REAL,       INTENT(IN)         ::   gfs_alpha,coef_ric_l,coef_ric_s
#endif


   REAL,       DIMENSION( kms:kme ),                              &
               OPTIONAL, INTENT(IN   )    ::               znu,   &
                                                           znw
!
   REAL,       INTENT(IN   )    ::     DT,DX,DY
   REAL,       DIMENSION( ims:ime, jms:jme ), INTENT(IN) , OPTIONAL :: &
                                                dx2d, area2d
   REAL,       INTENT(IN   ),OPTIONAL    ::     bldt
   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs
   LOGICAL,    INTENT(IN   ),OPTIONAL    ::     adapt_step_flag
   REAL,       INTENT(INOUT),OPTIONAL    ::     bldtacttime  
! Optional for Wind Turbine Parameterizations
   REAL, DIMENSION( ims:ime, kms:kme ,jms:jme ), &
         INTENT(IN), OPTIONAL    :: phb
   REAL, DIMENSION( ims:ime, jms:jme ), &
         INTENT(IN), OPTIONAL    :: xlat_u,xlong_u,xlat_v,xlong_v

   REAL,       DIMENSION( ims:ime, kms:kme ,jms:jme ),            &
               INTENT(IN), OPTIONAL    :: w
!
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(IN   )    ::                         p_phy, &
                                                          pi_phy, &
                                                             p8w, &
                                                             rho, &
                                                           t_phy, &
                                                           u_phy, &
                                                           v_phy, &
                                                            dz8w, &
                                                               z, &
                                                          th_phy

!1D variables required for CAMUWPBL scheme
   REAL , DIMENSION( kms:kme ) ,                                      &
        INTENT(IN   ) , OPTIONAL ::                                        fnm,  & !Factors for interpolation at "w" grid (interfaces)
                                                                fnp                                                                  
!3D Variables for camuwpbl scheme
    REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),           &
               INTENT(IN   ), OPTIONAL    ::              z_at_w, &
                                                   cldfra_old_mp, &
                                                          cldfra, &
                                                      rthratenlw, &
                                                      wsedl3d


!2D Variables required by camuwpbl scheme
    REAL,       DIMENSION( ims:ime , jms:jme ),                   &
               INTENT(INOUT   ), OPTIONAL    ::        tauresx2d, &
                                                       tauresy2d, &
                                                         tpert2d, &
                                                         qpert2d, &
                                                         wpert2d


!3D Variables for camuwpbl scheme - out
    REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),           &
               INTENT(OUT) , OPTIONAL   ::                      turbtype3d, &
                                                          smaw3d
!
! for grims shallow convection with ysupbl
!
    REAL,      DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT   ), OPTIONAL    ::          wstar
    REAL,      DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT   ), OPTIONAL    ::            delta
!
   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                         XLAND, &
                                                              HT, &
                                                            PSIM, &
                                                            PSIH, &
                                                              FM, &
                                                             FHH, &
                                                          GZ1OZ0, &
                                                              BR, &
                                                               F, &
                                                         CHKLOWQ
#if (HWRF==1)
   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN   )    ::                         SCURX, &
                                                           SCURY, &
                                                           MSANG
#endif

!
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)    ::                           TSK, &
                                                             UST, &
                                                            PBLH, &
                                                             HFX, &
                                                             QFX, &
                                                             ZNT, &
#if (HWRF==1)
                                                            MZNT, &  ! KWON FOR MOMENTUM Z0
#endif
                                                            QSFC, &
                                                            AKHS, &
                                                            AKMS, &
                                                           MIXHT, &
                                                             QZ0, &
                                                            THZ0, &
                                                             UZ0, &
                                                             VZ0, &
                                                              CT, &
                                                          GRDFLX, &
                                                             U10, &
                                                             V10, &
                                                            UOCE, &
                                                            VOCE, &
                                                              T2, &
                                                           POWER, &
                                                            WSPD
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               INTENT(INOUT)    ::                     HPBL_HOLD

!
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT)    ::                       RUBLTEN, &
                                                         RVBLTEN, &
                                                        RTHBLTEN, &
                                           EXCH_H,EXCH_M,TKE_PBL, &
                                                        RTHRATEN  ! for GBM PBL scheme
#if (NMM_CORE != 1)
 REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(OUT)    ::                       WU_TUR,WV_TUR,WT_TUR,WQ_TUR
!
#endif

!MYNN
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),     &
          INTENT(INOUT) ::        tsq,qsq,cov, & !,k_m,k_h,k_q &
                                  qke,Sh3d,                    &
                                  dqke,qWT,qSHEAR,qBUOY,qDISS, &
                                  qc_bl,qi_bl,cldfra_bl

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),     &
          INTENT(INOUT) ::         edmf_a,edmf_w,edmf_thl,     & !JOE- MYNN edmf
                                   edmf_qt,edmf_ent,edmf_qc,   & !JOE- MYNN edmf
                                   sub_thl3D,sub_sqv3D,        &
                                   det_thl3D,det_sqv3D

   INTEGER, OPTIONAL, INTENT(IN)  :: bl_mynn_tkebudget,        &
                                     grav_settling,            &
                                     bl_mynn_cloudpdf,         &
                                     bl_mynn_mixlength,        &
                                     bl_mynn_edmf,             &
                                     bl_mynn_edmf_mom,         &
                                     bl_mynn_edmf_tke,         &
                                     bl_mynn_mixscalars,       &
                                     bl_mynn_output,           &
                                     bl_mynn_cloudmix,         &
                                     bl_mynn_mixqt,            &
                                     icloud_bl

!ACF-QKE advection start
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
        & INTENT(INOUT) :: qke_adv
   LOGICAL, OPTIONAL, INTENT(IN) :: bl_mynn_tkeadvect
!ACF-QKE advection end
! Katata-added -
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL,              &
        &    INTENT(INOUT)::                               vdfg
! Katata-end
   INTEGER, OPTIONAL, DIMENSION( ims:ime , jms:jme ),           &
        &    INTENT(OUT) ::               nupdraft,ktop_plume
   REAL, OPTIONAL, DIMENSION( ims:ime , jms:jme ),              &
        &    INTENT(OUT) ::                               maxMF

    REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),     &
           INTENT(INOUT) ::         qnwfa_curr,qnifa_curr

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
        & INTENT(INOUT) ::  exch_tke      ! for GBM PBL scheme


   INTEGER, OPTIONAL :: id

   REAL,    DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(IN) ::  &
        & qcg,  ch
   REAL,    DIMENSION( ims:ime , jms:jme ), &
        &OPTIONAL, INTENT(INOUT) ::  rmol
   


!
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(OUT)    ::                          EL_PBL

   REAL ,                             INTENT(IN   )  ::  u_frame, &
                                                         v_frame
!

   INTEGER,    DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(INOUT) ::                             KPBL

   REAL,       DIMENSION( ims:ime , jms:jme ),                    &
               INTENT(IN)    :: XICE, SNOW, LH

   ! Stochastic parameter perturbations
   INTEGER,    INTENT(IN), OPTIONAL              ::             spp_pbl
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),INTENT(IN),OPTIONAL ::pattern_spp_pbl

! Bep changes: variable added for urban
   real, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::FRC_URB2D   ! URBAN Landuse fraction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep        ! Implicit component for the momemtum in X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep        ! Implicit component for the momemtum in Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep        ! Implicit component for the Pot. Temp.
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep        ! Implicit component for Moisture

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep        ! Implicit component for the TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep        ! Explicit component for the momemtum in X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep        ! Explicit component for the momemtum in Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep        ! Explicit component for the Pot. Temp.
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep        ! Explicit component for Moisture

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep        ! Explicit component for the TKE

   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep        ! Height above ground (L_ground in formula (24) of the BLM paper). 
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep        ! Length scale (lb in formula (22) ofthe BLM paper).
! urban surface and volumes        
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep           ! surfaces
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep            ! volumes
 
! Bep changes end
!  New variables for TEMF scheme
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(INOUT) :: te_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ),            &
               INTENT(  OUT) :: km_temf, kh_temf,        &
                         shf_temf,qf_temf,uw_temf,vw_temf,        &
                         wupd_temf,mf_temf,thup_temf,qtup_temf,   &
                         qlup_temf,cf3d_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(INOUT) :: flhc,flqc
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(  OUT) :: hd_temf, lcl_temf, hct_temf, cfm_temf
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ),            &
               INTENT(INOUT) :: exch_temf

!
!
! Optional
!
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
                                                     ,f_qg      &
                                                     ,f_qnc     & !used in CAMUWPBL
                                                     ,f_qni     & !used in CAMUWPBL
                                                     ,f_qnwfa   &
                                                     ,f_qnifa

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         OPTIONAL, INTENT(INOUT) ::                              &
                      ! optional moisture tracers
                      ! 2 time levels; if only one then use CURR
                      qv_curr, qc_curr, qr_curr                  &
                     ,qi_curr, qs_curr, qg_curr                  &
                     ,qnc_curr,qni_curr                          & !used in CAMUWPBL
                     ,rqvblten,rqcblten,rqrblten                 &
                     ,rqiblten,rqsblten,rqgblten,rqniblten         !rqniblten  used in CAMUWPBL

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::   & !local: added for MYNN
                      rqncblten,rqnwfablten,rqnifablten

   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(INOUT)    ::                           HOL, &
                                                             MOL, &
                                                          REGIME
   REAL,       DIMENSION( ims:ime, jms:jme )                    , &
               OPTIONAL                                         , &
               INTENT(IN)    ::                           mut
!
   INTEGER,    OPTIONAL, INTENT(IN)    ::               gwd_opt
   REAL,       OPTIONAL, INTENT(IN)    ::               p_top
!
  real,   dimension( ims:ime, kms:kme, jms:jme )                             , &
          optional                                                           , &
             intent(inout  )   ::                                     dtaux3d, &
                                                                      dtauy3d
!
  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(inout  )   ::                                      dusfcg, &
                                                                       dvsfcg
!
  real,   dimension( ims:ime, jms:jme )                                      , &
          optional                                                           , &
             intent(in  )   ::                                          var2d, &
                                                                        oc12d, &
                                                              oa1,oa2,oa3,oa4, &
                                                              ol1,ol2,ol3,ol4, &
                                                                    sina,cosa
! paj
   REAL, OPTIONAL,   DIMENSION( ims:ime , jms:jme ),                    &  !mchen
               INTENT(IN   )    ::                        CTOPO, &
                                                          CTOPO2

! Variables and Diagnostic for QNSE and EDKF JP
   INTEGER, INTENT(IN) ::  mfshconv

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,  &
               INTENT(OUT) ::    massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up,rc_mf  

    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer),INTENT(INOUT)   :: tracer_tend
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar
    REAL , OPTIONAL   ,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT)   :: scalar_tend

#if (WRF_CHEM == 1)
!   ACM Chem
    INTEGER, INTENT(IN) :: nchem, kdvel,ndvel,num_vert_mix
    REAL, INTENT(INOUT),  DIMENSION( ims:ime, kms:kme, jms:jme,nchem ) :: CHEM
    REAL, INTENT(IN)   ,  DIMENSION( ims:ime, kdvel, jms:jme, ndvel )  :: VD
#endif

!  LOCAL  VAR

   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::v_phytmp
   REAL,       DIMENSION( ims:ime, kms:kme, jms:jme ) ::u_phytmp

   REAL,       DIMENSION( ims:ime, jms:jme )          ::  TSKOLD, &
                                                          USTOLD, &
                                                          ZNTOLD, &
                                                             ZOL, &
                                                            PSFC

! make these allocatable depending on the setting of idiff
! Typically, we try to avoide allocating and deallocating local storage like this
! so as not to fragment the stack. But at this point, the idiff = 1 case is disabled
! (set to 0 for all cases) and has to be set manually by users who want to work with
! it.  When it becomes a more standard option, this should be redone, either defining
! these as state with package clauses to turn them on and off and passing them in,
! or pass in an integer flag that can be used to dimension the arrays to 1:1:1 as
! local variables.  JM 20100316

   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_u        ! Implicit component for the momemtum in X-direction
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_v        ! Implicit component for the momemtum in Y-direction
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_t        ! Implicit component for the Pot. Temp.
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::a_q        ! Implicit component for the water vapor

   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_u        ! Explicit component for the momemtum in X-direction
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_v        ! Explicit component for the momemtum in Y-direction
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_t        ! Explicit component for the Pot. Temp.
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::b_q        ! Explicit component for the water vapor

   REAL, ALLOCATABLE, DIMENSION( :, :, : )::sf         ! surfaces
   REAL, ALLOCATABLE, DIMENSION( :, :, : )::vl         ! volumes

   REAL    :: DTMIN,DTBL
!
   INTEGER :: initflag
!
   INTEGER :: i,J,K,NK,jj,ij,its,ite,jts,jte
   LOGICAL :: radiation
   LOGICAL :: flag_bep
   LOGICAL :: flag_myjsfc
   LOGICAL :: flag_qv, flag_qc, flag_qr, flag_qi, flag_qs, flag_qg, &
              flag_qnc,flag_qni, flag_qnwfa, flag_qnifa
   CHARACTER*256 :: message
   REAL    :: next_bl_time
   LOGICAL :: run_param , doing_adapt_dt , decided
   LOGICAL :: do_adapt
   integer iu_bep,iurb,idiff
   real seamask,thsk,zzz,unew,vnew,tnew,qnew,umom,vmom
   REAL :: z0,z1,z2,w1,w2
!
! FASDAS
!
   REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(  OUT) , OPTIONAL ::   QNORM
   INTEGER, INTENT(IN   )                                         ::   fasdas
   REAL                                                           ::   scrp1
!
! END FASDAS
!
!------------------------------------------------------------------
!
!!!!!!!if using BEP set flag_bep to true
    SELECT CASE(sf_urban_physics)
#if (NMM_CORE != 1)
      CASE (BEPSCHEME)
        flag_bep=.true.
      CASE (BEP_BEMSCHEME)
        flag_bep=.true.
#endif
      CASE DEFAULT
        flag_bep=.false.
    END SELECT

    SELECT CASE(sf_sfclay_physics)
      CASE (MYJSFCSCHEME)
         flag_myjsfc=.true.
      CASE DEFAULT
         flag_myjsfc=.false.
    END SELECT
!
  flag_qv = .FALSE. ; IF ( PRESENT( F_QV ) ) flag_qv = F_QV
  flag_qc = .FALSE. ; IF ( PRESENT( F_QC ) ) flag_qc = F_QC
  flag_qr = .FALSE. ; IF ( PRESENT( F_QR ) ) flag_qr = F_QR
  flag_qi = .FALSE. ; IF ( PRESENT( F_QI ) ) flag_qi = F_QI
  flag_qs = .FALSE. ; IF ( PRESENT( F_QS ) ) flag_qs = F_QS
  flag_qg = .FALSE. ; IF ( PRESENT( F_QG ) ) flag_qg = F_QG
  flag_qnc = .FALSE. ; IF ( PRESENT( F_QNC ) ) flag_qnc = F_QNC !Used in CAMUWPBL
  flag_qni = .FALSE. ; IF ( PRESENT( F_QNI ) ) flag_qni = F_QNI !Used in CAMUWPBL
  flag_qnwfa = .FALSE. ; IF ( PRESENT( F_QNWFA ) ) flag_qnwfa = F_QNWFA
  flag_qnifa = .FALSE. ; IF ( PRESENT( F_QNIFA ) ) flag_qnifa = F_QNIFA

  if (bl_pbl_physics .eq. 0) return

! RAINBL in mm (Accumulation between PBL calls)
!
   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( bldtacttime .eq. 0. ) THEN
            bldtacttime = CURR_SECS + bldt*60.
         END IF
      END IF
   END IF

!  Do we run through this scheme or not?

!    Test 1:  If this is the initial model time, then yes.
!                ITIMESTEP=1
!    Test 2:  If the user asked for the pbl to be run every time step, then yes.
!                BLDT=0 or STEPBL=1
!    Test 3:  If not adaptive dt, and this is on the requested pbl frequency, then yes.
!                MOD(ITIMESTEP,STEPBL)=0
!    Test 4:  If using adaptive dt and the current time is past the last requested activate pbl time, then yes.
!                CURR_SECS >= BLDTACTTIME

!  If we do run through the scheme, we set the flag run_param to TRUE and we set the decided flag
!  to TRUE.  The decided flag says that one of these tests was able to say "yes", run the scheme.
!  We only proceed to other tests if the previous tests all have left decided as FALSE.

!  If we set run_param to TRUE and this is adaptive time stepping, we set the time to the next
!  pbl run.

   run_param = .FALSE.
   decided = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( PRESENT(bldt) )THEN
      IF ( ( .NOT. decided ) .AND. &
           ( ( bldt .EQ. 0. ) .OR. ( stepbl .EQ. 1 ) ) ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   ELSE
      IF ( ( .NOT. decided ) .AND. &
                                   ( stepbl .EQ. 1 )   ) THEN
         run_param   = .TRUE.
         decided     = .TRUE.
      END IF
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepbl) .EQ. 0 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. bldtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      bldtacttime = curr_secs + bldt*60
   END IF


 IF (run_param) THEN
  radiation = .false.
  IF (ra_lw_physics .gt. 0) radiation = .true.

!---- 
! CALCULATE CONSTANT
 
   DTMIN=DT/60.
! PBL schemes need PBL time step for updates

    if (PRESENT(adapt_step_flag)) then
       if (adapt_step_flag) then
          do_adapt = .TRUE.
       else
          do_adapt = .FALSE.
       endif
    else
       do_adapt = .FALSE.
    endif

   if (PRESENT(BLDT)) then
      if (bldt .eq. 0) then
         DTBL = dt
      ELSE
         if (do_adapt) then
            IF ( curr_secs .LT. 2. * dt ) THEN
               call wrf_message("WARNING: When using an adaptive time-step the boundary layer"// &
                                " time-step should be 0 (i.e., equivalent to model time-step).  ")
               call wrf_message("In order to proceed, for boundary layer calculations, the "// &
                                "boundary layer time-step"// &
                                 " will be rounded to the nearest minute," )
                call wrf_message("possibly resulting in innacurate results.")
            END IF
            DTBL=bldt*60
         else
            DTBL=DT*STEPBL
         endif
      endif
   else
      DTBL=DT*STEPBL
   endif

#if (NMM_CORE != 1)
!!Alberto. If idiff=1, then compute the tendencies at the end in the routine diff3d
!!Alberto. If idiff=0, then compute the tendencies in each PBL routine (standard version).
       idiff=0
#else
! Added this else clause so that idiff is always initialized regardless of which core we are using
       idiff=0
#endif

   IF ( idiff .EQ. 1 ) THEN
     ALLOCATE (a_u(ims:ime,kms:kme,jms:jme))       ! Implicit component for the momemtum in X-direction
     ALLOCATE (a_v(ims:ime,kms:kme,jms:jme))       ! Implicit component for the momemtum in Y-direction
     ALLOCATE (a_t(ims:ime,kms:kme,jms:jme))       ! Implicit component for the Pot. Temp.
     ALLOCATE (a_q(ims:ime,kms:kme,jms:jme))       ! Implicit component for the water vapor
     ALLOCATE (b_u(ims:ime,kms:kme,jms:jme))       ! Explicit component for the momemtum in X-direction
     ALLOCATE (b_v(ims:ime,kms:kme,jms:jme))       ! Explicit component for the momemtum in Y-direction
     ALLOCATE (b_t(ims:ime,kms:kme,jms:jme))       ! Explicit component for the Pot. Temp.
     ALLOCATE (b_q(ims:ime,kms:kme,jms:jme))       ! Explicit component for the water vapor
     ALLOCATE (sf(ims:ime,kms:kme,jms:jme) )       ! surfaces
     ALLOCATE (vl(ims:ime,kms:kme,jms:jme) )       ! volumes
   ENDIF
   
! SAVE OLD VALUES

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )

   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         TSKOLD(i,j)=TSK(i,j)
         USTOLD(i,j)=UST(i,j)
         ZNTOLD(i,j)=ZNT(i,j)
         HPBL_HOLD(i,j)=PBLH(i,j)

         DO k=kts,kte
            v_phytmp(i,k,j)=v_phy(i,k,j)+v_frame
            u_phytmp(i,k,j)=u_phy(i,k,j)+u_frame
         ENDDO

! PSFC : in Pa

         PSFC(I,J)=p8w(I,kms,J)

         DO k=kts,min(kte+1,kde)
            RTHBLTEN(I,K,J)=0.
            RUBLTEN(I,K,J)=0.
            RVBLTEN(I,K,J)=0.
            IF ( PRESENT( RQCBLTEN )) RQCBLTEN(I,K,J)=0.
            IF ( PRESENT( RQVBLTEN )) RQVBLTEN(I,K,J)=0.
         ENDDO

         IF (flag_QI .AND. PRESENT(RQIBLTEN) ) THEN
            DO k=kts,min(kte+1,kde)
               RQIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
         !Following if condition is added for CAMUWPBL scheme
         IF (flag_QNI .AND. PRESENT(RQNIBLTEN) ) THEN
            DO k=kts,min(kte+1,kde)
               RQNIBLTEN(I,K,J)=0.
            ENDDO
         ENDIF
      ENDDO
      ENDDO

      IF (bl_pbl_physics .EQ. QNSEPBLSCHEME ) THEN
! use u_phytmp instead of wthv_mf, and v_phytmp instead of lm_bl89
           DO j=j_start(ij),j_end(ij)
           DO i=i_start(ij),i_end(ij)
           DO k=kms,kme
              u_phytmp(i,k,j)=0.
              v_phytmp(i,k,j)=0.
           ENDDO
           ENDDO
           ENDDO
      ENDIF

#if (NMM_CORE != 1)
      IF ( idiff.eq.1 ) THEN
!Alberto
! Here we should put a switch: 
! if we do not use BEP, just initialize the values of the a, and b to zero, except for the lowest model level, 
! where the heat and momentum fluxes are introduced         
! if we use BEP, use the values computed by BEP weigthed by the urban fraction.
! for the moment I put a simple "if" but it should be changed to a more elegant way after(using the CASE, maybe?)
!!!!!!!!!!!!!!
! This is the more general way to go, however some of these variables (e. g. a_t, a_q) may be zero nearly all time. 
! if we need to be as tight as possible for the memory we can think how to reduce the storage.
!!!!!!!!!!!!!!!!!!
! This stuff is becoming large, probably better to put in a subroutine
!!!!!!!!!!!!!!!!!!!
! preparing the a, b, sf, and vl terms
         
         IF (flag_bep) THEN    
           do j=j_start(ij),j_end(ij)
           do i=i_start(ij),i_end(ij)            
           do k=kts,kte
             a_u(i,k,j)=a_u_bep(i,k,j)
             a_v(i,k,j)=a_v_bep(i,k,j)
             a_t(i,k,j)=a_t_bep(i,k,j)
             a_q(i,k,j)=a_q_bep(i,k,j)
             b_u(i,k,j)=b_u_bep(i,k,j)
             b_v(i,k,j)=b_v_bep(i,k,j)
             b_t(i,k,j)=b_t_bep(i,k,j)
             b_q(i,k,j)=b_q_bep(i,k,j)
             vl(i,k,j)=vl_bep(i,k,j)
             sf(i,k,j)=sf_bep(i,k,j)
           enddo
           sf(i,kte+1,j)=1.
           enddo
           enddo
         ELSE
           do j=j_start(ij),j_end(ij)
           do i=i_start(ij),i_end(ij)
           do k=kts,kte
             a_u(i,k,j)=0.
             a_v(i,k,j)=0.
             a_t(i,k,j)=0.
             a_q(i,k,j)=0.
             b_u(i,k,j)=0.
             b_v(i,k,j)=0.
             b_t(i,k,j)=0.
             b_q(i,k,j)=0.
             vl(i,k,j)=1.
             sf(i,k,j)=1.
           enddo
           sf(i,kte+1,j)=1.
! fix the values for the surface fluxes (source/sink terms)
! here for momentum the resolution is done implicitely
! while for heat and moisture is done explicitely 
            a_u(i,1,j)=(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)           
            a_v(i,1,j)=(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)
            b_t(i,1,j)=hfx(i,j)/rho(i,1,j)/CP/dz8w(i,1,j)
            b_q(i,1,j)=qfx(i,j)/rho(i,1,j)/dz8w(i,1,j)
!! if you want to solve also the heat and moisture fluxes implicitely, uncomment the following lines.
!! (of course you will need the values of thz0,qz0,akhs).
!             b_t(i,1,j)=akhs(i,j)*(thz0(I,J))/dz8w(i,1,j)
!             b_q(i,1,j)=akhs(i,j)*(qz0(I,J))/dz8w(i,1,j)
!             a_t(i,1,j)=-1.*akhs(i,j)/dz8w(i,1,j)
!             a_q(i,1,j)=-1.*akhs(i,j)/dz8w(i,1,j)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           enddo
           enddo
           
         ENDIF

        endif      
                                           
     
 
!Alberto. From this point if some PBL scheme has a non local term 
! (not dependent on the local values of the variable)
! this can be added to b_t, b_q, or b_u, b_v.
!!!!!!!!!!!!!!!!!!!           
 
#endif 
      
   ENDDO
   !$OMP END PARALLEL DO
!
  !$OMP PARALLEL DO   &
  !$OMP PRIVATE ( ij, i,j,k, its, ite, jts, jte, z0, z1, z2, w1, w2, message, initflag )
  DO ij = 1 , num_tiles

   its = i_start(ij)
   ite = i_end(ij)
   jts = j_start(ij)
   jte = j_end(ij)

   pbl_select: SELECT CASE(bl_pbl_physics)

      CASE (TEMFPBLSCHEME)
! WA Test
       ! WRITE( message , * ) 'Calling TEMF PBL scheme, timestep = ', itimestep
       ! CALL wrf_message ( message )
       ! print *,'Calling TEMF PBL scheme'
        CALL wrf_debug(100,'in TEMF PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( te_temf ) .AND. PRESENT( cfm_temf ) .AND. &
                PRESENT( hol      ) ) THEN
             CALL temfpbl(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy,RHO=rho               &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,g=g,cp=cp,rcp=rcp,r_d=r_d,r_v=r_v,cpv=cpv                    &
              ,Z=z,XLV=XLV,PSFC=PSFC               &
              ,P_TOP=p_top                                          &
              ,ZNT=znt,HT=ht,UST=ust,ZOL=zol,HOL=hol,HPBL=pblh      &
              ,PSIM=psim,PSIH=psih,XLAND=xland                      &
              ,HFX=hfx,QFX=qfx,TSK=tskold,QSFC=qsfc,GZ1OZ0=gz1oz0   &
              ,U10=u10,V10=v10,T2=t2                                &
              ,WSPD=wspd,BR=br,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt                                       &
              ,te_temf=te_temf,kh_temf=kh_temf,km_temf=km_temf      &
              ,shf_temf=shf_temf,qf_temf=qf_temf                    &
              ,uw_temf=uw_temf,vw_temf=vw_temf                      &
              ,hd_temf=hd_temf,lcl_temf=lcl_temf,hct_temf=hct_temf  &
              ,wupd_temf=wupd_temf,mf_temf=mf_temf                  &
              ,thup_temf=thup_temf,qtup_temf=qtup_temf,qlup_temf=qlup_temf  &
              ,cf3d_temf=cf3d_temf,cfm_temf=cfm_temf                &
              ,flhc=flhc,flqc=flqc,exch_temf=exch_temf              &
              ,fCor=f                                            &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               CALL wrf_error_fatal('Lack arguments to call TEMF pbl')
           ENDIF

      CASE (YSUSCHEME)
        CALL wrf_debug(100,'in YSU PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( hol      ) ) THEN
!
             CALL ysu(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy                       &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,CP=cp,G=g,ROVCP=rcp,RD=r_D,ROVG=rovg                 &
              ,DZ8W=dz8w,XLV=XLV,RV=r_v,PSFC=PSFC                   &
              ,ZNT=znt,UST=ust,HPBL=pblh                            &
              ,PSIM=fm,PSIH=fhh,XLAND=xland                         &
              ,HFX=hfx,QFX=qfx                                      &
              ,U10=u10,V10=v10                                      &
              ,UOCE=uoce,VOCE=voce                                  &
! paj
              ,CTOPO=ctopo,CTOPO2=ctopo2                            &
              ,YSU_TOPDOWN_PBLMIX=ysu_topdown_pblmix                &
              ,WSPD=wspd,BR=br,DT=dtbl,KPBL2D=kpbl                  &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman                      &
              ,EXCH_H=exch_h,EXCH_M=exch_m,REGIME=regime            &
              ,RTHRATEN=RTHRATEN                                    &
! for grims shallow convection with ysupbl
              ,WSTAR=wstar,DELTA=delta                              &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
!
! FASDAS
!
              IF( fasdas == 1 ) THEN
                  DO j=j_start(ij),j_end(ij)
                  DO i=i_start(ij),i_end(ij)
                     scrp1 = rqvblten(i,1,j)
                     scrp1 = scrp1*(2.0*DT)/qv_curr(i,1,j)
                     scrp1 = scrp1*100.0
                     scrp1 = amax1(0.0,scrp1)
                     scrp1 = amin1(5.0,scrp1)
                     QNORM(I,J)= scrp1/100.0
                  ENDDO
                  ENDDO
              ENDIF
!
! END FASDAS
!

           ELSE
               WRITE ( message , FMT = '(A,7(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'qi_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'rqiblten, '//                                     &
                 'hol = ' ,                                         &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( qi_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( rqiblten ) ,                             &
                  PRESENT( hol      )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call YSU pbl')
           ENDIF

      CASE (SHINHONGSCHEME)
        CALL wrf_debug(100,'in SHINHONG PBL')
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( qi_curr )                            .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( rqiblten )                           .AND. &
                PRESENT( hol      ) ) THEN
!
             CALL shinhong(                                         &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy                       &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,FLAG_QI=flag_qi                                      &
              ,CP=cp,G=g,ROVCP=rcp,RD=r_D,ROVG=rovg                 &
              ,DZ8W=dz8w,XLV=XLV,RV=r_v,PSFC=PSFC                   &
              ,ZNU=znu,ZNW=znw,P_TOP=p_top                          &
              ,ZNT=znt,UST=ust,HPBL=pblh                            &
              ,PSIM=fm,PSIH=fhh,XLAND=xland                         &
              ,HFX=hfx,QFX=qfx                                      &
              ,U10=u10,V10=v10                                      &
! paj
              ,CTOPO=ctopo,CTOPO2=ctopo2                            &
              ,SHINHONG_TKE_DIAG=shinhong_tke_diag                  &
              ,WSPD=wspd,BR=br,DT=dtbl,KPBL2D=kpbl                  &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman                      &
              ,EXCH_H=exch_h,REGIME=regime                          &
! for grims shallow convection with shinhongpbl
              ,WSTAR=wstar,DELTA=delta                              &
              ,TKE_PBL=tke_pbl,EL_PBL=el_pbl,CORF=f                 &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
              ,DX=dx,DY=dy                                          &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,7(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'qi_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'rqiblten, '//                                     &
                 'hol = ' ,                                         &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( qi_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( rqiblten ) ,                             &
                  PRESENT( hol      )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call SHINHONG pbl')
           ENDIF

      CASE (MRFSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( hol      )                           .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MRF')
             CALL mrf(                                              &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr                                         &
              ,QC3D=qc_curr                                         &
              ,QI3D=qi_curr                                         &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,DZ8W=dz8w,Z=z,XLV=xlv,RV=r_v,PSFC=psfc               &
              ,P1000MB=p1000mb                                      &
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol                      &
              ,PBL=pblh,PSIM=psim,PSIH=psih                         &
              ,XLAND=xland,HFX=hfx,QFX=qfx,TSK=tskold               &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,BR=br                        &
              ,DT=dtbl,DTMIN=dtmin,KPBL2D=kpbl                      &
              ,SVP1=svp1,SVP2=svp2,SVP3=svp3,SVPT0=svpt0            &
              ,EP1=ep_1,EP2=ep_2,KARMAN=karman,EOMEG=eomeg          &
              ,STBOLT=stbolt,REGIME=regime                          &
              ,FLAG_QI=flag_qi                                      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    ) 
           ELSE
               WRITE ( message , FMT = '(A,5(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'hol = ' ,                                         &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( hol      )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call MRF pbl')
           ENDIF

      CASE (GFSSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in GFS')
             CALL bl_gfs(                                           &
               U3D=u_phytmp,V3D=v_phytmp                            &
              ,TH3D=th_phy,T3D=t_phy                                &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten                                    &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,DZ8W=dz8w,z=z,PSFC=psfc                              &
              ,UST=ust,PBL=pblh,PSIM=psim,PSIH=psih                 &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br                                      &
              ,DT=dtbl,KPBL2D=kpbl,EP1=ep_1,KARMAN=karman           &
#if (NMM_CORE==1)
              ,DISHEAT=DISHEAT                                      &
#endif
#if ( HWRF == 1 )
              ,ALPHA=gfs_alpha                                      &
              ,HPBL2D=HPBL2D, EVAP2D=EVAP2D, HEAT2D=HEAT2D          &   !Kwon add FOR SHAL. CON.
              ,VAR_RIC=VAR_RIC                                      &   !Kwon for variable Ric
              ,U10=U10,V10=V10,ZNT=ZNT,MZNT=MZNT,RC2D=RC2D          &   !Kwon for variable Ric
              ,DKU3D=DKU3D,DKT3D=DKT3D                              &
              ,coef_ric_l=coef_ric_l,coef_ric_s=coef_ric_s,xland=xland    &
              ,MSANG=MSANG,SCURX=SCURX,SCURY=SCURY,IWAVECPL=IWAVECPL,LCURR_SF=LCURR_SF &
              ,pert_pbl=pert_pbl                                    &
              ,ens_random_seed=ens_random_seed                      &
              ,ens_pblamp=ens_pblamp                                &
#endif
              ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar          &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ',                                     &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call GFS pbl')
           ENDIF

#if (HWRF==1)
      CASE (GFSEDMFSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in GFS')
             CALL bl_gfsedmf(                                       &
               U3D=u_phytmp,V3D=v_phytmp                            &
              ,TH3D=th_phy,T3D=t_phy                                &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten                                    &
              ,CP=cp,G=g,ROVCP=rcp,R=r_d,ROVG=rovg                  &
              ,P_QI=p_qi,P_FIRST_SCALAR=param_first_scalar          &
              ,DZ8W=dz8w,z=z,PSFC=psfc                              &
              ,UST=ust,PBL=pblh,PSIM=psim,PSIH=psih                 &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br                                      &
              ,DT=dtbl,KPBL2D=kpbl,EP1=ep_1,KARMAN=karman           &
              ,DISHEAT=DISHEAT                                      &
              ,RTHRATEN=RTHRATEN                    &
              ,HPBL2D=HPBL2D, EVAP2D=EVAP2D, HEAT2D=HEAT2D          &
              ,U10=u10,V10=v10,ZNT=mznt                             &
              ,DKU3D=dku3d,DKT3D=dkt3d                             &
              ,VAR_RIC=VAR_RIC                                      &
              ,coef_ric_l=coef_ric_l,coef_ric_s=coef_ric_s          &
              ,ALPHA=gfs_alpha                                      &
              ,xland=xland                                          &
#if (HWRF==1)
              ,pert_pbl=pert_pbl                                    &
              ,ens_random_seed=ens_random_seed                      &
              ,ens_pblamp=ens_pblamp                                &
#endif
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ',                                     &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call GFS edmf pbl')
           ENDIF
#endif
      CASE (MYJPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN

             CALL wrf_debug(100,'in MYJPBL')
            IF ( .not.flag_bep .and. idiff.ne.1) THEN
             CALL myjpbl(DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w          &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr,QCW=qc_curr,QCI=qi_curr,QCS=qs_curr       & !BSF
              ,QCR=qr_curr,QCG=qg_curr                              & !BSF
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_pbl,EXCH_H=exch_h,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh,MIXHT=mixht             &  
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,RQIBLTEN=rqiblten,RQSBLTEN=rqsblten                  & !BSF
              ,RQRBLTEN=rqrblten,RQGBLTEN=rqgblten                  & !BSF
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ELSE !(SF_URBAN_PHYSICS.EQ.2)
! Bep changes begin

             CALL myjurb(IDIFF=idiff,FLAG_BEP=flag_bep              &
              ,DT=dtbl,STEPBL=stepbl,HT=ht,DZ=dz8w                  &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0                              &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE_MYJ=tke_pbl,EXCH_H=exch_h,EXCH_M=exch_m          &
              ,USTAR=ust,ZNT=znt                                    &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
! Multi-layer UCM
              ,FRC_URB2D=frc_urb2d                                  &
              ,A_U_BEP=a_u_bep,A_V_BEP=a_v_bep,A_T_BEP=a_t_bep      &
              ,A_Q_BEP=a_q_bep                                      &
              ,A_E_BEP=a_e_bep,B_U_BEP=b_u_bep,B_V_BEP=b_v_bep      &
              ,B_T_BEP=b_t_bep,B_Q_BEP=b_q_bep                      &
              ,B_E_BEP=b_e_bep,DLG_BEP=dlg_bep                      &
              ,DL_U_BEP=dl_u_bep,SF_BEP=sf_bep,VL_BEP=vl_bep        &
! UCM end
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
            ENDIF

           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call MYJ pbl')
           ENDIF
 
      CASE (QNSEPBLSCHEME)
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
               IF ( MFSHCONV.EQ.1 )THEN
               CALL wrf_debug(100,'in MFSHCONVPBL')
               CALL mfshconvpbl(DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w         &
                    ,RHO=rho,PMID=p_phy,PINT=p8w,TH=th_phy,EXNER=pi_phy   &
                    ,QV=qv_curr, QC=qc_curr                    &
                    ,U=u_phy,V=v_phy                                      &
                    ,HFX=hfx, QFX=qfx,TKE=tke_pbl                         &
                    ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
                    ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
                    ,IDS=IDS,IDE=IDE,JDS=JDS,JDE=JDE,KDS=KDS,KDE=KDE      &
                    ,IMS=IMS,IME=IME,JMS=JMS,JME=JME,KMS=KMS,KME=KME      &
                    ,ITS=ITS,ITE=ITE,JTS=JTS,JTE=JTE,KTS=KTS,KTE=KTE      &
                    ,KRR=2,MASSFLUX_EDKF=massflux_EDKF                    &
                    ,ENTR_EDKF=entr_EDKF, DETR_EDKF=detr_EDKF             & 
                    ,THL_UP=thl_up, THV_UP=thv_up, RT_UP=rt_up            &
                    ,RV_UP=rv_up,RC_UP=rc_up, U_UP=u_up, V_UP=v_up        &
                    ,FRAC_UP=frac_up, RC_MF=rc_mf,WTHV=u_phytmp           &
                    ,PLM_BL89=v_phytmp   ) 
               ENDIF   

             CALL wrf_debug(100,'in QNSEPBL')
             CALL qnsepbl(                                           &
               DT=dt,STEPBL=stepbl,HT=ht,DZ=dz8w                    &
              ,PMID=p_phy,PINT=p8w,TH=th_phy,T=t_phy,EXNER=pi_phy   &
              ,QV=qv_curr, CWM=qc_curr                              &
              ,U=u_phy,V=v_phy,RHO=rho                              &
              ,TSK=tsk,QSFC=qsfc,CHKLOWQ=chklowq,THZ0=thz0          &
              ,QZ0=qz0,UZ0=uz0,VZ0=vz0,CORF=f                       &
              ,LOWLYR=lowlyr                                        &
              ,XLAND=xland,SICE=xice,SNOW=snow                      &
              ,TKE=tke_pbl,EXCH_H=exch_h,EXCH_M=exch_m,USTAR=ust,ZNT=znt      &
              ,EL_MYJ=el_pbl,PBLH=pblh,KPBL=kpbl,CT=ct              &
              ,AKHS=akhs,AKMS=akms,ELFLX=lh                         &
              ,HFX=hfx,WTHV_MF=u_phytmp,LM_BL89=v_phytmp            & 
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten    &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
! note the tendencies have been added inside qnse 
!            IF ( PRESENT (MFSHCONV) )THEN
!              IF (MFSHCONV.EQ.1)THEN
!              rublten(:,:,:)=rublten(:,:,:)+rumften(:,:,:)
!              rvblten(:,:,:)=rvblten(:,:,:)+rvmften(:,:,:)
!              rthblten(:,:,:)=rthblten(:,:,:)+rthmften(:,:,:)
!              rqvblten(:,:,:)=rqvblten(:,:,:)+rqvmften(:,:,:)
!              rqcblten(:,:,:)=rqcblten(:,:,:)+rqcmften(:,:,:)
!              ENDIF   
!            ENDIF   

            ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call QNSE pbl')
           ENDIF

      CASE (ACMPBLSCHEME)
           
           !!  These are values that are not supplied to pbl driver, but are required by ACM
           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                                                        .TRUE.  ) THEN
             CALL wrf_debug(100,'in ACM PBL')

             CALL ACMPBL(                                                        &
               XTIME=itimestep, DTPBL=dtbl,U3D=u_phytmp, V3D=v_phytmp          &
              ,PP3D=p_phy, DZ8W=dz8w, TH3D=th_phy, T3D=t_phy                   &
              ,QV3D=qv_curr, QC3D=qc_curr, QI3D=qi_curr, RR3D=rho                &
#if (WRF_CHEM == 1)
              ,CHEM3D=CHEM,   VD3D=VD,  NCHEM=nchem, KDVEL=kdvel              &
              ,NDVEL=ndvel, NUM_VERT_MIX=num_vert_mix                         &
#endif
              ,UST=UST, HFX=HFX, QFX=QFX, TSK=tsk                               &
              ,PSFC=PSFC, EP1=EP_1, G=g, ROVCP=rcp,RD=r_D,CPD=cp                 &
              ,PBLH=pblh, KPBL2D=kpbl, EXCH_H=exch_h, REGIME=regime              &
              ,GZ1OZ0=gz1oz0,WSPD=wspd,PSIM=psim, MUT=mut, RMOL=rmol             &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten                 &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten             &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde                   &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme                   &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte                   &   
                                                                      )
           ELSE
               WRITE ( message , FMT = '(A,4(L1,1X))' )             &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'rqvblten, '//                                     &
                 'rqcblten = ' ,                                    &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten )
               CALL wrf_debug(0,message)
               CALL wrf_error_fatal('Lack arguments to call ACM2 pbl')
           ENDIF

#if (EM_CORE==1)

        CASE (MYNNPBLSCHEME2, MYNNPBLSCHEME3)

           IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND. &
                PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
                PRESENT( qi_curr ) .AND. PRESENT( rqiblten ) .AND.  &
                PRESENT( rqniblten ) .AND. PRESENT( qni_curr ) .AND.&
                PRESENT(qke) .AND. PRESENT(tsq) .AND.               &
                PRESENT(qsq) .AND. PRESENT(cov) .AND.               &
                PRESENT(rmol) .AND.                                 &
                PRESENT(qcg) .AND. PRESENT(ch) .AND.                &
                PRESENT(grav_settling) .AND.                        &
                PRESENT(bl_mynn_tkebudget) .AND.                    &
!ACF,JOE-QKE advection
                PRESENT(qke_adv) .AND. PRESENT(bl_mynn_tkeadvect) .AND.&
!ACF,JOE-end
                PRESENT(vdfg) ) THEN

              CALL wrf_debug(100,'in MYNNPBL')

              IF (itimestep==1) THEN
                 initflag=1
              ELSE
                 initflag=0
              ENDIF
              
              CALL  mynn_bl_driver(&
                   &initflag=initflag,restart=restart,cycling=cycling,   &
                   &grav_settling=grav_settling,                         &
                   &delt=dtbl,dz=dz8w,dx=dx,znt=znt,                     &
                   &u=u_phy,v=v_phy,w=w,th=th_phy,qv=qv_curr,            &
                   &qc=qc_curr,qi=qi_curr,                               &
                   &qnc=qnc_curr,qni=qni_curr,                           &
                   &QNWFA=qnwfa_curr,QNIFA=qnifa_curr,                   &
                   &p=p_phy,exner=pi_phy,rho=rho,T3D=t_phy,              &
                   &xland=xland,ts=tsk,qsfc=qsfc,qcg=qcg,ps=psfc,        &
                   &ust=ust,ch=ch,hfx=hfx,qfx=qfx,rmol=rmol,wspd=wspd,   &
                   &uoce=uoce,voce=voce,                                 & !Ocean currents
                   &vdfg=vdfg,                                           & !Katata -added
                   &Qke=qke,Sh3d=Sh3d,                                   &
                   &qke_adv=qke_adv,bl_mynn_tkeadvect=bl_mynn_tkeadvect, &
#if (WRF_CHEM == 1)
                   &chem3d=chem,vd3d=vd,nchem=nchem,kdvel=kdvel,         & ! WA 7/31/15
                   &ndvel=ndvel,num_vert_mix=num_vert_mix,               &
#endif
                   &Tsq=tsq,Qsq=qsq,Cov=cov,                             &
                   &RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten,   &
                   &RQVBLTEN=rqvblten,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten,&
                   &RQNCBLTEN=rqncblten,RQNIBLTEN=rqniblten,             &
                   &RQNWFABLTEN=rqnwfablten,RQNIFABLTEN=rqnifablten,     &
                   &EXCH_H=exch_h,EXCH_M=exch_m,                         &
                   &pblh=pblh,KPBL=KPBL                                  &
                   &,el_pbl=el_pbl                                       &
                   &,dqke=dqke                                           &
                   &,qWT=qWT,qSHEAR=qSHEAR,qBUOY=qBUOY,qDISS=qDISS       &
                   &,WSTAR=wstar,DELTA=delta                             & ! for grims shallow-cu
                   &,bl_mynn_tkebudget=bl_mynn_tkebudget                 &
                   &,bl_mynn_cloudpdf=bl_mynn_cloudpdf                   &
                   &,bl_mynn_mixlength=bl_mynn_mixlength                 &
                   &,icloud_bl=icloud_bl,qc_bl=qc_bl,qi_bl=qi_bl         &
                   &,cldfra_bl=cldfra_bl                                 &
                   &,bl_mynn_edmf=bl_mynn_edmf                           &
                   &,bl_mynn_edmf_mom=bl_mynn_edmf_mom                   &
                   &,bl_mynn_edmf_tke=bl_mynn_edmf_tke                   &
                   &,bl_mynn_mixscalars=bl_mynn_mixscalars               &
                   &,bl_mynn_output=bl_mynn_output                       &
                   &,bl_mynn_cloudmix=bl_mynn_cloudmix                   &
                   &,bl_mynn_mixqt=bl_mynn_mixqt                         &
                   &,edmf_a=edmf_a,edmf_w=edmf_w,edmf_qt=edmf_qt         &
                   &,edmf_thl=edmf_thl,edmf_ent=edmf_ent,edmf_qc=edmf_qc &
                   &,sub_thl3D=sub_thl3D,sub_sqv3D=sub_sqv3D             &                                                                               
                   &,det_thl3D=det_thl3D,det_sqv3D=det_sqv3D             &
                   &,nupdraft=nupdraft,maxMF=maxMF                       &
                   &,ktop_plume=ktop_plume                               &
                   &,spp_pbl=spp_pbl,pattern_spp_pbl=pattern_spp_pbl     &
                   &,RTHRATEN=RTHRATEN                                   &
                   &,FLAG_QC=flag_qc,FLAG_QI=flag_qi                     &
                   &,FLAG_QNC=flag_qnc,FLAG_QNI=flag_qni                 &
                   &,FLAG_QNWFA=flag_qnwfa,FLAG_QNIFA=flag_qnifa         &
                   ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
                   ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
                   ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                   )
           ELSE
               WRITE ( message , FMT = '(A,20(L1,1X))' )            &
                 'present: '//                                      &
                 'qv_curr, '//                                      &
                 'qc_curr, '//                                      &
                 'qi_curr, '//                                      &
                 'qni_curr, '//                                     &
                 'rqvblten, '//                                     &
                 'rqcblten, '//                                     &
                 'rqiblten, '//                                     &
                 'rqniblten, '//                                    &
                 'qke, '//                                          &
                 'tsq, '//                                          &
                 'qsq, '//                                          &
                 'cov, '//                                          &
                 'rmol, '//                                         &
                 'qcg, '//                                          &
                 'ch, '//                                           &
                 'grav_settling, '//                                &
                 'bl_mynn_tkebudget, '//                            &
                 'qke_adv, '//                                      &
                 'bl_mynn_tkeadvect, '//                            &
                 'vdfg = ' ,                                        &
                  PRESENT( qv_curr ) ,                              &
                  PRESENT( qc_curr ) ,                              &
                  PRESENT( qi_curr ) ,                              &
                  PRESENT( qni_curr ) ,                             &
                  PRESENT( rqvblten ) ,                             &
                  PRESENT( rqcblten ) ,                             &
                  PRESENT( rqiblten ) ,                             &
                  PRESENT( rqniblten ) ,                            &
                  PRESENT( qke      ) ,                             &
                  PRESENT( tsq      ) ,                             &
                  PRESENT( qsq      ) ,                             &
                  PRESENT( cov      ) ,                             &
                  PRESENT( rmol     ) ,                             &
                  PRESENT( qcg      ) ,                             &
                  PRESENT( ch       ) ,                             &
                  PRESENT( grav_settling),                          &
                  PRESENT( bl_mynn_tkebudget) ,                     &
                  PRESENT( qke_adv  ) ,                             &
                  PRESENT( bl_mynn_tkeadvect) ,                     &
                  PRESENT( vdfg )
               CALL wrf_debug(0,message)
              CALL wrf_error_fatal('Lack arguments to call MYNN pbl')
           ENDIF

           !fill scalar_tend array. The RQN*BLTEN arrays are no longer
           !passed to module_physics_addtendc.F (for MYNN)
           IF (bl_mynn_mixscalars .eq. 1) THEN
              IF (PRESENT( qni_curr ) .AND. Flag_qni) THEN
                !print*,"Updating qni after mynn-edmf",P_QNI
                DO j=j_start(ij),j_end(ij)
                DO k=kts,kte 
                DO i=i_start(ij),i_end(ij)
                   scalar_tend(i,k,j,P_QNI)=RQNIBLTEN(i,k,j)
                enddo
                enddo
                enddo
              ENDIF
              IF (PRESENT( qnc_curr ) .AND. Flag_qnc) THEN
                !print*,"Updating qnc after mynn-edmf",P_QNC
                DO j=j_start(ij),j_end(ij)
                DO k=kts,kte
                DO i=i_start(ij),i_end(ij)
                   scalar_tend(i,k,j,P_QNC)=RQNCBLTEN(i,k,j)
                enddo
                enddo
                enddo
              ENDIF
              IF (PRESENT( qnwfa_curr ) .AND. Flag_qnwfa) THEN
                !print*,"Updating qnwfa after mynn-edmf",P_QNWFA
                DO j=j_start(ij),j_end(ij)
                DO k=kts,kte
                DO i=i_start(ij),i_end(ij)
                   scalar_tend(i,k,j,P_QNWFA)=RQNWFABLTEN(i,k,j)
                enddo
                enddo
                enddo
              ENDIF
              IF (PRESENT( qnifa_curr ) .AND. Flag_qnifa) THEN
                !print*,"Updating qnifa after mynn-edmf",P_QNIFA
                DO j=j_start(ij),j_end(ij)
                DO k=kts,kte
                DO i=i_start(ij),i_end(ij)
                   scalar_tend(i,k,j,P_QNIFA)=RQNIFABLTEN(i,k,j)
                enddo
                enddo
                enddo
              ENDIF
           ENDIF

        CASE (BOULACSCHEME)

             CALL wrf_debug(100,'in boulac')

             CALL BOULAC(FRC_URB2D=frc_urb2d,IDIFF=idiff,FLAG_BEP=flag_bep     &
              ,DZ8W=dz8w,DT=dt,U_PHY=u_phytmp                                  &
              ,V_PHY=v_phytmp,TH_PHY=th_phy                                    &
              ,RHO=rho,QV_CURR=qv_curr,QC_CURR=qc_curr,HFX=hfx                 &
              ,QFX=qfx,USTAR=ust,CP=cp,G=g                                     &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten               &
              ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                            &
              ,TKE=tke_pbl,DLK=el_pbl,WU=wu_tur,WV=wv_tur,WT=wt_tur,WQ=wq_tur  &
              ,EXCH_H=exch_h,EXCH_M=exch_m,PBLH=pblh                           &
              ,A_U_BEP=a_u_bep,A_V_BEP=a_v_bep,A_T_BEP=a_t_bep                 &
              ,A_Q_BEP=a_q_bep                                                 &
              ,A_E_BEP=a_e_bep,B_U_BEP=b_u_bep,B_V_BEP=b_v_bep                 &
              ,B_T_BEP=b_t_bep,B_E_BEP=b_e_bep,DLG_BEP=dlg_bep                 &
              ,B_Q_BEP=b_q_bep                                                 &
              ,DL_U_BEP=dl_u_bep,SF_BEP=sf_bep,VL_BEP=vl_bep                   &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde                 &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme                 &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte  )

           CASE (CAMUWPBLSCHEME)
              
              CALL wrf_debug(100,'in camuwpbl')
              IF ( PRESENT( qv_curr  )  .AND. PRESENT( qc_curr   )  .AND. &
                   PRESENT( qi_curr  )  .AND. PRESENT( qnc_curr  )  .AND. &
                   PRESENT( qni_curr )  .AND.                             &
                   PRESENT( rqvblten )  .AND. PRESENT( rqcblten  )  .AND. &
                   PRESENT( rqiblten )  .AND. PRESENT( rqniblten )        &                   
                 )THEN
                 CALL CAMUWPBL(DT=dt,U_PHY=u_phy,V_PHY=v_phy,TH_PHY=th_phy,RHO=rho   &
                      ,QV_CURR=qv_curr,HFX=hfx,QFX=qfx,USTAR=ust,P8W=p8w,P_PHY=p_phy &
                      ,Z=z,T_PHY=t_phy,QC_CURR=qc_curr,QI_CURR=qi_curr,Z_AT_W=z_at_w &
                      ,CLDFRA_OLD_mp=cldfra_old_mp,CLDFRA=cldfra,HT=ht               &
                      ,RTHRATENLW=rthratenlw,EXNER=pi_phy                            &
                      ,is_CAMMGMP_used=is_CAMMGMP_used                               &
                      ,ITIMESTEP=itimestep,QNC_CURR=qnc_curr,QNI_CURR=qni_curr       &
                      ,WSEDL3D=wsedl3d                                               &

                      ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde             &
                      ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme             &
                      ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte             &
!Intent-inout 
                      ,TAURESX2D=tauresx2d,TAURESY2D=tauresy2d                       &
                      ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten             &
                      ,RQIBLTEN=rqiblten,RQNIBLTEN=rqniblten,RQVBLTEN=rqvblten       &
                      ,RQCBLTEN=rqcblten,KVM3D=exch_m,KVH3D=exch_h                   &
!Intent-out
                      ,TPERT2D=tpert2d,QPERT2D=qpert2d,WPERT2D=wpert2d,SMAW3D=smaw3d &
                      ,TURBTYPE3D=turbtype3d                                         &
                      ,TKE_pbl=tke_pbl,PBLH2D=pblh,KPBL2D=kpbl                       )
              ELSE
                 WRITE ( message , FMT = '(A,8(L1,1X))' )            &
                      'present: '//                                      &
                      'qv_curr, '//                                      &
                      'qc_curr, '//                                      &
                      'qi_curr, '//                                      &
                      'qnc_curr, '//                                     &
                      'qni_curr, '//                                     &
                      'rqvblten, '//                                     &
                      'rqcblten, '//                                     &
                      'rqiblten, '//                                     &
                      'rqniblten= ',                                     &
                      PRESENT( qv_curr ) ,                              &
                      PRESENT( qc_curr ) ,                              &
                      PRESENT( qi_curr ) ,                              &
                      PRESENT( qnc_curr ) ,                             &
                      PRESENT( qni_curr ) ,                             &
                      PRESENT( rqvblten ) ,                             &
                      PRESENT( rqcblten ) ,                             &
                      PRESENT( rqiblten ) ,                             &
                      PRESENT( rqniblten ) 

                 CALL wrf_debug(0,message)
                 CALL wrf_error_fatal('Lack arguments to call CAMUWPBL pbl')
              ENDIF
              
#endif

           CASE (GBMPBLSCHEME)
               CALL wrf_debug(100,'in gbmpbl') 
               IF ( PRESENT( qv_curr )  .AND. PRESENT( qc_curr )  .AND.&
               PRESENT( qi_curr )                            .AND. &
               PRESENT( rqvblten ) .AND. PRESENT( rqcblten ) .AND. &
               PRESENT( rqiblten )                           .AND. &
               PRESENT( hol      )                           .AND. &
                                       .TRUE.  ) THEN
             CALL gbmpbl(                                           &
               U3D=u_phytmp,V3D=v_phytmp,TH3D=th_phy,T3D=t_phy      &
              ,QV3D=qv_curr,QC3D=qc_curr,QI3D=qi_curr               &
              ,P3D=p_phy,PI3D=pi_phy                                &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,RTHBLTEN=rthblten,RQVBLTEN=rqvblten                  &
              ,RQCBLTEN=rqcblten,RQIBLTEN=rqiblten                  &
              ,KZM_GBM=exch_m,KTH_GBM=exch_h,KETHL_GBM=exch_tke     & 
              ,EL_GBM=el_pbl,DZ8W=dz8w,Z=z,PSFC=PSFC                &
              ,TKE_PBL=tke_pbl,RTHRATEN=rthraten                    & 
              ,ZNT=znt,UST=ust,ZOL=zol,HOL=hol,PBL=pblh             & 
              ,KPBL2D=kpbl,REGIME=regime                            & 
              ,PSIM=psim,PSIH=psih,XLAND=xland                      &
              ,HFX=hfx,QFX=qfx,TSK=tskold,GZ1OZ0=gz1oz0             &
              ,WSPD=wspd,BR=br,DT=dtbl,DTMIN=dtmin                  &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                    )
              ELSE
               CALL wrf_error_fatal('Lack arguments to call GBM pbl')
              ENDIF

#if ( WRFPLUS == 1 )
! this scheme is for WRFPlus only
!-----------------------------------
        CASE (SURFDRAGSCHEME)

           CALL wrf_debug(100,'in SURFDRAG scheme')

           CALL surface_drag(                                     &
             RUBLTEN=rublten,RVBLTEN=rvblten                      &  !
            ,U_PHY=u_phy, V_PHY=v_phy, Z=z                                &  !
            ,XLAND=xland, HT=ht, KPBL2D=kpbl                      &  !
            ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
            ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
            ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      &
                                                                  )
#endif

     CASE DEFAULT

       WRITE( message , * ) 'The pbl option does not exist: bl_pbl_physics = ', bl_pbl_physics
       CALL wrf_error_fatal ( message )

   END SELECT pbl_select

#if (EM_CORE==1)
!
! ... paj: wind farm ...
!
    windfarm_select: SELECT CASE(windfarm_opt)

      CASE (fitchscheme)

                  IF (PRESENT(id) .AND.                                  &
                     PRESENT(z_at_w) ) THEN
!
                     CALL wrf_debug(100,'in phys/module_wind_fitch.F')
                     CALL dragforce(                                  &
                     & ID=id                                             &
                     &,Z_AT_W=z_at_w,u=u_phy,v=v_phy                     &
                     &,DX=dx,DZ=dz8w,DT=dt                               &
                     &,QKE=qke                                           &
                     &,DU=rublten,DV=rvblten                             &
                     &,WINDFARM_OPT=windfarm_opt,POWER=power             &
                     &,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde   &
                     &,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme   &
                     &,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte   &
                     &)
                  ELSE
                     WRITE ( message , FMT = '(A,6(L1,1X))' )           &
                     'present: '//                                      &
                     'ID, '//                                           &
                     'z_at_w, '//                                       &
                     'xlat_u, '//                                       &
                     'xlong_u, '//                                      &
                     'xlat_v, '//                                       &
                     'xlong_v = ' ,                                     &
                      PRESENT( id ) ,                                   &
                      PRESENT( z_at_w ) 
                     CALL wrf_debug(0,message)
                     CALL wrf_error_fatal('Lack arguments to call turbine_drag')
                  ENDIF

   END SELECT windfarm_select
#endif


   
   IF (PRESENT(dtaux3d)) THEN
       IF(gwd_opt .EQ. 1)THEN
             CALL gwdo(                                              &
               U3D=u_phy,V3D=v_phy,T3D=t_phy                        &
              ,QV3D=qv_curr                                         &
              ,P3D=p_phy,P3DI=p8w,PI3D=pi_phy,Z=z                        &
              ,RUBLTEN=rublten,RVBLTEN=rvblten                      &
              ,DTAUX3D=dtaux3d,DTAUY3D=dtauy3d                      &
              ,DUSFCG=dusfcg,DVSFCG=dvsfcg &
              ,VAR2D=var2d,OC12D=oc12d     &
              ,OA2D1=oa1,OA2D2=oa2,OA2D3=oa3,OA2D4=oa4  &
              ,OL2D1=ol1,OL2D2=ol2,OL2D3=ol3,OL2D4=ol4  &
              ,SINA=sina,COSA=cosa                      &
              ,ZNU=znu,ZNW=znw,P_TOP=p_top                &
              ,CP=cp,G=g,RD=r_d                           &
              ,RV=r_v,EP1=ep_1,PI=3.141592653                        &
              ,DT=dtbl,DX=dx,KPBL2D=kpbl,ITIMESTEP=itimestep      &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde      &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme      &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte      )
       ENDIF
   ENDIF

#if (EM_CORE==1)                                                                                                                                                                             
!JOE-added - fog (cloud) water deposition calculation
   IF (grav_settling .GE. 1) THEN
      IF ( PRESENT(vdfg) .AND. PRESENT(qc_curr)) THEN
         !NOTE 1: vdfg is calculated in the surface driver.
         !NOTE 2: as of now, only qc_curr settles, not # conc.

          CALL bl_fogdes(                                        &
               vdfg,qc_curr,dtbl,rho,dz8w,grav_settling,RQCBLTEN,&
               ids,ide, jds,jde, kds,kde,                        &
               ims,ime, jms,jme, kms,kme,                        &
               i_start(ij),i_end(ij),                            &
               j_start(ij),j_end(ij),kts,kte                     )
      ELSE
          CALL wrf_error_fatal('Missing args for bl_fogdes in pbl driver')
      ENDIF
   ENDIF
!JOE-END
#endif

#if (NMM_CORE != 1)

     IF (idiff.eq.1) THEN

!Alberto: here we call the general routine to solve the diffusion
! + all the source/sink terms.
! the only thing that should be passed from the PBL schemes is the value of the exch_h, and exch_m 
! (and the non local terms, if present, through the b).
! So, this routine can be used by any of the previous schemes. We only need to pass the right variables
! (and eliminate the computation of the tendencies from the previous routines, to avoid doing twice the job).
! As I explain below, in the routine, here we could extract the vertical turbulent 
! fluxes (something that will be general for all the routines).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
            
          CALL diff3d  (DT=dtbl,CP=cp,DZ=dz8w,TH=th_phy,QV=qv_curr,QC=qc_curr,T=t_phy  &
              ,U=u_phy,V=v_phy,RHO=rho,EXCH_H=exch_h                  &
              ,EXCH_M=exch_m                                          &
              ,RUBLTEN=rublten,RVBLTEN=rvblten,RTHBLTEN=rthblten      &
             ,RQVBLTEN=rqvblten,RQCBLTEN=rqcblten                    &
              ,WU=wu_tur,WV=wv_tur,WT=wt_tur,WQ=wq_tur                    &
              ,A_U=a_u,A_V=a_v,A_T=a_t,A_Q=a_q        &
              ,B_U=b_u,B_V=b_v,B_T=b_t,B_Q=b_q        &
              ,SF=sf,VL=vl            &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)

          DEALLOCATE (a_u)       ! Implicit component for the momemtum in X-direction
          DEALLOCATE (a_v)       ! Implicit component for the momemtum in Y-direction
          DEALLOCATE (a_t)       ! Implicit component for the Pot. Temp.
          DEALLOCATE (a_q)       ! Implicit component for the water vapor
          DEALLOCATE (b_u)       ! Explicit component for the momemtum in X-direction
          DEALLOCATE (b_v)       ! Explicit component for the momemtum in Y-direction
          DEALLOCATE (b_t)       ! Explicit component for the Pot. Temp.
          DEALLOCATE (b_q)       ! Explicit component for the water vapor
          DEALLOCATE (sf )       ! surfaces
          DEALLOCATE (vl )       ! volumes
     ENDIF       !idiff
      
          IF(scalar_pblmix .GT. 0)THEN
            CALL diff4d  (DT=dtbl,DZ=dz8w, SCALAR=scalar, is_scalar=.true.  &
              ,RHO=rho,EXCH_H=exch_h        &
              ,EXCH_M=exch_m                &
              ,SCALAR_TEND=scalar_tend      &
              ,NUM_SCALAR=num_scalar, PARAM_FIRST_SCALAR=param_first_scalar &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)
          ENDIF
          IF(tracer_pblmix .GT. 0)THEN
            CALL diff4d  (DT=dtbl,DZ=dz8w, SCALAR=tracer, is_scalar=.false.  &
              ,RHO=rho,EXCH_H=exch_h        &
              ,EXCH_M=exch_m                &
              ,SCALAR_TEND=tracer_tend      &
              ,NUM_SCALAR=num_tracer, PARAM_FIRST_SCALAR=param_first_scalar &
              ,IDS=ids,IDE=ide,JDS=jds,JDE=jde,KDS=kds,KDE=kde        &
              ,IMS=ims,IME=ime,JMS=jms,JME=jme,KMS=kms,KME=kme        &
              ,ITS=its,ITE=ite,JTS=jts,JTE=jte,KTS=kts,KTE=kte)
          ENDIF

#endif
      
   ENDDO
   !$OMP END PARALLEL DO

   ENDIF

   END SUBROUTINE pbl_driver

!=============================================================================
          SUBROUTINE diff3d(DT,CP,DZ,TH ,QV,QC,T,U,V,RHO                              &
              ,EXCH_H,EXCH_M                   &  
              ,RUBLTEN,RVBLTEN,RTHBLTEN    &
              ,RQVBLTEN,RQCBLTEN                  &
              ,WU,WV,WT,WQ                 &
              ,A_U,A_V,A_T,A_Q      &
              ,B_U,B_V,B_T,B_Q      &
              ,SF,VL        &
              ,IDS,IDE,JDS,JDE,KDS,KDE      &
              ,IMS,IME,JMS,JME,KMS,KME      &
              ,ITS,ITE,JTS,JTE,KTS,KTE      &
                                                                    )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Subroutine written by Alberto Martilli, CIEMAT, Spain,
!    e-mail:alberto_martilli@ciemat.es
!    August 2008.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ALberto
! This routine solves the vertical diffusion 
! + other source/sink terms (surface fluxes, building drag, non local terms, etc.)
! for U,V, potential temperature and moisture. The equation should be written 
! as follow, for a generic variable M:
! 
!      dM      1     d      dM
!     ---- =----  ---(rho*K----)+AM+B
!      dt     rho    dz     dz  
! where A and B are the implict and explcit coefficients of the source/sink terms
! (at the lowest level the surface fluxes should go in these terms)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------

      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

! INputs
      real DT,CP
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ ! Depth of vertical levels
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: TH ! Potential Temperature
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QV ! water vapor mixing ratio (kg/kg)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: QC ! cloud water mixing ratio (kg/kg)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: T  ! temperature
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: U ! X-compnent of the wind
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: V ! Y-compnent of the wind
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: RHO ! Air Density
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_H ! Turbulent coefficient for heat
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_M ! Turbulent coefficient for momentum 
      
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_U ! Implicit coefficient for DRAG (x -component)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_U ! Explicit coefficient for DRAG (x -component)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_V ! Implicit coefficient for DRAG (y -component)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_V ! Explicit coefficient for DRAG (y -component)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_T ! Implicit coefficient for Heat flux from buildings
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_T ! Explicit coefficient for Heat flux from buildings
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: A_Q ! Implicit coefficient for moisture flux
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: B_Q ! Explicit coefficient for moisture flux 
    
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: VL ! fraction of air volume in the grid cell
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: SF ! fraction of air at the face of the grid cell
!outputs
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RUBLTEN ! tendency for x-wind component
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RVBLTEN ! tendency for y-wind component
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RTHBLTEN ! tendency for Potential Temperature
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQVBLTEN ! tendency for water vapor mixing ratio (kg/kg)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: RQCBLTEN ! tendency for cloud water mixing ratio (kg/kg)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WU ! turbulent flux of momentum (x)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WV ! turbulent flux of momentum (y)
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WT ! turbulent flux of potential temperature
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(OUT) :: WQ ! turbulent flux of water vapor
! Parameters
     REAL ELOCP 
! locals (same meaning as above, but 1D)

      real u1d(kms:kme),v1d(kms:kme),exch_h1d(kms:kme)
      real the1d(kms:kme) ! Equivalent potential temperature
      real exch_m1d(kms:kme),qv1d(kms:kme),qc1d(kms:kme)
      real dz1d(kms:kme),rho1d(kms:kme),rhoz1d(kms:kme)
      real sf1d(kms:kme),vl1d(kms:kme)   
      real a_u1d(kms:kme),b_u1d(kms:kme)
      real a_v1d(kms:kme),b_v1d(kms:kme)
      real a_t1d(kms:kme),b_t1d(kms:kme)
      real a_q1d(kms:kme),b_q1d(kms:kme)
      real a_qc1d(kms:kme),b_qc1d(kms:kme)
      real wu1d(kms:kme),wv1d(kms:kme),wt1d(kms:kme),wq1d(kms:kme),wqc1d(kms:kme)
      real thnew
!
      integer i,k,j  
!----------------------------------------------------------------------
      ELOCP=2.72E6/CP
      u1d=0.
      v1d=0.
      exch_h1d=0.
      exch_m1d=0.
      qv1d=0.
      qc1d=0.
      dz1d=0.
      rho1d=0.
      rhoz1d=0.
      sf1d=0.
      vl1d=0.
      a_u1d=0.
      a_v1d=0.
      a_t1d=0.
      a_q1d=0.
      a_qc1d=0.
      b_u1d=0.
      b_v1d=0.
      b_t1d=0.
      b_q1d=0.
      b_qc1d=0.
       
      do j=jts,jte
      do i=its,ite

! put three D variables in temporary 1D variables

       do k=kts,kte
        u1d(k)=U(i,k,j)
        v1d(k)=V(i,k,j)
        the1d(k)=TH(i,k,j)*(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
        qv1d(k)=qv(i,k,j)
        dz1d(k)=dz(i,k,j)
        rho1d(k)=rho(i,k,j) 
        a_u1d(k)=a_u(i,k,j)
        b_u1d(k)=b_u(i,k,j)
        a_v1d(k)=a_v(i,k,j)
        b_v1d(k)=b_v(i,k,j)
        a_t1d(k)=a_t(i,k,j)
        b_t1d(k)=b_t(i,k,j)
        a_q1d(k)=a_q(i,k,j)
        b_q1d(k)=b_q(i,k,j)
        a_qc1d(k)=0.
        b_qc1d(k)=0.
        vl1d(k)=vl(i,k,j)
        sf1d(k)=sf(i,k,j)
       enddo
       sf1d(kte+1)=1. 
       do k=kts,kte    
        exch_h1d(k)=exch_h(i,k,j)
        exch_m1d(k)=exch_m(i,k,j)
       enddo
       exch_h1d(kts)=0.
!       exch_h1d(kte+1)=0  
       exch_m1d(kts)=0.
!       exch_m1d(kte+1)=0
        rhoz1d(kts)=rho1d(kts)
        do k=kts+1,kte
         rhoz1d(k)=(rho1d(k)*dz1d(k-1)+rho1d(k-1)*dz1d(k))/       &
     &                      (dz1d(k-1)+dz1d(k))
        enddo
        rhoz1d(kte+1)=rho1d(kte)


! solve the diffusion for x-component of the wind   
          
       call diff(kms,kme,kts,kte,dt,u1d,rho1d,rhoz1d,exch_m1d,a_u1d,b_u1d,sf1d, &
     &            vl1d,dz1d,wu1d) 

! solve the diffusion for y-component of the wind              

       call diff(kms,kme,kts,kte,dt,v1d,rho1d,rhoz1d,exch_m1d,a_v1d,b_v1d,sf1d, &
     &            vl1d,dz1d,wv1d) 

! solve the diffusion for equivalent potential temperature

       call diff(kms,kme,kts,kte,dt,the1d,rho1d,rhoz1d,exch_h1d,a_t1d,b_t1d,sf1d, &
     &            vl1d,dz1d,wt1d) 

! solve the diffusion for the water vapor mixing ratio

       call diff(kms,kme,kts,kte,dt,qv1d,rho1d,rhoz1d,exch_h1d,a_q1d,b_q1d,sf1d, &
     &            vl1d,dz1d,wq1d) 

! solve the diffusion for the cloud water mixing ratio

       call diff(kms,kme,kts,kte,dt,qc1d,rho1d,rhoz1d,exch_h1d,a_qc1d,b_qc1d,sf1d, &
     &            vl1d,dz1d,wqc1d)        

!     
! compute the tendencies

        do k=kts,kte
          rublten(i,k,j)=(u1d(k)-u(i,k,j))/dt
          rvblten(i,k,j)=(v1d(k)-v(i,k,j))/dt
          thnew=the1d(k)/(QC(i,k,j)*(-ELOCP/T(i,k,j))+1)
          rthblten(i,k,j)=(thnew-th(i,k,j))/dt
          rqvblten(i,k,j)=(qv1d(k)-qv(i,k,j))/dt
          rqcblten(i,k,j)=(qc1d(k)-qc(i,k,j))/dt
          wu(i,k,j)=wu1d(k)
          wv(i,k,j)=wv1d(k)
          wt(i,k,j)=wt1d(k)
          wq(i,k,j)=wq1d(k)
        enddo
      enddo
      enddo 
!!!!!!!!!!!!

        
      END SUBROUTINE diff3d
#if (EM_CORE==1)
! ===6=8===============================================================72
          SUBROUTINE diff4d(DT,DZ,SCALAR,IS_SCALAR,RHO              &
              ,EXCH_H,EXCH_M  &  
              ,SCALAR_TEND    &
              ,NUM_SCALAR, PARAM_FIRST_SCALAR     &
              ,IDS,IDE,JDS,JDE,KDS,KDE      &
              ,IMS,IME,JMS,JME,KMS,KME      &
              ,ITS,ITE,JTS,JTE,KTS,KTE      &
                                                                    )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Based on subroutine written by Alberto Martilli, CIEMAT, Spain,
!    e-mail:alberto_martilli@ciemat.es
!    August 2008.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ALberto
! This routine solves the vertical diffusion 
! + other source/sink terms (surface fluxes, building drag, non local terms, etc.)
! for U,V, potential temperature and moisture. The equation should be written 
! as follow, for a generic variable M:
! 
!      dM      1     d      dM
!     ---- =----  ---(rho*K----)+AM+B
!      dt     rho    dz     dz  
! where A and B are the implict and explcit coefficients of the source/sink terms
! (at the lowest level the surface fluxes should go in these terms)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!-----------------------------------------------------------------------
      USE module_state_description, ONLY: &
                  P_QNS, P_QNR, P_QNG, P_QT, P_QNH, P_QVOLG, P_QKE_ADV


      IMPLICIT NONE
!
!----------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NUM_SCALAR, PARAM_FIRST_SCALAR
      INTEGER,INTENT(IN) :: IDS,IDE,JDS,JDE,KDS,KDE                    &
     &                     ,IMS,IME,JMS,JME,KMS,KME                    &
     &                     ,ITS,ITE,JTS,JTE,KTS,KTE

! inputs
      REAL, INTENT(IN)   :: DT
      LOGICAL,INTENT(IN) :: IS_SCALAR
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: DZ ! Depth of vertical levels
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,NUM_SCALAR),INTENT(IN) :: SCALAR ! 4d scalar mixing ratio
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: RHO ! Air Density
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_H ! Turbulent coefficient for heat
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME),INTENT(IN) :: EXCH_M ! Turbulent coefficient for momentum 
      
! outputs
      REAL,DIMENSION(IMS:IME,KMS:KME,JMS:JME,NUM_SCALAR),INTENT(INOUT) :: SCALAR_TEND ! 4d scalar mixing ratio tendency
! locals (same meaning as above, but 1D)

      real s1d(kms:kme),exch_h1d(kms:kme)
      real exch_m1d(kms:kme)
      real dz1d(kms:kme),rho1d(kms:kme),rhoz1d(kms:kme)
      real sf1d(kms:kme),vl1d(kms:kme)   
      real a_s1d(kms:kme),b_s1d(kms:kme)
      real ws1d(kms:kme)
!
      integer i,k,j,im  
!----------------------------------------------------------------------
      s1d=0.
      exch_h1d=0.
      exch_m1d=0.
      rho1d=0.
      rhoz1d=0.
      sf1d=0.
      vl1d=0.
      a_s1d=0.
      b_s1d=0.
       
      DO im=PARAM_FIRST_SCALAR,NUM_SCALAR
! need to avoid mixing scalars associated with precipitating species (e.g. Nr)
        IF((IS_SCALAR .AND. im.NE.P_QNS .AND. im.NE.P_QNR .AND. im.NE.P_QNG &
          .AND. im.NE.P_QNH .AND. im.NE.P_QT .AND. im.NE.P_QVOLG .AND. im.NE.P_QKE_ADV) &
          .OR. (.not. IS_SCALAR)) THEN
        do j=jts,jte
        do i=its,ite

! put three D variables in temporary 1D variables

       do k=kts,kte
        s1d(k)=SCALAR(i,k,j,im)
        dz1d(k)=dz(i,k,j)
        rho1d(k)=rho(i,k,j) 
!        a_s1d(k)=a_s(i,k,j)   ! implicit source
!        b_s1d(k)=b_s(i,k,j)   ! explicit source
        vl1d(k)=1.
        sf1d(k)=1.
       enddo
       sf1d(kte+1)=1. 
       do k=kts,kte    
        exch_h1d(k)=exch_h(i,k,j)
        exch_m1d(k)=exch_m(i,k,j)
       enddo
       exch_h1d(kts)=0.
!       exch_h1d(kte+1)=0  
       exch_m1d(kts)=0.
!       exch_m1d(kte+1)=0
        rhoz1d(kts)=rho1d(kts)
        do k=kts+1,kte
         rhoz1d(k)=(rho1d(k)*dz1d(k-1)+rho1d(k-1)*dz1d(k))/       &
     &                      (dz1d(k-1)+dz1d(k))
        enddo
        rhoz1d(kte+1)=rho1d(kte)


! solve the diffusion for scalar
          
       call diff(kms,kme,kts,kte,dt,s1d,rho1d,rhoz1d,exch_h1d,a_s1d,b_s1d,sf1d, &
     &            vl1d,dz1d,ws1d) 

!     
! compute the tendencies

        do k=kts,kte
          scalar_tend(i,k,j,im)=(s1d(k)-scalar(i,k,j,im))/dt
!          ws(i,k,j)=ws1d(k)   ! output fluxes
        enddo
        enddo
        enddo 
      ELSE
!      print *,'scalar skipped im=',im, is_scalar
      ENDIF
      
      ENDDO ! im loop
!!!!!!!!!!!!

        
      END SUBROUTINE diff4d
! ===6=8===============================================================72
#endif

       subroutine diff(kms,kme,kts,kte,dt,co,da,daz,cd,aa,bb,sf,vl,dz,fc)

!------------------------------------------------------------------------
!           Calculation of the diffusion in 1D
!------------------------------------------------------------------------
!  - Input:
!       nz    : number of points
!       iz1   : first calculated point
!       co    : concentration of the variable of interest
!       dz    : vertical levels
!       cd    : diffusion coefficients
!       da    : density of the air at the center
!       daz   : density of the air at the face
!       dt    : diffusion time step
!  - Output:
!       co :concentration of the variable of interest

!  - Internal:
!       cddz  : constant terms in the equations
!---------------------------------------------------------------------

        implicit none
        integer iz,iz1,izf
        integer kms,kme,kts,kte
        real dt,dzv
        real co(kms:kme),cd(kms:kme),dz(kms:kme)
        real da(kms:kme),daz(kms:kme)
        real cddz(kms:kme),fc(kms:kme),df(kms:kme)
        real a(kms:kme,3),c(kms:kme)
        real sf(kms:kme),vl(kms:kme)
        real aa(kms:kme),bb(kms:kme)

        
! Compute cddz=2*cd/dz

        cddz(kts)=sf(kts)*daz(kts)*cd(kts)/dz(kts)
        do iz=kts+1,kte
         cddz(iz)=2.*sf(iz)*daz(iz)*cd(iz)/(dz(iz)+dz(iz-1))
        enddo
        cddz(kte+1)=sf(kte+1)*daz(kte+1)*cd(kte+1)/dz(kte)

          iz1=1
          izf=1

          do iz=iz1,kte-1

           dzv=vl(iz)*dz(iz)
           a(iz,1)=-cddz(iz)*dt/dzv/da(iz)
           a(iz,2)=1+dt*(cddz(iz)+cddz(iz+1))/dzv/da(iz)-aa(iz)*dt
           a(iz,3)=-cddz(iz+1)*dt/dzv/da(iz)
           c(iz)=co(iz)+bb(iz)*dt
          enddo

          do iz=kte-(izf-1),kte
           a(iz,1)=0.
           a(iz,2)=1
           a(iz,3)=0.
           c(iz)=co(iz)
          enddo
          call invert (kms,kme,kts,kte,a,c,co)
           
!let compute the fluxes, as diagnostic variable

          do iz=kts,iz1
           fc(iz)=0.
          enddo

          do iz=iz1+1,kte
           fc(iz)=-(cddz(iz)*(co(iz)-co(iz-1)))/da(iz)
          enddo



       return
       end subroutine diff
! ===6=8===============================================================72

       subroutine invert(kms,kme,kts,kte,a,c,x)

!ccccccccccccccccccccccccccccccc
! Aim: Inversion and resolution of a tridiagonal matrix
!          A X = C
! Input:
!  a(*,1) lower diagonal (Ai,i-1)
!  a(*,2) principal diagonal (Ai,i)
!  a(*,3) upper diagonal (Ai,i+1)
!  c
! Output
!  x     results
!ccccccccccccccccccccccccccccccc

       implicit none
       integer kms,kme,kts,kte,in
       real a(kms:kme,3),c(kms:kme),x(kms:kme)

        do in=kte-1,kts,-1
         c(in)=c(in)-a(in,3)*c(in+1)/a(in+1,2)
         a(in,2)=a(in,2)-a(in,3)*a(in+1,1)/a(in+1,2)
        enddo

        do in=kts+1,kte
         c(in)=c(in)-a(in,1)*c(in-1)/a(in-1,2)
        enddo

        do in=kts,kte
         x(in)=c(in)/a(in,2)
        enddo

        return
        end subroutine invert

! ===6=8===============================================================72













END MODULE module_pbl_driver
