!WRF:MEDIATION_LAYER:PHYSICS
! *** add new modules of schemes here
!
MODULE module_microphysics_driver
CONTAINS

SUBROUTINE microphysics_driver(                                          &
                       th, rho, pi_phy, p                                &
                      ,ht, dz8w, p8w, dt,dx,dy                           &
                      ,mp_physics, spec_zone                             &
                      ,specified, channel_switch                         &
                      ,warm_rain                                         &
                      ,t8w                                               &
                      ,chem_opt, progn                                   &
                      ,cldfra, cldfra_old, exch_h, nsource               &
                      ,qlsink, precr, preci, precs, precg                &
                      ,xland,snowh,itimestep                             &
                      ,f_ice_phy,f_rain_phy,f_rimef_phy                  &
                      ,lowlyr,sr, id                                     &
                      ,ids,ide, jds,jde, kds,kde                         &
                      ,ims,ime, jms,jme, kms,kme                         &
                      ,ips,ipe, jps,jpe, kps,kpe                         &
                      ,i_start,i_end,j_start,j_end,kts,kte               &
                      ,num_tiles, naer                                   &
                      ,irrigation,sf_surf_irr_scheme, irr_daily_amount   &   
                      ,irr_start_hour,irr_num_hours,julian_in            &
                      ,irr_start_julianday,irr_end_julianday             &
                      ,irr_freq,irr_ph,irr_rand_field                    &
                      ,gmt,xtime                                         &
!======================
                      !Variables required for CAMMGMP Scheme
                      ,dlf,dlf2,t_phy,p_hyd,p8w_hyd,tke_pbl,z_at_w,qfx   &
                      ,rliq,turbtype3d,smaw3d,wsedl3d,cldfra_old_mp      &
                      ,cldfra_mp,cldfra_mp_all,lradius,iradius           &
                      ,cldfrai,cldfral,cldfra_conv                       &
                      ,alt                                               &
                      ,accum_mode,aitken_mode,coarse_mode                &
                      ,icwmrsh3d,icwmrdp3d,shfrc3d,cmfmc3d,cmfmc2_3d     &
                      ,config_flags,fnm,fnp,rh_old_mp,lcd_old_mp         &
#if ( WRF_CHEM == 1 )
                      ,chem                                              &! For CAMMGMP scheme Prognostic aerosols
                      ,qme3d,prain3d,nevapr3d,rate1ord_cw2pr_st3d        &
                      ,dgnum4D,dgnumwet4D                                &
#endif
!======================                                   
                      ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr   &
                      ,qic_curr,qip_curr,qid_curr &
                      ,qnic_curr,qnip_curr,qnid_curr &
                      ,qndrop_curr,qni_curr,qh_curr,qnh_curr             &
                      ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr      &
                      ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr      &
                      ,qnwfa_curr,qnifa_curr                             & ! for water/ice-friendly aerosols
                      ,f_qnwfa,f_qnifa                                   & ! for water/ice-friendly aerosols
                      ,qvolg_curr,qvolh_curr                             &
                      ,qir_curr,qib_curr                                 & ! for P3
                      ,qi2_curr,qni2_curr,qir2_curr,qib2_curr            & ! for P3
                      ,qvoli_curr,qaoli_curr                             & ! for Jensen ISHMAEL
                      ,qvoli2_curr,qaoli2_curr                           & ! for Jensen ISHMAEL
                      ,qi3_curr,qni3_curr,qvoli3_curr,qaoli3_curr        & ! for Jensen ISHMAEL
                      ,effr_curr,ice_effr_curr,tot_effr_curr             &
                      ,qic_effr_curr,qip_effr_curr,qid_effr_curr         &             
                      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni      &
                      ,f_qns,f_qnr,f_qng,f_qnc,f_qnn,f_qh,f_qnh          &
                      ,            f_qzr,f_qzi,f_qzs,f_qzg,f_qzh         &
                      ,f_qvolg,f_qvolh                                   &
                      ,f_qic,f_qip,f_qid &
                      ,f_qnic,f_qnip,f_qnid &
                      ,f_qir,f_qib                                       & ! for P3
                      ,f_qi2,f_qni2,f_qir2,f_qib2                        & ! for P3
                      ,f_qvoli,f_qaoli                                   & ! for Jensen ISHMAEL
                      ,f_qvoli2,f_qaoli2                                 & ! for Jensen ISHMAEL
                      ,f_qi3,f_qni3,f_qvoli3,f_qaoli3                    & ! for Jensen ISHMAEL
                      ,f_effr,f_ice_effr,f_tot_effr                      &
                      ,f_qic_effr,f_qip_effr,f_qid_effr                  &                 
                      ,cu_used                                           &
                      ,qrcuten, qscuten, qicuten, qccuten                &
                      ,qt_curr,f_qt                                      &
                      ,mp_restart_state,tbpvs_state,tbpvs0_state         & ! for etampnew or fer_mp_hires
                      ,hail,ice2                                         & ! for mp_gsfcgce
!NUWRF JJS 20110525 vvvvv
                      ,phys_tot, physc, physe, physd, physs, physm, physf& ! for gsfcgce
                      ,acphys_tot, acphysc, acphyse, acphysd  & ! for gsfcgce
                      ,acphyss, acphysm, acphysf              & ! for gsfcgce

                      ,re_cloud_gsfc, re_rain_gsfc, re_ice_gsfc          &
                      ,re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc       & ! cloud effective radius
                      ,precr3d, preci3d, precs3d, precg3d, prech3d       &
#if ( WRF_CHEM == 1 )
                      ,icn_diag, nc_diag                           &  ! inline gocart
                      ,gsfcgce_gocart_coupling                           &
#endif
!NUWRF JJS 20110525 ^^^^^
!                     ,ccntype                                           & ! for mp_milbrandt2mom
                      ,u,v,w,z                                          &   
                      ,rainnc,    rainncv                                &
                      ,snownc,    snowncv                                &
                      ,hailnc,    hailncv                                &
                      ,graupelnc, graupelncv                             &
#if ( WRF_CHEM == 1 )
                      ,rainprod, evapprod                                &
                      ,qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp                &
#endif
                      ,qnwfa2d, qnifa2d                                  & ! for water/ice-friendly aerosols
                      ,refl_10cm                                         & ! HM, 9/22/09, add for refl
                      ,vmi3d                                             & ! for P3 
                      ,di3d                                              & ! for P3 
                      ,rhopo3d                                           & ! for P3 
                      ,phii3d                                            & ! for Jensen ISHMAEL
                      ,vmi3d_2                                           & ! for P3 
                      ,di3d_2                                            & ! for P3 
                      ,rhopo3d_2                                         & ! for P3 
                      ,phii3d_2                                          & ! for Jensen ISHMAEL
                      ,vmi3d_3                                           & ! for Jensen ISHMAEL
                      ,di3d_3                                            & ! for Jensen ISHMAEL
                      ,rhopo3d_3                                         & ! for Jensen ISHMAEL
                      ,phii3d_3                                          & ! for Jensen ISHMAEL
                      ,itype                                             & ! for Jensen ISHMAEL
                      ,itype_2                                           & ! for Jensen ISHMAEL
                      ,itype_3                                           & ! for Jensen ISHMAEL

! YLIN
! Added the RI_CURR array to the call
                      ,ri_curr                                           &
                      ,diagflag,   do_radar_ref                          &
                      ,ke_diag                                           &
                      ,re_cloud, re_ice, re_snow                         & ! G. Thompson
                      ,has_reqc, has_reqi, has_reqs                      & ! G. Thompson
                      ,ccn_conc                                          & ! RAS
                      ,scalar,num_scalar                                   &
                      ,kext_ql,kext_qs,kext_qg            &
                      ,kext_qh,kext_qa                         &
                      ,kext_qic,kext_qid,kext_qip         &
                      ,kext_ft_qic,kext_ft_qid,kext_ft_qip         &
                      ,kext_ft_qs,kext_ft_qg            &
                      ,height,tempc &
                      ,TH_OLD                                            &
                      ,QV_OLD                                            &
                      ,xlat,xlong,ivgtyp                                 &
                      ,qrimef_curr,f_qrimef                              &
                      ,aercu_opt                                         &
# if( EM_CORE==1 )
                      ,sbmradar,num_sbmradar                             &
                      ,sbm_diagnostics                                   &
                      ,aerocu,aercu_fct,no_src_types_cu                  &
                      ,PBL,EFCG,EFIG,EFSG,WACT,CCN1_GS,CCN2_GS           &
                      ,CCN3_GS,CCN4_GS,CCN5_GS,CCN6_GS,CCN7_GS           &
                      ,NR_CU,QR_CU,NS_CU,QS_CU,CU_UAF,mskf_refl_10cm     &
# endif
                                                                         )

! Framework
#if(NMM_CORE==1)
   USE module_state_description, ONLY :                                  &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME    &
                    ,WSM6SCHEME, ETAMPNEW, FER_MP_HIRES, etamp_HWRF,THOMPSON, THOMPSONAERO, MORR_TWO_MOMENT     &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, NSSL_2MOM, NSSL_2MOMCCN, NSSL_2MOMG  &
                    ,NSSL_1MOM,NSSL_1MOMLFO, FER_MP_HIRES_ADVECT &
                    ,WSM7SCHEME, WDM7SCHEME &
                    ,NUWRF4ICESCHEME & 
                    ,MILBRANDT2MOM, P3_1CATEGORY, P3_1CATEGORY_NC, JENSEN_ISHMAEL !, P3_2CATEGORY ,MILBRANDT3MOM 
#else
   USE module_state_description, ONLY :                                  &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME    &
                    ,WSM6SCHEME, ETAMPNEW, FER_MP_HIRES, THOMPSON, THOMPSONAERO, FAST_KHAIN_LYNN_SHPUND, MORR_TWO_MOMENT     &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, NSSL_2MOM, NSSL_2MOMCCN, NSSL_2MOMG  &
                    ,NSSL_1MOM,NSSL_1MOMLFO, FER_MP_HIRES_ADVECT & ! ,NSSL_3MOM       &
                    ,WSM7SCHEME, WDM7SCHEME &
                    ,NUWRF4ICESCHEME &
                    ,MILBRANDT2MOM , CAMMGMPSCHEME,FULL_KHAIN_LYNN, P3_1CATEGORY, P3_1CATEGORY_NC, P3_2CATEGORY, MORR_TM_AERO, JENSEN_ISHMAEL,SPRINKLER !,MILBRANDT3MOM
#if ( WRFPLUS == 1 )
   USE module_state_description, ONLY : LSCONDSCHEME, MKESSLERSCHEME
#endif
#endif
#if ( WRF_CHEM == 1 )
! NUWRF
   USE module_state_description, ONLY :                                 &
                     num_chem                                           & !inline gocart
                    ,p_bc1, p_bc2, p_oc1, p_oc2                         & !inline gocart
                    ,p_dust_1, p_dust_2, p_dust_3                       & !inline gocart
                    ,p_dust_4, p_dust_5                                 & !inline gocart
                    ,p_sulf, p_seas_1, p_seas_2                         & !inline gocart
                    ,p_seas_3, p_seas_4 

#endif

#ifdef DM_PARALLEL
  USE module_dm, ONLY : &
                 local_communicator, mytask,  wrf_dm_min_real, wrf_dm_max_real
#endif

! Model Layer
   USE module_model_constants
   USE module_wrf_error
   USE module_configure, only: grid_config_rec_type
#if ( WRF_CHEM == 1 )   
!mchen   USE module_state_description, only: num_scalar               ! For CAMMGMP scheme Prognostic aerosols
   USE module_state_description, only: num_chem               ! mchen 
   USE modal_aero_data, only:  ntot_amode_cam_mam => ntot_amode ! For CAMMGMP scheme Prognostic aerosols
#endif

! *** add new modules of schemes here

   USE module_mp_kessler
#if ( WRFPLUS == 1 )
   USE module_mp_mkessler
   USE module_mp_nconvp    !  added by Zhuxiao
#endif
   USE module_mp_lin
   USE module_mp_sbu_ylin
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_wsm7
   USE module_mp_etanew
   USE module_mp_fer_hires
   USE module_mp_thompson
   USE module_mp_full_sbm
#if ( BUILD_SBM_FAST == 1 )
   USE module_mp_fast_sbm
#endif
   USE module_mp_gsfcgce
   USE module_mp_gsfcgce_4ice_nuwrf, only: gsfcgce_4ice_nuwrf
   USE module_mp_morr_two_moment
   USE module_mp_p3
   USE module_mp_jensen_ishmael
# if (EM_CORE == 1)
   USE module_mp_morr_two_moment_aero
# endif
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_wdm7
   USE module_mp_milbrandt2mom
# if (EM_CORE == 1)
   USE module_mp_cammgmp_driver, ONLY: CAMMGMP ! CAM5's microphysics driver
# endif
!  USE module_mp_milbrandt3mom
   USE module_mp_nssl_2mom

   USE module_mp_HWRF
   USE module_mixactivate, only: prescribe_aerosol_mixactivate

! For checking model timestep is history time (for radar reflectivity)
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep
   USE module_irrigation

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
   !   * etampnew - what's in the operational 4-km High-Resolution Window Runs
   ! Milbrandt and Yau (2005)

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
! Qv         water vapor    mixing ratio (kg/kg)
! Qc         cloud water    mixing ratio (kg/kg)
! Qr         rain water     mixing ratio (kg/kg)
! Qi         cloud ice      mixing ratio (kg/kg)
! Qs         snow           mixing ratio (kg/kg)
! Qg         graupel        mixing ratio (kg/kg)
! Qh         hail           mixing ratio (kg/kg)
! Qndrop     droplet number mixing ratio (#/kg)
! Qni        cloud ice number concentration (#/kg)
! Qns        snow      number concentration (#/kg)
! Qnr        rain      number concentration (#/kg)
! Qng        graupel   number concentration (#/kg)
! Qnh        hail      number concentration (#/kg)

! Qzr        rain             reflectivity (m6/kg)
! Qzi        ice              reflectivity (m6/kg)
! Qzs        snow             reflectivity (m6/kg)
! Qzg        graupel          reflectivity (m6/kg)
! Qzh        hail             reflectivity (m6/kg)

! Qvolg        graupel   particle volume (m3/kg)

!
!----------------------------------------------------------------------
!-- th        potential temperature    (K)
!-- moist_new     updated moisture array   (kg/kg)
!-- moist_old     Old moisture array       (kg/kg)
!-- rho           density of air           (kg/m^3)
!-- pi_phy        exner function           (dimensionless)
!-- p             pressure                 (Pa)
!-- RAINNC        grid scale precipitation (mm)
!-- RAINNCV       one time step grid scale precipitation (mm/step)
!-- SNOWNC        grid scale snow and ice (mm)
!-- SNOWNCV       one time step grid scale snow and ice (mm/step)
!-- GRAUPELNC     grid scale graupel (mm)
!-- GRAUPELNCV    one time step grid scale graupel (mm/step)
!-- HAILNC        grid scale hail (mm)
!-- HAILNCV       one time step grid scale hail (mm/step)
!-- SR            one time step mass ratio of snow to total precip
!-- z             Height above sea level   (m)
!-- dt            Time step              (s)
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
!-- t8w           temperature at layer interfaces
!-- cldfra, cldfra_old, current, previous cloud fraction
!-- exch_h        vertical diffusivity (m2/s)
!-- qlsink        Fractional cloud water sink (/s)
!-- precr         rain precipitation rate at all levels (kg/m2/s)
!-- preci         ice precipitation rate at all levels (kg/m2/s)
!-- precs         snow precipitation rate at all levels (kg/m2/s)
!-- precg         graupel precipitation rate at all levels (kg/m2/s)                             &
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QS          species index for snow
!-- P_QG          species index for graupel
!-- P_QH          species index for hail
!-- P_QNDROP      species index for cloud drop mixing ratio
!-- P_QNR         species index for rain number concentration,
!-- P_QNI         species index for cloud ice number concentration
!-- P_QNS         species index for snow number concentration,
!-- P_QNG         species index for graupel number concentration,
!-- P_QNH         species index for hail number concentration,
!-- P_QZR         species index for rain    reflectivity
!-- P_QZI         species index for ice     reflectivity
!-- P_QZS         species index for snow    reflectivity
!-- P_QZG         species index for graupel reflectivity
!-- P_QZH         species index for hail    reflectivity
!-- P_QVOLG       species index for graupel particle volume,
!-- id            grid id number
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
!-- diagflag      Logical to tell us when to produce diagnostics for history or restart
!
!======================================================================
  INTEGER,parameter :: iunit=6
  INTEGER :: mpi_error_code=1

   TYPE(grid_config_rec_type),  INTENT(IN   ) , OPTIONAL   :: config_flags
   INTEGER,    INTENT(IN   )    :: mp_physics
   LOGICAL,    INTENT(IN   )    :: specified
   INTEGER, OPTIONAL, INTENT(IN   )    :: chem_opt, progn
   INTEGER, OPTIONAL, INTENT(IN   )    :: hail, ice2 !, ccntype
!
   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme,num_scalar
#if (EM_CORE == 1)
   INTEGER,      INTENT(IN   )    ::     num_sbmradar
   INTEGER,      INTENT(IN   )    ::     sbm_diagnostics
#endif
   INTEGER, OPTIONAL, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe
   INTEGER,      INTENT(IN   )    ::                         kts,kte
   INTEGER,      INTENT(IN   )    ::     itimestep,num_tiles,spec_zone
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   LOGICAL,      INTENT(IN   )    ::   warm_rain
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT) ::                                         th
!

!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                             &
                                                                 rho, &
                                                                dz8w, &
                                                                 p8w, &
                                                              pi_phy, &
                                                                   p
!NUWRF JJS 20101021   vvvvv
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT), OPTIONAL ::                        phys_tot,  &
                                                           physc,     &
                                                           physe,     &
                                                           physd,     &
                                                           physs,     &
                                                           physm,     &
                                                           physf,     &
                                                           acphys_tot,  &
                                                           acphysc,     &
                                                           acphyse,     &
                                                           acphysd,     &
                                                           acphyss,     &
                                                           acphysm,     &
                                                           acphysf

!NUWRF JJS 20101021   ^^^^^
!NUWRF JJS 20140225  vvvvv
  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                 &
        INTENT(INOUT), OPTIONAL ::                     re_cloud_gsfc,   &
                                                       re_rain_gsfc,    &
                                                       re_ice_gsfc,     &
                                                       re_snow_gsfc,    &
                                                       re_graupel_gsfc, &
                                                       re_hail_gsfc
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         OPTIONAL,          &
         INTENT(INOUT) ::   &
         precr3d, & ! rain precipitation rate at all levels (kg/m2/s)
         preci3d, & ! ice precipitation rate at all levels (kg/m2/s)
         precs3d, & ! snow precipitation rate at all levels (kg/m2/s)
         precg3d, & ! graupel precipitation rate at all levels (kg/m2/s)
         prech3d    ! graupel precipitation rate at all levels (kg/m2/s)
!NUWRF JJS 20140225  ^^^^^

    REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),INTENT(INOUT), OPTIONAL :: th_old,qv_old
    REAL,DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar),INTENT(INOUT), OPTIONAL   :: scalar
#if (EM_CORE == 1)
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_sbmradar),INTENT(INOUT) :: sbmradar
#endif
    INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(IN), OPTIONAL::   IVGTYP
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN), OPTIONAL    :: XLAT, XLONG

!=================
!Data for CAMMGMP scheme
   REAL,INTENT(IN), OPTIONAL ::accum_mode,aitken_mode,coarse_mode  
!1D variables required for CAMMGMP scheme
   REAL , DIMENSION( kms:kme ) ,                                      &
        INTENT(IN   ) , OPTIONAL ::                                        fnm,  & !Factors for interpolation at "w" grid (interfaces)
                                                                fnp     
!2D variables required for CAMMGMP scheme
   REAL, DIMENSION( ims:ime, jms:jme ),                               &
        INTENT(IN), OPTIONAL ::                                                 &
                                                                 qfx, &    !Moisture flux at surface (kg m-2 s-1)
                                                                 rliq      !Vertically-integrated reserved cloud condensate(m/s)
 
 !3D variables required for CAMMGMP scheme
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(IN), OPTIONAL ::                                                   &
                                                                 dlf, &    !Detraining cloud water tendendcy
                                                                dlf2, &    !dq/dt due to export of cloud water into env by shal conv (kg/kg/s)
                                                               t_phy, &    !Temprature at the mid points (K)
                                                               p_hyd, &    !Hydrostatic pressure(Pa)
                                                             p8w_hyd, &    !Hydrostatic Pressure at level interface (Pa)
                                                              z_at_w, &    !Height above sea level at layer interfaces (m) 
                                                             tke_pbl, &    !Turbulence kinetic energy
                                                          turbtype3d, &    !Turbulent interface types [ no unit ]
                                                              smaw3d, &    !Normalized Galperin instability function for momentum [no units]
                                                                 alt, &    !inverse density(m3/kg)
                                                           icwmrsh3d, &    !Shallow cumulus in-cloud water mixing ratio (kg/m2)
                                                           icwmrdp3d, &    !Deep Convection in-cloud water mixing ratio (kg/m2)
                                                             shfrc3d, &    !Shallow cloud fraction
                                                             cmfmc3d, &    !Deep + Shallow Convective mass flux [ kg /s/m^2 ]
                                                           cmfmc2_3d       !Shallow convective mass flux [ kg/s/m^2 ]
#if ( WRF_CHEM == 1 )
!4D variables required for CAMMGMP scheme
 REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme,ntot_amode_cam_mam ),     &
        INTENT(IN) ::                                                 &
                                                             dgnum4D, &
                                                          dgnumwet4D 
#endif
!In-outs
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(INOUT) , OPTIONAL ::                                                &
                                                       cldfra_old_mp, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                           rh_old_mp, &    !Old RH
                                                          lcd_old_mp       !Old liquid cloud fraction
!In-outs -optional
#if ( WRF_CHEM == 1 )
 REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem),     &
      INTENT(INOUT) ::                                                &
                                                                 chem      !Chem array for CAMMGMP scheme Prognostic aerosols      
#endif
!outs
REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(INOUT) , OPTIONAL::                                                 &
                                                            wsedl3d, &    !Sedimentation velocity of stratiform liquid cloud droplet (m/s) 
                                                          cldfra_mp, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                      cldfra_mp_all, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                            cldfrai, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                            cldfral, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                            lradius, &    !Old Cloud fraction for CAMMGMP microphysics only
                                                            iradius, &    !Old Cloud fraction for CAMMGMP microphysics only                                                            
                                                        cldfra_conv 



#if ( WRF_CHEM == 1 )
REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                        &
      INTENT(INOUT), OPTIONAL ::                                                 &
                                                              qme3d, &     !Net condensation rate (kg/kg/s)
                                                            prain3d, &     !Rate of conversion of condensate to precipitation (kg/kg/s)
                                                           nevapr3d, &     !Evaporation rate of rain + snow (kg/kg/s)
                                                rate1ord_cw2pr_st3d        !1st order rate for direct conversion of strat. cloud water to precip (1/s)
#endif

   REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
!!$#if ( WRF_CHEM == 1 )
!  REAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
   REAL, OPTIONAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
!!$#else
!!$  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
!!$#endif
         qlsink, & ! cloud water sink (/s)
         precr, & ! rain precipitation rate at all levels (kg/m2/s)
         preci, & ! ice precipitation rate at all levels (kg/m2/s)
         precs, & ! snow precipitation rate at all levels (kg/m2/s)
         precg    ! graupel precipitation rate at all levels (kg/m2/s)

!

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN), OPTIONAL   :: SNOWH

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(OUT)   :: SR

   REAL, INTENT(IN   ) :: dt,dx,dy

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR

!
! Optional
!
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: refl_10cm
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: vmi3d,di3d,rhopo3d,    & ! for P3
                                                                         vmi3d_2,di3d_2,rhopo3d_2,  & ! for P3
                                                                         phii3d,         & ! for Jensen ISHMAEL
                                                                         phii3d_2,       & ! for Jensen ISHMAEL
                                                                         vmi3d_3,di3d_3,rhopo3d_3,  & ! for Jensen ISHMAEL
                                                                         phii3d_3,       & ! for Jensen ISHMAEL
                                                                         itype,itype_2,itype_3             ! for Jensen ISHMAEL
   LOGICAL,  OPTIONAL,   INTENT(IN   )    :: channel_switch
   REAL, OPTIONAL,  INTENT(INOUT   ) :: naer  ! aerosol number concentration (/kg)
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) , OPTIONAL :: qnwfa2d, qnifa2d
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                  u,v,w, z, t8w                                       &
                 ,cldfra, cldfra_old, exch_h                      &
                 ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr &
                 ,qt_curr,qndrop_curr,qni_curr,qh_curr,qnh_curr   &
                 ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr    &
                 ,qic_curr,qip_curr,qid_curr &
                 ,qnic_curr,qnip_curr,qnid_curr &
                 ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr    &
                 ,qir_curr,qib_curr                               & ! for P3
                 ,qi2_curr,qni2_curr,qir2_curr,qib2_curr          & ! for P3
                 ,qvoli_curr,qaoli_curr                           & ! for Jensen ISHMAEL
                 ,qvoli2_curr,qaoli2_curr                         & ! for Jensen ISHMAEL
                 ,qi3_curr,qni3_curr,qvoli3_curr,qaoli3_curr      & ! for Jensen ISHMAEL
                 ,effr_curr,ice_effr_curr,tot_effr_curr           &
                 ,qic_effr_curr,qip_effr_curr,qid_effr_curr           &
                 ,kext_ql,kext_qs,kext_qg          &
                 ,kext_qh,kext_qa                       &
                 ,kext_qic,kext_qip,kext_qid,tempc,height      &
                 ,kext_ft_qic,kext_ft_qip,kext_ft_qid &
                 ,kext_ft_qs,kext_ft_qg                           &
                 ,qnwfa_curr,qnifa_curr                           & ! Added by G. Thompson
                 ,qvolg_curr,qvolh_curr, qrimef_curr

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN) :: qrcuten, qscuten, qicuten, qccuten
   INTEGER, INTENT(IN), optional ::     cu_used
#if ( WRF_CHEM == 1 )
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT) :: rainprod, evapprod
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT) :: qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp
#endif
! YLIN
! Added RI_CURR similar to microphysics fields above
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) :: ri_curr


   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                   &
         OPTIONAL,                                                &
         INTENT(OUT ) ::                                          &
                  nsource

!
   REAL, DIMENSION( ims:ime , jms:jme ),                          &
         INTENT(INOUT),                                           &
         OPTIONAL   ::                                            &
                                                           RAINNC &
                                                         ,RAINNCV &
                                                          ,SNOWNC &
                                                         ,SNOWNCV &
                                                       ,GRAUPELNC &
                                                      ,GRAUPELNCV &
                                                          ,HAILNC &
                                                          ,HAILNCV
                                                          
#if ( WRF_CHEM == 1)
! NUWRF JJS 20110525 vvvvv
! for inline Gocart coupling
 integer :: i24h
 INTEGER, PARAMETER :: num_go = 14  ! number of the gocart aerosol species
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_go) :: aero
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(OUT) :: icn_diag
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(OUT) :: nc_diag
 integer, intent(in) :: gsfcgce_gocart_coupling ! EMK
 REAL, PARAMETER :: frac(4)=(/ 0.01053,0.08421,0.25263,0.65263 /) !fraction for fine dust
 
! NUWRF JJS 20110525 ^^^^^
#endif



   INTEGER,OPTIONAL,INTENT(IN   )    ::                        id

   REAL , DIMENSION( ims:ime , jms:jme ) , OPTIONAL ,             &
         INTENT(IN)   ::                                       ht

   REAL, DIMENSION (:), OPTIONAL, INTENT(INOUT) :: mp_restart_state &
                                         ,tbpvs_state,tbpvs0_state
!

   LOGICAL, OPTIONAL :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt    &
                       ,f_qns,f_qnr,f_qng,f_qnn,f_qnc,f_qh,f_qnh,f_qzr       &
                      ,f_effr,f_ice_effr,f_tot_effr &
                       ,f_qic_effr,f_qip_effr,f_qid_effr &
                      ,f_qic,f_qip,f_qid &
                      ,f_qnic,f_qnip,f_qnid                                  &
                       ,f_qzi,f_qzs,f_qzg,f_qzh,f_qvolg,f_qvolh              &
                       ,f_qrimef                                             &
                       ,f_qir,f_qib                                          & ! for P3
                       ,f_qi2,f_qni2,f_qir2,f_qib2                           & ! for P3
                       ,f_qvoli,f_qaoli                                      & ! for Jensen ISHMAEL
                       ,f_qvoli2,f_qaoli2                                    & ! for Jensen ISHMAEL
                       ,f_qi3,f_qni3,f_qvoli3,f_qaoli3                       & ! for Jensen ISHMAEL
                       ,f_qnwfa, f_qnifa                         ! Added by G. Thompson


   LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
   INTEGER, OPTIONAL, INTENT(IN) :: ke_diag ! tells reflectivity calculation whether to do full depth or only k=1
   REAL, INTENT(IN) :: ccn_conc ! RAS
   INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) ::  & ! G. Thompson
                 re_cloud, re_ice, re_snow
   INTEGER, INTENT(IN):: has_reqc, has_reqi, has_reqs

  INTEGER,           INTENT(IN   )    :: aercu_opt
# if (EM_CORE == 1)
  INTEGER, OPTIONAL, INTENT(IN   )    :: PBL
  INTEGER,           INTENT(IN   )    :: no_src_types_cu
  REAL,              INTENT(IN   )    :: aercu_fct
  REAL,    OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types_cu), INTENT(INOUT) & 
                                      :: aerocu
  REAL,    OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme), INTENT(INOUT) &
                                      :: EFCG,           &
                                         EFIG,           &
                                         EFSG,           &
                                         WACT,           &
                                         CCN1_GS,        &
                                         CCN2_GS,        &
                                         CCN3_GS,        &
                                         CCN4_GS,        &
                                         CCN5_GS,        &
                                         CCN6_GS,        &
                                         CCN7_GS,        &
                                         NR_CU,          &
                                         QR_CU,          &
                                         NS_CU,          &
                                         QS_CU
   REAL,   OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(INOUT) &
                                      :: CU_UAF
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: mskf_refl_10cm
# endif

!  REAL , DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT) :: lwp

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij,sz,n
   LOGICAL :: channel
   LOGICAL :: nssl_progn = .false.
   REAL    :: z0, z1, z2, w1, w2

   integer, parameter :: ntot = 50
   real :: wmin, wmax
   integer :: ierr
! IRRIGATION

   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(IN):: IRRIGATION !ARI
   REAL,  OPTIONAL, INTENT(IN)::  irr_daily_amount, julian_in, xtime, gmt
   INTEGER, OPTIONAL, INTENT(IN ):: sf_surf_irr_scheme, irr_start_hour, irr_num_hours, &
                                    irr_start_julianday,irr_end_julianday,irr_freq,irr_ph
   REAL, PARAMETER    :: PI_GRECO=3.14159
   INTEGER  :: end_hour,a,b,xt24,irr_day,timing
   REAL :: constants_irrigation,tloc,irr_start,phase
   INTEGER, OPTIONAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: irr_rand_field

!---------------------------------------------------------------------
!  check for microphysics type.  We need a clean way to
!  specify these things!
!---------------------------------------------------------------------

   channel = .FALSE.
   IF ( PRESENT ( channel_switch ) ) channel = channel_switch

   if (mp_physics .eq. 0) return
   IF( specified ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF

! set this to true to print out the global max/min for W on each time step.
   IF ( .false. ) THEN
      wmax = maxval( w(ips:ipe,kps:kpe,jps:jpe) )
      wmin = minval( w(ips:ipe,kps:kpe,jps:jpe) )
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
      wmax = wrf_dm_max_real ( wmax )
      wmin = wrf_dm_min_real ( wmin )
#endif
      WRITE( wrf_err_message , * ) 'microphysics_driver: GLOBAL w max/min = ', wmax, wmin
      CALL wrf_message ( wrf_err_message )
   ENDIF

#ifdef XEON_OPTIMIZED_WSM5
   ! the OpenMP loops are inside the scheme when running on MIC
   IF ( mp_physics .EQ. WSM5SCHEME ) THEN
       IF (channel) THEN
         its = max(ips,ids)
         ite = min(ipe,ide-1)
       ELSE
         its = max(ips,ids+sz)
         ite = min(ipe,ide-1-sz)
       ENDIF
       jts = max(jps,jds+sz)
       jte = min(jpe,jde-1-sz)

       CALL wsm5(                                              &
             TH=th                                             &
            ,Q=qv_curr                                         &
            ,QC=qc_curr                                        &
            ,QR=qr_curr                                        &
            ,QI=qi_curr                                        &
            ,QS=qs_curr                                        &
            ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
            ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
            ,RD=r_d,RV=r_v,T0C=svpt0                           &
            ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
            ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
            ,DEN0=rhoair0, DENR=rhowater                       &
            ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
            ,RAIN=rainnc ,RAINNCV=rainncv                      &
            ,SNOW=snownc ,SNOWNCV=snowncv                      &
            ,SR=sr                                             &
            ,REFL_10CM=refl_10cm                               &
            ,diagflag=diagflag                                 &
            ,do_radar_ref=do_radar_ref                         &
            ,has_reqc=has_reqc                                 &  ! for radiation +
            ,has_reqi=has_reqi                                 &
            ,has_reqs=has_reqs                                 &
            ,re_cloud=re_cloud                                 &
            ,re_ice=re_ice                                     &
            ,re_snow=re_snow                                   &  ! for radiation -  
            ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
            ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
            ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
            )

   ELSE
#endif

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, i,j,k,n )

   DO ij = 1 , num_tiles
       IF (channel) THEN
         its = max(i_start(ij),ids)
         ite = min(i_end(ij),ide-1)
       ELSE
         its = max(i_start(ij),ids+sz)
         ite = min(i_end(ij),ide-1-sz)
       ENDIF
       jts = max(j_start(ij),jds+sz)
       jte = min(j_end(ij),jde-1-sz)

# if( EM_CORE==1 )
       sf_surf_irr: SELECT CASE(sf_surf_irr_scheme)
            CASE(SPRINKLER)
             CALL sprinkler_irrigation(  julian_in                    & 
     &          ,irrigation, irr_daily_amount,rho,dz8w                & 
     &          ,irr_start_hour,irr_num_hours,irr_start_julianday     & 
     &          ,irr_end_julianday,irr_freq,irr_ph,qr_curr            &
     &          ,gmt,xtime,dt,irr_rand_field                          &
     &          ,ids,ide, jds,jde                                     & ! domain dims
     &          ,ims,ime, jms,jme, kms,kme                            & ! memory dims
     &          ,its,ite, jts,jte               )
       END SELECT sf_surf_irr
#endif



! 2009-06009 rce - zero all these for safety
       IF( PRESENT(qlsink) ) qlsink(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precr ) ) precr(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(preci ) ) preci(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precs ) ) precs(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precg ) ) precg(its:ite,kts:kte,jts:jte)  = 0.

!-----------
       IF( PRESENT(chem_opt) .AND. PRESENT(progn) ) THEN
       
       ! ERM: check whether to use built-in droplet nucleation or use qndrop from CHEM
       IF ( mp_physics==NSSL_2MOMCCN .or. mp_physics==NSSL_2MOM .or. mp_physics==NSSL_2MOMG ) THEN
         IF ( progn > 0 ) THEN
          IF ( .not. (chem_opt == 0 .or. chem_opt == 401) ) nssl_progn = .true.
         ELSE
           nssl_progn = .false. ! use NUCOND for droplet nucleation
         ENDIF
       ENDIF
       
       !Add pass for dust-only wrf-chem option - RAS
       IF( (chem_opt==0 .OR. chem_opt==401) .AND. progn==1 .AND. (mp_physics==LINSCHEME  .OR. mp_physics==MORR_TWO_MOMENT)) THEN
          IF( PRESENT( QNDROP_CURR ) ) THEN
             CALL wrf_debug ( 100 , 'microphysics_driver: calling prescribe_aerosol_mixactivate' )
! 06-nov-2005 rce - id  & itimestep added to arg list
             call prescribe_aerosol_mixactivate (               &
                  id, itimestep, dt, naer,                      &
                  ccn_conc, chem_opt,                           & !RAS13.1
                  rho, th, pi_phy, w, cldfra, cldfra_old,       &
                  z, dz8w, p8w, t8w, exch_h,                    &
                  qv_curr, qc_curr, qi_curr, qndrop_curr,       &
                  nsource,                                      &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  F_QC=f_qc, F_QI=f_qi                          )
          END IF
       ELSEIF ( (chem_opt==0 .OR. chem_opt==401) .AND. progn==1 .AND. (mp_physics==NSSL_2MOMCCN .or.      &
                 mp_physics==NSSL_2MOM .or. mp_physics==NSSL_2MOMG)) THEN
!          Do nothing here for the moment. Use activation of CCN within the NSSL_2MOM scheme instead, based on nssl_cccn namelist value.
       ELSEIF ( progn==1 .AND. mp_physics/=LINSCHEME .AND. mp_physics/=MORR_TWO_MOMENT &
                .AND. mp_physics/=NSSL_2MOM .AND. mp_physics/=NSSL_2MOMCCN .AND. mp_physics/=NSSL_2MOMG ) THEN
             call wrf_error_fatal( &
             "SETTINGS ERROR: Prognostic cloud droplet number can only be used with the mp_physics=LINSCHEME or MORRISON or NSSL_2MOM.")
       END IF
       END IF

#if ( WRF_CHEM == 1)
!NUWRF JJS 20110525 vvvvv
! Pack gocart aerosol species
! All aerosol species in chem are in "ug/kg-dryair"
!  and conerted to (g/m**3)

   aero(:,:,:,:) = 0.
   do k = kts, kte
      do j = jts, jte
         do i = its, ite
         aero(i,k,j, 1) = max(0.0, chem(i,k,j,p_sulf)*1.0e-6*p(i,k,j)*    &
                          96.0/(8.314*th(i,k,j)*pi_phy(i,k,j)))  ! 1 = SO4
         aero(i,k,j, 2) = max(0.0, (chem(i,k,j,p_bc1)+chem(i,k,j,p_bc2))*1.0e-6*rho(i,k,j))  ! 2 = BC1+BC2
         aero(i,k,j, 3) = max(0.0, chem(i,k,j,p_oc1)*1.0e-6*rho(i,k,j)*1.4e0)          !  3 = OC1
         aero(i,k,j, 4) = max(0.0, chem(i,k,j,p_oc2)*1.0e-6*rho(i,k,j)*1.4e0)          !  4 = OC2
         aero(i,k,j, 5) = max(0.0, chem(i,k,j,p_seas_1)*1.0e-6*rho(i,k,j))           !  5 = SS1
         aero(i,k,j, 6) = max(0.0, (chem(i,k,j,p_seas_2)+chem(i,k,j,p_seas_3)+  &
                                   chem(i,k,j,p_seas_4))*1.0e-6*rho(i,k,j))          !  6 = SS2+SS3+SS4
         aero(i,k,j, 7) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(1))   !  7 = DU1 dust mode 1
         aero(i,k,j, 8) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(2))   !  8 = DU1 dust mode 2
         aero(i,k,j, 9) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(3))   !  9 = DU1 dust mode 3
         aero(i,k,j,10) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(4))   ! 10 = DU1 dust mode 4
         aero(i,k,j,11) = max(0.0, chem(i,k,j,p_dust_2)*1.0e-6*rho(i,k,j))           ! 11 = DU2 dust mode 5
         aero(i,k,j,12) = max(0.0, chem(i,k,j,p_dust_3)*1.0e-6*rho(i,k,j))           ! 11 = DU3 dust mode 6
         aero(i,k,j,13) = max(0.0, chem(i,k,j,p_dust_4)*1.0e-6*rho(i,k,j))           ! 11 = DU4 dust mode 7
         aero(i,k,j,14) = max(0.0, chem(i,k,j,p_dust_5)*1.0e-6*rho(i,k,j))           ! 11 = DU5 dust mode 8
         enddo ! !
      enddo ! j
   enddo ! k
!NUWRF JJS 20110525 ^^^^^
#endif

     micro_select: SELECT CASE(mp_physics)

        CASE (KESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling kessler' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT( QC_CURR ) .AND.  &
                                           PRESENT( QR_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                                           PRESENT( Z       ))  THEN
               CALL kessler(                                        &
                  T=th                                              &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,RHO=rho, PII=pi_phy,DT_IN=dt, Z=z, XLV=xlv, CP=cp &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0,RHOWATER=rhowater           &
                 ,DZ8W=dz8w                                         &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling kessler' )
             ENDIF
#if ( WRFPLUS == 1 )
        CASE (MKESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling mkessler' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT( QC_CURR ) .AND.  &
                                           PRESENT( QR_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                                           PRESENT( Z       ))  THEN
               CALL mkessler(                                        &
                  T=th                                              &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
!whl P is not recomputed in the HL' Kessler; need be added here
                 ,P=p                                               &
!end whl
                 ,RHO=rho, PII=pi_phy,DT_IN=dt, Z=z, XLV=xlv, CP=cp &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0,RHOWATER=rhowater           &
                 ,DZ8W=dz8w                                         &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling mkessler' )
             ENDIF
#endif
!
        CASE (THOMPSONAERO)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling thompson' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR )   .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR )   .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR )   .AND.  &
                  PRESENT( QNR_CURR) .AND. PRESENT ( QNI_CURR)   .AND.  &
                  PRESENT( QNC_CURR) .AND. PRESENT ( QNWFA_CURR) .AND.  &
                  PRESENT( QNIFA_CURR).AND.PRESENT ( QNWFA2D)    .AND.  &
                  PRESENT( QNIFA2D)                              .AND.  &
                  PRESENT( SNOWNC)   .AND. PRESENT ( SNOWNCV)    .AND.  &
                  PRESENT( GRAUPELNC).AND. PRESENT ( GRAUPELNCV) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) ) THEN
#if ( WRF_CHEM == 1 )
                 qv_b4mp(its:ite,kts:kte,jts:jte) = qv_curr(its:ite,kts:kte,jts:jte)
                 qc_b4mp(its:ite,kts:kte,jts:jte) = qc_curr(its:ite,kts:kte,jts:jte)
                 qi_b4mp(its:ite,kts:kte,jts:jte) = qi_curr(its:ite,kts:kte,jts:jte)
                 qs_b4mp(its:ite,kts:kte,jts:jte) = qs_curr(its:ite,kts:kte,jts:jte)
#endif
             CALL mp_gt_driver(                          &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     NI=qni_curr,                        &
                     NR=qnr_curr,                        &
                     NC=qnc_curr,                        &
                     NWFA=qnwfa_curr,                    &
                     NIFA=qnifa_curr,                    &
                     NWFA2D=qnwfa2d,                     &
                     NIFA2D=qnifa2d,                     &
                     TH=th,                              &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DT_IN=dt,                           &
                     ITIMESTEP=itimestep,                &
                     RAINNC=RAINNC,                      &
                     RAINNCV=RAINNCV,                    &
                     SNOWNC=SNOWNC,                      &
                     SNOWNCV=SNOWNCV,                    &
                     GRAUPELNC=GRAUPELNC,                &
                     GRAUPELNCV=GRAUPELNCV,              &
                     SR=SR,                              &
#if ( WRF_CHEM == 1 )
                     RAINPROD=rainprod,                  &
                     EVAPPROD=evapprod,                  &
#endif
                     REFL_10CM=refl_10cm,                &
                     diagflag=diagflag,                  &
                     ke_diag = ke_diag,                  &
                     do_radar_ref=do_radar_ref,          &
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & ! G. Thompson
                     has_reqi=has_reqi,                  & ! G. Thompson
                     has_reqs=has_reqs,                  & ! G. Thompson
                 IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                 IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                 ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling thompson_et_al' )
             ENDIF
!
        CASE (THOMPSON)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling thompson' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QNR_CURR) .AND. PRESENT ( QNI_CURR) .AND.  &
!                  PRESENT( SNOWNC) .AND. PRESENT ( SNOWNCV) .AND.        &
!                  PRESENT( GRAUPELNC) .AND. PRESENT ( GRAUPELNCV) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) ) THEN
#if ( WRF_CHEM == 1 )
                 qv_b4mp(its:ite,kts:kte,jts:jte) = qv_curr(its:ite,kts:kte,jts:jte)
                 qc_b4mp(its:ite,kts:kte,jts:jte) = qc_curr(its:ite,kts:kte,jts:jte)
                 qi_b4mp(its:ite,kts:kte,jts:jte) = qi_curr(its:ite,kts:kte,jts:jte)
                 qs_b4mp(its:ite,kts:kte,jts:jte) = qs_curr(its:ite,kts:kte,jts:jte)
#endif
             CALL mp_gt_driver(                          &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     NI=qni_curr,                        &
                     NR=qnr_curr,                        &
                     TH=th,                              &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DT_IN=dt,                           &
                     ITIMESTEP=itimestep,                &
                     RAINNC=RAINNC,                      &
                     RAINNCV=RAINNCV,                    &
                     SNOWNC=SNOWNC,                      &
                     SNOWNCV=SNOWNCV,                    &
                     GRAUPELNC=GRAUPELNC,                &
                     GRAUPELNCV=GRAUPELNCV,              &
                     SR=SR,                              &
#if ( WRF_CHEM == 1 )
                     RAINPROD=rainprod,                  &
                     EVAPPROD=evapprod,                  &
#endif
                     REFL_10CM=refl_10cm,                &
                     diagflag=diagflag,                  &
                     ke_diag = ke_diag,                  &
                     do_radar_ref=do_radar_ref,          &
                     re_cloud=re_cloud,                  & ! G. Thompson
                     re_ice=re_ice,                      & ! G. Thompson
                     re_snow=re_snow,                    & ! G. Thompson
                     has_reqc=has_reqc,                  & ! G. Thompson
                     has_reqi=has_reqi,                  & ! G. Thompson
                     has_reqs=has_reqs,                  & ! G. Thompson
                 IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                 IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                 ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte)
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling thompson_et_al' )
             ENDIF
#if (EM_CORE==1)
# if ( BUILD_SBM_FAST == 1 )
       CASE (FAST_KHAIN_LYNN_SHPUND)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbm' )
               CALL fast_sbm(W=w,U=u,V=v,TH_OLD=th_old              &
                 ,CHEM_new=scalar,N_CHEM=num_scalar                 &
                 ,ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p                     &
                 ,PI_PHY=pi_phy,TH_PHY=th                           &
                 ,xland=xland,domain_id=id                          &
                 ,ivgtyp=ivgtyp                                     &
                 ,xlat=xlat                                         &
                 ,xlong=xlong                                       &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QV_OLD=qv_old                                     &
                 ,QNC=qnc_curr                                      &
                 ,QNR=qnr_curr                                      &
                 ,QNI=qni_curr                                      &
                 ,QNS=qns_curr                                      &
                 ,QNG=qng_curr                                      &
                 ,QNA=qnn_curr                                      &
                 ,sbmradar=sbmradar,num_sbmradar=num_sbmradar       &
                 ,sbm_diagnostics=sbm_diagnostics                   &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,RAINNC=rainnc                                     &
                 ,RAINNCV=rainncv                                   &
                 ,SNOWNC=snownc                                     &
                 ,SNOWNCV=snowncv                                   &
                 ,GRAUPELNC=graupelnc                               &
                 ,GRAUPELNCV=graupelncv                             &
                 ,SR=sr                                             &
                                                                    )
# endif

!
       CASE (FULL_KHAIN_LYNN)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbm' )
               CALL sbm(W=w,U=u,V=v,TH_OLD=th_old          &
                 ,CHEM_new=scalar,N_CHEM=num_scalar                     &
                 ,ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,xland=xland                                       &
                 ,ivgtyp=ivgtyp                                      &
                 ,xlat=xlat                                        &
                 ,xlong=xlong                                        &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QIP=qip_curr                                        &
                 ,QIC=qic_curr                                        &
                 ,QID=qid_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,QV_OLD=qv_old                                     &
                 ,QNC=qnc_curr                                      &
                 ,QNR=qnr_curr                                      &
                 ,QNIP=qnip_curr                                      &
                 ,QNIC=qnic_curr                                      &
                 ,QNID=qnid_curr                                      &
                 ,QNS=qns_curr                                      &
                 ,QNG=qng_curr                                      &
                 ,QNH=qng_curr                                      &
                 ,QNA=qnn_curr                                      &
                 ,EFFR=effr_curr                                  &
                 ,ICE_EFFR=ice_effr_curr                                  &
                 ,TOT_EFFR=tot_effr_curr                                  &
                 ,QIC_EFFR=qic_effr_curr                                  &
                 ,QIP_EFFR=qip_effr_curr                                  &
                 ,QID_EFFR=qid_effr_curr                                  &
                 ,height=height                                        &
                 ,tempc=tempc                                         &
                 ,kext_ql=kext_ql                                       &
                 ,kext_qs=kext_qs                                       &
                 ,kext_qg=kext_qg                                       &
                 ,kext_qh=kext_qh                                       &
                 ,kext_qa=kext_qa                                       &
                 ,kext_qic=kext_qic                                       &
                 ,kext_qip=kext_qip                                       &
                 ,kext_qid=kext_qid                                       &
                 ,kext_ft_qic=kext_ft_qic                                       &
                 ,kext_ft_qip=kext_ft_qip                                       &
                 ,kext_ft_qid=kext_ft_qid                                       &
                 ,kext_ft_qs=kext_ft_qs                                       &
                 ,kext_ft_qg=kext_ft_qg                                       &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,REFL_10CM=refl_10cm                 &  ! added for radar reflectivity
                 ,diagflag=diagflag                   &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref           &  ! added for radar reflectivity
                 ,RAINNC=rainnc                       &
                 ,RAINNCV=rainncv                     &
                 ,SNOWNC=snownc                       &
                 ,SNOWNCV=snowncv                     &
                 ,GRAUPELNC=graupelnc                 &
                 ,GRAUPELNCV=graupelncv               &
                 ,HAILNC=hailnc                       &
                 ,HAILNCV=hailncv                     &
                 ,SR=sr                               &
                                                      )
#endif

!
     CASE (JENSEN_ISHMAEL)
        CALL wrf_debug(100, 'microphysics_driver: jensen_ishmael ')
        IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. &
             PRESENT (QI_CURR).AND. PRESENT (QNI_CURR) .AND. &
             PRESENT (QVOLI_CURR) .AND. PRESENT (QAOLI_CURR) .AND. &
             PRESENT (QI2_CURR).AND. PRESENT (QNI2_CURR) .AND. &
             PRESENT (QVOLI2_CURR) .AND. PRESENT (QAOLI2_CURR) .AND. &
             PRESENT (QI3_CURR).AND. PRESENT (QNI3_CURR) .AND. &
             PRESENT (QVOLI3_CURR).AND. PRESENT (QAOLI3_CURR)) THEN
             CALL mp_jensen_ishmael(             &
             ITIMESTEP=itimestep,                &  !*                                                                         
             DT_IN=dt,                           &  !*
             P=p,                                &  !*                                                                         
             DZ=dz8w,                            &  !* !
             TH=th,                              &  !*                                                                         
             QV=qv_curr,                         &  !*                                                                         
             QC=qc_curr,                         &  !*                                                                         
             QR=qr_curr,                         &  !*                                                                         
             NR=qnr_curr,                        &  !* !
             QI1=qi_curr,                        &  !*                                                                         
             NI1=qni_curr,                       &  !*                                                                         
             AI1=qvoli_curr,                     &  !*
             CI1=qaoli_curr,                     &  !*
             QI2=qi2_curr,                       &  !*                                                                         
             NI2=qni2_curr,                      &  !*                                                                         
             AI2=qvoli2_curr,                    &  !*
             CI2=qaoli2_curr,                    &  !*
             QI3=qi3_curr,                       &  !*                                                                         
             NI3=qni3_curr,                      &  !*                                                                         
             AI3=qvoli3_curr,                    &  !*
             CI3=qaoli3_curr,                    &  !*
             IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
             IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
             ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte, &
             RAINNC=RAINNC,                      &
             RAINNCV=RAINNCV,                    &
             SNOWNC=SNOWNC,                      &
             SNOWNCV=SNOWNCV,                    &
             diag_effc3d=re_cloud,               &
             diag_effi3d=re_ice,                 &
             diag_dbz3d=refl_10cm,               &
             diag_vmi3d_1=vmi3d,                 &
             diag_di3d_1=di3d,                   &
             diag_rhopo3d_1=rhopo3d,             &
             diag_phii3d_1=phii3d,               &
             diag_vmi3d_2=vmi3d_2,               &
             diag_di3d_2=di3d_2,                 &
             diag_rhopo3d_2=rhopo3d_2,           &
             diag_phii3d_2=phii3d_2,             &
             diag_vmi3d_3=vmi3d_3,               &
             diag_di3d_3=di3d_3,                 &
             diag_rhopo3d_3=rhopo3d_3,           &
             diag_phii3d_3=phii3d_3,             &
             diag_itype_1=itype,                 &
             diag_itype_2=itype_2,               &
             diag_itype_3=itype_3                &
             )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling jensen_ishamel')
        ENDIF

    CASE (MORR_TWO_MOMENT)
         CALL wrf_debug(100, 'microphysics_driver: calling morrison two moment')
         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QS_CURR) .AND. PRESENT (QG_CURR) .AND. &
         PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
         PRESENT (QNS_CURR) .AND. PRESENT (QNI_CURR).AND. &
         PRESENT (QNR_CURR) .AND. PRESENT (QNG_CURR).AND. &
         PRESENT (QSCUTEN).AND. &
         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
         PRESENT ( W      )  ) THEN
         CALL mp_morr_two_moment(                            &
                     ITIMESTEP=itimestep,                &  !*
                     TH=th,                              &  !*
                     QV=qv_curr,                         &  !*
                     QC=qc_curr,                         &  !*
                     QR=qr_curr,                         &  !*
                     QI=qi_curr,                         &  !*
                     QS=qs_curr,                         &  !*
                     QG=qg_curr,                         &  !*
                     NI=qni_curr,                        &  !*
                     NS=qns_curr,                        &  !* ! VVT
                     NR=qnr_curr,                        &  !* ! VVT
                     NG=qng_curr,                        &  !* ! VVT
                     RHO=rho,                            &  !*
                     PII=pi_phy,                         &  !*
                     P=p,                                &  !*
                     DT_IN=dt,                           &  !*
                     DZ=dz8w,                            &  !* !hm
                     HT=ht,                              &  !*
                     W=w                                 &  !*
                    ,RAINNC=RAINNC                       &  !*
                    ,RAINNCV=RAINNCV                     &  !*
                    ,SNOWNC=SNOWNC                       &  !*
                    ,SNOWNCV=SNOWNCV                     &  !*
                    ,GRAUPELNC=GRAUPELNC                 &  !*
                    ,GRAUPELNCV=GRAUPELNCV               &  !*
                    ,SR=SR                               &  !* !hm
                    ,REFL_10CM=refl_10cm                 &  ! added for radar reflectivity
                    ,diagflag=diagflag                   &  ! added for radar reflectivity
                    ,do_radar_ref=do_radar_ref           &  ! added for radar reflectivity
                    ,qrcuten=qrcuten                     &  ! hm
                    ,qscuten=qscuten                     &  ! hm
                    ,qicuten=qicuten                     &  ! hm
                    ,F_QNDROP=f_qndrop                   &  ! hm for wrf-chem
                 ,QNDROP=qndrop_curr                     &  ! hm for wrf-chem
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,QLSINK=qlsink                                     & ! jdf for wrf-chem
#if ( WRF_CHEM == 1 )
                 ,EVAPPROD=evapprod,RAINPROD=rainprod               &
#endif
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   & ! jdf for wrf-chem
                                                                    )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling morrison two moment')
        ENDIF

#if (EM_CORE==1)
    CASE (P3_1CATEGORY)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')
!         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
!             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
!         PRESENT (QNG_CURR) .AND. &
!         PRESENT (QNC_CURR) .AND. PRESENT (QNI_CURR).AND. &
!         PRESENT (QNR_CURR) .AND. &
!         PRESENT (QSCUTEN).AND. &
!         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
!         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
!         PRESENT (Z      ) .AND.PRESENT ( W      )  ) THEN

         CALL mp_p3_wrapper_wrf(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=1                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                                                                    )
!        ELSE
!           Call wrf_error_fatal( 'arguments not present for calling p3 one category')
!        ENDIF

    CASE (P3_1CATEGORY_NC)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')
!         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
!             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
!         PRESENT (QNG_CURR) .AND. &
!         PRESENT (QNC_CURR) .AND. PRESENT (QNI_CURR).AND. &
!         PRESENT (QNR_CURR) .AND. &
!         PRESENT (QSCUTEN).AND. &
!         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
!         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
!         PRESENT (Z      ) .AND.PRESENT ( W      )  ) THEN

         CALL mp_p3_wrapper_wrf(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     nc_3d=qnc_curr,                   &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=1                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                                                                    )
    CASE (P3_2CATEGORY)
         CALL wrf_debug(100, 'microphysics_driver: calling p3 one category')
!         IF (PRESENT (QV_CURR) .AND. PRESENT (QC_CURR) .AND. &
!             PRESENT (QR_CURR) .AND. PRESENT (QI_CURR) .AND. &
!         PRESENT (QNG_CURR) .AND. &
!         PRESENT (QNC_CURR) .AND. PRESENT (QNI_CURR).AND. &
!         PRESENT (QNR_CURR) .AND. &
!         PRESENT (QSCUTEN).AND. &
!         PRESENT (QRCUTEN) .AND. PRESENT (QICUTEN).AND. &
!         PRESENT (RAINNC ) .AND. PRESENT (RAINNCV) .AND. &
!         PRESENT (Z      ) .AND.PRESENT ( W      )  ) THEN

         CALL mp_p3_wrapper_wrf_2cat(                         &
                     ITIMESTEP=itimestep,                &
                     TH_3d=th,                            &
                     QV_3d=qv_curr,                       &
                     QC_3d=qc_curr,                       &
                     QR_3d=qr_curr,                       &
                     QNR_3d=qnr_curr,                     &
                     QI1_3d=qi_curr,                     &
                     QIR1_3d=qir_curr,                    &
                     QNI1_3d=qni_curr,                   &
                     QIB1_3d=qib_curr,                 &
                     QI2_3d=qi2_curr,                     &
                     QIR2_3d=qir2_curr,                    &
                     QNI2_3d=qni2_curr,                   &
                     QIB2_3d=qib2_curr,                 &
                     th_old_3d=th_old,                 &
                     qv_old_3d=qv_old,                 &
                     nc_3d=qnc_curr,                   &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT=dt,                           &
                     DZ=dz8w,                            &
                     W=w                                 &
                    ,RAINNC=RAINNC                       &
                    ,RAINNCV=RAINNCV                     &
                    ,SR=SR                               &
                    ,SNOWNC=SNOWNC                       &
                    ,SNOWNCV=SNOWNCV                     &
                    ,N_ICECAT=2                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,diag_zdbz_3d=refl_10cm,                                 &
                     diag_effc_3d=re_cloud,                    &
                     diag_effi_3d=re_ice                       &
                 ,diag_vmi_3d=vmi3d                                       &
                 ,diag_di_3d=di3d                                         &
                 ,diag_rhopo_3d=rhopo3d                                   &
                 ,diag_vmi2_3d=vmi3d_2                                       &
                 ,diag_di2_3d=di3d_2                                         &
                 ,diag_rhopo2_3d=rhopo3d_2                                   &
                                                                    )
#endif


# if (EM_CORE == 1)
    CASE (MORR_TM_AERO)
         CALL wrf_debug(100, 'microphysics_driver: calling morrison two moment')
         CALL mp_morr_two_moment_aero(                            &
                     ITIMESTEP=itimestep,                &  !*
                     TH=th,                              &  !*
                     QV=qv_curr,                         &  !*
                     QC=qc_curr,                         &  !*
                     QR=qr_curr,                         &  !*
                     QI=qi_curr,                         &  !*
                     QS=qs_curr,                         &  !*
                     QG=qg_curr,                         &  !*
                     NI=qni_curr,                        &  !*
                     NS=qns_curr,                        &  !* ! VVT
                     NR=qnr_curr,                        &  !* ! VVT
                     NG=qng_curr,                        &  !* ! VVT
                     NC=qnc_curr,                        &  ! TWG/amy added nc
                     KZH=exch_h,                         &  ! TWG/amy add
                     RHO=rho,                            &  !*
                     PII=pi_phy,                         &  !*
                     P=p,                                &  !*
                     DT_IN=dt,                           &  !*
                     DZ=dz8w,                            &  !* !hm
                     HT=ht,                              &  !*
                     W=w                                 &  !*
                    ,RAINNC=RAINNC                       &  !*
                    ,RAINNCV=RAINNCV                     &  !*
                    ,SNOWNC=SNOWNC                       &  !*
                    ,SNOWNCV=SNOWNCV                     &  !*
                    ,GRAUPELNC=GRAUPELNC                 &  !*
                    ,GRAUPELNCV=GRAUPELNCV               &  !*
                    ,SR=SR                               &  !* !hm
                    ,REFL_10CM=refl_10cm                 &  ! added for radar reflectivity
                    ,MSKF_REFL_10CM=MSKF_REFL_10CM       &  ! addef for radar reflectivity TWG 2017
                    ,diagflag=diagflag                   &  ! added for radar reflectivity
                    ,do_radar_ref=do_radar_ref           &  ! added for radar reflectivity
                    ,qrcuten=qrcuten                     &  ! hm
                    ,qscuten=qscuten                     &  ! hm
                    ,qicuten=qicuten                     &  ! hm
                    ,F_QNDROP=f_qndrop                   &  ! hm for wrf-chem
                 ,QNDROP=qndrop_curr                     &  ! hm for wrf-chem
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,PBL=PBL                                & ! TWG/amy add
                 ,aerocu=aerocu                          & ! TWG add
                 ,aercu_opt=aercu_opt                    & ! TWG add
                 ,aercu_fct=aercu_fct                    & ! TWG add
                 ,no_src_types_cu=no_src_types_cu        & ! TWG add
                 ,EFCG=EFCG                              & ! TWG add
                 ,EFIG=EFIG                              & ! TWG add
                 ,EFSG=EFSG                              & ! TWG add
                 ,WACT=WACT                              & ! TWG add
                 ,CCN1_GS=CCN1_GS                        & ! TWG add
                 ,CCN2_GS=CCN2_GS                        & ! TWG add
                 ,CCN3_GS=CCN3_GS                        & ! TWG add
                 ,CCN4_GS=CCN4_GS                        & ! TWG add
                 ,CCN5_GS=CCN5_GS                        & ! TWG add
                 ,CCN6_GS=CCN6_GS                        & ! TWG add
                 ,CCN7_GS=CCN7_GS                        & ! TWG add
                 ,NR_CU=NR_CU                            & ! TWG add
                 ,QR_CU=QR_CU                            & ! TWG add
                 ,NS_CU=NS_CU                            & ! TWG add
                 ,QS_CU=QS_CU                            & ! TWG add
                 ,CU_UAF=CU_UAF                          & ! TWG add
                 ,QLSINK=qlsink                                     & ! jdf for wrf-chem
#if ( WRF_CHEM == 1 )
                 ,EVAPPROD=evapprod,RAINPROD=rainprod               &
#endif
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   & ! jdf for wrf-chem
                                                                    )
#endif

    CASE (MILBRANDT2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling milbrandt2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
             PRESENT ( W      )  ) THEN
!            PRESENT (ccntype)                                 &

         CALL mp_milbrandt2mom_driver(                   &
                     ITIMESTEP=itimestep,                &
                     p8w=p8w,                              &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QG=qg_curr,                         &
                     QH=qh_curr,                         &
                     NC=qnc_curr,                        &
                     NR=qnr_curr,                        &
                     NI=qni_curr,                        &
                     NS=qns_curr,                        &
                     NG=qng_curr,                        &
                     NH=qnh_curr,                        &
                     PII=pi_phy,                         &
                     P=p,                                &
                     DT_IN=dt,                           &
                     DZ=dz8w,                            &
                     W=w,                                &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
!                    ccntype  = ccntype,                 &
                     Zet      = refl_10cm,               & ! HM, 9/22/09 for refl
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling milbrandt2mom')
        ENDIF


!     CASE (MILBRANDT3MOM)
!          CALL wrf_debug(100, 'microphysics_driver: calling milbrandt3mom')
!          IF (PRESENT (QV_CURR) .AND.                          &
!              PRESENT (QC_CURR) .AND. PRESENT (QNC_CURR) .AND. &
!              PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR) .AND. PRESENT (QZR_CURR) .AND.  &
!              PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR) .AND. PRESENT (QZI_CURR) .AND.  &
!              PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR) .AND. PRESENT (QZS_CURR) .AND.  &
!              PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR) .AND. PRESENT (QZG_CURR) .AND.  &
!              PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR) .AND. PRESENT (QZH_CURR) .AND.  &
!              PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)  .AND. &
!              PRESENT ( W      )  ) THEN
!          CALL mp_milbrandt3mom_driver(                   &
!                      ITIMESTEP=itimestep,                &  !*
!                      TH=th,                              &  !*
!                      QV=qv_curr,                         &  !*
!                      QC=qc_curr,                         &  !*
!                      QR=qr_curr,                         &  !*
!                      QI=qi_curr,                         &  !*
!                      QS=qs_curr,                         &  !*
!                      QG=qg_curr,                         &  !*
!                      QH=qh_curr,                         &  !*
!                      NC=qnc_curr,                        &  !*
!                      NR=qnr_curr,                        &  !*
!                      NI=qni_curr,                        &  !*
!                      NS=qns_curr,                        &  !*
!                      NG=qng_curr,                        &  !*
!                      NH=qnh_curr,                        &  !*
!                      ZR=qzr_curr,                        &  !*
!                      ZI=qzi_curr,                        &  !*
!                      ZS=qzs_curr,                        &  !*
!                      ZG=qzg_curr,                        &  !*
!                      ZH=qzh_curr,                        &  !*
!                      PII=pi_phy,                         &  !*
!                      P=p,                                &  !*
!                      DT_IN=dt,                           &  !*
!                      DZ=dz8w,                            &  !* ! h
!                      W=w                                 &  !*
!                     ,RAINNC=RAINNC                       &  !*
!                     ,RAINNCV=RAINNCV                     &  !*
!                     ,SR=SR                               &  !* !hm
!                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
!                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
!                  ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
!                                                                     )
!         ELSE
!            Call wrf_error_fatal( 'arguments not present for calling milbrandt3mom')
!         ENDIF

    CASE (NSSL_1MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND.  &
             PRESENT (QR_CURR) .AND.  &
             PRESENT (QI_CURR) .AND.  &
             PRESENT (QS_CURR) .AND.  &
             PRESENT (QG_CURR) .AND.  &
             PRESENT (QH_CURR) .AND.  &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
#if (EM_CORE==1)
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
#endif
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &
!                     CCW=qnc_curr,                       &
!                     CRW=qnr_curr,                       &
!                     CCI=qni_curr,                       &
!                     CSW=qns_curr,                       &
!                     CHW=qng_curr,                       &
!                     CHL=qnh_curr,                       &
                     VHW=qvolg_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling nssl_1mom')
        ENDIF


    CASE (NSSL_1MOMLFO)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl1mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND.  &
             PRESENT (QR_CURR) .AND.  &
             PRESENT (QI_CURR) .AND.  &
             PRESENT (QS_CURR) .AND.  &
             PRESENT (QG_CURR) .AND.  &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
#if (EM_CORE==1)
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
#endif
             PRESENT ( W      )  ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling nssl_1momlfo')
        ENDIF

    CASE (NSSL_2MOM)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNdrop_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
#if (EM_CORE==1)
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
#endif
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  .AND.         &
             PRESENT (QVOLH_CURR) .AND. F_QVOLH ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &
 !                    CCW=qnc_curr,                       &
                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     CHL=qnh_curr,                       &
                     VHW=qvolg_curr,                     &
                     VHL=qvolh_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
#if ( WRF_CHEM == 1 )
                    EVAPPROD=evapprod,RAINPROD=rainprod, &
#endif
                     nssl_progn=nssl_progn,              &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  ! hm
                     qscuten=qscuten,                    &  ! hm
                     qicuten=qicuten,                    &  ! hm
                     qccuten=qccuten,                    &  ! hm
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & ! ala G. Thompson
                     has_reqi=has_reqi,                  & ! ala G. Thompson
                     has_reqs=has_reqs,                  & ! ala G. Thompson
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )

        ELSE
           Call wrf_error_fatal( 'arguments not present for calling nssl_2mom')
        ENDIF

    CASE (NSSL_2MOMG)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl2mom')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNdrop_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
#if (EM_CORE==1)
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
#endif
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
 !                    CCW=qnc_curr,                       &
                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     VHW=qvolg_curr,                     &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
#if ( WRF_CHEM == 1 )
                    EVAPPROD=evapprod,RAINPROD=rainprod, &
#endif
                     nssl_progn=nssl_progn,              &
                      diagflag = diagflag,               &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  ! hm
                     qscuten=qscuten,                    &  ! hm
                     qicuten=qicuten,                    &  ! hm
                     qccuten=qccuten,                    &  ! hm
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & ! ala G. Thompson
                     has_reqi=has_reqi,                  & ! ala G. Thompson
                     has_reqs=has_reqs,                  & ! ala G. Thompson
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )

        ELSE
           Call wrf_error_fatal( 'arguments not present for calling nssl_2momg')
        ENDIF

    CASE (NSSL_2MOMCCN)
         CALL wrf_debug(100, 'microphysics_driver: calling nssl_2momccn')
         IF (PRESENT (QV_CURR) .AND.                           &
             PRESENT (QC_CURR) .AND. PRESENT (QNDROP_CURR)  .AND. &
             PRESENT (QR_CURR) .AND. PRESENT (QNR_CURR)  .AND. &
             PRESENT (QI_CURR) .AND. PRESENT (QNI_CURR)  .AND. &
             PRESENT (QS_CURR) .AND. PRESENT (QNS_CURR)  .AND. &
             PRESENT (QG_CURR) .AND. PRESENT (QNG_CURR)  .AND. &
             PRESENT (QH_CURR) .AND. PRESENT (QNH_CURR)  .AND. &
             PRESENT (RAINNC ) .AND. PRESENT (RAINNCV)   .AND. &
#if (EM_CORE==1)
             PRESENT (SNOWNC ) .AND. PRESENT (SNOWNCV)   .AND. &
             PRESENT (HAILNC ) .AND. PRESENT (HAILNCV)   .AND. &
             PRESENT (GRAUPELNC).AND.PRESENT (GRAUPELNCV).AND. &
#endif
             PRESENT ( W      )  .AND. &
             PRESENT (QVOLG_CURR) .AND. F_QVOLG  .AND.         &
             PRESENT (QVOLH_CURR) .AND. F_QVOLH  .AND.         &
             PRESENT( QNN_CURR )                          ) THEN
             

         CALL nssl_2mom_driver(                          &
                     ITIMESTEP=itimestep,                &
                     TH=th,                              &
                     QV=qv_curr,                         &
                     QC=qc_curr,                         &
                     QR=qr_curr,                         &
                     QI=qi_curr,                         &
                     QS=qs_curr,                         &
                     QH=qg_curr,                         &
                     QHL=qh_curr,                        &
!                     CCW=qnc_curr,                       &
                     CCW=qndrop_curr,                    &
                     CRW=qnr_curr,                       &
                     CCI=qni_curr,                       &
                     CSW=qns_curr,                       &
                     CHW=qng_curr,                       &
                     CHL=qnh_curr,                       &
                     VHW=qvolg_curr,                     &
                     VHL=qvolh_curr,                     &
                     cn=qnn_curr,                        &
                     PII=pi_phy,                         &
                     P=p,                                &
                     W=w,                                &
                     DZ=dz8w,                            &
                     DTP=dt,                             &
                     DN=rho,                             &
                     RAINNC   = RAINNC,                  &
                     RAINNCV  = RAINNCV,                 &
                     SNOWNC   = SNOWNC,                  &
                     SNOWNCV  = SNOWNCV,                 &
                     HAILNC   = HAILNC,                  &
                     HAILNCV  = HAILNCV,                 &
                     GRPLNC   = GRAUPELNC,               &
                     GRPLNCV  = GRAUPELNCV,              &
                     SR=SR,                              &
                     dbz      = refl_10cm,               &
#if ( WRF_CHEM == 1 )
                     EVAPPROD=evapprod,RAINPROD=rainprod,&
#endif
                     nssl_progn=nssl_progn,              &
                     diagflag = diagflag,                &
                     ke_diag = ke_diag,                &
                     cu_used=cu_used,                    &
                     qrcuten=qrcuten,                    &  ! hm
                     qscuten=qscuten,                    &  ! hm
                     qicuten=qicuten,                    &  ! hm
                     qccuten=qccuten,                    &  ! hm
                     re_cloud=re_cloud,                  &
                     re_ice=re_ice,                      &
                     re_snow=re_snow,                    &
                     has_reqc=has_reqc,                  & ! ala G. Thompson
                     has_reqi=has_reqi,                  & ! ala G. Thompson
                     has_reqs=has_reqs,                  & ! ala G. Thompson
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde, &
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme, &
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte  &
                                                                    )
        ELSE
           Call wrf_error_fatal( 'arguments not present for calling nssl_2momccn')
        ENDIF
!
        CASE (GSFCGCESCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling GSFCGCE' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( HAIL    ) .AND. PRESENT ( ICE2    ) .AND.  &
                  PRESENT( Z       ) .AND. PRESENT ( W       )  ) THEN
               CALL gsfcgce(                                        &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G                          &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,ITIMESTEP=itimestep                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv ,SR=sr             &
                 ,GRAUPELNC=graupelnc ,GRAUPELNCV=graupelncv        &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,F_QG=f_qg                                         &
                 ,QG=qg_curr                                        &
                 ,IHAIL=hail, ICE2=ice2                             &
                                                                    )
! HAIL = 1,  run gsfcgce with hail option
!        0,  run gsfcgce with graupel option   <---- default
!        note: no effect if ice2 = 1
! ICE2 = 1,  run gsfcgce with only snow, ice
!        2,  run gsfcgce with only graupel, ice
!        0,  run gsfcgce with snow, ice and hail/graupel   <---- default

             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling GSFCGCE' )
             ENDIF

        CASE (NUWRF4ICESCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling NUWRF4ICE' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( HAIL    ) .AND. PRESENT ( ICE2    ) .AND.  &
                  PRESENT( Z       ) .AND. PRESENT ( W       )  ) THEN
               CALL gsfcgce_4ice_nuwrf(                             &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QH=qh_curr                                        &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G, W=w                     &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,ITIMESTEP=itimestep, XLAND=xland, DX=dx           &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv ,SR=sr             &
                 ,GRAUPELNC=graupelnc ,GRAUPELNCV=graupelncv        &
                 ,HAILNC=hailnc, HAILNCV=hailncv                    &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,F_QG=f_qg                                         &
                 ,QG=qg_curr                                        &
!                 ,IHAIL=hail, ICE4=ice2                             & ! hardcoded in the 4ice scheme
                                                                       ! ihail = 0, ice4=4 
!NUWRF JJS 20110525 vvvvv
                 ,PHYSC=physc, PHYSE=physe, PHYSD=physd             &
                 ,PHYSS=physs, PHYSM=physm, PHYSF=physf             &
                 ,ACPHYSC=acphysc, ACPHYSE=acphyse, ACPHYSD=acphysd  &
                 ,ACPHYSS=acphyss, ACPHYSM=acphysm, ACPHYSF=acphysf  &
                 ,RE_CLOUD_GSFC=re_cloud_gsfc                       &
                 ,RE_RAIN_GSFC=re_rain_gsfc                         &
                 ,RE_ICE_GSFC=re_ice_gsfc                           &
                 ,RE_SNOW_GSFC=re_snow_gsfc                         &
                 ,RE_GRAUPEL_GSFC=re_graupel_gsfc                   &
                 ,RE_HAIL_GSFC=re_hail_gsfc                         &
                 ,PRECR3D=precr3d,PRECI3D=preci3d,PRECS3D=precs3d   &
                 ,PRECG3D=precg3d,PRECH3D=prech3d                   &
#if ( WRF_CHEM == 1)
                 ,AERO=aero                                         &
                 ,ICN_DIAG=icn_diag, NC_DIAG=nc_diag, GID=ID        &
                 ,CHEM_OPT=chem_opt                                 &
                 ,GSFCGCE_GOCART_COUPLING=gsfcgce_gocart_coupling   &
#endif
!NUWRF JJS 20110525 ^^^^^
                                                                    )

               do j=jts,jte
                  do k=kts,kte
                     do i=its,ite
                        ! Snapshot total latent heating rate [K/s]
                        phys_tot(i,k,j) = physc(i,k,j) + physe(i,k,j) + &
                             physd(i,k,j) + physs(i,k,j) + physm(i,k,j) + &
                             physf(i,k,j)
                        ! Accumulated total latent heating [K]
                        acphys_tot(i,k,j) = acphysc(i,k,j) + acphyse(i,k,j) + &
                             acphysd(i,k,j) + acphyss(i,k,j) + acphysm(i,k,j) + &
                             acphysf(i,k,j)
                        
                     end do
                  end do
               end do

             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling NUWRF4ICESFCGCE' )
             ENDIF

        CASE (LINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling lin_et_al' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( Z       ) ) THEN
               CALL lin_et_al(                                      &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QLSINK=qlsink                                     &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w, GRAV=G,  CP=cp                  &
                 ,RAIR=r_d, RVAPOR=R_v                              &
                 ,XLS=xls, XLV=xlv, XLF=xlf                         &
                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0                             &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
                 ,SNOWNC=snownc, SNOWNCV=snowncv                    &
                 ,GRAUPELNC=graupelnc, GRAUPELNCV=graupelncv, SR=sr &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   &
                 ,F_QG=f_qg, F_QNDROP=f_qndrop                      &
                 ,QG=qg_curr                                        &
                 ,QNDROP=qndrop_curr                                &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling lin_et_al' )
             ENDIF

       CASE (SBU_YLINSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling sbu_ylin' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR )                           .AND.  &
                  PRESENT( RI_CURR )                           .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( Z       ) ) THEN
               CALL sbu_ylin(                                       &
                  TH=th                                             &
                 ,QV=qv_curr                                        &
                 ,QL=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,RI3D=ri_curr                                      &
!                 ,QLSINK=qlsink                                     &
                 ,RHO=rho, PII=pi_phy, P=p, DT_IN=dt, Z=z           &
                 ,HT=ht, DZ8W=dz8w                                  &
!                 , GRAV=G,  CP=cp                  &
!                 ,RAIR=r_d, RVAPOR=R_v                              &
!                 ,XLS=xls, XLV=xlv, XLF=xlf                         &
!                 ,RHOWATER=rhowater, RHOSNOW=rhosnow                &
!                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
!                 ,SVP3=svp3,SVPT0=svpt0                             &
                 ,RAINNC=rainnc, RAINNCV=rainncv                    &
!                 ,SNOWNC=snownc, SNOWNCV=snowncv                    &
!                 ,GRAUPELNC=graupelnc, GRAUPELNCV=graupelncv, SR=sr &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
!                 ,PRECR=precr,PRECI=preci,PRECS=precs,PRECG=precg   &
!                 ,F_QG=f_qg                                         &
!                 ,F_QNDROP=f_qndrop                      &
!                 ,QG=qg_curr                                        &
!                 ,QNDROP=qndrop_curr                                &
                                                                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling sbu_ylin' )
             ENDIF


        CASE (WSM3SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm3' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( W       )                            ) THEN
             CALL wsm3(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QCI=qc_curr                                       &
                 ,QRS=qr_curr                                       &
                 ,W=w,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w              &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
# ifndef _ACCEL
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -  
# endif
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling wsm3' )
             ENDIF

#ifndef XEON_OPTIMIZED_WSM5
        CASE (WSM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm5(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
# ifndef _ACCEL
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -  
# endif
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling wsm5' )
             ENDIF
#endif

        CASE (WSM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm6(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
#ifdef WRF_CHEM
                 ,EVAPPROD=evapprod,RAINPROD=rainprod               &
#endif
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling wsm6' )
             ENDIF

        CASE (WSM7SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wsm7' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR ) .AND.                            &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wsm7(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv                        &
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,HAIL=hailnc ,HAILNCV=hailncv                      &
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling wsm7' )
             ENDIF

        CASE (WDM5SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm5' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT( QNN_CURR ) .AND.  &
                  PRESENT ( QNC_CURR ) .AND. PRESENT( QNR_CURR ).AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm5(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & ! RAS
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -       
                 ,ITIMESTEP=itimestep                               & 
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling wdm5')
             ENDIF

       CASE (WDM6SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm6' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QNN_CURR ) .AND. PRESENT ( QNC_CURR ) .AND. &
                  PRESENT( QNR_CURR ).AND.                            &
                 PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm6(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & ! RAS
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,xland=xland                                       &  ! land mask, 1: land, 2: water
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,ITIMESTEP=itimestep                               & 
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     & 
                 ,re_snow=re_snow                                   &  ! for radiation -  
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
               CALL wrf_error_fatal ( 'arguments not present for calling wdm6')
             ENDIF

        CASE (WDM7SCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling wdm7' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT ( QC_CURR ) .AND.  &
                  PRESENT( QR_CURR ) .AND. PRESENT ( QI_CURR ) .AND.  &
                  PRESENT( QS_CURR ) .AND. PRESENT ( QG_CURR ) .AND.  &
                  PRESENT( QH_CURR ) .AND.                            &
                  PRESENT( QNN_CURR ) .AND. PRESENT ( QNC_CURR ) .AND. &
                  PRESENT( QNR_CURR ).AND.                            &
                 PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )  ) THEN
             CALL wdm7(                                             &
                  TH=th                                             &
                 ,Q=qv_curr                                         &
                 ,QC=qc_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QI=qi_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QG=qg_curr                                        &
                 ,QH=qh_curr                                        &
                 ,NN=qnn_curr                                       &
                 ,NC=qnc_curr                                       &
                 ,NR=qnr_curr                                       &
                 ,DEN=rho,PII=pi_phy,P=p,DELZ=dz8w                  &
                 ,DELT=dt,G=g,CPD=cp,CPV=cpv,CCN0=ccn_conc          & ! RAS
                 ,RD=r_d,RV=r_v,T0C=svpt0                           &
                 ,EP1=ep_1, EP2=ep_2, QMIN=epsilon                  &
                 ,XLS=xls, XLV0=xlv, XLF0=xlf                       &
                 ,DEN0=rhoair0, DENR=rhowater                       &
                 ,CLIQ=cliq,CICE=cice,PSAT=psat                     &
                 ,xland=xland                                       &  ! land mask, 1: land, 2: water
                 ,RAIN=rainnc ,RAINNCV=rainncv                      &
                 ,SNOW=snownc ,SNOWNCV=snowncv                      &
                 ,SR=sr                                             &
                 ,REFL_10CM=refl_10cm                               &  ! added for radar reflectivity
                 ,diagflag=diagflag                                 &  ! added for radar reflectivity
                 ,do_radar_ref=do_radar_ref                         &  ! added for radar reflectivity
                 ,GRAUPEL=graupelnc ,GRAUPELNCV=graupelncv          &
                 ,HAIL=hailnc ,HAILNCV=hailncv                      &
                 ,ITIMESTEP=itimestep                               &
                 ,has_reqc=has_reqc                                 &  ! for radiation +
                 ,has_reqi=has_reqi                                 &
                 ,has_reqs=has_reqs                                 &
                 ,re_cloud=re_cloud                                 &
                 ,re_ice=re_ice                                     &
                 ,re_snow=re_snow                                   &  ! for radiation -
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
               CALL wrf_error_fatal ( 'arguments not present for calling wdm7')
             ENDIF

#if(NMM_CORE==1)
        CASE (ETAMP_HWRF)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew_HWRF')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN

               CALL ETAMP_NEW_HWRF(                                      &
                  ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy, GID=id &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,QV=qv_curr                                        &
                 ,QT=qt_curr                                        &
                 ,LOWLYR=LOWLYR,SR=SR                               &
                 ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY         &
                 ,F_RIMEF_PHY=F_RIMEF_PHY                           &
                 ,QC=qc_curr,QR=Qr_curr,QI=Qi_curr                  &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling etampnew' )
             ENDIF
#endif
        CASE (ETAMPNEW)    !-- Operational 4-km High-Resolution Window (HRW) version
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN
               CALL ETAMP_NEW(                                      &
                  ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy             &
                 ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                 ,QV=qv_curr                                        &
                 ,QC=qc_curr                                        &
                 ,QS=qs_curr                                        &
                 ,QR=qr_curr                                        &
                 ,QT=qt_curr                                        &
                 ,LOWLYR=LOWLYR,SR=SR                               &
                 ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY         &
                 ,F_RIMEF_PHY=F_RIMEF_PHY                           &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,MP_RESTART_STATE=mp_restart_state                 &
                 ,TBPVS_STATE=tbpvs_state,TBPVS0_STATE=tbpvs0_state &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling etampnew' )
             ENDIF
        CASE (FER_MP_HIRES)    !-- Operational Ferrier-Aligo High-Resolution Window(HRW) version
                            !   (2014/2 version) added by Weiguo Wang on
                            !   2014-11-19
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qt_curr ) .AND. &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN

             !  write(0,*)',f_qv,f_qc,f_qr,f_qi,f_qs,f_qg',f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
             !  write(0,*)'max qi=',maxval(qi_curr(its:ite,kts:kte,jts:jte))
             !  write(0,*)'max qs=',maxval(qs_curr(its:ite,kts:kte,jts:jte))

                CALL FER_HIRES(                                      &
                   ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy, GID=id &
                  ,RAINNC=rainnc,RAINNCV=rainncv                     &
                  ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                  ,QV=qv_curr                                        &
                  ,QT=qt_curr                                        &
                  ,LOWLYR=LOWLYR,SR=SR                               &
                  ,F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY         &
                  ,F_RIMEF_PHY=F_RIMEF_PHY                           &
                  ,QC=qc_curr,QR=Qr_curr,QI=Qi_curr                  &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                  ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling fer_hires' )
             ENDIF

        CASE (FER_MP_HIRES_ADVECT)    !-- Operational Ferrier-Aligo High-Resolution Window(HRW) version
                            !   (2014/2 version) added by Weiguo Wang on
                            !   2014-11-19
                            !   Modified for advection, Sam Trahan, August 2015
             CALL wrf_debug ( 100 , 'microphysics_driver: calling etampnew')

             IF ( PRESENT( qv_curr ) .AND. PRESENT( qi_curr ) .AND. &
                  PRESENT( qc_curr ) .and. PRESENT(qrimef_curr) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                  PRESENT( mp_restart_state )                  .AND. &
                  PRESENT( tbpvs_state )                      .AND. &
                  PRESENT( tbpvs0_state )                       ) THEN

             !  write(0,*)',f_qv,f_qc,f_qr,f_qi,f_qs,f_qg',f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
             !  write(0,*)'max qi=',maxval(qi_curr(its:ite,kts:kte,jts:jte))
             !  write(0,*)'max qs=',maxval(qs_curr(its:ite,kts:kte,jts:jte))

                CALL FER_HIRES_ADVECT(                               &
                   ITIMESTEP=itimestep,DT=dt,DX=dx,DY=dy, GID=id &
                  ,RAINNC=rainnc,RAINNCV=rainncv                     &
                  ,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,PI_PHY=pi_phy,TH_PHY=th &
                  ,QV=qv_curr                                        &
                  ,LOWLYR=LOWLYR,SR=SR                               &
                  ,QC=qc_curr,QR=Qr_curr,QI=Qi_curr,QRIMEF=qrimef_curr    &
                  ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                  ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                  ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling fer_hires' )
             ENDIF

#if(EM_CORE==1)
          CASE (CAMMGMPSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling CAMMGMPSCHEME')
             IF ( PRESENT( z          ) .AND. PRESENT( ht          ) .AND. &
                  PRESENT( qs_curr    ) .AND.                              &
                  PRESENT( qv_curr    ) .AND. PRESENT( qc_curr     ) .AND. &
                  PRESENT( qi_curr    ) .AND. PRESENT( f_qc        ) .AND. &
                  PRESENT( qr_curr    ) .AND. PRESENT( qndrop_curr ) .AND. &                  
                  PRESENT( f_qi       ) .AND. PRESENT( qnc_curr    ) .AND. &
                  PRESENT( RAINNCV    ) .AND. PRESENT( SNOWNCV     ) .AND. &
                  PRESENT( qns_curr   ) .AND. PRESENT( qnr_curr    ) .AND. &
#if ( WRF_CHEM == 1 )
                  PRESENT( chem       ) .AND. PRESENT(dgnum4D      )  .AND. &
                  PRESENT( dgnumwet4D ) .AND.                           &
#endif
                  PRESENT( qni_curr   ) .AND. PRESENT( RAINNC      ) ) THEN
#if ( WRF_CHEM == 1 )
                qv_b4mp(its:ite,kts:kte,jts:jte) = qv_curr(its:ite,kts:kte,jts:jte)
                qc_b4mp(its:ite,kts:kte,jts:jte) = qc_curr(its:ite,kts:kte,jts:jte)
                qi_b4mp(its:ite,kts:kte,jts:jte) = qi_curr(its:ite,kts:kte,jts:jte)
                qs_b4mp(its:ite,kts:kte,jts:jte) = qs_curr(its:ite,kts:kte,jts:jte)
#endif
                  
                CALL CAMMGMP(ITIMESTEP=itimestep,DT=dt,P8W=p8w_hyd,P_HYD=p_hyd    &
                     ,T_PHY=t_phy,PI_PHY=pi_phy,Z_AT_W=z_at_w,QFX=qfx             &
                     ,TKE_PBL=tke_pbl,TURBTYPE3D=turbtype3d,SMAW3D=smaw3d     &
                     ,DLF3D=dlf,DLF2_3D=dlf2,RLIQ2D=rliq,Z_SEA_LEVEL=z            &
                     ,KVH3D=exch_h,HT=ht,ALT=alt,ACCUM_MODE=accum_mode            &
                     ,AITKEN_MODE=aitken_mode,COARSE_MODE=coarse_mode             &
                     ,ICWMRSH3D=icwmrsh3d,ICWMRDP3D=icwmrdp3d,SHFRC3D=shfrc3d     &
                     ,CMFMC3D=cmfmc3d,CMFMC2_3D=cmfmc2_3d                         &
                     ,CONFIG_FLAGS=config_flags,F_ICE_PHY=f_ice_phy               &
                     ,F_RAIN_PHY=f_rain_phy                                       &
#if ( WRF_CHEM == 1 )
                     ,DGNUM4D=dgnum4D,DGNUMWET4D=dgnumwet4D                       &
#endif
                     ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde           &
                     ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme           &
                     ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte           &
!Output variables from CAMMGMP
                     ,TH=th,CLDFRA_OLD_MP=cldfra_old_mp,CLDFRA_MP=cldfra_mp       &
                     ,CLDFRA_MP_ALL=cldfra_mp_all,lradius=lradius,iradius=iradius &
                     ,CLDFRAI=cldfrai,CLDFRAL=cldfral                             &
                     ,CLDFRA_CONV=cldfra_conv,WSEDL3D=wsedl3d                     &
                     ,RAINNC=rainnc,RAINNCV=rainncv,SNOWNC=snownc,SNOWNCV=snowncv &
                     ,SR=sr,QV_CURR=qv_curr,QC_CURR=qc_curr,QI_CURR=qi_curr       &
                     ,QS_CURR=qs_curr,QR_CURR=qr_curr,NC3D=qnc_curr               &
                     ,NI3D=qni_curr,NS3D=qns_curr,NR3D=qnr_curr,QNDROP=qndrop_curr&
                     ,RH_OLD_MP=rh_old_mp,LCD_OLD_MP=lcd_old_mp                   &
#if ( WRF_CHEM == 1 )
                     ,CHEM=chem                                                   &
                     ,QME3D=qme3d,PRAIN3D=prain3d,NEVAPR3D=nevapr3d               &
                     ,RATE1ORD_CW2PR_ST3D=rate1ord_cw2pr_st3d                     &
#endif
                     ,XLAND=XLAND,SNOWH=SNOWH                                     &
                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling CAMMGMP SCHEME' )
             ENDIF

!    Added by Zhuxiao,  lscond (simplified Large-scale condensation scheme by Jimy )
#if ( WRFPLUS == 1 )
        CASE (LSCONDSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling lscond' )
             IF ( PRESENT( QV_CURR ) .AND.                          &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )) THEN
                                          
               CALL lscond(                                         &
                  TH=th                                              &
                 ,P=p                                               & 
                 ,QV=qv_curr                                        &
                 ,RHO=rho, PII=pi_phy, XLV=xlv, CP=cp               &  
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0                             & 
                 ,R_V= R_v                                          & ! added
                 ,DZ8W=dz8w                                         &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling lscond' )
             ENDIF
#endif
#endif

      CASE DEFAULT

         WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', mp_physics
         CALL wrf_error_fatal ( wrf_err_message )

      END SELECT micro_select

   ENDDO
   !$OMP END PARALLEL DO

#ifdef XEON_OPTIMIZED_WSM5
   ENDIF
#endif

! by ZCX
! IF ( PRESENT (LWP) ) THEN
!   DO ij = 1 , num_tiles
!      its = i_start(ij)
!      ite = i_end(ij)
!      jts = j_start(ij)
!      jte = j_end(ij)
!      DO j=jts,jte
!      DO i=its,ite
!         lwp(i,j) = 0.0
!         do k=kts,kte
!           lwp(i,j)=lwp(i,j)+qc_curr(i,k,j)*rho(i,k,j)*dz8w(i,k,j)
!         end do
!      ENDDO
!      ENDDO
!   ENDDO
! ENDIF
! ZCX

   CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )

   RETURN

   END SUBROUTINE microphysics_driver

END MODULE module_microphysics_driver
