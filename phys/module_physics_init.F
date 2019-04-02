!WRF:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform model start-up operations
!  for the individual domains.  This is the stage after inputting wrfinput and before
!  calling 'integrate'.

!  This MODULE CONTAINS the following routines:


MODULE module_physics_init

   USE module_state_description
   USE module_model_constants
   USE module_configure, ONLY : grid_config_rec_type
!  USE module_ssib_veg , ONLY : init_module_ssib_veg !fds (SSiB constants)
   !Local data for CAM's MG MP scheme
   integer  :: ixcldliq, ixcldice, ixnumliq, ixnumice
   LOGICAL  :: CAM_INITIALIZED = .FALSE.
   LOGICAL  :: WINDFARM_INITIALIZED = .FALSE.


CONTAINS


!=================================================================
   SUBROUTINE phy_init ( id, config_flags, DT, restart, zfull, zhalf,     &
                         p_top, TSK,RADT,BLDT,CUDT,MPDT,         &
                         RUCUTEN, RVCUTEN,                       &
                         RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                         RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                         RUSHTEN,  RVSHTEN,  RTHSHTEN,           &
                         RQVSHTEN, RQRSHTEN, RQCSHTEN,           &
                         RQSSHTEN, RQISHTEN, RQGSHTEN,           &
                         RUBLTEN,RVBLTEN,RTHBLTEN,               &
                         RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                         RTHRATEN,RTHRATENLW,RTHRATENSW,         &
#if ( EM_CORE == 1 )
                         !BSINGH - For WRFCuP scheme(11/12/2013)
                         cupflag,cldfra_cup,cldfratend_cup,      & !wig, 18-Sep-2006
                         shall,                                  & !wig, 18-Sep-2006
                         tcloud_cup,                             & !rce, 18-apr-2012
                         !BSINGH - ENDS
#endif
                         STEPBL,STEPRA,STEPCU,                   &
                         W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                         SNOWNC, SNOWNCV, GRAUPELNC, GRAUPELNCV, &
                         z_at_q, inv_dens, qnwfa2d, scalar, num_sc,  & ! G. Thompson
                         re_cloud, re_ice, re_snow,              & ! G. Thompson
                         has_reqc, has_reqi, has_reqs,           & ! G. Thompson
#if ( EM_CORE == 1 )
                         re_cloud_gsfc, re_ice_gsfc,             & 
                         re_snow_gsfc,                           & ! Goddard
                         re_graupel_gsfc, re_hail_gsfc,          &
                         re_rain_gsfc,                           & ! Goddard
#endif
                         NCA,swrad_scat,                         &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         CLDFRA,CLDFRA_OLD,GLW,GSW,EMISS,EMBCK,  & !EMBCK new
                         LU_INDEX,                               &
                         landuse_ISICE, landuse_LUCATS,          &
                         landuse_LUSEAS, landuse_ISN,            &
                         lu_state,                               &
                         XLAT,XLONG,xlong_u,xlat_v,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         levsiz, n_ozmixm, n_aerosolc, paerlev,  &
                         alevsiz, no_src_types,                  &
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_PBL,  &
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                         adv_moist_cond,is_CAMMGMP_used,         &
                         APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                         APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                         XICE,XICEM,VEGFRA,SNOW,CANWAT,SMSTAV,   &
                         SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,&
                         ACSNOM,IVGTYP,ISLTYP, SFCEVP, SMOIS,    &
                         SH2O, SNOWH, SMFR3D,                    &  ! temporary
                         SNOALB,                                 &
                         DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                         mp_restart_state,tbpvs_state,tbpvs0_state,&
                         allowed_to_read, moved, start_of_simulation,&
                         LAGDAY,                                 &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         its, ite, jts, jte, kts, kte,           &
                         num_urban_ndm,                          &
                         urban_map_zrd,                          &
                         urban_map_zwd,                          &
                         urban_map_gd,                           &
                         urban_map_zd,                           &
                         urban_map_zdf,                          &
                         urban_map_bd,                           &
                         urban_map_wd,                           &
                         urban_map_gbd,                          &
                         urban_map_fbd,                          &
                         NUM_URBAN_HI,                           &
                         raincv_a,raincv_b,                      &
                         gd_cloud,gd_cloud2,                     & 
                         gd_cloud_a,gd_cloud2_a,                 &
                         QC_CU,QI_CU,                            &
                         ozmixm,pin,                             &    ! Optional
                         aerodm,pina,                            &    ! Optional
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,& ! Optional
                         RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,         &    ! Optional
                         RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,       &    ! Optional
                         SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH, & ! fasdas
                         HFX_FDDA,                                    & ! fasdas
                         FGDT,STEPFG,                            &    ! Optional
                         cugd_tten,cugd_ttens,cugd_qvten,        &    ! Optional
                         cugd_qvtens,cugd_qcten,                 &    ! Optional
                         ISNOWXY, ZSNSOXY, TSNOXY,                        & ! Optional Noah-MP
                         SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,          & ! Optional Noah-MP
                         CANLIQXY, EAHXY, TAHXY, CMXY,                    & ! Optional Noah-MP
                         CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,       & ! Optional Noah-MP
                         WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY, & ! Optional Noah-MP
                         STMASSXY, WOODXY, STBLCPXY, FASTCPXY,            & ! Optional Noah-MP
                         GRAINXY, GDDXY,                                  & ! Optional Noah-MP
                         croptype, cropcat,                      &           ! Noah-MP Crop model
                         iopt_crop,                                       &  
                         gecros_state,                                    & ! Optional gecros crop
                         XSAIXY, LAI,                                     & ! Optional Noah-MP
                         T2MVXY, T2MBXY, CHSTARXY ,                       & ! Optional Noah-MP
                         SMOISEQ  ,SMCWTDXY ,RECHXY, DEEPRECHXY, AREAXY,  & ! Optional Noah-MP 
                         WTDDT , STEPWTD ,QRFSXY ,QSPRINGSXY ,QSLATXY,    & ! Optional Noah-MP
                         FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & ! Optional Noah-MP
                         rechclim  ,                                       & ! Optional Noah-MP
                         msftx, msfty,                           &           ! Optional Noah-MP
!                        num_roof_layers,num_wall_layers,        & !Optional urban
!                        num_road_layers,                        & !Optional urban
                         DZR, DZB, DZG,                          & !Optional urban
                         TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,    & !Optional urban
                         QC_URB2D, XXXR_URB2D,XXXB_URB2D,        & !Optional urban
                         XXXG_URB2D, XXXC_URB2D,                 & !Optional urban
                         TRL_URB3D, TBL_URB3D, TGL_URB3D,        & !Optional urban
                         SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,  & !Optional urban
                         TS_URB2D, FRC_URB2D, UTYPE_URB2D,       & !Optional urban
                         CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D, & !Optional urban
                         DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,    & !Optional urban
                         FLXHUMR_URB2D,FLXHUMB_URB2D,            & !Optional urban
                         FLXHUMG_URB2D,                          & !Optional urban
                         TRB_URB4D,TW1_URB4D,TW2_URB4D,          & !Optional multi-layer urban
                         TGB_URB4D,TLEV_URB3D,QLEV_URB3D,        & !Optional multi-layer urban
                         TW1LEV_URB3D,TW2LEV_URB3D,              & !Optional multi-layer urban
                         TGLEV_URB3D,TFLEV_URB3D,                & !Optional multi-layer urban
                         SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,    & !Optional multi-layer urban
                         SFVENT_URB3D,LFVENT_URB3D,              & !Optional multi-layer urban
                         SFWIN1_URB3D,SFWIN2_URB3D,              & !Optional multi-layer urban
                         SFW1_URB3D,SFW2_URB3D,                  & !Optional multi-layer urban
                         SFR_URB3D,SFG_URB3D,                    & !Optional multi-layer urban
                         LP_URB2D,HI_URB2D,LB_URB2D,             & !Optional multi-layer urban
                         HGT_URB2D,MH_URB2D,STDH_URB2D,          & !Optional multi-layer urban
                         LF_URB2D,                               & !Optional multi-layer urban
                         A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,        & !Optional multi-layer urban
                         A_E_BEP,B_U_BEP,B_V_BEP,                & !Optional multi-layer urban
                         B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,        & !Optional multi-layer urban
                         DL_U_BEP,SF_BEP,VL_BEP,                 & !Optional multi-layer urban
                         TML,T0ML,HML,H0ML,HUML,HVML,TMOML,      & !Optional oml
                         lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & !lake
                         snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & !lake
                         z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & !lake
                         h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & !lake
                         zi3d,         watsat3d,     csol3d,     tkmg3d,         & !lake
                         tkdry3d,      tksatu3d,     lake2d,     lakedepth_default, & !lake
                         lake_min_elev,   lake_depth,                               & !lake
#if ( EM_CORE == 1 )
                         lakemask,   lakeflag,                                   & !lake
#endif
                         lake_depth_flag, use_lakedepth,                         & !lake
                         sf_surface_mosaic, mosaic_cat, NLCAT,               & ! Noah tiling
!CLM variables
                         maxpatch,                                           &
                         numc,nump,snl,                                      &
                         snowdp,wtc,wtp,h2osno,t_grnd,t_veg,                 &
                         h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                         h2osoi_liq_s1,                                      &
                         h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                         h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                         h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                         h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                         h2osoi_ice_s1,h2osoi_ice_s2,                        &
                         h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                         h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                         h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                         h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                         t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                         t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                         t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                         t_soisno8,t_soisno9,t_soisno10,                     &
                         dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                         snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                         t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                         t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                         h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                         h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                         h2osoi_vol7,h2osoi_vol8,                            &
                         h2osoi_vol9,h2osoi_vol10,                           &
                         ht,                                                 &
                         ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                         Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                         SWUPsubgrid,lhsoi,lhveg,lhtran,                     &
! end of CLM variables
                         TSK_SAVE,                               & !Optional for fractional seaice
                         itimestep,                              & !Optional obs fdda
#if ( EM_CORE == 1 )
                         fdob,                                   & !Optional obs fdda
#endif
                         t00, p00, tlp,                          & !for obs-nudging
                         TYR,TYRA,TDLY,TLAG,NYEAR,NDAY,tmn_update,   &
                         ACHFX,ACLHF,ACGRDFLX,                   &
                         nssl_cccn,                              &
                         nssl_alphah,nssl_alphahl,               &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                             &
! next 2 flags for Explicit lightning:
                         nssl_ipelec,                             &
                         nssl_isaund,                             &
   ! OPTIONAL 
                         RQCNCUTEN, RQINCUTEN,                   &
                         rliq,                                   &  !BSINGH:01/31/2013 - Added rliq and is_CAMMGMP_used for CAM5 physics
                         cldfra_dp,cldfra_sh                     & !ckay for subgrid cloud
                         ,te_temf                                & ! WA 12/21/09
                         ,cf3d_temf                              & ! WA 9/27/10
                         ,wm_temf                                & ! WA 2/22/11
                         ,massflux_EDKF, entr_EDKF, detr_EDKF  & ! Optional for qnse
                         ,thl_up, thv_up, rt_up                & ! Optional for qnse
                         ,rv_up, rc_up, u_up, v_up, frac_up    & ! Optional for qnse
                         ,RDCASHTEN, RQCDCSHTEN                 &
                         ,cldareaa, cldareab, cldliqa, cldliqb  &
                         ,ca_rad, cw_rad                        &
                         ,pblmax, wub, ltopb, clddpthb, cldtopb &
                         ,capesave, ainckfsa, radsave           &
                         ,rainsh, rainshvb, kdcldtop, kdcldbas  & 
                         ,xtime1, PBLHAVG, TKEAVG               &
                         ,ccn_conc                             & ! RAS
                         ,QKE                                  & !for MYNN
                         ,landusef,landusef2,mosaic_cat_index                                                 & ! danli mosaic
                         ,TSK_mosaic,TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic                                     & ! danli mosaic
                         ,CANWAT_mosaic,SNOW_mosaic,SNOWH_mosaic,SNOWC_mosaic                                 & ! danli mosaic
                         ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic      & ! danli mosaic
                         ,TR_URB2D_mosaic,TB_URB2D_mosaic                                                     & ! danli mosaic 
                         ,TG_URB2D_mosaic,TC_URB2D_mosaic                                                     & ! danli mosaic 
                         ,QC_URB2D_mosaic                                                                     & ! danli mosaic
                         ,TRL_URB3D_mosaic,TBL_URB3D_mosaic                                                   & ! danli mosaic 
                         ,TGL_URB3D_mosaic                                                                    & ! danli mosaic 
                         ,SH_URB2D_mosaic,LH_URB2D_mosaic                                                     & ! danli mosaic 
                         ,G_URB2D_mosaic,RN_URB2D_mosaic                                                      & ! danli mosaic 
                         ,TS_URB2D_mosaic                                                                     & ! danli mosaic 
                         ,TS_RUL2D_mosaic                                                                     & ! danli mosaic
#if ( EM_CORE == 1 )
                         ,QR_CU,QS_CU,NC_CU,NI_CU,NR_CU,NS_CU,CCN_CU              & ! TWG
                         ,alevsiz_cu,num_months,no_src_types_cu,aeromcu,aeropcu   & ! PSH/TWG 06/10/16                         
                         ,EFCG,EFCS,EFIG,EFIS,EFSG,EFSS                           & ! TWG
#endif
                          )

!-----------------------------------------------------------------
   USE module_domain
   USE module_wrf_error
   use module_sf_lake, only : nlevsoil,nlevsnow,nlevlake
#if ( WRF_CHEM == 1 )
   USE module_gocart_coupling, ONLY : makelut_ccn_icn
   USE module_cam_support,     ONLY : cam_mam_aerosols
#endif
   USE module_wind_fitch
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   INTEGER , INTENT(IN) ,OPTIONAL       :: tmn_update
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   LOGICAL , INTENT(OUT)       :: is_CAMMGMP_used !BSINGH:01/31/2013 - Added for CAM5 physics
!   LOGICAL , INTENT (IN)       :: FNDSOILW, FNDSNOWH
   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers
   INTEGER , INTENT(IN)        :: lagday
   INTEGER , INTENT(OUT) ,OPTIONAL      :: nyear
   REAL    , INTENT(OUT) ,OPTIONAL      :: nday

   LOGICAL,  INTENT(IN)        :: start_of_simulation
   REAL,     INTENT(IN)        :: DT, p_top, DX, DY
   LOGICAL,  INTENT(IN)        :: restart
   REAL,     INTENT(IN)        :: RADT,BLDT,CUDT,MPDT
   REAL,     INTENT(IN)        :: swrad_scat

   REAL,     DIMENSION( kms:kme ) , INTENT(IN) :: zfull, zhalf
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: TSK, XLAT, XLONG,xlong_u,xlat_v
   REAL,  DIMENSION(ims:ime,jms:jme), INTENT(IN) , OPTIONAL :: msftx,msfty

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL, &
          INTENT(INOUT) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, n_ozmixm-1, no_src_types ), OPTIONAL, &
          INTENT(INOUT) ::                                  aerodm

#if ( EM_CORE == 1 )
  !PSH/TWG 06/10/16
   INTEGER,      INTENT(IN   )    ::   alevsiz_cu, num_months, no_src_types_cu !PSH/TWG 06/10/16
   REAL,  DIMENSION( ims:ime, alevsiz_cu, jms:jme, num_months, no_src_types_cu), OPTIONAL, &
          INTENT(INOUT) ::                                  aeromcu 
   REAL,  DIMENSION( ims:ime, alevsiz_cu, jms:jme, num_months), OPTIONAL,INTENT(INOUT)  :: aeropcu 

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          OPTIONAL, INTENT(INOUT   ) ::                              &
                               QR_CU, QS_CU,                         & !TWG
                               NC_CU, NI_CU, NR_CU, NS_CU,           & !TWG
                               CCN_CU, EFCG, EFCS, EFIG,             & !TWG
                               EFIS, EFSG, EFSS
  !PSH/TWG END
#endif

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::        PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(INOUT)  ::       PINA

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL,INTENT(INOUT)  ::          m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(INOUT) ::                    aerosolc_1, aerosolc_2

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                 INTENT(INOUT) :: SMOIS, SH2O,TSLB
   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ), INTENT(OUT) :: SMFR3D

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWC, &
                                                         SNOWH, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                        ACSNOW, &
                                                          XICE, &
                                                         XICEM, &
                                                        VEGFRA, &
                                                        ACSNOM
   REAL,    DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::   rliq    

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(INOUT)    ::                ACHFX, &
                                                         ACLHF, &
                                                      ACGRDFLX

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

#if ( EM_CORE == 1 )
   !BSINGH - For WRFCuP scheme
   REAL, DIMENSION( ims:ime, jms:jme )                        , &
        INTENT(INOUT)    ::                          shall, & !CuP, wig 18-Sep-2006
                                                    tcloud_cup    !CuP, rce 18-apr-2012
   !BSINGH - ENDS
#endif
! rad

!..Added by G. Thompson to couple cloud physics variables with RRTMG radiation.
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: re_cloud, &
             re_ice, re_snow
   INTEGER, INTENT(INOUT):: has_reqc, has_reqi, has_reqs
#if ( EM_CORE == 1 )
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: re_cloud_gsfc, &
             re_ice_gsfc, re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc, re_rain_gsfc
#endif

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &!BSINGH(PNNL)- should be declared inout
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: &!BSINGH(PNNL)- should be declared inout
             CLDFRA_OLD

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: & ! ckay for subgrid cloud
             cldfra_dp, cldfra_sh

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS,EMBCK                          !EMBCK new
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::   SNOALB


   REAL,     INTENT(IN) :: GMT

   INTEGER , INTENT(OUT) :: STEPRA, STEPBL, STEPCU
   INTEGER , INTENT(IN) :: JULYR, JULDAY

! cps

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: & !BSINGH(PNNL)- should be declared inout
             RUCUTEN, RVCUTEN, RTHCUTEN, RQVCUTEN, RQRCUTEN, RQCCUTEN, &
             RQSCUTEN, RQICUTEN,                                       &
             RUSHTEN, RVSHTEN, RTHSHTEN, RQVSHTEN, RQRSHTEN, RQCSHTEN, &
             RQSSHTEN, RQISHTEN, RQGSHTEN
#if ( EM_CORE == 1 )
   !BSINGH - For WRFCuP scheme
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: &
        cldfra_cup,cldfratend_cup                              !CuP, wig 18-Sep-2006

   LOGICAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: cupflag !CuP, wig 9-Oct-2006
   !BSINGH -ENDS
#endif

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: RQCNCUTEN, RQINCUTEN 

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: MASS_FLUX,   &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    & !BSINGH(PNNL)- should be declared inout
             RTHFTEN, RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::           &
             RAINNC, RAINC, RAINCV, RAINNCV,  &
             SNOWNC, SNOWNCV, GRAUPELNC, GRAUPELNCV
   REAL,     DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: z_at_q            !  G. Thompson
   REAL,     DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: inv_dens          !  G. Thompson
   REAL,     DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qnwfa2d                !  G. Thompson
   INTEGER,  INTENT(IN) :: num_sc                                                !  G. Thompson
   REAL,     DIMENSION(ims:ime,kms:kme,jms:jme,num_sc), INTENT(INOUT) :: scalar  !  G. Thompson

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: CLDEFI, NCA

   INTEGER,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: LOWLYR

!pbl

   ! soil layer


   REAL,     DIMENSION(1:num_soil_layers),      INTENT(INOUT) :: ZS,DZS

  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    & !BSINGH(PNNL)- should be declared inout
             RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN,EXCH_H,TKE_PBL
  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE

  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT), OPTIONAL :: &
                                            massflux_EDKF, entr_EDKF, detr_EDKF & 
                                                   ,thl_up, thv_up, rt_up       &
                                                   ,rv_up, rc_up, u_up, v_up    &
                                                   ,frac_up


   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             cugd_tten,cugd_ttens,cugd_qvten,                &
             cugd_qvtens,cugd_qcten
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             XLAND,ZNT,Z0,UST,MOL,LU_INDEX,                         &
             PBLH,THC,MAVAIL,HFX,QFX,RAINBL
   INTEGER , INTENT(INOUT)  :: landuse_ISICE, landuse_LUCATS
   INTEGER , INTENT(INOUT)  :: landuse_LUSEAS, landuse_ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TMN
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TYRA
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT),OPTIONAL :: TDLY
   REAL,     DIMENSION( ims:ime , 1:lagday , jms:jme ) , INTENT(INOUT),OPTIONAL :: TLAG

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          OPTIONAL, INTENT(INOUT   ) ::                              &
                               gd_cloud, gd_cloud2,                  &
                               gd_cloud_a, gd_cloud2_a,              &
                               QC_CU, QI_CU

   REAL,  DIMENSION( ims:ime ,  jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               raincv_a,raincv_b

!Noah-MP

   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ISNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:num_soil_layers, jms:jme) :: ZSNSOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: TSNOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TGXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: EAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CMXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FWETXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SNEQVOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ALBOLDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WSLAKEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ZWTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LFMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RTMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WOODXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: GRAINXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: GDDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STBLCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FASTCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: XSAIXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LAI
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MBXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHSTARXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme) :: SMOISEQ 
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SMCWTDXY   
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: DEEPRECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QRFSXY       
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSPRINGSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSLATXY 
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: AREAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: FDEPTHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: RIVERBEDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: EQZWT
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RIVERCONDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: PEXPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: rechclim
   INTEGER, OPTIONAL, DIMENSION(ims:ime,  jms:jme), INTENT(OUT) :: CROPCAT
   REAL,    OPTIONAL, DIMENSION(ims:ime,5,jms:jme), INTENT(IN ) :: CROPTYPE
   INTEGER, INTENT(IN ) :: iopt_crop
   REAL,    OPTIONAL, DIMENSION(ims:ime,60,jms:jme) :: gecros_state      ! Optional gecros crop

!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),  OPTIONAL,  INTENT(INOUT ) ::   qnn_curr

   INTEGER , OPTIONAL,  INTENT(OUT) :: STEPWTD
   REAL , OPTIONAL, INTENT(IN) :: WTDDT

!mp
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved
   REAL, INTENT(INOUT) :: ccn_conc ! RAS


! ocean mixed layer
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TML,T0ML,HML,H0ML,HUML,HVML,TMOML
! fractional seaice
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TSK_SAVE
!fdda
   REAL,     OPTIONAL, INTENT(IN) :: FGDT
   INTEGER , OPTIONAL, INTENT(OUT) :: STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &!BSINGH(PNNL)- should be declared inout
             RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, RPHNDGDTEN, RQVNDGDTEN
!
! FASDAS
!
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT), OPTIONAL ::  &
             SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH
!  INTEGER,  INTENT(IN   )                                            ::  fasdas
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             HFX_FDDA
!
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             RMUNDGDTEN

!URBAN
!   REAL, DIMENSION(1:num_roof_layers), INTENT(INOUT) :: DZR   !urban
!   REAL, DIMENSION(1:num_wall_layers), INTENT(INOUT) :: DZB   !urban
!   REAL, DIMENSION(1:num_road_layers), INTENT(INOUT) :: DZG   !urban
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR    !urban
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB    !urban
   REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG    !urban

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D !urban

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D

!   REAL, DIMENSION(ims:ime, 1:num_roof_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D !urban
!   REAL, DIMENSION(ims:ime, 1:num_wall_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D !urban
!   REAL, DIMENSION(ims:ime, 1:num_road_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGRL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: SMR_URB3D  !urban

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D !urban
   INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D !urban

   INTEGER , INTENT(IN)        :: num_urban_ndm
   INTEGER , INTENT(IN)        :: urban_map_zrd
   INTEGER , INTENT(IN)        :: urban_map_zwd
   INTEGER , INTENT(IN)        :: urban_map_gd
   INTEGER , INTENT(IN)        :: urban_map_zd
   INTEGER , INTENT(IN)        :: urban_map_zdf
   INTEGER , INTENT(IN)        :: urban_map_bd
   INTEGER , INTENT(IN)        :: urban_map_wd
   INTEGER , INTENT(IN)        :: urban_map_gbd
   INTEGER , INTENT(IN)        :: urban_map_fbd
   INTEGER , INTENT(IN)        :: num_urban_hi
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: TRB_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: TW1_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: TW2_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: TGB_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: TLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: QLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: SFG_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: SFR_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: SFW1_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: SFW2_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime,1:num_urban_hi, jms:jme), INTENT(INOUT) :: HI_URB2D  !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LP_URB2D  !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LB_URB2D  !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HGT_URB2D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: MH_URB2D  !SLUCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STDH_URB2D !SLUCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(INOUT) :: LF_URB2D  !SLUCM 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme), INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP
! lake varibles:
  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
 
  real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(out)            :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(inout) :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(inout) :: zi3d
  LOGICAL, DIMENSION( ims:ime, jms:jme ),intent(out)                      :: lake2d
!  REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)                      ::  HT
  REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  lake_depth
  real, intent(in)      ::      lakedepth_default, lake_min_elev
#if ( EM_CORE == 1 )
  REAL,              dimension(ims:ime,jms:jme )      ::  lakemask
  INTEGER, INTENT(IN)      ::  lakeflag
#endif
  INTEGER, INTENT(INOUT)      ::   lake_depth_flag
  INTEGER, INTENT(IN)      ::   use_lakedepth
 

!CLM
   INTEGER, INTENT(IN) ::       maxpatch
   REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  HT
  integer, OPTIONAL,   dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  integer, OPTIONAL,   dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real, OPTIONAL,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) ::  &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,     &
                t_ref2m,h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhtran,lhveg

!obs fdda
   INTEGER, OPTIONAL, INTENT(IN) :: itimestep
#if ( EM_CORE == 1 )
   TYPE(fdob_type), OPTIONAL, INTENT(INOUT) :: fdob
#endif
   REAL, OPTIONAL, INTENT(IN) :: p00, t00, tlp   ! for obs-nudging base-state calcn
   REAL, INTENT(IN) :: nssl_cccn, nssl_alphah, nssl_alphahl, &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs

   INTEGER, INTENT(IN) :: nssl_ipelec,nssl_isaund

! WA 12/21/09
   REAL,OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(OUT) ::    te_temf, cf3d_temf    
! WA 2/22/11
   REAL,OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(OUT) ::    wm_temf

   REAL, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(  OUT) ::    pblmax, clddpthb, cldtopb, rainsh, rainshvb, &
                              capesave, radsave, xtime1, PBLHAVG

   REAL, OPTIONAL, DIMENSION( ims:ime , 1:100, jms:jme ) , &
          INTENT(  OUT) ::    ainckfsa

   INTEGER, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(  OUT) ::    kdcldtop, kdcldbas, ltopb

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ,                &
          INTENT(  OUT) ::    wub, cldareaa,  cldareab,            &
                                   cldliqa,   cldliqb,   TKEAVG,   &
                                   ca_rad, cw_rad

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: &
             RDCASHTEN, RQCDCSHTEN

! Local data

   REAL    :: ALBLND,ZZLND,ZZWTR,THINLD,XMAVA,CEN_LAT,pptop
   REAL,     DIMENSION( kms:kme )  :: sfull, shalf
   REAL :: obs_twindo_cg, obs_twindo

   CHARACTER*256 :: MMINLU_loc
   CHARACTER*80 :: message
   INTEGER :: ISWATER
   INTEGER :: ISICE
   INTEGER :: ISURBAN
   INTEGER :: sf_urban_physics
   INTEGER :: sf_ocean_physics
   REAL    :: oml_hml0
   INTEGER :: LakeModel
   LOGICAL :: usemonalb
   LOGICAL :: rdmaxalb
   INTEGER :: mfshconv
   INTEGER :: icloud_cu
   INTEGER :: iopt_run
   INTEGER :: aercu_opt !PSH/TWG 
   REAL    :: aercu_fct !PSH/TWG 


   INTEGER :: i, j, k, itf, jtf, ktf, n
   INTEGER :: myproc

!-------------------------------------------------
! Noah-mosaic related variables are added to declaration  (danli)
!-------------------------------------------------
  
  INTEGER, INTENT(IN) :: sf_surface_mosaic, NLCAT   
  INTEGER, INTENT(IN) :: mosaic_cat
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(IN) , OPTIONAL::   LANDUSEF
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(INOUT) , OPTIONAL::   LANDUSEF2 
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT), OPTIONAL :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT):: &
        TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::                &
        ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT):: &
        TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::                &
        TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic,    &
        SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic  
   LOGICAL :: IPRINT
 
!-------------------------------------------------
! End of Noah-mosaic 
!-------------------------------------------------  
   
!-----------------------------------------------------------------

   aercu_opt=config_flags%aercu_opt !PSH/TWG 06/10/16
   aercu_fct=config_flags%aercu_fct !PSH/TWG 06/10/16
   sf_urban_physics=config_flags%sf_urban_physics
   usemonalb=config_flags%usemonalb
   rdmaxalb=config_flags%rdmaxalb
   LakeModel = config_flags%sf_lake_physics
   mfshconv=config_flags%mfshconv
   IF(PRESENT(SMOISEQ)) THEN
      iopt_run=config_flags%opt_run
   ELSE
      iopt_run=-1
   END IF
#if ( EM_CORE == 1 )
   obs_twindo_cg=model_config_rec%obs_twindo(1)
   obs_twindo=config_flags%obs_twindo
   oml_hml0=config_flags%oml_hml0
   sf_ocean_physics=config_flags%sf_ocean_physics
#else
   obs_twindo_cg=0
   obs_twindo=0
   oml_hml0=0
#endif

!..Added G. Thompson to determine if we will compute and pass radiative
!.. effective radii of cloud water, ice, and snow.  Currently ONLY properly
!.. connected if using Physics options Thompson-MP and RRTMG-LW/SW.
! 10/2014: T. Mansell added support for NSSL_2MOM options
   has_reqc = 0
   has_reqi = 0
   has_reqs = 0
   IF ( config_flags%use_mp_re .EQ. 1 ) THEN
   if (( (config_flags%ra_lw_physics .eq. RRTMG_LWSCHEME ) .or. &
#if( BUILD_RRTMG_FAST == 1)
         (config_flags%ra_lw_physics .eq. RRTMG_LWSCHEME_FAST ) .or. &
#endif
         (config_flags%ra_lw_physics .eq. goddardlwscheme ) .or. &
         (config_flags%ra_lw_physics .eq. RRTMK_LWSCHEME  ) ) .and. &
       ( (config_flags%ra_sw_physics .eq. RRTMG_SWSCHEME ) .or. &
#if( BUILD_RRTMG_FAST == 1)
         (config_flags%ra_sw_physics .eq. RRTMG_SWSCHEME_FAST ) .or. &
#endif
         (config_flags%ra_sw_physics .eq. goddardswscheme ) .or. &
         (config_flags%ra_sw_physics .eq. RRTMK_SWSCHEME ) ) .and. &
        (config_flags%mp_physics  .eq. THOMPSON .or.        &
         config_flags%mp_physics  .eq. THOMPSONAERO .or.    &
         config_flags%mp_physics  .eq. NSSL_2MOM .or.       &
         config_flags%mp_physics  .eq. NSSL_2MOMG .or.      &
         config_flags%mp_physics  .eq. NSSL_2MOMCCN .or.    &
         config_flags%mp_physics  .eq. WSM3SCHEME .or.      &
         config_flags%mp_physics  .eq. WSM5SCHEME .or.      &
         config_flags%mp_physics  .eq. WSM6SCHEME .or.      &
         config_flags%mp_physics  .eq. WSM7SCHEME .or.      &
         config_flags%mp_physics  .eq. WDM5SCHEME .or.      &
         config_flags%mp_physics  .eq. WDM6SCHEME .or.      &
         config_flags%mp_physics  .eq. WDM7SCHEME .or.      &
         config_flags%mp_physics  .eq. nuwrf4icescheme .or. &
         config_flags%mp_physics  .eq. JENSEN_ISHMAEL .or.  &
         config_flags%mp_physics  .eq. P3_1CATEGORY .or.    &
         config_flags%mp_physics  .eq. P3_1CATEGORY_NC .or. &
         config_flags%mp_physics  .eq. P3_2CATEGORY    ) ) then    ! P3
      has_reqc = 1
      has_reqi = 1
      has_reqs = 1
   endif

! for P3, to ensure correct coupling with predicted effective radii
   if (config_flags%mp_physics .eq. P3_1CATEGORY .or.       &
       config_flags%mp_physics .eq. P3_1CATEGORY_NC .or.    &
       config_flags%mp_physics .eq. JENSEN_ISHMAEL .or.    &
       config_flags%mp_physics .eq. P3_2CATEGORY      ) then
      has_reqs = 0
   end if

   ENDIF

!-- should be from the namelist

   sfull = 0.
   shalf = 0.

   CALL wrf_debug(100,'top of phy_init')

   WRITE(wrf_err_message,*) 'phy_init:  start_of_simulation = ',start_of_simulation
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)

   ZZLND=0.1
   ZZWTR=0.0001
   THINLD=0.04
   ALBLND=0.2
   XMAVA=0.3

#if (NMM_CORE == 1)
   if (.not.usemonalb) CALL wrf_error_fatal('usemonalb should always be true for NMM')
#endif

   CALL nl_get_cen_lat(id,cen_lat)
   CALL wrf_debug(100,'calling nl_get_iswater, nl_get_isice, nl_get_mminlu_loc')
   CALL nl_get_iswater(id,iswater)
   CALL nl_get_isice(id,isice)
   CALL nl_get_isurban(id,isurban)
!jm   CALL nl_get_mminlu( 1, mminlu_loc )
   CALL nl_get_mminlu( id, mminlu_loc )
#if (NMM_CORE == 1 && HWRF == 1 )
   IF ( trim(mminlu_loc) .eq. "" ) THEN
     CALL wrf_message('WARNING:   MMINLU NOT SET, USING USGS')
     MMINLU_LOC = 'USGS'
   ENDIF
#endif
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_isice, nl_get_mminlu_loc')
!-- temporary fix by ww
   landuse_ISICE = isice

!..Determine which cu_rad_feedback option to use

   icloud_cu = 0
   IF ( config_flags%cu_rad_feedback ) THEN
      IF ( config_flags%cu_physics == kfetascheme ) THEN
         icloud_cu = 2
      ELSE IF ( config_flags%cu_physics == gfscheme .OR. &
                config_flags%cu_physics == g3scheme .OR. &
                config_flags%cu_physics == gdscheme ) THEN
         icloud_cu = 1
      END IF
   END IF
#if (EM_CORE == 1)
   IF ( config_flags%cu_physics == mskfscheme) THEN
        icloud_cu = 2
   END IF

!--------------------INPUT FOR AEROSOL DATA-----------------------
!-----------------------------PSH/TWG-----------------------------
   IF ( aercu_opt .GT. 0 .AND. id .EQ. 1 ) THEN
      CALL aerosol_in_cu(aeromcu,alevsiz_cu,num_months,no_src_types_cu,XLAT,XLONG,aeropcu,&
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
   ENDIF
!-----------------------------PSH/TWG-----------------------------

! Initialize Cumulus Hydrometeors
    if (config_flags%aercu_opt == 1 )then
     do j=jts,jtf
       do k=kts,ktf
          do i=its,itf
            QC_CU(i,k,j) = 0.
            QI_CU(i,k,j) = 0.
            QR_CU(i,k,j) = 0.
            QS_CU(i,k,j) = 0.
            NC_CU(i,k,j) = 0.
            NI_CU(i,k,j) = 0.
            NR_CU(i,k,j) = 0.
            NS_CU(i,k,j) = 0.
            CCN_CU(i,k,j) = 0.
            EFCG(i,k,j) = 2.51
            EFCS(i,k,j) = 2.51
            EFIS(i,k,j) = 5.01
            EFIG(i,k,j) = 5.01
            EFSG(i,k,j) = 10.01
            EFSS(i,k,j) = 10.01
          end do
       end do
     end do
    endif

#endif
   CALL nl_set_icloud_cu ( id , icloud_cu )

  IF(.not.restart)THEN !{
!-- initialize common variables
     !BSINGH - When all the CAM parameterizations are not executed in WRF,
     !rliq can have undefined behaviour
 IF (config_flags%cu_physics == CAMZMSCHEME .or. config_flags%shcu_physics == CAMUWSHCUSCHEME ) THEN
     IF(PRESENT(rliq)) THEN
        rliq(:,:) = 0.0 
     ENDIF
 ENDIF
   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=float(config_flags%ideal_xland)
      GSW(i,j)=0.
      GLW(i,j)=0.
!-- initialize ust to a small value
      UST(i,j)=0.0001
      MOL(i,j)=0.0
      PBLH(i,j)=0.0
      HFX(i,j)=0.
      QFX(i,j)=0.
      RAINBL(i,j)=0.
      RAINNCV(i,j)=0.
      SNOWNCV(i,j)=0.
      GRAUPELNCV(i,j)=0.
      ACSNOW(i,j)=0.
      DO k=kms,kme  !wig, 17-May-2006: Added for idealized chem. runs
         EXCH_H(i,k,j) = 0.
      END DO
   ENDDO
   ENDDO
   ENDIF

   DO j=jts,jtf
   DO i=its,itf
     IVGTYP(i,j) = NINT(LU_INDEX(i,j))
   ENDDO
   ENDDO

!
   IF(PRESENT(TMN_UPDATE))THEN
   if(tmn_update.eq.1) then
   nyear=1
   nday=0.
   DO j=jts,jtf
   DO i=its,itf
      TYR(i,j)=TMN(i,j)
      TYRA(i,j)=0.0
      TDLY(i,j)=0.0
    DO n=1,lagday
      TLAG(i,n,j)=TMN(i,j)
    ENDDO
   ENDDO
   ENDDO
   endif
   ENDIF
!

!
   DO j=jts,jtf
   DO i=its,itf
     IF(XLAND(i,j) .LT. 1.5)THEN
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=ALBLND
       EMBCK(i,j)=0.85
       ALBEDO(i,j)=ALBBCK(i,j)
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZLND
#if  ! ( NMM_CORE == 1 )
       Z0(i,j)=ZZLND
#endif
       MAVAIL(i,j)=XMAVA
     ELSE
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=0.08
       ALBEDO(i,j)=ALBBCK(i,j)
       EMBCK(i,j)=0.98
       EMISS(i,j)=EMBCK(i,j)
       THC(i,j)=THINLD
       ZNT(i,j)=ZZWTR
#if  ! ( NMM_CORE == 1 )
       Z0(i,j)=ZZWTR
#endif
       MAVAIL(i,j)=1.0
     ENDIF

   ENDDO
   ENDDO

    if (config_flags%cu_diag == 1 )then
    do j=jts,jtf
       do k=kts,ktf
          do i=its,itf
             gd_cloud(i,k,j) = 0.
             gd_cloud2(i,k,j) = 0.
             gd_cloud_a(i,k,j) = 0.
             gd_cloud2_a(i,k,j) = 0.
             QC_CU(i,k,j) = 0.
             QI_CU(i,k,j) = 0.
          end do
       end do
    end do
    endif

    do j=jts,jtf
         do i=its,itf
           raincv_a(i,j)=0.
           raincv_b(i,j)=0.
      end do
    end do

!..Fill initial starting values of radiative effective radii for
!.. cloud water (2.51 microns), cloud ice (5.01 microns), and
!.. snow (10.01 microns).
    if (has_reqc.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_cloud(i,k,j) = 2.51E-6
          end do
          end do
       end do
    endif
    if (has_reqi.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_ice(i,k,j) = 5.01E-6
          end do
          end do
       end do
    endif
    if (has_reqs.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_snow(i,k,j) = 10.01E-6
          end do
          end do
       end do
    endif

!  Initialize cloud droplet effective radii for Goddard MP and/or Radiation
#if ( EM_CORE == 1 )

   if ( config_flags%ra_lw_physics .EQ. GODDARDLWSCHEME .OR. &
        config_flags%ra_sw_physics .EQ. GODDARDSWSCHEME .OR. &
        config_flags%mp_physics    .EQ. NUWRF4ICESCHEME ) THEN
      do j=jts,jtf
         do k=kts,ktf
            do i=its,itf
               re_cloud_gsfc(i,k,j)   = 0.
               re_rain_gsfc(i,k,j)    = 0.
               re_snow_gsfc(i,k,j)    = 0.
               re_ice_gsfc(i,k,j)     = 0.
               re_graupel_gsfc(i,k,j) = 0.
               re_hail_gsfc(i,k,j)    = 0.
            end do
         end do
      end do
   end if
#endif

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to landuse_init' )

   IF(mminlu_loc .ne. '    ')THEN
!-- initialize surface properties

     CALL landuse_init(lu_index, snowc, albedo, albbck, snoalb, mavail, emiss, embck, &
                znt, Z0, thc, xland, xice, xicem, julday, cen_lat, iswater, &
                TRIM ( mminlu_loc ) ,                               &
                landuse_ISICE, landuse_LUCATS,                      &
                landuse_LUSEAS, landuse_ISN,                        &
                config_flags%fractional_seaice,                      &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )
   ENDIF

  ENDIF !}

!-- convert zfull and zhalf to sigma values for ra_init (Eta CO2 needs these)
!-- zfull/zhalf may be either zeta or eta
!-- what is done here depends on coordinate (check this code if adding new coordinates)
   CALL z2sigma(zfull,zhalf,sfull,shalf,p_top,pptop,config_flags, &
                allowed_to_read,                                  &
                kds,kde,kms,kme,kts,kte)

   ! CAM parameterizations specific initializations [Currently incorporates initializations for CAMZM, CAMUWPBL, CAMUWSHCU and CAMMGMP parameterizations]

   !Findout whether CAMMGMP scheme is used used or not in this simulation
   is_CAMMGMP_used = .FALSE.
# if (EM_CORE == 1)
   if(config_flags%mp_physics == CAMMGMPSCHEME) is_CAMMGMP_used = .TRUE.
# endif

#if ( WRF_CHEM == 1 )
   !BSINGH:02/01/2013 - For WRF_CHEM simulations, initialize cam_mam_aerosols variable
   cam_mam_aerosols = .FALSE.
   if(config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM3_AQ &
        .OR. config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM7_AQ) cam_mam_aerosols = .TRUE.
      
#endif


   if(       config_flags%bl_pbl_physics == CAMUWPBLSCHEME     .OR. config_flags%cu_physics == CAMZMSCHEME      &
        .OR. config_flags%shcu_physics   == CAMUWSHCUSCHEME                                                     & 
# if (EM_CORE == 1)
        .OR. config_flags%mp_physics == CAMMGMPSCHEME                                                           &
# endif
#if ( WRF_CHEM == 1 )
        !For WRF_CHEM simulations, first five constituents are added in CAM_INIT and rest are added in the MODULE_CAM_MAM_INIT in chem/
        .OR. config_flags%chem_opt       == CBMZ_CAM_MAM3_NOAQ .OR. config_flags%chem_opt   == CBMZ_CAM_MAM3_AQ &
        .OR. config_flags%chem_opt       == CBMZ_CAM_MAM7_NOAQ .OR. config_flags%chem_opt   == CBMZ_CAM_MAM7_AQ &
#endif
      ) THEN
      CALL CAM_INIT(ixcldliq, ixcldice, ixnumliq, ixnumice, config_flags)
   ENDIF
!-- initialize physics
!-- ra: radiation
!-- bl: pbl
!-- cu: cumulus
!-- mp: microphysics

!
! .... paj ... initialize wind farm ...
!
  IF ( config_flags%windfarm_opt .EQ. 1 ) THEN
    CALL init_module_wind_fitch(id,config_flags,xlong,xlat,windfarm_initialized,ims,ime,jms,jme,its,ite,jts,jte,ids,ide,jds,jde)
  ENDIF


   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to ra_init' )

   CALL ra_init(id=id,STEPRA=STEPRA,RADT=RADT,DT=DT,RTHRATEN=RTHRATEN,RTHRATENLW=RTHRATENLW,             &
                RTHRATENSW=RTHRATENSW,CLDFRA=CLDFRA,EMISS=EMISS,cen_lat=cen_lat,JULYR=JULYR,JULDAY=JULDAY,GMT=GMT,    &
                levsiz=levsiz,XLAT=XLAT,XLONG=XLONG,n_ozmixm=n_ozmixm,     &
                alevsiz=alevsiz,no_src_types=no_src_types,                 &
                cldfra_dp=cldfra_dp,cldfra_sh=cldfra_sh,                   & ! optional for subgrid cloud by ckay
                cldfra_old=cldfra_old,                                     & ! Optional
                ozmixm=ozmixm,pin=pin,                                     & ! Optional
                aerodm=aerodm,pina=pina,                                   & ! Optional
                m_ps_1=m_ps_1,m_ps_2=m_ps_2,m_hybi=m_hybi,aerosolc_1=aerosolc_1,aerosolc_2=aerosolc_2,     & ! Optional
                paerlev=paerlev,n_aerosolc=n_aerosolc,                             &
                sfull=sfull,shalf=shalf,pptop=pptop,swrad_scat=swrad_scat,p_top=p_top,       &
                config_flags=config_flags,restart=restart,                           &
                allowed_to_read=allowed_to_read, start_of_simulation=start_of_simulation,           &
                ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde,                   &
                ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme,                   &
                its=its, ite=ite, jts=jts, jte=jte, kts=kts, kte=kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to bl_init' )
   CALL bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,        &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_PBL,mfshconv,               &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,MAVAIL,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
#if (NMM_CORE == 1)
                Z0,XLAND,XICE,                                  &
                DX,DY,                                          &
#else
                ZNT,XLAND,XICE,                                 &
                DX, DY, MSFTX, MSFTY,                           &
#endif
                QKE,                                            &
                SFCEVP,GRDFLX,                                  &
                TRIM (MMINLU_LOC),                              &
                XLAT,                                           &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,      &
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                GRAINXY, GDDXY,                                 & ! Noah-MP Crop model
                croptype, cropcat,                              & ! Noah-MP Crop model
                iopt_crop,                                      & 
                gecros_state,                                   & ! Optional gecros crop
                XSAIXY, LAI,                                    &
                SMOISEQ, SMCWTDXY, RECHXY, DEEPRECHXY, AREAXY,  &
                WTDDT, STEPWTD, QRFSXY ,QSPRINGSXY ,QSLATXY,   &
                FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & 
                rechclim  ,                                     &
                ISICE,                                 &
                T2MVXY,T2MBXY,CHSTARXY ,                        &
                allowed_to_read , iopt_run ,                    &
                start_of_simulation ,                           &
                lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & !lake
                snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & !lake
                z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & !lake
                h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & !lake
                zi3d,         watsat3d,     csol3d,     tkmg3d,         & !lake
                tkdry3d,      tksatu3d,     LakeModel,  lake2d,           & !lake
                lakedepth_default,            lake_min_elev, lake_depth,       & !lake
#if (EM_CORE == 1)
                lakemask, lakeflag,                                    & !lake
#endif
                lake_depth_flag, use_lakedepth,                        & !lake
                te_temf,cf3d_temf,wm_temf,                      & ! WA
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,          & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               & 
                SF_URBAN_PHYSICS,                               & !Optional urban
                CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,      & !Optional urban
                DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,            & !Optional urban
                FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,      & !Optional urban
                num_urban_ndm,                                  & !Optional multi-layer urban
                urban_map_zrd,                                  & !Optional multi-layer urban
                urban_map_zwd,                                  & !Optional multi-layer urban
                urban_map_gd ,                                  & !Optional multi-layer urban
                urban_map_zd ,                                  & !Optional multi-layer urban
                urban_map_zdf,                                  & !Optional multi-layer urban
                urban_map_bd ,                                  & !Optional multi-layer urban
                urban_map_wd ,                                  & !Optional multi-layer urban
                urban_map_gbd,                                  & !Optional multi-layer urban
                urban_map_fbd,                                  & !Optional multi-layer urban
                NUM_URBAN_HI,                                   & !Optional multi-layer urban
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & !Optional multi-layer urban
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & !Optional multi-layer urban
                TW1LEV_URB3D,TW2LEV_URB3D,                      & !Optional multi-layer urban
                TGLEV_URB3D,TFLEV_URB3D,                        & !Optional multi-layer urban
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & !Optional multi-layer urban
                SFVENT_URB3D,LFVENT_URB3D,                      & !Optional multi-layer urban
                SFWIN1_URB3D,SFWIN2_URB3D,                      & !Optional multi-layer urban
                SFW1_URB3D,SFW2_URB3D,                          & !Optional multi-layer urban
                SFR_URB3D,SFG_URB3D,                            & !Optional multi-layer urban
                LP_URB2D,HI_URB2D,LB_URB2D,                     & !Optional multi-layer urban
                HGT_URB2D,MH_URB2D,STDH_URB2D,                  & !Optional multi-layer urban
                LF_URB2D,                                       & !Optional multi-layer urban
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & !Optional multi-layer urban
                A_E_BEP,B_U_BEP,B_V_BEP,                        & !Optional multi-layer urban
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & !Optional multi-layer urban
                DL_U_BEP,SF_BEP,VL_BEP,                         & !Optional multi-layer urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, sf_ocean_physics,                     & !Optional oml
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML,              & !Optional oml
                is_CAMMGMP_used                                 &
               ,TSK_SAVE                                        & !Optional fractional seaice
! CLM vraiables 
               ,numc,nump,snl,                                      &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ht,maxpatch,                                        &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhveg,lhtran                      &
! end of CLM vraiables 
                ,landusef,landusef2,NLCAT                       & ! danli mosaic
                ,sf_surface_mosaic, mosaic_cat                  & ! danli mosaic
                ,mosaic_cat_index                               & ! danli mosaic  
                ,TSK_mosaic,TSLB_mosaic                         & ! danli mosaic
                ,SMOIS_mosaic,SH2O_mosaic                       & ! danli mosaic 
                ,CANWAT_mosaic,SNOW_mosaic                      & ! danli mosaic
                ,SNOWH_mosaic,SNOWC_mosaic                      & ! danli mosaic
                ,ALBEDO,ALBBCK, EMISS, EMBCK                    & ! danli mosaic          
                ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         ! danli mosaic
                ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  !danli mosaic 
                ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  !danli mosaic 
                ,QC_URB2D_mosaic                                &  !danli mosaic                  
                ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  !danli mosaic 
                ,TGL_URB3D_mosaic                               &  !danli mosaic 
                ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  !danli mosaic 
                ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  !danli mosaic 
                ,TS_URB2D_mosaic                                &  !danli mosaic 
                ,TS_RUL2D_mosaic                                &  !danli mosaic 
               ) 

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(DX,STEPCU,CUDT,DT,RUCUTEN,RVCUTEN,RTHCUTEN,     &
                RQVCUTEN,RQRCUTEN,RQCCUTEN,RQSCUTEN,RQICUTEN,   &
                NCA,RAINC,RAINCV,W0AVG,config_flags,restart,    &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
#if ( EM_CORE == 1 )
                !BSINGH - For WRFCuP Scheme
                cupflag,cldfra_cup,cldfratend_cup,              & !CuP, wig 18-Sep-2006
                shall,                                          & !CuP, wig 18-Sep-2006
                tcloud_cup,                                     & !CuP, rce 18-apr-2012
                !BSINGH -ENDS
#endif
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
                cugd_tten,cugd_ttens,cugd_qvten,                &
                cugd_qvtens,cugd_qcten,                         &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                RQCNCUTEN,RQINCUTEN)

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to shcu_init' )

   CALL shcu_init(STEPCU,CUDT,DT,RUSHTEN,RVSHTEN,RTHSHTEN,      &
                RQVSHTEN,RQRSHTEN,RQCSHTEN,                     &
                RQSSHTEN,RQISHTEN,RQGSHTEN,                     &
                NCA,RAINC,RAINCV,config_flags,restart,          &
                allowed_to_read, start_of_simulation,           &
                RDCASHTEN, RQCDCSHTEN, W0AVG          &
               ,cldareaa, cldareab, cldliqa, cldliqb  &
               ,ca_rad, cw_rad                  &
               ,pblmax, wub, ltopb, clddpthb, cldtopb &
               ,capesave, ainckfsa, radsave           &
               ,rainsh, rainshvb, kdcldtop, kdcldbas          &
                ,xtime1, PBLHAVG, TKEAVG,             &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to mp_init' )

   CALL mp_init(RAINNC,SNOWNC,GRAUPELNC,config_flags,restart,warm_rain,          &
                adv_moist_cond,                                 &
                MPDT, DT, DX, DY, LOWLYR,                       &
                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,               &
                mp_restart_state,tbpvs_state,tbpvs0_state,      &
                allowed_to_read, start_of_simulation,           &
!CAMMGMP specific variables
                ixcldliq, ixcldice, ixnumliq, ixnumice,         &
                nssl_cccn, nssl_alphah, nssl_alphahl,           &
                nssl_ipelec, nssl_isaund,                       &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                            &
                ccn_conc,                                        & ! RAS
                z_at_q, inv_dens, qnwfa2d, scalar, num_sc,      &  ! G. Thompson
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

#if  ( EM_CORE == 1 )
   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fg_init' )

   CALL fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,          &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
                SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH,    & ! fasdas
                HFX_FDDA,                                       & ! fasdas
                config_flags,restart,                           &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fdob_init' )

   CALL fdob_init(model_config_rec%obs_nudge_opt,               &
                  model_config_rec%max_dom,                     &
                  id,                                           &
                  model_config_rec%parent_id,                   &
                  model_config_rec%obs_idynin,                  &
                  model_config_rec%obs_dtramp,                  &
                  model_config_rec%fdda_end,                    &
                  model_config_rec%restart,                     &
                  obs_twindo_cg, obs_twindo,                    &
                  itimestep,                                    &
                  model_config_rec%obs_no_pbl_nudge_uv,         &
                  model_config_rec%obs_no_pbl_nudge_t,          &
                  model_config_rec%obs_no_pbl_nudge_q,          &
                  model_config_rec%obs_sfc_scheme_horiz,        &
                  model_config_rec%obs_sfc_scheme_vert,         &
                  model_config_rec%obs_max_sndng_gap,           &
                  model_config_rec%obs_sfcfact,                 &
                  model_config_rec%obs_sfcfacr,                 &
                  model_config_rec%obs_dpsmx,                   &
                  model_config_rec%obs_nudge_wind,              &
                  model_config_rec%obs_nudge_temp,              &
                  model_config_rec%obs_nudge_mois,              &
                  model_config_rec%obs_nudgezfullr1_uv,         &
                  model_config_rec%obs_nudgezrampr1_uv,         &
                  model_config_rec%obs_nudgezfullr2_uv,         &
                  model_config_rec%obs_nudgezrampr2_uv,         &
                  model_config_rec%obs_nudgezfullr4_uv,         &
                  model_config_rec%obs_nudgezrampr4_uv,         &
                  model_config_rec%obs_nudgezfullr1_t,          &
                  model_config_rec%obs_nudgezrampr1_t,          &
                  model_config_rec%obs_nudgezfullr2_t,          &
                  model_config_rec%obs_nudgezrampr2_t,          &
                  model_config_rec%obs_nudgezfullr4_t,          &
                  model_config_rec%obs_nudgezrampr4_t,          &
                  model_config_rec%obs_nudgezfullr1_q,          &
                  model_config_rec%obs_nudgezrampr1_q,          &
                  model_config_rec%obs_nudgezfullr2_q,          &
                  model_config_rec%obs_nudgezrampr2_q,          &
                  model_config_rec%obs_nudgezfullr4_q,          &
                  model_config_rec%obs_nudgezrampr4_q,          &
                  model_config_rec%obs_nudgezfullmin,           &
                  model_config_rec%obs_nudgezrampmin,           &
                  model_config_rec%obs_nudgezmax,               &
                  xlat,                                         &
                  xlong,                                        &
                  model_config_rec%start_year(id),              &
                  model_config_rec%start_month(id),             &
                  model_config_rec%start_day(id),               &
                  model_config_rec%start_hour(id),              &
                  model_config_rec%start_minute(id),            &
                  model_config_rec%start_second(id),            &
                  p00, t00, tlp,                                &
                  zhalf, p_top,                                 &
                  fdob,                                         &
                  model_config_rec%obs_ipf_init,                &
                  ids, ide, jds, jde, kds, kde,                 &
                  ims, ime, jms, jme, kms, kme,                 &
                  its, ite, jts, jte, kts, kte                  )

#endif
#if ( WRF_CHEM == 1)
   if ((config_flags%chem_opt == gocart_simple .or. &
        config_flags%chem_opt == gocartracm_kpp .or. &
        config_flags%chem_opt == 302 .or. &
        config_flags%chem_opt == gocartradm2) .and. &
        config_flags%gsfcgce_gocart_coupling == 1) then
      CALL wrf_debug(200 , 'phy_init: Before call to makelut_ccn_icn')
      call makelut_ccn_icn
   end if
#endif

   END SUBROUTINE phy_init

!=====================================================================
   SUBROUTINE landuse_init(lu_index, snowc, albedo, albbck, snoalb, mavail, emiss, embck, &
                znt,Z0,thc,xland, xice, xicem, julday, cen_lat, iswater, mminlu,  &
                ISICE, LUCATS, LUSEAS, ISN,                         &
                fractional_seaice,                                  &
                lu_state,                                           &
                allowed_to_read , usemonalb ,                       &
                ids, ide, jds, jde, kds, kde,                       &
                ims, ime, jms, jme, kms, kme,                       &
                its, ite, jts, jte, kts, kte                       )

   USE module_wrf_error
   IMPLICIT NONE

!---------------------------------------------------------------------
   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: iswater, julday
   REAL    , INTENT(IN)           :: cen_lat
   CHARACTER(LEN=*), INTENT(IN)        :: mminlu
   LOGICAL,  INTENT(IN)           :: allowed_to_read , usemonalb
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: lu_index, snowc, xice, snoalb 
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT  ) :: albedo, albbck, mavail, emiss, &
                                                               embck,                         &
                                                               znt, Z0, thc, xland, xicem
   INTEGER , INTENT(INOUT)  :: ISICE, LUCATS, LUSEAS, ISN, fractional_seaice
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL :: xice_threshold
!---------------------------------------------------------------------
! Local
   CHARACTER*256 LUTYPE
   CHARACTER*512 :: message
   INTEGER  :: landuse_unit, LS, LC, LI, LUN, NSN
   INTEGER  :: i, j, itf, jtf, is, cats, seas, curs
   INTEGER , PARAMETER :: OPEN_OK = 0
   INTEGER :: ierr
   INTEGER , PARAMETER :: max_cats = 100 , max_seas = 12
   REAL    , DIMENSION( max_cats, max_seas ) :: ALBD, SLMO, SFEM, SFZ0, THERIN, SFHC
   REAL    , DIMENSION( max_cats )     :: SCFX
! save these fields in case nest moves or has to be reinitialized
! and this routine is called with allowed_to_read set to false
! note that by saving these, we're locking in the same landuse for
! the duration of a run; possible implications for long climate runs
   LOGICAL :: found_lu, end_of_file
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

!---------------------------------------------------------------------

   CALL wrf_debug( 100 , 'top of landuse_init' )

   NSN=-1  ! set this to suppress uninitalized data messages from tools

  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif

! recover LU variables from state
   IF ( 6*(max_cats*max_seas)+1*max_cats .GT. 7501 ) THEN
      WRITE(message,*)'landuse_init: lu_state overflow. Make Registry dimspec p > ',6*(max_cats*max_seas)+1*max_cats
   ENDIF
   curs = 1
   DO cats = 1, max_cats
     SCFX(cats) =           lu_state(curs)         ; curs = curs + 1
     DO seas = 1, max_seas
       ALBD(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SLMO(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFEM(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFZ0(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       SFHC(cats,seas) =    lu_state(curs)         ; curs = curs + 1
       THERIN(cats,seas) =  lu_state(curs)         ; curs = curs + 1
     ENDDO
   ENDDO

! Determine season (summer=1, winter=2)
   ISN=1
   IF(JULDAY.LT.105.OR.JULDAY.GT.288)ISN=2
   IF(CEN_LAT.LT.0.0)ISN=3-ISN

   FOUND_LU = .TRUE.
   IF ( allowed_to_read ) THEN
      landuse_unit = 29
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(landuse_unit, FILE='LANDUSE.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF ( ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_physics_init.F: LANDUSE_INIT: open failure for LANDUSE.TBL'
          CALL wrf_error_fatal ( message )
        END IF
        REWIND(landuse_unit)
      ENDIF

! Read info from file LANDUSE.TBL
!     IF(MMINLU.EQ.'OLD ')THEN
!       ISWATER=7
!       ISICE=11
!     ELSE IF(MMINLU.EQ.'USGS')THEN
!       ISWATER=16
!       ISICE=24
!     ELSE IF(MMINLU.EQ.'SiB ')THEN
!       ISWATER=15
!       ISICE=16
!     ELSE IF(MMINLU.EQ.'LW12')THEN
!       ISWATER=15
!       ISICE=3
!     ELSE IF (MMINLU .EQ. 'MODIFIED_IGBP_MODIS_NOAH') THEN
!       ISICE = 15
!     ELSE
!        call wrf_error_fatal ("INPUT LandUse not found: "//TRIM(MMINLU))
!     ENDIF
      call wrf_message ( 'INPUT LandUse = "' // TRIM(MMINLU) // '"' )
      FOUND_LU = .FALSE.
      end_of_file = .FALSE.
!!! BEGINNING OF 1999 LOOP
 1999 CONTINUE
      IF ( wrf_dm_on_monitor() ) THEN
        READ (landuse_unit,*,END=2002)LUTYPE
        GOTO 2003
 2002   CONTINUE
        CALL wrf_message( 'INPUT FILE FOR LANDUSE REACHED END OF FILE' )
        end_of_file = .TRUE.
 2003   CONTINUE
        IF ( .NOT. end_of_file ) READ (landuse_unit,*)LUCATS,LUSEAS
        FOUND_LU = LUTYPE.EQ.MMINLU
      ENDIF
      CALL wrf_dm_bcast_bytes (end_of_file, LWORDSIZE )
      IF ( .NOT. end_of_file ) THEN
        CALL wrf_dm_bcast_string(lutype, 256)
        CALL wrf_dm_bcast_bytes (lucats,  IWORDSIZE )
        CALL wrf_dm_bcast_bytes (luseas,  IWORDSIZE )
        CALL wrf_dm_bcast_bytes (found_lu,  LWORDSIZE )
        IF(FOUND_LU)THEN
          LUN=LUCATS
          NSN=LUSEAS
            IF(LUTYPE.NE.'SSIB') THEN !this is not really true for ssib lsm (fds)
            write(message,*) 'LANDUSE TYPE = "' // TRIM (LUTYPE) // '" FOUND',        &
                   LUCATS,' CATEGORIES',LUSEAS,' SEASONS',     &
                   ' WATER CATEGORY = ',ISWATER,               &
                   ' SNOW CATEGORY = ',ISICE
            call wrf_message(message)
            ENDIF
        ENDIF
        DO ls=1,luseas
          if ( wrf_dm_on_monitor() ) then
            READ (landuse_unit,*)
          endif
          DO LC=1,LUCATS
            IF(found_lu)THEN
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)LI,ALBD(LC,LS),SLMO(LC,LS),SFEM(LC,LS),        &
                           SFZ0(LC,LS),THERIN(LC,LS),SCFX(LC),SFHC(LC,LS)
              ENDIF
              CALL wrf_dm_bcast_bytes (LI,  IWORDSIZE )
              IF(LC.NE.LI)CALL wrf_error_fatal ( 'module_start: MISSING LANDUSE UNIT ' )
            ELSE
              IF ( wrf_dm_on_monitor() ) THEN
                READ (landuse_unit,*)
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF(NSN.EQ.1.AND.FOUND_LU) THEN
           ISN = 1
        END IF
        CALL wrf_dm_bcast_bytes (albd,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (slmo,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfem,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfz0,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (therin, max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (sfhc,   max_cats * max_seas * RWORDSIZE )
        CALL wrf_dm_bcast_bytes (scfx,   max_cats *            RWORDSIZE )
      ENDIF

      IF(.NOT. found_lu .AND. .NOT. end_of_file ) GOTO 1999
!!! END OF 1999 LOOP

      IF(.NOT. found_lu .OR. end_of_file )THEN
        CALL wrf_message ( 'LANDUSE IN INPUT FILE DOES NOT MATCH LUTABLE: TABLE NOT USED' )
      ENDIF
    ENDIF  ! allowed_to_read

    IF(FOUND_LU)THEN
! Set arrays according to lu_index
      itf = min0(ite, ide-1)
      jtf = min0(jte, jde-1)
      IF(usemonalb)CALL wrf_message ( 'Climatological albedo is used instead of table values' )
      DO j = jts, jtf
        DO i = its, itf
          IS=nint(lu_index(i,j))
          ! only do this check on read-in data
          IF(allowed_to_read)THEN
             IF(IS.LT.0.OR.IS.GT.LUN)THEN
               WRITE ( wrf_err_message , * ) 'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J,' LUN= ',LUN
               CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
             ENDIF
          ENDIF
!   SET NO-DATA POINTS (IS=0) TO WATER
          IF(IS.EQ.0)THEN
            IS=ISWATER
          ENDIF
          IF(.NOT.usemonalb)ALBBCK(I,J)=ALBD(IS,ISN)/100.
          ALBEDO(I,J)=ALBBCK(I,J)
          IF(SNOWC(I,J) .GT. 0.5) THEN 
             IF (usemonalb) THEN
                 ALBEDO(I,J)=SNOALB(I,J)
             ELSE
                 ALBEDO(I,J)=ALBBCK(I,J)*(1.+SCFX(IS))
             ENDIF
          ENDIF
          THC(I,J)=THERIN(IS,ISN)/100.
          Z0(I,J)=SFZ0(IS,ISN)/100.
          ZNT(I,J)=Z0(I,J)
          EMBCK(I,J)=SFEM(IS,ISN)
          EMISS(I,J)=EMBCK(I,J)
          MAVAIL(I,J)=SLMO(IS,ISN)
          IF(IS.NE.ISWATER)THEN
            XLAND(I,J)=1.0
          ELSE
            XLAND(I,J)=2.0
          ENDIF
!    SET SEA-ICE POINTS TO LAND WITH ICE/SNOW SURFACE PROPERTIES
          XICEM(I,J)=XICE(I,J)
          IF(XICE(I,J).GE.xice_threshold)THEN
            XLAND(I,J)=1.0
            ALBBCK(I,J)=ALBD(ISICE,ISN)/100.
            EMBCK(I,J)=SFEM(ISICE,ISN)
            IF (FRACTIONAL_SEAICE == 1) THEN
               ! The 0.08 value is the albedo over open water.
               ! The 0.98 value is the emissivity over open water.
               ALBEDO(I,J) = ( XICE(I,J) * ALBBCK(I,J) ) + ( (1.0-XICE(I,J)) * 0.08 )
               EMISS(I,J)  = ( XICE(I,J) * EMBCK(I,J)  ) + ( (1.0-XICE(I,J)) * 0.98 )
            ELSE
               ALBEDO(I,J)=ALBBCK(I,J)
               EMISS(I,J)=EMBCK(I,J)
            ENDIF
            THC(I,J)=THERIN(ISICE,ISN)/100.
            Z0(I,J)=SFZ0(ISICE,ISN)/100.
            ZNT(I,J)=Z0(I,J)
            MAVAIL(I,J)=SLMO(ISICE,ISN)
          ENDIF
        ENDDO
      ENDDO
    ENDIF
    if ( wrf_dm_on_monitor() .and. allowed_to_read ) then
      CLOSE (landuse_unit)
    endif
    CALL wrf_debug( 100 , 'returning from of landuse_init' )

! restore LU variables from state
    curs = 1
    DO cats = 1, max_cats
      lu_state(curs) = SCFX(cats)                 ; curs = curs + 1
      DO seas = 1, max_seas
        lu_state(curs) = ALBD(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SLMO(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFEM(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFZ0(cats,seas)          ; curs = curs + 1
        lu_state(curs) = SFHC(cats,seas)          ; curs = curs + 1
        lu_state(curs) = THERIN(cats,seas)        ; curs = curs + 1
      ENDDO
    ENDDO
!
!-- fds (07/2010)
!-- SSIB's 12-category vegetation parameters are defined in module_ssib_veg
!-- Data in LANDUSE.TBL is only used temporarily until the first
!-- call to SSIB, and also to set water/ice points properties
!    IF(MMINLU.EQ.'SSIB')THEN
!       CALL init_module_ssib_veg
!       CALL wrf_message ( 'READING SSIB VEGETATION PARAMETERS' )
!    ENDIF
!

   END SUBROUTINE landuse_init

!=====================================================================
   SUBROUTINE ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       &
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      levsiz,XLAT,XLONG,n_ozmixm,                     &
                      alevsiz,no_src_types,                           &
                      cldfra_dp,cldfra_sh,                            & ! Optional for subgrid cloud by ckay
                      cldfra_old,                                     & ! Optional
                      ozmixm,pin,                                     & ! Optional
                      aerodm,pina,                                    & ! Optional
                      m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & ! Optional
                      paerlev,n_aerosolc,                             &
                      sfull,shalf,pptop,swrad_scat,p_top,            &
                      config_flags,restart,                          &
                      allowed_to_read, start_of_simulation,          &
                      ids, ide, jds, jde, kds, kde,                  &
                      ims, ime, jms, jme, kms, kme,                  &
                      its, ite, jts, jte, kts, kte                   )
!---------------------------------------------------------------------
   USE module_ra_rrtm      , ONLY : rrtminit
   USE module_ra_rrtmg_lw  , ONLY : rrtmg_lwinit
   USE module_ra_rrtmg_sw  , ONLY : rrtmg_swinit
#if( BUILD_RRTMG_FAST == 1)
   USE module_ra_rrtmg_lwf  , ONLY : rrtmg_lwinit_fast
   USE module_ra_rrtmg_swf  , ONLY : rrtmg_swinit_fast
#endif
#if (EM_CORE == 1)
   USE module_ra_rrtmg_lwk  , ONLY : rrtmg_lwinit_k
   USE module_ra_rrtmg_swk  , ONLY : rrtmg_swinit_k
#endif
   USE module_ra_cam       , ONLY : camradinit
   USE module_ra_cam_support , ONLY : oznini
   USE module_ra_sw        , ONLY : swinit
   USE module_ra_gsfcsw    , ONLY : gsfc_swinit
   USE module_ra_gfdleta   , ONLY : gfdletainit
   USE module_ra_goddard   , ONLY : init_goddardrad
#if(NMM_CORE==1)
   USE module_ra_hwrf      , ONLY : hwrfrainit
#endif
   USE module_ra_hs        , ONLY : hsinit
   USE module_domain
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   INTEGER,  INTENT(IN)           :: id
   TYPE (grid_config_rec_type)    :: config_flags
   LOGICAL , INTENT(IN)           :: restart
   LOGICAL,  INTENT(IN)           :: allowed_to_read

   INTEGER , INTENT(IN)           :: ids, ide, jds, jde, kds, kde,   &
                                     ims, ime, jms, jme, kms, kme,   &
                                     its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)           :: JULDAY,JULYR
   REAL ,    INTENT(IN)           :: DT, RADT, cen_lat, GMT, pptop,  &
                                     swrad_scat, p_top
   LOGICAL,  INTENT(IN)           :: start_of_simulation

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  XLAT, XLONG

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL,      &
          INTENT(INOUT) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, n_ozmixm-1, no_src_types ), OPTIONAL,      &
          INTENT(INOUT) ::                                  aerodm

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL, INTENT(INOUT)  ::         m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL,     &
          INTENT(INOUT) ::                      aerosolc_1, aerosolc_2

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::          PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(INOUT)  ::         PINA

   INTEGER , INTENT(INOUT)        :: STEPRA
   INTEGER :: isn

   REAL , DIMENSION( kms:kme ) , INTENT(IN) :: sfull, shalf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::           &!BSINGH(PNNL)- should be declared inout
                                                           RTHRATEN, &
                                                         RTHRATENLW, &
                                                         RTHRATENSW, &
                                                             CLDFRA

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) , & ! ckay for subgrid cloud
                                                         OPTIONAL :: &
                                                          cldfra_dp, &
                                                          cldfra_sh

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) :: &!BSINGH(PNNL)- should be declared inout
                                                         CLDFRA_OLD

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   LOGICAL :: hwrflw= .false.
   LOGICAL :: camlw = .false.
!   LOGICAL :: etamp = .false.
   LOGICAL :: acswalloc = .false.
   LOGICAL :: aclwalloc = .false.
   integer :: month,iday
   INTEGER :: i, j, k, itf, jtf, ktf
!---------------------------------------------------------------------

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

!---------------------------------------------------------------------

!-- calculate radiation time step

    STEPRA = nint(RADT*60./DT)
    STEPRA = max(STEPRA,1)

!-- initialization

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
        RTHRATEN(i,k,j)=0.
        RTHRATENLW(i,k,j)=0.
        RTHRATENSW(i,k,j)=0.
        CLDFRA(i,k,j)=0.
     ENDDO
     ENDDO
     ENDDO

     IF( PRESENT(cldfra_dp) ) THEN
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cldfra_dp(i,k,j)=0.
           cldfra_sh(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

     if( present(cldfra_old) ) then
        DO j=jts,jtf
        DO k=kts,ktf
        DO i=its,itf
           cldfra_old(i,k,j) = 0.
        ENDDO
        ENDDO
        ENDDO
     end if
   ENDIF

!-- ww: attempt to use CAM ozone and some aerosol profiles in all rad schemes
!   note that CAM option will still do the same. 
!   n_ozmixm: no of months; levsiz: = 59, vertical dim
!   Read in CAM ozone data, and interpolate data to model grid
!   Interpolation is done on domain 1 only

#if (EM_CORE==1) 
   IF ( config_flags%o3input .EQ. 2 .AND. id .EQ. 1 ) THEN
#else
   IF ( config_flags%o3input .EQ. 2 ) THEN
#endif
      CALL oznini(ozmixm,pin,levsiz,n_ozmixm,XLAT,                &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
   ENDIF

   IF ( config_flags%aer_opt .EQ. 1 .AND. id .EQ. 1 ) THEN
      CALL aerosol_in(aerodm,pina,alevsiz,n_ozmixm-1,no_src_types,XLAT,XLONG,   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
   ENDIF

!-- find out which microphysics option is used first

!   mp_select: SELECT CASE(config_flags%mp_physics)
!
!        CASE (ETAMPNEW)
!             etamp = .true.
!
!   END SELECT mp_select

!-- chose long wave radiation scheme

   lwrad_select: SELECT CASE(config_flags%ra_lw_physics)

        CASE (goddardlwscheme)
             CALL init_goddardrad(                          &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

              
        CASE (RRTMSCHEME)
             CALL rrtminit(                                 &
                           p_top, allowed_to_read ,         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (CAMLWSCHEME)
#ifdef MAC_KLUDGE
             CALL wrf_error_fatal ( 'CAM radiation scheme not supported under the chosen build configuration' )
#endif
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2)) THEN
             CALL camradinit(                                  &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop, &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,      &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling cam radiation' )
             ENDIF

             camlw = .true.
             aclwalloc = .true.

        CASE (RRTMG_LWSCHEME)
             CALL rrtmg_lwinit(                             &
                           p_top, allowed_to_read ,         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

            aclwalloc = .true.
#if ( EM_CORE == 1 )
        CASE (RRTMK_LWSCHEME)  

             CALL rrtmg_lwinit_k(                           &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             aclwalloc = .true.
#endif

#if( BUILD_RRTMG_FAST == 1)
        CASE (RRTMG_LWSCHEME_FAST)
             CALL rrtmg_lwinit_fast(                             &
                           p_top, allowed_to_read ,         &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             aclwalloc = .true.
#endif

        CASE (GFDLLWSCHEME)
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             etalw = .true.
#if(NMM_CORE==1)
        CASE (HWRFLWSCHEME)
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
! test this with standard jul-day calls
!     CALL nl_get_start_year(id,start_year)
!     CALL nl_get_start_month(id,start_month)
!     CALL nl_get_start_day(id,start_day)
!     CALL nl_get_start_hour(id,start_hour)
!     CALL nl_get_start_minute(id,start_minute)
!     CALL nl_get_start_second(id,start_second)
!     CALL jdn_sec(day_in_sec,start_year,start_month,start_day,0,0,0)
!     CALL jdn_sec(day_in_sec_ref,start_year,1,1,0,0,0)
!     julyr_start=start_year
!     julday_start=(day_in_sec-day_in_sec_ref)/(3600.*24.)+1
!     gmt_start=start_hour+real(start_minute)/60.+real(start_second)/3600.
              CALL hwrfrainit(sfull,shalf,pptop,JULYR,MONTH,IDAY,GMT,&
!             CALL hwrfrainit(sfull,shalf,pptop,JULYR_start,MONTH,IDAY,GMT_start,&
                            config_flags,allowed_to_read ,                         &
                           kds, kde, kms, kme, kts, kte     )
             hwrflw = .true.
#endif
        CASE (HELDSUAREZ)
             CALL hsinit(RTHRATEN,restart,             &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )

        CASE (FLGLWSCHEME)

        CASE DEFAULT

   END SELECT lwrad_select
!-- initialize short wave radiation scheme

   swrad_select: SELECT CASE(config_flags%ra_sw_physics)

        CASE (goddardswscheme)
             CALL init_goddardrad(                          &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

        CASE (SWRADSCHEME)
             CALL swinit(                                  &
                         swrad_scat,                       &
                         allowed_to_read ,                 &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )

        CASE (CAMSWSCHEME)
#ifdef MAC_KLUDGE
             CALL wrf_error_fatal ( 'CAM radiation scheme not supported under the chosen build configuration' )
#endif
             IF(.not.camlw)THEN
             CALL camradinit(                              &
                         R_D,R_V,CP,G,STBOLT,EP_2,shalf,pptop,               &
                         ozmixm,pin,levsiz,XLAT,n_ozmixm,     &
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,&
                         paerlev, n_aerosolc,              &
                         ids, ide, jds, jde, kds, kde,     &
                         ims, ime, jms, jme, kms, kme,     &
                         its, ite, jts, jte, kts, kte      )
             ENDIF
             acswalloc = .true.

        CASE (GSFCSWSCHEME)
             CALL gsfc_swinit(cen_lat, allowed_to_read )

        CASE (RRTMG_SWSCHEME)
             CALL rrtmg_swinit(                             &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             acswalloc = .true.

#if ( EM_CORE == 1 )
        CASE (RRTMK_SWSCHEME)

             CALL rrtmg_swinit_k(                           &
                           allowed_to_read ,                &
                          ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             acswalloc = .true.
#endif


#if( BUILD_RRTMG_FAST == 1)
        CASE (RRTMG_SWSCHEME_FAST)
             CALL rrtmg_swinit_fast(                             &
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             acswalloc = .true.
#endif


        CASE (GFDLSWSCHEME)
             IF(.not.etalw)THEN
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL gfdletainit(emiss,sfull,shalf,pptop,      &
                              julyr,month,iday,gmt,         &
                              config_flags,allowed_to_read, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )
             ENDIF
#if(NMM_CORE==1)
        CASE (HWRFSWSCHEME)
             IF(.not.hwrflw)THEN
             CALL nl_get_start_month(id,month)
             CALL nl_get_start_day(id,iday)
             CALL hwrfrainit(sfull,shalf,pptop,JULYR,MONTH,IDAY,GMT,&
                            config_flags,allowed_to_read,                          &
                           kds, kde, kms, kme, kts, kte     )
             ENDIF
#endif
        CASE (FLGSWSCHEME)

        CASE DEFAULT

   END SELECT swrad_select

#if ( EM_CORE == 1 ) 
   ! test for conditionally allocated arrays when using bucket_J

   IF(config_flags%bucket_J .gt. 0.0)THEN
     IF(.not. (acswalloc .and. aclwalloc))THEN
           CALL wrf_error_fatal ( 'Need CAM or RRTMG radiation for bucket_J option')
     ENDIF
   ENDIF
#endif

   END SUBROUTINE ra_init

   SUBROUTINE bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,  &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_PBL,mfshconv,               &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, &
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,mavail,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
#if  ( NMM_CORE == 1 )
                Z0,XLAND,XICE,DX,DY,                            &
#else
                ZNT,XLAND,XICE,DX, DY, MSFTX, MSFTY,            &
#endif
                QKE, SFCEVP,GRDFLX,                             &
                MMINLU,                                         &
                XLAT,                                           &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY, ALBOLDXY, QSNOWXY,      &
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                GRAINXY, GDDXY,                                 & ! Noah-MP Crop model
                croptype, cropcat,                              & ! Noah-MP Crop model
                iopt_crop,                                      &
                gecros_state,                                   & ! Optional gecros crop
                XSAIXY, LAI,                                    &
                SMOISEQ, SMCWTDXY, RECHXY, DEEPRECHXY, AREAXY,  &
                WTDDT, STEPWTD,QRFSXY ,QSPRINGSXY ,QSLATXY,     &
                FDEPTHXY, RIVERBEDXY, EQZWT, RIVERCONDXY, PEXPXY, & 
                rechclim  ,                                     &
                ISICE,                                 &
                T2MVXY, T2MBXY ,CHSTARXY,                       &
                allowed_to_read, iopt_run ,                     &
                start_of_simulation,                            &
                lakedepth2d,  savedtke12d,  snowdp2d,   h2osno2d,       & !lake
                snl2d,        t_grnd2d,     t_lake3d,   lake_icefrac3d, & !lake
                z_lake3d,     dz_lake3d,    t_soisno3d, h2osoi_ice3d,   & !lake
                h2osoi_liq3d, h2osoi_vol3d, z3d,        dz3d,           & !lake
                zi3d,         watsat3d,     csol3d,     tkmg3d,         & !lake
                tkdry3d,      tksatu3d,     LakeModel,  lake2d,           & !lake
                lakedepth_default,            lake_min_elev, lake_depth,       & !lake
#if (EM_CORE == 1)
                lakemask, lakeflag,                                      & !lake
#endif
                lake_depth_flag, use_lakedepth,                          & !lake
                te_temf,cf3d_temf,wm_temf,                      & ! WA
!                num_roof_layers,num_wall_layers,num_road_layers,& !Optional urban
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,             & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               &
                SF_URBAN_PHYSICS,                               & !Optional urban
                CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,      & !Optional urban
                DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,            & !Optional urban
                FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,      & !Optional urban
                num_urban_ndm,                                  & !Optional multi-layer urban
                urban_map_zrd,                                  & !Optional multi-layer urban
                urban_map_zwd,                                  & !Optional multi-layer urban
                urban_map_gd,                                   & !Optional multi-layer urban
                urban_map_zd,                                   & !Optional multi-layer urban
                urban_map_zdf,                                  & !Optional multi-layer urban
                urban_map_bd,                                   & !Optional multi-layer urban
                urban_map_wd,                                   & !Optional multi-layer urban
                urban_map_gbd,                                  & !Optional multi-layer urban
                urban_map_fbd,                                  & !Optional multi-layer urban
                NUM_URBAN_HI,                                   & !Optional multi-layer urban
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & !Optional multi-layer urban
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & !Optional multi-layer urban
                TW1LEV_URB3D,TW2LEV_URB3D,                      & !Optional multi-layer urban
                TGLEV_URB3D,TFLEV_URB3D,                        & !Optional multi-layer urban
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & !Optional multi-layer urban
                SFVENT_URB3D,LFVENT_URB3D,                      & !Optional multi-layer urban
                SFWIN1_URB3D,SFWIN2_URB3D,                      & !Optional multi-layer urban
                SFW1_URB3D,SFW2_URB3D,                          & !Optional multi-layer urban
                SFR_URB3D,SFG_URB3D,                            & !Optional multi-layer urban
                LP_URB2D,HI_URB2D,LB_URB2D,                     & !Optional multi-layer urban
                HGT_URB2D,MH_URB2D,STDH_URB2D,                  & !Optional multi-layer urban
                LF_URB2D,                                       & !Optional multi-layer urban
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & !Optional multi-layer urban
                A_E_BEP,B_U_BEP,B_V_BEP,                        & !Optional multi-layer urban
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & !Optional multi-layer urban
                DL_U_BEP,SF_BEP,VL_BEP,                         & !Optional multi-layer urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, sf_ocean_physics,                     & !Optional oml
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML,              &
                is_CAMMGMP_used                                 &
               ,TSK_SAVE                                        & !Optional fractional seaice
! CLM vraiables 
               ,numc,nump,snl,                                      &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ht,maxpatch,                                        &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhveg,lhtran                      &
! end of CLM vraiables 
                ,landusef,landusef2,NLCAT                       & ! danli mosaic
                ,sf_surface_mosaic, mosaic_cat                  & ! danli mosaic
                ,mosaic_cat_index                               & ! danli mosaic  
                ,TSK_mosaic,TSLB_mosaic                         & ! danli mosaic
                ,SMOIS_mosaic,SH2O_mosaic                       & ! danli mosaic
                ,CANWAT_mosaic,SNOW_mosaic                      & ! danli mosaic
                ,SNOWH_mosaic,SNOWC_mosaic                      & ! danli mosaic
                ,ALBEDO,ALBBCK, EMISS, EMBCK                    & ! danli mosaic 
                ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         ! danli mosaic
                ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  !danli mosaic 
                ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  !danli mosaic 
                ,QC_URB2D_mosaic                                &  !danli mosaic                  
                ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  !danli mosaic 
                ,TGL_URB3D_mosaic                               &  !danli mosaic 
                ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  !danli mosaic 
                ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  !danli mosaic 
                ,TS_URB2D_mosaic                                &  !danli mosaic 
                ,TS_RUL2D_mosaic                                &  !danli mosaic  
                                                                ) !Optional oml
!--------------------------------------------------------------------
   USE module_sf_sfclay
   USE module_sf_sfclayrev
   USE module_sf_slab
   USE module_sf_pxsfclay
   USE module_bl_ysu
   USE module_bl_shinhong
   USE module_bl_mrf
   USE module_bl_gfs
#if (HWRF==1)
   USE module_bl_gfsedmf, only : gfsedmfinit
#endif
   USE module_bl_acm
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_noahdrv
   USE module_sf_noahlsm, only : LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL
   USE module_sf_noahmpdrv
   USE noahmp_tables, ONLY: LOW_DENSITY_RESIDENTIAL_TABLE, HIGH_DENSITY_RESIDENTIAL_TABLE, HIGH_INTENSITY_INDUSTRIAL_TABLE
#ifdef WRF_USE_CLM
   USE module_sf_clm, only : clminit
#endif
   USE module_sf_urban
   USE module_sf_bep                                  !BEP
   USE module_sf_bep_bem
   USE module_sf_ruclsm
   USE module_sf_pxlsm
   USE module_sf_oml
   USE module_bl_myjpbl
   USE module_bl_myjurb
   USE module_bl_boulac
   USE module_bl_camuwpbl_driver, ONLY :  camuwpblinit
   USE module_bl_qnsepbl
   USE module_sf_lake
   USE module_bl_mfshconvpbl
   USE module_bl_gbmpbl
#if ( EM_CORE == 1 )
   USE module_bl_mynn
   USE module_bl_temf
#if ( WRFPLUS == 1 )
   USE module_bl_surface_drag
#endif
   USE module_sf_temfsfclay
   USE module_sf_mynn
#endif

#if (NMM_CORE == 1)
   USE module_sf_gfdl
#endif
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL, INTENT(IN)         ::   FNDSOILW, FNDSNOWH
   LOGICAL, INTENT(IN)         ::   RDMAXALB
   LOGICAL, INTENT(IN)         :: is_CAMMGMP_used !BSINGH:02/01/2013: For CAMUWPBL scheme

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     SF_URBAN_PHYSICS 
   INTEGER , INTENT(IN)        ::     IOPT_RUN

!   INTEGER , INTENT(IN)        ::     LakeModel
   INTEGER         ::     LakeModel
   real, intent(in)            ::      lakedepth_default,lake_min_elev
   REAL ,    INTENT(IN)        ::     DT, BLDT
   REAL ,    INTENT(IN)        ::     DX, DY
#if (EM_CORE == 1)
   REAL,    DIMENSION( ims:ime, jms:jme ) ,  INTENT(IN) :: MSFTX,MSFTY
#endif
   INTEGER , INTENT(INOUT)     ::     STEPBL

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),    &
             INTENT(OUT) :: SMFR3D

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers , jms:jme ),&
                   INTENT(INOUT) :: SMOIS,SH2O,TSLB

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        SNOALB, &
                                                        CANWAT, &
                                                        MAVAIL, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM, &
                                                        SFCEVP, &
                                                        GRDFLX, &
                                                           UST, &
#if ( NMM_CORE == 1 )
                                                            Z0, &
#else
                                                           ZNT, &
#endif
                                                         XLAND, &
                                                         XICE

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP, &
                                                        LOWLYR


   REAL,     DIMENSION(1:num_soil_layers), INTENT(INOUT)  ::  ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::       &!BSINGH(PNNL)- should be declared inout
                                                           RUBLTEN, &
                                                           RVBLTEN, &
                                                          EXCH_H,   &
                                                          RTHBLTEN, &
                                                          RQVBLTEN, &
                                                          RQCBLTEN, &
                                                          RQIBLTEN, &
                                                          TKE_PBL
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT), OPTIONAL :: &
                              massflux_EDKF, entr_EDKF, detr_EDKF & 
                                     ,thl_up, thv_up, rt_up       &
                                     ,rv_up, rc_up, u_up, v_up    &
                                     ,frac_up

   INTEGER, INTENT(IN)           ::  mfshconv ! WRF JP

   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   CHARACTER(LEN=*), INTENT(IN)   :: MMINLU
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER,  INTENT(IN)           :: ISURBAN
   INTEGER :: isn, isfc
   INTEGER :: k

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
            INTENT(OUT) :: te_temf, cf3d_temf !WA 
   REAL, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
            INTENT(OUT) :: wm_temf

!Noah-MP

   REAL,    DIMENSION(ims:ime, jms:jme) :: XLAT
   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ISNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:num_soil_layers, jms:jme) :: ZSNSOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: TSNOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,-2:0, jms:jme) :: SNLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TGXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANICEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CANLIQXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: EAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: TAHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CMXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FWETXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SNEQVOXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ALBOLDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSNOWXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WSLAKEXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: ZWTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WTXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LFMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RTMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STMASSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: WOODXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: GRAINXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: GDDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: STBLCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: FASTCPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: XSAIXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: LAI
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MVXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: T2MBXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: CHSTARXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,1:num_soil_layers,jms:jme) :: SMOISEQ
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: SMCWTDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: DEEPRECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RECHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QRFSXY  
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSPRINGSXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QSLATXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: AREAXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: FDEPTHXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RIVERBEDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: EQZWT
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: RIVERCONDXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: PEXPXY
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: rechclim
   INTEGER, OPTIONAL, DIMENSION(ims:ime,  jms:jme), INTENT(OUT) :: CROPCAT
   REAL,    OPTIONAL, DIMENSION(ims:ime,5,jms:jme), INTENT(IN ) :: CROPTYPE
   INTEGER , INTENT(IN)        ::     iopt_crop
   REAL,    OPTIONAL, DIMENSION(ims:ime,60,jms:jme) :: gecros_state                  ! Optional gecros crop
   INTEGER, OPTIONAL, INTENT(IN)  ::  ISICE
   INTEGER , OPTIONAL,  INTENT(OUT) :: STEPWTD
   REAL , OPTIONAL, INTENT(IN) :: WTDDT

!URBAN
!   REAL, DIMENSION(1:num_roof_layers), INTENT(INOUT) :: DZR  !Optional urban
!   REAL, DIMENSION(1:num_wall_layers), INTENT(INOUT) :: DZB  !Optional urban
!   REAL, DIMENSION(1:num_road_layers), INTENT(INOUT) :: DZG  !Optional urban
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZR  !Optional urban
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZB  !Optional urban
    REAL, OPTIONAL, DIMENSION(1:num_soil_layers), INTENT(INOUT) :: DZG  !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D    !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D     !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D !Optional urban
    INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D !Optional urban
!    REAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D !Optional urban
!    REAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D !Optional urban
!    REAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D !Optional urban
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D !Optional urban

    INTEGER , INTENT(IN)        ::     num_urban_ndm
    INTEGER , INTENT(IN)        ::     urban_map_zrd
    INTEGER , INTENT(IN)        ::     urban_map_zwd
    INTEGER , INTENT(IN)        ::     urban_map_gd
    INTEGER , INTENT(IN)        ::     urban_map_zd
    INTEGER , INTENT(IN)        ::     urban_map_zdf
    INTEGER , INTENT(IN)        ::     urban_map_bd
    INTEGER , INTENT(IN)        ::     urban_map_wd
    INTEGER , INTENT(IN)        ::     urban_map_gbd
    INTEGER , INTENT(IN)        ::     urban_map_fbd
    INTEGER , INTENT(IN)        ::     num_urban_hi
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: TRB_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: TW1_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: TW2_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: TGB_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: TLEV_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: QLEV_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: SFW1_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: SFW2_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: SFR_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: SFG_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime,1:num_urban_hi, jms:jme ), INTENT(INOUT) :: HI_URB2D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LP_URB2D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LB_URB2D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: HGT_URB2D !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: MH_URB2D !SLUCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: STDH_URB2D !SLUCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(INOUT) :: LF_URB2D !SLUCM
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: A_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_U_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_V_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_T_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_Q_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: B_E_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: VL_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DLG_BEP 
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme,jms:jme),INTENT(INOUT) :: SF_BEP
   REAL, OPTIONAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: DL_U_BEP
! lake varibles:
  real,    dimension(ims:ime,jms:jme ),intent(out)                        :: lakedepth2d,    &
                                                                             savedtke12d
  real,    dimension(ims:ime,jms:jme ),intent(inout)                      :: snowdp2d,       &
                                                                             h2osno2d,       &
                                                                             snl2d,          &
                                                                             t_grnd2d
 
  real,    dimension( ims:ime,1:nlevlake, jms:jme ),INTENT(out)            :: t_lake3d,       &
                                                                             lake_icefrac3d, &
                                                                             z_lake3d,       &
                                                                             dz_lake3d
  real,    dimension( ims:ime,-nlevsnow+1:nlevsoil, jms:jme ),INTENT(inout) :: t_soisno3d,     &
                                                                             h2osoi_ice3d,   &
                                                                             h2osoi_liq3d,   &
                                                                             h2osoi_vol3d,   &
                                                                             z3d,            &
                                                                             dz3d
  real,    dimension( ims:ime,1:nlevsoil, jms:jme ),INTENT(out)            :: watsat3d,       &
                                                                             csol3d,         &
                                                                             tkmg3d,         &
                                                                             tkdry3d,        &
                                                                             tksatu3d
  real,    dimension( ims:ime,-nlevsnow+0:nlevsoil, jms:jme ),INTENT(inout) :: zi3d
 
  logical,    dimension(ims:ime,jms:jme ),intent(out)                        :: lake2d
  REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  lake_depth
#if ( EM_CORE == 1 )
  REAL,              dimension(ims:ime,jms:jme ),intent(inout)      ::  lakemask
  INTEGER, INTENT(IN)      ::  lakeflag
#endif
  INTEGER, INTENT(IN)      ::   use_lakedepth
  INTEGER, INTENT(INOUT)      ::   lake_depth_flag
!  REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)                         ::  HT

   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        ACHFX,ACLHF,ACGRDFLX
! Optional OML variables
   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TML,T0ML,HML,H0ML,HUML,HVML,TMOML
   INTEGER,  OPTIONAL,  INTENT(IN) :: sf_ocean_physics
   REAL,  OPTIONAL,  INTENT(IN) :: oml_hml0
   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TSK_SAVE
   LOGICAL,  INTENT(IN) :: start_of_simulation
   INTEGER :: i,j

! CLM varaibles
   INTEGER , INTENT(IN)        ::     maxpatch
   REAL, OPTIONAL,    DIMENSION( ims:ime, jms:jme ), INTENT(IN)    ::  HT
  integer, OPTIONAL,   dimension(ims:ime,jms:jme ),intent(inout) :: numc,nump
  integer, OPTIONAL,   dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) :: snl
  real, OPTIONAL,  dimension(ims:ime,1:maxpatch,jms:jme ),intent(inout) ::  &
                snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                h2osoi_liq_s1,              &
                h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                h2osoi_ice_s1,h2osoi_ice_s2,                        &
                h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                t_soisno8,t_soisno9,t_soisno10,                     &
                dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                h2osoi_vol7,h2osoi_vol8,                            &
                h2osoi_vol9,h2osoi_vol10,                           &
                ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                SWUPsubgrid,lhsoi,lhtran,lhveg

!-------------------------------------------------
! Noah-mosaic related variables are added to declaration  (danli)
!-------------------------------------------------
  
  INTEGER, INTENT(IN) :: sf_surface_mosaic, NLCAT   
  INTEGER, INTENT(IN) :: mosaic_cat
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(IN)::   LANDUSEF
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(INOUT)::   LANDUSEF2
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT) :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic 
  REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(INOUT)::   ALBEDO,ALBBCK, EMISS, EMBCK 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic
  
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT)::   TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic,  &
                                                                                     SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                    
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic  
   
  REAL :: xice_threshold   ! lake input
   
  LOGICAL :: IPRINT
  
!-------------------------------------------------
! End of Noah-mosaic related variables 
!-------------------------------------------------  

#if ( EM_CORE == 1 )
!local mynn 
   INTEGER :: mynn_closure_level
#endif

  if ( config_flags%fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( config_flags%fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif
 
!-- calculate pbl time step

   STEPBL = nint(BLDT*60./DT)
   STEPBL = max(STEPBL,1)
!-- initialization

   IF(PRESENT(ACHFX))THEN
   IF(.not.restart)THEN
     DO j=jts,jte
     DO i=its,ite
        ACHFX(i,j)=0.
        ACLHF(i,j)=0.
        ACGRDFLX(i,j)=0.
        SFCEVP(i,j)=0.
     ENDDO
     ENDDO
   ENDIF
   ENDIF

   IF(PRESENT(TSK_SAVE))THEN
   IF(.NOT.restart .AND. config_flags%fractional_seaice.EQ.1)THEN
     DO j=jts,jte
     DO i=its,ite
        TSK_SAVE(i,j)=TSK(i,j)
     ENDDO
     ENDDO
   ENDIF
   ENDIF

!-- initialize surface layer scheme

   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
           isfc = 1
      CASE (SFCLAYREVSCHEME)
            CALL sfclayrevinit
           isfc = 1
      CASE (PXSFCSCHEME)
           CALL pxsfclayinit( allowed_to_read )
           isfc = 7
      CASE (MYJSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
#if ( NMM_CORE == 1 )
                                      Z0,                      &
#else
                                      ZNT,                     &
#endif
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2

      CASE (QNSESFCSCHEME)
           CALL qnsesfcinit(LOWLYR,UST,                         &
#if ( NMM_CORE == 1 )
                                      Z0,                      &
#else
                                      ZNT,                     &
#endif
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 4

      CASE (GFSSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
#if ( NMM_CORE == 1 )
                                      Z0,                      &
#else
                                      ZNT,                     &
#endif
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2
#if (NMM_CORE==1)
      CASE (GFDLSFCSCHEME)
           CALL myjsfcinit(LOWLYR,UST,                         &
                                      Z0,                      &
                                          XLAND,XICE,          &
                         IVGTYP,restart,                       &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
           isfc = 2
#endif

#if ( EM_CORE == 1 )
!mynn 

        CASE (MYNNSFCSCHEME)

           CALL mynn_sf_init_driver(allowed_to_read)
           isfc=5
!          isfc=3

      CASE (TEMFSFCSCHEME)
           CALL wrf_debug( 100, 'calling temfsfclayinit' )
           CALL temfsfclayinit( restart, allowed_to_read ,        &
                             wm_temf,                          &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
#endif

      CASE DEFAULT

   END SELECT sfclay_select


!-- initialize surface scheme

   sfc_select: SELECT CASE(config_flags%sf_surface_physics)

      CASE (SLABSCHEME)

           CALL slabinit(TSK,TMN,                              &
                         TSLB,ZS,DZS,num_soil_layers,          &
                         allowed_to_read ,start_of_simulation ,&
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )

#if (NMM_CORE == 1)
     CASE (GFDLSLAB)
           CALL hwrfsfcinit(isn,XICE,VEGFRA,SNOW,SNOWC, CANWAT,SMSTAV, &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,     &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,DZS,SFCEVP,   &
                     TMN,                                          &
                     num_soil_layers,                              &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

#endif
      CASE (LSMSCHEME)
          
          IF(TRIM(mminlu) .EQ. 'NLCD40')THEN
            CALL wrf_message('Using NLCD40 for Noah, redefine urban categories ')
            DO j=jts,jte
            DO i=its,ite
               IF(IVGTYP(i,j)==23) IVGTYP(i,j) = 24
               IF(IVGTYP(i,j)==25) IVGTYP(i,j) = 24
            ENDDO
            ENDDO
          ENDIF
 
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )


!URBAN
          IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN

             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN
                
                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers,                   & !urban
                                sf_urban_physics)
!                                num_roof_layers,num_wall_layers,road_soil_layers)   !urban
                               
                
                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,                     & !urban
                              ims,ime,jms,jme,kms,kme,num_soil_layers,               & !urban
!                              num_roof_layers,num_wall_layers,num_road_layers, & !urban
                              LOW_DENSITY_RESIDENTIAL,                       &
                              HIGH_DENSITY_RESIDENTIAL,                      &
                              HIGH_INTENSITY_INDUSTRIAL,                       &
                              restart,sf_urban_physics,                        & !urban
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & !urban
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & !urban
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & !urban
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & !urban
                              num_urban_ndm,                                   & !urban
                              urban_map_zrd,                                   & !urban
                              urban_map_zwd,                                   & !urban
                              urban_map_gd,                                    & !urban
                              urban_map_zd,                                    & !urban
                              urban_map_zdf,                                   & !urban
                              urban_map_bd,                                    & !urban
                              urban_map_wd,                                    & !urban
                              urban_map_gbd,                                   & !urban
                              urban_map_fbd,                                   & !urban
                              num_urban_hi,                                    & !urban
                              TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,         & !urban
                              TLEV_URB3D,QLEV_URB3D,                           & !urban
                              TW1LEV_URB3D,TW2LEV_URB3D,                       & !urban
                              TGLEV_URB3D,TFLEV_URB3D,                         & !urban
                              SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,             & !urban
                              SFVENT_URB3D,LFVENT_URB3D,                       & !urban
                              SFWIN1_URB3D,SFWIN2_URB3D,                       & !urban
                              SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,       & !urban
                              LP_URB2D,HI_URB2D,LB_URB2D,                      & !urban
                              HGT_URB2D,MH_URB2D,STDH_URB2D,                   & !urban
                              LF_URB2D,                                        & !urban
                              CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,       & !urban
                              DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,             & !urban
                              FLXHUMR_URB2D, FLXHUMB_URB2D, FLXHUMG_URB2D,     & !urban
                              A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                 & !multi-layer urban
                              A_E_BEP,B_U_BEP,B_V_BEP,                         & !multi-layer urban
                              B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                 & !multi-layer urban
                              DL_U_BEP,SF_BEP,VL_BEP,                          & !multi-layer urban
                              FRC_URB2D, UTYPE_URB2D)                            !urban
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling urban model' )
             ENDIF
          ENDIF
          
!danli mosaic

          IF (SF_surface_mosaic.eq.1) THEN

CALL lsm_mosaic_init(IVGTYP,config_flags%ISWATER,config_flags%ISURBAN,config_flags%ISICE, XLAND, XICE,config_flags%fractional_seaice,TSK,TSLB,SMOIS,SH2O,SNOW,SNOWC,SNOWH,CANWAT,  &                
                  ids,ide, jds,jde, kds,kde,  &
                  ims,ime, jms,jme, kms,kme,  &
                  its,ite, jts,jte, kts,kte, restart,             &
                  landusef,landusef2,NLCAT,num_soil_layers                  & 
                  ,sf_surface_mosaic, mosaic_cat                    & 
                  , mosaic_cat_index                              &   
                  ,TSK_mosaic,TSLB_mosaic                         &
                  ,SMOIS_mosaic,SH2O_mosaic                       & 
                  ,CANWAT_mosaic,SNOW_mosaic                      &
                  ,SNOWH_mosaic,SNOWC_mosaic                      &
                  ,ALBEDO,ALBBCK, EMISS, EMBCK,                    &         !danli  
#if ( NMM_CORE == 1 )
                                                            Z0, &
#else
                                                           ZNT, &
#endif  
                  ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic   &         !danli
                 ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  !danli mosaic 
                 ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  !danli mosaic 
                 ,QC_URB2D_mosaic                                &  !danli mosaic                  
                 ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  !danli mosaic 
                 ,TGL_URB3D_mosaic                               &  !danli mosaic 
                 ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  !danli mosaic 
                 ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  !danli mosaic 
                 ,TS_URB2D_mosaic                                &  !danli mosaic 
                 ,TS_RUL2D_mosaic                                &  !danli mosaic                        
                   )

          ENDIF               
          
!

      CASE (NOAHMPSCHEME)
          IF ( TRIM(mminlu) .EQ. 'NLCD40' ) THEN
             CALL wrf_error_fatal ( 'NoahMP does not work with NLCD data. Stop.' )
          ENDIF

          CALL NOAHMP_INIT(MMINLU, SNOW,SNOWH,CANWAT,ISLTYP,IVGTYP,XLAT,                 &
                     TSLB,SMOIS,SH2O,DZS, FNDSOILW, FNDSNOWH,          &
                     TSK,isnowxy  ,tvxy     ,tgxy     ,canicexy ,TMN,XICE,                  &
                     canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
                     fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
                     wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
                     stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,lai      ,           &
                     grainxy  ,gddxy    ,                                                   &
                     croptype ,cropcat  ,                      &
                     t2mvxy   ,t2mbxy   ,chstarxy ,            &
                     num_soil_layers, restart,                 &
                     allowed_to_read, iopt_run , iopt_crop,    &
                     sf_urban_physics,                         &  ! urban scheme
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 &
#if (EM_CORE == 1)
                     ,smoiseq  ,smcwtdxy ,rechxy   ,deeprechxy, areaxy ,dx, dy, msftx, msfty,&
                     wtddt    ,stepwtd  ,dt  ,qrfsxy ,qspringsxy  ,qslatxy,                  &
                     fdepthxy ,ht       ,riverbedxy ,eqzwt ,rivercondxy ,pexpxy,              &
                     rechclim,                  &
                     gecros_state                              & ! Optional gecros crop
#endif
                     )

          IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN
                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers,                   & !urban
                                sf_urban_physics)
                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,               & !urban
                              ims,ime,jms,jme,kms,kme,num_soil_layers,         & !urban
                              LOW_DENSITY_RESIDENTIAL_TABLE,                   &
		              HIGH_DENSITY_RESIDENTIAL_TABLE,                  &
		              HIGH_INTENSITY_INDUSTRIAL_TABLE,                 &
                              restart,sf_urban_physics,                        & !urban
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & !urban
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & !urban
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & !urban
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & !urban
                              num_urban_ndm,                                   & !urban
                              urban_map_zrd,                                   & !urban
                              urban_map_zwd,                                   & !urban
                              urban_map_gd,                                    & !urban
                              urban_map_zd,                                    & !urban
                              urban_map_zdf,                                   & !urban
                              urban_map_bd,                                    & !urban
                              urban_map_wd,                                    & !urban
                              urban_map_gbd,                                   & !urban
                              urban_map_fbd,                                   & !urban
                              num_urban_hi,                                    & !urban
                              TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,         & !urban
                              TLEV_URB3D,QLEV_URB3D,                           & !urban
                              TW1LEV_URB3D,TW2LEV_URB3D,                       & !urban
                              TGLEV_URB3D,TFLEV_URB3D,                         & !urban
                              SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,             & !urban
                              SFVENT_URB3D,LFVENT_URB3D,                       & !urban
                              SFWIN1_URB3D,SFWIN2_URB3D,                       & !urban
                              SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,       & !urban
                              LP_URB2D,HI_URB2D,LB_URB2D,                      & !urban
                              HGT_URB2D,MH_URB2D,STDH_URB2D,                   & !urban
                              LF_URB2D,                                        & !urban
                              CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,       & !urban
                              DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,             & !urban
                              FLXHUMR_URB2D, FLXHUMB_URB2D, FLXHUMG_URB2D,     & !urban
                              A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                 & !multi-layer urban
                              A_E_BEP,B_U_BEP,B_V_BEP,                         & !multi-layer urban
                              B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                 & !multi-layer urban
                              DL_U_BEP,SF_BEP,VL_BEP,                          & !multi-layer urban
                              FRC_URB2D, UTYPE_URB2D)                            !urban
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling urban model' )
             ENDIF
          ENDIF

      CASE (RUCLSMSCHEME)
!          if(isfc .ne. 2)CALL wrf_error_fatal &
!           ( 'module_physics_init: use myjsfc and myjpbl scheme for this lsm option' )
           CALL ruclsminit( SH2O,SMFR3D,TSLB,SMOIS,ISLTYP,IVGTYP,MMINLU,XICE,  &
                     mavail,num_soil_layers, config_flags%iswater,      &
#if (NMM_CORE == 1)
                     config_flags%isice, z0, restart,                  &
#else
                     config_flags%isice, znt, restart,                  &
#endif
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE (PXLSMSCHEME)
          if(config_flags%num_land_cat .ne. 20 .and. config_flags%num_land_cat .ne. 21 .and. & ! MODIS
             config_flags%num_land_cat .ne. 24 .and. config_flags%num_land_cat .ne. 28 .and. & ! USGS
             config_flags%num_land_cat .ne. 40 .and. config_flags%num_land_cat .ne. 50 )     & ! NLCD
          CALL wrf_error_fatal ( 'module_physics_init: PX LSM option requires USGS, MODIS, or NLCD' )
          CALL LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,  &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )

!--------------fds (06/2010)-----------------------------------
     CASE  (SSIBSCHEME)
!SSiB only works with sfclay and YSU schemes. Check this here!
          if(isfc .ne. 1)CALL wrf_error_fatal &
           ( 'module_physics_init: use sfclay scheme with SSiB' )
          if(config_flags%bl_pbl_physics .ne. 1)CALL wrf_error_fatal &
           ( 'module_physics_init: use ysu scheme with SSiB' )
! Add radiation scheme 4 (RRTMg) for SSiB, By Zhenxin 2011-06-20 ************************
          if(config_flags%ra_lw_physics .eq. 2 .or. config_flags%ra_lw_physics .gt. 4)CALL wrf_error_fatal &
           ( 'module_physics_init: SSiB only works with rrtm, cam scheme or rrtmg scheme (lw_phys=1,3,4)' )
          if(config_flags%ra_sw_physics .eq. 2 .or. config_flags%ra_sw_physics .gt. 4)CALL wrf_error_fatal &
           ( 'module_physics_init: SSiB only works with rrtm, cam scheme or rrtmg scheme (sw_phys=1,3,4)' )
! End of Adding radiation scheme 4 (RRTMg) for SSiB, By Zhenxin 2011-06-20 **************
          IF ( TRIM(mminlu) .EQ. 'NLCD40' ) THEN
             CALL wrf_error_fatal ( 'SSIB does not work with NLCD data. Stop.' )
          ENDIF
!--------------------------------------------------------------
! CLM Init Coupling
      CASE (CLMSCHEME)
        IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
                CALL wrf_error_fatal ( 'CLM DOES NOT WORK WITH URBAN SCHEME' ) 
        ENDIF
        IF ( TRIM(mminlu) .EQ. 'NLCD40' ) THEN
           CALL wrf_error_fatal ( 'CLM does not work with NLCD input. Stop' )
        ENDIF

#ifdef WRF_USE_CLM
        IF(PRESENT(numc))THEN
        ! added by Jiming Jin 10/14/2012 ; modified to use MODIS data 3/6/2014
        if((config_flags%num_land_cat .ne. 24) .AND. &
           (config_flags%num_land_cat .ne. 28) .AND. &
           (config_flags%num_land_cat .ne. 21) .AND. &
           (config_flags%num_land_cat .ne. 20)) CALL wrf_error_fatal &
            ('module_physics_init: USGS or MODIS must be used with CLM option')

            CALL CLMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,    &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,            &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS,  &
                     FNDSOILW, FNDSNOWH,                           &
                     num_soil_layers, restart,                     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     &
                    ,maxpatch                                            &
                    ,numc,nump,snl,                                      &
                     snowdp,wtc,wtp,h2osno,t_grnd,t_veg,         &
                     h2ocan,h2ocan_col,t2m_max,t2m_min,t_ref2m,          &
                     h2osoi_liq_s1,              &
                     h2osoi_liq_s2,h2osoi_liq_s3,h2osoi_liq_s4,          &
                     h2osoi_liq_s5,h2osoi_liq1,h2osoi_liq2,              &
                     h2osoi_liq3,h2osoi_liq4,h2osoi_liq5,h2osoi_liq6,    &
                     h2osoi_liq7,h2osoi_liq8,h2osoi_liq9,h2osoi_liq10,   &
                     h2osoi_ice_s1,h2osoi_ice_s2,                        &
                     h2osoi_ice_s3,h2osoi_ice_s4,h2osoi_ice_s5,          &
                     h2osoi_ice1,h2osoi_ice2,h2osoi_ice3,h2osoi_ice4,    &
                     h2osoi_ice5,h2osoi_ice6,h2osoi_ice7,                &
                     h2osoi_ice8,h2osoi_ice9,h2osoi_ice10,               &
                     t_soisno_s1,t_soisno_s2,t_soisno_s3,t_soisno_s4,    &
                     t_soisno_s5,t_soisno1,t_soisno2,t_soisno3,          &
                     t_soisno4,t_soisno5,t_soisno6,t_soisno7,            &
                     t_soisno8,t_soisno9,t_soisno10,                     &
                     dzsnow1,dzsnow2,dzsnow3,dzsnow4,dzsnow5,            &
                     snowrds1,snowrds2,snowrds3,snowrds4,snowrds5,       &
                     t_lake1,t_lake2,t_lake3,t_lake4,t_lake5,            &
                     t_lake6,t_lake7,t_lake8,t_lake9,t_lake10,           &
                     h2osoi_vol1,h2osoi_vol2,h2osoi_vol3,                &
                     h2osoi_vol4,h2osoi_vol5,h2osoi_vol6,                &
                     h2osoi_vol7,h2osoi_vol8,                            &
                     h2osoi_vol9,h2osoi_vol10,                           &
                     ht,XLAND,XICE                                       &
                    ,ALBEDOsubgrid,LHsubgrid,HFXsubgrid,LWUPsubgrid,     &
                     Q2subgrid,SABVsubgrid,SABGsubgrid,NRAsubgrid,       &
                     SWUPsubgrid,lhsoi,lhveg,lhtran                      &
                    )
        ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling CLM' )
        ENDIF
#else
        CALL wrf_error_fatal ( "SF CLM CURRENTLY ifdef'ed out, set -DWRF_USE_CLM in configure.wrf in ARCH_LOCAL" )
#endif

      CASE DEFAULT

   END SELECT sfc_select

#if ( EM_CORE == 1 )
   IF(PRESENT(SF_OCEAN_PHYSICS))THEN
     IF ( ( sf_ocean_physics .EQ. OMLSCHEME   ) .OR. &
          ( sf_ocean_physics .EQ. PWP3DSCHEME ) ) THEN
        CALL omlinit(oml_hml0, tsk,                           &
                     tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                     allowed_to_read, start_of_simulation,    &
                     ids,ide, jds,jde, kds,kde,               &
                     ims,ime, jms,jme, kms,kme,               &
                     its,ite, jts,jte, kts,kte                )
     ENDIF
   ENDIF
#endif

     IF ( LakeModel == 1 ) THEN
 
             call  lakeini(IVGTYP,         ISLTYP,          HT,              SNOW,           & !i
                           lake_min_elev,     restart,         lakedepth_default, lake_depth,     &
                           lakedepth2d,    savedtke12d,     snowdp2d,        h2osno2d,       & !o
                           snl2d,          t_grnd2d,        t_lake3d,        lake_icefrac3d, &
                           z_lake3d,       dz_lake3d,       t_soisno3d,      h2osoi_ice3d,   &
                           h2osoi_liq3d,   h2osoi_vol3d,    z3d,             dz3d,           &
                           zi3d,           watsat3d,        csol3d,          tkmg3d,         &
                           config_flags%ISWATER,  xice,     xice_threshold,  xland,  tsk,    &
#if (EM_CORE == 1)
                           lakemask,  lakeflag,                                              &
#endif
                           lake_depth_flag, use_lakedepth,              &
                           tkdry3d,        tksatu3d,        lake2d,          its, ite, jts, jte, &
                           ims,ime, jms,jme)
     ENDIF
 
!-- initialize pbl scheme

   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: Use sf_sfclay_physics= 1 or 91 for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL ysuinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (SHINHONGSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: Use sf_sfclay_physics= 1 or 91 for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL shinhonginit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,&
                        RQCBLTEN,RQIBLTEN,TKE_PBL,P_QI,       &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MRFSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: Use sf_sfclay_physics= 1 or 91 for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL mrfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (ACMPBLSCHEME)
           if(isfc .ne. 1 .and. isfc .ne. 7)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay or pxsfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL acminit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (GFSSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
#if (HWRF == 1)
      CASE (GFSEDMFSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL gfsedmfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,&
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )

#endif
      CASE (MYJPBLSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
          IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
           CALL myjurbinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          ELSE

           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          END IF
      CASE (QNSEPBLSCHEME)
           if(isfc .ne. 4)CALL wrf_error_fatal &
            ( 'module_physics_init: use qnsesfc scheme for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL qnsepblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_PBL,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
           
!          IF ( PRESENT (mfshconv) ) THEN
              if (mfshconv.EQ.1) &
              CALL mfshconvpblinit( massflux_EDKF, entr_EDKF, detr_EDKF & 
                                    ,thl_up, thv_up, rt_up              &
                                    ,rv_up, rc_up, u_up, v_up           &
                                    ,frac_up, restart,                  &
                                    allowed_to_read ,                   &
                                    ids, ide, jds, jde, kds, kde,       &
                                    ims, ime, jms, jme, kms, kme,       &
                                    its, ite, jts, jte, kts, kte   )  
!          ENDIF

#if (NMM_CORE != 1)
      CASE (BOULACSCHEME)
           if(isfc .ne. 1 .and. isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay or myjsfc scheme for this pbl option' )
           CALL boulacinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN, &
                         TKE_PBL,EXCH_H,restart,               &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
        CASE (CAMUWPBLSCHEME)
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           CALL camuwpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,        &
                restart,TKE_PBL,is_CAMMGMP_used, &
                ids, ide, jds, jde, kds, kde,                          &
                ims, ime, jms, jme, kms, kme,                          &
                its, ite, jts, jte, kts, kte                           )
#endif

#if ( EM_CORE == 1 )

!mynn 
           
        CASE (MYNNPBLSCHEME2, MYNNPBLSCHEME3)
           IF(isfc .NE. 5 .AND. isfc .NE. 1 .AND. isfc .NE. 2) CALL wrf_error_fatal &
                ( 'module_physics_init: use mynnsfc or sfclay or myjsfc scheme for this pbl option')
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
           
           SELECT CASE(config_flags%bl_pbl_physics)

             CASE(MYNNPBLSCHEME2)
                mynn_closure_level=2

             CASE(MYNNPBLSCHEME3)
                mynn_closure_level=3

             CASE DEFAULT

           END SELECT

           CALL mynn_bl_init_driver(&
                &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,&
                &RQIBLTEN,                                  &
                !RQNCBLTEN,RQNIBLTEN,                       &
                &QKE,                                       &
                &TKE_PBL,EXCH_H                             &
                &,restart,allowed_to_read,mynn_closure_level &
                &,IDS,IDE,JDS,JDE,KDS,KDE                    &
                &,IMS,IME,JMS,JME,KMS,KME                    &
                &,ITS,ITE,JTS,JTE,KTS,KTE)

      CASE (TEMFPBLSCHEME)
           ! if(isfc .ne. 0)CALL wrf_error_fatal &
           !  ( 'module_physics_init: use sfclay scheme = 0 for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use myj (option 2) or boulac (option 8) with BEP/BEM urban scheme' )
         IF ( PRESENT( te_temf ) .AND. PRESENT( cf3d_temf )) THEN
           CALL temfinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        te_temf,cf3d_temf,                    & ! WA
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling TEMF scheme' )
         ENDIF

#endif

      CASE (GBMPBLSCHEME) 
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: Use sf_sfclay_physics= 1 or 91 for this pbl option' )
         CALL gbmpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,   &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,TKE_PBL,           &
                        EXCH_H,                               &
                        restart,allowed_to_read ,             &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )

#if ( WRFPLUS == 1 )
      CASE (SURFDRAGSCHEME)
         CALL surface_drag_init(RUBLTEN,RVBLTEN,RTHBLTEN,   &
                        RQVBLTEN,restart,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
#endif


      CASE DEFAULT

   END SELECT pbl_select


   END SUBROUTINE bl_init

!==================================================================
   SUBROUTINE cu_init(DX,STEPCU,CUDT,DT,RUCUTEN,RVCUTEN,RTHCUTEN,  &
                      RQVCUTEN,RQRCUTEN,RQCCUTEN,RQSCUTEN,RQICUTEN,&
                      NCA,RAINC,RAINCV,W0AVG,config_flags,restart, &
                      CLDEFI,LOWLYR,MASS_FLUX,                     &
                      RTHFTEN, RQVFTEN,                            &
#if ( EM_CORE == 1 )
                      !BSINGH - For WRFCuP Scheme
                      cupflag,cldfra_cup,cldfratend_cup,          & !CuP, wig 18-Sep-2006
                      shall,                                      & !CuP, wig 18-Sep-2006
                      tcloud_cup,                                 & !CuP, rce 18-apr-2012
                      !BSINGH -ENDS
#endif
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,           &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,               &
                      cugd_tten,cugd_ttens,cugd_qvten,             &
                      cugd_qvtens,cugd_qcten,                      &
                      allowed_to_read, start_of_simulation,        &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte,                &
                      RQCNCUTEN,RQINCUTEN)
!------------------------------------------------------------------
   USE module_cu_kf
   USE module_cu_kfeta
   USE module_cu_mskf
   USE MODULE_CU_BMJ
   USE module_cu_gd,  ONLY : GDINIT
   USE module_cu_g3,  ONLY : G3INIT
   USE module_cu_sas
   USE module_cu_scalesas
   USE module_cu_osas
   USE module_cu_camzm_driver, ONLY : zm_conv_init
   USE module_cu_nsas
   USE module_cu_tiedtke
   USE module_cu_ntiedtke
#if ( EM_CORE == 1 )
   USE module_cu_kfcup !BSINGH - For WRFCuP Scheme
#endif
#if ( WRFPLUS == 1 )
   USE module_cu_du, ONLY : ducuinit
#endif
!------------------------------------------------------------------
   IMPLICIT NONE 
!------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT, DX
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &    
            RUCUTEN, RVCUTEN, RTHCUTEN, &
            RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN, RQSCUTEN
#if ( EM_CORE == 1 )
   !BSINGH - For WRFCuP Scheme
   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &    
        cldfra_cup,cldfratend_cup                               !CuP, wig 18-Sep-2006
   !BSINGH -ENDS
#endif

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &    
                        cugd_tten,cugd_ttens,cugd_qvten,            &    
                        cugd_qvtens,cugd_qcten, RQCNCUTEN, RQINCUTEN

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &!BSINGH(PNNL)- should be declared inout
            RTHFTEN, RQVFTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: CLDEFI
#if ( EM_CORE == 1 )
   !BSINGH - For WRFCuP Scheme
   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: shall, & !CuP, wig 18-Sep-2006
                                                     tcloud_cup    !CuP, rce 18-apr-2012
   LOGICAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: cupflag  !CuP, wig 9-Oct-2006 !BSINGH - For WRFCuP Scheme
   !BSINGH -ENDS
#endif

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: MASS_FLUX,   &
                                   APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &    
                                   APR_CAPMA,APR_CAPME,APR_CAPMI
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: LOWLYR

! LOCAL VAR

  INTEGER :: i,j,itf,jtf

!--------------------------------------------------------------------

!-- calculate cumulus parameterization time step

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
!
   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)

!-- initialization

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF

!-- deep convection and hybrid deep-shallow convection schemes
   cps_select: SELECT CASE(config_flags%cu_physics)

     CASE (KFSCHEME)
          CALL kfinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,        &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (BMJSCHEME)
          CALL bmjinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,       &
                      CLDEFI,LOWLYR,cp,r_d,restart,               &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (KFETASCHEME)
          CALL kf_eta_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

#if ( EM_CORE == 1 )
     CASE (MSKFSCHEME)
          CALL mskf_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,     &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
#endif

     CASE (GDSCHEME)
          CALL gdinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
     CASE (KSASSCHEME,NSASSCHEME)
         CALL nsasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      RUCUTEN,RVCUTEN,                            & 
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

#if ( EM_CORE == 1 )
     CASE (G3SCHEME,GFSCHEME)
          CALL g3init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,        &
                      MASS_FLUX,cp,restart,                       &
                      P_QC,P_QI,PARAM_FIRST_SCALAR,               &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      cugd_tten,cugd_ttens,cugd_qvten,            &
                      cugd_qvtens,cugd_qcten,                     &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
#endif
     CASE (SASSCHEME)
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
                      RUCUTEN,RVCUTEN,                            &   ! gopal's doing for SAS
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (SCALESASSCHEME)
          CALL scalesasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,  &
                      RUCUTEN,RVCUTEN,                            &   
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

#if ( EM_CORE == 1 )
          !BSINGH - For WRFCuP Scheme
    CASE (KFCUPSCHEME)  !wig: 18-Sep-2006
       CALL kf_cup_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,   &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      cupflag,cldfra_cup,cldfratend_cup,          &
                      shall,                                      &
                      tcloud_cup,                                 &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

    !BSINGH - ENDS
#endif

     CASE (OSASSCHEME)
          CALL osasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,      &
                      RUCUTEN,RVCUTEN,                            &   ! gopal's doing for SAS
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE (CAMZMSCHEME)
          IF (PRESENT(RQCNCUTEN)) THEN
          CALL zm_conv_init(DT, DX, rucuten, rvcuten, rthcuten, rqvcuten, &
                      rqccuten, rqicuten, rqcncuten, rqincuten,         &
                      p_qc, p_qi, p_qnc, p_qni, param_first_scalar,     &
                      restart,                                          &
                      ids, ide, jds, jde, kds, kde,                     &
                      ims, ime, jms, jme, kms, kme,                     &
                      its, ite, jts, jte, kts, kte                      )
          ELSE
          CALL wrf_error_fatal ( 'arguments not present for calling camzmscheme' )
          ENDIF

! Tiedtke Scheme - ZCX&YQW
      CASE (TIEDTKESCHEME)
          CALL tiedtkeinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,   &
                      RUCUTEN,RVCUTEN,                            & 
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

      CASE (NTIEDTKESCHEME)
          CALL ntiedtkeinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,  &
                      RUCUTEN,RVCUTEN,                            &
                      RTHFTEN, RQVFTEN,                           &
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

#if ( WRFPLUS == 1 )
! this is for WRFPlus only
      CASE (DUCUSCHEME)
          CALL ducuinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,      &
                      RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QC,P_QR,      &
                      SVP1,SVP2,SVP3,SVPT0,                       &
                      PARAM_FIRST_SCALAR,restart,                 &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
#endif

      CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cu_init

!==================================================================
   SUBROUTINE shcu_init(STEPCU,CUDT,DT,RUSHTEN,RVSHTEN,RTHSHTEN,   &
                      RQVSHTEN,RQRSHTEN,RQCSHTEN,                  &
                      RQSSHTEN,RQISHTEN,RQGSHTEN,                  &
                      NCA,RAINC,RAINCV,config_flags,restart,       &
                      allowed_to_read, start_of_simulation,        &
                      RDCASHTEN, RQCDCSHTEN, W0AVG,                &
                      cldareaa, cldareab, cldliqa,  cldliqb,       &
                      ca_rad, cw_rad,                              &
                      pblmax, wub, ltopb, clddpthb, cldtopb,       &
                      capesave, ainckfsa, radsave,                 &
                      rainsh, rainshvb, kdcldtop, kdcldbas, xtime1,&
                      PBLHAVG, TKEAVG,                             &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!------------------------------------------------------------------
   USE uwshcu,        ONLY: init_uwshcu
   USE module_shcu_grims, ONLY: grimsinit
   USE module_shcu_nscv , ONLY: nscvinit
   USE module_shcu_deng , ONLY: deng_shcu_init
   USE physconst,     ONLY: cpair, gravit, latice, latvap, mwdry, mwh2o, &
                            rair, zvir
   USE shr_kind_mod,  ONLY: r8 => shr_kind_r8

!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   TYPE (grid_config_rec_type) :: config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RUSHTEN, RVSHTEN, RTHSHTEN, &
            RQVSHTEN, RQCSHTEN, RQRSHTEN, RQISHTEN, RQSSHTEN, RQGSHTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(  OUT) ::    pblmax, clddpthb, cldtopb, rainsh, rainshvb, &
                              capesave, radsave, xtime1, PBLHAVG

   REAL, OPTIONAL, DIMENSION( ims:ime , 1:100, jms:jme ) , &
          INTENT(  OUT) ::    ainckfsa

   INTEGER, OPTIONAL, DIMENSION( ims:ime , jms:jme ) , &
          INTENT(  OUT) ::    kdcldtop, kdcldbas, ltopb

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(  OUT) ::    wub, cldareaa,  cldareab,                &
                                   cldliqa,   cldliqb,  W0AVG, TKEAVG, &
                                   ca_rad, cw_rad

   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: &
             RDCASHTEN, RQCDCSHTEN

! LOCAL VAR

  INTEGER :: i,j,itf,jtf

!--------------------------------------------------------------------

! Some of this stuff is redundant with deep convection, but redo it
! in case deep is turned off...

!-- calculate cumulus parameterization time step

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)
!
   STEPCU = nint(CUDT*60./DT)
   STEPCU = max(STEPCU,1)

!-- initialization

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINC(i,j)=0.
        RAINCV(i,j)=0.
     ENDDO
     ENDDO
   ENDIF

!-- independent shallow convection schemes
   shcu_select: SELECT CASE(config_flags%shcu_physics)

   CASE (CAMUWSHCUSCHEME)
      CALL init_uwshcu(r8,latvap,cpair,latice,zvir,rair,gravit,      &
           mwh2o/mwdry,                                              &
           rushten, rvshten, rthshten, rqvshten,                     &
           rqcshten, rqrshten, rqishten, rqsshten, rqgshten,         &
           p_qc, p_qr, p_qi, p_qs, p_qg,                             &
           config_flags%bl_pbl_physics, param_first_scalar, restart, &
           ids, ide, jds, jde, kds, kde,                             &
           ims, ime, jms, jme, kms, kme,                             &
           its, ite, jts, jte, kts, kte                              )

   CASE (GRIMSSHCUSCHEME)
      CALL grimsinit(rthshten,rqvshten,                              &
           restart,                                                  &
           ids, ide, jds, jde, kds, kde,                             &
           ims, ime, jms, jme, kms, kme,                             &
           its, ite, jts, jte, kts, kte                              )

   CASE (NSCVSHCUSCHEME)
      CALL nscvinit(rthshten,rqvshten,rqcshten,rqishten,             &
           rushten,rvshten,                                          &
           restart,p_qc,p_qi,param_first_scalar,                     &
           allowed_to_read ,                                         &
           ids, ide, jds, jde, kds, kde,                             &
           ims, ime, jms, jme, kms, kme,                             &
           its, ite, jts, jte, kts, kte                              )

#if ( EM_CORE == 1 )
   CASE (DENGSHCUSCHEME)
      CALL deng_shcu_init(RTHSHTEN,RQVSHTEN,RQCSHTEN,RQRSHTEN,      &
                     RUSHTEN,RVSHTEN,RDCASHTEN,RQCDCSHTEN,W0AVG,    &
                     PBLHAVG, TKEAVG,                               &
                     cldareaa, cldareab, cldliqa,  cldliqb,         &
                     ca_rad, cw_rad,                          &
                     wub, pblmax, ltopb, clddpthb, cldtopb,         &
                     capesave, ainckfsa, radsave,                   &
                     rainsh, rainshvb, kdcldtop, kdcldbas,                  &
                     xtime1, restart,                               &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
#endif
 
   CASE DEFAULT

   END SELECT shcu_select

   END SUBROUTINE shcu_init

!==================================================================
   SUBROUTINE mp_init(RAINNC,SNOWNC,GRAUPELNC,config_flags,restart,warm_rain,      &
                      adv_moist_cond,                             &
                      MPDT, DT, DX, DY, LOWLYR,                   & ! for eta mp
                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           & ! for eta mp
                      mp_restart_state,tbpvs_state,tbpvs0_state,   & ! eta mp
                      allowed_to_read, start_of_simulation,       &
!CAMMGMP specific variables
                      ixcldliq, ixcldice, ixnumliq, ixnumice,     &       
                      nssl_cccn, nssl_alphah, nssl_alphahl,       &
                      nssl_ipelec, nssl_isaund,                  &
                         nssl_cnoh, nssl_cnohl,                  &
                         nssl_cnor, nssl_cnos,                   &
                         nssl_rho_qh, nssl_rho_qhl,              &
                         nssl_rho_qs,                            &
                      ccn_conc,                                  & ! RAS
                      z_at_q, inv_dens, qnwfa2d, scalar, num_sc,  &  ! G. Thompson
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_wsm7
   USE module_mp_etanew
   USE module_mp_fer_hires
#if (NMM_CORE == 1)
   USE module_mp_HWRF
#endif
   USE module_mp_thompson
   USE module_mp_full_sbm
   USE module_mp_fast_sbm
   USE module_mp_morr_two_moment
   USE module_mp_p3
   USE module_mp_jensen_ishmael
   USE module_mp_milbrandt2mom
!  USE module_mp_milbrandt3mom
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_wdm7
   USE module_mp_nssl_2mom
#if (EM_CORE==1)
   USE module_mp_cammgmp_driver, ONLY:CAMMGMP_INIT !CAM5's microphysics
   USE module_mp_morr_two_moment_aero              !TWG2017
#endif
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
! Arguments
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   REAL    , INTENT(IN)        :: MPDT, DT, DX, DY
   REAL, INTENT(IN), OPTIONAL  :: nssl_cccn, nssl_alphah, nssl_alphahl, &
                                  nssl_cnoh, nssl_cnohl,                  &
                                  nssl_cnor, nssl_cnos,                   &
                                  nssl_rho_qh, nssl_rho_qhl,              &
                                  nssl_rho_qs
   INTEGER, INTENT(IN), OPTIONAL  :: nssl_ipelec, nssl_isaund

   LOGICAL , INTENT(IN)        :: start_of_simulation
   INTEGER , INTENT(IN)        :: ixcldliq, ixcldice, ixnumliq, ixnumice ! CAMMGMP specific variables

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   INTEGER , DIMENSION( ims:ime , jms:jme ) ,INTENT(INOUT)  :: LOWLYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RAINNC,SNOWNC,GRAUPELNC
   REAL,     DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: &
                                  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL , DIMENSION(:) ,INTENT(INOUT)  :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL , INTENT(IN)  :: allowed_to_read
   REAL, INTENT(INOUT) :: ccn_conc ! RAS
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT):: qnwfa2d                       !  G. Thompson
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN):: z_at_q                   !  G. Thompson
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN):: inv_dens                 !  G. Thompson
   INTEGER, INTENT(IN) :: num_sc                                               !  G. Thompson
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme, num_sc), INTENT(INOUT):: scalar    !  G. Thompson

! Local
   INTEGER :: i, j, itf, jtf
   REAL, DIMENSION(20) :: nssl_params
   INTEGER    :: nssl_ipelec_tmp
   INTEGER    :: i_err

   warm_rain = .false.
   adv_moist_cond = .true.
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINNC(i,j) = 0.
        SNOWNC(i,j) = 0.
        GRAUPELNC(i,j) = 0.
     ENDDO
     ENDDO
   ENDIF
   
   IF ( present( nssl_cccn ) ) THEN
     SELECT CASE(config_flags%mp_physics)
     CASE (NSSL_2MOM,NSSL_2MOMCCN)
       IF ( config_flags%elec_physics > 0 ) THEN
         nssl_ipelec_tmp = nssl_ipelec
       ELSE
         nssl_ipelec_tmp = 0.0
       ENDIF
     CASE DEFAULT
       nssl_ipelec_tmp = 0.0
     END SELECT
     
     nssl_params(1)  = nssl_cccn
     nssl_params(2)  = nssl_alphah
     nssl_params(3)  = nssl_alphahl
     nssl_params(4)  = nssl_cnoh
     nssl_params(5)  = nssl_cnohl
     nssl_params(6)  = nssl_cnor
     nssl_params(7)  = nssl_cnos
     nssl_params(8)  = nssl_rho_qh
     nssl_params(9)  = nssl_rho_qhl
     nssl_params(10) = nssl_rho_qs
     nssl_params(11) = nssl_ipelec_tmp
     nssl_params(12) = nssl_isaund

   ENDIF

   mp_select: SELECT CASE(config_flags%mp_physics)

     CASE (KESSLERSCHEME)
          warm_rain = .true.
#if ( WRFPLUS == 1 )
     CASE (MKESSLERSCHEME)
          warm_rain = .true.
#endif
     CASE (WSM3SCHEME)
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cpv, config_flags%hail_opt,allowed_to_read )
     CASE (WSM7SCHEME)
          CALL wsm7init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (ETAMPNEW)
         adv_moist_cond = .false.
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
     CASE (FER_MP_HIRES,FER_MP_HIRES_ADVECT)
               CALL fer_hires_init (MPDT,DT,DX,DY,LOWLYR,restart,         &
                            allowed_to_read,                        &
                            ids, ide, jds, jde, kds, kde,           &
                            ims, ime, jms, jme, kms, kme,           &
                            its, ite, jts, jte, kts, kte,           &
                            F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY)
#if(NMM_CORE==1)
     CASE (etamp_HWRF)
         CALL etanewinit_HWRF (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
#endif 
     CASE (THOMPSON)
         IF(start_of_simulation.or.restart.or.config_flags%cycling)     &
            CALL thompson_init(HGT=z_at_q,                              &
                          IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde,   &
                          IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme,   &
                          ITS=its, ITE=ite, JTS=jts, JTE=jte, KTS=kts, KTE=kte)

     CASE (THOMPSONAERO)
! Cycling the WRF forecast with moving nests will cause this initialization to be
! called for each nest move. This is potentially very computationally expensive.
         IF(start_of_simulation.or.restart.or.config_flags%cycling)     &
            CALL thompson_init(HGT=z_at_q,                              &
                          ORHO=inv_dens,                                &
                          NWFA2D=qnwfa2d,                               &
                          NWFA=scalar(ims,kms,jms,P_QNWFA),             &
                          NIFA=scalar(ims,kms,jms,P_QNIFA),             &
                          DX=DX, DY=DY,                                 &
                          is_start=start_of_simulation,                 &
                          IDS=ids, IDE=ide, JDS=jds, JDE=jde, KDS=kds, KDE=kde,   &
                          IMS=ims, IME=ime, JMS=jms, JME=jme, KMS=kms, KME=kme,   &
                          ITS=its, ITE=ite, JTS=jts, JTE=jte, KTS=kts, KTE=kte)

     CASE (MORR_TWO_MOMENT)
         CALL morr_two_moment_init( config_flags%morr_rimed_ice )
#if (EM_CORE==1)
     CASE (MORR_TM_AERO)
         CALL morr_two_moment_init_aero( config_flags%morr_rimed_ice )
#endif
     CASE (JENSEN_ISHMAEL)
        CALL jensen_ishmael_init()

     CASE (P3_1CATEGORY)
         CALL p3_init('.',1,'WRF',i_err,.false.)
     CASE (P3_1CATEGORY_NC)
         CALL p3_init('.',1,'WRF',i_err,.false.)
     CASE (P3_2CATEGORY)
         CALL p3_init('.',2,'WRF',i_err,.false.)
     CASE (MILBRANDT2MOM)
         CALL milbrandt2mom_init
!      CASE (MILBRANDT3MOM)
!          CALL milbrandt3mom_init
     CASE (WDM5SCHEME)
          CALL wdm5init(rhoair0,rhowater,rhosnow,cliq,cpv,ccn_conc,allowed_to_read )
     CASE (WDM6SCHEME)
          CALL wdm6init(rhoair0,rhowater,rhosnow,cliq,cpv,ccn_conc, &
                        config_flags%hail_opt, allowed_to_read )
     CASE (WDM7SCHEME)
          CALL wdm7init(rhoair0,rhowater,rhosnow,cliq,cpv,ccn_conc, allowed_to_read )
#if (EM_CORE==1)
    CASE (FULL_KHAIN_LYNN)
     IF(start_of_simulation.or.restart)THEN
          CALL full_hucminit(dt)
     END IF
    CASE (FAST_KHAIN_LYNN)
     IF(start_of_simulation.or.restart)THEN
          CALL fast_hucminit(dt)
     END IF
#endif
     CASE (NSSL_1MOMLFO)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=0,mixphase=0,ihvol=-1) ! no separate hail
     CASE (NSSL_1MOM)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=0,mixphase=0,ihvol=0)
     CASE (NSSL_2MOM)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=5,mixphase=0,ihvol=1)
     CASE (NSSL_2MOMG)
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=5,mixphase=0,ihvol=-1) ! turn off hail
     CASE (NSSL_2MOMCCN)
         ccn_conc = nssl_cccn/1.225 ! set this to have correct boundary conditions
         CALL nssl_2mom_init(ims,ime, jms,jme, kms,kme,nssl_params,ipctmp=5,mixphase=0,ihvol=1)
#if (EM_CORE==1)
     CASE (CAMMGMPSCHEME) ! CAM5's microphysics
          CALL CAMMGMP_INIT(ixcldliq, ixcldice, ixnumliq, ixnumice &
             ,config_flags%chem_opt                          &
             ,ids, ide, jds, jde, kds, kde                   & 
             ,ims, ime, jms, jme, kms, kme                   &
             ,its, ite, jts, jte, kts, kte                   )
#endif

     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init

#if  ( EM_CORE == 1 )
!==========================================================
   SUBROUTINE fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,    &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
                SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH,    & ! fasdas
                HFX_FDDA,                                       & ! fasdas
                config_flags,restart,                           &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )


!--------------------------------------------------------------------
   USE module_fdda_psufddagd
   USE module_fdda_spnudging, ONLY : fddaspnudginginit
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        ::     DT, FGDT
   INTEGER , INTENT(IN)        ::     id
   INTEGER , INTENT(INOUT)     ::     STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::       &!BSINGH(PNNL)- should be declared inout
                                                           RUNDGDTEN, &
                                                           RVNDGDTEN, &
                                                          RTHNDGDTEN, &
                                                          RPHNDGDTEN, &
                                                          RQVNDGDTEN
!
! FASDAS
!
   REAL ,    DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT)           ::    &
                                                             SDA_HFX, &
                                                             SDA_QFX, &
                                                             QNORM,HFX_BOTH,QFX_BOTH
!  INTEGER , INTENT(IN   )                                  ::  fasdas
   REAL ,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: &
                                                            HFX_FDDA

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RMUNDGDTEN !BSINGH(PNNL)- should be declared inout

   LOGICAL,  INTENT(IN)           :: allowed_to_read
!--------------------------------------------------------------------

!-- calculate pbl time step

   STEPFG = nint(FGDT*60./DT)
   STEPFG = max(STEPFG,1)


!-- initialize fdda scheme

   fdda_select: SELECT CASE(config_flags%grid_fdda)

      CASE (PSUFDDAGD)
           CALL fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
               SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH, config_flags%fasdas,& ! fasdas
               HFX_FDDA,                                                        & ! fasdas
               config_flags%run_hours, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_q, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%if_zfac_q, &
               config_flags%k_zfac_q, &
               config_flags%guv, &
               config_flags%gt, config_flags%gq, &
               config_flags%if_ramping, config_flags%dtramp_min, &
               config_flags%auxinput10_end_h, &
               config_flags%grid_sfdda, &
               config_flags%guv_sfc, &
               config_flags%gt_sfc, &
               config_flags%gq_sfc, &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

      CASE (SPNUDGING)
           CALL fddaspnudginginit(id,rundgdten,rvndgdten,rthndgdten,rphndgdten,rqvndgdten,&
               config_flags%run_hours, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_ph, &
               config_flags%if_no_pbl_nudging_q, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%dk_zfac_uv, &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%dk_zfac_t, &
               config_flags%if_zfac_ph, &
               config_flags%k_zfac_ph, &
               config_flags%dk_zfac_ph, &
               config_flags%if_zfac_q, &
               config_flags%k_zfac_q, &
               config_flags%dk_zfac_q, &
               config_flags%ktrop, &
               config_flags%guv, &
               config_flags%gt, config_flags%gph, config_flags%gq, &
               config_flags%if_ramping, config_flags%dtramp_min, &
               config_flags%auxinput9_end_h, &
               config_flags%xwavenum,config_flags%ywavenum, &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )

      CASE DEFAULT

   END SELECT fdda_select

   END SUBROUTINE fg_init

!-------------------------------------------------------------------
   SUBROUTINE fdob_init(obs_nudge_opt, maxdom, inest, parid,       &
                        idynin, dtramp, fdaend, restart,           &
                        obs_twindo_cg, obs_twindo, itimestep,      &
                        no_pbl_nudge_uv,                           &
                        no_pbl_nudge_t,                            &
                        no_pbl_nudge_q,                            &
                        sfc_scheme_horiz, sfc_scheme_vert,         &
                        maxsnd_gap,                                &
                        sfcfact, sfcfacr, dpsmx,                   &
                        nudge_wind, nudge_temp, nudge_mois,        &
                        nudgezfullr1_uv, nudgezrampr1_uv,          &
                        nudgezfullr2_uv, nudgezrampr2_uv,          &
                        nudgezfullr4_uv, nudgezrampr4_uv,          &
                        nudgezfullr1_t,  nudgezrampr1_t,           &
                        nudgezfullr2_t,  nudgezrampr2_t,           &
                        nudgezfullr4_t,  nudgezrampr4_t,           &
                        nudgezfullr1_q,  nudgezrampr1_q,           &
                        nudgezfullr2_q,  nudgezrampr2_q,           &
                        nudgezfullr4_q,  nudgezrampr4_q,           &
                        nudgezfullmin, nudgezrampmin, nudgezmax,   &
                        xlat, xlong,                               &
                        start_year, start_month, start_day,        &
                        start_hour, start_minute, start_second,    &
                        p00, t00, tlp,                             &
                        znu, p_top,                                &
                        fdob, ipf_init,                            &
                        ids, ide, jds, jde, kds, kde,              &
                        ims, ime, jms, jme, kms, kme,              &
                        its, ite, jts, jte, kts, kte               )

!--------------------------------------------------------------------
   USE module_domain
   USE module_fddaobs_rtfdda
   USE module_llxy
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER , INTENT(IN)    :: maxdom
   INTEGER , INTENT(IN)    :: obs_nudge_opt(maxdom)
   INTEGER , INTENT(IN)    :: ids,ide, jds,jde, kds,kde,           &
                              ims,ime, jms,jme, kms,kme,           &
                              its,ite, jts,jte, kts,kte
   INTEGER , INTENT(IN)    :: inest
   INTEGER , INTENT(IN)    :: parid(maxdom)
   INTEGER , INTENT(IN)    :: idynin          ! flag for dynamic initialization
   REAL    , INTENT(IN)    :: dtramp          ! time period for ramping (idynin)
   REAL    , INTENT(IN)    :: fdaend(maxdom)  ! nudging end time for domain (min)
   LOGICAL , INTENT(IN)    :: restart
   REAL    , INTENT(IN)    :: obs_twindo_cg   ! twindo on course grid
   REAL    , INTENT(IN)    :: obs_twindo
   INTEGER , INTENT(IN)    :: itimestep
   INTEGER , INTENT(IN)    :: no_pbl_nudge_uv(maxdom)  ! flags for no wind nudging in pbl
   INTEGER , INTENT(IN)    :: no_pbl_nudge_t(maxdom)   ! flags for no temperature nudging in pbl
   INTEGER , INTENT(IN)    :: no_pbl_nudge_q(maxdom)   ! flags for no moisture nudging in pbl
   INTEGER , INTENT(IN)    :: sfc_scheme_horiz ! horizontal spreading scheme for surf obs (wrf or orig mm5)
   INTEGER , INTENT(IN)    :: sfc_scheme_vert  ! vertical   spreading scheme for surf obs (orig or regime vif)
   REAL    , INTENT(IN)    :: maxsnd_gap       ! max allowed pressure gap in soundings for interp (centibars) 
   REAL    , INTENT(IN)    :: sfcfact      ! scale factor applied to time window for surface obs
   REAL    , INTENT(IN)    :: sfcfacr      ! scale fac applied to horiz rad of infl for sfc obs
   REAL    , INTENT(IN)    :: dpsmx        ! max pressure change allowed within horiz. infl. range
   INTEGER , INTENT(IN)    :: nudge_wind(maxdom)       ! wind-nudging flag
   INTEGER , INTENT(IN)    :: nudge_temp(maxdom)       ! temperature-nudging flag
   INTEGER , INTENT(IN)    :: nudge_mois(maxdom)       ! moisture-nudging flag
   REAL    , INTENT(IN)    :: nudgezfullr1_uv  ! vert infl fcn, regime=1 full-wt   hght, winds
   REAL    , INTENT(IN)    :: nudgezrampr1_uv  ! vert infl fcn, regime=1 ramp down hght, winds
   REAL    , INTENT(IN)    :: nudgezfullr2_uv  ! vert infl fcn, regime=2 full-wt   hght, winds
   REAL    , INTENT(IN)    :: nudgezrampr2_uv  ! vert infl fcn, regime=2 ramp down hght, winds
   REAL    , INTENT(IN)    :: nudgezfullr4_uv  ! vert infl fcn, regime=4 full-wt   hght, winds
   REAL    , INTENT(IN)    :: nudgezrampr4_uv  ! vert infl fcn, regime=4 ramp down hght, winds
   REAL    , INTENT(IN)    :: nudgezfullr1_t   ! vert infl fcn, regime=1 full-wt   hght, temp
   REAL    , INTENT(IN)    :: nudgezrampr1_t   ! vert infl fcn, regime=1 ramp down hght, temp
   REAL    , INTENT(IN)    :: nudgezfullr2_t   ! vert infl fcn, regime=2 full-wt   hght, temp
   REAL    , INTENT(IN)    :: nudgezrampr2_t   ! vert infl fcn, regime=2 ramp down hght, temp
   REAL    , INTENT(IN)    :: nudgezfullr4_t   ! vert infl fcn, regime=4 full-wt   hght, temp
   REAL    , INTENT(IN)    :: nudgezrampr4_t   ! vert infl fcn, regime=4 ramp down hght, temp
   REAL    , INTENT(IN)    :: nudgezfullr1_q   ! vert infl fcn, regime=1 full-wt   hght, mois
   REAL    , INTENT(IN)    :: nudgezrampr1_q   ! vert infl fcn, regime=1 ramp down hght, mois
   REAL    , INTENT(IN)    :: nudgezfullr2_q   ! vert infl fcn, regime=2 full-wt   hght, mois
   REAL    , INTENT(IN)    :: nudgezrampr2_q   ! vert infl fcn, regime=2 ramp down hght, mois
   REAL    , INTENT(IN)    :: nudgezfullr4_q   ! vert infl fcn, regime=4 full-wt   hght, mois
   REAL    , INTENT(IN)    :: nudgezrampr4_q   ! vert infl fcn, regime=4 ramp down hght, mois
   REAL    , INTENT(IN)    :: nudgezfullmin    ! min dpth thru which vert infl fcn remains 1.0 (m)
   REAL    , INTENT(IN)    :: nudgezrampmin    ! min dpth thru which vif decreases 1.0 to 0.0 (m)
   REAL    , INTENT(IN)    :: nudgezmax        ! max dpth in which vif is nonzero (m)
   REAL    , INTENT(IN)    :: xlat ( ims:ime, jms:jme )        ! latitudes on mass-point grid
   REAL    , INTENT(IN)    :: xlong( ims:ime, jms:jme )        ! longitudes on mass-point grid
   INTEGER , INTENT(INOUT) :: start_year
   INTEGER , INTENT(INOUT) :: start_month
   INTEGER , INTENT(INOUT) :: start_day
   INTEGER , INTENT(INOUT) :: start_hour
   INTEGER , INTENT(INOUT) :: start_minute
   INTEGER , INTENT(INOUT) :: start_second
   REAL    , INTENT(IN)    :: p00                      ! base state pressure
   REAL    , INTENT(IN)    :: t00                      ! base state temperature
   REAL    , INTENT(IN)    :: tlp                      ! base state lapse rate
   REAL    , INTENT(IN)    :: znu( kms:kme )           ! eta values on half (mass) levels
   REAL    , INTENT(IN)    :: p_top                    ! pressure at top of model
   TYPE(fdob_type), INTENT(INOUT)  :: fdob

   INTEGER                 :: e_sn         ! ending   north-south grid index
   LOGICAL                 :: ipf_init     ! print warnings detected at initialzn
!--------------------------------------------------------------------
!-- initialize fdda obs-nudging scheme

      IF ( obs_nudge_opt(inest) .eq. 0 ) RETURN

      e_sn = jde
      CALL fddaobs_init(obs_nudge_opt, maxdom, inest, parid,       &
                        idynin, dtramp, fdaend, restart,           &
                        obs_twindo_cg,                             &
                        obs_twindo, itimestep,                     &
                        no_pbl_nudge_uv,                           &
                        no_pbl_nudge_t,                            &
                        no_pbl_nudge_q,                            &
                        sfc_scheme_horiz, sfc_scheme_vert,         &
                        maxsnd_gap,                                &
                        sfcfact, sfcfacr, dpsmx,                   &
                        nudge_wind, nudge_temp, nudge_mois,        &
                        nudgezfullr1_uv, nudgezrampr1_uv,          &
                        nudgezfullr2_uv, nudgezrampr2_uv,          &
                        nudgezfullr4_uv, nudgezrampr4_uv,          &
                        nudgezfullr1_t,  nudgezrampr1_t,           &
                        nudgezfullr2_t,  nudgezrampr2_t,           &
                        nudgezfullr4_t,  nudgezrampr4_t,           &
                        nudgezfullr1_q,  nudgezrampr1_q,           &
                        nudgezfullr2_q,  nudgezrampr2_q,           &
                        nudgezfullr4_q,  nudgezrampr4_q,           &
                        nudgezfullmin,  nudgezrampmin, nudgezmax,  &
                        xlat, xlong,                               &
                        start_year, start_month, start_day,        &
                        start_hour, start_minute, start_second,    &
                        p00, t00, tlp,                             &
                        znu, p_top,                                &
                        fdob, ipf_init,                            &
                        ids,ide, jds,jde, kds,kde,                 &
                        ims,ime, jms,jme, kms,kme,                 &
                        its,ite, jts,jte, kts,kte)

   END SUBROUTINE fdob_init
#endif

!--------------------------------------------------------------------
   SUBROUTINE z2sigma(zf,zh,sf,sh,p_top,pptop,config_flags, &
                allowed_to_read , &
                kds,kde,kms,kme,kts,kte)
   IMPLICIT NONE
! Arguments
   INTEGER, INTENT(IN) :: kds,kde,kms,kme,kts,kte
   REAL , DIMENSION( kms:kme ), INTENT(IN) :: zf,zh
   REAL , DIMENSION( kms:kme ), INTENT(OUT):: sf,sh
   REAL , INTENT(IN) :: p_top
   REAL , INTENT(OUT) :: pptop
   TYPE (grid_config_rec_type)              :: config_flags
   LOGICAL , INTENT(IN) :: allowed_to_read
! Local
   REAL R, G, TS, GAMMA, PS, ZTROP, TSTRAT, PTROP, Z, T, P, ZTOP, PTOP
   INTEGER K

   IF(zf(kde/2) .GT. 1.0)THEN
! Height levels assumed (zeta coordinate)
! Convert to sigma using standard atmosphere for pressure-height relation
! constants for standard atmosphere definition
      r=287.05
      g=9.80665
      ts=288.15
      gamma=-6.5/1000.
      ps=1013.25
      ztrop=11000.
      tstrat=ts+gamma*ztrop
      ptrop=ps*(tstrat/ts)**(-g/(gamma*r))

      do k=kde,kds,-1
! full levels
        z=zf(k)
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        if(k.eq.kde)then
          ztop=zf(k)
          ptop=p
        endif
        sf(k)=(p-ptop)/(ps-ptop)
! half levels
        if(k.ne.kds)then
        z=0.5*(zf(k)+zf(k-1))
        if(z.le.ztrop)then
          t=ts+gamma*z
          p=ps*(t/ts)**(-g/(gamma*r))
        else
          t=tstrat
          p=ptrop*exp(-g*(z-ztrop)/(r*tstrat))
        endif
        sh(k-1)=(p-ptop)/(ps-ptop)
        endif
      enddo
      pptop=ptop/10.
   ELSE
!  Levels are already sigma/eta
      do k=kde,kds,-1
!        sf(k)=zf(kde-k+kds)
!        if(k .ne. kde)sh(k)=zh(kde-1-k+kds)
         sf(k)=zf(k)
         if(k .ne. kde)sh(k)=zh(k)
      enddo
      pptop=p_top/1000.

   ENDIF

   END SUBROUTINE z2sigma

!--------------------------------------------------------------------
   SUBROUTINE CAM_INIT (ixcldliq, ixcldice, ixnumliq, ixnumice,config_flags)
!  Purpose: To initialize a set of variables and arrays required by 
!           the CAM Parameterizations ported to WRF
!
!  Called by: Phy_init
!
!  Author:Balwinder.Singh@pnl.gov
!--------------------------------------------------------------------
     USE shr_kind_mod,               ONLY : r8 => shr_kind_r8
     USE module_cam_esinti,          ONLY : esinti
     USE physconst,                  ONLY : mwh2o, cpwv, epsilo, latvap, latice &
          , rh2o, cpair, tmelt,mwdry
     USE constituents,               ONLY : cnst_add
     USE module_cam_support,         ONLY : pcnst =>pcnst_runtime, pcnst_mp
     USE modal_aero_initialize_data_phys, ONLY : modal_aero_initialize_phys
     
     implicit none

     TYPE (grid_config_rec_type)              :: config_flags

     integer, intent(out) :: ixcldliq, ixcldice, ixnumliq, ixnumice

     !Local variables
     !Following variable declarations are from CAM's stratiform.F90 module
     integer, parameter  :: ncnstmax = 4                    ! Number of constituents     
     integer             :: mm
     character(len=8), dimension(ncnstmax), parameter :: cnst_names = & 
          (/'CLDLIQ', 'CLDICE','NUMLIQ','NUMICE'/)         ! Constituent names
     !Variables with dummy values
     integer  :: dumind 
     real(r8) :: one

     !set dynamic (runtime)pcnst
     pcnst = 5                                               !For [water vap, cld liq, cld ice, liq num and ice num]
#if ( EM_CORE == 1 )
     IF(config_flags%mp_physics == CAMMGMPSCHEME) pcnst = 12 !Additional storage for prescribed aerosols
# endif

     !pcnst for microphysics (pcnst_mp is different from pcnst ONLY if CAM MAM package amd CAMMGMP schemes are decoupled)
     pcnst_mp = pcnst

#if ( WRF_CHEM == 1 )
     !If CAM MAM package is selected
     !BSINGH (01/23/2014):Please make sure pcnst is equal to cam_mam_nspec in chem/module_cam_mam_init.F and registry.chem
     IF(config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ .OR. config_flags%chem_opt==CBMZ_CAM_MAM3_AQ) then
        pcnst = 85 !For 3 modes of aerosols [adjusted for DMS species-BSINGH (01/20/2014) suggested by PMA]
     else if(config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ .OR. config_flags%chem_opt == CBMZ_CAM_MAM7_AQ) then
        pcnst = 90 !For 7 modes of aerosols
     endif
     !If CAM MAM package is NOT selected then pcnst presumes its default value set above
     !*NOTE* : the value of pcnst is 'hardwired' here as it was a quick solution. The dynamic value
     !of pcnst is computed in module_cam_mam_init.F. In module_cam_mam_init.F, the value of PCNST set here
     !is tested for its accuracy. If the value computed in module_cam_mam_init.F differs from value mentioned
     !above, then the above values for pcnst should be adjusted accordingly

     !CAMMGMP Scheme is supposed to run with MODAL treatment of AEROSOLS only
     IF((config_flags%chem_opt .NE. CBMZ_CAM_MAM3_NOAQ .AND. config_flags%chem_opt .NE. CBMZ_CAM_MAM3_AQ &
          .AND. config_flags%chem_opt .NE. CBMZ_CAM_MAM7_NOAQ .AND. config_flags%chem_opt .NE. CBMZ_CAM_MAM7_AQ &
          .AND. config_flags%chem_opt .NE. 0) .AND. config_flags%mp_physics == CAMMGMPSCHEME) THEN
        call wrf_error_fatal('CAMMGMP (mp_physics=17) ONLY works with CAM MAM package (chem_opt =501,502,503 or 504)')
     ENDIF


     !For assisting decoupled microphysics (MP) CAM MAM simulations (simulations, where MAM package is coupled with 
     !radiation but uncoupled with MP- i.e. MP runs with 'prescribed' aerosols) 'pcnst_mp' is defined.'pcnst_mp' will
     !only be used in the CAMMGMP driver and its supporting modules (ndrop and microp_aero)
     pcnst_mp = pcnst
     if(.NOT.config_flags%CAM_MP_MAM_cpled)pcnst_mp = 12
#endif

     ! Initialize the saturation vapor pressure look-up table...      
     call esinti(epsilo, latvap, latice, rh2o, cpair, tmelt)
     
     IF(.NOT.CAM_INITIALIZED) THEN
        
        !Allocate module level CAM arrays
        call ALLOCATE_CAM_ARRAYS()     
        
        !-------------------------------------------------------------------------------------!
        !Calls to add constituents (these calls are imported from in initindx.F90 in CAM)     !
        !                                                                                     !
        ! Register water vapor.                                                               !
        ! ** This must be the first call to cnst_add so that water vapor is constituent 1.**  !
        !-------------------------------------------------------------------------------------!
        
        call cnst_add('Q', mwh2o, cpwv, 1.E-12_r8, mm, &
             longname='Specific humidity', readiv=.true. )
        
        
        !Following add constituent calls are imported from the stratiform.F90 in CAM
        
        call cnst_add(cnst_names(1), mwdry, cpair, 0._r8, ixcldliq, &
             longname='Grid box averaged cloud liquid amount')
        call cnst_add(cnst_names(2), mwdry, cpair, 0._r8, ixcldice, &
             longname='Grid box averaged cloud ice amount'   )
        call cnst_add(cnst_names(3), mwdry, cpair, 0._r8, ixnumliq, &
             longname='Grid box averaged cloud liquid number')
        call cnst_add(cnst_names(4), mwdry, cpair, 0._r8, ixnumice, &
             longname='Grid box averaged cloud ice number'   )

        !For prescribed aerosols
#if ( EM_CORE == 1 )
        IF(config_flags%mp_physics .EQ. CAMMGMPSCHEME &
#if ( WRF_CHEM == 1 )
             .AND.  config_flags%chem_opt .EQ. 0    &
#endif
             )THEN
           one = 1.0_r8
           call cnst_add('ACCUM_MASS', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged accumulation mode mass')
           call cnst_add('ACCUM_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged accumulation mode number')
           call cnst_add('AITKEN_MASS', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged aitken mode mass')
           call cnst_add('AITKEN_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged aitken mode number')
           call cnst_add('COARSE_MASS_1', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode1 mass')
           call cnst_add('COARSE_MASS_2', one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode2 mass')
           call cnst_add('COARSE_NUM' , one, cpair, 0._r8, dumind, &
                longname='Grid box averaged coarse mode number')
           
        ENDIF
#endif
        
        CAM_INITIALIZED = .TRUE.
     ENDIF
     
#if ( EM_CORE == 1 )
     IF(config_flags%mp_physics == CAMMGMPSCHEME)THEN
#if ( WRF_CHEM != 1 )
        !Aerosols must be initialized after adding the constituents otherwise the code may crash in WRF-Chem simulations     
        CALL modal_aero_initialize_phys
#else
        if(config_flags%chem_opt==0) then
           CALL modal_aero_initialize_phys
        endif
#endif
     ENDIF
#endif
   END SUBROUTINE CAM_INIT


!--------------------------------------------------------------------
 SUBROUTINE ALLOCATE_CAM_ARRAYS ()
!  Purpose: To allocate module level CAM arrays for CAM modules which
!           doesn't have an 'initialization' subroutine
!
!  Called by: CAM_INIT
!
!  Author:Balwinder.Singh@pnl.gov
!--------------------------------------------------------------------
   USE constituents,         ONLY : cnst_name,cnst_longname,cnst_cp,&
        cnst_cv,cnst_mw,cnst_type,cnst_rgas,qmin,qmincg,            &
        cnst_fixed_ubc,apcnst,bpcnst,hadvnam,vadvnam,dcconnam,      &
        fixcnam,tendnam,ptendnam,dmetendnam,sflxnam,tottnam  

   USE module_cam_support,   ONLY : pcnst =>pcnst_runtime, pcnst_mp

   USE modal_aero_data,      ONLY : cnst_name_cw,species_class,     &
        qneg3_worst_thresh_amode,cnst_name_cw_mp

   implicit none

   !Allocate module_cam_constituents.F arrays
   Allocate(cnst_name(pcnst),cnst_longname(pcnst),cnst_cp(pcnst),   &
        cnst_cv(pcnst),cnst_mw(pcnst),cnst_type(pcnst),             &
        cnst_rgas(pcnst),qmin(pcnst),qmincg(pcnst),                 &
        cnst_fixed_ubc(pcnst),apcnst(pcnst),bpcnst(pcnst),          &
        hadvnam(pcnst),vadvnam(pcnst),dcconnam(pcnst),              &
        fixcnam(pcnst),tendnam(pcnst),ptendnam(pcnst),              &
        dmetendnam(pcnst),sflxnam(pcnst),tottnam(pcnst)             )

   !Initialize module_cam_constituents.F variables
   cnst_fixed_ubc(:) = .false.

   !Allocate module_cam_mp_modal_aero_data_phys.F arrays
   Allocate(cnst_name_cw(pcnst),cnst_name_cw_mp(pcnst_mp),          &
        species_class(pcnst),qneg3_worst_thresh_amode(pcnst)        )
   
 END SUBROUTINE ALLOCATE_CAM_ARRAYS

subroutine aerosol_in(aerodm,pina,alevsiz,no_months,no_src_types,XLAT,XLONG,   &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
!
! Adaped from oznini in CAM 
! It should be replaced by monthly climatology that varies latitudinally and vertically
!
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte   

   INTEGER,      INTENT(IN   )    ::   alevsiz, no_months, no_src_types

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::     XLAT, XLONG

   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months, no_src_types ),      &
          INTENT(OUT   ) ::                                  aerodm

   REAL,  DIMENSION(alevsiz), INTENT(OUT )  ::                   pina

! Local
!  Data from Ryan Torn, computed from EC 6 types of aerosol data:
!    organic carbon, sea salt, dust, black carbon, sulfalte 
!    and stratospheric aerosol (volcanic ashes)
!  The data dimensions are 46 x 72 x 12 (pressure levels), and in unit of AOD per Pa

   INTEGER, PARAMETER :: latsiz = 46
   INTEGER, PARAMETER :: lonsiz = 72
   INTEGER :: i, j, k, itf, jtf, ktf, m, pin_unit, lat_unit, lon_unit, od_unit, ks, il, jl
   INTEGER :: ilon1, ilon2, jlat1, jlat2
   REAL    :: interp_pt, interp_pt_lat, interp_pt_lon, wlat1, wlat2, wlon1, wlon2
   CHARACTER*256 :: message

   REAL,  DIMENSION( lonsiz, alevsiz, latsiz, no_months, no_src_types )    ::   &
                                                            aerodin

   REAL,  DIMENSION(latsiz)                ::             lat_od, aertmp1
   REAL,  DIMENSION(lonsiz)                ::             lon_od, aertmp2

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

!-- read in aerosol optical depth pressure data

     WRITE(message,*)'no_months = ',no_months
     CALL wrf_debug(1,message)

! pressure in mb
      pin_unit = 27
        OPEN(pin_unit, FILE='aerosol_plev.formatted',FORM='FORMATTED',STATUS='OLD')
        do k = 1,alevsiz
        READ (pin_unit,*) pina(k)
        end do
      close(27)

!     do k=1,alevsiz
!       pina(k) = pina(k)*100.
!     end do

!-- read in aerosol optical depth lat data

      lat_unit = 28
        OPEN(lat_unit, FILE='aerosol_lat.formatted',FORM='FORMATTED',STATUS='OLD')
        do j = 1,latsiz
        READ (lat_unit,*) lat_od(j)
        end do
      close(28)

!-- read in aerosol optical depth lon data

      lon_unit = 29
        OPEN(lon_unit, FILE='aerosol_lon.formatted',FORM='FORMATTED',STATUS='OLD')
        do j = 1,lonsiz
        READ (lon_unit,*) lon_od(j)
        end do
      close(29)

!-- read in ozone data

      od_unit = 30
      OPEN(od_unit, FILE='aerosol.formatted',FORM='FORMATTED',STATUS='OLD')

      do ks=1,no_src_types
      do m=1,no_months
      do j=1,latsiz  ! latsiz=46
      do k=1,alevsiz ! alevsiz=12
      do i=1,lonsiz  ! lonsiz=72
         READ (od_unit,*) aerodin(i,k,j,m,ks)
      enddo
      enddo
      enddo
      enddo
      enddo
      close(30)

!-- latitudinally interpolate ozone data (and extend longitudinally)
!-- using function lin_interpol2(x, f, y) result(g)
! Purpose:
!   interpolates f(x) to point y
!   assuming f(x) = f(x0) + a * (x - x0)
!   where a = ( f(x1) - f(x0) ) / (x1 - x0)
!   x0 <= x <= x1
!   assumes x is monotonically increasing
!    real, intent(in), dimension(:) :: x  ! grid points
!    real, intent(in), dimension(:) :: f  ! grid function values
!    real, intent(in) :: y                ! interpolation point
!    real :: g                            ! interpolated function value
!---------------------------------------------------------------------------

      do j=jts,jtf
      do i=its,itf
        interp_pt_lat=XLAT(i,j)
        interp_pt_lon=XLONG(i,j)
        call interp_vec(lat_od,interp_pt_lat,.true.,jlat1,jlat2,wlat1,wlat2)
        call interp_vec(lon_od,interp_pt_lon,.true.,ilon1,ilon2,wlon1,wlon2)

        do ks = 1,no_src_types
        do m  = 1,no_months
        do k  = 1,alevsiz
          aerodm(i,k,j,m,ks) = wlon1 * (wlat1 * aerodin(ilon1,k,jlat1,m,ks)  + &
                                        wlat2 * aerodin(ilon1,k,jlat2,m,ks)) + &
                               wlon2 * (wlat1 * aerodin(ilon2,k,jlat1,m,ks)  + &
                                        wlat2 * aerodin(ilon2,k,jlat2,m,ks))
        end do
        end do
        end do

      end do
      end do

!     do j=jts,jtf
!     do i=its,itf
!        onefld(i,j) = aerodm(i,12,j,1,1)
!     enddo
!     enddo

END SUBROUTINE aerosol_in

  function lin_interp(x, f, y) result(g)

    ! Purpose:
    !   interpolates f(x) to point y
    !   assuming f(x) = f(x0) + a * (x - x0)
    !   where a = ( f(x1) - f(x0) ) / (x1 - x0)
    !   x0 <= x <= x1
    !   assumes x is monotonically increasing

    ! Author: D. Fillmore ::  J. Done changed from r8 to r4

    implicit none

    real, intent(in), dimension(:) :: x  ! grid points
    real, intent(in), dimension(:) :: f  ! grid function values
    real, intent(in) :: y                ! interpolation point
    real :: g                            ! interpolated function value

    integer :: k  ! interpolation point index
    integer :: n  ! length of x
    real    :: a

    n = size(x)

    ! find k such that x(k) < y =< x(k+1)
    ! set k = 1 if y <= x(1)  and  k = n-1 if y > x(n)

    if (y <= x(1)) then
      k = 1
    else if (y >= x(n)) then
      k = n - 1
    else
      k = 1
      do while (y > x(k+1) .and. k < n)
        k = k + 1
      end do
    end if

    ! interpolate
    a = (  f(k+1) - f(k) ) / ( x(k+1) - x(k) )
    g = f(k) + a * (y - x(k))

  end function lin_interp

  subroutine interp_vec(locvec,locwant,periodic,loc1,loc2,wght1,wght2)

  implicit none

  real, intent(in), dimension(:) :: locvec
  real, intent(in)               :: locwant
  logical, intent(in)            :: periodic
  integer, intent(out)           :: loc1, loc2
  real, intent(out)              :: wght1, wght2

  integer :: vsize, n
  real    :: locv1, locv2

  vsize = size(locvec)

  loc1 = -1
  loc2 = -1

  do n = 1, vsize-1
    if ( locvec(n) <= locwant .and. locvec(n+1) > locwant ) then
      loc1  = n
      loc2  = n+1
      locv1 = locvec(n)
      locv2 = locvec(n+1)
      exit
    end if
  end do

  if ( loc1 < 0 .and. loc2 < 0 ) then
    if ( periodic ) then
      if ( locwant < locvec(1) ) then
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)-360.0
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)
        locv2 = locvec(1)+360.0
      end if
    else
      if ( locwant < locvec(1) ) then
        loc1  = 1
        loc2  = 1
        locv1 = locvec(1)
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = vsize
        locv1 = locvec(vsize)
        locv2 = locvec(vsize)
      end if
    end if
  end if

  wght2 = (locwant-locv1) / (locv2-locv1)
  wght1 = 1.0 - wght2

  return
  end subroutine interp_vec

!!--------------------START   PSH/TWG   CHANGES-----------------------
!--------------------INPUT FOR AEROSOL DATA---------------------------
subroutine aerosol_in_cu(aeromcu,alevsiz,no_months,no_src_types,XLAT,XLONG,aeropcu, &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte)
!
! This module was taken from the radiation driver and modified for the
! cumulus aerosol input. - PSH
!
  use netcdf !PSH
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   alevsiz, no_months, no_src_types

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN   )  ::     XLAT, XLONG

   INTEGER :: i, j, k, itf, jtf, ktf, m, pin_unit, lat_unit,lon_unit,od_unit,ks,il, jl
   INTEGER :: ilon1cu, ilon2cu, jlat1cu, jlat2cu
   REAL    :: interp_ptcu, interp_pt_latcu, interp_pt_loncu,wlat1cu,wlat2cu,wlon1cu,wlon2cu
   CHARACTER*256 :: message

   integer                               :: ncId, lonID, latID, levID, &
                                            lonDimID, latDimId, timeDimId, &
                                            numLons, numLats, numLevs,    &
                                            ncfcode, &
                                            varID

   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months, no_src_types ),&
          INTENT(OUT   ) ::                                  aeromcu !PSH
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months), OPTIONAL, &
   INTENT(OUT)  :: aeropcu !PSH
   integer, dimension(nf90_max_var_dims) :: dimIDs
   real, dimension(:, :, :, :, :), allocatable  :: aeroin !PSH
   real, dimension(:, :, :, :), allocatable  :: aeropin !PSH
   CHARACTER (len=10), dimension(no_src_types) :: species !PSH
   CHARACTER (len=100) :: fname !PSH
   CHARACTER (len=8) :: frmt !PSH
   CHARACTER (len=2) :: mons !PSH
   integer :: v, mi !PSH
   REAL, dimension(:), ALLOCATABLE :: lon_aer, lat_aer !, lev_aer !PSH
   LOGICAL           :: exists
   INTEGER, EXTERNAL :: get_unused_unit
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER :: istatus, iunit
   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

! - - - - SPECIES OF INTEREST - - - - -
   species = (/"DUST1   ","DUST2   ","DUST3   ","DUST4   ","SEASALT2", &
               "SULFATE ","BCPHO   ","BCPHI   ","OCPHO   ","OCPHI   "/)
   frmt = '(I0.2)'

      INQUIRE(FILE='./CESM_RCP4.5_Aerosol_Data.dat', EXIST=exists)

      IF (exists) THEN
          iunit = get_unused_unit()
          IF ( iunit <= 0 ) THEN
             IF ( wrf_dm_on_monitor() ) THEN
                CALL wrf_error_fatal('Error in aerosol_in_cu: could not find a free Fortran unit.')
             END IF
          END IF

          ! Read aerosol information 
          OPEN(UNIT=iunit, FILE='CESM_RCP4.5_Aerosol_Data.dat', FORM='unformatted', &
               STATUS='old', IOSTAT=istatus)
            IF (istatus == 0) THEN
               READ(UNIT=iunit)  numLons, numLats,numLevs
               allocate(aeroin(numLons, numLats, numLevs, no_months, no_src_types))
               allocate(aeropin(numLons, numLats, numLevs, no_months))
               allocate(lon_aer(numLons))
               allocate(lat_aer(numLats))
               READ(UNIT=iunit)  lat_aer
               READ(UNIT=iunit)  lon_aer
               READ(UNIT=iunit)  aeropin
               READ(UNIT=iunit)  aeroin
            END IF
          CLOSE(iunit)
      END IF
!---------------------------------------------------------------------------
!-- latitudinally interpolate ozone data (and extend longitudinally)
!-- using function lin_interpol2(x, f, y) result(g)
! Purpose:
!   interpolates f(x) to point y
!   assuming f(x) = f(x0) + a * (x - x0)
!   where a = ( f(x1) - f(x0) ) / (x1 - x0)
!   x0 <= x <= x1
!   assumes x is monotonically increasing
!    real, intent(in), dimension(:) :: x  ! grid points
!    real, intent(in), dimension(:) :: f  ! grid function values
!    real, intent(in) :: y                ! interpolation point
!    real :: g                            ! interpolated function value
!---------------------------------------------------------------------------

      do j=jts,jtf
      do i=its,itf
        interp_pt_latcu=XLAT(i,j)
        interp_pt_loncu=XLONG(i,j)
        call interp_vec_cu(lat_aer,interp_pt_latcu,.true.,jlat1cu,jlat2cu,wlat1cu,wlat2cu)
        call interp_vec_cu(lon_aer,interp_pt_loncu,.true.,ilon1cu,ilon2cu,wlon1cu,wlon2cu)

        do m  = 1,no_months
        do k  = 1,alevsiz
          aeropcu(i,k,j,m) = wlon1cu * (wlat1cu * aeropin(ilon1cu,jlat1cu,k,m)+&
                                           wlat2cu*aeropin(ilon1cu,jlat2cu,k,m))+&
                                wlon2cu * (wlat1cu*aeropin(ilon2cu,jlat1cu,k,m)+&
                                           wlat2cu*aeropin(ilon2cu,jlat2cu,k,m))
        do ks = 1,no_src_types
          aeromcu(i,k,j,m,ks) = wlon1cu * (wlat1cu*aeroin(ilon1cu,jlat1cu,k,m,ks)  + &
                                           wlat2cu*aeroin(ilon1cu,jlat2cu,k,m,ks))+ &
                                wlon2cu * (wlat1cu*aeroin(ilon2cu,jlat1cu,k,m,ks)  + &
                                           wlat2cu*aeroin(ilon2cu,jlat2cu,k,m,ks))

        end do
        end do
        end do
      end do
      end do

END SUBROUTINE aerosol_in_cu

subroutine interp_vec_cu(locvec,locwant,periodic,loc1,loc2,wght1,wght2)
! This subroutine was taken and modified from the radiation driver. -PSH
  implicit none

  real, intent(in), dimension(:) :: locvec
  real, intent(in)               :: locwant
  logical, intent(in)            :: periodic
  integer, intent(out)           :: loc1, loc2
  real, intent(out)              :: wght1, wght2

  integer :: vsize, n
  real    :: locv1, locv2

  vsize = size(locvec)

  loc1 = -1
  loc2 = -1

  do n = 1, vsize-1
    if ( locvec(n) <= locwant .and. locvec(n+1) > locwant ) then
      loc1  = n
      loc2  = n+1
      locv1 = locvec(n)
      locv2 = locvec(n+1)
      exit
    end if
  end do

  if ( loc1 < 0 .and. loc2 < 0 ) then
    if ( periodic ) then
      if ( locwant < locvec(1) ) then
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)-360.0
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = 1
        locv1 = locvec(vsize)
        locv2 = locvec(1)+360.0
      end if
    else
      if ( locwant < locvec(1) ) then
        loc1  = 1
        loc2  = 1
        locv1 = locvec(1)
        locv2 = locvec(1)
      else
        loc1  = vsize
        loc2  = vsize
        locv1 = locvec(vsize)
        locv2 = locvec(vsize)
      end if
    end if
  end if

  wght2 = (locwant-locv1) / (locv2-locv1)
  wght1 = 1.0 - wght2

  return
  end subroutine interp_vec_cu
!---------------------END   PSH/TWG  CHANGES----------------

END MODULE module_physics_init
