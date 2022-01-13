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
                         PHB,PH,P,PB,QV,CCNTY,QDCN,QTCN,QCCN,    & ! for ntu3m
                         QRCN,QNIN,                              & ! for ntu3m
                         re_cloud_gsfc, re_ice_gsfc,             & 
                         re_snow_gsfc,                           & ! Goddard
                         re_graupel_gsfc, re_hail_gsfc,          &
                         re_rain_gsfc,                           & ! Goddard
#endif
                         NCA,swrad_scat,                         &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         urban_map_zgrd,                         &
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
                         CHXY, FWETXY, SNEQVOXY, ALBOLDXY,QSNOWXY,QRAINXY,& ! Optional Noah-MP
                         WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY, & ! Optional Noah-MP
                         STMASSXY, WOODXY, STBLCPXY, FASTCPXY,            & ! Optional Noah-MP
                         GRAINXY, GDDXY,                                  & ! Optional Noah-MP
                         croptype, cropcat,                               & ! Noah-MP Crop model
                         iopt_crop,                                       & 
                         iopt_irr, iopt_irrm,                             &   
                         IRNUMSI, IRNUMMI, IRNUMFI, IRWATSI, IRWATMI,     &   ! Optional Noah-MP
                         IRWATFI, IRELOSS, IRSIVOL, IRMIVOL, IRFIVOL,     &   ! Optional Noah-MP
                         IRRSPLH,                                         &
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
                         ,pep                                  & ! EEPS
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
                         ,irr_rand_field,irr_ph,irr_freq                                                      &
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
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT), OPTIONAL :: DX2D, AREA2D
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
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(IN)    ::                     U10, &
                                                           V10
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(OUT)    ::                   U10E, &
                                                          V10E
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
  REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE,pep

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
   INTEGER, INTENT(IN ) :: iopt_crop, iopt_irr, iopt_irrm 
   REAL,    OPTIONAL, DIMENSION(ims:ime,60,jms:jme) :: gecros_state      ! Optional gecros crop

!  Irrigation
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRWATSI 
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRWATMI
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRWATFI
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRELOSS
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRSIVOL
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRMIVOL
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRFIVOL
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRRSPLH
   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRNUMSI
   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRNUMMI
   INTEGER, OPTIONAL, DIMENSION(ims:ime,jms:jme) :: IRNUMFI
                                                                
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
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: EP_PV_URB3D!GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf,jms:jme ), INTENT(INOUT) :: T_PV_URB3D!GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme),INTENT(INOUT) :: TRV_URB4D ! GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme),INTENT(INOUT) :: QR_URB4D ! GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme), INTENT(INOUT) :: QGR_URB3D ! GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme), INTENT(INOUT) :: TGR_URB3D ! GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme),INTENT(INOUT) :: DRAIN_URB4D !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRAINGR_URB3D   !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme),INTENT(INOUT) :: SFRV_URB3D !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme),INTENT(INOUT) :: LFRV_URB3D ! GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: DGR_URB3D !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: DG_URB3D !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: LFR_URB3D !GRZ
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: LFG_URB3D !GRZ
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
#if (WRF_USE_CLM == 1) && ( WRF_CHEM == 1 )
   LOGICAL, INTENT(IN)          :: megan_mapped_emisfctrs
   CHARACTER(LEN=*), INTENT(IN) :: megan_factors_file
   CHARACTER(LEN=*), INTENT(IN) :: megan_specifier(:)
#endif



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
         config_flags%mp_physics  .eq. P3_2CATEGORY .or.    &    
         config_flags%mp_physics  .eq. P3_1CAT_3MOM   ) ) then    ! P3
      has_reqc = 1
      has_reqi = 1
      has_reqs = 1
   endif

! for P3, to ensure correct coupling with predicted effective radii
   if (config_flags%mp_physics .eq. P3_1CATEGORY .or.       &
       config_flags%mp_physics .eq. P3_1CATEGORY_NC .or.    &
       config_flags%mp_physics .eq. JENSEN_ISHMAEL .or.    &
      IF ((   config_flags%mp_physics == THOMPSON .OR.        &
              config_flags%mp_physics == THOMPSONAERO .OR.    &
              config_flags%mp_physics == WSM3SCHEME .OR.    &
              config_flags%mp_physics == WSM5SCHEME .OR.    &
              config_flags%mp_physics == WSM6SCHEME .OR.    &
              config_flags%mp_physics == WSM7SCHEME .OR.    &
              config_flags%mp_physics == WDM5SCHEME .OR.    &
              config_flags%mp_physics == WDM6SCHEME .OR.    &
              config_flags%mp_physics == WDM7SCHEME ).OR.     &
          (( has_reqc .EQ. 0 .AND. has_reqi .EQ. 0 .and. has_reqs .EQ. 0 ) .AND. &
           ( f_qc            ) .AND. &
           ( f_qi            ) .AND. &
           ( f_qs            ))) THEN
         !  everything is A-OK for FARMS
      ELSE
         CALL wrf_error_fatal ('--- ERROR: FARMS (swint_opt==2) requires a different MP scheme (Please see the module_physics_init')
      END IF
   END IF

   ENDIF ! use_mp_re .EQ. 1

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
!.. cloud water (2.49 microns), cloud ice (4.99 microns), and
!.. snow (9.99 microns).
    if (has_reqc.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_cloud(i,k,j) = RE_QC_BG
          end do
          end do
       end do
    endif
    if (has_reqi.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_ice(i,k,j) = RE_QI_BG
          end do
          end do
       end do
    endif
    if (has_reqs.ne.0) then
       do j=jts,jtf
          do k=kts,ktf
          do i=its,itf
             re_snow(i,k,j) = RE_QS_BG
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

!  Initializing special output from myjsfc and qnsesfc

   if ( config_flags%sf_sfclay_physics .EQ. MYJSFCSCHEME .OR. &
        config_flags%sf_sfclay_physics .EQ. QNSESFCSCHEME ) THEN
      do j=jts,jtf
         do i=its,itf
            u10e(i,j) = u10(i,j)
            v10e(i,j) = v10(i,j)
         end do
      end do
   end if

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
                num_soil_layers,TKE_PBL,mfshconv,pep,           &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, itimestep,id,                          &
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
                XLAT, XLONG,                                    &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY,ALBOLDXY,QSNOWXY,QRAINXY,&
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                GRAINXY, GDDXY,                                 & ! Noah-MP Crop model
                croptype, cropcat,                              & ! Noah-MP Crop model
                iopt_crop,                                      & 
                urban_map_zgrd,                                 & !Optional multi-layer urban
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
#if (WRF_USE_CLM == 1) && ( WRF_CHEM == 1)
                ,megan_specifier, megan_factors_file                &
                ,megan_mapped_emisfctrs                             &
#endif
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
                ,irr_rand_field,irr_ph,irr_freq                 &
               ) 

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(DX,STEPCU,CUDT,DT,RUCUTEN,RVCUTEN,RTHCUTEN,     &
                RQVCUTEN,RQRCUTEN,RQCCUTEN,RQSCUTEN,RQICUTEN,   &
                NCA,RAINC,RAINCV,W0AVG,config_flags,restart,    &
                CLDEFI,LOWLYR,                                  &
                MASS_FLUX,RTHFTEN,RQVFTEN,                      &
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
                num_soil_layers,TKE_PBL,mfshconv,pep,           &
                massflux_EDKF, entr_EDKF, detr_EDKF, & 
                thl_up, thv_up, rt_up,       &
                rv_up, rc_up, u_up, v_up,    &
                frac_up, itimestep, id,                         &
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
                XLAT, XLONG,                                    &
                ISNOWXY, ZSNSOXY, TSNOXY,                       &
                SNICEXY, SNLIQXY, TVXY, TGXY, CANICEXY,         &
                CANLIQXY, EAHXY, TAHXY, CMXY,                   &
                CHXY, FWETXY, SNEQVOXY,ALBOLDXY,QSNOWXY,QRAINXY,&
                WSLAKEXY, ZWTXY, WAXY, WTXY, LFMASSXY, RTMASSXY,&
                STMASSXY, WOODXY, STBLCPXY, FASTCPXY,           &
                GRAINXY, GDDXY,                                 & ! Noah-MP Crop model
                croptype, cropcat,                              & ! Noah-MP Crop model
                iopt_crop,                                      &
                urban_map_zgrd,                                 & !Optional multi-layer urban
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
#if (WRF_USE_CLM == 1) && ( WRF_CHEM == 1 )
                ,megan_specifier, megan_factors_file     &
                ,megan_mapped_emisfctrs                  &
#endif
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
                ,irr_rand_field,irr_ph,irr_freq                 &
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
   USE module_sf_noahlsm, only : LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11
   USE module_sf_noahmpdrv
   USE noahmp_tables, ONLY: LCZ_1_TABLE,LCZ_2_TABLE,LCZ_3_TABLE,LCZ_4_TABLE,LCZ_5_TABLE,LCZ_6_TABLE,LCZ_7_TABLE,LCZ_8_TABLE,LCZ_9_TABLE,LCZ_10_TABLE,LCZ_11_TABLE
#ifdef WRF_USE_CLM
   USE module_sf_clm, only : clminit
#endif
#ifdef WRF_USE_CTSM
   USE module_sf_ctsm, only : ctsm_init
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
   INTEGER , INTENT(IN)        ::     id
   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     SF_URBAN_PHYSICS 
   INTEGER , INTENT(IN)        ::     IOPT_RUN

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT), OPTIONAL :: QKE, pep

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
   REAL,    OPTIONAL, DIMENSION(ims:ime,jms:jme) :: QRAINXY
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
   INTEGER , INTENT(IN)        ::     iopt_crop, iopt_irr, iopt_irrm
   REAL,    OPTIONAL, DIMENSION(ims:ime,60,jms:jme) :: gecros_state                  ! Optional gecros crop
   INTEGER, OPTIONAL, INTENT(IN)  ::  ISICE
   INTEGER , OPTIONAL,  INTENT(OUT) :: STEPWTD
   REAL , OPTIONAL, INTENT(IN) :: WTDDT

    INTEGER , INTENT(IN)        ::     urban_map_zgrd
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
   INTEGER :: i,j,patch

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
  REAL :: max_utype_urb2d   
  LOGICAL :: IPRINT
  
!-------------------------------------------------
! End of Noah-mosaic related variables 
!-------------------------------------------------  

! IRRIGATION
  INTEGER,OPTIONAL,INTENT(OUT), DIMENSION( ims:ime,jms:jme):: irr_rand_field  
  INTEGER,OPTIONAL :: irr_ph,irr_freq  

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
                     irr_rand_field,irr_ph,irr_freq,           &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 &
                     )


!URBAN
          IF ((SF_URBAN_PHYSICS.eq.1).OR.(SF_URBAN_PHYSICS.EQ.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN

             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN
                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers,             & !urban
                                sf_urban_physics,config_flags%use_wudapt_lcz)    !urban
                               
                
                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,               & !urban
                              ims,ime,jms,jme,kms,kme,num_soil_layers,         & !urban
                              LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,                   &
                              LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11,           &
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
                              EP_PV_URB3D,T_PV_URB3D,                          & !GRZ
                              TRV_URB4D,QR_URB4D,QGR_URB3D,TGR_URB3D,          & !GRZ
                              DRAIN_URB4D,DRAINGR_URB3D,SFRV_URB3D,            & !GRZ
                              LFRV_URB3D,DGR_URB3D,DG_URB3D,LFR_URB3D,LFG_URB3D,&!GRZ
                              SMOIS,                                           & !GRZ
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
                              FRC_URB2D, UTYPE_URB2D,config_flags%use_wudapt_lcz)  !urban
          

               max_utype_urb2d = maxval(UTYPE_URB2D)*1.0
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
               max_utype_urb2d = wrf_dm_max_real(max_utype_urb2d)
#endif
               IF (config_flags%use_wudapt_lcz.eq.0 .and. max_utype_urb2d.gt.3.0) THEN  !new LCZ
                 CALL wrf_error_fatal &
                 ('USING 10 WUDAPT LCZ WITHOUT URBPARM_LCZ.TBL. SET USE_WUDAPT_LCZ=1')
               ENDIF
               IF (config_flags%use_wudapt_lcz.eq.1 .and. max_utype_urb2d.le.3.0) THEN  ! new LCZ
                 CALL wrf_error_fatal &
                 ('USING URBPARM_LCZ.TBL WITH OLD 3 URBAN CLASSES. SET USE_WUDAPT_LCZ=0')
               ENDIF


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
                     fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,qrainxy, wslakexy, zwtxy, waxy,&
                     wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
                     stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   ,lai      ,           &
                     grainxy  ,gddxy    ,                                                   &
                     croptype ,cropcat  ,                      &
                     irnumsi  ,irnummi  ,irnumfi  ,irwatsi,    &
                     irwatmi  ,irwatfi  ,ireloss  ,irsivol,    &
                     irmivol  ,irfivol  ,irrsplh  ,            &
                     t2mvxy   ,t2mbxy   ,chstarxy ,            &
                     num_soil_layers, restart,                 &
                     allowed_to_read, iopt_run , iopt_crop,    &
                     iopt_irr, iopt_irrm,                      &
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
                                sf_urban_physics,config_flags%use_wudapt_lcz)
                CALL urban_var_init(ISURBAN,TSK,TSLB,TMN,IVGTYP,               & !urban
                              ims,ime,jms,jme,kms,kme,num_soil_layers,         & !urban
                              LCZ_1_TABLE,LCZ_2_TABLE,LCZ_3_TABLE,LCZ_4_TABLE, & !urban
                              LCZ_5_TABLE,LCZ_6_TABLE,LCZ_7_TABLE,LCZ_8_TABLE, & !urban
                              LCZ_9_TABLE,LCZ_10_TABLE,LCZ_11_TABLE,           & !urban
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
                              EP_PV_URB3D,T_PV_URB3D,                          & !GRZ
                              TRV_URB4D,QR_URB4D,QGR_URB3D,TGR_URB3D,          & !GRZ
                              DRAIN_URB4D,DRAINGR_URB3D,SFRV_URB3D,            & !GRZ
                              LFRV_URB3D,DGR_URB3D,DG_URB3D,LFR_URB3D,LFG_URB3D,&!GRZ
                              SMOIS,                                           & 
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
                              FRC_URB2D, UTYPE_URB2D,config_flags%use_wudapt_lcz)!urban

               max_utype_urb2d = maxval(UTYPE_URB2D)*1.0
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
               max_utype_urb2d = wrf_dm_max_real(max_utype_urb2d)
#endif

               IF (config_flags%use_wudapt_lcz.eq.0 .and. max_utype_urb2d.gt.3.) THEN  !new LCZ
                 CALL wrf_error_fatal &
                 ('USING 10 WUDAPT LCZ WITHOUT URBPARM_LCZ.TBL. SET USE_WUDAPT_LCZ=1')
               ENDIF
               IF (config_flags%use_wudapt_lcz.eq.1 .and. max_utype_urb2d.le.3.) THEN  ! new LCZ
                 CALL wrf_error_fatal &
                 ('USING URBPARM_LCZ.TBL WITH OLD 3 URBAN CLASSES. SET USE_WUDAPT_LCZ=0')
               ENDIF


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
                     irr_rand_field,irr_ph,irr_freq,           &
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

#if ( WRF_CHEM == 1 )
        if( config_flags%bio_emiss_opt == MEGAN2_CLM ) then
            do_readmeganfile = .true.
        endif
#endif

            CALL CLMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,    &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,            &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS,  &
                     FNDSOILW, FNDSNOWH,                           &
                     num_soil_layers, restart,                     &
                     allowed_to_read, id, do_readmeganfile, &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte,                    &
#if ( WRF_CHEM == 1 )
                     megan_specifier,                              &
                     megan_mapped_emisfctrs,                       &
                     megan_factors_file,                           &
#endif
                     dt,maxpatch                                         &
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
                     SWUPsubgrid,lhsoi,lhveg,lhtran,config_flags%chem_opt  &
                    )
        ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling CLM' )
        ENDIF
#ifdef WRF_USE_CTSM
      CASE (CTSMSCHEME)

         CALL ctsm_init( &
              ids=ids, ide=ide, jds=jds, jde=jde, &
              ims=ims, ime=ime, jms=jms, jme=jme, &
              its=its, ite=ite, jts=jts, jte=jte, &
              dt=DT, xlat=XLAT, xlong=XLONG,      &
              atm_restart = restart)
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
           
           SELECT CASE(config_flags%bl_pbl_physics)

             CASE(MYNNPBLSCHEME2)
                mynn_closure_level=2

             CASE(MYNNPBLSCHEME3)
                mynn_closure_level=3

             CASE DEFAULT

           END SELECT

           CALL mynn_bl_init_driver(&
                &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN, &
                &RQIBLTEN,                                   &
                &QKE,EXCH_H                                  &
                &,restart,allowed_to_read,mynn_closure_level &
                &,IDS,IDE,JDS,JDE,KDS,KDE                    &
                &,IMS,IME,JMS,JME,KMS,KME                    &
                &,ITS,ITE,JTS,JTE,KTS,KTE)

      CASE (TEMFPBLSCHEME)
           ! if(isfc .ne. 0)CALL wrf_error_fatal &
           !  ( 'module_physics_init: use sfclay scheme = 0 for this pbl option' )
           IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) CALL wrf_error_fatal &
            ( 'module_physics_init: use ysu (option1), myj (option 2), or boulac (option 8) with BEP/BEM urban scheme' )
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

#if ( EM_CORE == 1 )
                      PHB,PH,P,PB,QV,XLAND,CCNTY,QDCN,QTCN,QCCN,  & ! for ntu3m
                      QRCN,QNIN,                                  & ! for ntu3m
#endif
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
#if (BUILD_SBM_FAST == 1)
   USE module_mp_fast_sbm
#endif
   USE module_mp_morr_two_moment
   USE module_mp_p3
   USE module_mp_jensen_ishmael
   USE module_mp_milbrandt2mom
!  USE module_mp_milbrandt3mom
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_wdm7
   USE module_mp_nssl_2mom, only: nssl_2mom_init
#if (EM_CORE==1)
   USE module_mp_cammgmp_driver, ONLY:CAMMGMP_INIT !CAM5's microphysics
   USE module_mp_morr_two_moment_aero              !TWG2017
   USE module_mp_ntu
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
         CALL p3_init('.',1,.false.,'WRF',i_err,.false.)
     CASE (P3_1CATEGORY_NC)
         CALL p3_init('.',1,.false.,'WRF',i_err,.false.)
     CASE (P3_2CATEGORY)
         CALL p3_init('.',2,.false.,'WRF',i_err,.false.)
     CASE (P3_1CAT_3MOM)
         CALL p3_init('.',1,.true.,'WRF',i_err,.false.)
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
   if (config_flags%madwrf_opt == 2 .and. (.not. f_qc .or. .not. f_qi .or. .not. f_qs)) &
        call wrf_error_fatal('madwrf_opt = 2 requires a mp_physics option with qc, qi, and qs')

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

   integer                               :: numLons, numLats, numLevs

   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months, no_src_types ),&
          INTENT(OUT   ) ::                                  aeromcu !PSH
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_months), OPTIONAL, &
   INTENT(OUT)  :: aeropcu !PSH
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

  subroutine compute_2d_dx_area(dx, dy, msftx, msfty, dx2d, area2d, &
                                ids, ide, jds, jde, kds, kde,       &
                                ims, ime, jms, jme, kms, kme,       &
                                its, ite, jts, jte, kts, kte)

     implicit none

     integer , intent(in) :: ids, ide, jds, jde, kds, kde,       &
                             ims, ime, jms, jme, kms, kme,       &
                             its, ite, jts, jte, kts, kte

     real,                             intent(in)  :: dx, dy
     real, dimension(ims:ime,jms:jme), intent(in)  :: msftx, msfty
     real, dimension(ims:ime,jms:jme), intent(out) :: dx2d, area2d

     integer :: i, j

#if 0
     do j = jts, min(jde-1,jte)
        do i = its, min(ide-1,ite)
           area2d(i,j) = dx/msftx(i,j) * dy/msfty(i,j)
           dx2d  (i,j) = sqrt ( area2d(i,j) )
        end do
     end do
#else
     do j = jts, min(jde-1,jte)
        do i = its, min(ide-1,ite)
           area2d(i,j) = dx * dy
           dx2d  (i,j) = dx
        end do
     end do
#endif

  end subroutine compute_2d_dx_area

END MODULE module_physics_init
