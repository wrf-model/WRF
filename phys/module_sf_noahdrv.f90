MODULE module_sf_noahdrv


  USE module_sf_noahlsm,  only: SFLX, XLF, XLV, CP, R_D, RHOWATER, NATURAL, SHDTBL, LUTYPE, SLTYPE, STBOLT, &
      &                         KARMAN, LUCATS, NROTBL, RSTBL, RGLTBL, HSTBL, SNUPTBL, MAXALB, LAIMINTBL,   &
      &                         LAIMAXTBL, Z0MINTBL, Z0MAXTBL, ALBEDOMINTBL, ALBEDOMAXTBL, EMISSMINTBL,     &
      &                         EMISSMAXTBL, TOPT_DATA, CMCMAX_DATA, CFACTR_DATA, RSMAX_DATA, BARE, NLUS,   &
      &                         SLCATS, BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ, &
      &                         NSLTYPE, SLPCATS, SLOPE_DATA, SBETA_DATA, FXEXP_DATA, CSOIL_DATA,           &
      &                         SALP_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA, CZIL_DATA,        &
      &                         SMLOW_DATA, SMHIGH_DATA, LVCOEF_DATA, NSLOPE, &
      &                         FRH2O,ZTOPVTBL,ZBOTVTBL, &
      &                         LCZ_1,LCZ_2,LCZ_3,LCZ_4,LCZ_5,LCZ_6,LCZ_7,LCZ_8,LCZ_9,LCZ_10,LCZ_11
  USE module_sf_urban,    only: urban, oasis, IRI_SCHEME
  USE module_sf_noahlsm_glacial_only, only: sflx_glacial
  USE module_sf_bep,      only: bep
  USE module_sf_bep_bem,  only: bep_bem
 use module_ra_gfdleta,  only: cal_mon_day
 use module_wrf_error



CONTAINS




   SUBROUTINE lsm(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
                  HFX,QFX,LH,GRDFLX, QGH,GSW,SWDOWN,SWDDIR,SWDDIF,&
                  GLW,SMSTAV,SMSTOT, &
                  SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,ISURBAN,ISICE,VEGFRA,    &
                  ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE,EMISS,EMBCK,   &
                  SNOWC,QSFC,RAINBL,MMINLU,                     &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,                       &
                  CHS,CHS2,CQS2,CPM,ROVCP,SR,chklowq,lai,qz0,   & 
                  myj,frpcpn,                                   &
                  SH2O,SNOWH,                                   & 
                  U_PHY,V_PHY,                                  & 
                  SNOALB,SHDMIN,SHDMAX,                         & 
                  SNOTIME,                                      & 
                  ACSNOM,ACSNOW,                                & 
                  SNOPCX,                                       & 
                  POTEVP,                                       & 
                  SMCREL,                                       & 
                  XICE_THRESHOLD,                               &
                  RDLAI2D,USEMONALB,                            &
                  RIB,                                          & 
                  NOAHRES,opt_thcnd,                            &

                  ua_phys,flx4_2d,fvb_2d,fbur_2d,fgsn_2d,       &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  sf_urban_physics,                             &
                  CMR_SFCDIF,CHR_SFCDIF,CMC_SFCDIF,CHC_SFCDIF,  &
                  CMGR_SFCDIF,CHGR_SFCDIF,                      &

                  TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D, & 
                  UC_URB2D,                                     & 
                  XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,  & 
                  TRL_URB3D,TBL_URB3D,TGL_URB3D,                & 
                  SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,TS_URB2D,  & 
                  PSIM_URB2D,PSIH_URB2D,U10_URB2D,V10_URB2D,    & 
                  GZ1OZ0_URB2D,  AKMS_URB2D,                    & 
                  TH2_URB2D,Q2_URB2D, UST_URB2D,                & 
                  DECLIN_URB,COSZ_URB2D,OMG_URB2D,              & 
                  XLAT_URB2D,                                   & 
                  num_roof_layers, num_wall_layers,             & 
                  num_road_layers, DZR, DZB, DZG,               & 
                  CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,    & 
                  DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,          & 
                  FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,    & 
                  julian, julyr,                                & 
                  FRC_URB2D,UTYPE_URB2D,                        & 
                  num_urban_ndm,                                & 
                  urban_map_zrd,                                & 
                  urban_map_zwd,                                & 
                  urban_map_gd,                                 & 
                  urban_map_zd,                                 & 
                  urban_map_zdf,                                & 
                  urban_map_bd,                                 & 
                  urban_map_wd,                                 & 
                  urban_map_gbd,                                & 
                  urban_map_fbd,                                & 
                  urban_map_zgrd,                                & 
                  num_urban_hi,                                 & 
                  tsk_rural_bep,                                & 
                  trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,      & 
                  tlev_urb3d,qlev_urb3d,                        & 
                  tw1lev_urb3d,tw2lev_urb3d,                    & 
                  tglev_urb3d,tflev_urb3d,                      & 
                  sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,          & 
                  sfvent_urb3d,lfvent_urb3d,                    & 
                  sfwin1_urb3d,sfwin2_urb3d,                    & 
                  sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,    & 
                  ep_pv_urb3d,t_pv_urb3d,                         & 
                  trv_urb4d,qr_urb4d,qgr_urb3d,tgr_urb3d,         & 
                  drain_urb4d,draingr_urb3d,sfrv_urb3d,           & 
                  lfrv_urb3d,dgr_urb3d,dg_urb3d,lfr_urb3d,lfg_urb3d,& 
                  lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,         & 
                  mh_urb2d,stdh_urb2d,lf_urb2d,                 & 
                  th_phy,rho,p_phy,ust,                         & 
                  gmt,julday,xlong,xlat,                        & 
                  a_u_bep,a_v_bep,a_t_bep,a_q_bep,              & 
                  a_e_bep,b_u_bep,b_v_bep,                      & 
                  b_t_bep,b_q_bep,b_e_bep,dlg_bep,              & 
                  dl_u_bep,sf_bep,vl_bep,sfcheadrt,INFXSRT, soldrain      &   
                 ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas     &   
                 ,RC2,XLAI2                                         &
                 ,IRR_CHAN                                          &
                 )


    IMPLICIT NONE








































































































































   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER,  INTENT(IN   )   ::  sf_urban_physics               
   INTEGER,  INTENT(IN   )   ::  isurban
   INTEGER,  INTENT(IN   )   ::  isice
   INTEGER,  INTENT(IN   )   ::  julian, julyr                  


    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
             INTENT(INOUT)  :: sfcheadrt,INFXSRT,soldrain 
    real :: etpnd1




   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        SHDMIN, &
                                                        SHDMAX, &
                                                        SNOALB, &
                                                           GSW, &
                                                        SWDOWN, & 
                                                           GLW, &
                                                        RAINBL, &
                                                        EMBCK,  &
                                                           SR,  &
                                                       SWDDIR,  &
                                                       SWDDIF

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         ALBBCK, &
                                                            Z0
   CHARACTER(LEN=*), INTENT(IN   )    ::                 MMINLU

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                         p8w3D, &
                                                          DZ8W, &
                                                          T3D
   REAL,     DIMENSION( ims:ime, jms:jme )                    , &
             INTENT(IN   )               ::               QGH,  &
                                                          CPM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                         IVGTYP, &
                                                        ISLTYP

   INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP

   REAL,     INTENT(IN   )   ::     DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS



   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & 
                                                         SH2O,  & 
                                                         TSLB     

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(OUT)     ::                         SMCREL

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & 
                                                           HFX, &
                                                           QFX, &
                                                            LH, &
                                                        GRDFLX, &
                                                          QSFC,&
                                                          CQS2,&
                                                          CHS,   &
                                                          CHS2,&
                                                          SNOW, &
                                                         SNOWC, &
                                                         SNOWH, & 
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOM, &
                                                        ACSNOW, &
                                                       SNOTIME, &
                                                        SNOPCX, &
                                                        EMISS,  &
                                                          RIB,  &
                                                        POTEVP, &
                                                        ALBEDO, &
                                                           ZNT
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(OUT)      ::                         NOAHRES
   INTEGER, INTENT(IN)       ::                         OPT_THCND


   LOGICAL,                                INTENT(IN)  :: UA_PHYS
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: FLX4_2D,FVB_2D,FBUR_2D,FGSN_2D
   REAL                                                :: FLX4,FVB,FBUR,FGSN

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
               INTENT(OUT)    ::                        CHKLOWQ
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LAI
   REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::        QZ0
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) ::  RC2, XLAI2

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF


      REAL, DIMENSION(1:num_soil_layers) ::  ET

      REAL, DIMENSION(1:num_soil_layers) ::  SMAV

      REAL  ::  BETA, ETP, SSOIL,EC, EDIR, ESNOW, ETT,        &
                FLX1,FLX2,FLX3, DRIP,DEW,FDOWN,RC,PC,RSMIN,XLAI,  &

                RCS,RCT,RCQ,RCSOIL,FFROZP

    LOGICAL,    INTENT(IN   )    ::     myj,frpcpn



      LOGICAL, PARAMETER :: LOCAL=.false.
      LOGICAL :: FRZGRA, SNOWNG

      LOGICAL :: IPRINT




      INTEGER :: I,J, ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER :: NROOT
      INTEGER :: KZ ,K
      INTEGER :: NS




      REAL  :: SHMIN,SHMAX,DQSDT2,LWDN,PRCP,PRCPRAIN,                    &
               Q2SAT,Q2SATI,SFCPRS,SFCSPD,SFCTMP,SHDFAC,SNOALB1,         &
               SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ALBEDOK, ETA, ETA_KINEMATIC, &
               EMBRD,                                                    &
               Z0K,RUNOFF1,RUNOFF2,RUNOFF3,SHEAT,SOLNET,E2SAT,SFCTSNO,   &

               SOLUP,LWUP,RNET,RES,Q1SFC,TAIRV,SATFLG

      REAL ::  FDTLIW

      REAL :: RIBB
      REAL ::  FDTW

      REAL  :: EMISSI

      REAL  :: SNCOVR,SNEQV,SNOWHK,CMC, CHK,TH2

      REAL  :: SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT,SOILM,SOILW,Q1,T1
      REAL  :: SNOTIME1    

      REAL  :: DUMMY,Z0BRD

      REAL  :: COSZ, SOLARDIRECT

      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC

      REAL, DIMENSION(1:num_soil_layers) ::     ZSOIL, RTDIS
      REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
                          T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)

      REAL, PARAMETER  :: ROW=1.E3,ELIW=XLF,ROWLIW=ROW*ELIW






     INTEGER, INTENT(IN) :: num_roof_layers
     INTEGER, INTENT(IN) :: num_wall_layers
     INTEGER, INTENT(IN) :: num_road_layers
     REAL, OPTIONAL, DIMENSION(1:num_roof_layers), INTENT(IN) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers), INTENT(IN) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers), INTENT(IN) :: DZG
     REAL, OPTIONAL, INTENT(IN) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: U_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: V_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: TH_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: P_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: RHO
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UST

     LOGICAL, intent(in) :: rdlai2d
     LOGICAL, intent(in) :: USEMONALB


     INTEGER :: UTYPE_URB 
     REAL :: TA_URB       
     REAL :: QA_URB       
     REAL :: UA_URB       
     REAL :: U1_URB       
     REAL :: V1_URB       
     REAL :: SSG_URB      
     REAL :: LLG_URB      
     REAL :: RAIN_URB     
     REAL :: RHOO_URB     
     REAL :: ZA_URB       
     REAL :: DELT_URB     
     REAL :: SSGD_URB     
     REAL :: SSGQ_URB     
     REAL :: XLAT_URB     
     REAL :: COSZ_URB     
     REAL :: OMG_URB      
     REAL :: ZNT_URB      
     REAL :: TR_URB
     REAL :: TB_URB
     REAL :: TG_URB
     REAL :: TC_URB
     REAL :: QC_URB
     REAL :: UC_URB
     REAL :: XXXR_URB
     REAL :: XXXB_URB
     REAL :: XXXG_URB
     REAL :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  
     LOGICAL  :: LSOLAR_URB


     INTEGER :: jmonth, jday, tloc
     INTEGER :: IRIOPTION, USOIL, DSOIL
     REAL :: AOASIS, OMG
     REAL :: DRELR_URB
     REAL :: DRELB_URB
     REAL :: DRELG_URB
     REAL :: FLXHUMR_URB
     REAL :: FLXHUMB_URB
     REAL :: FLXHUMG_URB
     REAL :: CMCR_URB
     REAL :: TGR_URB
     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB 

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D



     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D


     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D

     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: FRC_URB2D
     INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: UTYPE_URB2D



     REAL :: TS_URB     
     REAL :: QS_URB     
     REAL :: SH_URB     
     REAL :: LH_URB     
     REAL :: LH_KINEMATIC_URB 
     REAL :: SW_URB     
     REAL :: ALB_URB    
     REAL :: LW_URB     
     REAL :: G_URB      
     REAL :: RN_URB     
     REAL :: PSIM_URB   
     REAL :: PSIH_URB   
     REAL :: GZ1OZ0_URB   
     REAL :: U10_URB    
     REAL :: V10_URB    
     REAL :: TH2_URB    
     REAL :: Q2_URB     
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB

     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb

   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN  ) ::                               num_urban_ndm
   INTEGER, INTENT(IN  ) ::                               urban_map_zrd
   INTEGER, INTENT(IN  ) ::                               urban_map_zwd
   INTEGER, INTENT(IN  ) ::                               urban_map_gd
   INTEGER, INTENT(IN  ) ::                               urban_map_zd
   INTEGER, INTENT(IN  ) ::                               urban_map_zdf
   INTEGER, INTENT(IN  ) ::                               urban_map_bd
   INTEGER, INTENT(IN  ) ::                               urban_map_wd
   INTEGER, INTENT(IN  ) ::                               urban_map_gbd
   INTEGER, INTENT(IN  ) ::                               urban_map_fbd
   INTEGER, INTENT(IN  ) ::                               urban_map_zgrd
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_HI
   REAL, OPTIONAL, DIMENSION( ims:ime,                     jms:jme ), INTENT(INOUT) :: tsk_rural_bep
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ep_pv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: t_pv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: trv_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: qr_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: qgr_urb3d  
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: tgr_urb3d  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: drain_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: draingr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: sfrv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfrv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: dgr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: dg_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: lfg_urb3d 
   
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lp_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lb_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: hgt_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: mh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: stdh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(IN) :: lf_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  


   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_RURAL,LH_RURAL,GRDFLX_RURAL 
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_RURAL 
   REAL,    DIMENSION( its:ite, jts:jte ) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL 

   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_URB,UMOM_URB,VMOM_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_URB

   REAL, DIMENSION(its:ite,jts:jte) ::EMISS_URB
   REAL, DIMENSION(its:ite,jts:jte) :: RL_UP_URB
   REAL, DIMENSION(its:ite,jts:jte) ::RS_ABS_URB
   REAL, DIMENSION(its:ite,jts:jte) ::GRDFLX_URB
   REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
   REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
   REAL :: frc_urb,lb_urb
   REAL :: check 




  REAL, PARAMETER  :: CAPA=R_D/CP
  REAL :: APELM,APES,SFCTH2,PSFC
  real, intent(in) :: xice_threshold
  character(len=80) :: message_text



   REAL,    DIMENSION( ims:ime, jms:jme ),  OPTIONAL,                     &
            INTENT(INOUT)  ::   SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM
   INTEGER, INTENT(IN   )  ::  fasdas

   REAL                    ::  XSDA_HFX, XSDA_QFX, XQNORM
   REAL                    ::  HFX_PHY, QFX_PHY
   REAL                    ::  DZQ
   REAL                    ::  HCPCT_FASDAS

   REAL,OPTIONAL,INTENT(IN),DIMENSION( ims:ime, jms:jme )       :: IRR_CHAN
   REAL       ::  IRRIGATION_CHANNEL
   IRRIGATION_CHANNEL =0.0
   HFX_PHY = 0.0   
   QFX_PHY = 0.0
   XQNORM  = 0.0
   XSDA_HFX = 0.0
   XSDA_QFX = 0.0



  FLX4  = 0.0 
  FVB   = 0.0 
  FBUR  = 0.0 
  FGSN  = 0.0 
  SOILW = 0.0 

  sigma_sb=5.67e-08


      FDTLIW=DT/ROWLIW

      FDTW=DT/(XLV*RHOWATER)

         IPRINT=.false.


      SLOPETYP=1



      NSOIL=num_soil_layers

     DO NS=1,NSOIL
     SLDPTH(NS)=DZS(NS)
     ENDDO

     JLOOP : DO J=jts,jte

      IF(ITIMESTEP.EQ.1)THEN
        DO 50 I=its,ite











          IF((XLAND(I,J)-1.5).GE.0.)THEN


            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            DO NS=1,NSOIL
              SMOIS(I,NS,J)=1.0
              TSLB(I,NS,J)=273.16                                          
              SMCREL(I,NS,J)=1.0
            ENDDO
          ELSE
            IF ( XICE(I,J) .GE. XICE_THRESHOLD ) THEN

              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              DO NS=1,NSOIL
                SMOIS(I,NS,J)=1.0
                SMCREL(I,NS,J)=1.0
              ENDDO
            ENDIF
          ENDIF

   50   CONTINUE
      ENDIF                                                               


      ILOOP : DO I=its,ite

        PSFC=P8w3D(i,1,j)

        SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5

         Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))


         Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        


        IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
          SATFLG=0.
          CHKLOWQ(I,J)=0.
        ELSE
          SATFLG=1.0
          CHKLOWQ(I,J)=1.
        ENDIF

        SFCTMP=T3D(i,1,j)
        ZLVL=0.5*DZ8W(i,1,j)



         APES=(1.E5/PSFC)**CAPA
         APELM=(1.E5/SFCPRS)**CAPA
         SFCTH2=SFCTMP*APELM
         TH2=SFCTH2/APES

         EMISSI = EMISS(I,J)
         LWDN=GLW(I,J)*EMISSI

        SOLDN=SWDOWN(I,J)



        SOLNET=SOLDN*(1.-ALBEDO(I,J))
        PRCP=RAINBL(i,j)/DT
        IF(PRESENT(IRR_CHAN)) THEN
         IF(IRR_CHAN(i,j).NE.0) THEN
           IRRIGATION_CHANNEL=IRR_CHAN(i,j)/DT
         ELSE
           IRRIGATION_CHANNEL=0.
         END IF
        ENDIF
        VEGTYP=IVGTYP(I,J)
        SOILTYP=ISLTYP(I,J)
        SHDFAC=VEGFRA(I,J)/100.
        T1=TSK(I,J)
        CHK=CHS(I,J)
        SHMIN=SHDMIN(I,J)/100. 
        SHMAX=SHDMAX(I,J)/100. 

        SNEQV=SNOW(I,J)*0.001

        SNOWHK=SNOWH(I,J)
        SNCOVR=SNOWC(I,J)




       IF(FRPCPN) THEN
          FFROZP=SR(I,J)
        ELSE
          IF (SFCTMP <=  273.15) THEN
            FFROZP = 1.0
	  ELSE
	    FFROZP = 0.0
	  ENDIF
        ENDIF

        IF((XLAND(I,J)-1.5).GE.0.)THEN                                  

          TSK_RURAL(I,J)=TSK(I,J)
          HFX_RURAL(I,J)=HFX(I,J)
          QFX_RURAL(I,J)=QFX(I,J)
          LH_RURAL(I,J)=LH(I,J)
          EMISS_RURAL(I,J)=EMISS(I,J)
          GRDFLX_RURAL(I,J)=GRDFLX(I,J)

        ELSE


          IF (XICE(I,J) >= XICE_THRESHOLD) THEN
             
             ICE = 1
          ELSE IF ( VEGTYP == ISICE ) THEN
             
             ICE = -1
          ELSE
             
             ICE=0
          ENDIF
          DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2

          IF(SNOW(I,J).GT.0.0)THEN

            SFCTSNO=SFCTMP
            E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
            Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
            Q2SATI=Q2SATI/(1.0+Q2SATI)    
            IF (T1 .GT. 273.14) THEN

              Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
              DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
            ELSE

              Q2SAT=Q2SATI
              DQSDT2=Q2SATI*6174./(SFCTSNO**2)
            ENDIF


            IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0. .AND. SWDOWN(I,J) .GT. 10.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
          ENDIF

          
          TBOT=TMN(I,J)

          IF(ISURBAN.EQ.1) THEN

          IF(VEGTYP.EQ.25) SHDFAC=0.0000
          IF(VEGTYP.EQ.26) SHDFAC=0.0000
          IF(VEGTYP.EQ.27) SHDFAC=0.0000
          ENDIF
          IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
            SOILTYP=7
          ENDIF
          SNOALB1 = SNOALB(I,J)

          CMC=CANWAT(I,J)/1000.












        ALBBRD=ALBBCK(I,J)
        Z0BRD=Z0(I,J)
        EMBRD=EMBCK(I,J)
        SNOTIME1 = SNOTIME(I,J)
        RIBB=RIB(I,J)


          DO NS=1,NSOIL
            SMC(NS)=SMOIS(I,NS,J)
            STC(NS)=TSLB(I,NS,J)                                          
            SWC(NS)=SH2O(I,NS,J)
          ENDDO

          if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
            SNOWHK= 5.*SNEQV
          endif




                  IF(SF_URBAN_PHYSICS == 1.OR. SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN
                  IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                      IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                      IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                      IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
	
		 VEGTYP = NATURAL
                 SHDFAC = SHDTBL(NATURAL)
                 ALBEDOK =0.2         
                 ALBBRD  =0.2         
                 EMISSI = 0.98                                 
		 IF ( FRC_URB2D(I,J) < 0.99 ) THEN
                   if(sf_urban_physics.eq.1)then
                     T1= ( TSK(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
                   elseif((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then
                     T1=tsk_rural_bep(i,j)
                   endif
	         ELSE
		 T1 = TSK(I,J)
                 ENDIF
                ENDIF
           ELSE
            IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
                  VEGTYP = ISURBAN
                 ENDIF

           ENDIF


           AOASIS = 1.0
           USOIL = 1
           DSOIL = 2
           IRIOPTION=IRI_SCHEME
	  IF(SF_URBAN_PHYSICS == 1) THEN
           OMG= OMG_URB2D(I,J)   
           tloc=mod(int(OMG/3.14159*180./15.+12.+0.5 ),24)
           if (tloc.lt.0) tloc=tloc+24
           if (tloc==0) tloc=24
           CALL cal_mon_day(julian,julyr,jmonth,jday) 
             IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                 IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                 IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                 IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
                AOASIS = oasis  
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  
                   IF (jmonth==5 .or. jmonth==6 .or. jmonth==7 .or. jmonth==8 .or. jmonth==9) THEN


                      IF (SMC(USOIL) .LT. SMCREF) SMC(USOIL)= REFSMC(ISLTYP(I,J))                      
                      IF (SMC(DSOIL) .LT. SMCREF) SMC(DSOIL)= REFSMC(ISLTYP(I,J))                    
                   ENDIF
                   ENDIF
                   ENDIF
                ENDIF
           ENDIF

	  IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN
          IF(AOASIS > 1.0) THEN
          call wrf_error_fatal3("<stdin>",996,&
'Urban oasis option is for SF_URBAN_PHYSICS == 1 only' )
          ENDIF
          IF(IRIOPTION == 1) THEN
          call wrf_error_fatal3("<stdin>",1000,&
'Urban irrigation option is for SF_URBAN_PHYSICS == 1 only' )
          ENDIF
          ENDIF


          IF (rdlai2d) THEN
             IF (SHDFAC > 0.0 .AND. LAI(I,J) <= 0.0) LAI(I,J) = 0.01
             xlai = lai(i,j)
          endif

    IF ( ICE == 1 ) THEN

       

       DO NS = 1, NSOIL
          SH2O(I,NS,J) = 1.0
       ENDDO
       LAI(I,J) = 0.01

       CYCLE ILOOP

    ELSEIF (ICE == 0) THEN

       



       IF( fasdas == 1 ) THEN

         DZQ = DZ8W(I,1,J)
         XSDA_HFX= SDA_HFX(I,J)*RHO(I,1,J)*CPM(I,J)*DZQ  
         
         XSDA_QFX= SDA_QFX(I,J)*RHO(I,1,J)*DZQ          
         XQNORM = QNORM(I,J)

       ENDIF



       CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    
                 LOCAL,                                            &    
                 LUTYPE, SLTYPE,                                   &    
                 LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    
                 DUMMY,DUMMY, DUMMY,                               &    
                 TH2,Q2SAT,DQSDT2,                                 &    
                 VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    
                 ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    
                 CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    
                 ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    
                 EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    
                 BETA,ETP,SSOIL,                                   &    
                 FLX1,FLX2,FLX3,                                   &    
		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                       &    
                 SNOMLT,SNCOVR,                                    &    
                 RUNOFF1,RUNOFF2,RUNOFF3,                          &    
                 RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    
                 SOILW,SOILM,Q1,SMAV,                              &    
                 RDLAI2D,USEMONALB,                                &
                 SNOTIME1,                                         &
                 RIBB,                                             &
                 SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                 sfcheadrt(i,j),                                   &    
                 INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    &   
                ,IRRIGATION_CHANNEL)


    ELSEIF (ICE == -1) THEN

       
       

       
       SOILM = 0.0 
       XLAI = 0.01 
       RUNOFF2 = 0.0
       RUNOFF3 = 0.0
       DO NS = 1, NSOIL
          SWC(NS) = 1.0
          SMC(NS) = 1.0
          SMAV(NS) = 1.0
       ENDDO



       IF( fasdas == 1 ) THEN

         DZQ = DZ8W(I,1,J)
         XSDA_HFX= SDA_HFX(I,J)*RHO(I,1,J)*CPM(I,J)*DZQ  
         XSDA_QFX= 0.0          
         XQNORM = 0.0

       ENDIF




       CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    
            &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    
            &    TH2,Q2SAT,DQSDT2,                                &    
            &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    
            &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    
            &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    
            &    ESNOW,DEW,                                       &    
            &    ETP,SSOIL,                                       &    
            &    FLX1,FLX2,FLX3,                                  &    
            &    SNOMLT,SNCOVR,                                   &    
            &    RUNOFF1,                                         &    
            &    Q1,                                              &    
            &    SNOTIME1,                                        &
            &    RIBB)

    ENDIF

       lai(i,j) = xlai
       if (present(rc2) .and. present(xlai2)) then
       rc2(I,J) = RC                                              
       xlai2(I,J) = XLAI
       endif



          CANWAT(I,J)=CMC*1000.
          SNOW(I,J)=SNEQV*1000.

          SNOWH(I,J)=SNOWHK                   
          ALBEDO(I,J)=ALBEDOK
          ALB_RURAL(I,J)=ALBEDOK
          ALBBCK(I,J)=ALBBRD
          Z0(I,J)=Z0BRD
          EMISS(I,J) = EMISSI
          EMISS_RURAL(I,J) = EMISSI

          ZNT(I,J)=Z0K




       IF( fasdas == 1 ) THEN
           XSDA_QFX= XSDA_QFX*ELWV*XQNORM

        
        

           T1 = T1 + (XSDA_HFX-XSDA_QFX)*DT/(HCPCT_FASDAS*DZS(1))

       END IF



          TSK(I,J)=T1
          TSK_RURAL(I,J)=T1
          if (present(tsk_rural_bep)) then
             IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN
               TSK_RURAL_BEP(I,J)=T1
             END IF
          endif
          HFX(I,J)=SHEAT
          HFX_RURAL(I,J)=SHEAT

        POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
          QFX(I,J)=ETA_KINEMATIC
          QFX_RURAL(I,J)=ETA_KINEMATIC



          LH(I,J)=ETA
          LH_RURAL(I,J)=ETA
          GRDFLX(I,J)=SSOIL
          GRDFLX_RURAL(I,J)=SSOIL
          SNOWC(I,J)=SNCOVR
          CHS2(I,J)=CQS2(I,J)
          SNOTIME(I,J) = SNOTIME1









           QSFC(I,J)= Q1/(1.0-Q1)

           


          DO 80 NS=1,NSOIL
           SMOIS(I,NS,J)=SMC(NS)
           TSLB(I,NS,J)=STC(NS)                                        
           SH2O(I,NS,J)=SWC(NS)
   80     CONTINUE


        FLX4_2D(I,J)  = FLX4
	FVB_2D(I,J)   = FVB
	FBUR_2D(I,J)  = FBUR
	FGSN_2D(I,J)  = FGSN

     
     
     

     IF ( UA_PHYS ) THEN
         noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
              - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4

     ELSE
         noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
              - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
     ENDIF


        IF (SF_URBAN_PHYSICS == 1 ) THEN                                              





           IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
               IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
               IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
               IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN





            UTYPE_URB = UTYPE_URB2D(I,J) 

            TA_URB    = SFCTMP           
            QA_URB    = Q2K              
            UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
            U1_URB    = U_PHY(I,1,J)
            V1_URB    = V_PHY(I,1,J)
            IF(UA_URB < 1.) UA_URB=1.    
            SSG_URB   = SOLDN            
            SSGD_URB  = 0.8*SOLDN        
            SSGQ_URB  = SSG_URB-SSGD_URB 
            LLG_URB   = GLW(I,J)         
            RAIN_URB  = RAINBL(I,J)      
            RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) 
            ZA_URB    = ZLVL             
            DELT_URB  = DT               
            XLAT_URB  = XLAT_URB2D(I,J)  
            COSZ_URB  = COSZ_URB2D(I,J)  
            OMG_URB   = OMG_URB2D(I,J)   
            ZNT_URB   = ZNT(I,J)

            LSOLAR_URB = .FALSE.

            TR_URB = TR_URB2D(I,J)
            TB_URB = TB_URB2D(I,J)
            TG_URB = TG_URB2D(I,J)
            TC_URB = TC_URB2D(I,J)
            QC_URB = QC_URB2D(I,J)
            UC_URB = UC_URB2D(I,J)

            TGR_URB   = TGR_URB2D(I,J)
            CMCR_URB  = CMCR_URB2D(I,J)
            FLXHUMR_URB = FLXHUMR_URB2D(I,J)
            FLXHUMB_URB = FLXHUMB_URB2D(I,J)
            FLXHUMG_URB = FLXHUMG_URB2D(I,J)
            DRELR_URB = DRELR_URB2D(I,J)
            DRELB_URB = DRELB_URB2D(I,J)
            DRELG_URB = DRELG_URB2D(I,J)

            DO K = 1,num_roof_layers
              TRL_URB(K) = TRL_URB3D(I,K,J)
              SMR_URB(K) = SMR_URB3D(I,K,J)
              TGRL_URB(K)= TGRL_URB3D(I,K,J)
            END DO
            DO K = 1,num_wall_layers
              TBL_URB(K) = TBL_URB3D(I,K,J)
            END DO
            DO K = 1,num_road_layers
              TGL_URB(K) = TGL_URB3D(I,K,J)
            END DO

            XXXR_URB = XXXR_URB2D(I,J)
            XXXB_URB = XXXB_URB2D(I,J)
            XXXG_URB = XXXG_URB2D(I,J)
            XXXC_URB = XXXC_URB2D(I,J)



            if (CHS(I,J) < 1.0E-02) then
               CHS(I,J)  = 1.0E-02
            endif
            if (CHS2(I,J) < 1.0E-02) then
               CHS2(I,J)  = 1.0E-02
            endif
            if (CQS2(I,J) < 1.0E-02) then
               CQS2(I,J)  = 1.0E-02
            endif

            CHS_URB  = CHS(I,J)
            CHS2_URB = CHS2(I,J)
            IF (PRESENT(CMR_SFCDIF)) THEN
               CMR_URB = CMR_SFCDIF(I,J)
               CHR_URB = CHR_SFCDIF(I,J)
               CMGR_URB = CMGR_SFCDIF(I,J)
               CHGR_URB = CHGR_SFCDIF(I,J)
               CMC_URB = CMC_SFCDIF(I,J)
               CHC_URB = CHC_SFCDIF(I,J)
            ENDIF


            mh_urb = mh_urb2d(I,J)
            stdh_urb = stdh_urb2d(I,J)
            lp_urb = lp_urb2d(I,J)
            hgt_urb = hgt_urb2d(I,J)
            lf_urb = 0.0
            DO K = 1,4
              lf_urb(K)=lf_urb2d(I,K,J)
            ENDDO
            frc_urb = frc_urb2d(I,J)
            lb_urb = lb_urb2d(I,J)
            check = 0
            if (I.eq.73.and.J.eq.125)THEN
               check = 1
            end if


            CALL cal_mon_day(julian,julyr,jmonth,jday)
            CALL urban(LSOLAR_URB,                                      & 
                       num_roof_layers,num_wall_layers,num_road_layers, & 
                       DZR,DZB,DZG,                                     & 
                       UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & 
                       SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & 
                       ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & 
                       XLAT_URB,DELT_URB,ZNT_URB,                       & 
                       CHS_URB, CHS2_URB,                               & 
                       TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & 
                       TRL_URB,TBL_URB,TGL_URB,                         & 
                       XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & 
                       TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & 
                       SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & 
                       GZ1OZ0_URB,                                      & 
                       CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                       U10_URB, V10_URB, TH2_URB, Q2_URB,               & 
                       UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & 
                       hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & 
                       TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & 
                       DRELR_URB,DRELB_URB,                             & 
                       DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)                            


            TS_URB2D(I,J) = TS_URB

            ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   
            HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         
            QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                     + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                
            LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            
            GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       
            TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            
            Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            

            QSFC(I,J)= Q1/(1.0-Q1)
            UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      






            TR_URB2D(I,J) = TR_URB
            TB_URB2D(I,J) = TB_URB
            TG_URB2D(I,J) = TG_URB
            TC_URB2D(I,J) = TC_URB
            QC_URB2D(I,J) = QC_URB
            UC_URB2D(I,J) = UC_URB

            TGR_URB2D(I,J) =TGR_URB 
            CMCR_URB2D(I,J)=CMCR_URB
            FLXHUMR_URB2D(I,J)=FLXHUMR_URB 
            FLXHUMB_URB2D(I,J)=FLXHUMB_URB
            FLXHUMG_URB2D(I,J)=FLXHUMG_URB
            DRELR_URB2D(I,J) = DRELR_URB 
            DRELB_URB2D(I,J) = DRELB_URB 
            DRELG_URB2D(I,J) = DRELG_URB

            DO K = 1,num_roof_layers
              TRL_URB3D(I,K,J) = TRL_URB(K)
              SMR_URB3D(I,K,J) = SMR_URB(K)
              TGRL_URB3D(I,K,J)= TGRL_URB(K)
            END DO
            DO K = 1,num_wall_layers
              TBL_URB3D(I,K,J) = TBL_URB(K)
            END DO
            DO K = 1,num_road_layers
              TGL_URB3D(I,K,J) = TGL_URB(K)
            END DO
            XXXR_URB2D(I,J) = XXXR_URB
            XXXB_URB2D(I,J) = XXXB_URB
            XXXG_URB2D(I,J) = XXXG_URB
            XXXC_URB2D(I,J) = XXXC_URB

            SH_URB2D(I,J)    = SH_URB
            LH_URB2D(I,J)    = LH_URB
            G_URB2D(I,J)     = G_URB
            RN_URB2D(I,J)    = RN_URB
            PSIM_URB2D(I,J)  = PSIM_URB
            PSIH_URB2D(I,J)  = PSIH_URB
            GZ1OZ0_URB2D(I,J)= GZ1OZ0_URB
            U10_URB2D(I,J)   = U10_URB
            V10_URB2D(I,J)   = V10_URB
            TH2_URB2D(I,J)   = TH2_URB
            Q2_URB2D(I,J)    = Q2_URB
            UST_URB2D(I,J)   = UST_URB
            AKMS_URB2D(I,J)  = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
            IF (PRESENT(CMR_SFCDIF)) THEN
               CMR_SFCDIF(I,J) = CMR_URB
               CHR_SFCDIF(I,J) = CHR_URB
               CMGR_SFCDIF(I,J) = CMGR_URB
               CHGR_SFCDIF(I,J) = CHGR_URB
               CMC_SFCDIF(I,J) = CMC_URB
               CHC_SFCDIF(I,J) = CHC_URB
            ENDIF
          END IF

         ENDIF                                   





          SMSTAV(I,J)=SOILW
          SMSTOT(I,J)=SOILM*1000.
          DO NS=1,NSOIL
          SMCREL(I,NS,J)=SMAV(NS)
          ENDDO



          SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
          UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0

          IF(FFROZP.GT.0.5)THEN
            ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
          ENDIF
          IF(SNOW(I,J).GT.0.)THEN
            ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.

            SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
          ENDIF

        ENDIF                                                           

      ENDDO ILOOP                                                       
   ENDDO JLOOP                                                          

      IF (SF_URBAN_PHYSICS == 2) THEN


         do j=jts,jte
         do i=its,ite
            EMISS_URB(i,j)=0.
            RL_UP_URB(i,j)=0.
            RS_ABS_URB(i,j)=0.
            GRDFLX_URB(i,j)=0.
            b_q_bep(i,kts:kte,j)=0.
         end do
         end do
       CALL BEP(frc_urb2d,utype_urb2d,itimestep,dz8w,dt,u_phy,v_phy,   &
                th_phy,rho,p_phy,swdown,glw,                           &
                gmt,julday,xlong,xlat,declin_urb,cosz_urb2d,omg_urb2d, &
           num_urban_ndm, urban_map_zrd, urban_map_zwd, urban_map_gd,  &
            urban_map_zd, urban_map_zdf,  urban_map_bd, urban_map_wd,  &
           urban_map_gbd, urban_map_fbd,  num_urban_hi,                &
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,               &
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,             &
                lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,                  &
                a_u_bep,a_v_bep,a_t_bep,                               &
                a_e_bep,b_u_bep,b_v_bep,                               &
                b_t_bep,b_e_bep,b_q_bep,dlg_bep,                       &
                dl_u_bep,sf_bep,vl_bep,                                &
                rl_up_urb,rs_abs_urb,emiss_urb,grdflx_urb,             &
                ids,ide, jds,jde, kds,kde,                             &
                ims,ime, jms,jme, kms,kme,                             &
                its,ite, jts,jte, kts,kte )

       ENDIF

       
       IF (SF_URBAN_PHYSICS == 3) THEN


         do j=jts,jte
         do i=its,ite
            EMISS_URB(i,j)=0.
            RL_UP_URB(i,j)=0.
            RS_ABS_URB(i,j)=0.
            GRDFLX_URB(i,j)=0.
            b_q_bep(i,kts:kte,j)=0.
         end do
         end do
        

       CALL BEP_BEM(frc_urb2d,utype_urb2d,itimestep,dz8w,dt,u_phy,v_phy, &
                th_phy,rho,p_phy,swdown,glw,                           &
                gmt,julday,xlong,xlat,declin_urb,cosz_urb2d,omg_urb2d, &
           num_urban_ndm, urban_map_zrd, urban_map_zwd, urban_map_gd,  &
            urban_map_zd, urban_map_zdf,  urban_map_bd, urban_map_wd,  &
           urban_map_gbd, urban_map_fbd, urban_map_zgrd, num_urban_hi, &
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,               &
                tlev_urb3d,qlev_urb3d,tw1lev_urb3d,tw2lev_urb3d,       &
                tglev_urb3d,tflev_urb3d,sf_ac_urb3d,lf_ac_urb3d,       &
                cm_ac_urb3d,sfvent_urb3d,lfvent_urb3d,                 &
                sfwin1_urb3d,sfwin2_urb3d,                             &
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,             &
                ep_pv_urb3d,t_pv_urb3d,                                & 
                trv_urb4d,qr_urb4d,qgr_urb3d,tgr_urb3d,                & 
                drain_urb4d,draingr_urb3d,sfrv_urb3d,                  & 
                lfrv_urb3d,dgr_urb3d,dg_urb3d,lfr_urb3d,lfg_urb3d,     & 
                rainbl,swddir,swddif,                                  & 
                lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,                  &
                a_u_bep,a_v_bep,a_t_bep,                               &
                a_e_bep,b_u_bep,b_v_bep,                               &
                b_t_bep,b_e_bep,b_q_bep,dlg_bep,                       &
                dl_u_bep,sf_bep,vl_bep,                                &
                rl_up_urb,rs_abs_urb,emiss_urb,grdflx_urb,qv3d,        &
                ids,ide, jds,jde, kds,kde,                             &
                ims,ime, jms,jme, kms,kme,                             &
                its,ite, jts,jte, kts,kte )

       ENDIF

    if((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then         
         do j=jts,jte
         do i=its,ite
            UMOM_URB(I,J)=0.
            VMOM_URB(I,J)=0.
            HFX_URB(I,J)=0.
            QFX_URB(I,J)=0.
         do k=kts,kte
            a_u_bep(i,k,j)=a_u_bep(i,k,j)*frc_urb2d(i,j)
            a_v_bep(i,k,j)=a_v_bep(i,k,j)*frc_urb2d(i,j)
            a_t_bep(i,k,j)=a_t_bep(i,k,j)*frc_urb2d(i,j)
            a_q_bep(i,k,j)=0.        
            a_e_bep(i,k,j)=0.
            b_u_bep(i,k,j)=b_u_bep(i,k,j)*frc_urb2d(i,j)
            b_v_bep(i,k,j)=b_v_bep(i,k,j)*frc_urb2d(i,j)
            b_t_bep(i,k,j)=b_t_bep(i,k,j)*frc_urb2d(i,j)
            b_q_bep(i,k,j)=b_q_bep(i,k,j)*frc_urb2d(i,j)
            b_e_bep(i,k,j)=b_e_bep(i,k,j)*frc_urb2d(i,j)
            HFX_URB(I,J)=HFX_URB(I,J)+B_T_BEP(I,K,J)*RHO(I,K,J)*CP*                &
                          DZ8W(I,K,J)*VL_BEP(I,K,J)
            QFX_URB(I,J)=QFX_URB(I,J)+B_Q_BEP(I,K,J)*               &
                          DZ8W(I,K,J)*VL_BEP(I,K,J)
            UMOM_URB(I,J)=UMOM_URB(I,J)+ (A_U_BEP(I,K,J)*U_PHY(I,K,J)+             &
                          B_U_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
            VMOM_URB(I,J)=VMOM_URB(I,J)+ (A_V_BEP(I,K,J)*V_PHY(I,K,J)+             &
                          B_V_BEP(I,K,J))*DZ8W(I,K,J)*VL_BEP(I,K,J)
            vl_bep(i,k,j)=(1.-frc_urb2d(i,j))+vl_bep(i,k,j)*frc_urb2d(i,j)
            sf_bep(i,k,j)=(1.-frc_urb2d(i,j))+sf_bep(i,k,j)*frc_urb2d(i,j)
         end do
            a_u_bep(i,1,j)=(1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                           ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_u_bep(i,1,j)
            a_v_bep(i,1,j)=(1.-frc_urb2d(i,j))*(-ust(I,J)*ust(I,J))/dz8w(i,1,j)/   &
                           ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+a_v_bep(i,1,j)
            b_t_bep(i,1,j)=(1.-frc_urb2d(i,j))*hfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)/CP+ &
                            b_t_bep(i,1,j)
            b_q_bep(i,1,j)=(1.-frc_urb2d(i,j))*qfx_rural(i,j)/dz8w(i,1,j)/rho(i,1,j)+b_q_bep(i,1,j)
            umom=(1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*u_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+umom_urb(i,j)
            vmom=(1.-frc_urb2d(i,j))*ust(i,j)*ust(i,j)*v_phy(i,1,j)/               &
                         ((u_phy(i,1,j)**2+v_phy(i,1,j)**2.)**.5)+vmom_urb(i,j)
            sf_bep(i,1,j)=1.
           





           IF (FRC_URB2D(I,J).GT.0.) THEN
              rl_up_rural=-emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emissi)*glw(i,j)
              rl_up_tot=(1.-frc_urb2d(i,j))*rl_up_rural+frc_urb2d(i,j)*rl_up_urb(i,j)   
              emiss(i,j)=(1.-frc_urb2d(i,j))*emiss_rural(i,j)+frc_urb2d(i,j)*emiss_urb(i,j)
              ts_urb2d(i,j)=(max(0.,(-rl_up_urb(i,j)-(1.-emiss_urb(i,j))*glw(i,j))/emiss_urb(i,j)/sigma_sb))**0.25
              tsk(i,j)=(max(0., (-1.*rl_up_tot-(1.-emiss(i,j))*glw(i,j) )/emiss(i,j)/sigma_sb))**.25
           rs_abs_tot=(1.-frc_urb2d(i,j))*swdown(i,j)*(1.-albedo(i,j))+frc_urb2d(i,j)*rs_abs_urb(i,j)  
          if(swdown(i,j).gt.0.)then
           albedo(i,j)=1.-rs_abs_tot/swdown(i,j)
          else
           albedo(i,j)=alb_rural(i,j)
          endif

         grdflx(i,j)= (1.-frc_urb2d(i,j))*grdflx_rural(i,j)+frc_urb2d(i,j)*grdflx_urb(i,j) 
         qfx(i,j)=(1.-frc_urb2d(i,j))*qfx_rural(i,j)+qfx_urb(i,j)

         lh(i,j)=qfx(i,j)*xlv
         HFX(I,J) = HFX_URB(I,J)+(1-FRC_URB2D(I,J))*HFX_RURAL(I,J)         
            SH_URB2D(I,J)    = HFX_URB(I,J)/FRC_URB2D(I,J)
            LH_URB2D(I,J)    = qfx_urb(i,j)*xlv
            G_URB2D(I,J)     = grdflx_urb(i,j)
            RN_URB2D(I,J)    = rs_abs_urb(i,j)+emiss_urb(i,j)*glw(i,j)-rl_up_urb(i,j)
            ust(i,j)=(umom**2.+vmom**2.)**.25


















            else
              SH_URB2D(I,J)    = 0.
              LH_URB2D(I,J)    = 0.
              G_URB2D(I,J)     = 0.
              RN_URB2D(I,J)    = 0.
            endif
        enddo
        enddo

       endif                                                           


   END SUBROUTINE lsm







  SUBROUTINE LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,    &
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

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers

   LOGICAL , INTENT(IN) :: restart , allowed_to_read

   REAL,    DIMENSION( num_soil_layers), INTENT(INOUT) :: ZS, DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(INOUT)    ::                          SMOIS, &  
                                                         SH2O,  &  
                                                         TSLB      

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        SNOALB, &
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOW, &
                                                        VEGFRA, &
                                                        ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN)       ::                         IVGTYP, &
                                                        ISLTYP
   CHARACTER(LEN=*),  INTENT(IN)      ::                MMINLU

   LOGICAL, INTENT(IN)       ::                      FNDSOILW , &
                                                     FNDSNOWH
   LOGICAL, INTENT(IN)       ::                      RDMAXALB


   INTEGER                   :: L
   REAL                      :: BX, SMCMAX, PSISAT, FREE
   REAL, PARAMETER           :: BLIM = 5.5, HLICE = 3.335E5,    &
                                GRAV = 9.81, T0 = 273.15
   INTEGER                   :: errflag
   CHARACTER(LEN=80)         :: err_message
   INTEGER,DIMENSION(ims:ime, jms:jme ),INTENT(INOUT):: irr_rand_field   
   INTEGER , DIMENSION(jds:jde) :: my_seeds  
   INTEGER :: irr_ph,irr_freq         
   REAL,DIMENSION(ims:ime, jms:jme ) :: rand_tmp
   character*256 :: MMINSL
        MMINSL='STAS'



   IF ( allowed_to_read ) THEN
     CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
     CALL  SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
   ENDIF














   IF(.not.restart)THEN
   
   IF (irr_ph.NE.0)THEN
   DO i = its,ite
    DO j=jts,jte
      my_seeds(j) =sqrt(ide*(real(j-1)**2))+sqrt(real(jde*i))

    END DO
      CALL RANDOM_SEED ( PUT = my_seeds )
      CALL RANDOM_NUMBER ( rand_tmp(i,:) )
      CALL RANDOM_SEED ( GET = my_seeds )
      CALL RANDOM_NUMBER ( rand_tmp(i,:) )
      irr_rand_field(i,:)=int(modulo(rand_tmp(i,:)*100,real(irr_freq)))
   END DO
   END IF

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   errflag = 0
   DO j = jts,jtf
     DO i = its,itf
       IF ( ISLTYP( i,j ) .LT. 1 ) THEN
         errflag = 1
         WRITE(err_message,*)"module_sf_noahdrv.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
         CALL wrf_message(err_message)
       ENDIF
       IF(.not.RDMAXALB) THEN
          SNOALB(i,j)=MAXALB(IVGTYP(i,j))*0.01
       ENDIF
     ENDDO
   ENDDO
   IF ( errflag .EQ. 1 ) THEN
      CALL wrf_error_fatal3("<stdin>",1756,&
"module_sf_noahdrv.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
   ENDIF







        DO J = jts,jtf
        DO I = its,itf
          BX = BB(ISLTYP(I,J))
          SMCMAX = MAXSMC(ISLTYP(I,J))
          PSISAT = SATPSI(ISLTYP(I,J))
         if ((bx > 0.0).and.(smcmax > 0.0).and.(psisat > 0.0)) then
          DO NS=1, num_soil_layers


             IF (TSLB(I,NS,J) < 273.149) THEN




              BX = BB(ISLTYP(I,J))
              SMCMAX = MAXSMC(ISLTYP(I,J))
              PSISAT = SATPSI(ISLTYP(I,J))
              IF ( BX >  BLIM ) BX = BLIM
              FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                 ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
              IF (FK < 0.02) FK = 0.02
              SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )




              CALL FRH2O (FREE,TSLB(I,NS,J),SMOIS(I,NS,J),SH2O(I,NS,J),    &
                 SMCMAX,BX,PSISAT)
              SH2O(I,NS,J) = FREE
             ELSE             


              SH2O(I,NS,J)=SMOIS(I,NS,J)

             ENDIF            
          END DO              
         else                 
          DO NS=1, num_soil_layers
           SH2O(I,NS,J)=SMOIS(I,NS,J)
          END DO
         endif                
        ENDDO                 
        ENDDO                 




        IF(.NOT.FNDSNOWH)THEN

          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
          DO I = its,itf
            SNOWH(I,J)=SNOW(I,J)*0.005               
          ENDDO
          ENDDO
        ENDIF





          DO J = jts,jtf
          DO I = its,itf
            CANWAT(I,J)=0.0
          ENDDO
          ENDDO
 110      CONTINUE

   ENDIF

  END SUBROUTINE lsminit





        SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)


        USE module_wrf_error
        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(IN) :: MMINLU, MMINSL
        integer :: LUMATCH, IINDEX, LC, NUM_SLOPE
        integer :: ierr
        INTEGER , PARAMETER :: OPEN_OK = 0

        character*128 :: mess , message
        character*256 :: a_string
        logical, external :: wrf_dm_on_monitor
        integer , parameter :: loop_max   = 10
        integer             :: loop_count


























       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",1891,&
message )
        END IF


        LUMATCH=0

        loop_count = 0
        READ (19,FMT='(A)',END=2002) a_string
        FIND_LUTYPE : DO WHILE (LUMATCH == 0)
           READ (19,*,END=2002)LUTYPE
           READ (19,*)LUCATS,IINDEX

           IF(LUTYPE.EQ.MMINLU)THEN
              WRITE( mess , * ) 'LANDUSE TYPE = ' // TRIM ( LUTYPE ) // ' FOUND', LUCATS,' CATEGORIES'
              CALL wrf_message( mess )
              LUMATCH=1
           ELSE
              loop_count = loop_count+1
              call wrf_message ( "Skipping over LUTYPE = " // TRIM ( LUTYPE ) )
              FIND_VEGETATION_PARAMETER_FLAG : DO
                 READ (19,FMT='(A)', END=2002) a_string
                 IF ( a_string(1:21) .EQ. 'Vegetation Parameters' ) THEN
                    EXIT FIND_VEGETATION_PARAMETER_FLAG
                 ELSE IF ( loop_count .GE. loop_max ) THEN
                    CALL wrf_error_fatal3("<stdin>",1916,&
'Too many loops in VEGPARM.TBL')
                 ENDIF
              ENDDO FIND_VEGETATION_PARAMETER_FLAG
           ENDIF
        ENDDO FIND_LUTYPE

        IF ( SIZE(SHDTBL)       < LUCATS .OR. &
             SIZE(NROTBL)       < LUCATS .OR. &
             SIZE(RSTBL)        < LUCATS .OR. &
             SIZE(RGLTBL)       < LUCATS .OR. &
             SIZE(HSTBL)        < LUCATS .OR. &
             SIZE(SNUPTBL)      < LUCATS .OR. &
             SIZE(MAXALB)       < LUCATS .OR. &
             SIZE(LAIMINTBL)    < LUCATS .OR. &
             SIZE(LAIMAXTBL)    < LUCATS .OR. &
             SIZE(Z0MINTBL)     < LUCATS .OR. &
             SIZE(Z0MAXTBL)     < LUCATS .OR. &
             SIZE(ALBEDOMINTBL) < LUCATS .OR. &
             SIZE(ALBEDOMAXTBL) < LUCATS .OR. &
             SIZE(ZTOPVTBL) < LUCATS .OR. &
             SIZE(ZBOTVTBL) < LUCATS .OR. &
             SIZE(EMISSMINTBL ) < LUCATS .OR. &
             SIZE(EMISSMAXTBL ) < LUCATS ) THEN
           CALL wrf_error_fatal3("<stdin>",1940,&
'Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
        ENDIF

        IF(LUTYPE.EQ.MMINLU)THEN
          DO LC=1,LUCATS
              READ (19,*)IINDEX,SHDTBL(LC),                        &
                        NROTBL(LC),RSTBL(LC),RGLTBL(LC),HSTBL(LC), &
                        SNUPTBL(LC),MAXALB(LC), LAIMINTBL(LC),     &
                        LAIMAXTBL(LC),EMISSMINTBL(LC),             &
                        EMISSMAXTBL(LC), ALBEDOMINTBL(LC),         &
                        ALBEDOMAXTBL(LC), Z0MINTBL(LC), Z0MAXTBL(LC),&
			ZTOPVTBL(LC), ZBOTVTBL(LC)
          ENDDO

          READ (19,*)
          READ (19,*)TOPT_DATA
          READ (19,*)
          READ (19,*)CMCMAX_DATA
          READ (19,*)
          READ (19,*)CFACTR_DATA
          READ (19,*)
          READ (19,*)RSMAX_DATA
          READ (19,*)
          READ (19,*)BARE
          READ (19,*)
          READ (19,*)NATURAL
          READ (19,*)
          READ (19,*)
          READ (19,FMT='(A)') a_string
          IF ( a_string(1:21) .EQ. 'Vegetation Parameters' ) THEN
             CALL wrf_message     ("Expected low and high density residential, and high density industrial information in VEGPARM.TBL")
             CALL wrf_error_fatal3("<stdin>",1972,&
"This could be caused by using an older VEGPARM.TBL file with a newer WRF source code.")
        ENDIF
         READ (19,*)LCZ_1
          READ (19,*)
          READ (19,*)LCZ_2
          READ (19,*)
          READ (19,*)LCZ_3
          READ (19,*)
          READ (19,*)LCZ_4
          READ (19,*)
          READ (19,*)LCZ_5
          READ (19,*)
          READ (19,*)LCZ_6
          READ (19,*)
          READ (19,*)LCZ_7
          READ (19,*)
          READ (19,*)LCZ_8
          READ (19,*)
          READ (19,*)LCZ_9
          READ (19,*)
          READ (19,*)LCZ_10
          READ (19,*)
          READ (19,*)LCZ_11
        ENDIF
 2002   CONTINUE

        CLOSE (19)
        IF (LUMATCH == 0) then
           CALL wrf_error_fatal3("<stdin>",2001,&
"Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
        ENDIF
      ENDIF

      CALL wrf_dm_bcast_string  ( LUTYPE  , 4 )
      CALL wrf_dm_bcast_integer ( LUCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_real    ( SHDTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( NROTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( RSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( RGLTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( HSTBL   , NLUS )
      CALL wrf_dm_bcast_real    ( SNUPTBL , NLUS )
      CALL wrf_dm_bcast_real    ( LAIMINTBL    , NLUS )
      CALL wrf_dm_bcast_real    ( LAIMAXTBL    , NLUS )
      CALL wrf_dm_bcast_real    ( Z0MINTBL     , NLUS )
      CALL wrf_dm_bcast_real    ( Z0MAXTBL     , NLUS )
      CALL wrf_dm_bcast_real    ( EMISSMINTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( EMISSMAXTBL  , NLUS )
      CALL wrf_dm_bcast_real    ( ALBEDOMINTBL , NLUS )
      CALL wrf_dm_bcast_real    ( ALBEDOMAXTBL , NLUS )
      CALL wrf_dm_bcast_real    ( ZTOPVTBL , NLUS )
      CALL wrf_dm_bcast_real    ( ZBOTVTBL , NLUS )
      CALL wrf_dm_bcast_real    ( MAXALB  , NLUS )
      CALL wrf_dm_bcast_real    ( TOPT_DATA    , 1 )
      CALL wrf_dm_bcast_real    ( CMCMAX_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( CFACTR_DATA  , 1 )
      CALL wrf_dm_bcast_real    ( RSMAX_DATA  , 1 )
      CALL wrf_dm_bcast_integer ( BARE    , 1 )
      CALL wrf_dm_bcast_integer ( NATURAL , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_1 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_2 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_3 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_4 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_5 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_6 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_7 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_8 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_9 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_10 , 1 )
      CALL wrf_dm_bcast_integer ( LCZ_11 , 1 )




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",2053,&
message )
        END IF

        WRITE(mess,*) 'INPUT SOIL TEXTURE CLASSIFICATION = ', TRIM ( MMINSL )
        CALL wrf_message( mess )

        LUMATCH=0

        READ (19,*)
        READ (19,2000,END=2003)SLTYPE
 2000   FORMAT (A4)
        READ (19,*)SLCATS,IINDEX
        IF(SLTYPE.EQ.MMINSL)THEN
            WRITE( mess , * ) 'SOIL TEXTURE CLASSIFICATION = ', TRIM ( SLTYPE ) , ' FOUND', &
                  SLCATS,' CATEGORIES'
            CALL wrf_message ( mess )
          LUMATCH=1
        ENDIF

        IF ( SIZE(BB    ) < SLCATS .OR. &
             SIZE(DRYSMC) < SLCATS .OR. &
             SIZE(F11   ) < SLCATS .OR. &
             SIZE(MAXSMC) < SLCATS .OR. &
             SIZE(REFSMC) < SLCATS .OR. &
             SIZE(SATPSI) < SLCATS .OR. &
             SIZE(SATDK ) < SLCATS .OR. &
             SIZE(SATDW ) < SLCATS .OR. &
             SIZE(WLTSMC) < SLCATS .OR. &
             SIZE(QTZ   ) < SLCATS  ) THEN
           CALL wrf_error_fatal3("<stdin>",2083,&
'Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
        ENDIF
        IF(SLTYPE.EQ.MMINSL)THEN
          DO LC=1,SLCATS
              READ (19,*) IINDEX,BB(LC),DRYSMC(LC),F11(LC),MAXSMC(LC),&
                        REFSMC(LC),SATPSI(LC),SATDK(LC), SATDW(LC),   &
                        WLTSMC(LC), QTZ(LC)
          ENDDO
        ENDIF

 2003   CONTINUE

        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( LUMATCH , 1 )
      CALL wrf_dm_bcast_string  ( SLTYPE  , 4 )
      CALL wrf_dm_bcast_string  ( MMINSL  , 4 )  
      CALL wrf_dm_bcast_integer ( SLCATS  , 1 )
      CALL wrf_dm_bcast_integer ( IINDEX  , 1 )
      CALL wrf_dm_bcast_real    ( BB      , NSLTYPE )
      CALL wrf_dm_bcast_real    ( DRYSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( F11     , NSLTYPE )
      CALL wrf_dm_bcast_real    ( MAXSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( REFSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATPSI  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDK   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( SATDW   , NSLTYPE )
      CALL wrf_dm_bcast_real    ( WLTSMC  , NSLTYPE )
      CALL wrf_dm_bcast_real    ( QTZ     , NSLTYPE )

      IF(LUMATCH.EQ.0)THEN
          CALL wrf_message( 'SOIl TEXTURE IN INPUT FILE DOES NOT ' )
          CALL wrf_message( 'MATCH SOILPARM TABLE'                 )
          CALL wrf_error_fatal3("<stdin>",2118,&
'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF




      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal3("<stdin>",2130,&
message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE

          IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
            CALL wrf_error_fatal3("<stdin>",2141,&
'NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
          ENDIF

          DO LC=1,SLPCATS
              READ (19,*)SLOPE_DATA(LC)
          ENDDO

          READ (19,*)
          READ (19,*)SBETA_DATA
          READ (19,*)
          READ (19,*)FXEXP_DATA
          READ (19,*)
          READ (19,*)CSOIL_DATA
          READ (19,*)
          READ (19,*)SALP_DATA
          READ (19,*)
          READ (19,*)REFDK_DATA
          READ (19,*)
          READ (19,*)REFKDT_DATA
          READ (19,*)
          READ (19,*)FRZK_DATA
          READ (19,*)
          READ (19,*)ZBOT_DATA
          READ (19,*)
          READ (19,*)CZIL_DATA
          READ (19,*)
          READ (19,*)SMLOW_DATA
          READ (19,*)
          READ (19,*)SMHIGH_DATA
          READ (19,*)
          READ (19,*)LVCOEF_DATA
        CLOSE (19)
      ENDIF

      CALL wrf_dm_bcast_integer ( NUM_SLOPE    ,  1 )
      CALL wrf_dm_bcast_integer ( SLPCATS      ,  1 )
      CALL wrf_dm_bcast_real    ( SLOPE_DATA   ,  NSLOPE )
      CALL wrf_dm_bcast_real    ( SBETA_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( FXEXP_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( CSOIL_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SALP_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( REFDK_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( REFKDT_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( FRZK_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( ZBOT_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( CZIL_DATA    ,  1 )
      CALL wrf_dm_bcast_real    ( SMLOW_DATA   ,  1 )
      CALL wrf_dm_bcast_real    ( SMHIGH_DATA  ,  1 )
      CALL wrf_dm_bcast_real    ( LVCOEF_DATA  ,  1 )



      END SUBROUTINE SOIL_VEG_GEN_PARM








SUBROUTINE lsm_mosaic(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
                  HFX,QFX,LH,GRDFLX, QGH,GSW,SWDOWN,GLW,SMSTAV,SMSTOT, &
                  SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,ISURBAN,ISICE,VEGFRA,    &
                  ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE,EMISS,EMBCK,   &
                  SNOWC,QSFC,RAINBL,MMINLU,                     &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,                       &
                  CHS,CHS2,CQS2,CPM,ROVCP,SR,chklowq,lai,qz0,   & 
                  myj,frpcpn,                                   &
                  SH2O,SNOWH,                                   & 
                  U_PHY,V_PHY,                                  & 
                  SNOALB,SHDMIN,SHDMAX,                         & 
                  SNOTIME,                                      & 
                  ACSNOM,ACSNOW,                                & 
                  SNOPCX,                                       & 
                  POTEVP,                                       & 
                  SMCREL,                                       & 
                  XICE_THRESHOLD,                               &
                  RDLAI2D,USEMONALB,                            &
                  RIB,                                          & 
                  NOAHRES,OPT_THCND,                            &
                 NLCAT,landusef,landusef2,                       & 
                 sf_surface_mosaic,mosaic_cat,mosaic_cat_index,  & 
                 TSK_mosaic,QSFC_mosaic,                         & 
                 TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic,           & 
                 CANWAT_mosaic,SNOW_mosaic,                      & 
                 SNOWH_mosaic,SNOWC_mosaic,                      & 
                 ALBEDO_mosaic,ALBBCK_mosaic,                    & 
                 EMISS_mosaic, EMBCK_mosaic,                     & 
                 ZNT_mosaic, Z0_mosaic,                          & 
                 HFX_mosaic,QFX_mosaic,                          & 
                 LH_mosaic, GRDFLX_mosaic, SNOTIME_mosaic,       & 
                 RC_mosaic, LAI_mosaic,                          &

                  ua_phys,flx4_2d,fvb_2d,fbur_2d,fgsn_2d,       &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  sf_urban_physics,                             &
                  CMR_SFCDIF,CHR_SFCDIF,CMC_SFCDIF,CHC_SFCDIF,  &
                  CMGR_SFCDIF,CHGR_SFCDIF,                      &

                  TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D, & 
                  UC_URB2D,                                     & 
                  XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,  & 
                  TRL_URB3D,TBL_URB3D,TGL_URB3D,                & 
                  SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,TS_URB2D,  & 
                  TR_URB2D_mosaic,TB_URB2D_mosaic,              & 
                  TG_URB2D_mosaic,TC_URB2D_mosaic,              & 
                  QC_URB2D_mosaic,UC_URB2D_mosaic,              & 
                  TRL_URB3D_mosaic,TBL_URB3D_mosaic,            & 
                  TGL_URB3D_mosaic,                             & 
                  SH_URB2D_mosaic,LH_URB2D_mosaic,              & 
                  G_URB2D_mosaic,RN_URB2D_mosaic,               & 
                  TS_URB2D_mosaic,                              & 
                  TS_RUL2D_mosaic,                              & 
                  PSIM_URB2D,PSIH_URB2D,U10_URB2D,V10_URB2D,    & 
                  GZ1OZ0_URB2D,  AKMS_URB2D,                    & 
                  TH2_URB2D,Q2_URB2D, UST_URB2D,                & 
                  DECLIN_URB,COSZ_URB2D,OMG_URB2D,              & 
                  XLAT_URB2D,                                   & 
                  num_roof_layers, num_wall_layers,             & 
                  num_road_layers, DZR, DZB, DZG,               & 
                  CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,    & 
                  julian,julyr,                                 & 
                  DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,          & 
                  FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,    & 
                  FRC_URB2D,UTYPE_URB2D,                        & 
                  num_urban_ndm,                                & 
                  urban_map_zrd,                                & 
                  urban_map_zwd,                                & 
                  urban_map_gd,                                 & 
                  urban_map_zd,                                 & 
                  urban_map_zdf,                                & 
                  urban_map_bd,                                 & 
                  urban_map_wd,                                 & 
                  urban_map_gbd,                                & 
                  urban_map_fbd,                                & 
                  urban_map_zgrd,                               & 
                  num_urban_hi,                                 & 
                  tsk_rural_bep,                                & 
                  trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,      & 
                  tlev_urb3d,qlev_urb3d,                        & 
                  tw1lev_urb3d,tw2lev_urb3d,                    & 
                  tglev_urb3d,tflev_urb3d,                      & 
                  sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,          & 
                  sfvent_urb3d,lfvent_urb3d,                    & 
                  sfwin1_urb3d,sfwin2_urb3d,                    & 
                  sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,    & 
                  ep_pv_urb3d,t_pv_urb3d,                         & 
                  trv_urb4d,qr_urb4d,qgr_urb3d,tgr_urb3d,         & 
                  drain_urb4d,draingr_urb3d,sfrv_urb3d,           & 
                  lfrv_urb3d,dgr_urb3d,dg_urb3d,lfr_urb3d,lfg_urb3d,& 
                  lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,         & 
                  mh_urb2d,stdh_urb2d,lf_urb2d,                 & 
                  th_phy,rho,p_phy,ust,                         & 
                  gmt,julday,xlong,xlat,                        & 
                  a_u_bep,a_v_bep,a_t_bep,a_q_bep,              & 
                  a_e_bep,b_u_bep,b_v_bep,                      & 
                  b_t_bep,b_q_bep,b_e_bep,dlg_bep,              & 
                  dl_u_bep,sf_bep,vl_bep                        & 
                 ,sfcheadrt,INFXSRT, soldrain                   & 
                 ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas     &   
                 ,RC2,XLAI2                                     & 
                 ,IRR_CHAN                                      &
                  )


    IMPLICIT NONE






































































































































   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER,  INTENT(IN   )   ::  sf_urban_physics               
   INTEGER,  INTENT(IN   )   ::  isurban
   INTEGER,  INTENT(IN   )   ::  isice
   INTEGER,  INTENT(IN   )   ::  julian,julyr


    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
             INTENT(INOUT)  :: sfcheadrt,INFXSRT,soldrain 
    real :: etpnd1


   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        SHDMIN, &
                                                        SHDMAX, &
                                                        SNOALB, &
                                                           GSW, &
                                                        SWDOWN, & 
                                                           GLW, &
                                                        RAINBL, &
                                                        SR
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         ALBBCK, &
                                                            Z0, &
                                                         EMBCK                  
                                                         
   CHARACTER(LEN=*), INTENT(IN   )    ::                 MMINLU

   REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
            INTENT(IN   )    ::                           QV3D, &
                                                         p8w3D, &
                                                          DZ8W, &
                                                          T3D
   REAL,     DIMENSION( ims:ime, jms:jme )                    , &
             INTENT(IN   )               ::               QGH,  &
                                                          CPM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                          ISLTYP
                                                        
   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT   )    ::                         IVGTYP                   

   INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP

   REAL,     INTENT(IN   )   ::     DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS



   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & 
                                                         SH2O,  & 
                                                         TSLB     

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(OUT)     ::                         SMCREL

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & 
                                                           HFX, &
                                                           QFX, &
                                                            LH, &
                                                        GRDFLX, &
                                                          QSFC,&
                                                          CQS2,&
                                                          CHS,   &
                                                          CHS2,&
                                                          SNOW, &
                                                         SNOWC, &
                                                         SNOWH, & 
                                                        CANWAT, &
                                                        SMSTAV, &
                                                        SMSTOT, &
                                                     SFCRUNOFF, &
                                                      UDRUNOFF, &
                                                        ACSNOM, &
                                                        ACSNOW, &
                                                       SNOTIME, &
                                                        SNOPCX, &
                                                        EMISS,  &
                                                          RIB,  &
                                                        POTEVP, &
                                                        ALBEDO, &
                                                           ZNT
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(OUT)      ::                         NOAHRES
   INTEGER,                                INTENT(IN)  :: OPT_THCND


   LOGICAL,                                INTENT(IN)  :: UA_PHYS
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: FLX4_2D,FVB_2D,FBUR_2D,FGSN_2D
   REAL                                                :: FLX4,FVB,FBUR,FGSN

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
               INTENT(OUT)    ::                        CHKLOWQ
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LAI
   REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::        QZ0
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT)   :: RC2, XLAI2

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF


      REAL, DIMENSION(1:num_soil_layers) ::  ET

      REAL, DIMENSION(1:num_soil_layers) ::  SMAV

      REAL  ::  BETA, ETP, SSOIL,EC, EDIR, ESNOW, ETT,        &
                FLX1,FLX2,FLX3, DRIP,DEW,FDOWN,RC,PC,RSMIN,XLAI,  &

                RCS,RCT,RCQ,RCSOIL,FFROZP

    LOGICAL,    INTENT(IN   )    ::     myj,frpcpn



      LOGICAL, PARAMETER :: LOCAL=.false.
      LOGICAL :: FRZGRA, SNOWNG

      LOGICAL :: IPRINT




      INTEGER :: I,J, ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER :: NROOT
      INTEGER :: KZ ,K
      INTEGER :: NS




      REAL  :: SHMIN,SHMAX,DQSDT2,LWDN,PRCP,PRCPRAIN,                    &
               Q2SAT,Q2SATI,SFCPRS,SFCSPD,SFCTMP,SHDFAC,SNOALB1,         &
               SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ALBEDOK, ETA, ETA_KINEMATIC, &
               EMBRD,                                                    &
               Z0K,RUNOFF1,RUNOFF2,RUNOFF3,SHEAT,SOLNET,E2SAT,SFCTSNO,   &

               SOLUP,LWUP,RNET,RES,Q1SFC,TAIRV,SATFLG

      REAL ::  FDTLIW

      REAL :: RIBB
      REAL ::  FDTW

      REAL  :: EMISSI

      REAL  :: SNCOVR,SNEQV,SNOWHK,CMC, CHK,TH2

      REAL  :: SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT,SOILM,SOILW,Q1,T1
      REAL  :: SNOTIME1    

      REAL  :: DUMMY,Z0BRD

      REAL  :: COSZ, SOLARDIRECT

      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC

      REAL, DIMENSION(1:num_soil_layers) ::     ZSOIL, RTDIS
      REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
                          T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)

      REAL, PARAMETER  :: ROW=1.E3,ELIW=XLF,ROWLIW=ROW*ELIW






     INTEGER, INTENT(IN) :: num_roof_layers
     INTEGER, INTENT(IN) :: num_wall_layers
     INTEGER, INTENT(IN) :: num_road_layers
     REAL, OPTIONAL, DIMENSION(1:num_roof_layers), INTENT(IN) :: DZR
     REAL, OPTIONAL, DIMENSION(1:num_wall_layers), INTENT(IN) :: DZB
     REAL, OPTIONAL, DIMENSION(1:num_road_layers), INTENT(IN) :: DZG
     REAL, OPTIONAL, INTENT(IN) :: DECLIN_URB
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: OMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: U_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: V_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: TH_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: P_PHY
     REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: RHO
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UST

     LOGICAL, intent(in) :: rdlai2d
     LOGICAL, intent(in) :: USEMONALB


     INTEGER :: UTYPE_URB 
     REAL :: TA_URB       
     REAL :: QA_URB       
     REAL :: UA_URB       
     REAL :: U1_URB       
     REAL :: V1_URB       
     REAL :: SSG_URB      
     REAL :: LLG_URB      
     REAL :: RAIN_URB     
     REAL :: RHOO_URB     
     REAL :: ZA_URB       
     REAL :: DELT_URB     
     REAL :: SSGD_URB     
     REAL :: SSGQ_URB     
     REAL :: XLAT_URB     
     REAL :: COSZ_URB     
     REAL :: OMG_URB      
     REAL :: ZNT_URB      
     REAL :: TR_URB
     REAL :: TB_URB
     REAL :: TG_URB
     REAL :: TC_URB
     REAL :: QC_URB
     REAL :: UC_URB
     REAL :: XXXR_URB
     REAL :: XXXB_URB
     REAL :: XXXG_URB
     REAL :: XXXC_URB
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  
     LOGICAL  :: LSOLAR_URB


     INTEGER :: jmonth, jday, tloc
     INTEGER :: IRIOPTION, USOIL, DSOIL
     REAL :: AOASIS, OMG
     REAL :: DRELR_URB
     REAL :: DRELB_URB
     REAL :: DRELG_URB
     REAL :: FLXHUMR_URB
     REAL :: FLXHUMB_URB
     REAL :: FLXHUMG_URB
     REAL :: CMCR_URB
     REAL :: TGR_URB
     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB 

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: DRELG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FLXHUMG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMCR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TGR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TGRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: SMR_URB3D


     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: QC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXR_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXB_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXG_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: XXXC_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D


     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D

     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D                
     INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: UTYPE_URB2D


     REAL :: TS_URB     
     REAL :: QS_URB     
     REAL :: SH_URB     
     REAL :: LH_URB     
     REAL :: LH_KINEMATIC_URB 
     REAL :: SW_URB     
     REAL :: ALB_URB    
     REAL :: LW_URB     
     REAL :: G_URB      
     REAL :: RN_URB     
     REAL :: PSIM_URB   
     REAL :: PSIH_URB   
     REAL :: GZ1OZ0_URB   
     REAL :: U10_URB    
     REAL :: V10_URB    
     REAL :: TH2_URB    
     REAL :: Q2_URB     
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB

     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb

   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN  ) ::                               num_urban_ndm
   INTEGER, INTENT(IN  ) ::                               urban_map_zrd
   INTEGER, INTENT(IN  ) ::                               urban_map_zwd
   INTEGER, INTENT(IN  ) ::                               urban_map_gd
   INTEGER, INTENT(IN  ) ::                               urban_map_zd
   INTEGER, INTENT(IN  ) ::                               urban_map_zdf
   INTEGER, INTENT(IN  ) ::                               urban_map_bd
   INTEGER, INTENT(IN  ) ::                               urban_map_wd
   INTEGER, INTENT(IN  ) ::                               urban_map_gbd
   INTEGER, INTENT(IN  ) ::                               urban_map_fbd
   INTEGER, INTENT(IN  ) ::                               urban_map_zgrd
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_HI
   REAL, OPTIONAL, DIMENSION( ims:ime,                     jms:jme ), INTENT(INOUT) :: tsk_rural_bep
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zrd, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zwd, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gd , jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_bd , jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_gbd, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_fbd, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_wd , jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zd , jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ep_pv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ), INTENT(INOUT) :: t_pv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: trv_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zgrd, jms:jme ),INTENT(INOUT) :: qr_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: qgr_urb3d  
   REAL, OPTIONAL, DIMENSION( ims:ime,jms:jme ), INTENT(INOUT) :: tgr_urb3d  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: drain_urb4d 
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: draingr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: sfrv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfrv_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: dgr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: dg_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:urban_map_zdf, jms:jme ),INTENT(INOUT) :: lfr_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_ndm, jms:jme ),INTENT(INOUT) :: lfg_urb3d 
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lp_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lb_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: hgt_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: mh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: stdh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(IN) :: lf_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  


   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_RURAL,LH_RURAL,GRDFLX_RURAL 
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_RURAL 
   REAL,    DIMENSION( its:ite, jts:jte ) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL 

   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_URB,UMOM_URB,VMOM_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_URB

   REAL, DIMENSION(its:ite,jts:jte) ::EMISS_URB
   REAL, DIMENSION(its:ite,jts:jte) :: RL_UP_URB
   REAL, DIMENSION(its:ite,jts:jte) ::RS_ABS_URB
   REAL, DIMENSION(its:ite,jts:jte) ::GRDFLX_URB
   REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
   REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
   REAL :: frc_urb,lb_urb
   REAL :: check 






  
  INTEGER, INTENT(IN) :: sf_surface_mosaic    
  INTEGER, INTENT(IN) :: mosaic_cat, NLCAT
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(IN) :: landusef 
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT) ::landusef2 
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT) :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        TSK_mosaic, QSFC_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic 
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic,   &
        HFX_mosaic,QFX_mosaic, LH_mosaic, GRDFLX_mosaic,SNOTIME_mosaic    
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT)::   &
        TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT):: LAI_mosaic, RC_mosaic

  REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_mosaic_avg, QSFC_mosaic_avg, CANWAT_mosaic_avg,SNOW_mosaic_avg,SNOWH_mosaic_avg, &
                                         SNOWC_mosaic_avg, HFX_mosaic_avg, QFX_mosaic_avg, LH_mosaic_avg, GRDFLX_mosaic_avg,  &
                                         ALBEDO_mosaic_avg, ALBBCK_mosaic_avg, EMISS_mosaic_avg, EMBCK_mosaic_avg,            &
                                         ZNT_mosaic_avg, Z0_mosaic_avg, LAI_mosaic_avg, RC_mosaic_avg, SNOTIME_mosaic_avg,    &
                                         FAREA_mosaic_avg
  REAL, DIMENSION( ims:ime, 1:num_soil_layers, jms:jme )                     ::   &
        TSLB_mosaic_avg,SMOIS_mosaic_avg,SH2O_mosaic_avg
  
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic, UC_URB2D_mosaic, &
        SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                  
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic

  INTEGER, DIMENSION( ims:ime, jms:jme ) ::    IVGTYP_dominant
  INTEGER ::  mosaic_i, URBAN_METHOD, zo_avg_option
  REAL :: FAREA
  LOGICAL :: IPRINT_mosaic, Noah_call




  REAL, PARAMETER  :: CAPA=R_D/CP
  REAL :: APELM,APES,SFCTH2,PSFC
  real, intent(in) :: xice_threshold
  character(len=80) :: message_text



   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)  ::   SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM
   INTEGER, INTENT(IN   )  ::  fasdas
   REAL                    ::  XSDA_HFX, XSDA_QFX, XQNORM
   REAL                    ::  HFX_PHY, QFX_PHY
   REAL                    ::  DZQ
   REAL                    ::  HCPCT_FASDAS
   REAL,OPTIONAL,DIMENSION( ims:ime, jms:jme ) :: IRR_CHAN
   REAL               :: IRRIGATION_CHANNEL 
   IRRIGATION_CHANNEL=0.0
   HFX_PHY = 0.0   
   QFX_PHY = 0.0
   XQNORM  = 0.0
   XSDA_HFX = 0.0
   XSDA_QFX = 0.0




      FDTLIW=DT/ROWLIW

      FDTW=DT/(XLV*RHOWATER)

      IPRINT=.false.
      IPRINT_mosaic=.false.


      SLOPETYP=1


      NSOIL=num_soil_layers

     DO NS=1,NSOIL
     SLDPTH(NS)=DZS(NS)
     ENDDO

     JLOOP : DO J=jts,jte

      IF(ITIMESTEP.EQ.1)THEN
        DO 50 I=its,ite











          IF((XLAND(I,J)-1.5).GE.0.)THEN


            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            DO NS=1,NSOIL
              SMOIS(I,NS,J)=1.0
              TSLB(I,NS,J)=273.16                                          
              SMCREL(I,NS,J)=1.0
            ENDDO
          ELSE
            IF ( XICE(I,J) .GE. XICE_THRESHOLD ) THEN

              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              DO NS=1,NSOIL
                SMOIS(I,NS,J)=1.0
                SMCREL(I,NS,J)=1.0
              ENDDO
            ENDIF
          ENDIF

   50   CONTINUE
      ENDIF                                                               


      ILOOP : DO I=its,ite
      
         IF (((XLAND(I,J)-1.5).LT.0.) .AND. (XICE(I,J) < XICE_THRESHOLD) ) THEN
               
               IVGTYP_dominant(I,J)=IVGTYP(I,J)                                   
      
            
               
                 TSK_mosaic_avg(i,j)= 0.0                              
                 QSFC_mosaic_avg(i,j)= 0.0
                 CANWAT_mosaic_avg(i,j)= 0.0
                 SNOW_mosaic_avg(i,j)= 0.0
                 SNOWH_mosaic_avg(i,j)= 0.0
                 SNOWC_mosaic_avg(i,j)= 0.0
            
                          DO NS=1,NSOIL
            
                     TSLB_mosaic_avg(i,NS,j)=0.0
                     SMOIS_mosaic_avg(i,NS,j)=0.0
                     SH2O_mosaic_avg(i,NS,j)=0.0
            
                         ENDDO
            
                 HFX_mosaic_avg(i,j)= 0.0
                 QFX_mosaic_avg(i,j)= 0.0
                 LH_mosaic_avg(i,j)=  0.0
                 GRDFLX_mosaic_avg(i,j)= 0.0
                 ALBEDO_mosaic_avg(i,j)=0.0
                 ALBBCK_mosaic_avg(i,j)=0.0
                 EMISS_mosaic_avg(i,j)=0.0
                 EMBCK_mosaic_avg(i,j)=0.0
                 ZNT_mosaic_avg(i,j)=0.0
                 Z0_mosaic_avg(i,j)=0.0
                 LAI_mosaic_avg(i,j)=0.0
                 RC_mosaic_avg(i,j)=0.0
                 FAREA_mosaic_avg(i,j)=0.0
            
            
            
               DO mosaic_i = mosaic_cat, 1, -1
               
            


            
               
               IVGTYP(I,J)=mosaic_cat_index(I,mosaic_i,J)                         
               TSK(I,J)=TSK_mosaic(I,mosaic_i,J)                                  
               QSFC(i,j)=QSFC_mosaic(I,mosaic_i,J)
               CANWAT(i,j)=CANWAT_mosaic(i,mosaic_i,j) 
               SNOW(i,j)=SNOW_mosaic(i,mosaic_i,j) 
               SNOWH(i,j)=SNOWH_mosaic(i,mosaic_i,j)  
               SNOWC(i,j)=SNOWC_mosaic(i,mosaic_i,j)
            
                ALBEDO(i,j) = ALBEDO_mosaic(i,mosaic_i,j)
                ALBBCK(i,j)= ALBBCK_mosaic(i,mosaic_i,j) 
                EMISS(i,j)= EMISS_mosaic(i,mosaic_i,j) 
                EMBCK(i,j)= EMBCK_mosaic(i,mosaic_i,j) 
                ZNT(i,j)= ZNT_mosaic(i,mosaic_i,j) 
                Z0(i,j)= Z0_mosaic(i,mosaic_i,j)    
            
                 SNOTIME(i,j)= SNOTIME_mosaic(i,mosaic_i,j)          
              
                          DO NS=1,NSOIL
            
                     TSLB(i,NS,j)=TSLB_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)
                     SMOIS(i,NS,j)=SMOIS_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)
                     SH2O(i,NS,j)=SH2O_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)
            
                          ENDDO
            
                       IF(IPRINT_mosaic) THEN
            
                   print*, 'BEFORE SFLX, in Noahdrv.F'
                   print*, 'mosaic_cat', mosaic_cat, 'IVGTYP',IVGTYP(i,j), 'TSK',TSK(i,j),'HFX',HFX(i,j), 'QSFC', QSFC(i,j),   &
                   'CANWAT', CANWAT(i,j), 'SNOW',SNOW(i,j), 'ALBEDO',ALBEDO(i,j), 'TSLB',TSLB(i,1,j),'CHS',CHS(i,j),'ZNT',ZNT(i,j)
            
                       ENDIF
            
            
            
            
            
      
              PSFC=P8w3D(i,1,j)
      
              SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
      
               Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))
      
      
               Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        
      
      
              IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
                SATFLG=0.
                CHKLOWQ(I,J)=0.
              ELSE
                SATFLG=1.0
                CHKLOWQ(I,J)=1.
              ENDIF
      
              SFCTMP=T3D(i,1,j)
              ZLVL=0.5*DZ8W(i,1,j)
      
      
      
               APES=(1.E5/PSFC)**CAPA
               APELM=(1.E5/SFCPRS)**CAPA
               SFCTH2=SFCTMP*APELM
               TH2=SFCTH2/APES
      
               EMISSI = EMISS(I,J)
               LWDN=GLW(I,J)*EMISSI
      
              SOLDN=SWDOWN(I,J)
      
      
      
              SOLNET=SOLDN*(1.-ALBEDO(I,J))
              PRCP=RAINBL(i,j)/DT
              IF(PRESENT(IRR_CHAN)) THEN
               IF(IRR_CHAN(i,j).NE.0) THEN
                IRRIGATION_CHANNEL=IRR_CHAN(i,j)/DT
               ELSE
                IRRIGATION_CHANNEL=0.
               END IF
              ENDIF
              VEGTYP=IVGTYP(I,J)
              SOILTYP=ISLTYP(I,J)
              SHDFAC=VEGFRA(I,J)/100.
              T1=TSK(I,J)
              CHK=CHS(I,J)
              SHMIN=SHDMIN(I,J)/100. 
              SHMAX=SHDMAX(I,J)/100. 
      
              SNEQV=SNOW(I,J)*0.001
      
              SNOWHK=SNOWH(I,J)
              SNCOVR=SNOWC(I,J)
      
      
      
      
             IF(FRPCPN) THEN
                FFROZP=SR(I,J)
              ELSE
                IF (SFCTMP <=  273.15) THEN
                  FFROZP = 1.0
      	  ELSE
      	    FFROZP = 0.0
      	  ENDIF
              ENDIF
      
              IF((XLAND(I,J)-1.5).GE.0.)THEN                                  
      
                TSK_RURAL(I,J)=TSK(I,J)
                HFX_RURAL(I,J)=HFX(I,J)
                QFX_RURAL(I,J)=QFX(I,J)
                LH_RURAL(I,J)=LH(I,J)
                EMISS_RURAL(I,J)=EMISS(I,J)
                GRDFLX_RURAL(I,J)=GRDFLX(I,J)
              ELSE
      
      
                IF (XICE(I,J) >= XICE_THRESHOLD) THEN
                   
                   ICE = 1
                ELSE IF ( VEGTYP == ISICE ) THEN
                   
                   ICE = -1
                ELSE
                   
                   ICE=0
                ENDIF
                DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2
      
                IF(SNOW(I,J).GT.0.0)THEN
      
                  SFCTSNO=SFCTMP
                  E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
                  Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
                  Q2SATI=Q2SATI/(1.0+Q2SATI)    
                  IF (T1 .GT. 273.14) THEN
      
                    Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
                    DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
                  ELSE
      
                    Q2SAT=Q2SATI
                    DQSDT2=Q2SATI*6174./(SFCTSNO**2)
                  ENDIF
      
                  IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
                ENDIF
      
                
                TBOT=TMN(I,J)
      
                IF(VEGTYP.EQ.25) SHDFAC=0.0000
                IF(VEGTYP.EQ.26) SHDFAC=0.0000
                IF(VEGTYP.EQ.27) SHDFAC=0.0000
                IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
                  SOILTYP=7
                ENDIF
                SNOALB1 = SNOALB(I,J)

                CMC=CANWAT(I,J)/1000.
      
      
      
      
      
      
      
      
      
      
      
      
              ALBBRD=ALBBCK(I,J)
              Z0BRD=Z0(I,J)
              EMBRD=EMBCK(I,J)
              SNOTIME1 = SNOTIME(I,J)
              RIBB=RIB(I,J)
      
      
                DO NS=1,NSOIL
                  SMC(NS)=SMOIS(I,NS,J)
                  STC(NS)=TSLB(I,NS,J)                                          
                  SWC(NS)=SH2O(I,NS,J)
                ENDDO
      
                if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
                  SNOWHK= 5.*SNEQV
                endif
      
      
      
      
      	
      	
                       
      
           
           
           
           
           
           
           
           
          
          
          
          
          
          
         
         
         
         
         
         
          
           
          
           
           
           
      
            Noah_call=.TRUE.
      
            If ( SF_URBAN_PHYSICS == 0 ) THEN   
             IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                 IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                 IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                 IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
 
                       Noah_call = .TRUE.                 
                       VEGTYP = ISURBAN
                 ENDIF
      
            ENDIF
            
            IF(SF_URBAN_PHYSICS == 1) THEN
      
               IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                   IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                   IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                   IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN	
                       Noah_call = .TRUE.                       
              	       VEGTYP = NATURAL                                              
                       SHDFAC = SHDTBL(NATURAL)
                       ALBEDOK =0.2         
                       ALBBRD  =0.2         
                       EMISSI = 0.98        
                       
      		      T1= TS_RUL2D_mosaic(I,mosaic_i,J)
                       
                 ENDIF
      
            ENDIF


           AOASIS = 1.0
           USOIL = 1
           DSOIL = 2
           IRIOPTION=IRI_SCHEME
           OMG= OMG_URB2D(I,J)   
           tloc=mod(int(OMG/3.14159*180./15.+12.+0.5 ),24)
           if (tloc.lt.0) tloc=tloc+24
           if (tloc==0) tloc=24
           CALL cal_mon_day(julian,julyr,jmonth,jday) 
	  IF(SF_URBAN_PHYSICS == 1) THEN
             IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                 IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                 IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                 IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN

                   AOASIS = oasis  
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  
                   IF (jmonth==5 .or. jmonth==6 .or. jmonth==7 .or. jmonth==8 .or. jmonth==9) THEN
                      IF (SMC(USOIL) .LT. SMCREF) SMC(USOIL)= REFSMC(ISLTYP(I,J))
                      IF (SMC(DSOIL) .LT. SMCREF) SMC(DSOIL)= REFSMC(ISLTYP(I,J))
                   ENDIF
                   ENDIF
                   ENDIF
                ENDIF
           ENDIF

	  IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN
          IF(AOASIS > 1.0) THEN
          CALL wrf_error_fatal3("<stdin>",3326,&
'Urban oasis option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          IF(IRIOPTION == 1) THEN
          CALL wrf_error_fatal3("<stdin>",3330,&
'Urban irrigation option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          ENDIF
            
            IF( SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN

            ENDIF
      
         IF (Noah_call) THEN
      
                IF (rdlai2d) THEN
                   IF (SHDFAC > 0.0 .AND. LAI(I,J) <= 0.0) LAI(I,J) = 0.01
                   xlai = lai(i,j)
                endif
      
          IF ( ICE == 1 ) THEN
      
             
      
             DO NS = 1, NSOIL
                SH2O(I,NS,J) = 1.0
             ENDDO
             LAI(I,J) = 0.01
      
             CYCLE ILOOP
      
          ELSEIF (ICE == 0) THEN
      
             
      
             CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    
                       LOCAL,                                            &    
                       LUTYPE, SLTYPE,                                   &    
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    
                       DUMMY,DUMMY, DUMMY,                               &    
                       TH2,Q2SAT,DQSDT2,                                 &    
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    
                       ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    
                       CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    
                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    
                       BETA,ETP,SSOIL,                                   &    
                       FLX1,FLX2,FLX3,                                   &    
      		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                             &    
                       SNOMLT,SNCOVR,                                    &    
                       RUNOFF1,RUNOFF2,RUNOFF3,                          &    
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    
                       SOILW,SOILM,Q1,SMAV,                              &    
                       RDLAI2D,USEMONALB,                                &
                       SNOTIME1,                                         &
                       RIBB,                                             &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                       sfcheadrt(i,j),                                   &    
                       INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    & 
                 ,IRRIGATION_CHANNEL  )
      
          ELSEIF (ICE == -1) THEN
      
             
             

             
             SOILM = 0.0 
             XLAI = 0.01 
             RUNOFF2 = 0.0
             RUNOFF3 = 0.0
             DO NS = 1, NSOIL
                SWC(NS) = 1.0
                SMC(NS) = 1.0
                SMAV(NS) = 1.0
             ENDDO
             CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    
                  &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    
                  &    TH2,Q2SAT,DQSDT2,                                &    
                  &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    
                  &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    
                  &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    
                  &    ESNOW,DEW,                                       &    
                  &    ETP,SSOIL,                                       &    
                  &    FLX1,FLX2,FLX3,                                  &    
                  &    SNOMLT,SNCOVR,                                   &    
                  &    RUNOFF1,                                         &    
                  &    Q1,                                              &    
                  &    SNOTIME1,                                        &
                  &    RIBB)
      
          ENDIF
             lai(i,j) = xlai
      
      
                CANWAT(I,J)=CMC*1000.
                SNOW(I,J)=SNEQV*1000.
      
                SNOWH(I,J)=SNOWHK                   
                ALBEDO(I,J)=ALBEDOK
                ALB_RURAL(I,J)=ALBEDOK
                ALBBCK(I,J)=ALBBRD
                Z0(I,J)=Z0BRD
                EMISS(I,J) = EMISSI
                EMISS_RURAL(I,J) = EMISSI
      
                ZNT(I,J)=Z0K
                TSK(I,J)=T1
                TSK_RURAL(I,J)=T1
                HFX(I,J)=SHEAT
                HFX_RURAL(I,J)=SHEAT
      
              POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
                QFX(I,J)=ETA_KINEMATIC
                QFX_RURAL(I,J)=ETA_KINEMATIC
      
      
                LH(I,J)=ETA
                LH_RURAL(I,J)=ETA
                GRDFLX(I,J)=SSOIL
                GRDFLX_RURAL(I,J)=SSOIL
                SNOWC(I,J)=SNCOVR
                CHS2(I,J)=CQS2(I,J)
                SNOTIME(I,J) = SNOTIME1
      
      
      
                IF (Q1 .GT. QSFC(I,J)) THEN
                  CQS2(I,J) = CHS(I,J)
                ENDIF
      
      
                 QSFC(I,J)= Q1/(1.0-Q1)
      
                 
      
      
                DO 81 NS=1,NSOIL
                 SMOIS(I,NS,J)=SMC(NS)
                 TSLB(I,NS,J)=STC(NS)                                        
                 SH2O(I,NS,J)=SWC(NS)
         81     CONTINUE
      
      
              FLX4_2D(I,J)  = FLX4
      	FVB_2D(I,J)   = FVB
      	FBUR_2D(I,J)  = FBUR
      	FGSN_2D(I,J)  = FGSN
      
           
           
           
      
           IF ( UA_PHYS ) THEN
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4
      
           ELSE
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
           ENDIF
      
               ENDIF   
               
              IF (SF_URBAN_PHYSICS == 1 ) THEN                                              
      
      
      
      
                 IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                     IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                     IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                     IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
 
      
                
                
                IF(IVGTYP(I,J)==ISURBAN) UTYPE_URB=5
                IF(IVGTYP(I,J)==LCZ_1) UTYPE_URB=1
                IF(IVGTYP(I,J)==LCZ_2) UTYPE_URB=2
                IF(IVGTYP(I,J)==LCZ_3) UTYPE_URB=3
                IF(IVGTYP(I,J)==LCZ_4) UTYPE_URB=4
                IF(IVGTYP(I,J)==LCZ_5) UTYPE_URB=5
                IF(IVGTYP(I,J)==LCZ_6) UTYPE_URB=6
                IF(IVGTYP(I,J)==LCZ_7) UTYPE_URB=7
                IF(IVGTYP(I,J)==LCZ_8) UTYPE_URB=8
                IF(IVGTYP(I,J)==LCZ_9) UTYPE_URB=9
                IF(IVGTYP(I,J)==LCZ_10) UTYPE_URB=10
                IF(IVGTYP(I,J)==LCZ_11) UTYPE_URB=11
 
                        
                IF(UTYPE_URB==1) FRC_URB2D(I,J)=1.
                IF(UTYPE_URB==2) FRC_URB2D(I,J)=0.99
                IF(UTYPE_URB==3) FRC_URB2D(I,J)=1.00
                IF(UTYPE_URB==4) FRC_URB2D(I,J)=0.65
                IF(UTYPE_URB==5) FRC_URB2D(I,J)=0.7
                IF(UTYPE_URB==6) FRC_URB2D(I,J)=0.65
                IF(UTYPE_URB==7) FRC_URB2D(I,J)=0.3
                IF(UTYPE_URB==8) FRC_URB2D(I,J)=0.85
                IF(UTYPE_URB==9) FRC_URB2D(I,J)=0.3
                IF(UTYPE_URB==10) FRC_URB2D(I,J)=0.55
                IF(UTYPE_URB==11) FRC_URB2D(I,J)=1.

                  TA_URB    = SFCTMP           
                  QA_URB    = Q2K              
                  UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
                  U1_URB    = U_PHY(I,1,J)
                  V1_URB    = V_PHY(I,1,J)
                  IF(UA_URB < 1.) UA_URB=1.    
                  SSG_URB   = SOLDN            
                  SSGD_URB  = 0.8*SOLDN        
                  SSGQ_URB  = SSG_URB-SSGD_URB 
                  LLG_URB   = GLW(I,J)         
                  RAIN_URB  = RAINBL(I,J)      
                  RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) 
                  ZA_URB    = ZLVL             
                  DELT_URB  = DT               
                  XLAT_URB  = XLAT_URB2D(I,J)  
                  COSZ_URB  = COSZ_URB2D(I,J)  
                  OMG_URB   = OMG_URB2D(I,J)   
                  ZNT_URB   = ZNT(I,J)
      
                  LSOLAR_URB = .FALSE.
                  
      
      
         TR_URB2D(I,J)=TR_URB2D_mosaic(I,mosaic_i,J)                         
         TB_URB2D(I,J)=TB_URB2D_mosaic(I,mosaic_i,J)                         
         TG_URB2D(I,J)=TG_URB2D_mosaic(I,mosaic_i,J)                         
         TC_URB2D(I,J)=TC_URB2D_mosaic(I,mosaic_i,J)                         
         QC_URB2D(I,J)=QC_URB2D_mosaic(I,mosaic_i,J)                         
         UC_URB2D(I,J)=UC_URB2D_mosaic(I,mosaic_i,J)                         
         TS_URB2D(I,J)=TS_URB2D_mosaic(I,mosaic_i,J)                         
      
                  DO K = 1,num_roof_layers
                    TRL_URB3D(I,K,J) = TRL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB3D(I,K,J) = TBL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB3D(I,K,J) = TGL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
      
      
      
                  TR_URB = TR_URB2D(I,J)
                  TB_URB = TB_URB2D(I,J)
                  TG_URB = TG_URB2D(I,J)
                  TC_URB = TC_URB2D(I,J)
                  QC_URB = QC_URB2D(I,J)
                  UC_URB = UC_URB2D(I,J)
      
                  DO K = 1,num_roof_layers
                    TRL_URB(K) = TRL_URB3D(I,K,J)
                    SMR_URB(K) = SMR_URB3D(I,K,J)
                    TGRL_URB(K)= TGRL_URB3D(I,K,J)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB(K) = TBL_URB3D(I,K,J)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB(K) = TGL_URB3D(I,K,J)
                  END DO
                  
                  TGR_URB   = TGR_URB2D(I,J)
                  CMCR_URB  = CMCR_URB2D(I,J)
                  FLXHUMR_URB = FLXHUMR_URB2D(I,J)
                  FLXHUMB_URB = FLXHUMB_URB2D(I,J)
                  FLXHUMG_URB = FLXHUMG_URB2D(I,J)
                  DRELR_URB = DRELR_URB2D(I,J)
                  DRELB_URB = DRELB_URB2D(I,J)
                  DRELG_URB = DRELG_URB2D(I,J)
      
                  XXXR_URB = XXXR_URB2D(I,J)
                  XXXB_URB = XXXB_URB2D(I,J)
                  XXXG_URB = XXXG_URB2D(I,J)
                  XXXC_URB = XXXC_URB2D(I,J)
      
      
                  if (CHS(I,J) < 1.0E-02) then
                     CHS(I,J)  = 1.0E-02
                  endif
                  if (CHS2(I,J) < 1.0E-02) then
                     CHS2(I,J)  = 1.0E-02
                  endif
                  if (CQS2(I,J) < 1.0E-02) then
                     CQS2(I,J)  = 1.0E-02
                  endif
      
                  CHS_URB  = CHS(I,J)
                  CHS2_URB = CHS2(I,J)
                  IF (PRESENT(CMR_SFCDIF)) THEN
                     CMR_URB = CMR_SFCDIF(I,J)
                     CHR_URB = CHR_SFCDIF(I,J)
                     CMGR_URB = CMGR_SFCDIF(I,J)
                     CHGR_URB = CHGR_SFCDIF(I,J)
                     CMC_URB = CMC_SFCDIF(I,J)
                     CHC_URB = CHC_SFCDIF(I,J)
                  ENDIF
      
      
                  mh_urb = mh_urb2d(I,J)
                  stdh_urb = stdh_urb2d(I,J)
                  lp_urb = lp_urb2d(I,J)
                  hgt_urb = hgt_urb2d(I,J)
                  lf_urb = 0.0
                  DO K = 1,4
                    lf_urb(K)=lf_urb2d(I,K,J)
                  ENDDO
                  frc_urb = frc_urb2d(I,J)
                  lb_urb = lb_urb2d(I,J)
                  check = 0
                  if (I.eq.73.and.J.eq.125)THEN
                     check = 1
                  end if
      
      
                  CALL cal_mon_day(julian,julyr,jmonth,jday)      
                  CALL urban(LSOLAR_URB,                                      & 
                             num_roof_layers,num_wall_layers,num_road_layers, & 
                             DZR,DZB,DZG,                                     & 
                             UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & 
                             SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & 
                             ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & 
                             XLAT_URB,DELT_URB,ZNT_URB,                       & 
                             CHS_URB, CHS2_URB,                               & 
                             TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & 
                             TRL_URB,TBL_URB,TGL_URB,                         & 
                             XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & 
                             TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & 
                             SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & 
                             GZ1OZ0_URB,                                      & 
                             CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                             U10_URB, V10_URB, TH2_URB, Q2_URB,               & 
                             UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & 
                             hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & 
                             TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & 
                             DRELR_URB,DRELB_URB,                             & 
                             DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)     
      
      
                  TS_URB2D(I,J) = TS_URB
      
                  ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   
                  HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         
                  QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                           + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                
                  LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            
                  GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       
                  TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            
                  Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            
      
                  QSFC(I,J)= Q1/(1.0-Q1)
                  UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      
                  ZNT(I,J)= EXP(FRC_URB2D(I,J)*ALOG(ZNT_URB)+(1-FRC_URB2D(I,J))* ALOG(ZNT(I,J)))   

      
      
      
                  TR_URB2D(I,J) = TR_URB
                  TB_URB2D(I,J) = TB_URB
                  TG_URB2D(I,J) = TG_URB
                  TC_URB2D(I,J) = TC_URB
                  QC_URB2D(I,J) = QC_URB
                  UC_URB2D(I,J) = UC_URB
      
                  DO K = 1,num_roof_layers
                    TRL_URB3D(I,K,J) = TRL_URB(K)
                    SMR_URB3D(I,K,J) = SMR_URB(K)
                    TGRL_URB3D(I,K,J)= TGRL_URB(K)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB3D(I,K,J) = TBL_URB(K)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB3D(I,K,J) = TGL_URB(K)
                  END DO

                  TGR_URB2D(I,J) =TGR_URB 
                  CMCR_URB2D(I,J)=CMCR_URB
            	    FLXHUMR_URB2D(I,J)=FLXHUMR_URB 
                  FLXHUMB_URB2D(I,J)=FLXHUMB_URB
                  FLXHUMG_URB2D(I,J)=FLXHUMG_URB
                  DRELR_URB2D(I,J) = DRELR_URB 
                  DRELB_URB2D(I,J) = DRELB_URB 
                  DRELG_URB2D(I,J) = DRELG_URB
           
                  XXXR_URB2D(I,J) = XXXR_URB
                  XXXB_URB2D(I,J) = XXXB_URB
                  XXXG_URB2D(I,J) = XXXG_URB
                  XXXC_URB2D(I,J) = XXXC_URB
      
                  SH_URB2D(I,J)    = SH_URB
                  LH_URB2D(I,J)    = LH_URB
                  G_URB2D(I,J)     = G_URB
                  RN_URB2D(I,J)    = RN_URB
                  PSIM_URB2D(I,J)  = PSIM_URB
                  PSIH_URB2D(I,J)  = PSIH_URB
                  GZ1OZ0_URB2D(I,J)= GZ1OZ0_URB
                  U10_URB2D(I,J)   = U10_URB
                  V10_URB2D(I,J)   = V10_URB
                  TH2_URB2D(I,J)   = TH2_URB
                  Q2_URB2D(I,J)    = Q2_URB
                  UST_URB2D(I,J)   = UST_URB
                  AKMS_URB2D(I,J)  = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
                  IF (PRESENT(CMR_SFCDIF)) THEN
                     CMR_SFCDIF(I,J) = CMR_URB
                     CHR_SFCDIF(I,J) = CHR_URB
                     CMGR_SFCDIF(I,J) = CMGR_URB
                     CHGR_SFCDIF(I,J) = CHGR_URB
                     CMC_SFCDIF(I,J) = CMC_URB
                     CHC_SFCDIF(I,J) = CHC_URB
                  ENDIF
                  
                     
      	    	              
      	    	                       TR_URB2D_mosaic(I,mosaic_i,J)=TR_URB2D(I,J)                               
      	    	    	               TB_URB2D_mosaic(I,mosaic_i,J)=TB_URB2D(I,J)                                   
      	    	    	               TG_URB2D_mosaic(I,mosaic_i,J)=TG_URB2D(I,J)                                   
      	    	    	               TC_URB2D_mosaic(I,mosaic_i,J)=TC_URB2D(I,J)                                  
      	    	    	               QC_URB2D_mosaic(I,mosaic_i,J)=QC_URB2D(I,J)                                   
      	    	    	               UC_URB2D_mosaic(I,mosaic_i,J)=UC_URB2D(I,J)                                   
      	    	    	               TS_URB2D_mosaic(I,mosaic_i,J)=TS_URB2D(I,J)                                   
      	    	    	               TS_RUL2D_mosaic(I,mosaic_i,J)=T1                                
      	    	    	  	  
      	    	    	  	              DO K = 1,num_roof_layers
      	    	    	  	                TRL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)=TRL_URB3D(I,K,J) 
      	    	    	  	              END DO
      	    	    	  	              DO K = 1,num_wall_layers
      	    	    	  	                TBL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)=TBL_URB3D(I,K,J) 
      	    	    	  	              END DO
      	    	    	  	              DO K = 1,num_road_layers
      	    	    	  	                TGL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)=TGL_URB3D(I,K,J) 
      	    	    	                        END DO
      	    	    	            
      	    	    	              SH_URB2D_mosaic(I,mosaic_i,J) = SH_URB2D(I,J)
      	    	    	  	      LH_URB2D_mosaic(I,mosaic_i,J) = LH_URB2D(I,J)
      	    	    	              G_URB2D_mosaic(I,mosaic_i,J)  = G_URB2D(I,J)
                                    RN_URB2D_mosaic(I,mosaic_i,J) = RN_URB2D(I,J)
                                    
                END IF
      
               ENDIF                                   
      
      
      
      
      
                SMSTAV(I,J)=SOILW
                SMSTOT(I,J)=SOILM*1000.
                DO NS=1,NSOIL
                SMCREL(I,NS,J)=SMAV(NS)
                ENDDO
      
      
                SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
                UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0
      
                IF(FFROZP.GT.0.5)THEN
                  ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
                ENDIF
                IF(SNOW(I,J).GT.0.)THEN
                  ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
      
                  SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
                ENDIF
      
              ENDIF                                                           
      
      
      
      
      
                  TSK_mosaic(i,mosaic_i,j)=TSK(i,j)                           
                  QSFC_mosaic(i,mosaic_i,j)=QSFC(i,j)
                  CANWAT_mosaic(i,mosaic_i,j)=CANWAT(i,j) 
                  SNOW_mosaic(i,mosaic_i,j)=SNOW(i,j) 
                  SNOWH_mosaic(i,mosaic_i,j)=SNOWH(i,j)  
                  SNOWC_mosaic(i,mosaic_i,j)=SNOWC(i,j) 
      
                  ALBEDO_mosaic(i,mosaic_i,j)=ALBEDO(i,j) 
                  ALBBCK_mosaic(i,mosaic_i,j)=ALBBCK(i,j)  
                  EMISS_mosaic(i,mosaic_i,j)=EMISS(i,j) 
                  EMBCK_mosaic(i,mosaic_i,j)=EMBCK(i,j)  
                  ZNT_mosaic(i,mosaic_i,j)=ZNT(i,j)  
                  Z0_mosaic(i,mosaic_i,j)=Z0(i,j)    
                  LAI_mosaic(i,mosaic_i,j)=XLAI
                  RC_mosaic(i,mosaic_i,j)=RC
                                                                                       
                  HFX_mosaic(i,mosaic_i,j)=HFX(i,j) 
                  QFX_mosaic(i,mosaic_i,j)=QFX(i,j)  
                  LH_mosaic(i,mosaic_i,j)=LH(i,j)  
                  GRDFLX_mosaic(i,mosaic_i,j)=GRDFLX(i,j) 
                  SNOTIME_mosaic(i,mosaic_i,j)=SNOTIME(i,j) 
       
                  DO NS=1,NSOIL
        
                  TSLB_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)=TSLB(i,NS,j)
                  SMOIS_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)=SMOIS(i,NS,j)
                  SH2O_mosaic(i,NSOIL*(mosaic_i-1)+NS,j)=SH2O(i,NS,j)  
      
                  ENDDO
                  
                  
      

      
      
                  FAREA  = landusef2(i,mosaic_i,j)
      
                  TSK_mosaic_avg(i,j) = TSK_mosaic_avg(i,j) + (EMISS_mosaic(i,mosaic_i,j)*TSK_mosaic(i,mosaic_i,j)**4)*FAREA    
                  
                  QSFC_mosaic_avg(i,j) = QSFC_mosaic_avg(i,j) + QSFC_mosaic(i,mosaic_i,j)*FAREA
                  CANWAT_mosaic_avg(i,j) = CANWAT_mosaic_avg(i,j) + CANWAT_mosaic(i,mosaic_i,j)*FAREA
                  SNOW_mosaic_avg(i,j) = SNOW_mosaic_avg(i,j) + SNOW_mosaic(i,mosaic_i,j)*FAREA
                  SNOWH_mosaic_avg(i,j) = SNOWH_mosaic_avg(i,j) + SNOWH_mosaic(i,mosaic_i,j)*FAREA
                  SNOWC_mosaic_avg(i,j) = SNOWC_mosaic_avg(i,j) + SNOWC_mosaic(i,mosaic_i,j)*FAREA
      
                   DO NS=1,NSOIL
      
                 TSLB_mosaic_avg(i,NS,j)=TSLB_mosaic_avg(i,NS,j) + TSLB_mosaic(i,NS*mosaic_i,j)*FAREA
                 SMOIS_mosaic_avg(i,NS,j)=SMOIS_mosaic_avg(i,NS,j) + SMOIS_mosaic(i,NS*mosaic_i,j)*FAREA
                 SH2O_mosaic_avg(i,NS,j)=SH2O_mosaic_avg(i,NS,j) + SH2O_mosaic(i,NS*mosaic_i,j)*FAREA
      
                   ENDDO
      
                  FAREA_mosaic_avg(i,j)=FAREA_mosaic_avg(i,j)+FAREA
                  HFX_mosaic_avg(i,j) = HFX_mosaic_avg(i,j) + HFX_mosaic(i,mosaic_i,j)*FAREA
                  QFX_mosaic_avg(i,j) = QFX_mosaic_avg(i,j) + QFX_mosaic(i,mosaic_i,j)*FAREA
                  LH_mosaic_avg(i,j) = LH_mosaic_avg(i,j) + LH_mosaic(i,mosaic_i,j)*FAREA
                  GRDFLX_mosaic_avg(i,j)=GRDFLX_mosaic_avg(i,j)+GRDFLX_mosaic(i,mosaic_i,j)*FAREA
                  
                  ALBEDO_mosaic_avg(i,j)=ALBEDO_mosaic_avg(i,j)+ALBEDO_mosaic(i,mosaic_i,j)*FAREA
                  ALBBCK_mosaic_avg(i,j)=ALBBCK_mosaic_avg(i,j)+ALBBCK_mosaic(i,mosaic_i,j)*FAREA
                  EMISS_mosaic_avg(i,j)=EMISS_mosaic_avg(i,j)+EMISS_mosaic(i,mosaic_i,j)*FAREA
                  EMBCK_mosaic_avg(i,j)=EMBCK_mosaic_avg(i,j)+EMBCK_mosaic(i,mosaic_i,j)*FAREA
                  ZNT_mosaic_avg(i,j)=ZNT_mosaic_avg(i,j)+ALOG(ZNT_mosaic(i,mosaic_i,j))*FAREA
                  Z0_mosaic_avg(i,j)=Z0_mosaic_avg(i,j)+ALOG(Z0_mosaic(i,mosaic_i,j))*FAREA
                  LAI_mosaic_avg(i,j)=LAI_mosaic_avg(i,j)+LAI_mosaic(i,mosaic_i,j)*FAREA
                  if(RC_mosaic(i,mosaic_i,j) .Gt. 0.0) Then
                     RC_mosaic_avg(i,j) = RC_mosaic_avg(i,j)+1.0/RC_mosaic(i,mosaic_i,j)*FAREA
                  else 
                     RC_mosaic_avg(i,j) = RC_mosaic_avg(i,j) + RC_mosaic(i,mosaic_i,j)*FAREA
                  End If
                  ENDDO                     

      

      
      
          IVGTYP(I,J)=IVGTYP_dominant(I,J)                                 
          ALBEDO(i,j)=ALBEDO_mosaic_avg(i,j)
          ALBBCK(i,j)=ALBBCK_mosaic_avg(i,j) 
          EMISS(i,j)= EMISS_mosaic_avg(i,j) 
          EMBCK(i,j)= EMBCK_mosaic_avg(i,j)  
          ZNT(i,j)= EXP(ZNT_mosaic_avg(i,j)/FAREA_mosaic_avg(i,j))
          Z0(i,j)= EXP(Z0_mosaic_avg(i,j)/FAREA_mosaic_avg(i,j))
          XLAI2(i,j)= LAI_mosaic_avg(i,j)
          IF (RC_mosaic_avg(i,j) .Gt. 0.0) THEN
              rc2(i,j) = 1.0/(RC_mosaic_avg(i,j))
          ELSE

              rc2(i,j) = RC_mosaic_avg(i,j)
          END IF
          TSK(i,j)=(TSK_mosaic_avg(I,J)/EMISS_mosaic_avg(I,J))**(0.25)                                  
          QSFC(i,j)=QSFC_mosaic_avg(I,J)
          CANWAT(i,j) = CANWAT_mosaic_avg(i,j)
          SNOW(i,j) = SNOW_mosaic_avg(i,j)
          SNOWH(i,j) = SNOWH_mosaic_avg(i,j)  
          SNOWC(i,j) = SNOWC_mosaic_avg(i,j)    
          
          HFX(i,j) = HFX_mosaic_avg(i,j) 
          QFX(i,j) = QFX_mosaic_avg(i,j) 
          LH(i,j) = LH_mosaic_avg(i,j) 
          GRDFLX(i,j)=GRDFLX_mosaic_avg(i,j)
        
                    DO NS=1,NSOIL
      
               TSLB(i,NS,j)=TSLB_mosaic_avg(i,NS,j)
               SMOIS(i,NS,j)=SMOIS_mosaic_avg(i,NS,j)
               SH2O(i,NS,j)=SH2O_mosaic_avg(i,NS,j)
      
                    ENDDO
      
      ELSE    
      
      
              PSFC=P8w3D(i,1,j)
      
              SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
      
               Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))
      
      
               Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        
      
      
              IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
                SATFLG=0.
                CHKLOWQ(I,J)=0.
              ELSE
                SATFLG=1.0
                CHKLOWQ(I,J)=1.
              ENDIF
      
              SFCTMP=T3D(i,1,j)
              ZLVL=0.5*DZ8W(i,1,j)
      
      
      
               APES=(1.E5/PSFC)**CAPA
               APELM=(1.E5/SFCPRS)**CAPA
               SFCTH2=SFCTMP*APELM
               TH2=SFCTH2/APES
      
               EMISSI = EMISS(I,J)
               LWDN=GLW(I,J)*EMISSI
      
              SOLDN=SWDOWN(I,J)
      
      
      
              SOLNET=SOLDN*(1.-ALBEDO(I,J))
              PRCP=RAINBL(i,j)/DT
              VEGTYP=IVGTYP(I,J)
              SOILTYP=ISLTYP(I,J)
              SHDFAC=VEGFRA(I,J)/100.
              T1=TSK(I,J)
              CHK=CHS(I,J)
              SHMIN=SHDMIN(I,J)/100. 
              SHMAX=SHDMAX(I,J)/100. 
      
              SNEQV=SNOW(I,J)*0.001
      
              SNOWHK=SNOWH(I,J)
              SNCOVR=SNOWC(I,J)
      
      
      
      
             IF(FRPCPN) THEN
                FFROZP=SR(I,J)
              ELSE
                IF (SFCTMP <=  273.15) THEN
                  FFROZP = 1.0
      	  ELSE
      	    FFROZP = 0.0
      	  ENDIF
              ENDIF
      
              IF((XLAND(I,J)-1.5).GE.0.)THEN                                  
      
                TSK_RURAL(I,J)=TSK(I,J)
                HFX_RURAL(I,J)=HFX(I,J)
                QFX_RURAL(I,J)=QFX(I,J)
                LH_RURAL(I,J)=LH(I,J)
                EMISS_RURAL(I,J)=EMISS(I,J)
                GRDFLX_RURAL(I,J)=GRDFLX(I,J)
              ELSE
      
      
                IF (XICE(I,J) >= XICE_THRESHOLD) THEN
                   
                   ICE = 1
                ELSE IF ( VEGTYP == ISICE ) THEN
                   
                   ICE = -1
                ELSE
                   
                   ICE=0
                ENDIF
                DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2
      
                IF(SNOW(I,J).GT.0.0)THEN
      
                  SFCTSNO=SFCTMP
                  E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
                  Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
                  Q2SATI=Q2SATI/(1.0+Q2SATI)    
                  IF (T1 .GT. 273.14) THEN
      
                    Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
                    DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
                  ELSE
      
                    Q2SAT=Q2SATI
                    DQSDT2=Q2SATI*6174./(SFCTSNO**2)
                  ENDIF
      
                  IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
                ENDIF
      
                
                TBOT=TMN(I,J)
      
                IF(VEGTYP.EQ.25) SHDFAC=0.0000
                IF(VEGTYP.EQ.26) SHDFAC=0.0000
                IF(VEGTYP.EQ.27) SHDFAC=0.0000
                IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
                  SOILTYP=7
                ENDIF
                SNOALB1 = SNOALB(I,J)
                CMC=CANWAT(I,J)/1000.
      
      
      
      
      
      
      
      
      
      
      
      
              ALBBRD=ALBBCK(I,J)
              Z0BRD=Z0(I,J)
              EMBRD=EMBCK(I,J)
              SNOTIME1 = SNOTIME(I,J)
              RIBB=RIB(I,J)
      
      
                DO NS=1,NSOIL
                  SMC(NS)=SMOIS(I,NS,J)
                  STC(NS)=TSLB(I,NS,J)                                          
                  SWC(NS)=SH2O(I,NS,J)
                ENDDO
      
                if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
                  SNOWHK= 5.*SNEQV
                endif
      
      
      
      
                 IF(SF_URBAN_PHYSICS == 1.OR. SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN

                IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                    IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                    IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                    IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
	
      		 VEGTYP = NATURAL
                       SHDFAC = SHDTBL(NATURAL)
                       ALBEDOK =0.2         
                       ALBBRD  =0.2         
                       EMISSI = 0.98                                 
      		 IF ( FRC_URB2D(I,J) < 0.99 ) THEN
                   if(sf_urban_physics.eq.1)then
                     T1= ( TSK(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
                   elseif((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then
                     T1=tsk_rural_bep(i,j)
                   endif
     	         ELSE
      		 T1 = TSK(I,J)
                       ENDIF
                      ENDIF
                 ELSE
                         IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                             IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                             IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                             IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
                        VEGTYP = ISURBAN
                 	 ENDIF
                 ENDIF



           AOASIS = 1.0
           USOIL = 1
           DSOIL = 2
           IRIOPTION=IRI_SCHEME
           OMG= OMG_URB2D(I,J)   
           tloc=mod(int(OMG/3.14159*180./15.+12.+0.5 ),24)
           if (tloc.lt.0) tloc=tloc+24
           if (tloc==0) tloc=24
           CALL cal_mon_day(julian,julyr,jmonth,jday) 
	  IF(SF_URBAN_PHYSICS == 1) THEN
                          IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                             IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                             IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                             IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN

                AOASIS = oasis  
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  
                   IF (jmonth==5 .or. jmonth==6 .or. jmonth==7 .or. jmonth==8 .or. jmonth==9) THEN
                      IF (SMC(USOIL) .LT. SMCREF) SMC(USOIL)= REFSMC(ISLTYP(I,J))
                      IF (SMC(DSOIL) .LT. SMCREF) SMC(DSOIL)= REFSMC(ISLTYP(I,J))
                   ENDIF
                   ENDIF
                   ENDIF
                ENDIF
           ENDIF

	  IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN
          IF(AOASIS > 1.0) THEN
          CALL wrf_error_fatal3("<stdin>",4125,&
'Urban oasis option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          IF(IRIOPTION == 1) THEN
          CALL wrf_error_fatal3("<stdin>",4129,&
'Urban irrigation option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          ENDIF

      
                IF (rdlai2d) THEN
                   xlai = lai(i,j)
                endif
      
          IF ( ICE == 1 ) THEN
      
             
      
             DO NS = 1, NSOIL
                SH2O(I,NS,J) = 1.0
             ENDDO
             LAI(I,J) = 0.01
      
             CYCLE ILOOP
      
          ELSEIF (ICE == 0) THEN
      
             
      
             CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    
                       LOCAL,                                            &    
                       LUTYPE, SLTYPE,                                   &    
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    
                       DUMMY,DUMMY, DUMMY,                               &    
                       TH2,Q2SAT,DQSDT2,                                 &    
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    
                       ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    
                       CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    
                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    
                       BETA,ETP,SSOIL,                                   &    
                       FLX1,FLX2,FLX3,                                   &    
      		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                             &    
                       SNOMLT,SNCOVR,                                    &    
                       RUNOFF1,RUNOFF2,RUNOFF3,                          &    
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    
                       SOILW,SOILM,Q1,SMAV,                              &    
                       RDLAI2D,USEMONALB,                                &
                       SNOTIME1,                                         &
                       RIBB,                                             &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                       sfcheadrt(i,j),                                   &    
                       INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    & 
                      ,IRRIGATION_CHANNEL )
      
          ELSEIF (ICE == -1) THEN
      
             
             

             
             SOILM = 0.0 
             XLAI = 0.01 
             RUNOFF2 = 0.0
             RUNOFF3 = 0.0
             DO NS = 1, NSOIL
                SWC(NS) = 1.0
                SMC(NS) = 1.0
                SMAV(NS) = 1.0
             ENDDO
             CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    
                  &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    
                  &    TH2,Q2SAT,DQSDT2,                                &    
                  &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    
                  &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    
                  &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    
                  &    ESNOW,DEW,                                       &    
                  &    ETP,SSOIL,                                       &    
                  &    FLX1,FLX2,FLX3,                                  &    
                  &    SNOMLT,SNCOVR,                                   &    
                  &    RUNOFF1,                                         &    
                  &    Q1,                                              &    
                  &    SNOTIME1,                                        &
                  &    RIBB)
      
          ENDIF
      
             lai(i,j) = xlai

      
      
                CANWAT(I,J)=CMC*1000.
                SNOW(I,J)=SNEQV*1000.
      
                SNOWH(I,J)=SNOWHK                   
                ALBEDO(I,J)=ALBEDOK
                ALB_RURAL(I,J)=ALBEDOK
                ALBBCK(I,J)=ALBBRD
                Z0(I,J)=Z0BRD
                EMISS(I,J) = EMISSI
                EMISS_RURAL(I,J) = EMISSI
      
                ZNT(I,J)=Z0K
                TSK(I,J)=T1
                TSK_RURAL(I,J)=T1
                HFX(I,J)=SHEAT
                HFX_RURAL(I,J)=SHEAT
      
              POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
                QFX(I,J)=ETA_KINEMATIC
                QFX_RURAL(I,J)=ETA_KINEMATIC
      
      
                LH(I,J)=ETA
                LH_RURAL(I,J)=ETA
                GRDFLX(I,J)=SSOIL
                GRDFLX_RURAL(I,J)=SSOIL
                SNOWC(I,J)=SNCOVR
                CHS2(I,J)=CQS2(I,J)
                SNOTIME(I,J) = SNOTIME1
      
      
      
                IF (Q1 .GT. QSFC(I,J)) THEN
                  CQS2(I,J) = CHS(I,J)
                ENDIF
      
      
                 QSFC(I,J)= Q1/(1.0-Q1)
      
                 
      
      
                DO 80 NS=1,NSOIL
                 SMOIS(I,NS,J)=SMC(NS)
                 TSLB(I,NS,J)=STC(NS)                                        
                 SH2O(I,NS,J)=SWC(NS)
         80     CONTINUE
      
      
              FLX4_2D(I,J)  = FLX4
      	FVB_2D(I,J)   = FVB
      	FBUR_2D(I,J)  = FBUR
      	FGSN_2D(I,J)  = FGSN
           
           
           
      
           IF ( UA_PHYS ) THEN
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4
      
           ELSE
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
           ENDIF
      
              IF (SF_URBAN_PHYSICS == 1 ) THEN                                              
      
      
      
      
                  IF( IVGTYP(I,J) == ISURBAN    .or. IVGTYP(I,J) == LCZ_1 .or. IVGTYP(I,J) == LCZ_2 .or. &
                      IVGTYP(I,J) == LCZ_3      .or. IVGTYP(I,J) == LCZ_4 .or. IVGTYP(I,J) == LCZ_5 .or. &
                      IVGTYP(I,J) == LCZ_6      .or. IVGTYP(I,J) == LCZ_7 .or. IVGTYP(I,J) == LCZ_8 .or. &
                      IVGTYP(I,J) == LCZ_9      .or. IVGTYP(I,J) == LCZ_10 .or. IVGTYP(I,J) == LCZ_11 ) THEN
 
      
      
      
                  UTYPE_URB = UTYPE_URB2D(I,J) 
      
                  TA_URB    = SFCTMP           
                  QA_URB    = Q2K              
                  UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
                  U1_URB    = U_PHY(I,1,J)
                  V1_URB    = V_PHY(I,1,J)
                  IF(UA_URB < 1.) UA_URB=1.    
                  SSG_URB   = SOLDN            
                  SSGD_URB  = 0.8*SOLDN        
                  SSGQ_URB  = SSG_URB-SSGD_URB 
                  LLG_URB   = GLW(I,J)         
                  RAIN_URB  = RAINBL(I,J)      
                  RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) 
                  ZA_URB    = ZLVL             
                  DELT_URB  = DT               
                  XLAT_URB  = XLAT_URB2D(I,J)  
                  COSZ_URB  = COSZ_URB2D(I,J)  
                  OMG_URB   = OMG_URB2D(I,J)   
                  ZNT_URB   = ZNT(I,J)
      
                  LSOLAR_URB = .FALSE.
      
                  TR_URB = TR_URB2D(I,J)
                  TB_URB = TB_URB2D(I,J)
                  TG_URB = TG_URB2D(I,J)
                  TC_URB = TC_URB2D(I,J)
                  QC_URB = QC_URB2D(I,J)
                  UC_URB = UC_URB2D(I,J)
      
                  DO K = 1,num_roof_layers
                    TRL_URB(K) = TRL_URB3D(I,K,J)
                    SMR_URB(K) = SMR_URB3D(I,K,J)
             	      TGRL_URB(K)= TGRL_URB3D(I,K,J)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB(K) = TBL_URB3D(I,K,J)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB(K) = TGL_URB3D(I,K,J)
                  END DO

                  TGR_URB   = TGR_URB2D(I,J)
                  CMCR_URB  = CMCR_URB2D(I,J)
                  FLXHUMR_URB = FLXHUMR_URB2D(I,J)
                  FLXHUMB_URB = FLXHUMB_URB2D(I,J)
                  FLXHUMG_URB = FLXHUMG_URB2D(I,J)
                  DRELR_URB = DRELR_URB2D(I,J)
                  DRELB_URB = DRELB_URB2D(I,J)
                  DRELG_URB = DRELG_URB2D(I,J)
      
                  XXXR_URB = XXXR_URB2D(I,J)
                  XXXB_URB = XXXB_URB2D(I,J)
                  XXXG_URB = XXXG_URB2D(I,J)
                  XXXC_URB = XXXC_URB2D(I,J)
      
      
                  if (CHS(I,J) < 1.0E-02) then
                     CHS(I,J)  = 1.0E-02
                  endif
                  if (CHS2(I,J) < 1.0E-02) then
                     CHS2(I,J)  = 1.0E-02
                  endif
                  if (CQS2(I,J) < 1.0E-02) then
                     CQS2(I,J)  = 1.0E-02
                  endif
      
                  CHS_URB  = CHS(I,J)
                  CHS2_URB = CHS2(I,J)
                  IF (PRESENT(CMR_SFCDIF)) THEN
                     CMR_URB = CMR_SFCDIF(I,J)
                     CHR_URB = CHR_SFCDIF(I,J)
                     CMGR_URB = CMGR_SFCDIF(I,J)
                     CHGR_URB = CHGR_SFCDIF(I,J)
                     CMC_URB = CMC_SFCDIF(I,J)
                     CHC_URB = CHC_SFCDIF(I,J)
                  ENDIF
      
      
                  mh_urb = mh_urb2d(I,J)
                  stdh_urb = stdh_urb2d(I,J)
                  lp_urb = lp_urb2d(I,J)
                  hgt_urb = hgt_urb2d(I,J)
                  lf_urb = 0.0
                  DO K = 1,4
                    lf_urb(K)=lf_urb2d(I,K,J)
                  ENDDO
                  frc_urb = frc_urb2d(I,J)
                  lb_urb = lb_urb2d(I,J)
                  check = 0
                  if (I.eq.73.and.J.eq.125)THEN
                     check = 1
                  end if
      
      
                  CALL cal_mon_day(julian,julyr,jmonth,jday)        
                  CALL urban(LSOLAR_URB,                                      & 
                             num_roof_layers,num_wall_layers,num_road_layers, & 
                             DZR,DZB,DZG,                                     & 
                             UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & 
                             SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & 
                             ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & 
                             XLAT_URB,DELT_URB,ZNT_URB,                       & 
                             CHS_URB, CHS2_URB,                               & 
                             TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & 
                             TRL_URB,TBL_URB,TGL_URB,                         & 
                             XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & 
                             TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & 
                             SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & 
                             GZ1OZ0_URB,                                      & 
                             CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                             U10_URB, V10_URB, TH2_URB, Q2_URB,               & 
                             UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & 
                             hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & 
                             TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & 
                             DRELR_URB,DRELB_URB,                             & 
                             DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)  
      
      
                  TS_URB2D(I,J) = TS_URB
      
                  ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   
                  HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         
                  QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                           + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                
                  LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            
                  GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       
                  TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            
                  Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            
      
                  QSFC(I,J)= Q1/(1.0-Q1)
                  UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      
      
      
      
      
                  TR_URB2D(I,J) = TR_URB
                  TB_URB2D(I,J) = TB_URB
                  TG_URB2D(I,J) = TG_URB
                  TC_URB2D(I,J) = TC_URB
                  QC_URB2D(I,J) = QC_URB
                  UC_URB2D(I,J) = UC_URB
      
                  DO K = 1,num_roof_layers
                    TRL_URB3D(I,K,J) = TRL_URB(K)
                    SMR_URB3D(I,K,J) = SMR_URB(K)
                    TGRL_URB3D(I,K,J)= TGRL_URB(K)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB3D(I,K,J) = TBL_URB(K)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB3D(I,K,J) = TGL_URB(K)
                  END DO
                 
                  TGR_URB2D(I,J) =TGR_URB 
                  CMCR_URB2D(I,J)=CMCR_URB
                  FLXHUMR_URB2D(I,J)=FLXHUMR_URB 
                  FLXHUMB_URB2D(I,J)=FLXHUMB_URB
                  FLXHUMG_URB2D(I,J)=FLXHUMG_URB
                  DRELR_URB2D(I,J) = DRELR_URB 
                  DRELB_URB2D(I,J) = DRELB_URB 
                  DRELG_URB2D(I,J) = DRELG_URB

                  XXXR_URB2D(I,J) = XXXR_URB
                  XXXB_URB2D(I,J) = XXXB_URB
                  XXXG_URB2D(I,J) = XXXG_URB
                  XXXC_URB2D(I,J) = XXXC_URB
      
                  SH_URB2D(I,J)    = SH_URB
                  LH_URB2D(I,J)    = LH_URB
                  G_URB2D(I,J)     = G_URB
                  RN_URB2D(I,J)    = RN_URB
                  PSIM_URB2D(I,J)  = PSIM_URB
                  PSIH_URB2D(I,J)  = PSIH_URB
                  GZ1OZ0_URB2D(I,J)= GZ1OZ0_URB
                  U10_URB2D(I,J)   = U10_URB
                  V10_URB2D(I,J)   = V10_URB
                  TH2_URB2D(I,J)   = TH2_URB
                  Q2_URB2D(I,J)    = Q2_URB
                  UST_URB2D(I,J)   = UST_URB
                  AKMS_URB2D(I,J)  = KARMAN * UST_URB2D(I,J)/(GZ1OZ0_URB2D(I,J)-PSIM_URB2D(I,J))
                  IF (PRESENT(CMR_SFCDIF)) THEN
                     CMR_SFCDIF(I,J) = CMR_URB
                     CHR_SFCDIF(I,J) = CHR_URB
                     CMGR_SFCDIF(I,J) = CMGR_URB
                     CHGR_SFCDIF(I,J) = CHGR_URB
                     CMC_SFCDIF(I,J) = CMC_URB
                     CHC_SFCDIF(I,J) = CHC_URB
                  ENDIF
                END IF
      
               ENDIF                                   
      
      
      
      
      
                SMSTAV(I,J)=SOILW
                SMSTOT(I,J)=SOILM*1000.
                DO NS=1,NSOIL
                SMCREL(I,NS,J)=SMAV(NS)
                ENDDO
      
      
                SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
                UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0
      
                IF(FFROZP.GT.0.5)THEN
                  ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
                ENDIF
                IF(SNOW(I,J).GT.0.)THEN
                  ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
      
                  SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
                ENDIF
      
              ENDIF                                                           

      ENDIF                                           

      ENDDO ILOOP                                                       
   ENDDO JLOOP                                                          
       

   END SUBROUTINE lsm_mosaic






  
   SUBROUTINE lsm_mosaic_init(IVGTYP,ISWATER,ISURBAN,ISICE, XLAND, XICE,fractional_seaice, &
                  TSK,TSLB,SMOIS,SH2O,SNOW,SNOWC,SNOWH,CANWAT,    &
                  ids,ide, jds,jde, kds,kde,                      &
                  ims,ime, jms,jme, kms,kme,                      &
                  its,ite, jts,jte, kts,kte, restart,             &
                  landusef,landusef2,NLCAT,num_soil_layers        & 
                  ,sf_surface_mosaic, mosaic_cat                  & 
                  ,mosaic_cat_index                               &   
                  ,TSK_mosaic,TSLB_mosaic                         &
                  ,SMOIS_mosaic,SH2O_mosaic                       & 
                  ,CANWAT_mosaic,SNOW_mosaic                      &
                  ,SNOWH_mosaic,SNOWC_mosaic                      &
                  ,ALBEDO,ALBBCK, EMISS, EMBCK,Z0                 &  
                  ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic      &  
                  ,EMBCK_mosaic, ZNT_mosaic, Z0_mosaic            &  
                  ,TR_URB2D_mosaic,TB_URB2D_mosaic                &  
                  ,TG_URB2D_mosaic,TC_URB2D_mosaic                &  
                  ,QC_URB2D_mosaic                                &  
                  ,TRL_URB3D_mosaic,TBL_URB3D_mosaic              &  
                  ,TGL_URB3D_mosaic                               &  
                  ,SH_URB2D_mosaic,LH_URB2D_mosaic                &  
                  ,G_URB2D_mosaic,RN_URB2D_mosaic                 &  
                  ,TS_URB2D_mosaic                                &  
                  ,TS_RUL2D_mosaic                                &  
                   ) 
  
    INTEGER,  INTENT(IN)   ::       ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte 

   INTEGER, INTENT(IN)       ::     NLCAT, num_soil_layers, ISWATER,ISURBAN, ISICE, fractional_seaice

   LOGICAL , INTENT(IN) :: restart 



   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(IN)    ::                             SMOIS, &  
                                                         SH2O,  &  
                                                         TSLB      

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN)    ::                           SNOW, &
                                                         SNOWH, &
                                                         SNOWC, &
                                                        CANWAT, &
                                                        TSK, XICE, XLAND         
  
  INTEGER, INTENT(IN) :: sf_surface_mosaic  
  INTEGER, INTENT(IN) :: mosaic_cat
  INTEGER, DIMENSION( ims:ime, jms:jme ),INTENT(IN) :: IVGTYP
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(IN)::   LANDUSEF
  REAL, DIMENSION( ims:ime, NLCAT, jms:jme ) , INTENT(INOUT)::   LANDUSEF2
  
  INTEGER, DIMENSION( ims:ime, NLCAT, jms:jme ), INTENT(INOUT) :: mosaic_cat_index 

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        TSK_mosaic, CANWAT_mosaic, SNOW_mosaic,SNOWH_mosaic, SNOWC_mosaic 
  REAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), OPTIONAL, INTENT(INOUT)::   &
        TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic
  
  REAL, DIMENSION( ims:ime, jms:jme ) , INTENT(IN)::   ALBEDO, ALBBCK, EMISS, EMBCK, Z0 
  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic, EMBCK_mosaic, ZNT_mosaic, Z0_mosaic

  REAL, DIMENSION( ims:ime, 1:mosaic_cat, jms:jme ) , OPTIONAL, INTENT(INOUT)::   &
        TR_URB2D_mosaic, TB_URB2D_mosaic, TG_URB2D_mosaic, TC_URB2D_mosaic,QC_URB2D_mosaic,  &
        SH_URB2D_mosaic,LH_URB2D_mosaic,G_URB2D_mosaic,RN_URB2D_mosaic,TS_URB2D_mosaic, TS_RUL2D_mosaic  
                    
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TRL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TBL_URB3D_mosaic
  REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_soil_layers*mosaic_cat, jms:jme ), INTENT(INOUT) :: TGL_URB3D_mosaic    

  INTEGER :: ij,i,j,mosaic_i,LastSwap,NumPairs,soil_k, Temp2,Temp5,Temp7, ICE,temp_index
  REAL :: Temp, Temp3,Temp4,Temp6,xice_threshold
  LOGICAL :: IPRINT
  CHARACTER(len=256) :: message_text

  IPRINT=.false.

  if ( fractional_seaice == 0 ) then
     xice_threshold = 0.5
  else if ( fractional_seaice == 1 ) then
     xice_threshold = 0.02
  endif

    IF(.not.restart)THEN
  
  
  
  
  itf=min0(ite,ide-1)
  jtf=min0(jte,jde-1)

  
   
  DO i = its,itf
     DO j = jts,jtf 
        IF ((xland(i,j).LT. 1.5 ) .AND. (IVGTYP(i,j) .EQ. ISWATER)) THEN
           PRINT*, 'BEFORE MOSAIC_INIT'
           CALL wrf_message("BEFORE MOSAIC_INIT")
           WRITE(message_text,fmt='(a,2I6,2F8.2,2I6)') 'I,J,xland,xice,mosaic_cat_index,ivgtyp = ', &
                 I,J,xland(i,j),xice(i,j),mosaic_cat_index(I,1,J),IVGTYP(i,j)
           CALL wrf_message(message_text)
        ENDIF
     ENDDO
  ENDDO

     DO i = its,itf
        DO j = jts,jtf
           DO mosaic_i=1,NLCAT
              LANDUSEF2(i,mosaic_i,j)=LANDUSEF(i,mosaic_i,j)
              mosaic_cat_index(i,mosaic_i,j)=mosaic_i
           ENDDO
        ENDDO
     ENDDO

     DO i = its,itf
        DO j = jts,jtf
          
          NumPairs=NLCAT-1
          
          DO 
               IF (NumPairs == 0) EXIT
                   LastSwap = 1
          DO  mosaic_i=1, NumPairs
            IF(LANDUSEF2(i,mosaic_i, j) < LANDUSEF2(i,mosaic_i+1, j)  ) THEN
               Temp = LANDUSEF2(i,mosaic_i, j)
               LANDUSEF2(i,mosaic_i, j)=LANDUSEF2(i,mosaic_i+1, j)
               LANDUSEF2(i,mosaic_i+1, j)=Temp            
               LastSwap = mosaic_i 
            
               Temp2 =  mosaic_cat_index(i,mosaic_i,j)
               mosaic_cat_index(i,mosaic_i,j)=mosaic_cat_index(i,mosaic_i+1,j)
               mosaic_cat_index(i,mosaic_i+1,j)=Temp2
            ENDIF
          ENDDO
               NumPairs = LastSwap - 1
          ENDDO
          
        ENDDO
      ENDDO

  
  
  

     DO i = its,itf
        DO j = jts,jtf
        
         IF   (XLAND(I,J).LT.1.5)  THEN

             ICE = 0
                 IF( XICE(I,J).GE. XICE_THRESHOLD ) THEN
                   WRITE (message_text,fmt='(a,2I5)') 'sea-ice at point, I and J = ', i,j
                   CALL wrf_message(message_text)
                 ICE = 1
                 ENDIF   
           
          IF (ICE == 1)   Then         

          IF (IVGTYP(i,j) == isice)  THEN    

          ELSE

                DO mosaic_i=2,mosaic_cat
                   IF (mosaic_cat_index(i,mosaic_i,j) == isice ) THEN
                       Temp4=LANDUSEF2(i,mosaic_i,j)
                       Temp5=mosaic_cat_index(i,mosaic_i,j)

                       LANDUSEF2(i,mosaic_i:NLCAT-1,j)=LANDUSEF2(i,mosaic_i+1:NLCAT,j)                       
                       mosaic_cat_index(i,mosaic_i:NLCAT-1,j)=mosaic_cat_index(i,mosaic_i+1:NLCAT,j)

                       LANDUSEF2(i,NLCAT,j)=Temp4
                       mosaic_cat_index(i,NLCAT,j)=Temp5
                   ENDIF 
                 ENDDO

          ENDIF   
          
          ELSEIF (ICE ==0)  THEN
          
          IF ((mosaic_cat_index(I,1,J) .EQ. ISWATER)) THEN    
          
          
	             
           IF (IVGTYP(i,j) .EQ. ISWATER) THEN  
           
           
           
              CALL wrf_message("IN MOSAIC_INIT")
              WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j) 
              CALL wrf_message(message_text)
              CALL wrf_message("xland < 1.5 but the dominant land use category based on our calculation is water."//&
                   "In addition, the dominant land use category based on the geogrid calculation is water, this must be wrong")
           
           ENDIF  
           
           IF (IVGTYP(i,j) .NE. ISWATER) THEN 
           
           
           
	       Temp4=LANDUSEF2(i,1,j)
	       Temp5=mosaic_cat_index(i,1,j)
	  
	       LANDUSEF2(i,1:NLCAT-1,j)=LANDUSEF2(i,2:NLCAT,j)                       
	       mosaic_cat_index(i,1:NLCAT-1,j)=mosaic_cat_index(i,2:NLCAT,j)
	  
	       LANDUSEF2(i,NLCAT,j)=Temp4
	       mosaic_cat_index(i,NLCAT,j)=Temp5
	             
              CALL wrf_message("IN MOSAIC_INIT")
              WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j) 
              CALL wrf_message(message_text)
              CALL wrf_message("xland < 1.5 but the dominant land use category based on our calculation is water."//&
                   "this is fine as long as we change our calculation so that the dominant land use category is"//&
                   "stwiched back to not water.")
              WRITE(message_text,fmt='(a,2I6)') 'land use category has been switched, before and after values are ', &
                   temp5,mosaic_cat_index(i,1,j)
              CALL wrf_message(message_text)
              WRITE(message_text,fmt='(a,2I6)') 'new dominant and second dominant cat are ', mosaic_cat_index(i,1,j),mosaic_cat_index(i,2,j)
              CALL wrf_message(message_text)
	             
           ENDIF  
          
           ELSE    
           
                     DO mosaic_i=2,mosaic_cat
	                IF (mosaic_cat_index(i,mosaic_i,j) == iswater ) THEN
	                   Temp4=LANDUSEF2(i,mosaic_i,j)
	                   Temp5=mosaic_cat_index(i,mosaic_i,j)
	   
	                   LANDUSEF2(i,mosaic_i:NLCAT-1,j)=LANDUSEF2(i,mosaic_i+1:NLCAT,j)                       
	                   mosaic_cat_index(i,mosaic_i:NLCAT-1,j)=mosaic_cat_index(i,mosaic_i+1:NLCAT,j)
	  
	                   LANDUSEF2(i,NLCAT,j)=Temp4
	                   mosaic_cat_index(i,NLCAT,j)=Temp5
	                ENDIF 
	              ENDDO
             
           ENDIF 
             
          ENDIF  
          
      ELSE  
      
                 ICE = 0
      
                     IF( XICE(I,J).GE. XICE_THRESHOLD ) THEN
                       WRITE (message_text,fmt='(a,2I6)') 'sea-ice at water point, I and J = ', i,j
                       CALL wrf_message(message_text)
                       ICE = 1
                     ENDIF  
      
           IF ((mosaic_cat_index(I,1,J) .NE. ISWATER)) THEN    
                
                
      	             
                 IF (IVGTYP(i,j) .NE. ISWATER) THEN  
                 
                 
                 CALL wrf_message("IN MOSAIC_INIT")
                 WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j)
                 CALL wrf_message(message_text)
                 CALL wrf_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "in addition, the dominant land use category based on the geogrid calculation is not water,"//  &
                      "this must be wrong.")
                 ENDIF  
                 
                 IF (IVGTYP(i,j) .EQ. ISWATER) THEN 
                 
                 

                 CALL wrf_message("IN MOSAIC_INIT")
                 WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j)
                 CALL wrf_message(message_text)
                 CALL wrf_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "however, the dominant land use category based on the geogrid calculation is water")
                 CALL wrf_message("This is fine. We do not need to do anyting because in the noaddrv, "//&
                      "we use xland as a criterion for whether using"// &
                      "mosaic or not when xland > 1.5, no mosaic will be used anyway")
      	             
                 ENDIF  
                
           ENDIF 
      
        ENDIF  

          ENDDO
      ENDDO
      
  
  
  

     DO i = its,itf
        DO j = jts,jtf

          Temp6=0

            DO mosaic_i=1,mosaic_cat
               Temp6=Temp6+LANDUSEF2(i,mosaic_i,j)
            ENDDO
            
            if (Temp6 .LT. 1e-5)  then
            
            Temp6 = 1e-5
            WRITE (message_text,fmt='(a,e8.1)') 'the total land surface fraction is less than ', temp6
            CALL wrf_message(message_text)
            WRITE (message_text,fmt='(a,2I6,4F8.2)') 'some landusef values at i,j are ', &
                 i,j,landusef2(i,1,j),landusef2(i,2,j),landusef2(i,3,j),landusef2(i,4,j)
            CALL wrf_message(message_text)
            WRITE (message_text,fmt='(a,2I6,3I6)') 'some mosaic cat values at i,j are ', &
                 i,j,mosaic_cat_index(i,1,j),mosaic_cat_index(i,2,j),mosaic_cat_index(i,3,j) 
            CALL wrf_message(message_text)
            
            endif

            LANDUSEF2(i,1:mosaic_cat, j)=LANDUSEF2(i,1:mosaic_cat,j)*(1/Temp6)

          ENDDO
      ENDDO
      
  
  
  

     DO i = its,itf
        DO j = jts,jtf
    
             DO mosaic_i=1,mosaic_cat
        
            TSK_mosaic(i,mosaic_i,j)=TSK(i,j)          
            CANWAT_mosaic(i,mosaic_i,j)=CANWAT(i,j) 
            SNOW_mosaic(i,mosaic_i,j)=SNOW(i,j) 
            SNOWH_mosaic(i,mosaic_i,j)=SNOWH(i,j)  
            SNOWC_mosaic(i,mosaic_i,j)=SNOWC(i,j) 

            ALBEDO_mosaic(i,mosaic_i,j)=ALBEDO(i,j)
            ALBBCK_mosaic(i,mosaic_i,j)=ALBBCK(i,j)
            EMISS_mosaic(i,mosaic_i,j)=EMISS(i,j) 
            EMBCK_mosaic(i,mosaic_i,j)=EMBCK(i,j) 
            ZNT_mosaic(i,mosaic_i,j)=Z0(i,j)
            Z0_mosaic(i,mosaic_i,j)=Z0(i,j)
  
              DO soil_k=1,num_soil_layers 
  
            TSLB_mosaic(i,num_soil_layers*(mosaic_i-1)+soil_k,j)=TSLB(i,soil_k,j)
            SMOIS_mosaic(i,num_soil_layers*(mosaic_i-1)+soil_k,j)=SMOIS(i,soil_k,j)
            SH2O_mosaic(i,num_soil_layers*(mosaic_i-1)+soil_k,j)=SH2O(i,soil_k,j)  
          
              ENDDO
           
           TR_URB2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           TB_URB2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           TG_URB2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           TC_URB2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           TS_URB2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           TS_RUL2D_mosaic(i,mosaic_i,j)=TSK(i,j)   
           QC_URB2D_mosaic(i,mosaic_i,j)=0.01
           SH_URB2D_mosaic(i,mosaic_i,j)=0
           LH_URB2D_mosaic(i,mosaic_i,j)=0
           G_URB2D_mosaic(i,mosaic_i,j)=0
           RN_URB2D_mosaic(i,mosaic_i,j)=0
          
          TRL_URB3D_mosaic(I,4*(mosaic_i-1)+1,J)=TSLB(I,1,J)+0.
          TRL_URB3D_mosaic(I,4*(mosaic_i-1)+2,J)=0.5*(TSLB(I,1,J)+TSLB(I,2,J))
          TRL_URB3D_mosaic(I,4*(mosaic_i-1)+3,J)=TSLB(I,2,J)+0.
          TRL_URB3D_mosaic(I,4*(mosaic_i-1)+4,J)=TSLB(I,2,J)+(TSLB(I,3,J)-TSLB(I,2,J))*0.29

          TBL_URB3D_mosaic(I,4*(mosaic_i-1)+1,J)=TSLB(I,1,J)+0.
          TBL_URB3D_mosaic(I,4*(mosaic_i-1)+2,J)=0.5*(TSLB(I,1,J)+TSLB(I,2,J))
          TBL_URB3D_mosaic(I,4*(mosaic_i-1)+3,J)=TSLB(I,2,J)+0.
          TBL_URB3D_mosaic(I,4*(mosaic_i-1)+4,J)=TSLB(I,2,J)+(TSLB(I,3,J)-TSLB(I,2,J))*0.29           
                    
          TGL_URB3D_mosaic(I,4*(mosaic_i-1)+1,J)=TSLB(I,1,J)
          TGL_URB3D_mosaic(I,4*(mosaic_i-1)+2,J)=TSLB(I,2,J)
          TGL_URB3D_mosaic(I,4*(mosaic_i-1)+3,J)=TSLB(I,3,J)
          TGL_URB3D_mosaic(I,4*(mosaic_i-1)+4,J)=TSLB(I,4,J)
           
            ENDDO
          ENDDO
      ENDDO

   
   
       DO i = its,itf
        DO j = jts,jtf 
   
           IF ((xland(i,j).LT. 1.5 ) .AND. (mosaic_cat_index(I,1,J) .EQ. ISWATER)) THEN
             CALL wrf_message("After MOSAIC_INIT")
             WRITE (message_text,fmt='(a,2I6,2F8.2,2I6)') 'weird xland,xice,mosaic_cat_index and ivgtyp at I,J = ', &
                i,j,xland(i,j),xice(i,j),mosaic_cat_index(I,1,J),IVGTYP(i,j)
             CALL wrf_message(message_text)
           ENDIF
           
        ENDDO
      ENDDO
      
 ENDIF      
      

  END SUBROUTINE lsm_mosaic_init  


END MODULE module_sf_noahdrv
