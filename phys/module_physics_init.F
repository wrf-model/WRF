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

CONTAINS


!=================================================================
   SUBROUTINE phy_init ( id, config_flags, DT, restart, zfull, zhalf,     &
                         p_top, TSK,RADT,BLDT,CUDT,MPDT,         &
                         RTHCUTEN, RQVCUTEN, RQRCUTEN,           &
                         RQCCUTEN, RQSCUTEN, RQICUTEN,           &
                         RUBLTEN,RVBLTEN,RTHBLTEN,               &
                         RQVBLTEN,RQCBLTEN,RQIBLTEN,             &
                         RTHRATEN,RTHRATENLW,RTHRATENSW,         &
                         STEPBL,STEPRA,STEPCU,                   &
                         W0AVG, RAINNC, RAINC, RAINCV, RAINNCV,  &
                         NCA,swrad_scat,                         &
                         CLDEFI,LOWLYR,                          &
                         MASS_FLUX,                              &
                         RTHFTEN, RQVFTEN,                       &
                         CLDFRA,CLDFRA_OLD,GLW,GSW,EMISS,EMBCK,  & !EMBCK new
                         LU_INDEX,                               &
                         landuse_ISICE, landuse_LUCATS,          &
                         landuse_LUSEAS, landuse_ISN,            &
                         lu_state,                               &
                         XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         levsiz, n_ozmixm, n_aerosolc, paerlev,  &
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_MYJ,  &
#if (NMM_CORE != 1)
                         TKE_PBL,  &
#endif
#if (NMM_CORE == 1)
                         RUCUTEN, RVCUTEN,  &
#endif
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  &
                         adv_moist_cond,                         &
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
                         NUM_URBAN_LAYERS,                       &
                         ozmixm,pin,                             &    ! Optional
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,& ! Optional
                         RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,         &    ! Optional
                         RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,       &    ! Optional
                         FGDT,STEPFG,                            &    ! Optional
                         cugd_tten,cugd_ttens,cugd_qvten,        &    ! Optional
                         cugd_qvtens,cugd_qcten,                 &    ! Optional
!                        num_roof_layers,num_wall_layers,        & !Optional urban
!                        num_road_layers,                        & !Optional urban
                         DZR, DZB, DZG,                          & !Optional urban
                         TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,    & !Optional urban
                         QC_URB2D, XXXR_URB2D,XXXB_URB2D,        & !Optional urban
                         XXXG_URB2D, XXXC_URB2D,                 & !Optional urban
                         TRL_URB3D, TBL_URB3D, TGL_URB3D,        & !Optional urban
                         SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,  & !Optional urban
                         TS_URB2D, FRC_URB2D, UTYPE_URB2D,       & !Optional urban
                         TRB_URB4D,TW1_URB4D,TW2_URB4D,          & !Optional multi-layer urban
                         TGB_URB4D,TLEV_URB3D,QLEV_URB3D,        & !Optional multi-layer urban
                         TW1LEV_URB3D,TW2LEV_URB3D,              & !Optional multi-layer urban
                         TGLEV_URB3D,TFLEV_URB3D,                & !Optional multi-layer urban
                         SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,    & !Optional multi-layer urban
                         SFVENT_URB3D,LFVENT_URB3D,              & !Optional multi-layer urban
                         SFWIN1_URB3D,SFWIN2_URB3D,              & !Optional multi-layer urban
                         SFW1_URB3D,SFW2_URB3D,                  & !Optional multi-layer urban
                         SFR_URB3D,SFG_URB3D,                    & !Optional multi-layer urban
                         A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,        & !Optional multi-layer urban
                         A_E_BEP,B_U_BEP,B_V_BEP,                & !Optional multi-layer urban
                         B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,        & !Optional multi-layer urban
                         DL_U_BEP,SF_BEP,VL_BEP,                 & !Optional multi-layer urban
                         TML,T0ML,HML,H0ML,HUML,HVML,TMOML,      & !Optional oml
                         itimestep,                              & !Optional obs fdda
#if ( EM_CORE == 1 )
                         fdob,                                   & !Optional obs fdda
#endif
                         t00, p00, tlp,                          & !for obs-nudging
                         TYR,TYRA,TDLY,TLAG,NYEAR,NDAY,tmn_update,   &
                         ACHFX,ACLHF,ACGRDFLX                   &
                         )

!-----------------------------------------------------------------
   USE module_domain
   USE module_wrf_error
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   INTEGER , INTENT(IN) ,OPTIONAL       :: tmn_update
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
!   LOGICAL , INTENT (IN)       :: FNDSOILW, FNDSNOWH
   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers
   INTEGER , INTENT(IN)        :: lagday
   INTEGER , INTENT(OUT) ,OPTIONAL      :: nyear, nday

   LOGICAL,  INTENT(IN)        :: start_of_simulation
   REAL,     INTENT(IN)        :: DT, p_top, DX, DY
   LOGICAL,  INTENT(IN)        :: restart
   REAL,     INTENT(IN)        :: RADT,BLDT,CUDT,MPDT
   REAL,     INTENT(IN)        :: swrad_scat

   REAL,     DIMENSION( kms:kme ) , INTENT(IN) :: zfull, zhalf
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: TSK, XLAT, XLONG

   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL, &
          INTENT(INOUT) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::        PIN

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

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            OPTIONAL, INTENT(INOUT)    ::                ACHFX, &
                                                         ACLHF, &
                                                      ACGRDFLX

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

! rad

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) :: &
             CLDFRA_OLD

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS,EMBCK                          !EMBCK new
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::   SNOALB


   REAL,     INTENT(IN) :: GMT

   INTEGER , INTENT(OUT) :: STEPRA, STEPBL, STEPCU
   INTEGER , INTENT(IN) :: JULYR, JULDAY

! cps

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHCUTEN, RQVCUTEN, RQRCUTEN, RQCCUTEN, RQSCUTEN,   &
             RQICUTEN

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: MASS_FLUX,   &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHFTEN, RQVFTEN

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) ::           &
             RAINNC, RAINC, RAINCV, RAINNCV

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: CLDEFI, NCA

   INTEGER,  DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: LOWLYR

!pbl

   ! soil layer


   REAL,     DIMENSION(1:num_soil_layers),      INTENT(INOUT) :: ZS,DZS

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN,RQIBLTEN,EXCH_H,TKE_MYJ
#if (NMM_CORE != 1)
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    TKE_PBL
#endif 
#if (NMM_CORE == 1)
   REAL,     DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(INOUT) :: &
             RUCUTEN, RVCUTEN
#endif
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

!mp
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved

! ocean mixed layer
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
             TML,T0ML,HML,H0ML,HUML,HVML,TMOML

!fdda
   REAL,     OPTIONAL, INTENT(IN) :: FGDT
   INTEGER , OPTIONAL, INTENT(OUT) :: STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, RPHNDGDTEN, RQVNDGDTEN
   REAL,     DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
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

!   REAL, DIMENSION(ims:ime, 1:num_roof_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D !urban
!   REAL, DIMENSION(ims:ime, 1:num_wall_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D !urban
!   REAL, DIMENSION(ims:ime, 1:num_road_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TRL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TBL_URB3D  !urban
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_soil_layers, jms:jme), INTENT(INOUT) :: TGL_URB3D  !urban

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SH_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LH_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: G_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: RN_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D !urban
   INTEGER, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: UTYPE_URB2D !urban

   INTEGER , INTENT(IN)        :: num_urban_layers
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TRB_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW1_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TW2_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TGB_URB4D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: TLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: QLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D !multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFG_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFR_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW1_URB3D ! multi-layer UCM
   REAL, OPTIONAL, DIMENSION(ims:ime, 1:num_urban_layers, jms:jme), INTENT(INOUT) :: SFW2_URB3D ! multi-layer UCM
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

!obs fdda
   INTEGER, OPTIONAL, INTENT(IN) :: itimestep
#if ( EM_CORE == 1 )
   TYPE(fdob_type), OPTIONAL, INTENT(INOUT) :: fdob
#endif
   REAL, OPTIONAL, INTENT(IN) :: p00, t00, tlp   ! for obs-nudging base-state calcn

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
   INTEGER :: omlcall
   REAL    :: oml_hml0
   LOGICAL :: usemonalb
   LOGICAL :: rdmaxalb

   INTEGER :: i, j, k, itf, jtf, n
integer myproc

!-----------------------------------------------------------------

   sf_urban_physics=config_flags%sf_urban_physics
   usemonalb=config_flags%usemonalb
   rdmaxalb=config_flags%rdmaxalb
#if ( EM_CORE == 1 )
   obs_twindo_cg=model_config_rec%obs_twindo(1)
   obs_twindo=config_flags%obs_twindo
   oml_hml0=config_flags%oml_hml0
   omlcall=config_flags%omlcall
#endif

!-- should be from the namelist

   sfull = 0.
   shalf = 0.

   CALL wrf_debug(100,'top of phy_init')

   WRITE(wrf_err_message,*) 'phy_init:  start_of_simulation = ',start_of_simulation
   CALL wrf_debug ( 100, TRIM(wrf_err_message) )

   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

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
   CALL nl_get_mminlu( 1, mminlu_loc )
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_isice, nl_get_mminlu_loc')
!-- temporary fix by ww
   landuse_ISICE = isice

  IF(.not.restart)THEN
!-- initialize common variables

   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=1.
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
      ACSNOW(i,j)=0.
      DO k=kms,kme  !wig, 17-May-2006: Added for idealized chem. runs
         EXCH_H(i,k,j) = 0.
      END DO
   ENDDO
   ENDDO
   ENDIF
!
   IF(PRESENT(TMN_UPDATE))THEN
   if(tmn_update.eq.1) then
   nyear=1
   nday=1
   DO j=jts,jtf
   DO i=its,itf
      TYR(i,j)=TMN(i,j)
      TYRA(i,j)=TMN(i,j)
      TDLY(i,j)=TMN(i,j)
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

  ENDIF

!-- convert zfull and zhalf to sigma values for ra_init (Eta CO2 needs these)
!-- zfull/zhalf may be either zeta or eta
!-- what is done here depends on coordinate (check this code if adding new coordinates)
   CALL z2sigma(zfull,zhalf,sfull,shalf,p_top,pptop,config_flags, &
                allowed_to_read,                                  &
                kds,kde,kms,kme,kts,kte)

!-- initialize physics
!-- ra: radiation
!-- bl: pbl
!-- cu: cumulus
!-- mp: microphysics

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to ra_init' )

   CALL ra_init(id=id,STEPRA=STEPRA,RADT=RADT,DT=DT,RTHRATEN=RTHRATEN,RTHRATENLW=RTHRATENLW,             &
                RTHRATENSW=RTHRATENSW,CLDFRA=CLDFRA,EMISS=EMISS,cen_lat=cen_lat,JULYR=JULYR,JULDAY=JULDAY,GMT=GMT,    &
                levsiz=levsiz,XLAT=XLAT,n_ozmixm=n_ozmixm,                           &
                cldfra_old=cldfra_old,                                     & ! Optional
                ozmixm=ozmixm,pin=pin,                                     & ! Optional
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
                num_soil_layers,TKE_MYJ,                        &
#if (NMM_CORE != 1)
                TKE_PBL,                                     &
#endif
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,MAVAIL,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
#if (NMM_CORE == 1)
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                TRIM (MMINLU_LOC),                              &
                allowed_to_read ,                               &
                start_of_simulation ,                           &
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,          & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               & 
                SF_URBAN_PHYSICS,                               & !Optional urban
                NUM_URBAN_LAYERS,                               & !Optional multi-layer urban
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & !Optional multi-layer urban
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & !Optional multi-layer urban
                TW1LEV_URB3D,TW2LEV_URB3D,                      & !Optional multi-layer urban
                TGLEV_URB3D,TFLEV_URB3D,                        & !Optional multi-layer urban
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & !Optional multi-layer urban
                SFVENT_URB3D,LFVENT_URB3D,                      & !Optional multi-layer urban
                SFWIN1_URB3D,SFWIN2_URB3D,                      & !Optional multi-layer urban
                SFW1_URB3D,SFW2_URB3D,                          & !Optional multi-layer urban
                SFR_URB3D,SFG_URB3D,                            & !Optional multi-layer urban
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & !Optional multi-layer urban
                A_E_BEP,B_U_BEP,B_V_BEP,                        & !Optional multi-layer urban
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & !Optional multi-layer urban
                DL_U_BEP,SF_BEP,VL_BEP,                         & !Optional multi-layer urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, omlcall,                              & !Optional oml
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML               ) !Optional oml

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,      &
                RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,           &
#if (NMM_CORE == 1)
                         RUCUTEN, RVCUTEN,  &
#endif
                RAINCV,W0AVG,config_flags,restart,              &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
                cugd_tten,cugd_ttens,cugd_qvten,                &
                cugd_qvtens,cugd_qcten,                         &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to mp_init' )

   CALL mp_init(RAINNC,config_flags,restart,warm_rain,          &
                adv_moist_cond,                                 &
                MPDT, DT, DX, DY, LOWLYR,                       &
                F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,               &
                mp_restart_state,tbpvs_state,tbpvs0_state,      &
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

#if  ( EM_CORE == 1 )
   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fg_init' )

   CALL fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,          &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
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
   CHARACTER*80 :: message
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
            PRINT *, 'LANDUSE TYPE = "' // TRIM (LUTYPE) // '" FOUND',        &
                   LUCATS,' CATEGORIES',LUSEAS,' SEASONS',     &
                   ' WATER CATEGORY = ',ISWATER,               &
                   ' SNOW CATEGORY = ',ISICE
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
          IF(IS.LT.0.OR.IS.GT.LUN.AND.allowed_to_read)THEN
            WRITE ( wrf_err_message , * ) 'ERROR: LANDUSE OUTSIDE RANGE =',IS,' AT ',I,J,' LUN= ',LUN
            CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
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


   END SUBROUTINE landuse_init

!=====================================================================
   SUBROUTINE ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       &
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      levsiz,XLAT,n_ozmixm,                           &
                      cldfra_old,                                     & ! Optional
                      ozmixm,pin,                                     & ! Optional
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
   USE module_ra_cam       , ONLY : camradinit
   USE module_ra_sw        , ONLY : swinit
   USE module_ra_gsfcsw    , ONLY : gsfc_swinit
   USE module_ra_gfdleta   , ONLY : gfdletainit
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

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  XLAT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ), OPTIONAL,      &
          INTENT(INOUT) ::                                  OZMIXM

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(INOUT)  :: m_ps_1,m_ps_2
   REAL,  DIMENSION(paerlev), OPTIONAL, INTENT(INOUT)  ::         m_hybi
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL,     &
          INTENT(INOUT) ::                      aerosolc_1, aerosolc_2

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(INOUT)  ::          PIN

   INTEGER , INTENT(INOUT)        :: STEPRA
   INTEGER :: isn

   REAL , DIMENSION( kms:kme ) , INTENT(IN) :: sfull, shalf
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::           &
                                                           RTHRATEN, &
                                                         RTHRATENLW, &
                                                         RTHRATENSW, &
                                                             CLDFRA

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) :: &
                                                         CLDFRA_OLD

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   LOGICAL :: hwrflw= .false.
   LOGICAL :: camlw = .false.
   LOGICAL :: etamp = .false.
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

!-- find out which microphysics option is used first

   mp_select: SELECT CASE(config_flags%mp_physics)

        CASE (ETAMPNEW)
             etamp = .true.

   END SELECT mp_select

!-- chose long wave radiation scheme

   lwrad_select: SELECT CASE(config_flags%ra_lw_physics)

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
                           allowed_to_read ,                &
                           ids, ide, jds, jde, kds, kde,    &
                           ims, ime, jms, jme, kms, kme,    &
                           its, ite, jts, jte, kts, kte     )

             aclwalloc = .true.

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
                           allowed_to_read ,                         &
                           kds, kde, kms, kme, kts, kte     )
             hwrflw = .true.
#endif
        CASE (HELDSUAREZ)
             CALL hsinit(RTHRATEN,restart,             &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte )
        CASE DEFAULT

   END SELECT lwrad_select
!-- initialize short wave radiation scheme

   swrad_select: SELECT CASE(config_flags%ra_sw_physics)

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
                           allowed_to_read,                          &
                           kds, kde, kms, kme, kts, kte     )
             ENDIF
#endif
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
                num_soil_layers,TKE_MYJ,                        &
#if (NMM_CORE != 1)
                TKE_PBL,                                     &
#endif
                EXCH_H,VEGFRA,                                  &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,ISURBAN,SMOIS,SMFR3D,mavail,      &
                SNOWH,SH2O,SNOALB,FNDSOILW,FNDSNOWH,RDMAXALB,   &
#if  ( NMM_CORE == 1 )
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                MMINLU,                                         &
                allowed_to_read,                                &
                start_of_simulation,                            &
!                num_roof_layers,num_wall_layers,num_road_layers,& !Optional urban
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,             & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,               &
                SF_URBAN_PHYSICS,                               & !Optional urban
                NUM_URBAN_LAYERS,                               & !Optional multi-layer urban
                TRB_URB4D,TW1_URB4D,TW2_URB4D,                  & !Optional multi-layer urban
                TGB_URB4D,TLEV_URB3D,QLEV_URB3D,                & !Optional multi-layer urban
                TW1LEV_URB3D,TW2LEV_URB3D,                      & !Optional multi-layer urban
                TGLEV_URB3D,TFLEV_URB3D,                        & !Optional multi-layer urban
                SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,            & !Optional multi-layer urban
                SFVENT_URB3D,LFVENT_URB3D,                      & !Optional multi-layer urban
                SFWIN1_URB3D,SFWIN2_URB3D,                      & !Optional multi-layer urban
                SFW1_URB3D,SFW2_URB3D,                          & !Optional multi-layer urban
                SFR_URB3D,SFG_URB3D,                            & !Optional multi-layer urban
                A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                & !Optional multi-layer urban
                A_E_BEP,B_U_BEP,B_V_BEP,                        & !Optional multi-layer urban
                B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                & !Optional multi-layer urban
                DL_U_BEP,SF_BEP,VL_BEP,                         & !Optional multi-layer urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte,                   &
                ACHFX,ACLHF,ACGRDFLX,                           &
                oml_hml0, omlcall,                              & !Optional oml
                TML,T0ML,HML,H0ML,HUML,HVML,TMOML               ) !Optional oml
!--------------------------------------------------------------------
   USE module_sf_sfclay
   USE module_sf_slab
   USE module_sf_pxsfclay
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_bl_acm
   USE module_sf_myjsfc
   USE module_sf_qnsesfc
   USE module_sf_noahdrv
   USE module_sf_urban
   USE module_sf_bep                                  !BEP
   USE module_sf_bep_bem
   USE module_sf_ruclsm
   USE module_sf_pxlsm
   USE module_sf_oml
   USE module_bl_myjpbl
   USE module_bl_myjurb
   USE module_bl_boulac
   USE module_bl_qnsepbl
#if ( EM_CORE == 1 )
   USE module_bl_mynn
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

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     SF_URBAN_PHYSICS 

   REAL ,    INTENT(IN)        ::     DT, BLDT
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

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                           RUBLTEN, &
                                                           RVBLTEN, &
 						          EXCH_H,   &
                                                          RTHBLTEN, &
                                                          RQVBLTEN, &
                                                          RQCBLTEN, &
                                                          RQIBLTEN, &
                                                          TKE_MYJ
#if (NMM_CORE != 1)
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                          TKE_PBL
#endif
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   CHARACTER(LEN=*), INTENT(IN)   :: MMINLU
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER,  INTENT(IN)           :: ISURBAN
   INTEGER :: isn, isfc
   INTEGER :: k

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

    INTEGER , INTENT(IN)        ::     num_urban_layers
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TRB_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGB_URB4D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TLEV_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: QLEV_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW1LEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TW2LEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TGLEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: TFLEV_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LF_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SF_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CM_AC_URB3D  !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: SFVENT_URB3D !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LFVENT_URB3D !multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN1_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFWIN2_URB3D ! multi-layer UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW1_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFW2_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFR_URB3D !Optional UCM
    REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: SFG_URB3D !Optional UCM
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

   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        ACHFX,ACLHF,ACGRDFLX
! Optional OML variables
   REAL,  DIMENSION( ims:ime , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                                        TML,T0ML,HML,H0ML,HUML,HVML,TMOML
   INTEGER,  OPTIONAL,  INTENT(IN) :: omlcall
   REAL,  OPTIONAL,  INTENT(IN) :: oml_hml0
   LOGICAL,  INTENT(IN) :: start_of_simulation
   INTEGER :: i,j


#if ( EM_CORE == 1 )
!local mynn 
   INTEGER :: mynn_closure_level
#endif

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

!-- initialize surface layer scheme

   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
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
                              restart,sf_urban_physics,                        & !urban
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & !urban
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & !urban
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & !urban
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & !urban
                              num_urban_layers,                                & !urban
                              TRB_URB4D,TW1_URB4D,TW2_URB4D,TGB_URB4D,         & !urban
                              TLEV_URB3D,QLEV_URB3D,                           & !urban
                              TW1LEV_URB3D,TW2LEV_URB3D,                       & !urban
                              TGLEV_URB3D,TFLEV_URB3D,                         & !urban
                              SF_AC_URB3D,LF_AC_URB3D,CM_AC_URB3D,             & !urban
                              SFVENT_URB3D,LFVENT_URB3D,                       & !urban
                              SFWIN1_URB3D,SFWIN2_URB3D,                       & !urban
                              SFW1_URB3D,SFW2_URB3D,SFR_URB3D,SFG_URB3D,       & !urban
                              A_U_BEP,A_V_BEP,A_T_BEP,A_Q_BEP,                 & !multi-layer urban
                              A_E_BEP,B_U_BEP,B_V_BEP,                         & !multi-layer urban
                              B_T_BEP,B_Q_BEP,B_E_BEP,DLG_BEP,                 & !multi-layer urban
                              DL_U_BEP,SF_BEP,VL_BEP,                          & !multi-layer urban
                              FRC_URB2D, UTYPE_URB2D)                            !urban
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling urban model' )
             ENDIF
          ENDIF
!
      CASE (RUCLSMSCHEME)
!          if(isfc .ne. 2)CALL wrf_error_fatal &
!           ( 'module_physics_init: use myjsfc and myjpbl scheme for this lsm option' )
           CALL ruclsminit( SH2O,SMFR3D,TSLB,SMOIS,ISLTYP,IVGTYP,XICE,  &
                     mavail,num_soil_layers, config_flags%iswater,      &
                     config_flags%isice, restart,                  &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE (PXLSMSCHEME)
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

      CASE DEFAULT

   END SELECT sfc_select

   IF(PRESENT(OMLCALL))THEN
     IF (omlcall .EQ. 1) THEN
        CALL omlinit(oml_hml0, tsk,                           &
                     tml,t0ml,hml,h0ml,huml,hvml,tmoml,       &
                     allowed_to_read, start_of_simulation,    &
                     ids,ide, jds,jde, kds,kde,               &
                     ims,ime, jms,jme, kms,kme,               &
                     its,ite, jts,jte, kts,kte                )
     ENDIF
   ENDIF

!-- initialize pbl scheme

   pbl_select: SELECT CASE(config_flags%bl_pbl_physics)

      CASE (YSUSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
           CALL ysuinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MRFSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
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
           CALL gfsinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,    &
                        RQCBLTEN,RQIBLTEN,P_QI,               &
                        PARAM_FIRST_SCALAR,                   &
                        restart,                              &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE (MYJPBLSCHEME)
           if(isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use myjsfc scheme for this pbl option' )
          IF ((SF_URBAN_PHYSICS.eq.2).OR.(SF_URBAN_PHYSICS.EQ.3)) THEN
           CALL myjurbinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          ELSE

           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
          END IF
      CASE (QNSEPBLSCHEME)
           if(isfc .ne. 4)CALL wrf_error_fatal &
            ( 'module_physics_init: use qnsesfc scheme for this pbl option' )
           CALL qnsepblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
#if (NMM_CORE != 1)
      CASE (BOULACSCHEME)
           if(isfc .ne. 1 .and. isfc .ne. 2)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay or myjsfc scheme for this pbl option' )
           CALL boulacinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                         TKE_PBL,EXCH_H,restart,               &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
#endif

#if ( EM_CORE == 1 )

!mynn 
           
        CASE (MYNNPBLSCHEME2, MYNNPBLSCHEME3)
           IF(isfc .NE. 5 .AND. isfc .NE. 1 .AND. isfc .NE. 2) CALL wrf_error_fatal &
                ( 'module_physics_init: use mynnsfc or sfclay or myjsfc scheme for this pbl option')
           
           SELECT CASE(config_flags%bl_pbl_physics)

             CASE(MYNNPBLSCHEME2)
                mynn_closure_level=2

             CASE(MYNNPBLSCHEME3)
                mynn_closure_level=3

             CASE DEFAULT

           END SELECT

           CALL mynn_bl_init_driver(&
                &RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN,RQCBLTEN&
                &,restart,allowed_to_read,mynn_closure_level &
                &,IDS,IDE,JDS,JDE,KDS,KDE                    &
                &,IMS,IME,JMS,JME,KMS,KME                    &
                &,ITS,ITE,JTS,JTE,KTS,KTE)

#endif

      CASE DEFAULT

   END SELECT pbl_select


   END SUBROUTINE bl_init

!==================================================================
   SUBROUTINE cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,  &
                      RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,       &
#if (NMM_CORE == 1)
                         RUCUTEN, RVCUTEN,  &
#endif
                      RAINCV,W0AVG,config_flags,restart,          &
                      CLDEFI,LOWLYR,MASS_FLUX,                    &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      cugd_tten,cugd_ttens,cugd_qvten,            &
                      cugd_qvtens,cugd_qcten,                     &
                      allowed_to_read, start_of_simulation,       &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_cu_kf
   USE module_cu_kfeta
   USE MODULE_CU_BMJ
   USE module_cu_gd,  ONLY : GDINIT
   USE module_cu_g3,  ONLY : G3INIT
   USE module_cu_sas
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart


   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   REAL ,    INTENT(IN)        :: DT, CUDT
   LOGICAL , INTENT(IN)        :: start_of_simulation
   LOGICAL , INTENT(IN)        :: allowed_to_read
   INTEGER , INTENT(INOUT)     :: STEPCU

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) ::    &
            RTHCUTEN, RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN, RQSCUTEN
   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(INOUT) ::    &
                        cugd_tten,cugd_ttens,cugd_qvten,            &
                        cugd_qvtens,cugd_qcten

   REAL ,   DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: W0AVG

   REAL,    DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
            RTHFTEN, RQVFTEN

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: RAINC, RAINCV

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: CLDEFI

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: NCA

   REAL ,   DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: MASS_FLUX,   &
                                   APR_GR,APR_W,APR_MC,APR_ST,APR_AS,    &
                                   APR_CAPMA,APR_CAPME,APR_CAPMI

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: LOWLYR
#if (NMM_CORE == 1)
   REAL,     DIMENSION(IMS:IME,JMS:JME,KMS:KME), INTENT(INOUT) :: &
             RUCUTEN, RVCUTEN
#endif

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
#if ( EM_CORE == 1 )
     CASE (G3SCHEME)
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
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
#if (NMM_CORE == 1)
                      RUCUTEN,RVCUTEN,                            &   ! gopal's doing for SAS
#endif
                      restart,P_QC,P_QI,PARAM_FIRST_SCALAR,       &
                      allowed_to_read ,                           &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

     CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cu_init

!==================================================================
   SUBROUTINE mp_init(RAINNC,config_flags,restart,warm_rain,      &
                      adv_moist_cond,                             &
                      MPDT, DT, DX, DY, LOWLYR,                   & ! for eta mp
                      F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,           & ! for eta mp
                      mp_restart_state,tbpvs_state,tbpvs0_state,   & ! eta mp
                      allowed_to_read, start_of_simulation,       &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
#if (NMM_CORE == 1)
   USE module_mp_HWRF
#endif
   USE module_mp_thompson
   USE module_mp_thompson07
   USE module_mp_morr_two_moment
   USE module_mp_milbrandt2mom
!  USE module_mp_milbrandt3mom
   USE module_mp_wdm5
   USE module_mp_wdm6
!------------------------------------------------------------------
   IMPLICIT NONE
!------------------------------------------------------------------
! Arguments
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
   REAL    , INTENT(IN)        :: MPDT, DT, DX, DY
   LOGICAL , INTENT(IN)        :: start_of_simulation

   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,   &
                                  ims, ime, jms, jme, kms, kme,   &
                                  its, ite, jts, jte, kts, kte

   INTEGER , DIMENSION( ims:ime , jms:jme ) ,INTENT(INOUT)  :: LOWLYR
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: RAINNC
   REAL,     DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) :: &
                                  F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL , DIMENSION(:) ,INTENT(INOUT)  :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL , INTENT(IN)  :: allowed_to_read

! Local
   INTEGER :: i, j, itf, jtf

   warm_rain = .false.
   adv_moist_cond = .true.
   itf=min0(ite,ide-1)
   jtf=min0(jte,jde-1)

   IF(start_of_simulation)THEN
     DO j=jts,jtf
     DO i=its,itf
        RAINNC(i,j) = 0.
     ENDDO
     ENDDO
   ENDIF

   mp_select: SELECT CASE(config_flags%mp_physics)

     CASE (KESSLERSCHEME)
          warm_rain = .true.
     CASE (WSM3SCHEME)
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cpv, allowed_to_read )
     CASE (ETAMPNEW)
         adv_moist_cond = .false.
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
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
! Cycling the WRF forecast with moving nests will cause this initialization to be
! called for each nest move. This is potentially very computationally expensive.
         IF(start_of_simulation.or.restart.or.config_flags%cycling)CALL thompson_init

     CASE (THOMPSON07)
         IF(start_of_simulation.or.restart.or.config_flags%cycling)CALL thompson07_init

     CASE (MORR_TWO_MOMENT)
         CALL morr_two_moment_init
     CASE (MILBRANDT2MOM)
         CALL milbrandt2mom_init
!      CASE (MILBRANDT3MOM)
!          CALL milbrandt3mom_init
     CASE (WDM5SCHEME)
          CALL wdm5init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )
     CASE (WDM6SCHEME)
          CALL wdm6init(rhoair0,rhowater,rhosnow,cliq,cpv,n_ccn0,allowed_to_read )

     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init

#if  ( EM_CORE == 1 )
!==========================================================
   SUBROUTINE fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,    &
                RTHNDGDTEN,RPHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,    &
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
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::       &
                                                           RUNDGDTEN, &
                                                           RVNDGDTEN, &
                                                          RTHNDGDTEN, &
                                                          RPHNDGDTEN, &
                                                          RQVNDGDTEN
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT) :: RMUNDGDTEN

   LOGICAL,  INTENT(IN)           :: allowed_to_read
!--------------------------------------------------------------------

!-- calculate pbl time step

   STEPFG = nint(FGDT*60./DT)
   STEPFG = max(STEPFG,1)


!-- initialize fdda scheme

   fdda_select: SELECT CASE(config_flags%grid_fdda)

      CASE (PSUFDDAGD)
           CALL fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
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
           CALL fddaspnudginginit(id,rundgdten,rvndgdten,rthndgdten,rphndgdten,&
               config_flags%run_hours, &
               config_flags%if_no_pbl_nudging_uv, &
               config_flags%if_no_pbl_nudging_t, &
               config_flags%if_no_pbl_nudging_ph, &
               config_flags%if_zfac_uv, &
               config_flags%k_zfac_uv, &
               config_flags%dk_zfac_uv, &
               config_flags%if_zfac_t, &
               config_flags%k_zfac_t, &
               config_flags%dk_zfac_t, &
               config_flags%if_zfac_ph, &
               config_flags%k_zfac_ph, &
               config_flags%dk_zfac_ph, &
               config_flags%guv, &
               config_flags%gt, config_flags%gph, &
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

END MODULE module_physics_init
