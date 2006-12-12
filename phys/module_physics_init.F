!WRF:MODEL_LAYER:INITIALIZATION
!

!  This MODULE holds the routines which are used to perform model start-up operations
!  for the individual domains.  This is the stage after inputting wrfinput and before
!  calling 'integrate'.

!  This MODULE CONTAINS the following routines:


MODULE module_physics_init

!  USE module_io_domain
   USE module_state_description
   USE module_model_constants
!  USE module_timing
   USE module_configure
#ifdef DM_PARALLEL
   USE module_dm
#endif

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
                         CLDFRA,GLW,GSW,EMISS,LU_INDEX,          &
                         landuse_ISICE, landuse_LUCATS,          &
                         landuse_LUSEAS, landuse_ISN,            &
                         lu_state,                               &
                         XLAT,XLONG,ALBEDO,ALBBCK,GMT,JULYR,JULDAY,&
                         levsiz, n_ozmixm, n_aerosolc, paerlev,  &
                         TMN,XLAND,ZNT,Z0,UST,MOL,PBLH,TKE_MYJ,  &
                         EXCH_H,THC,SNOWC,MAVAIL,HFX,QFX,RAINBL, &
                         TSLB,ZS,DZS,num_soil_layers,warm_rain,  & 
                         adv_moist_cond,                         &
                         APR_GR,APR_W,APR_MC,APR_ST,APR_AS,      &
                         APR_CAPMA,APR_CAPME,APR_CAPMI,          &
                         XICE,VEGFRA,SNOW,CANWAT,SMSTAV,         &
                         SMSTOT, SFCRUNOFF,UDRUNOFF,GRDFLX,ACSNOW,&
                         ACSNOM,IVGTYP,ISLTYP, SFCEVP, SMOIS,    &
                         SH2O, SNOWH, SMFR3D,                    &  ! temporary
                         DX,DY,F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY, &
                         mp_restart_state,tbpvs_state,tbpvs0_state,&
                         allowed_to_read, moved, start_of_simulation,&
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         its, ite, jts, jte, kts, kte,           &
                         ozmixm,pin,                             &    ! Optional
                         m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,& ! Optional
                         RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,         &    ! Optional
                         RQVNDGDTEN,RMUNDGDTEN,                  &    ! Optional
                         FGDT,STEPFG,                            &    ! Optional
!                        num_roof_layers,num_wall_layers,        & !Optional urban
!                        num_road_layers,                        & !Optional urban
                         DZR, DZB, DZG,                          & !Optional urban
                         TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,    & !Optional urban
                         QC_URB2D, XXXR_URB2D,XXXB_URB2D,        & !Optional urban
                         XXXG_URB2D, XXXC_URB2D,                 & !Optional urban
                         TRL_URB3D, TBL_URB3D, TGL_URB3D,        & !Optional urban
                         SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,  & !Optional urban
                         TS_URB2D, FRC_URB2D, UTYPE_URB2D,       & !Optional urban
                         itimestep                               & !Optional obs fdda
#if ( EM_CORE == 1 )
                         ,fdob                                   & !Optional obs fdda
#endif
                         )

!-----------------------------------------------------------------
   USE module_domain
   USE module_wrf_error
   IMPLICIT NONE
!-----------------------------------------------------------------
   TYPE (grid_config_rec_type)              :: config_flags

   INTEGER , INTENT(IN)        :: id
   LOGICAL , INTENT(OUT)       :: warm_rain,adv_moist_cond
!   LOGICAL , INTENT (IN)       :: FNDSOILW, FNDSNOWH
   LOGICAL, PARAMETER          :: FNDSOILW=.true., FNDSNOWH=.true.
   INTEGER , INTENT(IN)        :: ids, ide, jds, jde, kds, kde,  &
                                  ims, ime, jms, jme, kms, kme,  &
                                  its, ite, jts, jte, kts, kte

   INTEGER , INTENT(IN)        :: num_soil_layers

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
                                                        VEGFRA, &
                                                        ACSNOM

   INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         IVGTYP, &
                                                        ISLTYP

! rad

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::    &
             RTHRATEN, RTHRATENLW, RTHRATENSW, CLDFRA

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             GSW,ALBEDO,ALBBCK,GLW,EMISS

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

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::         &
             XLAND,ZNT,Z0,UST,MOL,LU_INDEX,                         &
             PBLH,THC,MAVAIL,HFX,QFX,RAINBL
   INTEGER , INTENT(INOUT)  :: landuse_ISICE, landuse_LUCATS
   INTEGER , INTENT(INOUT)  :: landuse_LUSEAS, landuse_ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: TMN

!mp
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) ::   &
             F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
   REAL, DIMENSION(:), INTENT(INOUT)   :: mp_restart_state,tbpvs_state,tbpvs0_state
   LOGICAL,  INTENT(IN)  :: allowed_to_read, moved

!fdda
   REAL,     OPTIONAL, INTENT(IN) :: FGDT
   INTEGER , OPTIONAL, INTENT(OUT) :: STEPFG
   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , OPTIONAL, INTENT(OUT) ::    &
             RUNDGDTEN, RVNDGDTEN, RTHNDGDTEN, RQVNDGDTEN
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

!obs fdda
   INTEGER, OPTIONAL, INTENT(IN) :: itimestep
#if ( EM_CORE == 1 ) 
   TYPE(fdob_type), OPTIONAL, INTENT(INOUT) :: fdob
#endif

! Local data

   REAL    :: ALBLND,ZZLND,ZZWTR,THINLD,XMAVA,CEN_LAT,pptop 
   REAL,     DIMENSION( kms:kme )  :: sfull, shalf
   REAL :: obs_twindo
   
   CHARACTER*4 :: MMINLU_loc
   CHARACTER*80 :: message
   INTEGER :: ISWATER
   INTEGER :: ucmcall
! to be added to namelist: option to use climatological monthly albedo
   LOGICAL :: usebgalb

   INTEGER :: i, j, itf, jtf
integer myproc

!-----------------------------------------------------------------
   ucmcall=config_flags%ucmcall
#if ( EM_CORE == 1 ) 
   obs_twindo=config_flags%obs_twindo
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
   usebgalb = .FALSE.

   CALL nl_get_cen_lat(id,cen_lat)
   CALL wrf_debug(100,'calling nl_get_iswater, nl_get_mminlu_loc')
   CALL nl_get_iswater(id,iswater)
   CALL nl_get_mminlu( 1, mminlu_loc )
   CALL wrf_debug(100,'after nl_get_iswater, nl_get_mminlu_loc')

  IF(.not.restart)THEN
!-- initialize common variables

   IF ( .NOT. moved ) THEN
   DO j=jts,jtf
   DO i=its,itf
      XLAND(i,j)=1.
      GSW(i,j)=0.
      GLW(i,j)=0.
      UST(i,j)=0.
      MOL(i,j)=0.0
      PBLH(i,j)=0.0
      HFX(i,j)=0.
      QFX(i,j)=0.
      RAINBL(i,j)=0.
      RAINNCV(i,j)=0.
      ACSNOW(i,j)=0.
   ENDDO
   ENDDO
   ENDIF

!
   DO j=jts,jtf
   DO i=its,itf
     IF(XLAND(i,j) .LT. 1.5)THEN
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=ALBLND
       ALBEDO(i,j)=ALBBCK(i,j)
       EMISS(i,j)=0.85
       THC(i,j)=THINLD
       ZNT(i,j)=ZZLND
#if  ! ( NMM_CORE == 1 ) 
       Z0(i,j)=ZZLND
#endif
       MAVAIL(i,j)=XMAVA
     ELSE
       IF(mminlu_loc .EQ. '    ') ALBBCK(i,j)=0.08
       ALBEDO(i,j)=ALBBCK(i,j)
       EMISS(i,j)=0.98
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

     CALL landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss,      &
                znt, Z0, thc, xland, xice, julday, cen_lat, iswater, mminlu_loc,  &
                landuse_ISICE, landuse_LUCATS,                      &
                landuse_LUSEAS, landuse_ISN,                        &
                lu_state,                                           &
                allowed_to_read , usebgalb ,                        &
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

   CALL ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,             &
                RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                levsiz,XLAT,n_ozmixm,                           &
                ozmixm,pin,                                     & ! Optional
                m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & ! Optional
                paerlev,n_aerosolc,                             &
                sfull,shalf,pptop,swrad_scat,                   &
                config_flags,restart,                           & 
                allowed_to_read, start_of_simulation,           &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to bl_init' )

   CALL bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,        &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,EXCH_H,VEGFRA,          &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,SMOIS,SMFR3D,MAVAIL,              &
                SNOWH,SH2O,FNDSOILW, FNDSNOWH,                  &
#if (NMM_CORE == 1)
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                allowed_to_read ,                               &
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D, LH_URB2D, G_URB2D, RN_URB2D,          & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D, UCMCALL,      & !Optional urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )

   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to cu_init' )

   CALL cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,      &
                RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,           &
                RAINCV,W0AVG,config_flags,restart,              &
                CLDEFI,LOWLYR,MASS_FLUX,                        &
                RTHFTEN, RQVFTEN,                               &
                APR_GR,APR_W,APR_MC,APR_ST,APR_AS,              &
                APR_CAPMA,APR_CAPME,APR_CAPMI,                  &
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

   write(message,*)'STEPRA,STEPCU,STEPBL',STEPRA,STEPCU,STEPBL
   CALL wrf_message( message )

#if  ( EM_CORE == 1 )
   CALL wrf_debug ( 200 , 'module_start: phy_init: Before call to fg_init' )

   CALL fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,          &
                RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,               &
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
                  model_config_rec%dx(1),                       &
                  config_flags%restart,                         &
                  obs_twindo,                                   &
                  itimestep,                                    &
                  model_config_rec%s_sn(1),                     &
                  model_config_rec%e_sn(1),                     &
                  model_config_rec%s_we(1),                     &
                  model_config_rec%e_we(1),                     &
                  fdob,                                         &
                  ids, ide, jds, jde, kds, kde,                 &
                  ims, ime, jms, jme, kms, kme,                 &
                  its, ite, jts, jte, kts, kte                  )

#endif

   END SUBROUTINE phy_init

!=====================================================================
   SUBROUTINE landuse_init(lu_index, snowc, albedo, albbck, mavail, emiss,  &
                znt,Z0,thc,xland, xice, julday, cen_lat, iswater, mminlu, &
                ISICE, LUCATS, LUSEAS, ISN,                         &
                lu_state,                                           &
                allowed_to_read , usebgalb ,                        &
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
   CHARACTER*4, INTENT(IN)        :: mminlu
   LOGICAL,  INTENT(IN)           :: allowed_to_read , usebgalb
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: lu_index, snowc, xice
   REAL,     DIMENSION( ims:ime , jms:jme ) , INTENT(OUT  ) :: albedo, albbck, mavail, emiss, &
                                                               znt, Z0, thc, xland
   INTEGER , INTENT(INOUT)  :: ISICE, LUCATS, LUSEAS, ISN
   REAL    , INTENT(INOUT)  , DIMENSION( : ) :: lu_state

!---------------------------------------------------------------------
! Local
   CHARACTER*4 LUTYPE
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
      IF(MMINLU.EQ.'OLD ')THEN
!       ISWATER=7
        ISICE=11 
      ELSE IF(MMINLU.EQ.'USGS')THEN
!       ISWATER=16
        ISICE=24
      ELSE IF(MMINLU.EQ.'SiB ')THEN
!       ISWATER=15
        ISICE=16
      ELSE IF(MMINLU.EQ.'LW12')THEN
!       ISWATER=15
        ISICE=3
      ENDIF
      PRINT *, 'INPUT LANDUSE = ',MMINLU
      FOUND_LU = .FALSE.
      end_of_file = .FALSE.
!!! BEGINNING OF 1999 LOOP
 1999 CONTINUE                                                      
      IF ( wrf_dm_on_monitor() ) THEN
        READ (landuse_unit,2000,END=2002)LUTYPE                                
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
        CALL wrf_dm_bcast_string(lutype, 4)
        CALL wrf_dm_bcast_bytes (lucats,  IWORDSIZE )
        CALL wrf_dm_bcast_bytes (luseas,  IWORDSIZE )
        CALL wrf_dm_bcast_bytes (found_lu,  LWORDSIZE )
 2000   FORMAT (A4)                                                
        IF(FOUND_LU)THEN                                  
          LUN=LUCATS                                             
          NSN=LUSEAS                                            
            PRINT *, 'LANDUSE TYPE = ',LUTYPE,' FOUND',        &
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
      IF(usebgalb)CALL wrf_message ( 'Climatological albedo is used instead of table values' )
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
          IF(.NOT.usebgalb)ALBBCK(I,J)=ALBD(IS,ISN)/100.                                  
          ALBEDO(I,J)=ALBBCK(I,J)
          IF(SNOWC(I,J) .GT. 0.5)ALBEDO(I,J)=ALBBCK(I,J)*(1.+SCFX(IS))
          THC(I,J)=THERIN(IS,ISN)/100.                               
          Z0(I,J)=SFZ0(IS,ISN)/100.                                
          ZNT(I,J)=Z0(I,J)
          EMISS(I,J)=SFEM(IS,ISN)                                  
          MAVAIL(I,J)=SLMO(IS,ISN)                                
          IF(IS.NE.ISWATER)THEN                                  
            XLAND(I,J)=1.0                                      
          ELSE                                                 
            XLAND(I,J)=2.0                                    
          ENDIF                                              
!    SET SEA-ICE POINTS TO LAND WITH ICE/SNOW SURFACE PROPERTIES
          IF(XICE(I,J).GT.0.5)THEN
            XLAND(I,J)=1.0
            ALBBCK(I,J)=ALBD(ISICE,ISN)/100.
            ALBEDO(I,J)=ALBBCK(I,J)
            THC(I,J)=THERIN(ISICE,ISN)/100.                              
            Z0(I,J)=SFZ0(ISICE,ISN)/100.                               
            ZNT(I,J)=Z0(I,J)
            EMISS(I,J)=SFEM(ISICE,ISN)                                 
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

    RETURN
        
   END SUBROUTINE landuse_init 

!=====================================================================
   SUBROUTINE ra_init(id,STEPRA,RADT,DT,RTHRATEN,RTHRATENLW,       & 
                      RTHRATENSW,CLDFRA,EMISS,cen_lat,JULYR,JULDAY,GMT,    &
                      levsiz,XLAT,n_ozmixm,                           &
                      ozmixm,pin,                                     & ! Optional
                      m_ps_1,m_ps_2,m_hybi,aerosolc_1,aerosolc_2,     & ! Optional
                      paerlev,n_aerosolc,                             &
                      sfull,shalf,pptop,swrad_scat,                  &
                      config_flags,restart,                          & 
                      allowed_to_read, start_of_simulation,          &
                      ids, ide, jds, jde, kds, kde,                  &
                      ims, ime, jms, jme, kms, kme,                  &
                      its, ite, jts, jte, kts, kte                   )
!---------------------------------------------------------------------
   USE module_ra_rrtm
   USE module_ra_cam
   USE module_ra_sw
   USE module_ra_gsfcsw
   USE module_ra_gfdleta
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
                                     swrad_scat
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
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) :: EMISS
   LOGICAL :: etalw = .false.
   LOGICAL :: camlw = .false.
   LOGICAL :: etamp = .false.
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
                           allowed_to_read ,                &
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

        CASE (GSFCSWSCHEME)
             CALL gsfc_swinit(cen_lat, allowed_to_read )

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

        CASE DEFAULT

   END SELECT swrad_select

   END SUBROUTINE ra_init

   SUBROUTINE bl_init(STEPBL,BLDT,DT,RUBLTEN,RVBLTEN,RTHBLTEN,  &
                RQVBLTEN,RQCBLTEN,RQIBLTEN,TSK,TMN,             &
                config_flags,restart,UST,LOWLYR,TSLB,ZS,DZS,    &
                num_soil_layers,TKE_MYJ,EXCH_H,VEGFRA,          &
                SNOW,SNOWC, CANWAT,SMSTAV,                      &
                SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,ACSNOM,       &
                IVGTYP,ISLTYP,SMOIS,SMFR3D,mavail,              &
                SNOWH,SH2O,FNDSOILW, FNDSNOWH,                  &
#if  ( NMM_CORE == 1 )
                Z0,XLAND,XICE,                                  &
#else
                ZNT,XLAND,XICE,                                 &
#endif
                SFCEVP,GRDFLX,                                  &
                allowed_to_read,                                &
!                num_roof_layers,num_wall_layers,num_road_layers,& !Optional urban
                DZR, DZB, DZG,                                  & !Optional urban
                TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,   & !Optional urban
                XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,    & !Optional urban
                TRL_URB3D, TBL_URB3D, TGL_URB3D,                & !Optional urban
                SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,             & !Optional urban
                TS_URB2D, FRC_URB2D, UTYPE_URB2D,UCMCALL,       & !Optional urban
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )
!--------------------------------------------------------------------
   USE module_sf_sfclay
   USE module_sf_slab
   USE module_bl_ysu
   USE module_bl_mrf
   USE module_bl_gfs
   USE module_sf_myjsfc
   USE module_sf_noahlsm
   USE module_sf_urban
   USE module_sf_ruclsm
   USE module_bl_myjpbl
#if (NMM_CORE == 1)
   USE module_sf_lsm_nmm
#endif
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   TYPE (grid_config_rec_type) ::     config_flags
   LOGICAL , INTENT(IN)        :: restart
   LOGICAL, INTENT(IN)         ::   FNDSOILW, FNDSNOWH

   INTEGER , INTENT(IN)        ::     ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)        ::     num_soil_layers
   INTEGER , INTENT(IN)        ::     UCMCALL 

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

   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::     TSK
   REAL,  DIMENSION( ims:ime , jms:jme ) , INTENT(INOUT) ::  TMN
   LOGICAL,  INTENT(IN)           :: allowed_to_read
   INTEGER :: isn, isfc

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


!-- calculate pbl time step

   STEPBL = nint(BLDT*60./DT)
   STEPBL = max(STEPBL,1)


!-- initialize surface layer scheme

   sfclay_select: SELECT CASE(config_flags%sf_sfclay_physics)

      CASE (SFCLAYSCHEME)
           CALL sfclayinit( allowed_to_read )
           isfc = 1
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
           isfc = 1

      CASE DEFAULT

   END SELECT sfclay_select


!-- initialize surface scheme

   sfc_select: SELECT CASE(config_flags%sf_surface_physics)

      CASE (SLABSCHEME)
           CALL slabinit(TSK,TMN,                              &
                         TSLB,ZS,DZS,num_soil_layers,          & 
                         restart,                              &
                         allowed_to_read ,                     &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )
#if (NMM_CORE == 1)
      CASE (NMMLSMSCHEME)
           CALL nmmlsminit(isn,XICE,VEGFRA,SNOW,SNOWC, CANWAT,SMSTAV, &
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
                     FNDSOILW, FNDSNOWH,                       &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )

!URBAN
          IF(UCMCALL.eq.1) THEN

             IF ( PRESENT( FRC_URB2D ) .AND. PRESENT( UTYPE_URB2D )) THEN

                CALL urban_param_init(DZR,DZB,DZG,num_soil_layers                    & !urban
                                )
!                                num_roof_layers,num_wall_layers,road_soil_layers)   !urban
                CALL urban_var_init(TSK,TSLB,TMN,IVGTYP,                             & !urban
                              ims,ime,jms,jme,num_soil_layers,                 & !urban
!                              num_roof_layers,num_wall_layers,num_road_layers, & !urban
                              XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,     & !urban
                              TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D,    & !urban
                              TRL_URB3D,TBL_URB3D,TGL_URB3D,                   & !urban
                              SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D, TS_URB2D,    & ! urban
                              FRC_URB2D, UTYPE_URB2D)                            !urban
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling urban model' )
             ENDIF
          ENDIF


      CASE (RUCLSMSCHEME)
!          if(isfc .ne. 2)CALL wrf_error_fatal &
!           ( 'module_physics_init: use myjsfc and myjpbl scheme for this lsm option' )
           CALL lsmrucinit( SMFR3D,TSLB,SMOIS,ISLTYP,mavail,       &
                     num_soil_layers, restart,                     &
                     allowed_to_read ,                             &
                     ids,ide, jds,jde, kds,kde,                    &
                     ims,ime, jms,jme, kms,kme,                    &
                     its,ite, jts,jte, kts,kte                     )

      CASE DEFAULT

   END SELECT sfc_select


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
      CASE (GFSSCHEME)
           if(isfc .ne. 1)CALL wrf_error_fatal &
            ( 'module_physics_init: use sfclay scheme for this pbl option' )
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
           CALL myjpblinit(RUBLTEN,RVBLTEN,RTHBLTEN,RQVBLTEN, &
                        TKE_MYJ,EXCH_H,restart,               &
                        allowed_to_read ,                     &
                        ids, ide, jds, jde, kds, kde,         &
                        ims, ime, jms, jme, kms, kme,         &
                        its, ite, jts, jte, kts, kte          )
      CASE DEFAULT

   END SELECT pbl_select

   END SUBROUTINE bl_init

!==================================================================
   SUBROUTINE cu_init(STEPCU,CUDT,DT,RTHCUTEN,RQVCUTEN,RQRCUTEN,  &
                      RQCCUTEN,RQSCUTEN,RQICUTEN,NCA,RAINC,       &
                      RAINCV,W0AVG,config_flags,restart,          &
                      CLDEFI,LOWLYR,MASS_FLUX,                    &
                      RTHFTEN, RQVFTEN,                           &
                      APR_GR,APR_W,APR_MC,APR_ST,APR_AS,          &
                      APR_CAPMA,APR_CAPME,APR_CAPMI,              &
                      allowed_to_read, start_of_simulation,       &
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )
!------------------------------------------------------------------
   USE module_cu_kf
   USE module_cu_kfeta
   USE MODULE_CU_BMJ
   USE module_cu_gd
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
            RTHCUTEN, RQVCUTEN, RQCCUTEN, RQRCUTEN, RQICUTEN,     &
            RQSCUTEN

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
     CASE (SASSCHEME)
          CALL sasinit(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQICUTEN,       &
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
   USE module_mp_ncloud3
   USE module_mp_ncloud5
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_thompson
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
          CALL wsm3init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (WSM5SCHEME)
          CALL wsm5init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (WSM6SCHEME)
          CALL wsm6init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (ETAMPNEW)
         adv_moist_cond = .false.
         CALL etanewinit (MPDT,DT,DX,DY,LOWLYR,restart,           &
                          F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY,       &
                          mp_restart_state,tbpvs_state,tbpvs0_state,&
                          allowed_to_read,                        &
                          ids, ide, jds, jde, kds, kde,           &
                          ims, ime, jms, jme, kms, kme,           &
                          its, ite, jts, jte, kts, kte            )
     CASE (THOMPSON)
         CALL thompson_init
     CASE (NCEPCLOUD3)
          CALL ncloud3init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )
     CASE (NCEPCLOUD5)
          CALL ncloud5init(rhoair0,rhowater,rhosnow,cliq,cv, allowed_to_read )

     CASE DEFAULT

   END SELECT mp_select

   END SUBROUTINE mp_init

#if  ( EM_CORE == 1 )
!==========================================================
   SUBROUTINE fg_init(STEPFG,FGDT,DT,id,RUNDGDTEN,RVNDGDTEN,    &
                RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,               &
                config_flags,restart,                           &
                allowed_to_read ,                               &
                ids, ide, jds, jde, kds, kde,                   &
                ims, ime, jms, jme, kms, kme,                   &
                its, ite, jts, jte, kts, kte                    )


!--------------------------------------------------------------------
   USE module_fdda_psufddagd
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
               config_flags%gfdda_end_h, &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
      CASE DEFAULT

   END SELECT fdda_select

   END SUBROUTINE fg_init

!-------------------------------------------------------------------
   SUBROUTINE fdob_init(obs_nudge_opt, maxdom, inest, parid,       &
                        dx_coarse, restart, obs_twindo, itimestep, &
                        s_sn_cg, e_sn_cg, s_we_cg, e_we_cg,  &
                        fdob,                                      &
                        ids, ide, jds, jde, kds, kde,              &
                        ims, ime, jms, jme, kms, kme,              &
                        its, ite, jts, jte, kts, kte               )


!--------------------------------------------------------------------
   USE module_domain
   USE module_fddaobs_rtfdda
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
   REAL    , INTENT(IN)    :: dx_coarse
   LOGICAL , INTENT(IN)    :: restart
   REAL    , INTENT(INOUT) :: obs_twindo
   INTEGER , INTENT(IN)    :: itimestep
   INTEGER, intent(in)     :: s_sn_cg      ! starting north-south coarse-grid index
   INTEGER, intent(in)     :: e_sn_cg      ! ending   north-south coarse-grid index
   INTEGER, intent(in)     :: s_we_cg      ! starting west-east   coarse-grid index
   INTEGER, intent(in)     :: e_we_cg      ! ending   west-east   coarse-grid index

   TYPE(fdob_type), INTENT(INOUT)  :: fdob

   INTEGER                 :: e_sn         ! ending   north-south grid index
!--------------------------------------------------------------------
!-- initialize fdda obs-nudging scheme

      e_sn = jde
      CALL fddaobs_init(obs_nudge_opt, maxdom, inest, parid,       &  
                        dx_coarse, restart, obs_twindo, itimestep, &
                        e_sn, s_sn_cg, e_sn_cg, s_we_cg, e_we_cg,  &
                        fdob,                                      &  
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
