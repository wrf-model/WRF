MODULE module_sf_noahdrv

!-------------------------------
  USE module_sf_noahlsm,  only: SFLX, XLF, XLV, CP, R_D, RHOWATER, NATURAL, SHDTBL, LUTYPE, SLTYPE, STBOLT, &
      &                         KARMAN, LUCATS, NROTBL, RSTBL, RGLTBL, HSTBL, SNUPTBL, MAXALB, LAIMINTBL,   &
      &                         LAIMAXTBL, Z0MINTBL, Z0MAXTBL, ALBEDOMINTBL, ALBEDOMAXTBL, EMISSMINTBL,     &
      &                         EMISSMAXTBL, TOPT_DATA, CMCMAX_DATA, CFACTR_DATA, RSMAX_DATA, BARE, NLUS,   &
      &                         SLCATS, BB, DRYSMC, F11, MAXSMC, REFSMC, SATPSI, SATDK, SATDW, WLTSMC, QTZ, &
      &                         NSLTYPE, SLPCATS, SLOPE_DATA, SBETA_DATA, FXEXP_DATA, CSOIL_DATA,           &
      &                         SALP_DATA, REFDK_DATA, REFKDT_DATA, FRZK_DATA, ZBOT_DATA, CZIL_DATA,        &
      &                         SMLOW_DATA, SMHIGH_DATA, LVCOEF_DATA, NSLOPE, &
      &                         FRH2O,ZTOPVTBL,ZBOTVTBL, &
      &                         LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL

  USE module_sf_urban,    only: urban, oasis, IRI_SCHEME
  USE module_sf_noahlsm_glacial_only, only: sflx_glacial
  USE module_sf_bep,      only: bep
  USE module_sf_bep_bem,  only: bep_bem
  USE module_ra_gfdleta,  only: cal_mon_day
#if ( WRF_CHEM == 1 )
  USE module_data_gocart_dust
#endif
!-------------------------------

!
CONTAINS
!
!----------------------------------------------------------------
! Urban related variable are added to arguments - urban
!----------------------------------------------------------------
   SUBROUTINE lsm(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
                  HFX,QFX,LH,GRDFLX, QGH,GSW,SWDOWN,GLW,SMSTAV,SMSTOT, &
                  SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,ISURBAN,ISICE,VEGFRA,    &
                  ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE,EMISS,EMBCK,   &
                  SNOWC,QSFC,RAINBL,MMINLU,                     &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,                       &
                  CHS,CHS2,CQS2,CPM,ROVCP,SR,chklowq,lai,qz0,   & !H
                  myj,frpcpn,                                   &
                  SH2O,SNOWH,                                   & !H
                  U_PHY,V_PHY,                                  & !I
                  SNOALB,SHDMIN,SHDMAX,                         & !I
                  SNOTIME,                                      & !?
                  ACSNOM,ACSNOW,                                & !O
                  SNOPCX,                                       & !O
                  POTEVP,                                       & !O
                  SMCREL,                                       & !O
                  XICE_THRESHOLD,                               &
                  RDLAI2D,USEMONALB,                            &
                  RIB,                                          & !?
                  NOAHRES,opt_thcnd,                            &
! Noah UA changes
                  ua_phys,flx4_2d,fvb_2d,fbur_2d,fgsn_2d,       &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  sf_urban_physics,                             &
                  CMR_SFCDIF,CHR_SFCDIF,CMC_SFCDIF,CHC_SFCDIF,  &
                  CMGR_SFCDIF,CHGR_SFCDIF,                      &
!Optional Urban
                  TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D, & !H urban
                  UC_URB2D,                                     & !H urban
                  XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,  & !H urban
                  TRL_URB3D,TBL_URB3D,TGL_URB3D,                & !H urban
                  SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,TS_URB2D,  & !H urban
                  PSIM_URB2D,PSIH_URB2D,U10_URB2D,V10_URB2D,    & !O urban
                  GZ1OZ0_URB2D,  AKMS_URB2D,                    & !O urban
                  TH2_URB2D,Q2_URB2D, UST_URB2D,                & !O urban
                  DECLIN_URB,COSZ_URB2D,OMG_URB2D,              & !I urban
                  XLAT_URB2D,                                   & !I urban
                  num_roof_layers, num_wall_layers,             & !I urban
                  num_road_layers, DZR, DZB, DZG,               & !I urban
                  CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,    & !H urban
                  DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,          & !H urban
                  FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,    & !H urban
                  julian, julyr,                                & !H urban
                  FRC_URB2D,UTYPE_URB2D,                        & !O
                  num_urban_layers,                             & !I multi-layer urban
                  num_urban_hi,                                 & !I multi-layer urban
                  trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,      & !H multi-layer urban
                  tlev_urb3d,qlev_urb3d,                        & !H multi-layer urban
                  tw1lev_urb3d,tw2lev_urb3d,                    & !H multi-layer urban
                  tglev_urb3d,tflev_urb3d,                      & !H multi-layer urban
                  sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,          & !H multi-layer urban
                  sfvent_urb3d,lfvent_urb3d,                    & !H multi-layer urban
                  sfwin1_urb3d,sfwin2_urb3d,                    & !H multi-layer urban
                  sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,    & !H multi-layer urban
                  lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,         & !H multi-layer urban
                  mh_urb2d,stdh_urb2d,lf_urb2d,                 & !SLUCM
                  th_phy,rho,p_phy,ust,                         & !I multi-layer urban
                  gmt,julday,xlong,xlat,                        & !I multi-layer urban
                  a_u_bep,a_v_bep,a_t_bep,a_q_bep,              & !O multi-layer urban
                  a_e_bep,b_u_bep,b_v_bep,                      & !O multi-layer urban
                  b_t_bep,b_q_bep,b_e_bep,dlg_bep,              & !O multi-layer urban
                  dl_u_bep,sf_bep,vl_bep,sfcheadrt,INFXSRT, soldrain      &   !O multi-layer urban         
                 ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas     &   !fasdas
                  )   !O multi-layer urban         

!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------
!----------------------------------------------------------------
! --- atmospheric (WRF generic) variables
!-- DT          time step (seconds)
!-- DZ8W        thickness of layers (m)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- QGH         saturated mixing ratio at 2 meter
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- History variables
!-- CANWAT      canopy moisture content (mm)
!-- TSK         surface temperature (K)
!-- TSLB        soil temp (k)
!-- SMOIS       total soil moisture content (volumetric fraction)
!-- SH2O        unfrozen soil moisture content (volumetric fraction)
!                note: frozen soil moisture (i.e., soil ice) = SMOIS - SH2O
!-- SNOWH       actual snow depth (m)
!-- SNOW        liquid water-equivalent snow depth (m)
!-- ALBEDO      time-varying surface albedo including snow effect (unitless fraction)
!-- ALBBCK      background surface albedo (unitless fraction)
!-- CHS          surface exchange coefficient for heat and moisture (m s-1);
!-- CHS2        2m surface exchange coefficient for heat  (m s-1);
!-- CQS2        2m surface exchange coefficient for moisture (m s-1);
! --- soil variables
!-- num_soil_layers   the number of soil layers
!-- ZS          depths of centers of soil layers   (m)
!-- DZS         thicknesses of soil layers (m)
!-- SLDPTH      thickness of each soil layer (m, same as DZS)
!-- TMN         soil temperature at lower boundary (K)
!-- SMCWLT      wilting point (volumetric)
!-- SMCDRY      dry soil moisture threshold where direct evap from
!               top soil layer ends (volumetric)
!-- SMCREF      soil moisture threshold below which transpiration begins to
!                   stress (volumetric)
!-- SMCMAX      porosity, i.e. saturated value of soil moisture (volumetric)
!-- NROOT       number of root layers, a function of veg type, determined
!               in subroutine redprm.
!-- SMSTAV      Soil moisture availability for evapotranspiration (
!                   fraction between SMCWLT and SMCMXA)
!-- SMSTOT      Total soil moisture content frozen+unfrozen) in the soil column (mm)
! --- snow variables
!-- SNOWC       fraction snow coverage (0-1.0)
! --- vegetation variables
!-- SNOALB      upper bound on maximum albedo over deep snow
!-- SHDMIN      minimum areal fractional coverage of annual green vegetation
!-- SHDMAX      maximum areal fractional coverage of annual green vegetation
!-- XLAI        leaf area index (dimensionless)
!-- Z0BRD       Background fixed roughness length (M)
!-- Z0          Background vroughness length (M) as function
!-- ZNT         Time varying roughness length (M) as function
!-- ALBD(IVGTPK,ISN) background albedo reading from a table
! --- LSM output
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          upward moisture flux at the surface (W m-2)
!-- GRDFLX(I,J) ground heat flux (W m-2)
!-- FDOWN       radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
!----------------------------------------------------------------------------
!-- EC          canopy water evaporation ((W m-2)
!-- EDIR        direct soil evaporation (W m-2)
!-- ET          plant transpiration from a particular root layer (W m-2)
!-- ETT         total plant transpiration (W m-2)
!-- ESNOW       sublimation from (or deposition to if <0) snowpack (W m-2)
!-- DRIP        through-fall of precip and/or dew in excess of canopy
!                 water-holding capacity (m)
!-- DEW         dewfall (or frostfall for t<273.15) (M)
!-- SMAV        Soil Moisture Availability for each layer, as a fraction
!                 between SMCWLT and SMCMAX (dimensionless fraction)
! ----------------------------------------------------------------------
!-- BETA        ratio of actual/potential evap (dimensionless)
!-- ETP         potential evaporation (W m-2)
! ----------------------------------------------------------------------
!-- FLX1        precip-snow sfc (W m-2)
!-- FLX2        freezing rain latent heat flux (W m-2)
!-- FLX3        phase-change heat flux from snowmelt (W m-2)
! ----------------------------------------------------------------------
!-- ACSNOM      snow melt (mm) (water equivalent)
!-- ACSNOW      accumulated snow fall (mm) (water equivalent)
!-- SNOPCX      snow phase change heat flux (W/m^2)
!-- POTEVP      accumulated potential evaporation (m)
!-- RIB         Documentation needed!!!
! ----------------------------------------------------------------------
!-- RUNOFF1     surface runoff (m s-1), not infiltrating the surface
!-- RUNOFF2     subsurface runoff (m s-1), drainage out bottom of last
!                  soil layer (baseflow)
!  important note: here RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3
!-- RUNOFF3     numerical trunctation in excess of porosity (smcmax)
!                  for a given soil layer at the end of a time step (m s-1).
!SFCRUNOFF     Surface Runoff (mm)
!UDRUNOFF      Total Underground Runoff (mm), which is the sum of RUNOFF2 and RUNOFF3
! ----------------------------------------------------------------------
!-- RC          canopy resistance (s m-1)
!-- PC          plant coefficient (unitless fraction, 0-1) where PC*ETP = actual transp
!-- RSMIN       minimum canopy resistance (s m-1)
!-- RCS         incoming solar rc factor (dimensionless)
!-- RCT         air temperature rc factor (dimensionless)
!-- RCQ         atmos vapor pressure deficit rc factor (dimensionless)
!-- RCSOIL      soil moisture rc factor (dimensionless)

!-- EMISS       surface emissivity (between 0 and 1)
!-- EMBCK       Background surface emissivity (between 0 and 1)

!-- ROVCP       R/CP
!               (R_d/R_v) (dimensionless)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!
!-- SR          fraction of frozen precip (0.0 to 1.0)
!----------------------------------------------------------------

! IN only

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER,  INTENT(IN   )   ::  sf_urban_physics               !urban
   INTEGER,  INTENT(IN   )   ::  isurban
   INTEGER,  INTENT(IN   )   ::  isice
   INTEGER,  INTENT(IN   )   ::  julian, julyr                  !urban

!added by Wei Yu  for routing
    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
             INTENT(INOUT)  :: sfcheadrt,INFXSRT,soldrain 
    real :: etpnd1
!end added



   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        SHDMIN, &
                                                        SHDMAX, &
                                                        SNOALB, &
                                                           GSW, &
                                                        SWDOWN, & !added 10 jan 2007
                                                           GLW, &
                                                        RAINBL, &
                                                        EMBCK,  &
                                                        SR

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

! IN and OUT

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & ! total soil moisture
                                                         SH2O,  & ! new soil liquid
                                                         TSLB     ! TSLB     STEMP

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(OUT)     ::                         SMCREL

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & !was TGB (temperature)
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
                                                         SNOWH, & !new
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

! Noah UA changes
   LOGICAL,                                INTENT(IN)  :: UA_PHYS
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: FLX4_2D,FVB_2D,FBUR_2D,FGSN_2D
   REAL                                                :: FLX4,FVB,FBUR,FGSN

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
               INTENT(OUT)    ::                        CHKLOWQ
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LAI
   REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::        QZ0

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF
! Local variables (moved here from driver to make routine thread safe, 20031007 jm)

      REAL, DIMENSION(1:num_soil_layers) ::  ET

      REAL, DIMENSION(1:num_soil_layers) ::  SMAV

      REAL  ::  BETA, ETP, SSOIL,EC, EDIR, ESNOW, ETT,        &
                FLX1,FLX2,FLX3, DRIP,DEW,FDOWN,RC,PC,RSMIN,XLAI,  &
!                RCS,RCT,RCQ,RCSOIL
                RCS,RCT,RCQ,RCSOIL,FFROZP

    LOGICAL,    INTENT(IN   )    ::     myj,frpcpn

! DECLARATIONS - LOGICAL
! ----------------------------------------------------------------------
      LOGICAL, PARAMETER :: LOCAL=.false.
      LOGICAL :: FRZGRA, SNOWNG

      LOGICAL :: IPRINT

! ----------------------------------------------------------------------
! DECLARATIONS - INTEGER
! ----------------------------------------------------------------------
      INTEGER :: I,J, ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER :: NROOT
      INTEGER :: KZ ,K
      INTEGER :: NS
! ----------------------------------------------------------------------
! DECLARATIONS - REAL
! ----------------------------------------------------------------------

      REAL  :: SHMIN,SHMAX,DQSDT2,LWDN,PRCP,PRCPRAIN,                    &
               Q2SAT,Q2SATI,SFCPRS,SFCSPD,SFCTMP,SHDFAC,SNOALB1,         &
               SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ALBEDOK, ETA, ETA_KINEMATIC, &
               EMBRD,                                                    &
               Z0K,RUNOFF1,RUNOFF2,RUNOFF3,SHEAT,SOLNET,E2SAT,SFCTSNO,   &
! mek, WRF testing, expanded diagnostics
               SOLUP,LWUP,RNET,RES,Q1SFC,TAIRV,SATFLG
! MEK MAY 2007
      REAL ::  FDTLIW
! MEK JUL2007 for pot. evap.
      REAL :: RIBB
      REAL ::  FDTW

      REAL  :: EMISSI

      REAL  :: SNCOVR,SNEQV,SNOWHK,CMC, CHK,TH2

      REAL  :: SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT,SOILM,SOILW,Q1,T1
      REAL  :: SNOTIME1    ! LSTSNW1 INITIAL NUMBER OF TIMESTEPS SINCE LAST SNOWFALL

      REAL  :: DUMMY,Z0BRD
!
      REAL  :: COSZ, SOLARDIRECT
!
      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC
!
      REAL, DIMENSION(1:num_soil_layers) ::     ZSOIL, RTDIS
      REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
                          T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)
! MEK MAY 2007
      REAL, PARAMETER  :: ROW=1.E3,ELIW=XLF,ROWLIW=ROW*ELIW

! ----------------------------------------------------------------------
! DECLARATIONS START - urban
! ----------------------------------------------------------------------

! input variables surface_driver --> lsm
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

! input variables lsm --> urban
     INTEGER :: UTYPE_URB ! urban type [urban=1, suburban=2, rural=3]
     REAL :: TA_URB       ! potential temp at 1st atmospheric level [K]
     REAL :: QA_URB       ! mixing ratio at 1st atmospheric level  [kg/kg]
     REAL :: UA_URB       ! wind speed at 1st atmospheric level    [m/s]
     REAL :: U1_URB       ! u at 1st atmospheric level             [m/s]
     REAL :: V1_URB       ! v at 1st atmospheric level             [m/s]
     REAL :: SSG_URB      ! downward total short wave radiation    [W/m/m]
     REAL :: LLG_URB      ! downward long wave radiation           [W/m/m]
     REAL :: RAIN_URB     ! precipitation                          [mm/h]
     REAL :: RHOO_URB     ! air density                            [kg/m^3]
     REAL :: ZA_URB       ! first atmospheric level                [m]
     REAL :: DELT_URB     ! time step                              [s]
     REAL :: SSGD_URB     ! downward direct short wave radiation   [W/m/m]
     REAL :: SSGQ_URB     ! downward diffuse short wave radiation  [W/m/m]
     REAL :: XLAT_URB     ! latitude                               [deg]
     REAL :: COSZ_URB     ! cosz
     REAL :: OMG_URB      ! hour angle
     REAL :: ZNT_URB      ! roughness length                       [m]
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
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  ! roof layer temp [K]
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  ! wall layer temp [K]
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  ! road layer temp [K]
     LOGICAL  :: LSOLAR_URB

!===Yang,2014/10/08,hydrological variable for single layer UCM===
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
     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  ! green roof layer moisture
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB ! green roof layer temp [K]

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


! state variable surface_driver <--> lsm <--> urban
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
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D

! output variable lsm --> surface_driver
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
!
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: FRC_URB2D
     INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: UTYPE_URB2D


! output variables urban --> lsm
     REAL :: TS_URB     ! surface radiative temperature    [K]
     REAL :: QS_URB     ! surface humidity                 [-]
     REAL :: SH_URB     ! sensible heat flux               [W/m/m]
     REAL :: LH_URB     ! latent heat flux                 [W/m/m]
     REAL :: LH_KINEMATIC_URB ! latent heat flux, kinetic  [kg/m/m/s]
     REAL :: SW_URB     ! upward short wave radiation flux [W/m/m]
     REAL :: ALB_URB    ! time-varying albedo            [fraction]
     REAL :: LW_URB     ! upward long wave radiation flux  [W/m/m]
     REAL :: G_URB      ! heat flux into the ground        [W/m/m]
     REAL :: RN_URB     ! net radiation                    [W/m/m]
     REAL :: PSIM_URB   ! shear f for momentum             [-]
     REAL :: PSIH_URB   ! shear f for heat                 [-]
     REAL :: GZ1OZ0_URB   ! shear f for heat                 [-]
     REAL :: U10_URB    ! wind u component at 10 m         [m/s]
     REAL :: V10_URB    ! wind v component at 10 m         [m/s]
     REAL :: TH2_URB    ! potential temperature at 2 m     [K]
     REAL :: Q2_URB     ! humidity at 2 m                  [-]
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB
! NUDAPT Parameters urban --> lam
     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb
! Variables for multi-layer UCM (Martilli et al. 2002)
   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_LAYERS
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_HI
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lp_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lb_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: hgt_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: mh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: stdh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(IN) :: lf_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   !Implicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   !Explicit momentum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   !Explicit momentum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   !Explicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    !Fraction air volume in grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   !Height above ground
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  !Fraction air at the face of grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  !Length scale

! Local variables for multi-layer UCM (Martilli et al. 2002)
   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_RURAL,LH_RURAL,GRDFLX_RURAL ! ,RN_RURAL
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_RURAL ! ,QSFC_RURAL,UMOM_RURAL,VMOM_RURAL
   REAL,    DIMENSION( its:ite, jts:jte ) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL ! ,UST_RURAL
!   REAL,    DIMENSION( ims:ime, jms:jme ) :: QSFC_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_URB,UMOM_URB,VMOM_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_URB
!   REAL,    DIMENSION( ims:ime, jms:jme ) :: ALBEDO_URB,EMISS_URB,UMOM,VMOM,UST
   REAL, DIMENSION(its:ite,jts:jte) ::EMISS_URB
   REAL, DIMENSION(its:ite,jts:jte) :: RL_UP_URB
   REAL, DIMENSION(its:ite,jts:jte) ::RS_ABS_URB
   REAL, DIMENSION(its:ite,jts:jte) ::GRDFLX_URB
   REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
   REAL :: r1,r2,r3
   REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
   REAL :: frc_urb,lb_urb
   REAL :: check 
! ----------------------------------------------------------------------
! DECLARATIONS END - urban
! ----------------------------------------------------------------------

  REAL, PARAMETER  :: CAPA=R_D/CP
  REAL :: APELM,APES,SFCTH2,PSFC
  real, intent(in) :: xice_threshold
  character(len=80) :: message_text
!
!  FASDAS
!
   REAL,    DIMENSION( ims:ime, jms:jme ),  OPTIONAL,                     &
            INTENT(INOUT)  ::   SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM
   INTEGER, INTENT(IN   )  ::  fasdas
!  local vars
   REAL                    ::  XSDA_HFX, XSDA_QFX, XQNORM
   REAL                    ::  HFX_PHY, QFX_PHY
   REAL                    ::  DZQ
   REAL                    ::  HCPCT_FASDAS

   HFX_PHY = 0.0   ! initialize
   QFX_PHY = 0.0
   XQNORM  = 0.0
!
!  END FASDAS
!
  FLX4  = 0.0 !BSINGH - Initialized to 0.0
  FVB   = 0.0 !BSINGH - Initialized to 0.0
  FBUR  = 0.0 !BSINGH - Initialized to 0.0
  FGSN  = 0.0 !BSINGH - Initialized to 0.0
  SOILW = 0.0 !BSINGH - Initialized to 0.0

! MEK MAY 2007
      FDTLIW=DT/ROWLIW
! MEK JUL2007
      FDTW=DT/(XLV*RHOWATER)
! debug printout
         IPRINT=.false.

!      SLOPETYP=2
      SLOPETYP=1
!      SHDMIN=0.00


      NSOIL=num_soil_layers

     DO NS=1,NSOIL
     SLDPTH(NS)=DZS(NS)
     ENDDO

     JLOOP : DO J=jts,jte

      IF(ITIMESTEP.EQ.1)THEN
        DO 50 I=its,ite
!*** initialize soil conditions for IHOP 31 May case
!         IF((XLAND(I,J)-1.5) < 0.)THEN
!            if (I==108.and.j==85) then
!                  DO NS=1,NSOIL
!                      SMOIS(I,NS,J)=0.10
!                      SH2O(I,NS,J)=0.10
!                  enddo
!             endif
!         ENDIF

!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
          IF((XLAND(I,J)-1.5).GE.0.)THEN
! check sea-ice point
#if 0
            IF( XICE(I,J).GE. XICE_THRESHOLD .and. IPRINT ) PRINT*, ' sea-ice at water point, I=',I,'J=',J
#endif
!***   Open Water Case
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            DO NS=1,NSOIL
              SMOIS(I,NS,J)=1.0
              TSLB(I,NS,J)=273.16                                          !STEMP
              SMCREL(I,NS,J)=1.0
            ENDDO
          ELSE
            IF ( XICE(I,J) .GE. XICE_THRESHOLD ) THEN
!***        SEA-ICE CASE
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              DO NS=1,NSOIL
                SMOIS(I,NS,J)=1.0
                SMCREL(I,NS,J)=1.0
              ENDDO
            ENDIF
          ENDIF
!
   50   CONTINUE
      ENDIF                                                               ! end of initialization over ocean

!-----------------------------------------------------------------------
      ILOOP : DO I=its,ite
! surface pressure
        PSFC=P8w3D(i,1,j)
! pressure in middle of lowest layer
        SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
! convert from mixing ratio to specific humidity
         Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))
!
!         Q2SAT=QGH(I,j)
         Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        ! Q2SAT is sp humidity
! add check on myj=.true.
!        IF((Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
        IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
          SATFLG=0.
          CHKLOWQ(I,J)=0.
        ELSE
          SATFLG=1.0
          CHKLOWQ(I,J)=1.
        ENDIF

        SFCTMP=T3D(i,1,j)
        ZLVL=0.5*DZ8W(i,1,j)

!        TH2=SFCTMP+(0.0097545*ZLVL)
! calculate SFCTH2 via Exner function vs lapse-rate (above)
         APES=(1.E5/PSFC)**CAPA
         APELM=(1.E5/SFCPRS)**CAPA
         SFCTH2=SFCTMP*APELM
         TH2=SFCTH2/APES
!
         EMISSI = EMISS(I,J)
         LWDN=GLW(I,J)*EMISSI
! SOLDN is total incoming solar
        SOLDN=SWDOWN(I,J)
! GSW is net downward solar
!        SOLNET=GSW(I,J)
! use mid-day albedo to determine net downward solar (no solar zenith angle correction)
        SOLNET=SOLDN*(1.-ALBEDO(I,J))
        PRCP=RAINBL(i,j)/DT
        VEGTYP=IVGTYP(I,J)
        SOILTYP=ISLTYP(I,J)
        SHDFAC=VEGFRA(I,J)/100.
        T1=TSK(I,J)
        CHK=CHS(I,J)
        SHMIN=SHDMIN(I,J)/100. !NEW
        SHMAX=SHDMAX(I,J)/100. !NEW
! convert snow water equivalent from mm to meter
        SNEQV=SNOW(I,J)*0.001
! snow depth in meters
        SNOWHK=SNOWH(I,J)
        SNCOVR=SNOWC(I,J)

! if "SR" present, set frac of frozen precip ("FFROZP") = snow-ratio ("SR", range:0-1)
! SR from e.g. Ferrier microphysics
! otherwise define from 1st atmos level temperature
       IF(FRPCPN) THEN
          FFROZP=SR(I,J)
        ELSE
          IF (SFCTMP <=  273.15) THEN
            FFROZP = 1.0
	  ELSE
	    FFROZP = 0.0
	  ENDIF
        ENDIF
!***
        IF((XLAND(I,J)-1.5).GE.0.)THEN                                  ! begining of land/sea if block
! Open water points
          TSK_RURAL(I,J)=TSK(I,J)
          HFX_RURAL(I,J)=HFX(I,J)
          QFX_RURAL(I,J)=QFX(I,J)
          LH_RURAL(I,J)=LH(I,J)
          EMISS_RURAL(I,J)=EMISS(I,J)
          GRDFLX_RURAL(I,J)=GRDFLX(I,J)

        ELSE
! Land or sea-ice case

          IF (XICE(I,J) >= XICE_THRESHOLD) THEN
             ! Sea-ice point
             ICE = 1
          ELSE IF ( VEGTYP == ISICE ) THEN
             ! Land-ice point
             ICE = -1
          ELSE
             ! Neither sea ice or land ice.
             ICE=0
          ENDIF
          DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2

          IF(SNOW(I,J).GT.0.0)THEN
! snow on surface (use ice saturation properties)
            SFCTSNO=SFCTMP
            E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
            Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
            Q2SATI=Q2SATI/(1.0+Q2SATI)    ! spec. hum.
            IF (T1 .GT. 273.14) THEN
! warm ground temps, weight the saturation between ice and water according to SNOWC
              Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
              DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
            ELSE
! cold ground temps, use ice saturation only
              Q2SAT=Q2SATI
              DQSDT2=Q2SATI*6174./(SFCTSNO**2)
            ENDIF
! for snow cover fraction at 0 C, ground temp will not change, so DQSDT2 effectively zero
! V3.8 add condition for SWDOWN to restrict condition to positive forcing (JD)
            IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0. .AND. SWDOWN(I,J) .GT. 10.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
          ENDIF

          ! Land-ice or land points use the usual deep-soil temperature.
          TBOT=TMN(I,J)

          IF(VEGTYP.EQ.25) SHDFAC=0.0000
          IF(VEGTYP.EQ.26) SHDFAC=0.0000
          IF(VEGTYP.EQ.27) SHDFAC=0.0000
          IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
#if 0
         IF(IPRINT)PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
         IF(IPRINT)PRINT*,i,j,'RESET SOIL in surfce.F'
#endif
            SOILTYP=7
          ENDIF
          SNOALB1 = SNOALB(I,J)
          CMC=CANWAT(I,J)

!-------------------------------------------
!*** convert snow depth from mm to meter
!
!          IF(RDMAXALB) THEN
!           SNOALB=ALBMAX(I,J)*0.01
!         ELSE
!           SNOALB=MAXALB(IVGTPK)*0.01
!         ENDIF

!        SNOALB1=0.80
!        SHMIN=0.00
        ALBBRD=ALBBCK(I,J)
        Z0BRD=Z0(I,J)
        EMBRD=EMBCK(I,J)
        SNOTIME1 = SNOTIME(I,J)
        RIBB=RIB(I,J)
!FEI: temporaray arrays above need to be changed later by using SI

          DO NS=1,NSOIL
            SMC(NS)=SMOIS(I,NS,J)
            STC(NS)=TSLB(I,NS,J)                                          !STEMP
            SWC(NS)=SH2O(I,NS,J)
          ENDDO
!
          if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
            SNOWHK= 5.*SNEQV
          endif
!

!Fei: urban. for urban surface, if calling UCM, redefine the natural surface in cities as
! the "NATURAL" category in the VEGPARM.TBL
	
	   IF(SF_URBAN_PHYSICS == 1.OR. SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN
                IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                  IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
		 VEGTYP = NATURAL
                 SHDFAC = SHDTBL(NATURAL)
                 ALBEDOK =0.2         !  0.2
                 ALBBRD  =0.2         !0.2
                 EMISSI = 0.98                                 !for VEGTYP=5
		 IF ( FRC_URB2D(I,J) < 0.99 ) THEN
                   if(sf_urban_physics.eq.1)then
           T1= ( TSK(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
                   elseif((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then
                r1= (tsk(i,j)**4.)
                r2= frc_urb2d(i,j)*(ts_urb2d(i,j)**4.)
                r3= (1.-frc_urb2d(i,j))
                t1= ((r1-r2)/r3)**.25
                   endif
	         ELSE
		 T1 = TSK(I,J)
                 ENDIF
                ENDIF
           ELSE
                 IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                  IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
                  VEGTYP = ISURBAN
           	 ENDIF
           ENDIF

!===Yang, 2014/10/08, hydrological processes for urban vegetation in single layer UCM===
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
           IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
            AOASIS = oasis  ! urban oasis effect
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  !irrigation on vegetaion in urban area, MAY-SEP, 9-10pm
                   IF (jmonth==5 .or. jmonth==6 .or. jmonth==7 .or. jmonth==8 .or. jmonth==9) THEN
!                      IF (SMC(USOIL) .LT. SMCREF) SMC(USOIL)= SMCREF
!                      IF (SMC(DSOIL) .LT. SMCREF) SMC(DSOIL)= SMCREF
                      IF (SMC(USOIL) .LT. SMCREF) SMC(USOIL)= REFSMC(ISLTYP(I,J))                      
                      IF (SMC(DSOIL) .LT. SMCREF) SMC(DSOIL)= REFSMC(ISLTYP(I,J))                    
                   ENDIF
                   ENDIF
                   ENDIF
                ENDIF
           ENDIF

	  IF(SF_URBAN_PHYSICS == 2 .or. SF_URBAN_PHYSICS == 3) THEN
          IF(AOASIS > 1.0) THEN
          CALL wrf_error_fatal('Urban oasis option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          IF(IRIOPTION == 1) THEN
          CALL wrf_error_fatal('Urban irrigation option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          ENDIF

#if 0
          IF(IPRINT) THEN
!
       print*, 'BEFORE SFLX, in Noahlsm_driver'
       print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
       'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
        LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
        'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
         'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
         'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
         'SHMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB1',SNOALB1,'TBOT',&
          TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
          STC, 'SMC',SMC, 'SWC',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
          'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
          'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
          'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
          'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
          'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
          'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
          'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
          'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
          'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
          'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
           endif
#endif

          IF (rdlai2d) THEN
             xlai = lai(i,j)
          endif

    IF ( ICE == 1 ) THEN

       ! Sea-ice case

       DO NS = 1, NSOIL
          SH2O(I,NS,J) = 1.0
       ENDDO
       LAI(I,J) = 0.01

       CYCLE ILOOP

    ELSEIF (ICE == 0) THEN

       ! Non-glacial land
!
!  FASDAS
!
       IF( fasdas == 1 ) THEN

         DZQ = DZ8W(I,1,J)
         XSDA_HFX= SDA_HFX(I,J)*RHO(I,1,J)*CPM(I,J)*DZQ  ! W/m^2
         ! TWG2015 Bugfix remove factor of 1000.0 for correct units
         XSDA_QFX= SDA_QFX(I,J)*RHO(I,1,J)*DZQ          ! Kg/m2/s of water
         XQNORM = QNORM(I,J)

       ENDIF
!
!  END FASDAS
!
       CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    !C
                 LOCAL,                                            &    !L
                 LUTYPE, SLTYPE,                                   &    !CL
                 LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    !F
                 DUMMY,DUMMY, DUMMY,                               &    !F PRCPRAIN not used
                 TH2,Q2SAT,DQSDT2,                                 &    !I
                 VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    !I
                 ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    !S
                 CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    !H
                 ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    !O
                 EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    !O
                 BETA,ETP,SSOIL,                                   &    !O
                 FLX1,FLX2,FLX3,                                   &    !O
		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                       &    !UA 
                 SNOMLT,SNCOVR,                                    &    !O
                 RUNOFF1,RUNOFF2,RUNOFF3,                          &    !O
                 RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    !O
                 SOILW,SOILM,Q1,SMAV,                              &    !D
                 RDLAI2D,USEMONALB,                                &
                 SNOTIME1,                                         &
                 RIBB,                                             &
                 SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                 sfcheadrt(i,j),                                   &    !I
                 INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    !O
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    &   ! fasdas
                 )


#ifdef WRF_HYDRO
                 soldrain(i,j) = RUNOFF2*DT*1000.0
#endif
    ELSEIF (ICE == -1) THEN

       !
       ! Set values that the LSM is expected to update,
       ! but don't get updated for glacial points.
       !
       SOILM = 0.0 !BSINGH(PNNL)- SOILM is undefined for this case, it is used for diagnostics so setting it to zero
       XLAI = 0.01 ! KWM Should this be Zero over land ice?  Does this value matter?
       RUNOFF2 = 0.0
       RUNOFF3 = 0.0
       DO NS = 1, NSOIL
          SWC(NS) = 1.0
          SMC(NS) = 1.0
          SMAV(NS) = 1.0
       ENDDO
       CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    !C
            &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    !F
            &    TH2,Q2SAT,DQSDT2,                                &    !I
            &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    !S
            &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    !H
            &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    !O
            &    ESNOW,DEW,                                       &    !O
            &    ETP,SSOIL,                                       &    !O
            &    FLX1,FLX2,FLX3,                                  &    !O
            &    SNOMLT,SNCOVR,                                   &    !O
            &    RUNOFF1,                                         &    !O
            &    Q1,                                              &    !D
            &    SNOTIME1,                                        &
            &    RIBB)

    ENDIF

       lai(i,j) = xlai

#if 0
          IF(IPRINT) THEN

       print*, 'AFTER SFLX, in Noahlsm_driver'
       print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
       'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
        LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
        'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
         'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
          'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
         'SHDMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB',SNOALB1,'TBOT',&
          TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
          STC, 'SMC',SMC, 'SWc',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
          'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
          'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
          'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
          'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
          'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
          'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
          'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
          'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
          'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
          'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
           endif
#endif

!***  UPDATE STATE VARIABLES
          CANWAT(I,J)=CMC
          SNOW(I,J)=SNEQV*1000.
!          SNOWH(I,J)=SNOWHK*1000.
          SNOWH(I,J)=SNOWHK                   ! SNOWHK in meters
          ALBEDO(I,J)=ALBEDOK
          ALB_RURAL(I,J)=ALBEDOK
          ALBBCK(I,J)=ALBBRD
          Z0(I,J)=Z0BRD
          EMISS(I,J) = EMISSI
          EMISS_RURAL(I,J) = EMISSI
! Noah: activate time-varying roughness length (V3.3 Feb 2011)
          ZNT(I,J)=Z0K
!
! FASDAS
!
! Update Skin Temperature
       IF( fasdas == 1 ) THEN
           XSDA_QFX= XSDA_QFX*ELWV*XQNORM

        !TWG2015 Bugfix to multiply Heat Capacity by Soil Depth for correct
        !units 

           T1 = T1 + (XSDA_HFX-XSDA_QFX)*DT/(HCPCT_FASDAS*DZS(1))

       END IF
!
! END FASDAS
!
          TSK(I,J)=T1
          TSK_RURAL(I,J)=T1
          HFX(I,J)=SHEAT
          HFX_RURAL(I,J)=SHEAT
! MEk Jul07 add potential evap accum
        POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
          QFX(I,J)=ETA_KINEMATIC
          QFX_RURAL(I,J)=ETA_KINEMATIC

#ifdef WRF_HYDRO
!added by Wei Yu
!         QFX(I,J) = QFX(I,J) + ETPND1
!         ETA = ETA + ETPND1/2.501E6*dt
!end added by Wei Yu
#endif


          LH(I,J)=ETA
          LH_RURAL(I,J)=ETA
          GRDFLX(I,J)=SSOIL
          GRDFLX_RURAL(I,J)=SSOIL
          SNOWC(I,J)=SNCOVR
          CHS2(I,J)=CQS2(I,J)
          SNOTIME(I,J) = SNOTIME1
!      prevent diagnostic ground q (q1) from being greater than qsat(tsk)
!      as happens over snow cover where the cqs2 value also becomes irrelevant
!      by setting cqs2=chs in this situation the 2m q should become just qv(k=1)
! ww: comment out this change to avoid Q2 drop due to change of radiative flux
!         IF (Q1 .GT. QSFC(I,J)) THEN
!           CQS2(I,J) = CHS(I,J)
!         ENDIF
!          QSFC(I,J)=Q1
! Convert QSFC back to mixing ratio
           QSFC(I,J)= Q1/(1.0-Q1)
!
           ! QSFC_RURAL(I,J)= Q1/(1.0-Q1)
! Calculate momentum flux from rural surface for use with multi-layer UCM (Martilli et al. 2002)

          DO 80 NS=1,NSOIL
           SMOIS(I,NS,J)=SMC(NS)
           TSLB(I,NS,J)=STC(NS)                                        !  STEMP
           SH2O(I,NS,J)=SWC(NS)
   80     CONTINUE
!       ENDIF

        FLX4_2D(I,J)  = FLX4
	FVB_2D(I,J)   = FVB
	FBUR_2D(I,J)  = FBUR
	FGSN_2D(I,J)  = FGSN

     !
     ! Residual of surface energy balance equation terms
     !

     IF ( UA_PHYS ) THEN
         noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
              - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4

     ELSE
         noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
              - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
     ENDIF


        IF (SF_URBAN_PHYSICS == 1 ) THEN                                              ! Beginning of UCM CALL if block
!--------------------------------------
! URBAN CANOPY MODEL START - urban
!--------------------------------------
! Input variables lsm --> urban


          IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
              IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL ) THEN

! Call urban

!
            UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial)

            TA_URB    = SFCTMP           ! [K]
            QA_URB    = Q2K              ! [kg/kg]
            UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
            U1_URB    = U_PHY(I,1,J)
            V1_URB    = V_PHY(I,1,J)
            IF(UA_URB < 1.) UA_URB=1.    ! [m/s]
            SSG_URB   = SOLDN            ! [W/m/m]
            SSGD_URB  = 0.8*SOLDN        ! [W/m/m]
            SSGQ_URB  = SSG_URB-SSGD_URB ! [W/m/m]
            LLG_URB   = GLW(I,J)         ! [W/m/m]
            RAIN_URB  = RAINBL(I,J)      ! [mm]
            RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) ![kg/m/m/m]
            ZA_URB    = ZLVL             ! [m]
            DELT_URB  = DT               ! [sec]
            XLAT_URB  = XLAT_URB2D(I,J)  ! [deg]
            COSZ_URB  = COSZ_URB2D(I,J)  !
            OMG_URB   = OMG_URB2D(I,J)   !
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
!
!
!      Limits to avoid dividing by small number
            if (CHS(I,J) < 1.0E-04) then
               CHS(I,J)  = 1.0E-04
            endif
            if (CHS2(I,J) < 1.0E-04) then
               CHS2(I,J)  = 1.0E-04
            endif
            if (CQS2(I,J) < 1.0E-04) then
               CQS2(I,J)  = 1.0E-04
            endif
!
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

! NUDAPT for SLUCM
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
!
! Call urban
            CALL cal_mon_day(julian,julyr,jmonth,jday)
            CALL urban(LSOLAR_URB,                                      & ! I
                       num_roof_layers,num_wall_layers,num_road_layers, & ! C
                       DZR,DZB,DZG,                                     & ! C
                       UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & ! I
                       SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & ! I
                       ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & ! I
                       XLAT_URB,DELT_URB,ZNT_URB,                       & ! I
                       CHS_URB, CHS2_URB,                               & ! I
                       TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & ! H
                       TRL_URB,TBL_URB,TGL_URB,                         & ! H
                       XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & ! H
                       TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & ! O
                       SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & ! O
                       GZ1OZ0_URB,                                      & !O
                       CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                       U10_URB, V10_URB, TH2_URB, Q2_URB,               & ! O
                       UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & ! 0
                       hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & ! H
                       TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & ! H
                       DRELR_URB,DRELB_URB,                             & ! H
                       DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)                            

#if 0
          IF(IPRINT) THEN

       print*, 'AFTER CALL URBAN'
       print*,'num_roof_layers',num_roof_layers, 'num_wall_layers',  &
        num_wall_layers,                                             &
       'DZR',DZR,'DZB',DZB,'DZG',DZG,'UTYPE_URB',UTYPE_URB,'TA_URB', &
        TA_URB,                                                      &
        'QA_URB',QA_URB,'UA_URB',UA_URB,'U1_URB',U1_URB,'V1_URB',    &
         V1_URB,                                                     &
         'SSG_URB',SSG_URB,'SSGD_URB',SSGD_URB,'SSGQ_URB',SSGQ_URB,  &
        'LLG_URB',LLG_URB,'RAIN_URB',RAIN_URB,'RHOO_URB',RHOO_URB,   &
        'ZA_URB',ZA_URB, 'DECLIN_URB',DECLIN_URB,'COSZ_URB',COSZ_URB,&
        'OMG_URB',OMG_URB,'XLAT_URB',XLAT_URB,'DELT_URB',DELT_URB,   &
         'ZNT_URB',ZNT_URB,'TR_URB',TR_URB, 'TB_URB',TB_URB,'TG_URB',&
         TG_URB,'TC_URB',TC_URB,'QC_URB',QC_URB,'TRL_URB',TRL_URB,   &
          'TBL_URB',TBL_URB,'TGL_URB',TGL_URB,'XXXR_URB',XXXR_URB,   &
         'XXXB_URB',XXXB_URB,'XXXG_URB',XXXG_URB,'XXXC_URB',XXXC_URB,&
         'TS_URB',TS_URB,'QS_URB',QS_URB,'SH_URB',SH_URB,'LH_URB',   &
         LH_URB, 'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'SW_URB',SW_URB,&
         'ALB_URB',ALB_URB,'LW_URB',LW_URB,'G_URB',G_URB,'RN_URB',   &
          RN_URB, 'PSIM_URB',PSIM_URB,'PSIH_URB',PSIH_URB,          &
         'U10_URB',U10_URB,'V10_URB',V10_URB,'TH2_URB',TH2_URB,      &
          'Q2_URB',Q2_URB,'CHS_URB',CHS_URB,'CHS2_URB',CHS2_URB
           endif
#endif

            TS_URB2D(I,J) = TS_URB

            ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   ![-]
            HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         ![W/m/m]
            QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                     + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                ![kg/m/m/s]
            LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            ![W/m/m]
            GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       ![W/m/m]
            TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            ![K]
            Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            ![-]
! Convert QSFC back to mixing ratio
            QSFC(I,J)= Q1/(1.0-Q1)
            UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      ![m/s]

#if 0
    IF(IPRINT)THEN

    print*, ' FRC_URB2D', FRC_URB2D,                        &
    'ALB_URB',ALB_URB, 'ALBEDOK',ALBEDOK, &
    'ALBEDO(I,J)',  ALBEDO(I,J),                  &
    'SH_URB',SH_URB,'SHEAT',SHEAT, 'HFX(I,J)',HFX(I,J),  &
    'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'ETA_KINEMATIC',  &
     ETA_KINEMATIC, 'QFX(I,J)',QFX(I,J),                  &
    'LH_URB',LH_URB, 'ETA',ETA, 'LH(I,J)',LH(I,J),        &
    'G_URB',G_URB,'SSOIL',SSOIL,'GRDFLX(I,J)', GRDFLX(I,J),&
    'TS_URB',TS_URB,'T1',T1,'TSK(I,J)',TSK(I,J),          &
    'QS_URB',QS_URB,'Q1',Q1,'QSFC(I,J)',QSFC(I,J)
     endif
#endif



! Renew Urban State Varialbes

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

         ENDIF                                   ! end of UCM CALL if block
!--------------------------------------
! Urban Part End - urban
!--------------------------------------

!***  DIAGNOSTICS
          SMSTAV(I,J)=SOILW
          SMSTOT(I,J)=SOILM*1000.
          DO NS=1,NSOIL
          SMCREL(I,NS,J)=SMAV(NS)
          ENDDO


!         Convert the water unit into mm
          SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
          UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0
! snow defined when fraction of frozen precip (FFROZP) > 0.5,
          IF(FFROZP.GT.0.5)THEN
            ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
          ENDIF
          IF(SNOW(I,J).GT.0.)THEN
            ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
! accumulated snow-melt energy
            SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
          ENDIF

        ENDIF                                                           ! endif of land-sea test

      ENDDO ILOOP                                                       ! of I loop
   ENDDO JLOOP                                                          ! of J loop

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
                num_urban_layers,num_urban_hi,                         &
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
                num_urban_layers,num_urban_hi,                         &
                trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,               &
                tlev_urb3d,qlev_urb3d,tw1lev_urb3d,tw2lev_urb3d,       &
                tglev_urb3d,tflev_urb3d,sf_ac_urb3d,lf_ac_urb3d,       &
                cm_ac_urb3d,sfvent_urb3d,lfvent_urb3d,                 &
                sfwin1_urb3d,sfwin2_urb3d,                             &
                sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,             &
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

    if((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then         !Bep begin
! fix the value of the Stefan-Boltzmann constant
         sigma_sb=5.67e-08
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
           
! compute upward longwave radiation from the rural part and total
!           rl_up_rural=-emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emiss_rural(i,j))*glw(i,j)
!           rl_up_tot=(1.-frc_urb2d(i,j))*rl_up_rural+frc_urb2d(i,j)*rl_up_urb(i,j)   
!           emiss(i,j)=(1.-frc_urb2d(i,j))*emiss_rural(i,j)+frc_urb2d(i,j)*emiss_urb(i,j)
! using the emissivity and the total longwave upward radiation estimate the averaged skin temperature
           IF (FRC_URB2D(I,J).GT.0.) THEN
              rl_up_rural=-emiss_rural(i,j)*sigma_sb*(tsk_rural(i,j)**4.)-(1.-emiss_rural(i,j))*glw(i,j)
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
! rename *_urb to sh_urb2d,lh_urb2d,g_urb2d,rn_urb2d
         grdflx(i,j)= (1.-frc_urb2d(i,j))*grdflx_rural(i,j)+frc_urb2d(i,j)*grdflx_urb(i,j) 
         qfx(i,j)=(1.-frc_urb2d(i,j))*qfx_rural(i,j)+qfx_urb(i,j)
!        lh(i,j)=(1.-frc_urb2d(i,j))*qfx_rural(i,j)*xlv
         lh(i,j)=qfx(i,j)*xlv
         HFX(I,J) = HFX_URB(I,J)+(1-FRC_URB2D(I,J))*HFX_RURAL(I,J)         ![W/m/m]
            SH_URB2D(I,J)    = HFX_URB(I,J)/FRC_URB2D(I,J)
            LH_URB2D(I,J)    = qfx_urb(i,j)*xlv
            G_URB2D(I,J)     = grdflx_urb(i,j)
            RN_URB2D(I,J)    = rs_abs_urb(i,j)+emiss_urb(i,j)*glw(i,j)-rl_up_urb(i,j)
            ust(i,j)=(umom**2.+vmom**2.)**.25
!          if(tsk(i,j).gt.350)write(*,*)'tsk too big!',i,j,tsk(i,j)
!          if(tsk(i,j).lt.260)write(*,*)'tsk too small!',i,j,tsk(i,j),rl_up_tot,rl_up_urb(i,j),rl_up_rural
!                    print*,'ivgtyp,i,j,sigma_sb',ivgtyp(i,j),i,j,sigma_sb
!                    print*,'hfx,lh,qfx,grdflx,ts_urb2d',hfx(i,j),lh(i,j),qfx(i,j),grdflx(i,j),ts_urb2d(i,j)
!                    print*,'tsk,albedo,emiss',tsk(i,j),albedo(i,j),emiss(i,j)
!                if(i.eq.56.and.j.eq.29)then
!                    print*,'ivgtyp, qfx, hfx',ivgtyp(i,j),hfx_rural(i,j),qfx_rural(i,j)
!                    print*,'emiss_rural,emiss_urb',emiss_rural(i,j),emiss_urb(i,j)
!                    print*,'rl_up_rural,rl_up_urb(i,j)',rl_up_rural,rl_up_urb(i,j)
!                    print*,'tsk_rural,ts_urb2d(i,j),tsk',tsk_rural(i,j),ts_urb2d(i,j),tsk(i,j)
!                    print*,'reconstruction fei',((emiss(i,j)*tsk(i,j)**4.-frc_urb2d(i,j)*emiss_urb(i,j)*ts_urb2d(i,j)**4.)/(emiss_rural(i,j)*(1.-frc_urb2d(i,j))))**.25
!                    print*,'ivgtyp,hfx,hfx_urb,hfx_rural',hfx(i,j),hfx_urb(i,j),hfx_rural(i,j)
!                    print*,'lh,lh_rural',lh(i,j),lh_rural(i,j)
!                    print*,'qfx',qfx(i,j)
!                    print*,'ts_urb2d',ts_urb2d(i,j)
!                    print*,'ust',ust(i,j)
!                    print*,'swdown,glw',swdown(i,j),glw(i,j)
!                endif
            else
              SH_URB2D(I,J)    = 0.
              LH_URB2D(I,J)    = 0.
              G_URB2D(I,J)     = 0.
              RN_URB2D(I,J)    = 0.
            endif
!          IF( IVGTYP(I,J) == 1 .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
!                  IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
!                    print*,'ivgtyp, qfx, hfx',ivgtyp(i,j),hfx_rural(i,j),qfx_rural(i,j)
!                    print*,'ivgtyp,hfx,hfx_urb,hfx_rural',hfx(i,j),hfx_urb(i,j),hfx_rural(i,j)
!                    print*,'lh,lh_rural',lh(i,j),lh_rural(i,j)
!                    print*,'qfx',qfx(i,j)
!                    print*,'ts_urb2d',ts_urb2d(i,j)
!                    print*,'ust',ust(i,j)
!          endif
        enddo
        enddo


       endif                                                           !Bep end

!------------------------------------------------------
   END SUBROUTINE lsm
!------------------------------------------------------

  SUBROUTINE LSMINIT(VEGFRA,SNOW,SNOWC,SNOWH,CANWAT,SMSTAV,    &
                     SMSTOT, SFCRUNOFF,UDRUNOFF,ACSNOW,        &
                     ACSNOM,IVGTYP,ISLTYP,TSLB,SMOIS,SH2O,ZS,DZS, &
                     MMINLU,                                   &
                     SNOALB, FNDSOILW, FNDSNOWH, RDMAXALB,     &
                     num_soil_layers, restart,                 &
                     allowed_to_read ,                         &
                     ids,ide, jds,jde, kds,kde,                &
                     ims,ime, jms,jme, kms,kme,                &
                     its,ite, jts,jte, kts,kte                 )

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN)       ::     num_soil_layers

   LOGICAL , INTENT(IN) :: restart , allowed_to_read

   REAL,    DIMENSION( num_soil_layers), INTENT(INOUT) :: ZS, DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(INOUT)    ::                          SMOIS, &  !Total soil moisture
                                                         SH2O,  &  !liquid soil moisture
                                                         TSLB      !STEMP

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

   character*256 :: MMINSL
        MMINSL='STAS'
!

! initialize three Noah LSM related tables
   IF ( allowed_to_read ) THEN
     CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
     CALL  SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
   ENDIF

! GAC-->
! 20130219 - No longer need these - see module_data_gocart_dust
!#if ( WRF_CHEM == 1 )
!!
!! need this parameter for dust parameterization in wrf/chem
!!
!   do I=1,NSLTYPE
!      porosity(i)=maxsmc(i)
!      drypoint(i)=drysmc(i)
!   enddo
!#endif
! <--GAC

   IF(.not.restart)THEN

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
#if ( HWRF == 1 ) 
      CALL wrf_message( "WARNING: message only; was fatal. module_sf_noahdrv.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
#else
      CALL wrf_error_fatal( "module_sf_noahdrv.F: lsminit: out of range value "// &
                            "of ISLTYP. Is this field in the input?" )
#endif
   ENDIF

! initialize soil liquid water content SH2O

!  IF(.NOT.FNDSOILW) THEN

! If no SWC, do the following
!         PRINT *,'SOIL WATER NOT FOUND - VALUE SET IN LSMINIT'
        DO J = jts,jtf
        DO I = its,itf
          BX = BB(ISLTYP(I,J))
          SMCMAX = MAXSMC(ISLTYP(I,J))
          PSISAT = SATPSI(ISLTYP(I,J))
         if ((bx > 0.0).and.(smcmax > 0.0).and.(psisat > 0.0)) then
          DO NS=1, num_soil_layers
! ----------------------------------------------------------------------
!SH2O  <= SMOIS for T < 273.149K (-0.001C)
             IF (TSLB(I,NS,J) < 273.149) THEN
! ----------------------------------------------------------------------
! first guess following explicit solution for Flerchinger Eqn from Koren
! et al, JGR, 1999, Eqn 17 (KCOUNT=0 in FUNCTION FRH2O).
! ISLTPK is soil type
              BX = BB(ISLTYP(I,J))
              SMCMAX = MAXSMC(ISLTYP(I,J))
              PSISAT = SATPSI(ISLTYP(I,J))
              IF ( BX >  BLIM ) BX = BLIM
              FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                 ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
              IF (FK < 0.02) FK = 0.02
              SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )
! ----------------------------------------------------------------------
! now use iterative solution for liquid soil water content using
! FUNCTION FRH2O with the initial guess for SH2O from above explicit
! first guess.
              CALL FRH2O (FREE,TSLB(I,NS,J),SMOIS(I,NS,J),SH2O(I,NS,J),    &
                 SMCMAX,BX,PSISAT)
              SH2O(I,NS,J) = FREE
             ELSE             ! of IF (TSLB(I,NS,J)
! ----------------------------------------------------------------------
! SH2O = SMOIS ( for T => 273.149K (-0.001C)
              SH2O(I,NS,J)=SMOIS(I,NS,J)
! ----------------------------------------------------------------------
             ENDIF            ! of IF (TSLB(I,NS,J)
          END DO              ! of DO NS=1, num_soil_layers
         else                 ! of if ((bx > 0.0)
          DO NS=1, num_soil_layers
           SH2O(I,NS,J)=SMOIS(I,NS,J)
          END DO
         endif                ! of if ((bx > 0.0)
        ENDDO                 ! DO I = its,itf
        ENDDO                 ! DO J = jts,jtf
!  ENDIF                       ! of IF(.NOT.FNDSOILW)THEN

! initialize physical snow height SNOWH

        IF(.NOT.FNDSNOWH)THEN
! If no SNOWH do the following
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
          DO I = its,itf
            SNOWH(I,J)=SNOW(I,J)*0.005               ! SNOW in mm and SNOWH in m
          ENDDO
          ENDDO
        ENDIF

! initialize canopy water to ZERO

!          GO TO 110
!         print*,'Note that canopy water content (CANWAT) is set to ZERO in LSMINIT'
          DO J = jts,jtf
          DO I = its,itf
            CANWAT(I,J)=0.0
          ENDDO
          ENDDO
 110      CONTINUE

   ENDIF
!------------------------------------------------------------------------------
  END SUBROUTINE lsminit
!------------------------------------------------------------------------------



!-----------------------------------------------------------------
        SUBROUTINE SOIL_VEG_GEN_PARM( MMINLU, MMINSL)
!-----------------------------------------------------------------

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


!-----SPECIFY VEGETATION RELATED CHARACTERISTICS :
!             ALBBCK: SFC albedo (in percentage)
!                 Z0: Roughness length (m)
!             SHDFAC: Green vegetation fraction (in percentage)
!  Note: The ALBEDO, Z0, and SHDFAC values read from the following table
!          ALBEDO, amd Z0 are specified in LAND-USE TABLE; and SHDFAC is
!          the monthly green vegetation data
!             CMXTBL: MAX CNPY Capacity (m)
!             NROTBL: Rooting depth (layer)
!              RSMIN: Mimimum stomatal resistance (s m-1)
!              RSMAX: Max. stomatal resistance (s m-1)
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature. (K)
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculati
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100% snow cover
!                LAI: Leaf area index (dimensionless)
!             MAXALB: Upper bound on maximum albedo over deep snow
!
!-----READ IN VEGETAION PROPERTIES FROM VEGPARM.TBL
!

       IF ( wrf_dm_on_monitor() ) THEN

        OPEN(19, FILE='VEGPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening VEGPARM.TBL'
          CALL wrf_error_fatal ( message )
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
                    CALL wrf_error_fatal ( 'Too many loops in VEGPARM.TBL')
                 ENDIF
              ENDDO FIND_VEGETATION_PARAMETER_FLAG
           ENDIF
        ENDDO FIND_LUTYPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
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
           CALL wrf_error_fatal('Table sizes too small for value of LUCATS in module_sf_noahdrv.F')
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
!
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
             CALL wrf_error_fatal ("This could be caused by using an older VEGPARM.TBL file with a newer WRF source code.")
          ENDIF
          READ (19,*)LOW_DENSITY_RESIDENTIAL
          READ (19,*)
          READ (19,*)HIGH_DENSITY_RESIDENTIAL
          READ (19,*)
          READ (19,*)HIGH_INTENSITY_INDUSTRIAL
        ENDIF
!
 2002   CONTINUE

        CLOSE (19)
        IF (LUMATCH == 0) then
           CALL wrf_error_fatal ("Land Use Dataset '"//MMINLU//"' not found in VEGPARM.TBL.")
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
      CALL wrf_dm_bcast_integer ( LOW_DENSITY_RESIDENTIAL , 1 )
      CALL wrf_dm_bcast_integer ( HIGH_DENSITY_RESIDENTIAL , 1 )
      CALL wrf_dm_bcast_integer ( HIGH_INTENSITY_INDUSTRIAL , 1 )

!
!-----READ IN SOIL PROPERTIES FROM SOILPARM.TBL
!
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='SOILPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening SOILPARM.TBL'
          CALL wrf_error_fatal ( message )
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
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
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
           CALL wrf_error_fatal('Table sizes too small for value of SLCATS in module_sf_noahdrv.F')
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
      CALL wrf_dm_bcast_string  ( MMINSL  , 4 )  ! since this is reset above, see oct2 ^
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
          CALL wrf_error_fatal ( 'INCONSISTENT OR MISSING SOILPARM FILE' )
      ENDIF

!
!-----READ IN GENERAL PARAMETERS FROM GENPARM.TBL
!
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(19, FILE='GENPARM.TBL',FORM='FORMATTED',STATUS='OLD',IOSTAT=ierr)
        IF(ierr .NE. OPEN_OK ) THEN
          WRITE(message,FMT='(A)') &
          'module_sf_noahlsm.F: soil_veg_gen_parm: failure opening GENPARM.TBL'
          CALL wrf_error_fatal ( message )
        END IF

        READ (19,*)
        READ (19,*)
        READ (19,*) NUM_SLOPE

          SLPCATS=NUM_SLOPE
! prevent possible array overwrite, Bill Bovermann, IBM, May 6, 2008
          IF ( SIZE(slope_data) < NUM_SLOPE ) THEN
            CALL wrf_error_fatal('NUM_SLOPE too large for slope_data array in module_sf_noahdrv')
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


!-----------------------------------------------------------------
      END SUBROUTINE SOIL_VEG_GEN_PARM
!-----------------------------------------------------------------

!===========================================================================
!
! subroutine lsm_mosaic: a tiling approach for Noah LSM
!
!=========================================================================== 

SUBROUTINE lsm_mosaic(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
                  HFX,QFX,LH,GRDFLX, QGH,GSW,SWDOWN,GLW,SMSTAV,SMSTOT, &
                  SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,ISURBAN,ISICE,VEGFRA,    &
                  ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE,EMISS,EMBCK,   &
                  SNOWC,QSFC,RAINBL,MMINLU,                     &
                  num_soil_layers,DT,DZS,ITIMESTEP,             &
                  SMOIS,TSLB,SNOW,CANWAT,                       &
                  CHS,CHS2,CQS2,CPM,ROVCP,SR,chklowq,lai,qz0,   & !H
                  myj,frpcpn,                                   &
                  SH2O,SNOWH,                                   & !H
                  U_PHY,V_PHY,                                  & !I
                  SNOALB,SHDMIN,SHDMAX,                         & !I
                  SNOTIME,                                      & !?
                  ACSNOM,ACSNOW,                                & !O
                  SNOPCX,                                       & !O
                  POTEVP,                                       & !O
                  SMCREL,                                       & !O
                  XICE_THRESHOLD,                               &
                  RDLAI2D,USEMONALB,                            &
                  RIB,                                          & !?
                  NOAHRES,OPT_THCND,                            &
                 NLCAT,landusef,landusef2,                       & ! danli mosaic
                 sf_surface_mosaic,mosaic_cat,mosaic_cat_index,  & ! danli mosaic 
                 TSK_mosaic,QSFC_mosaic,                         & ! danli mosaic 
                 TSLB_mosaic,SMOIS_mosaic,SH2O_mosaic,           & ! danli mosaic 
                 CANWAT_mosaic,SNOW_mosaic,                      & ! danli mosaic
                 SNOWH_mosaic,SNOWC_mosaic,                      & ! danli mosaic 
                 ALBEDO_mosaic,ALBBCK_mosaic,                    & ! danli mosaic
                 EMISS_mosaic, EMBCK_mosaic,                     & ! danli mosaic
                 ZNT_mosaic, Z0_mosaic,                          & ! danli mosaic 
                 HFX_mosaic,QFX_mosaic,                          & ! danli mosaic
                 LH_mosaic, GRDFLX_mosaic, SNOTIME_mosaic,       & ! danli mosaic                   
! Noah UA changes
                  ua_phys,flx4_2d,fvb_2d,fbur_2d,fgsn_2d,       &
                  ids,ide, jds,jde, kds,kde,                    &
                  ims,ime, jms,jme, kms,kme,                    &
                  its,ite, jts,jte, kts,kte,                    &
                  sf_urban_physics,                             &
                  CMR_SFCDIF,CHR_SFCDIF,CMC_SFCDIF,CHC_SFCDIF,  &
                  CMGR_SFCDIF,CHGR_SFCDIF,                      &
!Optional Urban
                  TR_URB2D,TB_URB2D,TG_URB2D,TC_URB2D,QC_URB2D, & !H urban
                  UC_URB2D,                                     & !H urban
                  XXXR_URB2D,XXXB_URB2D,XXXG_URB2D,XXXC_URB2D,  & !H urban
                  TRL_URB3D,TBL_URB3D,TGL_URB3D,                & !H urban
                  SH_URB2D,LH_URB2D,G_URB2D,RN_URB2D,TS_URB2D,  & !H urban
                  TR_URB2D_mosaic,TB_URB2D_mosaic,              & !H urban  danli mosaic
                  TG_URB2D_mosaic,TC_URB2D_mosaic,              & !H urban  danli mosaic
                  QC_URB2D_mosaic,UC_URB2D_mosaic,              & !H urban  danli mosaic                  
                  TRL_URB3D_mosaic,TBL_URB3D_mosaic,            & !H urban  danli mosaic
                  TGL_URB3D_mosaic,                             & !H urban  danli mosaic
                  SH_URB2D_mosaic,LH_URB2D_mosaic,              & !H urban  danli mosaic
                  G_URB2D_mosaic,RN_URB2D_mosaic,               & !H urban  danli mosaic
                  TS_URB2D_mosaic,                              & !H urban  danli mosaic
                  TS_RUL2D_mosaic,                              & !H urban  danli mosaic                  
                  PSIM_URB2D,PSIH_URB2D,U10_URB2D,V10_URB2D,    & !O urban
                  GZ1OZ0_URB2D,  AKMS_URB2D,                    & !O urban
                  TH2_URB2D,Q2_URB2D, UST_URB2D,                & !O urban
                  DECLIN_URB,COSZ_URB2D,OMG_URB2D,              & !I urban
                  XLAT_URB2D,                                   & !I urban
                  num_roof_layers, num_wall_layers,             & !I urban
                  num_road_layers, DZR, DZB, DZG,               & !I urban
                  CMCR_URB2D,TGR_URB2D,TGRL_URB3D,SMR_URB3D,    & !H urban
                  julian,julyr,                                 & !H urban
                  DRELR_URB2D,DRELB_URB2D,DRELG_URB2D,          & !H urban
                  FLXHUMR_URB2D,FLXHUMB_URB2D,FLXHUMG_URB2D,    & !H urban
                  FRC_URB2D,UTYPE_URB2D,                        & !O
                  num_urban_layers,                             & !I multi-layer urban
                  num_urban_hi,                                 & !I multi-layer urban
                  trb_urb4d,tw1_urb4d,tw2_urb4d,tgb_urb4d,      & !H multi-layer urban
                  tlev_urb3d,qlev_urb3d,                        & !H multi-layer urban
                  tw1lev_urb3d,tw2lev_urb3d,                    & !H multi-layer urban
                  tglev_urb3d,tflev_urb3d,                      & !H multi-layer urban
                  sf_ac_urb3d,lf_ac_urb3d,cm_ac_urb3d,          & !H multi-layer urban
                  sfvent_urb3d,lfvent_urb3d,                    & !H multi-layer urban
                  sfwin1_urb3d,sfwin2_urb3d,                    & !H multi-layer urban
                  sfw1_urb3d,sfw2_urb3d,sfr_urb3d,sfg_urb3d,    & !H multi-layer urban
                  lp_urb2d,hi_urb2d,lb_urb2d,hgt_urb2d,         & !H multi-layer urban
                  mh_urb2d,stdh_urb2d,lf_urb2d,                 & !SLUCM
                  th_phy,rho,p_phy,ust,                         & !I multi-layer urban
                  gmt,julday,xlong,xlat,                        & !I multi-layer urban
                  a_u_bep,a_v_bep,a_t_bep,a_q_bep,              & !O multi-layer urban
                  a_e_bep,b_u_bep,b_v_bep,                      & !O multi-layer urban
                  b_t_bep,b_q_bep,b_e_bep,dlg_bep,              & !O multi-layer urban
                  dl_u_bep,sf_bep,vl_bep                        & !O multi-layer urban
                 ,sfcheadrt,INFXSRT, soldrain                   & !hydro
                 ,SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM, fasdas     &   !fasdas
                  )

!----------------------------------------------------------------
    IMPLICIT NONE
!----------------------------------------------------------------
!----------------------------------------------------------------
! --- atmospheric (WRF generic) variables
!-- DT          time step (seconds)
!-- DZ8W        thickness of layers (m)
!-- T3D         temperature (K)
!-- QV3D        3D water vapor mixing ratio (Kg/Kg)
!-- P3D         3D pressure (Pa)
!-- FLHC        exchange coefficient for heat (m/s)
!-- FLQC        exchange coefficient for moisture (m/s)
!-- PSFC        surface pressure (Pa)
!-- XLAND       land mask (1 for land, 2 for water)
!-- QGH         saturated mixing ratio at 2 meter
!-- GSW         downward short wave flux at ground surface (W/m^2)
!-- GLW         downward long wave flux at ground surface (W/m^2)
!-- History variables
!-- CANWAT      canopy moisture content (mm)
!-- TSK         surface temperature (K)
!-- TSLB        soil temp (k)
!-- SMOIS       total soil moisture content (volumetric fraction)
!-- SH2O        unfrozen soil moisture content (volumetric fraction)
!                note: frozen soil moisture (i.e., soil ice) = SMOIS - SH2O
!-- SNOWH       actual snow depth (m)
!-- SNOW        liquid water-equivalent snow depth (m)
!-- ALBEDO      time-varying surface albedo including snow effect (unitless fraction)
!-- ALBBCK      background surface albedo (unitless fraction)
!-- CHS          surface exchange coefficient for heat and moisture (m s-1);
!-- CHS2        2m surface exchange coefficient for heat  (m s-1);
!-- CQS2        2m surface exchange coefficient for moisture (m s-1);
! --- soil variables
!-- num_soil_layers   the number of soil layers
!-- ZS          depths of centers of soil layers   (m)
!-- DZS         thicknesses of soil layers (m)
!-- SLDPTH      thickness of each soil layer (m, same as DZS)
!-- TMN         soil temperature at lower boundary (K)
!-- SMCWLT      wilting point (volumetric)
!-- SMCDRY      dry soil moisture threshold where direct evap from
!               top soil layer ends (volumetric)
!-- SMCREF      soil moisture threshold below which transpiration begins to
!                   stress (volumetric)
!-- SMCMAX      porosity, i.e. saturated value of soil moisture (volumetric)
!-- NROOT       number of root layers, a function of veg type, determined
!               in subroutine redprm.
!-- SMSTAV      Soil moisture availability for evapotranspiration (
!                   fraction between SMCWLT and SMCMXA)
!-- SMSTOT      Total soil moisture content frozen+unfrozen) in the soil column (mm)
! --- snow variables
!-- SNOWC       fraction snow coverage (0-1.0)
! --- vegetation variables
!-- SNOALB      upper bound on maximum albedo over deep snow
!-- SHDMIN      minimum areal fractional coverage of annual green vegetation
!-- SHDMAX      maximum areal fractional coverage of annual green vegetation
!-- XLAI        leaf area index (dimensionless)
!-- Z0BRD       Background fixed roughness length (M)
!-- Z0          Background vroughness length (M) as function
!-- ZNT         Time varying roughness length (M) as function
!-- ALBD(IVGTPK,ISN) background albedo reading from a table
! --- LSM output
!-- HFX         upward heat flux at the surface (W/m^2)
!-- QFX         upward moisture flux at the surface (kg/m^2/s)
!-- LH          upward moisture flux at the surface (W m-2)
!-- GRDFLX(I,J) ground heat flux (W m-2)
!-- FDOWN       radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
!----------------------------------------------------------------------------
!-- EC          canopy water evaporation ((W m-2)
!-- EDIR        direct soil evaporation (W m-2)
!-- ET          plant transpiration from a particular root layer (W m-2)
!-- ETT         total plant transpiration (W m-2)
!-- ESNOW       sublimation from (or deposition to if <0) snowpack (W m-2)
!-- DRIP        through-fall of precip and/or dew in excess of canopy
!                 water-holding capacity (m)
!-- DEW         dewfall (or frostfall for t<273.15) (M)
!-- SMAV        Soil Moisture Availability for each layer, as a fraction
!                 between SMCWLT and SMCMAX (dimensionless fraction)
! ----------------------------------------------------------------------
!-- BETA        ratio of actual/potential evap (dimensionless)
!-- ETP         potential evaporation (W m-2)
! ----------------------------------------------------------------------
!-- FLX1        precip-snow sfc (W m-2)
!-- FLX2        freezing rain latent heat flux (W m-2)
!-- FLX3        phase-change heat flux from snowmelt (W m-2)
! ----------------------------------------------------------------------
!-- ACSNOM      snow melt (mm) (water equivalent)
!-- ACSNOW      accumulated snow fall (mm) (water equivalent)
!-- SNOPCX      snow phase change heat flux (W/m^2)
!-- POTEVP      accumulated potential evaporation (m)
!-- RIB         Documentation needed!!!
! ----------------------------------------------------------------------
!-- RUNOFF1     surface runoff (m s-1), not infiltrating the surface
!-- RUNOFF2     subsurface runoff (m s-1), drainage out bottom of last
!                  soil layer (baseflow)
!  important note: here RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3
!-- RUNOFF3     numerical trunctation in excess of porosity (smcmax)
!                  for a given soil layer at the end of a time step (m s-1).
!SFCRUNOFF     Surface Runoff (mm)
!UDRUNOFF      Total Underground Runoff (mm), which is the sum of RUNOFF2 and RUNOFF3
! ----------------------------------------------------------------------
!-- RC          canopy resistance (s m-1)
!-- PC          plant coefficient (unitless fraction, 0-1) where PC*ETP = actual transp
!-- RSMIN       minimum canopy resistance (s m-1)
!-- RCS         incoming solar rc factor (dimensionless)
!-- RCT         air temperature rc factor (dimensionless)
!-- RCQ         atmos vapor pressure deficit rc factor (dimensionless)
!-- RCSOIL      soil moisture rc factor (dimensionless)

!-- EMISS       surface emissivity (between 0 and 1)
!-- EMBCK       Background surface emissivity (between 0 and 1)

!-- ROVCP       R/CP
!               (R_d/R_v) (dimensionless)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!
!-- SR          fraction of frozen precip (0.0 to 1.0)
!----------------------------------------------------------------

! IN only

   INTEGER,  INTENT(IN   )   ::     ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte

   INTEGER,  INTENT(IN   )   ::  sf_urban_physics               !urban
   INTEGER,  INTENT(IN   )   ::  isurban
   INTEGER,  INTENT(IN   )   ::  isice
   INTEGER,  INTENT(IN   )   ::  julian,julyr

!added by Wei Yu  for routing
    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
             INTENT(INOUT)  :: sfcheadrt,INFXSRT,soldrain 
    real :: etpnd1
!end added

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(IN   )    ::                            TMN, &
                                                         XLAND, &
                                                          XICE, &
                                                        VEGFRA, &
                                                        SHDMIN, &
                                                        SHDMAX, &
                                                        SNOALB, &
                                                           GSW, &
                                                        SWDOWN, & !added 10 jan 2007
                                                           GLW, &
                                                        RAINBL, &
                                                        SR

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                         ALBBCK, &
                                                            Z0, &
                                                         EMBCK                  ! danli mosaic
                                                         
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
            INTENT(INOUT   )    ::                         IVGTYP                   ! for mosaic danli

   INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP

   REAL,     INTENT(IN   )   ::     DT,ROVCP

   REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

! IN and OUT

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(INOUT)   ::                          SMOIS, & ! total soil moisture
                                                         SH2O,  & ! new soil liquid
                                                         TSLB     ! TSLB     STEMP

   REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
             INTENT(OUT)     ::                         SMCREL

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)    ::                            TSK, & !was TGB (temperature)
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
                                                         SNOWH, & !new
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

! Noah UA changes
   LOGICAL,                                INTENT(IN)  :: UA_PHYS
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: FLX4_2D,FVB_2D,FBUR_2D,FGSN_2D
   REAL                                                :: FLX4,FVB,FBUR,FGSN

   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
               INTENT(OUT)    ::                        CHKLOWQ
   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: LAI
   REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::        QZ0

   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHGR_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CMC_SFCDIF
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: CHC_SFCDIF
! Local variables (moved here from driver to make routine thread safe, 20031007 jm)

      REAL, DIMENSION(1:num_soil_layers) ::  ET

      REAL, DIMENSION(1:num_soil_layers) ::  SMAV

      REAL  ::  BETA, ETP, SSOIL,EC, EDIR, ESNOW, ETT,        &
                FLX1,FLX2,FLX3, DRIP,DEW,FDOWN,RC,PC,RSMIN,XLAI,  &
!                RCS,RCT,RCQ,RCSOIL
                RCS,RCT,RCQ,RCSOIL,FFROZP

    LOGICAL,    INTENT(IN   )    ::     myj,frpcpn

! DECLARATIONS - LOGICAL
! ----------------------------------------------------------------------
      LOGICAL, PARAMETER :: LOCAL=.false.
      LOGICAL :: FRZGRA, SNOWNG

      LOGICAL :: IPRINT

! ----------------------------------------------------------------------
! DECLARATIONS - INTEGER
! ----------------------------------------------------------------------
      INTEGER :: I,J, ICE,NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER :: NROOT
      INTEGER :: KZ ,K
      INTEGER :: NS
! ----------------------------------------------------------------------
! DECLARATIONS - REAL
! ----------------------------------------------------------------------

      REAL  :: SHMIN,SHMAX,DQSDT2,LWDN,PRCP,PRCPRAIN,                    &
               Q2SAT,Q2SATI,SFCPRS,SFCSPD,SFCTMP,SHDFAC,SNOALB1,         &
               SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ALBEDOK, ETA, ETA_KINEMATIC, &
               EMBRD,                                                    &
               Z0K,RUNOFF1,RUNOFF2,RUNOFF3,SHEAT,SOLNET,E2SAT,SFCTSNO,   &
! mek, WRF testing, expanded diagnostics
               SOLUP,LWUP,RNET,RES,Q1SFC,TAIRV,SATFLG
! MEK MAY 2007
      REAL ::  FDTLIW
! MEK JUL2007 for pot. evap.
      REAL :: RIBB
      REAL ::  FDTW

      REAL  :: EMISSI

      REAL  :: SNCOVR,SNEQV,SNOWHK,CMC, CHK,TH2

      REAL  :: SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT,SOILM,SOILW,Q1,T1
      REAL  :: SNOTIME1    ! LSTSNW1 INITIAL NUMBER OF TIMESTEPS SINCE LAST SNOWFALL

      REAL  :: DUMMY,Z0BRD
!
      REAL  :: COSZ, SOLARDIRECT
!
      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC
!
      REAL, DIMENSION(1:num_soil_layers) ::     ZSOIL, RTDIS
      REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
                          T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)
! MEK MAY 2007
      REAL, PARAMETER  :: ROW=1.E3,ELIW=XLF,ROWLIW=ROW*ELIW

! ----------------------------------------------------------------------
! DECLARATIONS START - urban
! ----------------------------------------------------------------------

! input variables surface_driver --> lsm
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

! input variables lsm --> urban
     INTEGER :: UTYPE_URB ! urban type [urban=1, suburban=2, rural=3]
     REAL :: TA_URB       ! potential temp at 1st atmospheric level [K]
     REAL :: QA_URB       ! mixing ratio at 1st atmospheric level  [kg/kg]
     REAL :: UA_URB       ! wind speed at 1st atmospheric level    [m/s]
     REAL :: U1_URB       ! u at 1st atmospheric level             [m/s]
     REAL :: V1_URB       ! v at 1st atmospheric level             [m/s]
     REAL :: SSG_URB      ! downward total short wave radiation    [W/m/m]
     REAL :: LLG_URB      ! downward long wave radiation           [W/m/m]
     REAL :: RAIN_URB     ! precipitation                          [mm/h]
     REAL :: RHOO_URB     ! air density                            [kg/m^3]
     REAL :: ZA_URB       ! first atmospheric level                [m]
     REAL :: DELT_URB     ! time step                              [s]
     REAL :: SSGD_URB     ! downward direct short wave radiation   [W/m/m]
     REAL :: SSGQ_URB     ! downward diffuse short wave radiation  [W/m/m]
     REAL :: XLAT_URB     ! latitude                               [deg]
     REAL :: COSZ_URB     ! cosz
     REAL :: OMG_URB      ! hour angle
     REAL :: ZNT_URB      ! roughness length                       [m]
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
     REAL, DIMENSION(1:num_roof_layers) :: TRL_URB  ! roof layer temp [K]
     REAL, DIMENSION(1:num_wall_layers) :: TBL_URB  ! wall layer temp [K]
     REAL, DIMENSION(1:num_road_layers) :: TGL_URB  ! road layer temp [K]
     LOGICAL  :: LSOLAR_URB

!===Yang,2014/10/08,hydrological variable for single layer UCM===
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
     REAL, DIMENSION(1:num_roof_layers) :: SMR_URB  ! green roof layer moisture
     REAL, DIMENSION(1:num_roof_layers) :: TGRL_URB ! green roof layer temp [K]

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

! state variable surface_driver <--> lsm <--> urban
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
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: TS_URB2D

     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_roof_layers, jms:jme ), INTENT(INOUT) :: TRL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_wall_layers, jms:jme ), INTENT(INOUT) :: TBL_URB3D
     REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_road_layers, jms:jme ), INTENT(INOUT) :: TGL_URB3D

! output variable lsm --> surface_driver
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIM_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: PSIH_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: GZ1OZ0_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: U10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: V10_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: TH2_URB2D
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: Q2_URB2D
!
     REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: AKMS_URB2D
!
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: UST_URB2D
     REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: FRC_URB2D                ! change this to inout, danli mosaic
     INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: UTYPE_URB2D

! output variables urban --> lsm
     REAL :: TS_URB     ! surface radiative temperature    [K]
     REAL :: QS_URB     ! surface humidity                 [-]
     REAL :: SH_URB     ! sensible heat flux               [W/m/m]
     REAL :: LH_URB     ! latent heat flux                 [W/m/m]
     REAL :: LH_KINEMATIC_URB ! latent heat flux, kinetic  [kg/m/m/s]
     REAL :: SW_URB     ! upward short wave radiation flux [W/m/m]
     REAL :: ALB_URB    ! time-varying albedo            [fraction]
     REAL :: LW_URB     ! upward long wave radiation flux  [W/m/m]
     REAL :: G_URB      ! heat flux into the ground        [W/m/m]
     REAL :: RN_URB     ! net radiation                    [W/m/m]
     REAL :: PSIM_URB   ! shear f for momentum             [-]
     REAL :: PSIH_URB   ! shear f for heat                 [-]
     REAL :: GZ1OZ0_URB   ! shear f for heat                 [-]
     REAL :: U10_URB    ! wind u component at 10 m         [m/s]
     REAL :: V10_URB    ! wind v component at 10 m         [m/s]
     REAL :: TH2_URB    ! potential temperature at 2 m     [K]
     REAL :: Q2_URB     ! humidity at 2 m                  [-]
     REAL :: CHS_URB
     REAL :: CHS2_URB
     REAL :: UST_URB
! NUDAPT Parameters urban --> lam
     REAL :: mh_urb
     REAL :: stdh_urb
     REAL :: lp_urb
     REAL :: hgt_urb
     REAL, DIMENSION(4) :: lf_urb
! Variables for multi-layer UCM (Martilli et al. 2002)
   REAL, OPTIONAL, INTENT(IN  )   ::                                   GMT 
   INTEGER, OPTIONAL, INTENT(IN  ) ::                               JULDAY
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   )        ::XLAT, XLONG
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_LAYERS
   INTEGER, INTENT(IN  ) ::                               NUM_URBAN_HI
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: trb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tgb_urb4d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: qlev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw1lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tw2lev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tglev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: tflev_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sf_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: cm_ac_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: sfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: lfvent_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfwin2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw1_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfw2_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfr_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_layers, jms:jme ), INTENT(INOUT) :: sfg_urb3d
   REAL, OPTIONAL, DIMENSION( ims:ime, 1:num_urban_hi, jms:jme ), INTENT(IN) :: hi_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lp_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: lb_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: hgt_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: mh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: stdh_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, 4, jms:jme ), INTENT(IN) :: lf_urb2d
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_u_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_v_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_t_bep   !Implicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_q_bep   !Implicit momemtum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::a_e_bep   !Implicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_u_bep   !Explicit momentum component X-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_v_bep   !Explicit momentum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_t_bep   !Explicit component pot. temperature
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_q_bep   !Implicit momemtum component Y-direction
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::b_e_bep   !Explicit component TKE
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::vl_bep    !Fraction air volume in grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dlg_bep   !Height above ground
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::sf_bep  !Fraction air at the face of grid cell
   REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT) ::dl_u_bep  !Length scale

! Local variables for multi-layer UCM (Martilli et al. 2002)
   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_RURAL,LH_RURAL,GRDFLX_RURAL ! ,RN_RURAL
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_RURAL ! ,QSFC_RURAL,UMOM_RURAL,VMOM_RURAL
   REAL,    DIMENSION( its:ite, jts:jte ) :: ALB_RURAL,EMISS_RURAL,TSK_RURAL ! ,UST_RURAL
!   REAL,    DIMENSION( ims:ime, jms:jme ) :: QSFC_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: HFX_URB,UMOM_URB,VMOM_URB
   REAL,    DIMENSION( its:ite, jts:jte ) :: QFX_URB
!   REAL,    DIMENSION( ims:ime, jms:jme ) :: ALBEDO_URB,EMISS_URB,UMOM,VMOM,UST
   REAL, DIMENSION(its:ite,jts:jte) ::EMISS_URB
   REAL, DIMENSION(its:ite,jts:jte) :: RL_UP_URB
   REAL, DIMENSION(its:ite,jts:jte) ::RS_ABS_URB
   REAL, DIMENSION(its:ite,jts:jte) ::GRDFLX_URB
   REAL :: SIGMA_SB,RL_UP_RURAL,RL_UP_TOT,RS_ABS_TOT,UMOM,VMOM
   REAL :: r1,r2,r3
   REAL :: CMR_URB, CHR_URB, CMC_URB, CHC_URB, CMGR_URB, CHGR_URB
   REAL :: frc_urb,lb_urb
   REAL :: check 
! ----------------------------------------------------------------------
! DECLARATIONS END - urban
! ----------------------------------------------------------------------
!-------------------------------------------------
! Noah-mosaic related variables are added to declaration  (danli)
!-------------------------------------------------
  
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
  REAL, DIMENSION( ims:ime, jms:jme ) :: TSK_mosaic_avg, QSFC_mosaic_avg, CANWAT_mosaic_avg,SNOW_mosaic_avg,SNOWH_mosaic_avg, &
                                         SNOWC_mosaic_avg, HFX_mosaic_avg, QFX_mosaic_avg, LH_mosaic_avg, GRDFLX_mosaic_avg,  &
                                         ALBEDO_mosaic_avg, ALBBCK_mosaic_avg, EMISS_mosaic_avg, EMBCK_mosaic_avg,            &
                                         ZNT_mosaic_avg, Z0_mosaic_avg, SNOTIME_mosaic_avg, FAREA_mosaic_avg  
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
!-------------------------------------------------
! Noah-mosaic related variables declaration end (danli)
!-------------------------------------------------

  REAL, PARAMETER  :: CAPA=R_D/CP
  REAL :: APELM,APES,SFCTH2,PSFC
  real, intent(in) :: xice_threshold
  character(len=80) :: message_text
!
!  FASDAS: it doesn't work for mosaic, but we need the variables to call sflx
!
   REAL,    DIMENSION( ims:ime, jms:jme )                     , &
            INTENT(INOUT)  ::   SDA_HFX, SDA_QFX, HFX_BOTH, QFX_BOTH, QNORM
   INTEGER, INTENT(IN   )  ::  fasdas
   REAL                    ::  XSDA_HFX, XSDA_QFX, XQNORM
   REAL                    ::  HFX_PHY, QFX_PHY
   REAL                    ::  DZQ
   REAL                    ::  HCPCT_FASDAS

   HFX_PHY = 0.0   ! initialize
   QFX_PHY = 0.0
   XQNORM  = 0.0
!
!  END FASDAS
!
! MEK MAY 2007
      FDTLIW=DT/ROWLIW
! MEK JUL2007
      FDTW=DT/(XLV*RHOWATER)
! debug printout
      IPRINT=.false.
      IPRINT_mosaic=.false.

!      SLOPETYP=2
      SLOPETYP=1
!      SHDMIN=0.00

      NSOIL=num_soil_layers

     DO NS=1,NSOIL
     SLDPTH(NS)=DZS(NS)
     ENDDO

     JLOOP : DO J=jts,jte

      IF(ITIMESTEP.EQ.1)THEN
        DO 50 I=its,ite
!*** initialize soil conditions for IHOP 31 May case
!         IF((XLAND(I,J)-1.5) < 0.)THEN
!            if (I==108.and.j==85) then
!                  DO NS=1,NSOIL
!                      SMOIS(I,NS,J)=0.10
!                      SH2O(I,NS,J)=0.10
!                  enddo
!             endif
!         ENDIF

!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
          IF((XLAND(I,J)-1.5).GE.0.)THEN
! check sea-ice point
#if 0
            IF( XICE(I,J).GE. XICE_THRESHOLD .and. IPRINT ) PRINT*, ' sea-ice at water point, I=',I,'J=',J
#endif
!***   Open Water Case
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            DO NS=1,NSOIL
              SMOIS(I,NS,J)=1.0
              TSLB(I,NS,J)=273.16                                          !STEMP
              SMCREL(I,NS,J)=1.0
            ENDDO
          ELSE
            IF ( XICE(I,J) .GE. XICE_THRESHOLD ) THEN
!***        SEA-ICE CASE
              SMSTAV(I,J)=1.0
              SMSTOT(I,J)=1.0
              DO NS=1,NSOIL
                SMOIS(I,NS,J)=1.0
                SMCREL(I,NS,J)=1.0
              ENDDO
            ENDIF
          ENDIF
!
   50   CONTINUE
      ENDIF                                                               ! end of initialization over ocean

!-----------------------------------------------------------------------
      ILOOP : DO I=its,ite
      
         IF (((XLAND(I,J)-1.5).LT.0.) .AND. (XICE(I,J) < XICE_THRESHOLD) ) THEN
               
               IVGTYP_dominant(I,J)=IVGTYP(I,J)                                   ! save this
      
            ! INITIALIZE THE AREA-AVERAGED FLUXES 
               
                 TSK_mosaic_avg(i,j)= 0                              ! from 3D to 2D
                 QSFC_mosaic_avg(i,j)= 0
                 CANWAT_mosaic_avg(i,j)= 0
                 SNOW_mosaic_avg(i,j)= 0
                 SNOWH_mosaic_avg(i,j)= 0
                 SNOWC_mosaic_avg(i,j)= 0
            
                          DO NS=1,NSOIL
            
                     TSLB_mosaic_avg(i,NS,j)=0
                     SMOIS_mosaic_avg(i,NS,j)=0
                     SH2O_mosaic_avg(i,NS,j)=0
            
                         ENDDO
            
                 HFX_mosaic_avg(i,j)= 0
                 QFX_mosaic_avg(i,j)= 0  
                 LH_mosaic_avg(i,j)=  0 
                 GRDFLX_mosaic_avg(i,j)= 0
                 ALBEDO_mosaic_avg(i,j)=0
                 ALBBCK_mosaic_avg(i,j)=0
                 EMISS_mosaic_avg(i,j)=0
                 EMBCK_mosaic_avg(i,j)=0
                 ZNT_mosaic_avg(i,j)=0
                 Z0_mosaic_avg(i,j)=0  
                 FAREA_mosaic_avg(i,j)=0  
            
            ! add a new loop for the mosaic_cat
            
               DO mosaic_i = mosaic_cat, 1, -1
               
            !   if (mosaic_cat_index(I,mosaic_i,J) .EQ. 16 ) then
            !   PRINT*, 'you still have water tiles at','i=',i,'j=',j, 'mosaic_i',mosaic_i
            !   PRINT*, 'xland',xland(i,j),'xice',xice(i,j)
            !   endif
               
               IVGTYP(I,J)=mosaic_cat_index(I,mosaic_i,J)                         ! replace it with the mosaic one          
               TSK(I,J)=TSK_mosaic(I,mosaic_i,J)                                  ! from 3D to 2D
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
            
            !-----------------------------------------------------------------------
            ! insert the NOAH model here for the non-urban one and the urban one  DANLI
            !-----------------------------------------------------------------------
            
      ! surface pressure
              PSFC=P8w3D(i,1,j)
      ! pressure in middle of lowest layer
              SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
      ! convert from mixing ratio to specific humidity
               Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))
      !
      !         Q2SAT=QGH(I,j)
               Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        ! Q2SAT is sp humidity
      ! add check on myj=.true.
      !        IF((Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
              IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
                SATFLG=0.
                CHKLOWQ(I,J)=0.
              ELSE
                SATFLG=1.0
                CHKLOWQ(I,J)=1.
              ENDIF
      
              SFCTMP=T3D(i,1,j)
              ZLVL=0.5*DZ8W(i,1,j)
      
      !        TH2=SFCTMP+(0.0097545*ZLVL)
      ! calculate SFCTH2 via Exner function vs lapse-rate (above)
               APES=(1.E5/PSFC)**CAPA
               APELM=(1.E5/SFCPRS)**CAPA
               SFCTH2=SFCTMP*APELM
               TH2=SFCTH2/APES
      !
               EMISSI = EMISS(I,J)
               LWDN=GLW(I,J)*EMISSI
      ! SOLDN is total incoming solar
              SOLDN=SWDOWN(I,J)
      ! GSW is net downward solar
      !        SOLNET=GSW(I,J)
      ! use mid-day albedo to determine net downward solar (no solar zenith angle correction)
              SOLNET=SOLDN*(1.-ALBEDO(I,J))
              PRCP=RAINBL(i,j)/DT
              VEGTYP=IVGTYP(I,J)
              SOILTYP=ISLTYP(I,J)
              SHDFAC=VEGFRA(I,J)/100.
              T1=TSK(I,J)
              CHK=CHS(I,J)
              SHMIN=SHDMIN(I,J)/100. !NEW
              SHMAX=SHDMAX(I,J)/100. !NEW
      ! convert snow water equivalent from mm to meter
              SNEQV=SNOW(I,J)*0.001
      ! snow depth in meters
              SNOWHK=SNOWH(I,J)
              SNCOVR=SNOWC(I,J)
      
      ! if "SR" present, set frac of frozen precip ("FFROZP") = snow-ratio ("SR", range:0-1)
      ! SR from e.g. Ferrier microphysics
      ! otherwise define from 1st atmos level temperature
             IF(FRPCPN) THEN
                FFROZP=SR(I,J)
              ELSE
                IF (SFCTMP <=  273.15) THEN
                  FFROZP = 1.0
      	  ELSE
      	    FFROZP = 0.0
      	  ENDIF
              ENDIF
      !***
              IF((XLAND(I,J)-1.5).GE.0.)THEN                                  ! begining of land/sea if block
      ! Open water points
                TSK_RURAL(I,J)=TSK(I,J)
                HFX_RURAL(I,J)=HFX(I,J)
                QFX_RURAL(I,J)=QFX(I,J)
                LH_RURAL(I,J)=LH(I,J)
                EMISS_RURAL(I,J)=EMISS(I,J)
                GRDFLX_RURAL(I,J)=GRDFLX(I,J)
              ELSE
      ! Land or sea-ice case
      
                IF (XICE(I,J) >= XICE_THRESHOLD) THEN
                   ! Sea-ice point
                   ICE = 1
                ELSE IF ( VEGTYP == ISICE ) THEN
                   ! Land-ice point
                   ICE = -1
                ELSE
                   ! Neither sea ice or land ice.
                   ICE=0
                ENDIF
                DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2
      
                IF(SNOW(I,J).GT.0.0)THEN
      ! snow on surface (use ice saturation properties)
                  SFCTSNO=SFCTMP
                  E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
                  Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
                  Q2SATI=Q2SATI/(1.0+Q2SATI)    ! spec. hum.
                  IF (T1 .GT. 273.14) THEN
      ! warm ground temps, weight the saturation between ice and water according to SNOWC
                    Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
                    DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
                  ELSE
      ! cold ground temps, use ice saturation only
                    Q2SAT=Q2SATI
                    DQSDT2=Q2SATI*6174./(SFCTSNO**2)
                  ENDIF
      ! for snow cover fraction at 0 C, ground temp will not change, so DQSDT2 effectively zero
                  IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
                ENDIF
      
                ! Land-ice or land points use the usual deep-soil temperature.
                TBOT=TMN(I,J)
      
                IF(VEGTYP.EQ.25) SHDFAC=0.0000
                IF(VEGTYP.EQ.26) SHDFAC=0.0000
                IF(VEGTYP.EQ.27) SHDFAC=0.0000
                IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
#if 0
               IF(IPRINT)PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
               IF(IPRINT)PRINT*,i,j,'RESET SOIL in surfce.F'
#endif
                  SOILTYP=7
                ENDIF
                SNOALB1 = SNOALB(I,J)
                CMC=CANWAT(I,J)
      
      !-------------------------------------------
      !*** convert snow depth from mm to meter
      !
      !          IF(RDMAXALB) THEN
      !           SNOALB=ALBMAX(I,J)*0.01
      !         ELSE
      !           SNOALB=MAXALB(IVGTPK)*0.01
      !         ENDIF
      
      !        SNOALB1=0.80
      !        SHMIN=0.00
              ALBBRD=ALBBCK(I,J)
              Z0BRD=Z0(I,J)
              EMBRD=EMBCK(I,J)
              SNOTIME1 = SNOTIME(I,J)
              RIBB=RIB(I,J)
      !FEI: temporaray arrays above need to be changed later by using SI
      
                DO NS=1,NSOIL
                  SMC(NS)=SMOIS(I,NS,J)
                  STC(NS)=TSLB(I,NS,J)                                          !STEMP
                  SWC(NS)=SH2O(I,NS,J)
                ENDDO
      !
                if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
                  SNOWHK= 5.*SNEQV
                endif
      !
      
      !Fei: urban. for urban surface, if calling UCM, redefine the natural surface in cities as
      ! the "NATURAL" category in the VEGPARM.TBL
      	
      	!   IF(SF_URBAN_PHYSICS == 1.OR. SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN
                       
      
           !           IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
           !            IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
           !	 VEGTYP = NATURAL
           !            SHDFAC = SHDTBL(NATURAL)
           !            ALBEDOK =0.2         !  0.2
           !            ALBBRD  =0.2         !0.2
           !            EMISSI = 0.98                                 !for VEGTYP=5
           !	 IF ( FRC_URB2D(I,J) < 0.99 ) THEN
          !               if(sf_urban_physics.eq.1)then
          !       T1= ( TSK(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
          !               elseif((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then
          !            r1= (tsk(i,j)**4.)
          !            r2= frc_urb2d(i,j)*(ts_urb2d(i,j)**4.)
          !            r3= (1.-frc_urb2d(i,j))
         !             t1= ((r1-r2)/r3)**.25
         !                endif
         !	         ELSE
         !		 T1 = TSK(I,J)
         !              ENDIF
         !             ENDIF
          !       ELSE
           !            IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
          !              IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
           !             VEGTYP = ISURBAN
           !      	 ENDIF
           !      ENDIF
      
            Noah_call=.TRUE.
      
            If ( SF_URBAN_PHYSICS == 0 ) THEN   ! ONLY NOAH
      
                 IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                       IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
                       Noah_call = .TRUE.                 
                       VEGTYP = ISURBAN
                 ENDIF
      
            ENDIF
            
            IF(SF_URBAN_PHYSICS == 1) THEN
      
                 IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                       IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN   
              	
                       Noah_call = .TRUE.                       
              	       VEGTYP = NATURAL                                              
                       SHDFAC = SHDTBL(NATURAL)
                       ALBEDOK =0.2         !  0.2
                       ALBBRD  =0.2         !  0.2
                       EMISSI = 0.98        !  for VEGTYP=5       
                       
      		      T1= TS_RUL2D_mosaic(I,mosaic_i,J)
                       
                 ENDIF
      
            ENDIF

!===Yang, 2014/10/08, hydrological processes for urban vegetation in single layer UCM===
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
           IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
            AOASIS = oasis  ! urban oasis effect
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  !irrigation on vegetaion in urban area, MAY-SEP, 9-10pm
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
          CALL wrf_error_fatal('Urban oasis option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          IF(IRIOPTION == 1) THEN
          CALL wrf_error_fatal('Urban irrigation option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          ENDIF
            
            IF( SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN
!             print*, 'MOSAIC is not designed to work with SF_URBAN_PHYSICS=2 or SF_URBAN_PHYSICS=3'
            ENDIF
      
         IF (Noah_call) THEN
#if 0
                IF(IPRINT) THEN
      !
             print*, 'BEFORE SFLX, in Noahlsm_driver'
             print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
             'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
              LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
              'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
               'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
               'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
               'SHMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB1',SNOALB1,'TBOT',&
                TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
                STC, 'SMC',SMC, 'SWC',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
                'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
                'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
                'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
                'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
                'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
                'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
                'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
                'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
                'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
                'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
                 endif
#endif
      
                IF (rdlai2d) THEN
                   xlai = lai(i,j)
                endif
      
          IF ( ICE == 1 ) THEN
      
             ! Sea-ice case
      
             DO NS = 1, NSOIL
                SH2O(I,NS,J) = 1.0
             ENDDO
             LAI(I,J) = 0.01
      
             CYCLE ILOOP
      
          ELSEIF (ICE == 0) THEN
      
             ! Non-glacial land
      
             CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    !C
                       LOCAL,                                            &    !L
                       LUTYPE, SLTYPE,                                   &    !CL
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    !F
                       DUMMY,DUMMY, DUMMY,                               &    !F PRCPRAIN not used
                       TH2,Q2SAT,DQSDT2,                                 &    !I
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    !I
                       ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    !S
                       CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    !H
                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    !O
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    !O
                       BETA,ETP,SSOIL,                                   &    !O
                       FLX1,FLX2,FLX3,                                   &    !O
      		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                             &    !UA 
                       SNOMLT,SNCOVR,                                    &    !O
                       RUNOFF1,RUNOFF2,RUNOFF3,                          &    !O
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    !O
                       SOILW,SOILM,Q1,SMAV,                              &    !D
                       RDLAI2D,USEMONALB,                                &
                       SNOTIME1,                                         &
                       RIBB,                                             &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                       sfcheadrt(i,j),                                   &    !I
                       INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    !O
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    & ! fasdas vars
                       )
      
#ifdef WRF_HYDRO
                       soldrain(i,j) = RUNOFF2*DT*1000.0
#endif
          ELSEIF (ICE == -1) THEN
      
             !
             ! Set values that the LSM is expected to update,
             ! but don't get updated for glacial points.
             !
             SOILM = 0.0 !BSINGH(PNNL)- SOILM is undefined for this case, it is used for diagnostics so setting it to zero
             XLAI = 0.01 ! KWM Should this be Zero over land ice?  Does this value matter?
             RUNOFF2 = 0.0
             RUNOFF3 = 0.0
             DO NS = 1, NSOIL
                SWC(NS) = 1.0
                SMC(NS) = 1.0
                SMAV(NS) = 1.0
             ENDDO
             CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    !C
                  &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    !F
                  &    TH2,Q2SAT,DQSDT2,                                &    !I
                  &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    !S
                  &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    !H
                  &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    !O
                  &    ESNOW,DEW,                                       &    !O
                  &    ETP,SSOIL,                                       &    !O
                  &    FLX1,FLX2,FLX3,                                  &    !O
                  &    SNOMLT,SNCOVR,                                   &    !O
                  &    RUNOFF1,                                         &    !O
                  &    Q1,                                              &    !D
                  &    SNOTIME1,                                        &
                  &    RIBB)
      
          ENDIF
      
             lai(i,j) = xlai
      
#if 0
                IF(IPRINT) THEN
      
             print*, 'AFTER SFLX, in Noahlsm_driver'
             print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
             'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
              LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
              'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
               'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
                'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
               'SHDMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB',SNOALB1,'TBOT',&
                TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
                STC, 'SMC',SMC, 'SWc',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
                'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
                'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
                'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
                'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
                'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
                'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
                'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
                'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
                'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
                'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
                 endif
#endif
      
      !***  UPDATE STATE VARIABLES
                CANWAT(I,J)=CMC
                SNOW(I,J)=SNEQV*1000.
      !          SNOWH(I,J)=SNOWHK*1000.
                SNOWH(I,J)=SNOWHK                   ! SNOWHK in meters
                ALBEDO(I,J)=ALBEDOK
                ALB_RURAL(I,J)=ALBEDOK
                ALBBCK(I,J)=ALBBRD
                Z0(I,J)=Z0BRD
                EMISS(I,J) = EMISSI
                EMISS_RURAL(I,J) = EMISSI
      ! Noah: activate time-varying roughness length (V3.3 Feb 2011)
                ZNT(I,J)=Z0K
                TSK(I,J)=T1
                TSK_RURAL(I,J)=T1
                HFX(I,J)=SHEAT
                HFX_RURAL(I,J)=SHEAT
      ! MEk Jul07 add potential evap accum
              POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
                QFX(I,J)=ETA_KINEMATIC
                QFX_RURAL(I,J)=ETA_KINEMATIC
      
#ifdef WRF_HYDRO
      !added by Wei Yu
      !         QFX(I,J) = QFX(I,J) + ETPND1
      !         ETA = ETA + ETPND1/2.501E6*dt
      !end added by Wei Yu
#endif
      
                LH(I,J)=ETA
                LH_RURAL(I,J)=ETA
                GRDFLX(I,J)=SSOIL
                GRDFLX_RURAL(I,J)=SSOIL
                SNOWC(I,J)=SNCOVR
                CHS2(I,J)=CQS2(I,J)
                SNOTIME(I,J) = SNOTIME1
      !      prevent diagnostic ground q (q1) from being greater than qsat(tsk)
      !      as happens over snow cover where the cqs2 value also becomes irrelevant
      !      by setting cqs2=chs in this situation the 2m q should become just qv(k=1)
                IF (Q1 .GT. QSFC(I,J)) THEN
                  CQS2(I,J) = CHS(I,J)
                ENDIF
      !          QSFC(I,J)=Q1
      ! Convert QSFC back to mixing ratio
                 QSFC(I,J)= Q1/(1.0-Q1)
      !
                 ! QSFC_RURAL(I,J)= Q1/(1.0-Q1)
      ! Calculate momentum flux from rural surface for use with multi-layer UCM (Martilli et al. 2002)
      
                DO 81 NS=1,NSOIL
                 SMOIS(I,NS,J)=SMC(NS)
                 TSLB(I,NS,J)=STC(NS)                                        !  STEMP
                 SH2O(I,NS,J)=SWC(NS)
         81     CONTINUE
      !       ENDIF
      
              FLX4_2D(I,J)  = FLX4
      	FVB_2D(I,J)   = FVB
      	FBUR_2D(I,J)  = FBUR
      	FGSN_2D(I,J)  = FGSN
      
           !
           ! Residual of surface energy balance equation terms
           !
      
           IF ( UA_PHYS ) THEN
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4
      
           ELSE
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
           ENDIF
      
               ENDIF   !ENDIF FOR Noah_call
               
              IF (SF_URBAN_PHYSICS == 1 ) THEN                                              ! Beginning of UCM CALL if block
      !--------------------------------------
      ! URBAN CANOPY MODEL START - urban
      !--------------------------------------
      ! Input variables lsm --> urban
      
                IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                    IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL ) THEN            
      
                !  UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial)
                !  this need to be changed in the mosaic danli
      
                IF(IVGTYP(I,J)==ISURBAN) UTYPE_URB=2
                IF(IVGTYP(I,J)==LOW_DENSITY_RESIDENTIAL) UTYPE_URB=1
                IF(IVGTYP(I,J)==HIGH_DENSITY_RESIDENTIAL) UTYPE_URB=2
                IF(IVGTYP(I,J)==HIGH_INTENSITY_INDUSTRIAL) UTYPE_URB=3
                        
                IF(UTYPE_URB==1) FRC_URB2D(I,J)=0.5
                IF(UTYPE_URB==2) FRC_URB2D(I,J)=0.9
                IF(UTYPE_URB==3) FRC_URB2D(I,J)=0.95
      
                  TA_URB    = SFCTMP           ! [K]
                  QA_URB    = Q2K              ! [kg/kg]
                  UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
                  U1_URB    = U_PHY(I,1,J)
                  V1_URB    = V_PHY(I,1,J)
                  IF(UA_URB < 1.) UA_URB=1.    ! [m/s]
                  SSG_URB   = SOLDN            ! [W/m/m]
                  SSGD_URB  = 0.8*SOLDN        ! [W/m/m]
                  SSGQ_URB  = SSG_URB-SSGD_URB ! [W/m/m]
                  LLG_URB   = GLW(I,J)         ! [W/m/m]
                  RAIN_URB  = RAINBL(I,J)      ! [mm]
                  RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) ![kg/m/m/m]
                  ZA_URB    = ZLVL             ! [m]
                  DELT_URB  = DT               ! [sec]
                  XLAT_URB  = XLAT_URB2D(I,J)  ! [deg]
                  COSZ_URB  = COSZ_URB2D(I,J)  !
                  OMG_URB   = OMG_URB2D(I,J)   !
                  ZNT_URB   = ZNT(I,J)
      
                  LSOLAR_URB = .FALSE.
                  
      ! mosaic 3D to 2D
      
         TR_URB2D(I,J)=TR_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         TB_URB2D(I,J)=TB_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         TG_URB2D(I,J)=TG_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         TC_URB2D(I,J)=TC_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         QC_URB2D(I,J)=QC_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         UC_URB2D(I,J)=UC_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
         TS_URB2D(I,J)=TS_URB2D_mosaic(I,mosaic_i,J)                         ! replace it with the mosaic one          
      
                  DO K = 1,num_roof_layers
                    TRL_URB3D(I,K,J) = TRL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
                  DO K = 1,num_wall_layers
                    TBL_URB3D(I,K,J) = TBL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
                  DO K = 1,num_road_layers
                    TGL_URB3D(I,K,J) = TGL_URB3D_mosaic(I,K+(mosaic_i-1)*num_roof_layers,J)
                  END DO
      
      ! mosaic 2D to 1D            
      
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
      !
      !      Limits to avoid dividing by small number
                  if (CHS(I,J) < 1.0E-02) then
                     CHS(I,J)  = 1.0E-02
                  endif
                  if (CHS2(I,J) < 1.0E-02) then
                     CHS2(I,J)  = 1.0E-02
                  endif
                  if (CQS2(I,J) < 1.0E-02) then
                     CQS2(I,J)  = 1.0E-02
                  endif
      !
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
      
      ! NUDAPT for SLUCM
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
      !
      ! Call urban
                  CALL cal_mon_day(julian,julyr,jmonth,jday)      
                  CALL urban(LSOLAR_URB,                                      & ! I
                             num_roof_layers,num_wall_layers,num_road_layers, & ! C
                             DZR,DZB,DZG,                                     & ! C
                             UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & ! I
                             SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & ! I
                             ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & ! I
                             XLAT_URB,DELT_URB,ZNT_URB,                       & ! I
                             CHS_URB, CHS2_URB,                               & ! I
                             TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & ! H
                             TRL_URB,TBL_URB,TGL_URB,                         & ! H
                             XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & ! H
                             TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & ! O
                             SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & ! O
                             GZ1OZ0_URB,                                      & !O
                             CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                             U10_URB, V10_URB, TH2_URB, Q2_URB,               & ! O
                             UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & ! 0
                             hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & ! H
                             TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & ! H
                             DRELR_URB,DRELB_URB,                             & ! H
                             DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)     
      
#if 0
                IF(IPRINT) THEN
      
             print*, 'AFTER CALL URBAN'
             print*,'num_roof_layers',num_roof_layers, 'num_wall_layers',  &
              num_wall_layers,                                             &
             'DZR',DZR,'DZB',DZB,'DZG',DZG,'UTYPE_URB',UTYPE_URB,'TA_URB', &
              TA_URB,                                                      &
              'QA_URB',QA_URB,'UA_URB',UA_URB,'U1_URB',U1_URB,'V1_URB',    &
               V1_URB,                                                     &
               'SSG_URB',SSG_URB,'SSGD_URB',SSGD_URB,'SSGQ_URB',SSGQ_URB,  &
              'LLG_URB',LLG_URB,'RAIN_URB',RAIN_URB,'RHOO_URB',RHOO_URB,   &
              'ZA_URB',ZA_URB, 'DECLIN_URB',DECLIN_URB,'COSZ_URB',COSZ_URB,&
              'OMG_URB',OMG_URB,'XLAT_URB',XLAT_URB,'DELT_URB',DELT_URB,   &
               'ZNT_URB',ZNT_URB,'TR_URB',TR_URB, 'TB_URB',TB_URB,'TG_URB',&
               TG_URB,'TC_URB',TC_URB,'QC_URB',QC_URB,'TRL_URB',TRL_URB,   &
                'TBL_URB',TBL_URB,'TGL_URB',TGL_URB,'XXXR_URB',XXXR_URB,   &
               'XXXB_URB',XXXB_URB,'XXXG_URB',XXXG_URB,'XXXC_URB',XXXC_URB,&
               'TS_URB',TS_URB,'QS_URB',QS_URB,'SH_URB',SH_URB,'LH_URB',   &
               LH_URB, 'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'SW_URB',SW_URB,&
               'ALB_URB',ALB_URB,'LW_URB',LW_URB,'G_URB',G_URB,'RN_URB',   &
                RN_URB, 'PSIM_URB',PSIM_URB,'PSIH_URB',PSIH_URB,          &
               'U10_URB',U10_URB,'V10_URB',V10_URB,'TH2_URB',TH2_URB,      &
                'Q2_URB',Q2_URB,'CHS_URB',CHS_URB,'CHS2_URB',CHS2_URB
                 endif
#endif
      
                  TS_URB2D(I,J) = TS_URB
      
                  ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   ![-]
                  HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         ![W/m/m]
                  QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                           + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                ![kg/m/m/s]
                  LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            ![W/m/m]
                  GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       ![W/m/m]
                  TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            ![K]
                  Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            ![-]
      ! Convert QSFC back to mixing ratio
                  QSFC(I,J)= Q1/(1.0-Q1)
                  UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      ![m/s]
                  ZNT(I,J)= EXP(FRC_URB2D(I,J)*ALOG(ZNT_URB)+(1-FRC_URB2D(I,J))* ALOG(ZNT(I,J)))   ! ADD BY DAN

#if 0
          IF(IPRINT)THEN
      
          print*, ' FRC_URB2D', FRC_URB2D,                        &
          'ALB_URB',ALB_URB, 'ALBEDOK',ALBEDOK, &
          'ALBEDO(I,J)',  ALBEDO(I,J),                  &
          'SH_URB',SH_URB,'SHEAT',SHEAT, 'HFX(I,J)',HFX(I,J),  &
          'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'ETA_KINEMATIC',  &
           ETA_KINEMATIC, 'QFX(I,J)',QFX(I,J),                  &
          'LH_URB',LH_URB, 'ETA',ETA, 'LH(I,J)',LH(I,J),        &
          'G_URB',G_URB,'SSOIL',SSOIL,'GRDFLX(I,J)', GRDFLX(I,J),&
          'TS_URB',TS_URB,'T1',T1,'TSK(I,J)',TSK(I,J),          &
          'QS_URB',QS_URB,'Q1',Q1,'QSFC(I,J)',QSFC(I,J)
           endif
#endif
      
      ! Renew Urban State Varialbes
      
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
                  
                     ! 2D to 3D  mosaic danli
      	    	              
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
      
               ENDIF                                   ! end of UCM CALL if block
      !--------------------------------------
      ! Urban Part End - urban
      !--------------------------------------
      
      !***  DIAGNOSTICS
                SMSTAV(I,J)=SOILW
                SMSTOT(I,J)=SOILM*1000.
                DO NS=1,NSOIL
                SMCREL(I,NS,J)=SMAV(NS)
                ENDDO
      
      !         Convert the water unit into mm
                SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
                UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0
      ! snow defined when fraction of frozen precip (FFROZP) > 0.5,
                IF(FFROZP.GT.0.5)THEN
                  ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
                ENDIF
                IF(SNOW(I,J).GT.0.)THEN
                  ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
      ! accumulated snow-melt energy
                  SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
                ENDIF
      
              ENDIF                                                           ! endif of land-sea test
      
      !-----------------------------------------------------------------------
      ! Done with the Noah-UCM MOSAIC  DANLI
      !-----------------------------------------------------------------------
      
                  TSK_mosaic(i,mosaic_i,j)=TSK(i,j)                           ! from 2D to 3D
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
                  
#if 0
            IF(TSK_mosaic(i,mosaic_i,j) > 350 .OR. TSK_mosaic(i,mosaic_i,j) < 250 .OR. abs(HFX_mosaic(i,mosaic_i,j)) > 700 ) THEN
                  print*, 'I', I, 'J', J, 'MOSAIC_I', MOSAIC_I
                  print*, 'mosaic_cat_index',mosaic_cat_index(I,mosaic_i,J), 'landusef2',landusef2(i,mosaic_i,j)
                  print*, 'TSK_mosaic', TSK_mosaic(i,mosaic_i,j), 'HFX_mosaic', HFX_mosaic(i,mosaic_i,j), &
                          'LH_mosaic',LH_mosaic(i,mosaic_i,j),'GRDFLX_mosaic',GRDFLX_mosaic(i,mosaic_i,j)
                  print*, 'ZNT_mosaic', ZNT_mosaic(i, mosaic_i,j), 'Z0_mosaic', Z0_mosaic(i,mosaic_i,j) 
                  print*, 'FRC_URB2D',FRC_URB2D(I,J)
                  print*, 'TS_URB',TS_URB2D(I,J),'T1',T1
                  print*, 'SH_URB2D',SH_URB2D(I,J),'SHEAT',SHEAT
                  print*, 'LH_URB',LH_URB2D(I,J),'ETA',ETA
                  print*, 'TS_RUL2D',TS_RUL2D_mosaic(I,mosaic_i,J)
                  
            ENDIF
#endif
                  
      !-----------------------------------------------------------------------
      ! Now let's do the grid-averaging
      !-----------------------------------------------------------------------
      
                  FAREA  = landusef2(i,mosaic_i,j)
      
                  TSK_mosaic_avg(i,j) = TSK_mosaic_avg(i,j) + (EMISS_mosaic(i,mosaic_i,j)*TSK_mosaic(i,mosaic_i,j)**4)*FAREA    ! conserve the longwave radiation
                  
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
       
         ENDDO                     ! ENDDO FOR mosaic_i = 1, mosaic_cat
      
      !-----------------------------------------------------------------------
      ! Now let's send the 3D values to the 2D variables that might be needed in other routines
      !-----------------------------------------------------------------------
      
          IVGTYP(I,J)=IVGTYP_dominant(I,J)                                 ! the dominant vege category 
          ALBEDO(i,j)=ALBEDO_mosaic_avg(i,j)
          ALBBCK(i,j)=ALBBCK_mosaic_avg(i,j) 
          EMISS(i,j)= EMISS_mosaic_avg(i,j) 
          EMBCK(i,j)= EMBCK_mosaic_avg(i,j)  
          ZNT(i,j)= EXP(ZNT_mosaic_avg(i,j)/FAREA_mosaic_avg(i,j))
          Z0(i,j)= EXP(Z0_mosaic_avg(i,j)/FAREA_mosaic_avg(i,j))
                                                                                       
          TSK(i,j)=(TSK_mosaic_avg(I,J)/EMISS_mosaic_avg(I,J))**(0.25)                                  ! from 3D to 2D
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
      
      ELSE    ! This corresponds to IF ((sf_surface_mosaic == 1) .AND. ((XLAND(I,J)-1.5).LT.0.) .AND. (XICE(I,J) < XICE_THRESHOLD) ) THEN
      
      ! surface pressure
              PSFC=P8w3D(i,1,j)
      ! pressure in middle of lowest layer
              SFCPRS=(P8W3D(I,KTS+1,j)+P8W3D(i,KTS,j))*0.5
      ! convert from mixing ratio to specific humidity
               Q2K=QV3D(i,1,j)/(1.0+QV3D(i,1,j))
      !
      !         Q2SAT=QGH(I,j)
               Q2SAT=QGH(I,J)/(1.0+QGH(I,J))        ! Q2SAT is sp humidity
      ! add check on myj=.true.
      !        IF((Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
              IF((myj).AND.(Q2K.GE.Q2SAT*TRESH).AND.Q2K.LT.QZ0(I,J))THEN
                SATFLG=0.
                CHKLOWQ(I,J)=0.
              ELSE
                SATFLG=1.0
                CHKLOWQ(I,J)=1.
              ENDIF
      
              SFCTMP=T3D(i,1,j)
              ZLVL=0.5*DZ8W(i,1,j)
      
      !        TH2=SFCTMP+(0.0097545*ZLVL)
      ! calculate SFCTH2 via Exner function vs lapse-rate (above)
               APES=(1.E5/PSFC)**CAPA
               APELM=(1.E5/SFCPRS)**CAPA
               SFCTH2=SFCTMP*APELM
               TH2=SFCTH2/APES
      !
               EMISSI = EMISS(I,J)
               LWDN=GLW(I,J)*EMISSI
      ! SOLDN is total incoming solar
              SOLDN=SWDOWN(I,J)
      ! GSW is net downward solar
      !        SOLNET=GSW(I,J)
      ! use mid-day albedo to determine net downward solar (no solar zenith angle correction)
              SOLNET=SOLDN*(1.-ALBEDO(I,J))
              PRCP=RAINBL(i,j)/DT
              VEGTYP=IVGTYP(I,J)
              SOILTYP=ISLTYP(I,J)
              SHDFAC=VEGFRA(I,J)/100.
              T1=TSK(I,J)
              CHK=CHS(I,J)
              SHMIN=SHDMIN(I,J)/100. !NEW
              SHMAX=SHDMAX(I,J)/100. !NEW
      ! convert snow water equivalent from mm to meter
              SNEQV=SNOW(I,J)*0.001
      ! snow depth in meters
              SNOWHK=SNOWH(I,J)
              SNCOVR=SNOWC(I,J)
      
      ! if "SR" present, set frac of frozen precip ("FFROZP") = snow-ratio ("SR", range:0-1)
      ! SR from e.g. Ferrier microphysics
      ! otherwise define from 1st atmos level temperature
             IF(FRPCPN) THEN
                FFROZP=SR(I,J)
              ELSE
                IF (SFCTMP <=  273.15) THEN
                  FFROZP = 1.0
      	  ELSE
      	    FFROZP = 0.0
      	  ENDIF
              ENDIF
      !***
              IF((XLAND(I,J)-1.5).GE.0.)THEN                                  ! begining of land/sea if block
      ! Open water points
                TSK_RURAL(I,J)=TSK(I,J)
                HFX_RURAL(I,J)=HFX(I,J)
                QFX_RURAL(I,J)=QFX(I,J)
                LH_RURAL(I,J)=LH(I,J)
                EMISS_RURAL(I,J)=EMISS(I,J)
                GRDFLX_RURAL(I,J)=GRDFLX(I,J)
              ELSE
      ! Land or sea-ice case
      
                IF (XICE(I,J) >= XICE_THRESHOLD) THEN
                   ! Sea-ice point
                   ICE = 1
                ELSE IF ( VEGTYP == ISICE ) THEN
                   ! Land-ice point
                   ICE = -1
                ELSE
                   ! Neither sea ice or land ice.
                   ICE=0
                ENDIF
                DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2
      
                IF(SNOW(I,J).GT.0.0)THEN
      ! snow on surface (use ice saturation properties)
                  SFCTSNO=SFCTMP
                  E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
                  Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
                  Q2SATI=Q2SATI/(1.0+Q2SATI)    ! spec. hum.
                  IF (T1 .GT. 273.14) THEN
      ! warm ground temps, weight the saturation between ice and water according to SNOWC
                    Q2SAT=Q2SAT*(1.-SNOWC(I,J)) + Q2SATI*SNOWC(I,J)
                    DQSDT2=DQSDT2*(1.-SNOWC(I,J)) + Q2SATI*6174./(SFCTSNO**2)*SNOWC(I,J)
                  ELSE
      ! cold ground temps, use ice saturation only
                    Q2SAT=Q2SATI
                    DQSDT2=Q2SATI*6174./(SFCTSNO**2)
                  ENDIF
      ! for snow cover fraction at 0 C, ground temp will not change, so DQSDT2 effectively zero
                  IF(T1 .GT. 273. .AND. SNOWC(I,J) .GT. 0.)DQSDT2=DQSDT2*(1.-SNOWC(I,J))
                ENDIF
      
                ! Land-ice or land points use the usual deep-soil temperature.
                TBOT=TMN(I,J)
      
                IF(VEGTYP.EQ.25) SHDFAC=0.0000
                IF(VEGTYP.EQ.26) SHDFAC=0.0000
                IF(VEGTYP.EQ.27) SHDFAC=0.0000
                IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
#if 0
               IF(IPRINT)PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
               IF(IPRINT)PRINT*,i,j,'RESET SOIL in surfce.F'
#endif
                  SOILTYP=7
                ENDIF
                SNOALB1 = SNOALB(I,J)
                CMC=CANWAT(I,J)
      
      !-------------------------------------------
      !*** convert snow depth from mm to meter
      !
      !          IF(RDMAXALB) THEN
      !           SNOALB=ALBMAX(I,J)*0.01
      !         ELSE
      !           SNOALB=MAXALB(IVGTPK)*0.01
      !         ENDIF
      
      !        SNOALB1=0.80
      !        SHMIN=0.00
              ALBBRD=ALBBCK(I,J)
              Z0BRD=Z0(I,J)
              EMBRD=EMBCK(I,J)
              SNOTIME1 = SNOTIME(I,J)
              RIBB=RIB(I,J)
      !FEI: temporaray arrays above need to be changed later by using SI
      
                DO NS=1,NSOIL
                  SMC(NS)=SMOIS(I,NS,J)
                  STC(NS)=TSLB(I,NS,J)                                          !STEMP
                  SWC(NS)=SH2O(I,NS,J)
                ENDDO
      !
                if ( (SNEQV.ne.0..AND.SNOWHK.eq.0.).or.(SNOWHK.le.SNEQV) )THEN
                  SNOWHK= 5.*SNEQV
                endif
      !
      
      !Fei: urban. for urban surface, if calling UCM, redefine the natural surface in cities as
      ! the "NATURAL" category in the VEGPARM.TBL
      	
      	   IF(SF_URBAN_PHYSICS == 1.OR. SF_URBAN_PHYSICS==2.OR.SF_URBAN_PHYSICS==3 ) THEN
                      IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                        IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
      		 VEGTYP = NATURAL
                       SHDFAC = SHDTBL(NATURAL)
                       ALBEDOK =0.2         !  0.2
                       ALBBRD  =0.2         !0.2
                       EMISSI = 0.98                                 !for VEGTYP=5
      		 IF ( FRC_URB2D(I,J) < 0.99 ) THEN
                         if(sf_urban_physics.eq.1)then
                 T1= ( TSK(I,J) -FRC_URB2D(I,J) * TS_URB2D (I,J) )/ (1-FRC_URB2D(I,J))
                         elseif((sf_urban_physics.eq.2).OR.(sf_urban_physics.eq.3))then
                      r1= (tsk(i,j)**4.)
                      r2= frc_urb2d(i,j)*(ts_urb2d(i,j)**4.)
                      r3= (1.-frc_urb2d(i,j))
                      t1= ((r1-r2)/r3)**.25
                         endif
      	         ELSE
      		 T1 = TSK(I,J)
                       ENDIF
                      ENDIF
                 ELSE
                       IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                        IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
                        VEGTYP = ISURBAN
                 	 ENDIF
                 ENDIF


!===Yang, 2014/10/08, hydrological processes for urban vegetation in single layer UCM===
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
           IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL) THEN
            AOASIS = oasis  ! urban oasis effect
                   IF (IRIOPTION ==1) THEN
                   IF (tloc==21 .or. tloc==22) THEN  !irrigation on vegetaion in urban area, MAY-SEP, 9-10pm
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
          CALL wrf_error_fatal('Urban oasis option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          IF(IRIOPTION == 1) THEN
          CALL wrf_error_fatal('Urban irrigation option is for SF_URBAN_PHYSICS == 1 only')
          ENDIF
          ENDIF

#if 0
                IF(IPRINT) THEN
      !
             print*, 'BEFORE SFLX, in Noahlsm_driver'
             print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
             'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
              LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
              'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
               'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
               'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
               'SHMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB1',SNOALB1,'TBOT',&
                TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
                STC, 'SMC',SMC, 'SWC',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
                'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
                'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
                'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
                'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
                'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
                'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
                'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
                'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
                'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
                'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
                 endif
#endif
      
                IF (rdlai2d) THEN
                   xlai = lai(i,j)
                endif
      
          IF ( ICE == 1 ) THEN
      
             ! Sea-ice case
      
             DO NS = 1, NSOIL
                SH2O(I,NS,J) = 1.0
             ENDDO
             LAI(I,J) = 0.01
      
             CYCLE ILOOP
      
          ELSEIF (ICE == 0) THEN
      
             ! Non-glacial land
      
             CALL SFLX (I,J,FFROZP, ISURBAN, DT,ZLVL,NSOIL,SLDPTH,       &    !C
                       LOCAL,                                            &    !L
                       LUTYPE, SLTYPE,                                   &    !CL
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,DUMMY,   &    !F
                       DUMMY,DUMMY, DUMMY,                               &    !F PRCPRAIN not used
                       TH2,Q2SAT,DQSDT2,                                 &    !I
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHMIN,SHMAX,       &    !I
                       ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD,  &    !S
                       CMC,T1,STC,SMC,SWC,SNOWHK,SNEQV,ALBEDOK,CHK,dummy,&    !H
                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                   &    !O
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                    &    !O
                       BETA,ETP,SSOIL,                                   &    !O
                       FLX1,FLX2,FLX3,                                   &    !O
      		 FLX4,FVB,FBUR,FGSN,UA_PHYS,                             &    !UA 
                       SNOMLT,SNCOVR,                                    &    !O
                       RUNOFF1,RUNOFF2,RUNOFF3,                          &    !O
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,              &    !O
                       SOILW,SOILM,Q1,SMAV,                              &    !D
                       RDLAI2D,USEMONALB,                                &
                       SNOTIME1,                                         &
                       RIBB,                                             &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,                &
                       sfcheadrt(i,j),                                   &    !I
                       INFXSRT(i,j),ETPND1,OPT_THCND,AOASIS              &    !O
                ,XSDA_QFX, HFX_PHY, QFX_PHY, XQNORM, fasdas, HCPCT_FASDAS    & ! fasdas vars
                       )
      
#ifdef WRF_HYDRO
                       soldrain(i,j) = RUNOFF2*DT*1000.0
#endif
          ELSEIF (ICE == -1) THEN
      
             !
             ! Set values that the LSM is expected to update,
             ! but don't get updated for glacial points.
             !
             SOILM = 0.0 !BSINGH(PNNL)- SOILM is undefined for this case, it is used for diagnostics so setting it to zero
             XLAI = 0.01 ! KWM Should this be Zero over land ice?  Does this value matter?
             RUNOFF2 = 0.0
             RUNOFF3 = 0.0
             DO NS = 1, NSOIL
                SWC(NS) = 1.0
                SMC(NS) = 1.0
                SMAV(NS) = 1.0
             ENDDO
             CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    !C
                  &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    !F
                  &    TH2,Q2SAT,DQSDT2,                                &    !I
                  &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    !S
                  &    T1,STC(1:NSOIL),SNOWHK,SNEQV,ALBEDOK,CHK,        &    !H
                  &    ETA,SHEAT,ETA_KINEMATIC,FDOWN,                   &    !O
                  &    ESNOW,DEW,                                       &    !O
                  &    ETP,SSOIL,                                       &    !O
                  &    FLX1,FLX2,FLX3,                                  &    !O
                  &    SNOMLT,SNCOVR,                                   &    !O
                  &    RUNOFF1,                                         &    !O
                  &    Q1,                                              &    !D
                  &    SNOTIME1,                                        &
                  &    RIBB)
      
          ENDIF
      
             lai(i,j) = xlai
      
#if 0
                IF(IPRINT) THEN
      
             print*, 'AFTER SFLX, in Noahlsm_driver'
             print*, 'ICE', ICE, 'DT',DT, 'ZLVL',ZLVL, 'NSOIL', NSOIL,   &
             'SLDPTH', SLDPTH, 'LOCAL',LOCAL, 'LUTYPE',&
              LUTYPE, 'SLTYPE',SLTYPE, 'LWDN',LWDN, 'SOLDN',SOLDN,      &
              'SFCPRS',SFCPRS, 'PRCP',PRCP,'SFCTMP',SFCTMP,'Q2K',Q2K,   &
               'TH2',TH2,'Q2SAT',Q2SAT,'DQSDT2',DQSDT2,'VEGTYP', VEGTYP,&
                'SOILTYP',SOILTYP, 'SLOPETYP',SLOPETYP, 'SHDFAC',SHDFAC,&
               'SHDMIN',SHMIN, 'ALBBRD',ALBBRD,'SNOALB',SNOALB1,'TBOT',&
                TBOT, 'Z0BRD',Z0BRD, 'Z0K',Z0K, 'CMC',CMC, 'T1',T1,'STC',&
                STC, 'SMC',SMC, 'SWc',SWC,'SNOWHK',SNOWHK,'SNEQV',SNEQV,&
                'ALBEDOK',ALBEDOK,'CHK',CHK,'ETA',ETA,'SHEAT',SHEAT,      &
                'ETA_KINEMATIC',ETA_KINEMATIC, 'FDOWN',FDOWN,'EC',EC,   &
                'EDIR',EDIR,'ET',ET,'ETT',ETT,'ESNOW',ESNOW,'DRIP',DRIP,&
                'DEW',DEW,'BETA',BETA,'ETP',ETP,'SSOIL',SSOIL,'FLX1',FLX1,&
                'FLX2',FLX2,'FLX3',FLX3,'SNOMLT',SNOMLT,'SNCOVR',SNCOVR,&
                'RUNOFF1',RUNOFF1,'RUNOFF2',RUNOFF2,'RUNOFF3',RUNOFF3, &
                'RC',RC, 'PC',PC,'RSMIN',RSMIN,'XLAI',XLAI,'RCS',RCS,  &
                'RCT',RCT,'RCQ',RCQ,'RCSOIL',RCSOIL,'SOILW',SOILW,     &
                'SOILM',SOILM,'Q1',Q1,'SMCWLT',SMCWLT,'SMCDRY',SMCDRY,&
                'SMCREF',SMCREF,'SMCMAX',SMCMAX,'NROOT',NROOT
                 endif
#endif
      
      !***  UPDATE STATE VARIABLES
                CANWAT(I,J)=CMC
                SNOW(I,J)=SNEQV*1000.
      !          SNOWH(I,J)=SNOWHK*1000.
                SNOWH(I,J)=SNOWHK                   ! SNOWHK in meters
                ALBEDO(I,J)=ALBEDOK
                ALB_RURAL(I,J)=ALBEDOK
                ALBBCK(I,J)=ALBBRD
                Z0(I,J)=Z0BRD
                EMISS(I,J) = EMISSI
                EMISS_RURAL(I,J) = EMISSI
      ! Noah: activate time-varying roughness length (V3.3 Feb 2011)
                ZNT(I,J)=Z0K
                TSK(I,J)=T1
                TSK_RURAL(I,J)=T1
                HFX(I,J)=SHEAT
                HFX_RURAL(I,J)=SHEAT
      ! MEk Jul07 add potential evap accum
              POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW
                QFX(I,J)=ETA_KINEMATIC
                QFX_RURAL(I,J)=ETA_KINEMATIC
      
#ifdef WRF_HYDRO
      !added by Wei Yu
      !         QFX(I,J) = QFX(I,J) + ETPND1
      !         ETA = ETA + ETPND1/2.501E6*dt
      !end added by Wei Yu
#endif
      
                LH(I,J)=ETA
                LH_RURAL(I,J)=ETA
                GRDFLX(I,J)=SSOIL
                GRDFLX_RURAL(I,J)=SSOIL
                SNOWC(I,J)=SNCOVR
                CHS2(I,J)=CQS2(I,J)
                SNOTIME(I,J) = SNOTIME1
      !      prevent diagnostic ground q (q1) from being greater than qsat(tsk)
      !      as happens over snow cover where the cqs2 value also becomes irrelevant
      !      by setting cqs2=chs in this situation the 2m q should become just qv(k=1)
                IF (Q1 .GT. QSFC(I,J)) THEN
                  CQS2(I,J) = CHS(I,J)
                ENDIF
      !          QSFC(I,J)=Q1
      ! Convert QSFC back to mixing ratio
                 QSFC(I,J)= Q1/(1.0-Q1)
      !
                 ! QSFC_RURAL(I,J)= Q1/(1.0-Q1)
      ! Calculate momentum flux from rural surface for use with multi-layer UCM (Martilli et al. 2002)
      
                DO 80 NS=1,NSOIL
                 SMOIS(I,NS,J)=SMC(NS)
                 TSLB(I,NS,J)=STC(NS)                                        !  STEMP
                 SH2O(I,NS,J)=SWC(NS)
         80     CONTINUE
      !       ENDIF
      
              FLX4_2D(I,J)  = FLX4
      	FVB_2D(I,J)   = FVB
      	FBUR_2D(I,J)  = FBUR
      	FGSN_2D(I,J)  = FGSN
           !
           ! Residual of surface energy balance equation terms
           !
      
           IF ( UA_PHYS ) THEN
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3 - flx4
      
           ELSE
               noahres(i,j) = ( solnet + lwdn ) - sheat + ssoil - eta &
                    - ( emissi * STBOLT * (t1**4) ) - flx1 - flx2 - flx3
           ENDIF
      
              IF (SF_URBAN_PHYSICS == 1 ) THEN                                              ! Beginning of UCM CALL if block
      !--------------------------------------
      ! URBAN CANOPY MODEL START - urban
      !--------------------------------------
      ! Input variables lsm --> urban
      
                IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
                    IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) ==  HIGH_INTENSITY_INDUSTRIAL) THEN
      
      ! Call urban
      !
                  UTYPE_URB = UTYPE_URB2D(I,J) !urban type (low, high or industrial)
      
                  TA_URB    = SFCTMP           ! [K]
                  QA_URB    = Q2K              ! [kg/kg]
                  UA_URB    = SQRT(U_PHY(I,1,J)**2.+V_PHY(I,1,J)**2.)
                  U1_URB    = U_PHY(I,1,J)
                  V1_URB    = V_PHY(I,1,J)
                  IF(UA_URB < 1.) UA_URB=1.    ! [m/s]
                  SSG_URB   = SOLDN            ! [W/m/m]
                  SSGD_URB  = 0.8*SOLDN        ! [W/m/m]
                  SSGQ_URB  = SSG_URB-SSGD_URB ! [W/m/m]
                  LLG_URB   = GLW(I,J)         ! [W/m/m]
                  RAIN_URB  = RAINBL(I,J)      ! [mm]
                  RHOO_URB  = SFCPRS / (287.04 * SFCTMP * (1.0+ 0.61 * Q2K)) ![kg/m/m/m]
                  ZA_URB    = ZLVL             ! [m]
                  DELT_URB  = DT               ! [sec]
                  XLAT_URB  = XLAT_URB2D(I,J)  ! [deg]
                  COSZ_URB  = COSZ_URB2D(I,J)  !
                  OMG_URB   = OMG_URB2D(I,J)   !
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
      !
      !      Limits to avoid dividing by small number
                  if (CHS(I,J) < 1.0E-02) then
                     CHS(I,J)  = 1.0E-02
                  endif
                  if (CHS2(I,J) < 1.0E-02) then
                     CHS2(I,J)  = 1.0E-02
                  endif
                  if (CQS2(I,J) < 1.0E-02) then
                     CQS2(I,J)  = 1.0E-02
                  endif
      !
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
      
      ! NUDAPT for SLUCM
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
      !
      ! Call urban
                  CALL cal_mon_day(julian,julyr,jmonth,jday)        
                  CALL urban(LSOLAR_URB,                                      & ! I
                             num_roof_layers,num_wall_layers,num_road_layers, & ! C
                             DZR,DZB,DZG,                                     & ! C
                             UTYPE_URB,TA_URB,QA_URB,UA_URB,U1_URB,V1_URB,SSG_URB, & ! I
                             SSGD_URB,SSGQ_URB,LLG_URB,RAIN_URB,RHOO_URB,     & ! I
                             ZA_URB,DECLIN_URB,COSZ_URB,OMG_URB,              & ! I
                             XLAT_URB,DELT_URB,ZNT_URB,                       & ! I
                             CHS_URB, CHS2_URB,                               & ! I
                             TR_URB, TB_URB, TG_URB, TC_URB, QC_URB,UC_URB,   & ! H
                             TRL_URB,TBL_URB,TGL_URB,                         & ! H
                             XXXR_URB, XXXB_URB, XXXG_URB, XXXC_URB,          & ! H
                             TS_URB,QS_URB,SH_URB,LH_URB,LH_KINEMATIC_URB,    & ! O
                             SW_URB,ALB_URB,LW_URB,G_URB,RN_URB,PSIM_URB,PSIH_URB, & ! O
                             GZ1OZ0_URB,                                      & !O
                             CMR_URB, CHR_URB, CMC_URB, CHC_URB,              &
                             U10_URB, V10_URB, TH2_URB, Q2_URB,               & ! O
                             UST_URB,mh_urb, stdh_urb, lf_urb, lp_urb,        & ! 0
                             hgt_urb,frc_urb,lb_urb, check,CMCR_URB,TGR_URB,  & ! H
                             TGRL_URB,SMR_URB,CMGR_URB,CHGR_URB,jmonth,       & ! H
                             DRELR_URB,DRELB_URB,                             & ! H
                             DRELG_URB,FLXHUMR_URB,FLXHUMB_URB,FLXHUMG_URB)  
      
#if 0
                IF(IPRINT) THEN
      
             print*, 'AFTER CALL URBAN'
             print*,'num_roof_layers',num_roof_layers, 'num_wall_layers',  &
              num_wall_layers,                                             &
             'DZR',DZR,'DZB',DZB,'DZG',DZG,'UTYPE_URB',UTYPE_URB,'TA_URB', &
              TA_URB,                                                      &
              'QA_URB',QA_URB,'UA_URB',UA_URB,'U1_URB',U1_URB,'V1_URB',    &
               V1_URB,                                                     &
               'SSG_URB',SSG_URB,'SSGD_URB',SSGD_URB,'SSGQ_URB',SSGQ_URB,  &
              'LLG_URB',LLG_URB,'RAIN_URB',RAIN_URB,'RHOO_URB',RHOO_URB,   &
              'ZA_URB',ZA_URB, 'DECLIN_URB',DECLIN_URB,'COSZ_URB',COSZ_URB,&
              'OMG_URB',OMG_URB,'XLAT_URB',XLAT_URB,'DELT_URB',DELT_URB,   &
               'ZNT_URB',ZNT_URB,'TR_URB',TR_URB, 'TB_URB',TB_URB,'TG_URB',&
               TG_URB,'TC_URB',TC_URB,'QC_URB',QC_URB,'TRL_URB',TRL_URB,   &
                'TBL_URB',TBL_URB,'TGL_URB',TGL_URB,'XXXR_URB',XXXR_URB,   &
               'XXXB_URB',XXXB_URB,'XXXG_URB',XXXG_URB,'XXXC_URB',XXXC_URB,&
               'TS_URB',TS_URB,'QS_URB',QS_URB,'SH_URB',SH_URB,'LH_URB',   &
               LH_URB, 'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'SW_URB',SW_URB,&
               'ALB_URB',ALB_URB,'LW_URB',LW_URB,'G_URB',G_URB,'RN_URB',   &
                RN_URB, 'PSIM_URB',PSIM_URB,'PSIH_URB',PSIH_URB,          &
               'U10_URB',U10_URB,'V10_URB',V10_URB,'TH2_URB',TH2_URB,      &
                'Q2_URB',Q2_URB,'CHS_URB',CHS_URB,'CHS2_URB',CHS2_URB
                 endif
#endif
      
                  TS_URB2D(I,J) = TS_URB
      
                  ALBEDO(I,J) = FRC_URB2D(I,J)*ALB_URB+(1-FRC_URB2D(I,J))*ALBEDOK   ![-]
                  HFX(I,J) = FRC_URB2D(I,J)*SH_URB+(1-FRC_URB2D(I,J))*SHEAT         ![W/m/m]
                  QFX(I,J) = FRC_URB2D(I,J)*LH_KINEMATIC_URB &
                           + (1-FRC_URB2D(I,J))*ETA_KINEMATIC                ![kg/m/m/s]
                  LH(I,J) = FRC_URB2D(I,J)*LH_URB+(1-FRC_URB2D(I,J))*ETA            ![W/m/m]
                  GRDFLX(I,J) = FRC_URB2D(I,J)*G_URB+(1-FRC_URB2D(I,J))*SSOIL       ![W/m/m]
                  TSK(I,J) = FRC_URB2D(I,J)*TS_URB+(1-FRC_URB2D(I,J))*T1            ![K]
                  Q1 = FRC_URB2D(I,J)*QS_URB+(1-FRC_URB2D(I,J))*Q1            ![-]
      ! Convert QSFC back to mixing ratio
                  QSFC(I,J)= Q1/(1.0-Q1)
                  UST(I,J)= FRC_URB2D(I,J)*UST_URB+(1-FRC_URB2D(I,J))*UST(I,J)      ![m/s]
      
#if 0
          IF(IPRINT)THEN
      
          print*, ' FRC_URB2D', FRC_URB2D,                        &
          'ALB_URB',ALB_URB, 'ALBEDOK',ALBEDOK, &
          'ALBEDO(I,J)',  ALBEDO(I,J),                  &
          'SH_URB',SH_URB,'SHEAT',SHEAT, 'HFX(I,J)',HFX(I,J),  &
          'LH_KINEMATIC_URB',LH_KINEMATIC_URB,'ETA_KINEMATIC',  &
           ETA_KINEMATIC, 'QFX(I,J)',QFX(I,J),                  &
          'LH_URB',LH_URB, 'ETA',ETA, 'LH(I,J)',LH(I,J),        &
          'G_URB',G_URB,'SSOIL',SSOIL,'GRDFLX(I,J)', GRDFLX(I,J),&
          'TS_URB',TS_URB,'T1',T1,'TSK(I,J)',TSK(I,J),          &
          'QS_URB',QS_URB,'Q1',Q1,'QSFC(I,J)',QSFC(I,J)
           endif
#endif
      
      ! Renew Urban State Varialbes
      
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
      
               ENDIF                                   ! end of UCM CALL if block
      !--------------------------------------
      ! Urban Part End - urban
      !--------------------------------------
      
      !***  DIAGNOSTICS
                SMSTAV(I,J)=SOILW
                SMSTOT(I,J)=SOILM*1000.
                DO NS=1,NSOIL
                SMCREL(I,NS,J)=SMAV(NS)
                ENDDO
      
      !         Convert the water unit into mm
                SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
                UDRUNOFF(I,J)=UDRUNOFF(I,J)+RUNOFF2*DT*1000.0
      ! snow defined when fraction of frozen precip (FFROZP) > 0.5,
                IF(FFROZP.GT.0.5)THEN
                  ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
                ENDIF
                IF(SNOW(I,J).GT.0.)THEN
                  ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
      ! accumulated snow-melt energy
                  SNOPCX(I,J)=SNOPCX(I,J)-SNOMLT/FDTLIW
                ENDIF
      
              ENDIF                                                           ! endif of land-sea test

      ENDIF                                           ! ENDIF FOR MOSAIC DANLI  ! This corresponds to IF ((sf_surface_mosaic == 1) .AND. ((XLAND(I,J)-1.5).LT.0.) .AND. (XICE(I,J) < XICE_THRESHOLD) ) THEN

      ENDDO ILOOP                                                       ! of I loop
   ENDDO JLOOP                                                          ! of J loop   
       
!------------------------------------------------------
   END SUBROUTINE lsm_mosaic
!------------------------------------------------------
!===========================================================================
!
! subroutine lsm_mosaic_init: initialization of mosaic state variables
!
!===========================================================================   
  
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
                  ,ALBEDO,ALBBCK, EMISS, EMBCK,Z0                 &  !danli  
                  ,ALBEDO_mosaic,ALBBCK_mosaic, EMISS_mosaic      &  !danli
                  ,EMBCK_mosaic, ZNT_mosaic, Z0_mosaic            &  !danli
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
  
    INTEGER,  INTENT(IN)   ::       ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte 

   INTEGER, INTENT(IN)       ::     NLCAT, num_soil_layers, ISWATER,ISURBAN, ISICE, fractional_seaice

   LOGICAL , INTENT(IN) :: restart 

!   REAL,    DIMENSION( num_soil_layers), INTENT(INOUT) :: ZS, DZS

   REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme )    , &
            INTENT(IN)    ::                             SMOIS, &  !Total soil moisture
                                                         SH2O,  &  !liquid soil moisture       
                                                         TSLB      !STEMP

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
  !===========================================================================   
  ! CHOOSE THE TILES
  !===========================================================================  
  
  itf=min0(ite,ide-1)
  jtf=min0(jte,jde-1)

  ! simple test
   
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

  !===========================================================================   
  ! For non-seaice grids, eliminate the seaice-tiles
  !=========================================================================== 

     DO i = its,itf
        DO j = jts,jtf
        
         IF   (XLAND(I,J).LT.1.5)  THEN

             ICE = 0
                 IF( XICE(I,J).GE. XICE_THRESHOLD ) THEN
                   WRITE (message_text,fmt='(a,2I5)') 'sea-ice at point, I and J = ', i,j
                   CALL wrf_message(message_text)
                 ICE = 1
                 ENDIF   
           
          IF (ICE == 1)   Then         ! sea-ice case , eliminate sea-ice if they are not the dominant ones

          IF (IVGTYP(i,j) == isice)  THEN    ! if this grid cell is dominanted by ice, then do nothing

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

          ENDIF   ! for (IVGTYP(i,j) == isice )
          
          ELSEIF (ICE ==0)  THEN
          
          IF ((mosaic_cat_index(I,1,J) .EQ. ISWATER)) THEN    
          
          ! xland < 1.5 but the dominant land use category based on our calculation is water
	             
           IF (IVGTYP(i,j) .EQ. ISWATER) THEN  
           
           ! xland < 1.5 but the dominant land use category based on the geogrid calculation is water, this must be wrong
           
              CALL wrf_message("IN MOSAIC_INIT")
              WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j) 
              CALL wrf_message(message_text)
              CALL wrf_message("xland < 1.5 but the dominant land use category based on our calculation is water."//&
                   "In addition, the dominant land use category based on the geogrid calculation is water, this must be wrong")
           
           ENDIF  ! for (IVGTYP(i,j) .EQ. ISWATER)
           
           IF (IVGTYP(i,j) .NE. ISWATER) THEN 
           
           ! xland < 1.5,   the dominant land use category based on our calculation is water, but based on the geogrid calculation is not water, which might be due to the inconsistence between land use data and land-sea mask
           
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
	             
           ENDIF  ! for (IVGTYP(i,j) .NE. ISWATER)
          
           ELSE    !  for (mosaic_cat_index(I,1,J) .EQ. ISWATER)
           
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
             
           ENDIF !  for (mosaic_cat_index(I,1,J) .EQ. ISWATER)
             
          ENDIF  !  for ICE == 1
          
      ELSE  ! FOR (XLAND(I,J).LT.1.5)
      
                 ICE = 0
      
                     IF( XICE(I,J).GE. XICE_THRESHOLD ) THEN
                       WRITE (message_text,fmt='(a,2I6)') 'sea-ice at water point, I and J = ', i,j
                       CALL wrf_message(message_text)
                       ICE = 1
                     ENDIF  
      
           IF ((mosaic_cat_index(I,1,J) .NE. ISWATER)) THEN    
                
                ! xland > 1.5 and the dominant land use category based on our calculation is not water
      	             
                 IF (IVGTYP(i,j) .NE. ISWATER) THEN  
                 
                 ! xland > 1.5 but the dominant land use category based on the geogrid calculation is not water, this must be wrong
                 CALL wrf_message("IN MOSAIC_INIT")
                 WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j)
                 CALL wrf_message(message_text)
                 CALL wrf_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "in addition, the dominant land use category based on the geogrid calculation is not water,"//  &
                      "this must be wrong.")
                 ENDIF  ! for (IVGTYP(i,j) .NE. ISWATER)
                 
                 IF (IVGTYP(i,j) .EQ. ISWATER) THEN 
                 
                 ! xland > 1.5,   the dominant land use category based on our calculation is not water, but based on the geogrid calculation is water, which might be due to the inconsistence between land use data and land-sea mask

                 CALL wrf_message("IN MOSAIC_INIT")
                 WRITE(message_text,fmt='(a,3I6,2F8.2)') 'I,J,IVGTYP,XLAND,XICE = ',I,J,IVGTYP(I,J),xland(i,j),xice(i,j)
                 CALL wrf_message(message_text)
                 CALL wrf_message("xland > 1.5 but the dominant land use category based on our calculation is not water."// &
                      "however, the dominant land use category based on the geogrid calculation is water")
                 CALL wrf_message("This is fine. We do not need to do anyting because in the noaddrv, "//&
                      "we use xland as a criterion for whether using"// &
                      "mosaic or not when xland > 1.5, no mosaic will be used anyway")
      	             
                 ENDIF  ! for (IVGTYP(i,j) .NE. ISWATER)
                
           ENDIF !  for (mosaic_cat_index(I,1,J) .NE. ISWATER)
      
        ENDIF  ! FOR (XLAND(I,J).LT.1.5)

          ENDDO
      ENDDO
      
  !===========================================================================   
  ! normalize
  !=========================================================================== 

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
      
  !===========================================================================   
  ! initilize the variables
  !===========================================================================   

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

   ! simple test
   
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
      
 ENDIF      !  for not restart
      
!--------------------------------      
  END SUBROUTINE lsm_mosaic_init  
!--------------------------------  

END MODULE module_sf_noahdrv
