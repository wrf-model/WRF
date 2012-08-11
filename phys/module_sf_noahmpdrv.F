MODULE module_sf_noahmpdrv

!-------------------------------
  USE module_sf_noahmplsm
  USE module_sf_urban
  USE module_model_constants, ONLY : R_D, CP, XLF, XLV, RHOWATER, KARMAN
  USE module_sf_noahdrv, ONLY : SOIL_VEG_GEN_PARM
  USE module_sf_noah_seaice
  USE module_sf_noahlsm_glacial_only
  USE MODULE_RA_GFDLETA, ONLY: CAL_MON_DAY
#ifdef WRF_CHEM
  USE module_data_gocart_dust
#endif
!-------------------------------

!
CONTAINS
!
  SUBROUTINE noahmplsm(DZ8W,QV3D,P8W3D,T3D,TSK,                      &
       HFX,QFX,LH,GRDFLX, QGH,GSW,SWDOWN,GLW,SMSTAV,SMSTOT, &
       SFCRUNOFF, UDRUNOFF,IVGTYP,ISLTYP,VEGFRA,     &
       ALBEDO,ALBBCK,ZNT,Z0,TMN,XLAND,XICE,XICE_THRESHOLD,ISICE,EMISS,EMBCK,   &
       SNOWC,QSFC,RAINBL,                            &
       num_soil_layers,DT,DZS,ITIMESTEP,             &
       SMOIS,TSLB,SNOW,CANWAT,                       &
       CHS,CHS2,CQS2,CPM,ROVCP,SR,chklowq,qz0,       & !H
       myj,RIB,frpcpn,                               &
       SH2O,SNOWH,                                   & !H
       U_PHY,V_PHY,                                  & !I
       COSZ_URB2D, XLAT_URB2D,                       & !I
       SNOALB,                                       & !I
       SNOTIME,ACSNOM,ACSNOW,                        & !O
       idveg    ,iopt_crs ,iopt_btr ,iopt_run ,iopt_sfc ,iopt_frz ,iopt_inf , &
       iopt_rad ,iopt_alb ,iopt_snf ,iopt_tbot,iopt_stc ,                     &
       isnowxy  ,tvxy     ,tgxy     ,canicexy ,                               &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xlaixy   ,xsaixy   ,           &
       tradxy   ,tsxy     ,neexy    ,gppxy    ,nppxy    ,fvegxy   ,qinxy    , &
       runsfxy  ,runsbxy  ,ecanxy   ,edirxy   ,etranxy  ,fsaxy    ,firaxy   , &
       aparxy   ,psnxy    ,savxy    ,sagxy    ,taussxy  ,                     &
       fsnoxy   ,YR       ,JULIAN   ,                                         &
       potevp,                                       & !O

!jref:start
       qcxy     ,pblhxy   ,isurban  ,iz0tlnd  ,dx     , & !I
       chstarxy ,t2mvxy   ,t2mbxy   ,rssunxy  ,rsshaxy, bgapxy ,wgapxy ,gapxy,   & !O
       tgvxy    ,tgbxy    ,q2mvxy   ,q2mbxy  ,shdmaxxy, chvxy  ,chbxy  ,         &
!jref:end

       ids,ide, jds,jde, kds,kde,                    &
       ims,ime, jms,jme, kms,kme,                    &
       its,ite, jts,jte, kts,kte                     )
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
!-- ESNOW       sublimation from (or deposition to if <0) snowpack (W m-2)
!-- DEW         dewfall (or frostfall for t<273.15) (M)
! ----------------------------------------------------------------------
!-- ETP         potential evaporation (W m-2)
! ----------------------------------------------------------------------
!-- FLX1        precip-snow sfc (W m-2)
!-- FLX2        freezing rain latent heat flux (W m-2)
!-- FLX3        phase-change heat flux from snowmelt (W m-2)
! ----------------------------------------------------------------------
!-- ACSNOM      snow melt (mm) (water equivalent)
!-- ACSNOW      accumulated snow fall (mm) (water equivalent)
!-- POTEVP      accumulated potential evaporation (W/m^2)
!-- RIB         Bulk Richardson number from SFCLAY routine
! ----------------------------------------------------------------------
!-- RUNOFF1     surface runoff (m s-1), not infiltrating the surface
!-- RUNOFF2     subsurface runoff (m s-1), drainage out bottom of last
!                  soil layer (baseflow)
! ----------------------------------------------------------------------
!-- RC          canopy resistance (s m-1)
!-- PC          plant coefficient (unitless fraction, 0-1) where PC*ETP = actual transp
!-- EMISS       surface emissivity (between 0 and 1)
!-- EMBCK       Background surface emissivity (between 0 and 1)
!-- SHDMAX      Maximum vegetation fraction

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
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte
    
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: U_PHY
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) :: V_PHY
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: COSZ_URB2D
    REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: XLAT_URB2D


    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(IN   )    ::                            TMN, &
         &                                                XLAND, &
         &                                                 XICE, &
         &                                               VEGFRA, &
         &                                               SNOALB, &
         &                                                  GSW, &
         &                                               SWDOWN, &
         &                                                  GLW, &
         &                                                   Z0, &
         &                                               ALBBCK, &
         &                                               RAINBL, &
         &                                                EMBCK, &
         &                                                   SR


    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme )            , &
         INTENT(IN   )    ::                           QV3D, &
         p8w3D, &
         DZ8W, &
         T3D
!jref:start - changed to inout        
    REAL,     DIMENSION( ims:ime, jms:jme )                    , &
         INTENT(INOUT   )               ::               QGH,  &
         CHS,   &
         CPM
!jref:end         

    INTEGER, DIMENSION( ims:ime, jms:jme )                     , &
         INTENT(IN   )    ::                         IVGTYP, &
         ISLTYP

    INTEGER, INTENT(IN)       ::     num_soil_layers,ITIMESTEP
!jref:start - xice_threshold
    REAL,     INTENT(IN   )   ::     DT,ROVCP,XICE_THRESHOLD
    INTEGER,  INTENT(IN   )   ::     ISICE
!jref:end    

    REAL,     DIMENSION(1:num_soil_layers), INTENT(IN)::DZS

! IN and OUT

    REAL,     DIMENSION( ims:ime , 1:num_soil_layers, jms:jme ), &
         &    INTENT(INOUT)   ::                          SMOIS, &
         &                                                 SH2O, &
         &                                                 TSLB

    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         &   INTENT(INOUT)    ::                            TSK, &
         &                                                  HFX, &
         &                                                  QFX, &
         &                                                   LH, &
         &                                               GRDFLX, &
         &                                                 QSFC, &
         &                                                 CQS2, &
         &                                                 CHS2, &
         &                                                 SNOW, &
         &                                                SNOWC, &
         &                                                SNOWH, &
         &                                               CANWAT, &
         &                                               SMSTAV, &
         &                                               SMSTOT, &
         &                                            SFCRUNOFF, &
         &                                             UDRUNOFF, &
         &                                               ACSNOM, &
         &                                               ACSNOW, &
         &                                                EMISS, &
         &                                               POTEVP, &
         &                                                  RIB, &
         &                                               ALBEDO, &
         &                                                  ZNT

    REAL,    DIMENSION( ims:ime, jms:jme )                     , &
         INTENT(OUT)    ::                        CHKLOWQ
    REAL,DIMENSION(IMS:IME,JMS:JME),INTENT(IN) ::        QZ0

!niuin: 
! in
    INTEGER,  INTENT(IN) :: idveg     !dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1      
    INTEGER,  INTENT(IN) :: iopt_crs  !canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
    INTEGER,  INTENT(IN) :: iopt_btr  !soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
    INTEGER,  INTENT(IN) :: iopt_run  !runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
    INTEGER,  INTENT(IN) :: iopt_sfc  !surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
    INTEGER,  INTENT(IN) :: iopt_frz  !supercooled liquid water (1-> NY06; 2->Koren99)
    INTEGER,  INTENT(IN) :: iopt_inf  !frozen soil permeability (1-> NY06; 2->Koren99)
    INTEGER,  INTENT(IN) :: iopt_rad  !radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
    INTEGER,  INTENT(IN) :: iopt_alb  !snow surface albedo (1->BATS; 2->CLASS)
    INTEGER,  INTENT(IN) :: iopt_snf  !rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
    INTEGER,  INTENT(IN) :: iopt_tbot !lower boundary of soil temperature (1->zero-flux; 2->Noah)
    INTEGER,  INTENT(IN) :: iopt_stc  !snow/soil temperature time scheme

! in & out
    INTEGER, INTENT(IN) :: YR
    REAL,    INTENT(IN) :: JULIAN
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     !actual no. of snow layers
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        !vegetation canopy temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        !ground surface temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    !canopy-intercepted ice (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    !canopy-intercepted liquid water (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       !canopy air vapor pressure (pa)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       !canopy air temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        !momentum drag coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        !sensible heat exchange coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      !wetted or snowed fraction of the canopy (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    !snow mass at last time step(mm h2o)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    !snow albedo at last time step (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: taussxy     !non-dimensional snow age
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     !snowfall on the ground [mm/s]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    !lake water storage [mm]

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       !water table depth [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        !water in the "aquifer" [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        !groundwater storage [mm]
    REAL, DIMENSION(ims:ime,-2:num_soil_layers,jms:jme), INTENT(INOUT) :: zsnsoxy  !snow layer depth [m]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: tsnoxy   !snow temperature [K]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snicexy  !snow layer ice [mm]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snliqxy  !snow layer liquid water [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    !leaf mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    !mass of fine roots [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    !stem mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      !mass of wood (incl. woody roots) [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    !stable carbon in deep soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    !short-lived carbon, shallow soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xlaixy      !leaf area index
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      !stem area index
!jref:start    
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: SNOTIME      !snow age time
!jref:end    

!out
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: tradxy      !surface radiative temperature (k)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: tsxy        !surface temperature (k)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: neexy       !net ecosys exchange (g/m2/s CO2)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: gppxy       !gross primary assimilation [g/m2/s C]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: nppxy       !net primary productivity [g/m2/s C]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: fvegxy      !greenness vegetation fraction [-]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: qinxy       !groundwater recharge [mm/s]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: runsfxy     !surface runoff [mm/s]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: runsbxy     !subsurface runoff [mm/s]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: ecanxy      !evaporation of intercepted water (mm/s)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: edirxy      !soil surface evaporation rate (mm/s]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: etranxy     !transpiration rate (mm/s)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: fsaxy       !total absorbed solar radiation (w/m2)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: firaxy      !total net longwave rad (w/m2) [+ to atm]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: aparxy      !photosyn active energy by canopy (w/m2)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: psnxy       !total photosynthesis (umol co2/m2/s) [+]
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: savxy       !solar rad absorbed by veg. (w/m2)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: sagxy       !solar rad absorbed by ground (w/m2)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)  :: fsnoxy      !snow cover fraction (-)

!jref:start 
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)  :: chstarxy    !effective ch
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)  :: t2mvxy      !2m temperature of vegetation part
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)  :: t2mbxy      !2m temperature of bare ground part
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)  :: q2mvxy      !2m mixing ratio of vegetation part
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)  :: q2mbxy      !2m mixing ratio of bare ground part

    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)   :: qcxy        !cloud water mixing ratio
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)   :: pblhxy      !Planetary boundary layer from sfclay
    INTEGER                          , INTENT(IN)   :: isurban
    INTEGER                          , INTENT(IN)   :: iz0tlnd
    REAL                             , INTENT(IN)   :: dx
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: rssunxy        !sunlit leaf stomatal resistance (s/m)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: rsshaxy        !shaded leaf stomatal resistance (s/m)
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: bgapxy        !between gap fraction
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: wgapxy        !within gap fraction
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: gapxy        !within gap fraction
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: tgvxy
    REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT) :: tgbxy        
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN )::   shdmaxxy
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: chvxy        !sensible heat exchange coefficient vegetated
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: chbxy        !sensible heat exchange coefficient bare-ground
!jref:end 


!niuout

! Local variables (moved here from driver to make routine thread safe, 20031007 jm)

    INTEGER :: YEARLEN

    REAL  ::  ETP, SSOIL,EC, ESNOW,             &
         FLX1,FLX2,FLX3,DEW,FDOWN,RC,PC,FFROZP

!niuin
!locals (prognostic):

    INTEGER                             :: isnow        !actual no. of snow layers
    REAL, DIMENSION(-2:num_soil_layers) :: stc          !snow/soil tmperatures
    REAL, DIMENSION( 1:num_soil_layers) :: smc          !vol. soil moisture (m3/m3)
    REAL, DIMENSION( 1:num_soil_layers) :: smh2o        !vol. soil liquid water (m3/m3)
    REAL                                :: tv           !vegetation canopy temperature
    REAL                                :: tg           !ground surface temperature
    REAL                                :: canice       !canopy-intercepted ice (mm)
    REAL                                :: canliq       !canopy-intercepted liquid water (mm)
    REAL                                :: snowd        !snow depth (m)
    REAL                                :: swe          !snow water equivalent (mm)
    REAL                                :: eah          !canopy air vapor pressure (pa)
    REAL                                :: tah          !canopy air temperature (k)
    REAL                                :: cm           !momentum drag coefficient
    REAL                                :: ch           !sensible heat exchange coefficient
    REAL                                :: fwet         !wetted or snowed fraction of the canopy (-)
    REAL                                :: sneqvo       !snow mass at last time step(mm h2o)
    REAL                                :: albold       !snow albedo at last time step (-)
    REAL                                :: qsnow        !snowfall on the ground [mm/s]
    REAL                                :: wslake       !lake water storage [mm]

    REAL                                :: zwt          !water table depth [m]
    REAL                                :: wa           !water in the "aquifer" [mm]
    REAL                                :: wt           !groundwater storage [mm]
    REAL, DIMENSION(-2:num_soil_layers) :: zsnso        !snow layer depth [m]
    REAL, DIMENSION(-2:              0) :: tsno         !snow temperature [K]
    REAL, DIMENSION(-2:              0) :: snice        !snow layer ice [mm]
    REAL, DIMENSION(-2:              0) :: snliq        !snow layer liquid water [mm]
    REAL                                :: lfmass       !leaf mass [g/m2]
    REAL                                :: rtmass       !mass of fine roots [g/m2]
    REAL                                :: stmass       !stem mass [g/m2]
    REAL                                :: wood         !mass of wood (incl. woody roots) [g/m2]
    REAL                                :: stblcp       !stable carbon in deep soil [g/m2]
    REAL                                :: fastcp       !short-lived carbon, shallow soil [g/m2]
    REAL                                :: plai         !leaf area index
    REAL                                :: psai         !stem area index
    REAL                                :: tauss        !non-dimensional snow age

!jref:start
    REAL                                :: chstar2
    REAL                                :: cqstar2
    REAL                                :: chstar       !effective ch
    REAL                                :: tstar
    REAL                                :: t2mv         !2m temperature of vegetation part
    REAL                                :: t2mb         !2m temperature of bare ground part
    REAL                                :: q2mv         !2m mixing ratio of vegetation part
    REAL                                :: q2mb         !2m mixing ratio of bare ground part
    REAL                                :: qc           !
    REAL                                :: t2m
    REAL                                :: pblh 
    REAL                                :: qsfc1d
    REAL, DIMENSION(ims:ime,jms:jme)    :: tstarxy      !effective skin temperature
    REAL, DIMENSION(ims:ime,jms:jme)    :: chstar2xy    !effective 2m exchange coefficients
    REAL                                :: rssun
    REAL                                :: rssha
    REAL                                :: bgap
    REAL                                :: wgap
    REAL                                :: gap
    REAL                                :: tgv
    REAL                                :: tgb
    REAL                                :: snowhk
    REAL                                :: snotime1
    REAL                                :: qv1d         !mixing ratio
    REAL                                :: dz8w1d
    REAL                                :: shdmax
    REAL                                :: chv          !sensible heat exchange coefficient vegetated
    REAL                                :: chb          !sensible heat exchange coefficient bare-ground
!jref:end

!out (outputs)

    REAL                                :: trad         !surface radiative temperature (k)
    REAL                                :: ts           !surface temperature (k)
    REAL                                :: nee          !net ecosys exchange (g/m2/s CO2)
    REAL                                :: gpp          !gross primary assimilation [g/m2/s C]
    REAL                                :: npp          !net primary productivity [g/m2/s C]
    REAL                                :: fveg         !greenness vegetation fraction [-]
    REAL                                :: qin          !groundwater recharge [mm/s]
    REAL                                :: runsf        !surface runoff [mm/s]
    REAL                                :: runsb        !subsurface runoff [mm/s]
    REAL                                :: ecan         !evaporation of intercepted water (mm/s)
    REAL                                :: esoil        !soil surface evaporation rate (mm/s]
    REAL                                :: etran        !transpiration rate (mm/s)
    REAL                                :: fsa          !total absorbed solar radiation (w/m2)
    REAL                                :: fira         !total net longwave rad (w/m2) [+ to atm]
    REAL                                :: fsh          !total sensible heat (w/m2) [+ to atm]
    REAL                                :: flh          !total latent heat (w/m2) [+ to atm]
    REAL                                :: apar         !photosyn active energy by canopy (w/m2)
    REAL                                :: psn          !total photosynthesis (umol co2/m2/s) [+]
    REAL                                :: sav          !solar rad absorbed by veg. (w/m2)
    REAL                                :: sag          !solar rad absorbed by ground (w/m2)
    REAL                                :: fsno         !snow cover fraction (-)
    REAL                                :: salb         !surface albedo (-)
    REAL                                :: errwat
    REAL                                :: qmelt
    REAL                                :: ponding
    REAL                                :: ponding1
    REAL                                :: ponding2

!local

    real                                :: fsr          !total reflected solar radiation (w/m2)
    real                                :: fcev         !canopy evaporation heat (w/m2) [+ to atm]
    real                                :: fgev         !ground evaporation heat (w/m2) [+ to atm]
    real                                :: fctr         !transpiration heat flux (w/m2) [+ to atm]
    real, dimension(-2:              0) :: ficeold      !snow layer liquid water [mm]

    INTEGER :: ILOC      !grid index
    INTEGER :: JLOC      !grid index
    INTEGER :: ISC       !soil color index
    INTEGER :: IST       !surface type 1-soil; 2-lake

!niuout

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

    REAL  :: DQSDT2, LWDN, PRCP, PSFC, UU, VV, CO2AIR, O2AIR,         &
         &   Q2SAT,Q2SATI,SFCPRS,SFCTMP,SHDFAC,SNOALB1,               &
         &   SOLDN,TBOT,ZLVL, Q2K,ALBBRD, ETA, ETA_KINEMATIC,         &
         &   EMBRD, FOLN, LAT,                                        &
         &   Z0K,RUNOFF1,RUNOFF2,SOLNET,E2SAT,SFCTSNO

    REAL :: RIBB
    REAL ::  FDTW

    REAL  :: EMISSI

    REAL  :: SNCOVR,SNEQV,CHK,TH2

    REAL  :: SMCMAX,SNOMLT,SOILM,SOILW,Q1,T1

    REAL  :: Z0BRD
!
    REAL  :: COSZ
!
!niu      REAL, DIMENSION(1:num_soil_layers)::  SLDPTH, STC,SMC,SWC
    REAL, DIMENSION(1:num_soil_layers)::  SLDPTH,SWC

!jref:start    
    REAL, DIMENSION(1:num_soil_layers):: STCNEW
!jref:end     
!
    REAL, DIMENSION(1:num_soil_layers) ::     ZSOIL, RTDIS
    REAL, PARAMETER  :: TRESH=.95E0, A2=17.67,A3=273.15,A4=29.65,   &
         T0=273.16E0, ELWV=2.50E6,  A23M4=A2*(A3-A4)

    ! Used for calculating the 2-m Potential Temperature:
    REAL, PARAMETER                                       :: CAPA=R_D/CP
    REAL                                                  :: APELM
    REAL                                                  :: APES
    REAL                                                  :: SFCTH2

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

! MEK JUL2007
    FDTW=DT/(XLV*RHOWATER)
! debug printout
    IPRINT=.false.

!      SLOPETYP=2
    SLOPETYP=1
!      SHDMIN=0.00

    YEARLEN = 365
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    NSOIL=num_soil_layers

    DO NS=1,NSOIL
       SLDPTH(NS)=DZS(NS)
    ENDDO

    call noahmp_options(idveg     ,iopt_crs  ,iopt_btr  ,iopt_run  ,iopt_sfc  ,iopt_frz , &
         iopt_inf  ,iopt_rad  ,iopt_alb  ,iopt_snf  ,iopt_tbot, iopt_stc )

    ISC = 4                  ! soil color: assuming a middle color category ?????????

    ZSOIL(1) = -SLDPTH(1)    ! move out of  x-y do loops
    DO KZ = 2, NSOIL
       ZSOIL(KZ)         = -SLDPTH(KZ) + ZSOIL(KZ-1)
    END DO
    FOLN                 = 1.0

!niuout

    DO J=jts,jte

       IF(ITIMESTEP.EQ.1)THEN
          DO I=its,ite

!*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
             IF((XLAND(I,J)-1.5).GE.0.)THEN
! check sea-ice point
                IF(XICE(I,J).EQ.1..and.IPRINT)PRINT*,' sea-ice at water point, I=',I,  &
                     'J=',J
!***   Open Water Case
                SMSTAV(I,J)=1.0
                SMSTOT(I,J)=1.0
                DO NS=1,NSOIL
                   SMOIS(I,NS,J)=1.0
                   TSLB(I,NS,J)=273.16                                          !STEMP
                ENDDO
             ELSE
                IF(XICE(I,J).EQ.1.)THEN
!***        SEA-ICE CASE
                   SMSTAV(I,J)=1.0
                   SMSTOT(I,J)=1.0
                   DO NS=1,NSOIL
                      SMOIS(I,NS,J)=1.0
                   ENDDO
                ENDIF
             ENDIF
!
          ENDDO
       ENDIF                                                               ! end of initialization over ocean


!-----------------------------------------------------------------------
       DO I=its,ite
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
             CHKLOWQ(I,J)=0.
          ELSE
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
!          LWDN=GLW(I,J)*EMISSI
          LWDN=GLW(I,J)
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
          SNOALB1=SNOALB(I,J)     !NEW

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
          ELSE
! Land or sea-ice case

             IF (XICE(I,J) .GT. 0.5) THEN
                ICE=1
             ELSE
                ICE=0
             ENDIF
             DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2

             IF(SNOW(I,J).GT.0.0)THEN
! snow on surface (use ice saturation properties)
                SFCTSNO=SFCTMP
                E2SAT=611.2*EXP(6174.*(1./273.15 - 1./SFCTSNO))
                Q2SATI=0.622*E2SAT/(SFCPRS-E2SAT)
                Q2SATI=Q2SATI/(1.0+Q2SATI)    ! spec. hum.
                IF(T1 .GT. 273.15)THEN
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

             IF(ICE.EQ.0)THEN
                TBOT=TMN(I,J)
             ELSE
                TBOT=271.16
             ENDIF
             IF(VEGTYP.EQ.25) SHDFAC=0.0000
             IF(VEGTYP.EQ.26) SHDFAC=0.0000
             IF(VEGTYP.EQ.27) SHDFAC=0.0000
             IF(SOILTYP.EQ.14.AND.XICE(I,J).EQ.0.)THEN
                IF(IPRINT)PRINT*,' SOIL TYPE FOUND TO BE WATER AT A LAND-POINT'
                IF(IPRINT)PRINT*,i,j,'RESET SOIL in surfce.F'
                SOILTYP=7
             ENDIF

!-------------------------------------------
             ALBBRD=ALBBCK(I,J)
             Z0BRD=Z0(I,J)
             EMBRD=EMBCK(I,J)
!jref:start - check if this is correct!! Maybe snowd
             RIBB=RIB(I,J)
             SNOTIME1 = SNOTIME(I,J)
!jref:end             

!FEI: temporaray arrays above need to be changed later by using SI

!niu          DO 70 NS=1,NSOIL
!niu            SMC(NS)=SMOIS(I,NS,J)
!niu            STC(NS)=TSLB(I,NS,J)                                          !STEMP
!niu            SWC(NS)=SH2O(I,NS,J)
!niu   70     CONTINUE
!

             IF( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == 31 .or. &
                  IVGTYP(I,J) == 32 .or. IVGTYP(I,J) == 33) THEN
                VEGTYP = ISURBAN
             ENDIF

             IST    = 1 
             IF ( VEGTYP == ISWATER ) IST = 2 ! lake points

             CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,SLDPTH,ZSOIL,NSOIL,ISURBAN)

             UU                   = U_PHY(I,1,J)
             VV                   = V_PHY(I,1,J)
             CO2AIR               = 395.E-06 * SFCPRS       !partial pressure co2 (pa)
             O2AIR                = 0.209    * SFCPRS       !partial pressure  o2 (pa)
             COSZ                 = COSZ_URB2D(I,J)
             LAT                  = XLAT_URB2D(I,J)

             isnow                = isnowxy (i,j)
             stc  (isnow+1:    0) = tsnoxy  (i,isnow+1:    0,j)
             stc  (      1:nsoil) = tslb    (i,      1:nsoil,j)
             smc (       1:nsoil) = smois   (i,      1:nsoil,j)
             smh2o(      1:nsoil) = sh2o    (i,      1:nsoil,j)
             tv                   = tvxy    (i,j)
             tg                   = tgxy    (i,j)
             canliq               = canliqxy(i,j)
             canice               = canicexy(i,j)
             snowd                = snowh   (i,j)
             swe                  = snow    (i,j)
             eah                  = eahxy   (i,j)
             tah                  = tahxy   (i,j)
             cm                   = cmxy    (i,j)
             ch                   = chxy    (i,j)
             tauss                = taussxy (i,j)
!jref:start             
             chstar               = chs     (i,j)
             chstar2              = chs2    (i,j)
             cqstar2              = cqs2    (i,j)
             tstar                = T1
             qc                   = qcxy    (i,j)
             pblh                 = pblhxy  (i,j)
             qsfc1d               = qsfc    (i,j)
             t2mv                 = t2mvxy  (i,j)
             t2mb                 = t2mbxy  (i,j)
             q2mv                 = q2mvxy  (i,j)
             q2mb                 = q2mbxy  (i,j)
             qv1d                 = qv3d    (i,1,j) ! seaice/glacial needs mixing ratio (q2k = specific hum).             
             dz8w1d               = dz8w    (i,1,j)
             shdmax               = shdmaxxy (i,j)/100. !fraction
!jref:end             

             fwet                 = fwetxy  (i,j)
             sneqvo               = sneqvoxy(i,j)
             albold               = alboldxy(i,j)
             qsnow                = qsnowxy (i,j)
             wslake               = wslakexy(i,j)

             zwt                  = zwtxy   (i,j)
             wa                   = waxy    (i,j)
             wt                   = wtxy    (i,j)
             zsnso(isnow+1:nsoil) = zsnsoxy (i,isnow+1:nsoil,j)
             snice(isnow+1:    0) = snicexy (i,isnow+1:    0,j)
             snliq(isnow+1:    0) = snliqxy (i,isnow+1:    0,j)

             lfmass               = lfmassxy(i,j)
             rtmass               = rtmassxy(i,j)
             stmass               = stmassxy(i,j)
             wood                 = woodxy  (i,j)
             stblcp               = stblcpxy(i,j)
             fastcp               = fastcpxy(i,j)
             plai                 = xlaixy  (i,j)
             psai                 = xsaixy  (i,j)

             ficeold(isnow+1:0)   = snicexy(i,isnow+1:0,j) &
                  /(snicexy(i,isnow+1:0,j)+snliqxy(i,isnow+1:0,j))

! glacial, seaice split - jref

    IF ( XICE(I,J) >= XICE_THRESHOLD ) THEN

       SH2O  (i,1:nsoil,j) = 1.0
       XLAIXY(i,j)         = 0.01

       cycle ! Skip any processing at sea-ice points

    ELSE IF ( VEGTYP == ISICE ) THEN

       SNCOVR = SNOWC(I,J)
       swe = swe*0.001 !jref mm -> m
       if ( (swe.ne.0..AND.snowd.eq.0.).or.(snowd.le.swe) )THEN
             snowd= 5.*swe
       endif

       CALL SFLX_GLACIAL(I,J,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,   &    !C
            &    LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2K,              &    !F
            &    TH2,Q2SAT,DQSDT2,                                &    !I
            &    ALBBRD, SNOALB1,TBOT, Z0BRD, Z0K, EMISSI, EMBRD, &    !S
            &    tstar,STC(1:NSOIL),snowd,swe,salb,chstar,        &    !H
            &    ETA,fsh, ETA_KINEMATIC,FDOWN,                    &    !O
            &    ESNOW,DEW,                                       &    !O
            &    ETP,SSOIL,                                       &    !O
            &    FLX1,FLX2,FLX3,                                  &    !O
            &    SNOMLT,SNCOVR,                                   &    !O
            &    runsf,                                         &    !O
            &    Q1,                                              &    !D
            &    SNOTIME1,                                        &
            &    RIBB)

       tgb    = sfctmp ! Bare ground temperature will be the surface temperature over glacial points. 
       tgv    = 0.0    ! Temperature under vegetation undefined over glacial points.
       swe    = swe*1000.
       plai   = 0.01  ! Should make this zero?
       smc    = 1.00
       smh2o  = 1.00  ! Something else?
       runsb  = 0.00
       fgev   = ETA
       fcev   = 0.
       fctr   = 0.
       soilm  = 1.0   ! Something else?
!       SMAV    = 1.00  ! Something else?
       SNOWC(I,J) = 1.0
       
       QFX(I,J) = eta_kinematic
       POTEVP(I,J)=POTEVP(I,J)+ETP*FDTW

       CHS2(I,J) = CQS2(I,J)
       IF ( Q1 .GT. QSFC(I,J) ) THEN
          CQS2(I,J) = CHS(I,J)
       ENDIF

    ELSE
!jref:end

             nee = -1.E36
             npp = -1.E36

#if 0
             if ( I == 15 .and. J == 5 ) then
                ! Intent (IN) or Intent (INOUT), but not Intent (OUT)
                write(*,'("Before call to NOAHMP_SFLX, at point ", I8, I8)') i, j
                write(*,'(10x, "ICE        = ",  I10   )') ICE
                write(*,'(10x, "IST        = ",  I10   )') IST
                write(*,'(10x, "VEGTYP     = ",  I10   )') VEGTYP
                write(*,'(10x, "ISC        = ",  I10   )') ISC
                write(*,'(10x, "NSOIL      = ",  I10   )') NSOIL
                write(*,'(10x, "ZSOIL      = ", 7F20.10)') ZSOIL
                write(*,'(10x, "DT         = ",  F20.10)') DT
                write(*,'(10x, "QV1D       = ",  F20.10)') QV1D
                write(*,'(10x, "SFCTMP     = ",  F20.10)') SFCTMP
                write(*,'(10x, "UU         = ",  F20.10)') UU
                write(*,'(10x, "VV         = ",  F20.10)') VV
                write(*,'(10x, "SOLDN      = ",  F20.10)') SOLDN
                write(*,'(10x, "LWDN       = ",  F20.10)') LWDN
                write(*,'(10x, "PRCP       = ",  F20.10)') PRCP
                write(*,'(10x, "ZLVL       = ",  F20.10)') ZLVL
                write(*,'(10x, "CO2AIR     = ",  F20.10)') CO2AIR
                write(*,'(10x, "O2AIR      = ",  F20.10)') O2AIR
                write(*,'(10x, "COSZ       = ",  F20.10)') COSZ
                write(*,'(10x, "TBOT       = ",  F20.10)') TBOT
                write(*,'(10x, "FOLN       = ",  F20.10)') FOLN
                write(*,'(10x, "SFCPRS     = ",  F20.10)') SFCPRS
                write(*,'(10x, "SHDFAC     = ",  F20.10)') SHDFAC
                write(*,'(10x, "LAT        = ",  F20.10)') LAT
                write(*,'(10x, "DZ8W1D     = ",  F20.10)') DZ8W1D
                write(*,'(10x, "EAH        = ",  F20.10)') EAH
                write(*,'(10x, "TAH        = ",  F20.10)') TAH
                write(*,'(10x, "FWET       = ",  F20.10)') FWET
                write(*,'(10x, "FICEOLD    = ", 7F20.10)') FICEOLD
                write(*,'(10x, "QSNOW      = ",  F20.10)') QSNOW
                write(*,'(10x, "SNEQVO     = ",  F20.10)') SNEQVO
                write(*,'(10x, "ISNOW      = ",  F20.10)') ISNOW
                write(*,'(10x, "ZSNSO      = ", 7F20.10)') ZSNSO
                write(*,'(10x, "CANLIQ     = ",  F20.10)') CANLIQ
                write(*,'(10x, "CANICE     = ",  F20.10)') CANICE
                write(*,'(10x, "SNOWD      = ",  F20.10)') SNOWD
                write(*,'(10x, "SWE        = ",  F20.10)') SWE
                write(*,'(10x, "SNICE      = ", 7F20.10)') SNICE
                write(*,'(10x, "SNLIQ      = ", 7F20.10)') SNLIQ
                write(*,'(10x, "TV         = ",  F20.10)') TV
                write(*,'(10x, "TG         = ",  F20.10)') TG
                write(*,'(10x, "STC        = ", 7F20.10)') STC
                write(*,'(10x, "SMH2O      = ", 7F20.10)') SMH2O
                write(*,'(10x, "SMC        = ", 7F20.10)') SMC
                write(*,'(10x, "ZWT        = ",  F20.10)') ZWT
                write(*,'(10x, "WA         = ",  F20.10)') WA
                write(*,'(10x, "WT         = ",  F20.10)') WT
                write(*,'(10x, "WSLAKE     = ",  F20.10)') WSLAKE
                write(*,'(10x, "LFMASS     = ",  F20.10)') LFMASS
                write(*,'(10x, "RTMASS     = ",  F20.10)') RTMASS
                write(*,'(10x, "STMASS     = ",  F20.10)') STMASS
                write(*,'(10x, "WOOD       = ",  F20.10)') WOOD
                write(*,'(10x, "STBLCP     = ",  F20.10)') STBLCP
                write(*,'(10x, "FASTCP     = ",  F20.10)') FASTCP
                write(*,'(10x, "PLAI       = ",  F20.10)') PLAI
                write(*,'(10x, "PSAI       = ",  F20.10)') PSAI
                write(*,'(10x, "ALBOLD     = ",  F20.10)') ALBOLD
                write(*,'(10x, "CM         = ",  F20.10)') CM
                write(*,'(10x, "CH         = ",  F20.10)') CH
                write(*,'(10x, "DX         = ",  F20.10)') DX
                write(*,'(10x, "ISURBAN    = ",  I10   )') ISURBAN
                write(*,'(10x, "IZ0TLND    = ",  I10   )') IZ0TLND
                write(*,'(10x, "QC         = ",  F20.10)') QC
                write(*,'(10x, "PBLH       = ",  F20.10)') PBLH
                write(*,'(10x, "QSFC1D     = ",  F20.10)') QSFC1D
                write(*,'(10x, "PSFC       = ",  F20.10)') PSFC
             endif
#endif

             CALL NOAHMP_SFLX (&
                  I       , J       , LAT     , YEARLEN , JULIAN  , COSZ    , & ! IN : Time/Space-related
                  DT      , DX      , DZ8W1D  , NSOIL   , ZSOIL   , 3       , & ! IN : Model configuration 
                  SHDFAC  , SHDMAX  , VEGTYP  , ISURBAN , ICE     , IST     , & ! IN : Vegetation/Soil characteristics
                  ISC     ,                                                   & ! IN : Vegetation/Soil characteristics
                  IZ0TLND ,                                                   & ! IN : User options
                  SFCTMP  , SFCPRS  , PSFC    , UU      , VV      , QV1D    , & ! IN : Forcing
                  QC      , SOLDN   , LWDN    , PRCP    , TBOT    , CO2AIR  , & ! IN : Forcing
                  O2AIR   , FOLN    , FICEOLD , PBLH   ,                      & ! IN : Forcing
                  ZLVL    , ALBOLD  , SNEQVO  ,                               & ! IN/OUT : 
                  STC     , SMH2O   , SMC     , TAH     , EAH     , FWET    , & ! IN/OUT : 
                  CANLIQ  , CANICE  , TV      , TG      , QSFC1D  , QSNOW   , & ! IN/OUT : 
                  ISNOW   , ZSNSO   , SNOWD   , SWE     , SNICE   , SNLIQ   , & ! IN/OUT : 
                  ZWT     , WA      , WT      , WSLAKE  , LFMASS  , RTMASS  , & ! IN/OUT : 
                  STMASS  , WOOD    , STBLCP  , FASTCP  , PLAI    , PSAI    , & ! IN/OUT : 
                  CM      , CH      , CHSTAR  , TAUSS   ,                     & ! IN/OUT : 
                  FSA     , FSR     , FIRA    , FSH     , SSOIL   , FCEV    , & ! OUT : 
                  FGEV    , FCTR    , ECAN    , ETRAN   , ESOIL   , TRAD    , & ! OUT : 
                  TS      , TGB     , TGV     , T2MV    , T2MB    , TSTAR   , & ! OUT : 
                  Q1      , Q2MV    , Q2MB    , RUNSF   , RUNSB   , APAR    , & ! OUT : 
                  PSN     , SAV     , SAG     , FSNO    , NEE     , GPP     , & ! OUT : 
                  NPP     , FVEG    , SALB    , QMELT   , PONDING , PONDING1, & ! OUT : 
                  PONDING2, RSSUN   , RSSHA   , BGAP    , WGAP    , GAP     , & ! OUT : 
                  ERRWAT  , CHV     , CHB     , EMISSI)                         ! OUT :

#if 0
            if ( I == 15 .and. J == 5 ) then
                ! Intent (OUT) or Intent (INOUT), but not Intent (IN)
                write(*,'("After  call to NOAHMP_SFLX, at point ", I8, I8)') i, j
                write(*,'(10x, "ZLVL       = ", 7F20.10)') ZLVL
                write(*,'(10x, "EAH        = ",  F20.10)') EAH
                write(*,'(10x, "TAH        = ",  F20.10)') TAH
                write(*,'(10x, "FWET       = ",  F20.10)') FWET
                write(*,'(10x, "QSNOW      = ",  F20.10)') QSNOW
                write(*,'(10x, "SNEQVO     = ",  F20.10)') SNEQVO
                write(*,'(10x, "ISNOW      = ",  F20.10)') ISNOW
                write(*,'(10x, "ZSNSO      = ", 7F20.10)') ZSNSO
                write(*,'(10x, "CANLIQ     = ",  F20.10)') CANLIQ
                write(*,'(10x, "CANICE     = ",  F20.10)') CANICE
                write(*,'(10x, "SNOWD      = ",  F20.10)') SNOWD
                write(*,'(10x, "SWE        = ",  F20.10)') SWE
                write(*,'(10x, "SNICE      = ", 3F20.10)') SNICE
                write(*,'(10x, "SNLIQ      = ", 3F20.10)') SNLIQ
                write(*,'(10x, "TV         = ",  F20.10)') TV
                write(*,'(10x, "TG         = ",  F20.10)') TG
                write(*,'(10x, "STC        = ", 7F20.10)') STC
                write(*,'(10x, "SMH2O      = ", 7F20.10)') SMH2O
                write(*,'(10x, "SMC        = ", 7F20.10)') SMC
                write(*,'(10x, "ZWT        = ",  F20.10)') ZWT
                write(*,'(10x, "WA         = ",  F20.10)') WA
                write(*,'(10x, "WT         = ",  F20.10)') WT
                write(*,'(10x, "WSLAKE     = ",  F20.10)') WSLAKE
                write(*,'(10x, "LFMASS     = ",  F20.10)') LFMASS
                write(*,'(10x, "RTMASS     = ",  F20.10)') RTMASS
                write(*,'(10x, "STMASS     = ",  F20.10)') STMASS
                write(*,'(10x, "WOOD       = ",  F20.10)') WOOD
                write(*,'(10x, "STBLCP     = ",  F20.10)') STBLCP
                write(*,'(10x, "FASTCP     = ",  F20.10)') FASTCP
                write(*,'(10x, "PLAI       = ",  F20.10)') PLAI
                write(*,'(10x, "PSAI       = ",  F20.10)') PSAI
                write(*,'(10x, "ALBOLD     = ",  F20.10)') ALBOLD
                write(*,'(10x, "CM         = ",  F20.10)') CM
                write(*,'(10x, "CH         = ",  F20.10)') CH
                write(*,'(10x, "FSA        = ",  F20.10)') FSA
                write(*,'(10x, "FSR        = ",  F20.10)') FSR
                write(*,'(10x, "FIRA       = ",  F20.10)') FIRA
                write(*,'(10x, "FSH        = ",  F20.10)') FSH
                write(*,'(10x, "SSOIL      = ",  F20.10)') SSOIL
                write(*,'(10x, "FCEV       = ",  F20.10)') FCEV
                write(*,'(10x, "FGEV       = ",  F20.10)') FGEV
                write(*,'(10x, "FCTR       = ",  F20.10)') FCTR
                write(*,'(10x, "TRAD       = ",  F20.10)') TRAD
                write(*,'(10x, "ECAN       = ",  F20.10)') ECAN
                write(*,'(10x, "ETRAN      = ",  F20.10)') ETRAN
                write(*,'(10x, "ESOIL      = ",  F20.10)') ESOIL
                write(*,'(10x, "RUNSF      = ",  F20.10)') RUNSF
                write(*,'(10x, "RUNSB      = ",  F20.10)') RUNSB
                write(*,'(10x, "APAR       = ",  F20.10)') APAR
                write(*,'(10x, "PSN        = ",  F20.10)') PSN
                write(*,'(10x, "SAV        = ",  F20.10)') SAV
                write(*,'(10x, "SAG        = ",  F20.10)') SAG
                write(*,'(10x, "FSNO       = ",  F20.10)') FSNO
                write(*,'(10x, "NEE        = ",  F20.10)') NEE
                write(*,'(10x, "GPP        = ",  F20.10)') GPP
                write(*,'(10x, "NPP        = ",  F20.10)') NPP
                write(*,'(10x, "TS         = ",  F20.10)') TS
                write(*,'(10x, "FVEG       = ",  F20.10)') FVEG
                write(*,'(10x, "SALB       = ",  F20.10)') SALB
                write(*,'(10x, "ERRWAT     = ",  F20.10)') ERRWAT
                write(*,'(10x, "QMELT      = ",  F20.10)') QMELT
                write(*,'(10x, "PONDING    = ",  F20.10)') PONDING
                write(*,'(10x, "PONDING1   = ",  F20.10)') PONDING1
                write(*,'(10x, "PONDING2   = ",  F20.10)') PONDING2
                write(*,'(10x, "QSFC1D     = ",  F20.10)') QSFC1D
                write(*,'(10x, "CHSTAR     = ",  F20.10)') CHSTAR
                write(*,'(10x, "TSTAR      = ",  F20.10)') TSTAR
                write(*,'(10x, "T2MV       = ",  F20.10)') T2MV
                write(*,'(10x, "T2MB       = ",  F20.10)') T2MB
                write(*,'(10x, "RSSUN      = ",  F20.10)') RSSUN
                write(*,'(10x, "RSSHA      = ",  F20.10)') RSSHA
                write(*,'(10x, "BGAP       = ",  F20.10)') BGAP
                write(*,'(10x, "WGAP       = ",  F20.10)') WGAP
                write(*,'(10x, "GAP        = ",  F20.10)') GAP
                write(*,'(10x, "TGV        = ",  F20.10)') TGV
                write(*,'(10x, "TGB        = ",  F20.10)') TGB
                write(*,'(10x, "Q1         = ",  F20.10)') Q1
             endif
#endif

             !Q1           = eah * 0.622 / (SFCPRS - 0.378*eah)

             chs2     (i,j)               = chstar2
             cqs2     (i,j)               = cqstar2
             QFX      (I,J)               = ecan + esoil + etran
             SNOWC    (I,J)               = fsno

   ENDIF ! glacial, seaice split ends 
!jref:end 

             isnowxy  (i,j)               = isnow
             canliqxy (i,j)               = canliq
             canicexy (i,j)               = canice
             snowh    (i,j)               = snowd
             snow     (i,j)               = swe
             zsnsoxy  (i,isnow+1:nsoil,j) = zsnso (isnow+1:nsoil)
             tslb     (i,      1:nsoil,j) = stc   (      1:nsoil)
             tsnoxy   (i,isnow+1:    0,j) = stc   (isnow+1:    0)
             smois    (i,      1:nsoil,j) = smc   (      1:nsoil)
             sh2o     (i,      1:nsoil,j) = smh2o (      1:nsoil)
             snicexy  (i,isnow+1:    0,j) = snice (isnow+1:    0)
             snliqxy  (i,isnow+1:    0,j) = snliq (isnow+1:    0)
             tvxy     (i,j)               = tv
             tgxy     (i,j)               = tg
             zwtxy    (i,j)               = zwt
             waxy     (i,j)               = wa
             wtxy     (i,j)               = wt
             lfmassxy (i,j)               = lfmass
             rtmassxy (i,j)               = rtmass
             stmassxy (i,j)               = stmass
             woodxy   (i,j)               = wood
             stblcpxy (i,j)               = stblcp
             fastcpxy (i,j)               = fastcp
             xlaixy   (i,j)               = plai
             xsaixy   (i,j)               = psai
             emiss    (i,j)               = emissi
             eahxy    (i,j)               = eah
             tahxy    (i,j)               = tah
             fwetxy   (i,j)               = fwet
             sneqvoxy (i,j)               = sneqvo
             alboldxy (i,j)               = albold
             taussxy  (i,j)               = tauss
             qsnowxy  (i,j)               = qsnow
             wslakexy (i,j)               = wslake
             cmxy     (i,j)               = cm
!jref:start             
             chxy     (i,j)               = chstar
             rssunxy  (i,j)               = rssun
             rsshaxy  (i,j)               = rssha
             bgapxy   (i,j)               = bgap
             wgapxy   (i,j)               = wgap
             gapxy    (i,j)               = gap
             tgvxy    (i,j)               = tgv
             tgbxy    (i,j)               = tgb
             chvxy    (i,j)               = chv
             chbxy    (i,j)               = chb
!jref:end             

             !for output

             runsfxy  (i,j)               = runsf
             runsbxy  (i,j)               = runsb
             ecanxy   (i,j)               = ecan
             edirxy   (i,j)               = esoil
             etranxy  (i,j)               = etran
             aparxy   (i,j)               = apar
             psnxy    (i,j)               = psn
             savxy    (i,j)               = sav
             sagxy    (i,j)               = sag
             fsnoxy   (i,j)               = fsno
             fsaxy    (i,j)               = fsa
             firaxy   (i,j)               = fira
             hfx      (i,j)               = fsh
             lh       (i,j)               = fcev + fgev + fctr
             grdflx   (i,j)               = ssoil
             tradxy   (i,j)               = trad
             tsxy     (i,j)               = ts
             neexy    (i,j)               = nee
             gppxy    (i,j)               = gpp
             nppxy    (i,j)               = npp
             fvegxy   (i,j)               = fveg
!jref:4/21/2011
             t2mvxy   (i,j)               = t2mv
             t2mbxy   (i,j)               = t2mb
             q2mvxy   (i,j)               = q2mv
             q2mbxy   (i,j)               = q2mb
             chstarxy (i,j)               = chstar
             chs      (i,j)               = chstar
             tstarxy  (i,j)               = tstar
!jref:4/21/2011 

             CANWAT(I,J)  = canliqxy (i,j) + canicexy (i,j)
             IF ( SALB > -999 ) THEN
                ALBEDO(I,J)  = salb
             ENDIF
             TSK(I,J)     = tradxy   (i,j)
!KWM          TSK(I,J)     = tstarxy  (i,j)
!niu          POTEVP(I,J)  = ???
!jref         CHS2(I,J)    = chxy     (i,j)

             !IF (Q1.GT.QSFC(I,J)) THEN
             !   CQS2(I,J) = CHS(I,J)
             !END IF
             QSFC(I,J)    = Q1/(1.0-Q1)
!jref: specific humidity to mixing ratio
             q2mvxy(i,j)  = q2mvxy(i,j)/(1.0-q2mvxy(i,j))
!             IF (VEGTYP == ISURBAN) write(*,*) "IN SFCDRV: q2mb=",q2mb,"q2mbxy(i,j)=",q2mbxy(i,j)
             q2mbxy(i,j)  = q2mbxy(i,j)/(1.0-q2mbxy(i,j))

!***  DIAGNOSTICS                   
!jref:start - THESE SHOULD BE LOOKED AT!!!
             SNOTIME(I,J) = SNOTIME1
             SMSTAV(I,J)=SOILW
             SMSTOT(I,J)=SOILM*1000.
!         Convert the water unit into mm
             SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+runsfxy(i,j)*DT*1000.0
             UDRUNOFF(I,J)=UDRUNOFF(I,J)+runsbxy(i,j)*DT*1000.0
!jref        SFCRUNOFF(I,J)=SFCRUNOFF(I,J)+RUNOFF1*DT*1000.0
!jref        UDRUNOFF(I,J)=UDRUNOFF(I,J)+(RUNOFF2+RUNOFF3)*DT*1000.0
!jref:end             
! snow defined when fraction of frozen precip (FFROZP) > 0.5,
             IF(FFROZP.GT.0.5)THEN
                ACSNOW(I,J)=ACSNOW(I,J)+PRCP*DT
             ENDIF
             IF(SNOW(I,J).GT.0.)THEN
!KWM                ACSNOM(I,J)=ACSNOM(I,J)+SNOMLT*1000.
             ENDIF

          ENDIF                                                           ! endif of land-sea test
!jref:start  make sure exchange coeff and TSK include water points         
!          IF((XLAND(I,J)-1.5).GE.0.)THEN                                  ! begining of land/sea if block
!             chstar2xy(i,j) = CHS2(i,j)
!             chstarxy(i,j) = CHS(i,j)    
!             tstarxy(i,j)  = T1 !TSK(i,j) test with T1
!          ENDIF
!jref:end


       ENDDO

    ENDDO                                                                ! of J loop
!------------------------------------------------------
  END SUBROUTINE noahmplsm
!------------------------------------------------------

  SUBROUTINE NOAHMP_INIT ( MMINLU, SNOW , SNOWH , CANWAT , ISLTYP ,           &
       TSLB , SMOIS , SH2O , DZS , FNDSOILW , FNDSNOWH ,                      &
       TSK, isnowxy , tvxy     ,tgxy     ,canicexy ,                          &
       canliqxy ,eahxy    ,tahxy    ,cmxy     ,chxy     ,                     &
       fwetxy   ,sneqvoxy ,alboldxy ,qsnowxy  ,wslakexy ,zwtxy    ,waxy     , &
       wtxy     ,tsnoxy   ,zsnsoxy  ,snicexy  ,snliqxy  ,lfmassxy ,rtmassxy , &
       stmassxy ,woodxy   ,stblcpxy ,fastcpxy ,xsaixy   , &
!jref:start
       t2mvxy   ,t2mbxy   ,chstarxy ,            &
!jref:end       
       num_soil_layers, restart,                 &
       allowed_to_read ,                         &
       ids,ide, jds,jde, kds,kde,                &
       ims,ime, jms,jme, kms,kme,                &
       its,ite, jts,jte, kts,kte                 )

! Initializing Canopy air temperature to 287 K seems dangerous to me [KWM].

    INTEGER, INTENT(IN   )    ::     ids,ide, jds,jde, kds,kde,  &
         &                           ims,ime, jms,jme, kms,kme,  &
         &                           its,ite, jts,jte, kts,kte

    INTEGER, INTENT(IN)       ::     num_soil_layers

    LOGICAL, INTENT(IN)       ::     restart,                    &
         &                           allowed_to_read

    REAL,    DIMENSION( num_soil_layers), INTENT(IN)    ::     DZS  ! Thickness of the soil layers [m]

    REAL,    DIMENSION( ims:ime, num_soil_layers, jms:jme ) ,    &
         &   INTENT(INOUT)    ::     SMOIS,                      &
         &                           SH2O,                       &
         &                           TSLB

    REAL,    DIMENSION( ims:ime, jms:jme ) ,                     &
         &   INTENT(INOUT)    ::     SNOW,                       &
         &                           SNOWH,                      &
         &                           CANWAT

    INTEGER, DIMENSION( ims:ime, jms:jme ),                      &
         &   INTENT(IN)       ::     ISLTYP

    LOGICAL, INTENT(IN)       ::     FNDSOILW,                   &
         &                           FNDSNOWH

    REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN) :: TSK         !skin temperature (k)
    INTEGER, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: isnowxy     !actual no. of snow layers
    REAL, DIMENSION(ims:ime,-2:num_soil_layers,jms:jme), INTENT(INOUT) :: zsnsoxy  !snow layer depth [m]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: tsnoxy   !snow temperature [K]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snicexy  !snow layer ice [mm]
    REAL, DIMENSION(ims:ime,-2:              0,jms:jme), INTENT(INOUT) :: snliqxy  !snow layer liquid water [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tvxy        !vegetation canopy temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tgxy        !ground surface temperature
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canicexy    !canopy-intercepted ice (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: canliqxy    !canopy-intercepted liquid water (mm)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: eahxy       !canopy air vapor pressure (pa)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: tahxy       !canopy air temperature (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: cmxy        !momentum drag coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chxy        !sensible heat exchange coefficient
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fwetxy      !wetted or snowed fraction of the canopy (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: sneqvoxy    !snow mass at last time step(mm h2o)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: alboldxy    !snow albedo at last time step (-)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: qsnowxy     !snowfall on the ground [mm/s]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wslakexy    !lake water storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: zwtxy       !water table depth [m]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: waxy        !water in the "aquifer" [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: wtxy        !groundwater storage [mm]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: lfmassxy    !leaf mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rtmassxy    !mass of fine roots [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stmassxy    !stem mass [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: woodxy      !mass of wood (incl. woody roots) [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: stblcpxy    !stable carbon in deep soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: fastcpxy    !short-lived carbon, shallow soil [g/m2]
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: xsaixy      !stem area index

!jref:start
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mvxy        !2m temperature vegetation part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: t2mbxy        !2m temperature bare ground part (k)
    REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: chstarxy    !effective exchange coefficient
!jref:end


    REAL, DIMENSION(1:num_soil_layers)  :: ZSOIL      ! Depth of the soil layer bottom (m) from 
    !                                                   the surface (negative)

    REAL                      :: BX, SMCMAX, PSISAT, FREE

    REAL, PARAMETER           :: BLIM  = 5.5
    REAL, PARAMETER           :: HLICE = 3.335E5
    REAL, PARAMETER           :: GRAV = 9.81
    REAL, PARAMETER           :: T0 = 273.15

    INTEGER                   :: errflag

    character(len=80) :: err_message
    character(len=4)  :: MMINSL
    character(len=*), intent(in) :: MMINLU
    MMINSL='STAS'

    call read_mp_veg_parameters(trim(MMINLU))

    !
    ! initialize three Noah LSM related tables
    !
    IF ( allowed_to_read ) THEN
       CALL wrf_message( 'INITIALIZE THREE Noah LSM RELATED TABLES' )
       CALL  SOIL_VEG_GEN_PARM( MMINLU, MMINSL )
    ENDIF

    IF( .NOT. restart ) THEN

       itf=min0(ite,ide-1)
       jtf=min0(jte,jde-1)

       errflag = 0
       DO j = jts,jtf
          DO i = its,itf
             IF ( ISLTYP( i,j ) .LT. 1 ) THEN
                errflag = 1
                WRITE(err_message,*)"module_sf_noahlsm.F: lsminit: out of range ISLTYP ",i,j,ISLTYP( i,j )
                CALL wrf_message(err_message)
             ENDIF
          ENDDO
       ENDDO
       IF ( errflag .EQ. 1 ) THEN
          CALL wrf_error_fatal( "module_sf_noahlsm.F: lsminit: out of range value "// &
               "of ISLTYP. Is this field in the input?" )
       ENDIF
#ifdef WRF_CHEM
       !
       ! need this parameter for dust parameterization in wrf/chem
       !
       do I=1,NSLTYPE
          porosity(i)=maxsmc(i)
       enddo
#endif

! initialize soil liquid water content SH2O

!  IF(.NOT.FNDSOILW) THEN

! If no SWC, do the following
!         PRINT *,'SOIL WATER NOT FOUND - VALUE SET IN LSMINIT'
       DO J = jts , jtf
          DO I = its , itf
             BX = BB(ISLTYP(I,J))
             SMCMAX = MAXSMC(ISLTYP(I,J))
             PSISAT = SATPSI(ISLTYP(I,J))
             IF ( ( bx > 0.0 ) .AND. ( smcmax > 0.0 ) .AND. ( psisat > 0.0 ) ) THEN
                DO NS=1, num_soil_layers

                   IF ( TSLB(I,NS,J) < 273.149 ) THEN
                      ! SH2O <= SMOIS for T < 273.149K (-0.001C)

                      ! First guess of SH2O following explicit solution for 
                      ! Flerchinger Eqn from Koren et al, JGR, 1999, Eqn 17 
                      ! (KCOUNT=0 in function FRH2O).
                      BX = BB(ISLTYP(I,J))
                      SMCMAX = MAXSMC(ISLTYP(I,J))
                      PSISAT = SATPSI(ISLTYP(I,J))
                      IF ( BX >  BLIM ) BX = BLIM
                      FK=(( (HLICE/(GRAV*(-PSISAT))) *                              &
                           ((TSLB(I,NS,J)-T0)/TSLB(I,NS,J)) )**(-1/BX) )*SMCMAX
                      FK = MAX(FK, 0.02)
                      SH2O(I,NS,J) = MIN( FK, SMOIS(I,NS,J) )

                      ! Use iterative solution for liquid soil water content
                      ! using function FRH2O, with the initial guess for SH2O 
                      ! from the above explicit first guess.
                      CALL FRH2O ( FREE , TSLB(I,NS,J) , SMOIS(I,NS,J) , SH2O(I,NS,J) )
                      SH2O(I,NS,J) = FREE
                   ELSE
                      ! SH2O = SMOIS ( for T => 273.149K (-0.001C)
                      SH2O(I,NS,J)=SMOIS(I,NS,J)
                   ENDIF
                END DO
             ELSE
                DO NS=1, num_soil_layers
                   SH2O(I,NS,J)=SMOIS(I,NS,J)
                END DO
             ENDIF
          ENDDO
       ENDDO
!  ENDIF


       !
       ! initialize physical snow height SNOWH
       !
       IF(.NOT.FNDSNOWH)THEN
          ! If no SNOWH do the following
          CALL wrf_message( 'SNOW HEIGHT NOT FOUND - VALUE DEFINED IN LSMINIT' )
          DO J = jts,jtf
             DO I = its,itf
                SNOWH(I,J)=SNOW(I,J)*0.005               ! SNOW in mm and SNOWH in m
             ENDDO
          ENDDO
       ENDIF

       DO J = jts,jtf
          DO I = its,itf
             tvxy       (I,J) = TSK(I,J)
             tgxy       (I,J) = TSK(I,J)
             CANWAT     (I,J) = 0.0
             canliqxy   (I,J) = CANWAT(I,J)
             canicexy   (I,J) = 0.
             eahxy      (I,J) = 2000. 
             tahxy      (I,J) = 287.
!jref:start
             t2mvxy     (I,J) = TSK(I,J)
             t2mbxy     (I,J) = TSK(I,J)
             chstarxy   (I,J) = 0.0     
!jref:end

             cmxy       (I,J) = 0.0
             chxy       (I,J) = 0.0
             fwetxy     (I,J) = 0.0
             sneqvoxy   (I,J) = 0.0
             alboldxy   (I,J) = 0.65
             qsnowxy    (I,J) = 0.0
             wslakexy   (I,J) = 0.0

             waxy       (I,J) = 4900.                                       !???
             wtxy       (I,J) = waxy(i,j)                                   !???
             zwtxy      (I,J) = (25. + 2.0) - waxy(i,j)/1000/0.2            !???

             lfmassxy   (I,J) = 50.         !
             stmassxy   (I,J) = 50.0        !
             rtmassxy   (I,J) = 500.0       !
             woodxy     (I,J) = 500.0       !
             stblcpxy   (I,J) = 1000.0      !
             fastcpxy   (I,J) = 1000.0      !
             xsaixy     (I,J) = 0.1         !

          enddo
       enddo

       ! Given the soil layer thicknesses (in DZS), initialize the soil layer
       ! depths from the surface.
       ZSOIL(1)         = -DZS(1)          ! negative
       DO NS=2, num_soil_layers
          ZSOIL(NS)       = ZSOIL(NS-1) - DZS(NS)
       END DO

       ! Initialize snow/soil layer arrays ZSNSOXY, TSNOXY, SNICEXY, SNLIQXY, 
       ! and ISNOWXY
       CALL snow_init ( ims , ime , jms , jme , its , itf , jts , jtf , 3 , &
            &           num_soil_layers , zsoil , snow , tgxy , snowh ,     &
            &           zsnsoxy , tsnoxy , snicexy , snliqxy , isnowxy )

    ENDIF
  END SUBROUTINE NOAHMP_INIT

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

  SUBROUTINE SNOW_INIT ( ims , ime , jms , jme , its , itf , jts , jtf ,                  &
       &                 NSNOW , NSOIL , ZSOIL , SWE , TGXY , SNODEP ,                    &
       &                 ZSNSOXY , TSNOXY , SNICEXY ,SNLIQXY , ISNOWXY )
!------------------------------------------------------------------------------------------
!   Initialize snow arrays for Noah-MP LSM, based in input SNOWDEP, NSNOW
!   ISNOWXY is an index array, indicating the index of the top snow layer.  Valid indices
!           for snow layers range from 0 (no snow) and -1 (shallow snow) to (-NSNOW)+1 (deep snow).
!   TSNOXY holds the temperature of the snow layer.  Snow layers are initialized with 
!          temperature = ground temperature [?].  Snow-free levels in the array have value 0.0
!   SNICEXY is the frozen content of a snow layer.  Initial estimate based on SNODEP and SWE
!   SNLIQXY is the liquid content of a snow layer.  Initialized to 0.0
!   ZNSNOXY is the layer depth from the surface.  
!------------------------------------------------------------------------------------------
    IMPLICIT NONE
!------------------------------------------------------------------------------------------
    INTEGER, INTENT(IN)                              :: ims, ime, jms, jme
    INTEGER, INTENT(IN)                              :: its, itf, jts, jtf
    INTEGER, INTENT(IN)                              :: NSNOW
    INTEGER, INTENT(IN)                              :: NSOIL
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SWE 
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: SNODEP
    REAL,    INTENT(IN), DIMENSION(ims:ime, jms:jme) :: TGXY
    REAL,    INTENT(IN), DIMENSION(1:NSOIL)          :: ZSOIL

    INTEGER, INTENT(OUT), DIMENSION(ims:ime, jms:jme)                :: ISNOWXY ! Top snow layer index
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:NSOIL,jms:jme) :: ZSNSOXY ! Snow/soil layer depth from surface [m]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: TSNOXY  ! Snow layer temperature [K]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNICEXY ! Snow layer ice content [mm]
    REAL,    INTENT(OUT), DIMENSION(ims:ime, -NSNOW+1:    0,jms:jme) :: SNLIQXY ! snow layer liquid content [mm]

! Local variables:
!   DZSNO   holds the thicknesses of the various snow layers.
!   DZSNOSO holds the thicknesses of the various soil/snow layers.
    INTEGER                           :: I,J,IZ
    REAL,   DIMENSION(-NSNOW+1:    0) :: DZSNO
    REAL,   DIMENSION(-NSNOW+1:NSOIL) :: DZSNSO

!------------------------------------------------------------------------------------------

    DO J = jts , jtf
       DO I = its , itf
          IF ( SNODEP(I,J) < 0.025 ) THEN
             ISNOWXY(I,J) = 0
             DZSNO(-NSNOW+1:0) = 0.
          ELSE
             IF ( ( SNODEP(I,J) >= 0.025 ) .AND. ( SNODEP(I,J) <= 0.05 ) ) THEN
                ISNOWXY(I,J)    = -1
                DZSNO(0)  = SNODEP(I,J)
             ELSE IF ( ( SNODEP(I,J) > 0.05 ) .AND. ( SNODEP(I,J) <= 0.10 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = SNODEP(I,J)/2.
                DZSNO( 0) = SNODEP(I,J)/2.
             ELSE IF ( (SNODEP(I,J) > 0.10 ) .AND. ( SNODEP(I,J) <= 0.25 ) ) THEN
                ISNOWXY(I,J)    = -2
                DZSNO(-1) = 0.05
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1)
             ELSE IF ( ( SNODEP(I,J) > 0.25 ) .AND. ( SNODEP(I,J) <= 0.35 ) ) THEN
                ISNOWXY(I,J)    = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.5*(SNODEP(I,J)-DZSNO(-2))
                DZSNO( 0) = 0.5*(SNODEP(I,J)-DZSNO(-2))
             ELSE IF ( SNODEP(I,J) > 0.35 ) THEN
                ISNOWXY(I,J)     = -3
                DZSNO(-2) = 0.05
                DZSNO(-1) = 0.10
                DZSNO( 0) = SNODEP(I,J) - DZSNO(-1) - DZSNO(-2)
             ELSE
                CALL wrf_error_fatal("Problem with the logic assigning snow layers.")
             END IF
          END IF

          TSNOXY (I,-NSNOW+1:0,J) = 0.
          SNICEXY(I,-NSNOW+1:0,J) = 0.
          SNLIQXY(I,-NSNOW+1:0,J) = 0.
          DO IZ = ISNOWXY(I,J)+1 , 0
             TSNOXY(I,IZ,J)  = TGXY(I,J)  ! [k]
             SNLIQXY(I,IZ,J) = 0.00
             SNICEXY(I,IZ,J) = 1.00 * DZSNO(IZ) * (SWE(I,J)/SNODEP(I,J))  ! [kg/m3]
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for snow layers
          DO IZ = ISNOWXY(I,J)+1 , 0
             DZSNSO(IZ) = -DZSNO(IZ)
          END DO

          ! Assign local variable DZSNSO, the soil/snow layer thicknesses, for soil layers
          DZSNSO(1) = ZSOIL(1)
          DO IZ = 2 , NSOIL
             DZSNSO(IZ) = (ZSOIL(IZ) - ZSOIL(IZ-1))
          END DO

          ! Assign ZSNSOXY, the layer depths, for soil and snow layers
          ZSNSOXY(I,ISNOWXY(I,J)+1,J) = DZSNSO(ISNOWXY(I,J)+1)
          DO IZ = ISNOWXY(I,J)+2 , NSOIL
             ZSNSOXY(I,IZ,J) = ZSNSOXY(I,IZ-1,J) + DZSNSO(IZ)
          ENDDO

       END DO
    END DO

  END SUBROUTINE SNOW_INIT
!
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!
END MODULE module_sf_noahmpdrv
