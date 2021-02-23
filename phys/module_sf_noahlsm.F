MODULE module_sf_noahlsm
#if defined(mpas)
use mpas_atmphys_constants,only: CP=>cp,R_D=>R_d,XLF=>xlf,XLV=>xlv,RHOWATER=>rho_w,STBOLT=>stbolt,KARMAN=>karman
use mpas_atmphys_utilities, only: physics_error_fatal
#define FATAL_ERROR(M) call physics_error_fatal( M )
#else
USE module_model_constants, only : CP, R_D, XLF, XLV, RHOWATER, STBOLT, KARMAN
use module_wrf_error
#define FATAL_ERROR(M) call wrf_error_fatal( M )
#endif

!ckay=KIRAN ALAPATY @ US EPA -- November 01, 2015
!
! Tim Glotfelty@CNSU; AJ Deng@PSU
!modified for use with FASDAS 
!Flux Adjusting Surface Data Assimilation System to assimilate 
!surface layer and soil layers temperature and moisture using
! surfance reanalsys 
!Reference: Alapaty et al., 2008: Development of the flux-adjusting surface
! data assimilation system for mesoscale models. JAMC, 47, 2331-2350
!

!   REAL, PARAMETER    :: CP = 1004.5
  REAL, PARAMETER      :: RD = 287.04, SIGMA = 5.67E-8,                 &
                          CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
                          LSUBF = 3.335E+5,                             &
                          EMISSI_S = 0.95

! VEGETATION PARAMETERS
        INTEGER :: LUCATS , BARE
        INTEGER :: NATURAL
        INTEGER :: LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL
        integer, PARAMETER :: NLUS=50
        CHARACTER(LEN=256) LUTYPE
        INTEGER, DIMENSION(1:NLUS) :: NROTBL
        real, dimension(1:NLUS) ::  SNUPTBL, RSTBL, RGLTBL, HSTBL,                &
                                    SHDTBL, MAXALB,                               &
                                    EMISSMINTBL, EMISSMAXTBL,                     &
                                    LAIMINTBL, LAIMAXTBL,                         &
                                    Z0MINTBL, Z0MAXTBL,                           &
                                    ALBEDOMINTBL, ALBEDOMAXTBL,                   &
                                    ZTOPVTBL,ZBOTVTBL
        REAL ::   TOPT_DATA,CMCMAX_DATA,CFACTR_DATA,RSMAX_DATA

! SOIL PARAMETERS
        INTEGER :: SLCATS
        INTEGER, PARAMETER :: NSLTYPE=30
        CHARACTER(LEN=256) SLTYPE
        REAL, DIMENSION (1:NSLTYPE) :: BB,DRYSMC,F11,                           &
        MAXSMC, REFSMC,SATPSI,SATDK,SATDW, WLTSMC,QTZ

! LSM GENERAL PARAMETERS
        INTEGER :: SLPCATS
        INTEGER, PARAMETER :: NSLOPE=30
        REAL, DIMENSION (1:NSLOPE) :: SLOPE_DATA
        REAL ::  SBETA_DATA,FXEXP_DATA,CSOIL_DATA,SALP_DATA,REFDK_DATA,           &
                 REFKDT_DATA,FRZK_DATA,ZBOT_DATA,  SMLOW_DATA,SMHIGH_DATA,        &
                        CZIL_DATA
        REAL ::  LVCOEF_DATA

        CHARACTER*256  :: err_message

        integer, private :: iloc, jloc
!$omp threadprivate(iloc, jloc)
!
CONTAINS
!

      SUBROUTINE SFLX (IILOC,JJLOC,FFROZP,ISURBAN,DT,ZLVL,NSOIL,SLDPTH, &    !C
                       LOCAL,                                           &    !L
                       LLANDUSE, LSOIL,                                 &    !CL
                       LWDN,SOLDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,  &    !F
                       COSZ,PRCPRAIN, SOLARDIRECT,                      &    !F
                       TH2,Q2SAT,DQSDT2,                                &    !I
                       VEGTYP,SOILTYP,SLOPETYP,SHDFAC,SHDMIN,SHDMAX,    &    !I
                       ALB, SNOALB,TBOT, Z0BRD, Z0, EMISSI, EMBRD,      &    !S
                       CMC,T1,STC,SMC,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,    &    !H
! ----------------------------------------------------------------------
! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN
! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA
! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
! ----------------------------------------------------------------------
                       ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O
                       EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O
                       BETA,ETP,SSOIL,                                  &    !O
                       FLX1,FLX2,FLX3,                                  &    !O
		       FLX4,FVB,FBUR,FGSN,UA_PHYS,                      &    !UA 
                       SNOMLT,SNCOVR,                                   &    !O
                       RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O
                       RC,PC,RSMIN,XLAI,RCS,RCT,RCQ,RCSOIL,             &    !O
                       SOILW,SOILM,Q1,SMAV,                             &    !D
                       RDLAI2D,USEMONALB,                               &
                       SNOTIME1,                                        &
                       RIBB,                                            &
                       SMCWLT,SMCDRY,SMCREF,SMCMAX,NROOT,   &
                       SFHEAD1RT,                                       &    !I
                       INFXS1RT,ETPND1,OPT_THCND,AOASIS                 &    !P
                      ,XSDA_QFX,HFX_PHY,QFX_PHY,XQNORM                  &    !fasdas
                      ,fasdas,HCPCT_FASDAS,IRRIGATION_CHANNEL           )    !fasdas

! ----------------------------------------------------------------------
! SUBROUTINE SFLX - UNIFIED NOAHLSM VERSION 1.0 JULY 2007
! ----------------------------------------------------------------------
! SUB-DRIVER FOR "Noah LSM" FAMILY OF PHYSICS SUBROUTINES FOR A
! SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE SOIL MOISTURE, SOIL
! ICE, SOIL TEMPERATURE, SKIN TEMPERATURE, SNOWPACK WATER CONTENT,
! SNOWDEPTH, AND ALL TERMS OF THE SURFACE ENERGY BALANCE AND SURFACE
! WATER BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF DOWNWARD
! RADIATION AND PRECIP)
! ----------------------------------------------------------------------
! SFLX ARGUMENT LIST KEY:
! ----------------------------------------------------------------------
!  C  CONFIGURATION INFORMATION
!  L  LOGICAL
! CL  4-string character bearing logical meaning
!  F  FORCING DATA
!  I  OTHER (INPUT) FORCING DATA
!  S  SURFACE CHARACTERISTICS
!  H  HISTORY (STATE) VARIABLES
!  O  OUTPUT VARIABLES
!  D  DIAGNOSTIC OUTPUT
!  P  Parameters
!  Msic Miscellaneous terms passed from gridded driver
! ----------------------------------------------------------------------
! 1. CONFIGURATION INFORMATION (C):
! ----------------------------------------------------------------------
!   DT         TIMESTEP (SEC) (DT SHOULD NOT EXCEED 3600 SECS, RECOMMEND
!                1800 SECS OR LESS)
!   ZLVL       HEIGHT (M) ABOVE GROUND OF ATMOSPHERIC FORCING VARIABLES
!   NSOIL      NUMBER OF SOIL LAYERS (AT LEAST 2, AND NOT GREATER THAN
!                PARAMETER NSOLD SET BELOW)
!   SLDPTH     THE THICKNESS OF EACH SOIL LAYER (M)
! ----------------------------------------------------------------------
! 2. LOGICAL:
! ----------------------------------------------------------------------
!   LCH       Exchange coefficient (Ch) calculation flag (false: using
!                ch-routine SFCDIF; true: Ch is brought in)
!   LOCAL      Flag for local-site simulation (where there is no
!              maps for albedo, veg fraction, and roughness
!             true:  all LSM parameters (inluding albedo, veg fraction and
!                    roughness length) will be defined by three tables
!   LLANDUSE  (=USGS, using USGS landuse classification)
!   LSOIL     (=STAS, using FAO/STATSGO soil texture classification)
!   OPT_THCND option for how to treat thermal conductivity
! ----------------------------------------------------------------------
! 3. FORCING DATA (F):
! ----------------------------------------------------------------------
!   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
!   SOLDN      SOLAR DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET SOLAR)
!   SOLNET     NET DOWNWARD SOLAR RADIATION ((W M-2; POSITIVE)
!   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
!   PRCP       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)
!   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
!   COSZ       Solar zenith angle (not used for now)
!   PRCPRAIN   Liquid-precipitation rate (KG M-2 S-1) (not used)
! SOLARDIRECT  Direct component of downward solar radiation (W M-2) (not used)
!   FFROZP     FRACTION OF FROZEN PRECIPITATION
! ----------------------------------------------------------------------
! 4. OTHER FORCING (INPUT) DATA (I):
! ----------------------------------------------------------------------
!   SFCSPD     WIND SPEED (M S-1) AT HEIGHT ZLVL ABOVE GROUND
!   Q2SAT      SAT SPECIFIC HUMIDITY AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
!   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP
!                (KG KG-1 K-1)
! ----------------------------------------------------------------------
! 5. CANOPY/SOIL CHARACTERISTICS (S):
! ----------------------------------------------------------------------
!   VEGTYP     VEGETATION TYPE (INTEGER INDEX)
!   SOILTYP    SOIL TYPE (INTEGER INDEX)
!   SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
!   SHDFAC     AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0)
!   SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0) <= SHDFAC
!   PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
!                VEG PARMS)
!   ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
!                INCLUDE DIURNAL SUN ANGLE EFFECT)
!   SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
!                TEMPERATURE)
!   Z0BRD      Background fixed roughness length (M)
!   Z0         Time varying roughness length (M) as function of snow depth
!
!   EMBRD      Background surface emissivity (between 0 and 1)
!   EMISSI     Surface emissivity (between 0 and 1)
! ----------------------------------------------------------------------
! 6. HISTORY (STATE) VARIABLES (H):
! ----------------------------------------------------------------------
!  CMC         CANOPY MOISTURE CONTENT (M)
!  T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
!  STC(NSOIL)  SOIL TEMP (K)
!  SMC(NSOIL)  TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
!  SH2O(NSOIL) UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION)
!                NOTE: FROZEN SOIL MOISTURE = SMC - SH2O
!  SNOWH       ACTUAL SNOW DEPTH (M)
!  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
!                NOTE: SNOW DENSITY = SNEQV/SNOWH
!  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
!                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR
!                =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) WHEN SNEQV>0
!  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE
!                IT HAS BEEN MULTIPLIED BY WIND SPEED.
!  CM          SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM (M S-1); NOTE:
!                CM IS TECHNICALLY A CONDUCTANCE SINCE IT HAS BEEN
!                MULTIPLIED BY WIND SPEED.
! ----------------------------------------------------------------------
! 7. OUTPUT (O):
! ----------------------------------------------------------------------
! OUTPUT VARIABLES NECESSARY FOR A COUPLED NUMERICAL WEATHER PREDICTION
! MODEL, E.G. NOAA/NWS/NCEP MESOSCALE ETA MODEL.  FOR THIS APPLICATION,
! THE REMAINING OUTPUT/DIAGNOSTIC/PARAMETER BLOCKS BELOW ARE NOT
! NECESSARY.  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
!   ETA        ACTUAL LATENT HEAT FLUX (W m-2: NEGATIVE, IF UP FROM
!              SURFACE)
!  ETA_KINEMATIC atctual latent heat flux in Kg m-2 s-1
!   SHEAT      SENSIBLE HEAT FLUX (W M-2: POSITIVE, IF UPWARD FROM
!              SURFACE)
!   FDOWN      Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
! ----------------------------------------------------------------------
!   EC         CANOPY WATER EVAPORATION (W m-2)
!   EDIR       DIRECT SOIL EVAPORATION (W m-2)
!   ET(NSOIL)  PLANT TRANSPIRATION FROM A PARTICULAR ROOT (SOIL) LAYER
!                 (W m-2)
!   ETT        TOTAL PLANT TRANSPIRATION (W m-2)
!   ESNOW      SUBLIMATION FROM (OR DEPOSITION TO IF <0) SNOWPACK
!                (W m-2)
!   DRIP       THROUGH-FALL OF PRECIP AND/OR DEW IN EXCESS OF CANOPY
!                WATER-HOLDING CAPACITY (M)
!   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
! ----------------------------------------------------------------------
!   BETA       RATIO OF ACTUAL/POTENTIAL EVAP (DIMENSIONLESS)
!   ETP        POTENTIAL EVAPORATION (W m-2)
!   SSOIL      SOIL HEAT FLUX (W M-2: NEGATIVE IF DOWNWARD FROM SURFACE)
! ----------------------------------------------------------------------
!   FLX1       PRECIP-SNOW SFC (W M-2)
!   FLX2       FREEZING RAIN LATENT HEAT FLUX (W M-2)
!   FLX3       PHASE-CHANGE HEAT FLUX FROM SNOWMELT (W M-2)
! ----------------------------------------------------------------------
!   SNOMLT     SNOW MELT (M) (WATER EQUIVALENT)
!   SNCOVR     FRACTIONAL SNOW COVER (UNITLESS FRACTION, 0-1)
! ----------------------------------------------------------------------
!   RUNOFF1    SURFACE RUNOFF (M S-1), NOT INFILTRATING THE SURFACE
!   RUNOFF2    SUBSURFACE RUNOFF (M S-1), DRAINAGE OUT BOTTOM OF LAST
!                SOIL LAYER (BASEFLOW)
!   RUNOFF3    NUMERICAL TRUNCTATION IN EXCESS OF POROSITY (SMCMAX)
!                FOR A GIVEN SOIL LAYER AT THE END OF A TIME STEP (M S-1).
! Note: the above RUNOFF2 is actually the sum of RUNOFF2 and RUNOFF3
! ----------------------------------------------------------------------
!   RC         CANOPY RESISTANCE (S M-1)
!   PC         PLANT COEFFICIENT (UNITLESS FRACTION, 0-1) WHERE PC*ETP
!                = ACTUAL TRANSP
!   XLAI       LEAF AREA INDEX (DIMENSIONLESS)
!   RSMIN      MINIMUM CANOPY RESISTANCE (S M-1)
!   RCS        INCOMING SOLAR RC FACTOR (DIMENSIONLESS)
!   RCT        AIR TEMPERATURE RC FACTOR (DIMENSIONLESS)
!   RCQ        ATMOS VAPOR PRESSURE DEFICIT RC FACTOR (DIMENSIONLESS)
!   RCSOIL     SOIL MOISTURE RC FACTOR (DIMENSIONLESS)
! ----------------------------------------------------------------------
! 8. DIAGNOSTIC OUTPUT (D):
! ----------------------------------------------------------------------
!   SOILW      AVAILABLE SOIL MOISTURE IN ROOT ZONE (UNITLESS FRACTION
!              BETWEEN SMCWLT AND SMCMAX)
!   SOILM      TOTAL SOIL COLUMN MOISTURE CONTENT (FROZEN+UNFROZEN) (M)
!   Q1         Effective mixing ratio at surface (kg kg-1), used for
!              diagnosing the mixing ratio at 2 meter for coupled model
!   SMAV       Soil Moisture Availability for each layer, as a fraction
!              between SMCWLT and SMCMAX.
!  Documentation for SNOTIME1 and SNOABL2 ?????
!  What categories of arguments do these variables fall into ????
!  Documentation for RIBB ?????
!  What category of argument does RIBB fall into ?????
! ----------------------------------------------------------------------
! 9. PARAMETERS (P):
! ----------------------------------------------------------------------
!   SMCWLT     WILTING POINT (VOLUMETRIC)
!   SMCDRY     DRY SOIL MOISTURE THRESHOLD WHERE DIRECT EVAP FRM TOP
!                LAYER ENDS (VOLUMETRIC)
!   SMCREF     SOIL MOISTURE THRESHOLD WHERE TRANSPIRATION BEGINS TO
!                STRESS (VOLUMETRIC)
!   SMCMAX     POROSITY, I.E. SATURATED VALUE OF SOIL MOISTURE
!                (VOLUMETRIC)
!   NROOT      NUMBER OF ROOT LAYERS, A FUNCTION OF VEG TYPE, DETERMINED
!              IN SUBROUTINE REDPRM.
! ----------------------------------------------------------------------


      IMPLICIT NONE
! ----------------------------------------------------------------------

! DECLARATIONS - LOGICAL AND CHARACTERS
! ----------------------------------------------------------------------

      INTEGER, INTENT(IN) :: IILOC, JJLOC
      LOGICAL, INTENT(IN)::  LOCAL
      LOGICAL            ::  FRZGRA, SNOWNG
      CHARACTER (LEN=256), INTENT(IN)::  LLANDUSE, LSOIL

! ----------------------------------------------------------------------
! 1. CONFIGURATION INFORMATION (C):
! ----------------------------------------------------------------------
      INTEGER,INTENT(IN) ::  NSOIL,SLOPETYP,SOILTYP,VEGTYP
      INTEGER, INTENT(IN) :: ISURBAN
      INTEGER,INTENT(OUT)::  NROOT
      INTEGER  KZ, K, iout

! ----------------------------------------------------------------------
! 2. LOGICAL:
! ----------------------------------------------------------------------
      LOGICAL, INTENT(IN) :: RDLAI2D
      LOGICAL, INTENT(IN) :: USEMONALB
      INTEGER, INTENT(IN) :: OPT_THCND

      REAL, INTENT(INOUT):: SFHEAD1RT,INFXS1RT, ETPND1

      REAL, INTENT(IN)   :: SHDMIN,SHDMAX,DT,DQSDT2,LWDN,PRCP,PRCPRAIN,     &
                            Q2,Q2SAT,SFCPRS,SFCSPD,SFCTMP, SNOALB,          &
                            SOLDN,SOLNET,TBOT,TH2,ZLVL,                            &
                            FFROZP,AOASIS
      REAL, INTENT(OUT)  :: EMBRD
      REAL, INTENT(OUT)  :: ALBEDO
      REAL, INTENT(INOUT):: COSZ, SOLARDIRECT,CH,CM,                        &
                            CMC,SNEQV,SNCOVR,SNOWH,T1,XLAI,SHDFAC,Z0BRD,    &
                            EMISSI, ALB
      REAL, INTENT(INOUT):: SNOTIME1
      REAL, INTENT(INOUT):: RIBB
      REAL, DIMENSION(1:NSOIL), INTENT(IN) :: SLDPTH
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: ET
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: SMAV
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) ::  SH2O, SMC, STC
      REAL,DIMENSION(1:NSOIL)::   RTDIS, ZSOIL

      REAL,INTENT(OUT)   :: ETA_KINEMATIC,BETA,DEW,DRIP,EC,EDIR,ESNOW,ETA,  &
                            ETP,FLX1,FLX2,FLX3,SHEAT,PC,RUNOFF1,RUNOFF2,    &
                            RUNOFF3,RC,RSMIN,RCQ,RCS,RCSOIL,RCT,SSOIL,      &
                            SMCDRY,SMCMAX,SMCREF,SMCWLT,SNOMLT, SOILM,      &
                            SOILW,FDOWN,Q1
      LOGICAL, INTENT(IN) :: UA_PHYS   ! UA: flag for UA option
      REAL,INTENT(OUT)    :: FLX4      ! UA: energy added to sensible heat
      REAL,INTENT(OUT)    :: FVB       ! UA: frac. veg. w/snow beneath
      REAL,INTENT(OUT)    :: FBUR      ! UA: fraction of canopy buried
      REAL,INTENT(OUT)    :: FGSN      ! UA: ground snow cover fraction
      REAL                :: ZTOPV     ! UA: height of canopy top
      REAL                :: ZBOTV     ! UA: height of canopy bottom
      REAL                :: GAMA      ! UA: = EXP(-1.* XLAI)
      REAL                :: FNET      ! UA:
      REAL                :: ETPN      ! UA:
      REAL                :: RU        ! UA:

      REAL :: BEXP,CFACTR,CMCMAX,CSOIL,CZIL,DF1,DF1H,DF1A,DKSAT,DWSAT,      &
              DSOIL,DTOT,ETT,FRCSNO,FRCSOI,EPSCA,F1,FXEXP,FRZX,HS,          &
              KDT,LVH2O,PRCP1,PSISAT,QUARTZ,R,RCH,REFKDT,RR,RGL,            &
              RSMAX,                                                        &
              RSNOW,SNDENS,SNCOND,SBETA,SN_NEW,SLOPE,SNUP,SALP,SOILWM,      &
              SOILWW,T1V,T24,T2V,TH2V,TOPT,TFREEZ,TSNOW,ZBOT,Z0,PRCPF,      &
              ETNS,PTU,LSUBS
        REAL ::  LVCOEF
      REAL :: INTERP_FRACTION
      REAL :: LAIMIN,    LAIMAX
      REAL :: ALBEDOMIN, ALBEDOMAX
      REAL :: EMISSMIN,  EMISSMAX
      REAL :: Z0MIN,     Z0MAX

! ----------------------------------------------------------------------
! DECLARATIONS - PARAMETERS
! ----------------------------------------------------------------------
      PARAMETER (TFREEZ = 273.15)
      PARAMETER (LVH2O = 2.501E+6)
      PARAMETER (LSUBS = 2.83E+6)
      PARAMETER (R = 287.04)
!
!  FASDAS
!
   INTEGER, INTENT(IN   )  ::  fasdas
   REAL,    INTENT(INOUT)  ::  XSDA_QFX, XQNORM
   REAL,    INTENT(INOUT)  ::  HFX_PHY, QFX_PHY
   REAL,    INTENT(  OUT)  ::  HCPCT_FASDAS
!
!  END FASDAS
!
! IRRIGATION
  REAL, OPTIONAL,   INTENT(INOUT)  :: IRRIGATION_CHANNEL

! ----------------------------------------------------------------------
!   INITIALIZATION
! ----------------------------------------------------------------------
      ILOC = IILOC
      JLOC = JJLOC

      RUNOFF1 = 0.0
      RUNOFF2 = 0.0
      RUNOFF3 = 0.0
      SNOMLT = 0.0

      IF ( .NOT. UA_PHYS ) THEN
          FLX4 = 0.0
          FVB  = 0.0
          FBUR = 0.0
          FGSN = 0.0
      ENDIF

! ----------------------------------------------------------------------
! CALCULATE DEPTH (NEGATIVE) BELOW GROUND FROM TOP SKIN SFC TO BOTTOM OF
!   EACH SOIL LAYER.  NOTE:  SIGN OF ZSOIL IS NEGATIVE (DENOTING BELOW
!   GROUND)
! ----------------------------------------------------------------------
      ZSOIL (1) = - SLDPTH (1)
      DO KZ = 2,NSOIL
         ZSOIL (KZ) = - SLDPTH (KZ) + ZSOIL (KZ -1)
      END DO

! ----------------------------------------------------------------------
! NEXT IS CRUCIAL CALL TO SET THE LAND-SURFACE PARAMETERS, INCLUDING
! SOIL-TYPE AND VEG-TYPE DEPENDENT PARAMETERS.
! ----------------------------------------------------------------------
         CALL REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,TOPT,   &
                       REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,    &
                         PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &
                         SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &
                         RTDIS,SLDPTH,ZSOIL,NROOT,NSOIL,CZIL,              &
                         LAIMIN, LAIMAX, EMISSMIN, EMISSMAX, ALBEDOMIN,    &
                         ALBEDOMAX, Z0MIN, Z0MAX, CSOIL, PTU, LLANDUSE,    &
                         LSOIL,LOCAL,LVCOEF,ZTOPV,ZBOTV)

!urban
         IF(VEGTYP==ISURBAN)THEN
              SHDFAC=0.05
              RSMIN=400.0
              SMCMAX = 0.45
              SMCREF = 0.42
              SMCWLT = 0.40
              SMCDRY = 0.40
         ENDIF

         IF ( SHDFAC >= SHDMAX ) THEN
            EMBRD = EMISSMAX
            IF (.NOT. RDLAI2D) THEN
               XLAI  = LAIMAX
            ENDIF
            IF (.NOT. USEMONALB) THEN
               ALB   = ALBEDOMIN
            ENDIF
            Z0BRD = Z0MAX
         ELSE IF ( SHDFAC <= SHDMIN ) THEN
            EMBRD = EMISSMIN
            IF(.NOT. RDLAI2D) THEN
               XLAI  = LAIMIN
            ENDIF
            IF(.NOT. USEMONALB) then
               ALB   = ALBEDOMAX
            ENDIF
            Z0BRD = Z0MIN
         ELSE

            IF ( SHDMAX > SHDMIN ) THEN

               INTERP_FRACTION = ( SHDFAC - SHDMIN ) / ( SHDMAX - SHDMIN )
               ! Bound INTERP_FRACTION between 0 and 1
               INTERP_FRACTION = MIN ( INTERP_FRACTION, 1.0 )
               INTERP_FRACTION = MAX ( INTERP_FRACTION, 0.0 )
               ! Scale Emissivity and LAI between EMISSMIN and EMISSMAX by INTERP_FRACTION
               EMBRD = ( ( 1.0 - INTERP_FRACTION ) * EMISSMIN  ) + ( INTERP_FRACTION * EMISSMAX  )
               IF (.NOT. RDLAI2D) THEN
                  XLAI  = ( ( 1.0 - INTERP_FRACTION ) * LAIMIN    ) + ( INTERP_FRACTION * LAIMAX    )
               ENDIF
               if (.not. USEMONALB) then
                  ALB   = ( ( 1.0 - INTERP_FRACTION ) * ALBEDOMAX ) + ( INTERP_FRACTION * ALBEDOMIN )
               endif
               Z0BRD = ( ( 1.0 - INTERP_FRACTION ) * Z0MIN     ) + ( INTERP_FRACTION * Z0MAX     )

            ELSE

               EMBRD = 0.5 * EMISSMIN  + 0.5 * EMISSMAX
               IF (.NOT. RDLAI2D) THEN
                  XLAI  = 0.5 * LAIMIN    + 0.5 * LAIMAX
               ENDIF
               if (.not. USEMONALB) then
                  ALB   = 0.5 * ALBEDOMIN + 0.5 * ALBEDOMAX
               endif
               Z0BRD = 0.5 * Z0MIN     + 0.5 * Z0MAX

            ENDIF

         ENDIF
! ----------------------------------------------------------------------
!  INITIALIZE PRECIPITATION LOGICALS.
! ----------------------------------------------------------------------
         SNOWNG = .FALSE.
         FRZGRA = .FALSE.

! ----------------------------------------------------------------------
! IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND
!   SNOW THERMAL CONDUCTIVITY "SNCOND" (NOTE THAT CSNOW IS A FUNCTION
!   SUBROUTINE)
! ----------------------------------------------------------------------
         IF ( SNEQV <= 1.E-7 ) THEN ! safer IF	kmh (2008/03/25)
            SNEQV = 0.0
            SNDENS = 0.0
            SNOWH = 0.0
            SNCOND = 1.0
         ELSE
            SNDENS = SNEQV / SNOWH
            IF(SNDENS > 1.0) THEN
             FATAL_ERROR( 'Physical snow depth is less than snow water equiv.' )
            ENDIF
            CALL CSNOW (SNCOND,SNDENS)
         END IF
! ----------------------------------------------------------------------
! DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
! IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
! IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
! TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
! ----------------------------------------------------------------------
         IF (PRCP > 0.0) THEN
! snow defined when fraction of frozen precip (FFROZP) > 0.5,
! passed in from model microphysics.
            IF (FFROZP .GT. 0.5) THEN
               SNOWNG = .TRUE.
            ELSE
               IF (T1 <= TFREEZ) FRZGRA = .TRUE.
            END IF
         END IF
! ----------------------------------------------------------------------
! IF EITHER PRCP FLAG IS SET, DETERMINE NEW SNOWFALL (CONVERTING PRCP
! RATE FROM KG M-2 S-1 TO A LIQUID EQUIV SNOW DEPTH IN METERS) AND ADD
! IT TO THE EXISTING SNOWPACK.
! NOTE THAT SINCE ALL PRECIP IS ADDED TO SNOWPACK, NO PRECIP INFILTRATES
! INTO THE SOIL SO THAT PRCP1 IS SET TO ZERO.
! ----------------------------------------------------------------------
         IF ( (SNOWNG) .OR. (FRZGRA) ) THEN
            SN_NEW = PRCP * DT * 0.001
            SNEQV = SNEQV + SN_NEW
            PRCPF = 0.0

! ----------------------------------------------------------------------
! UPDATE SNOW DENSITY BASED ON NEW SNOWFALL, USING OLD AND NEW SNOW.
! UPDATE SNOW THERMAL CONDUCTIVITY
! ----------------------------------------------------------------------
            CALL SNOW_NEW (SFCTMP,SN_NEW,SNOWH,SNDENS)
            CALL CSNOW (SNCOND,SNDENS)

! ----------------------------------------------------------------------
! PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
! LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL (ALONG WITH
! ANY CANOPY "DRIP" ADDED TO THIS LATER)
! ----------------------------------------------------------------------
         ELSE
            PRCPF = PRCP
         ENDIF
! ----------------------------------------------------------------------
! DETERMINE SNOWCOVER AND ALBEDO OVER LAND.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! IF SNOW DEPTH=0, SET SNOW FRACTION=0, ALBEDO=SNOW FREE ALBEDO.
! ----------------------------------------------------------------------
         IF (SNEQV  == 0.0) THEN
            SNCOVR = 0.0
            ALBEDO = ALB
            EMISSI = EMBRD
	    IF(UA_PHYS) FGSN = 0.0
	    IF(UA_PHYS) FVB = 0.0
	    IF(UA_PHYS) FBUR = 0.0
         ELSE
! ----------------------------------------------------------------------
! DETERMINE SNOW FRACTIONAL COVERAGE.
! DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.
! ----------------------------------------------------------------------
            CALL SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR, &
                         XLAI,SHDFAC,FVB,GAMA,FBUR,    &
                         FGSN,ZTOPV,ZBOTV,UA_PHYS)

            IF ( UA_PHYS ) then
              IF(SFCTMP <= T1) THEN
                RU = 0.
              ELSE
                RU = 100.*SHDFAC*FGSN*MIN((SFCTMP-T1)/5., 1.)*(1.-EXP(-XLAI))
              ENDIF
              CH = CH/(1.+RU*CH)
            ENDIF

            SNCOVR = MIN(SNCOVR,0.98) 

            CALL ALCALC (ALB,SNOALB,EMBRD,SHDFAC,SHDMIN,SNCOVR,T1, &
                 ALBEDO,EMISSI,DT,SNOWNG,SNOTIME1,LVCOEF)
         ENDIF
! ----------------------------------------------------------------------
! NEXT CALCULATE THE SUBSURFACE HEAT FLUX, WHICH FIRST REQUIRES
! CALCULATION OF THE THERMAL DIFFUSIVITY.  TREATMENT OF THE
! LATTER FOLLOWS THAT ON PAGES 148-149 FROM "HEAT TRANSFER IN
! COLD CLIMATES", BY V. J. LUNARDINI (PUBLISHED IN 1981
! BY VAN NOSTRAND REINHOLD CO.) I.E. TREATMENT OF TWO CONTIGUOUS
! "PLANE PARALLEL" MEDIUMS (NAMELY HERE THE FIRST SOIL LAYER
! AND THE SNOWPACK LAYER, IF ANY). THIS DIFFUSIVITY TREATMENT
! BEHAVES WELL FOR BOTH ZERO AND NONZERO SNOWPACK, INCLUDING THE
! LIMIT OF VERY THIN SNOWPACK.  THIS TREATMENT ALSO ELIMINATES
! THE NEED TO IMPOSE AN ARBITRARY UPPER BOUND ON SUBSURFACE
! HEAT FLUX WHEN THE SNOWPACK BECOMES EXTREMELY THIN.
! ----------------------------------------------------------------------
! FIRST CALCULATE THERMAL DIFFUSIVITY OF TOP SOIL LAYER, USING
! BOTH THE FROZEN AND LIQUID SOIL MOISTURE, FOLLOWING THE
! SOIL THERMAL DIFFUSIVITY FUNCTION OF PETERS-LIDARD ET AL.
! (1998,JAS, VOL 55, 1209-1224), WHICH REQUIRES THE SPECIFYING
! THE QUARTZ CONTENT OF THE GIVEN SOIL CLASS (SEE ROUTINE REDPRM)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! NEXT ADD SUBSURFACE HEAT FLUX REDUCTION EFFECT FROM THE
! OVERLYING GREEN CANOPY, ADAPTED FROM SECTION 2.1.2 OF
! PETERS-LIDARD ET AL. (1997, JGR, VOL 102(D4))
! ----------------------------------------------------------------------
            CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1),BEXP, PSISAT, SOILTYP, OPT_THCND)

!urban
            IF ( VEGTYP == ISURBAN ) DF1=3.24

            DF1 = DF1 * EXP (SBETA * SHDFAC)
!
! kmh 09/03/2006
! kmh 03/25/2008  change SNCOVR threshold to 0.97
!
            IF (  SNCOVR .GT. 0.97 ) THEN
               DF1 = SNCOND
            ENDIF
!
! ----------------------------------------------------------------------
! FINALLY "PLANE PARALLEL" SNOWPACK EFFECT FOLLOWING
! V.J. LINARDINI REFERENCE CITED ABOVE. NOTE THAT DTOT IS
! COMBINED DEPTH OF SNOWDEPTH AND THICKNESS OF FIRST SOIL LAYER
! ----------------------------------------------------------------------

         DSOIL = - (0.5 * ZSOIL (1))
         IF (SNEQV == 0.) THEN
            SSOIL = DF1 * (T1- STC (1) ) / DSOIL
         ELSE
            DTOT = SNOWH + DSOIL
            FRCSNO = SNOWH / DTOT

! 1. HARMONIC MEAN (SERIES FLOW)
!        DF1 = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
            FRCSOI = DSOIL / DTOT
! 2. ARITHMETIC MEAN (PARALLEL FLOW)
!        DF1 = FRCSNO*SNCOND + FRCSOI*DF1
            DF1H = (SNCOND * DF1)/ (FRCSOI * SNCOND+ FRCSNO * DF1)

! 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)
!        DF1 = (SNCOND**FRCSNO)*(DF1**FRCSOI)
! weigh DF by snow fraction
!        DF1 = DF1H*SNCOVR + DF1A*(1.0-SNCOVR)
!        DF1 = DF1H*SNCOVR + DF1*(1.0-SNCOVR)
            DF1A = FRCSNO * SNCOND+ FRCSOI * DF1

! ----------------------------------------------------------------------
! CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY
! OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP
! MID-LAYER SOIL TEMPERATURE
! ----------------------------------------------------------------------
            DF1 = DF1A * SNCOVR + DF1* (1.0- SNCOVR)
            SSOIL = DF1 * (T1- STC (1) ) / DTOT
         END IF
! ----------------------------------------------------------------------
! DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM
! THE PREVIOUS TIMESTEP.
! ----------------------------------------------------------------------
         IF (SNCOVR  > 0. ) THEN
            CALL SNOWZ0 (SNCOVR,Z0,Z0BRD,SNOWH,FBUR,FGSN,SHDMAX,UA_PHYS)
         ELSE
            Z0=Z0BRD
            IF(UA_PHYS) CALL SNOWZ0 (SNCOVR,Z0,Z0BRD,SNOWH,FBUR,FGSN, &
	                             SHDMAX,UA_PHYS)
         END IF
! ----------------------------------------------------------------------
! NEXT CALL ROUTINE SFCDIF TO CALCULATE THE SFC EXCHANGE COEF (CH) FOR
! HEAT AND MOISTURE.

! NOTE !!!
! DO NOT CALL SFCDIF UNTIL AFTER ABOVE CALL TO REDPRM, IN CASE
! ALTERNATIVE VALUES OF ROUGHNESS LENGTH (Z0) AND ZILINTINKEVICH COEF
! (CZIL) ARE SET THERE VIA NAMELIST I/O.

! NOTE !!!
! ROUTINE SFCDIF RETURNS A CH THAT REPRESENTS THE WIND SPD TIMES THE
! "ORIGINAL" NONDIMENSIONAL "Ch" TYPICAL IN LITERATURE.  HENCE THE CH
! RETURNED FROM SFCDIF HAS UNITS OF M/S.  THE IMPORTANT COMPANION
! COEFFICIENT OF CH, CARRIED HERE AS "RCH", IS THE CH FROM SFCDIF TIMES
! AIR DENSITY AND PARAMETER "CP".  "RCH" IS COMPUTED IN "CALL PENMAN".
! RCH RATHER THAN CH IS THE COEFF USUALLY INVOKED LATER IN EQNS.

! NOTE !!!
! ----------------------------------------------------------------------
! SFCDIF ALSO RETURNS THE SURFACE EXCHANGE COEFFICIENT FOR MOMENTUM, CM,
! ALSO KNOWN AS THE SURFACE DRAGE COEFFICIENT. Needed as a state variable
! for iterative/implicit solution of CH in SFCDIF
! ----------------------------------------------------------------------
!        IF(.NOT.LCH) THEN
!          T1V = T1 * (1.0+ 0.61 * Q2)
!          TH2V = TH2 * (1.0+ 0.61 * Q2)
!          CALL SFCDIF_off (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)
!        ENDIF

! ----------------------------------------------------------------------
! CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND
! OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER
! CALCULATIONS.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN
! PENMAN EP SUBROUTINE THAT FOLLOWS
! ----------------------------------------------------------------------
!         FDOWN = SOLDN * (1.0- ALBEDO) + LWDN
         FDOWN =  SOLNET + LWDN
! ----------------------------------------------------------------------
! CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES
! PENMAN.
         T2V = SFCTMP * (1.0+ 0.61 * Q2 )

         iout=0
         if(iout.eq.1) then
         print*,'before penman'
         print*,' SFCTMP',SFCTMP,'SFCPRS',SFCPRS,'CH',CH,'T2V',T2V,      &
       'TH2',TH2,'PRCP',PRCP,'FDOWN',FDOWN,'T24',T24,'SSOIL',SSOIL,      &
        'Q2',Q2,'Q2SAT',Q2SAT,'ETP',ETP,'RCH',RCH,                       &
        'EPSCA',EPSCA,'RR',RR  ,'SNOWNG',SNOWNG,'FRZGRA',FRZGRA,           &
        'DQSDT2',DQSDT2,'FLX2',FLX2,'SNOWH',SNOWH,'SNEQV',SNEQV,         &
        ' DSOIL',DSOIL,' FRCSNO',FRCSNO,' SNCOVR',SNCOVR,' DTOT',DTOT,   &
       ' ZSOIL (1)',ZSOIL(1),' DF1',DF1,'T1',T1,' STC1',STC(1),          &
        'ALBEDO',ALBEDO,'SMC',SMC,'STC',STC,'SH2O',SH2O
         endif

         CALL PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL,     &
                       Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,          &
                       DQSDT2,FLX2,EMISSI,SNEQV,T1,SNCOVR,AOASIS,        &
		       ALBEDO,SOLDN,FVB,GAMA,STC(1),ETPN,FLX4,UA_PHYS)
!
! ----------------------------------------------------------------------
! CALL CANRES TO CALCULATE THE CANOPY RESISTANCE AND CONVERT IT INTO PC
! IF NONZERO GREENNESS FRACTION
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!  FROZEN GROUND EXTENSION: TOTAL SOIL WATER "SMC" WAS REPLACED
!  BY UNFROZEN SOIL WATER "SH2O" IN CALL TO CANRES BELOW
! ----------------------------------------------------------------------
         IF ( (SHDFAC > 0.) .AND. (XLAI > 0.) ) THEN
            CALL CANRES (SOLDN,CH,SFCTMP,Q2,SFCPRS,SH2O,ZSOIL,NSOIL,     &
                          SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,  &
                          TOPT,RSMAX,RGL,HS,XLAI,                        &
                          RCS,RCT,RCQ,RCSOIL,EMISSI)
         ELSE
            RC = 0.0
         END IF
! ----------------------------------------------------------------------
! NOW DECIDE MAJOR PATHWAY BRANCH TO TAKE DEPENDING ON WHETHER SNOWPACK
! EXISTS OR NOT:
! ----------------------------------------------------------------------
         ESNOW = 0.0
         IF (SNEQV  == 0.0) THEN
            CALL NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                  &
                            SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,           &
                            SHDFAC,                                      &
                            SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,  &
                            SSOIL,                                       &
                            STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,             &
                            SH2O,SLOPE,KDT,FRZX,PSISAT,ZSOIL,            &
                            DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,       &
                            RUNOFF3,EDIR,EC,ET,ETT,NROOT,RTDIS,          &
                            QUARTZ,FXEXP,CSOIL,                          &
                            BETA,DRIP,DEW,FLX1,FLX3,VEGTYP,ISURBAN,      &
                            SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND  &
                           ,XSDA_QFX,QFX_PHY,XQNORM,fasdas,HCPCT_FASDAS,IRRIGATION_CHANNEL  ) !fasdas

            ETA_KINEMATIC = ETA
         ELSE
            CALL SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,    &
                         SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,              &
                         SBETA,DF1,                                      &
                         Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,  &
                         SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,SNEQV,SNDENS,&
                         SNOWH,SH2O,SLOPE,KDT,FRZX,PSISAT,               &
                         ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,     &
                         RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,    &
                         RTDIS,QUARTZ,FXEXP,CSOIL,                       &
                         BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI, &
                         RIBB,SOLDN,                                     &
                         ISURBAN,                                        &
                         VEGTYP,                                         &
                         ETPN,FLX4,UA_PHYS,                              &
                         SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND     &
                        ,QFX_PHY,fasdas,HCPCT_FASDAS                     ) !fasdas
            ETA_KINEMATIC =  ESNOW + ETNS - 1000.0*DEW
         END IF

!     Calculate effective mixing ratio at grnd level (skin)
!
!     Q1=Q2+ETA*CP/RCH
     Q1=Q2+ETA_KINEMATIC*CP/RCH
!
! ----------------------------------------------------------------------
! DETERMINE SENSIBLE HEAT (H) IN ENERGY UNITS (W M-2)
! ----------------------------------------------------------------------

         SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )
         IF(UA_PHYS) SHEAT = SHEAT + FLX4   
!
! FASDAS
!
      IF ( fasdas == 1 ) THEN
        HFX_PHY = SHEAT
      ENDIF
!
! END FASDAS
!
! ----------------------------------------------------------------------
! CONVERT EVAP TERMS FROM KINEMATIC (KG M-2 S-1) TO ENERGY UNITS (W M-2)
! ----------------------------------------------------------------------
      EDIR = EDIR * LVH2O
      EC = EC * LVH2O
      DO K=1,4
      ET(K) = ET(K) * LVH2O
      ENDDO
      ETT = ETT * LVH2O
      
      ETPND1=ETPND1 * LVH2O

      ESNOW = ESNOW * LSUBS
      ETP = ETP*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF(UA_PHYS) ETPN = ETPN*((1.-SNCOVR)*LVH2O + SNCOVR*LSUBS)
      IF (ETP .GT. 0.) THEN
         ETA = EDIR + EC + ETT + ESNOW
      ELSE
        ETA = ETP
      ENDIF
! ----------------------------------------------------------------------
! DETERMINE BETA (RATIO OF ACTUAL TO POTENTIAL EVAP)
! ----------------------------------------------------------------------
      IF (ETP == 0.0) THEN
        BETA = 0.0
      ELSE
        BETA = ETA/ETP
      ENDIF

! ----------------------------------------------------------------------
! CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
!   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)
!   SSOIL<0: COOL THE SURFACE  (DAY TIME)
! ----------------------------------------------------------------------
         SSOIL = -1.0* SSOIL

! ----------------------------------------------------------------------
!  FOR THE CASE OF LAND:
!  CONVERT RUNOFF3 (INTERNAL LAYER RUNOFF FROM SUPERSAT) FROM M TO M S-1
!  AND ADD TO SUBSURFACE RUNOFF/DRAINAGE/BASEFLOW.  RUNOFF2 IS ALREADY
!  A RATE AT THIS POINT
! ----------------------------------------------------------------------
         RUNOFF3 = RUNOFF3/ DT
         RUNOFF2 = RUNOFF2+ RUNOFF3
         SOILM = -1.0* SMC (1)* ZSOIL (1)
         DO K = 2,NSOIL
            SOILM = SOILM + SMC (K)* (ZSOIL (K -1) - ZSOIL (K))
         END DO
         SOILWM = -1.0* (SMCMAX - SMCWLT)* ZSOIL (1)
         SOILWW = -1.0* (SMC (1) - SMCWLT)* ZSOIL (1)

         DO K = 1,NSOIL
            SMAV(K)=(SMC(K) - SMCWLT)/(SMCMAX - SMCWLT)
         END DO

         IF (NROOT >= 2) THEN
            DO K = 2,NROOT
                SOILWM = SOILWM + (SMCMAX - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))
                SOILWW = SOILWW + (SMC(K) - SMCWLT)* (ZSOIL (K -1) - ZSOIL (K))
            END DO
         END IF
         IF (SOILWM .LT. 1.E-6) THEN
           SOILWM = 0.0
           SOILW  = 0.0
           SOILM  = 0.0
         ELSE
           SOILW = SOILWW / SOILWM
         END IF

! ----------------------------------------------------------------------
  END SUBROUTINE SFLX
! ----------------------------------------------------------------------

      SUBROUTINE ALCALC (ALB,SNOALB,EMBRD,SHDFAC,SHDMIN,SNCOVR,TSNOW,ALBEDO,EMISSI,   &
                         DT,SNOWNG,SNOTIME1,LVCOEF)

! ----------------------------------------------------------------------
! CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)
!   ALB     SNOWFREE ALBEDO
!   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO
!   SHDFAC    AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!   SHDMIN    MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!   SNCOVR  FRACTIONAL SNOW COVER
!   ALBEDO  SURFACE ALBEDO INCLUDING SNOW EFFECT
!   TSNOW   SNOW SURFACE TEMPERATURE (K)
! ----------------------------------------------------------------------
      IMPLICIT NONE

! ----------------------------------------------------------------------
! SNOALB IS ARGUMENT REPRESENTING MAXIMUM ALBEDO OVER DEEP SNOW,
! AS PASSED INTO SFLX, AND ADAPTED FROM THE SATELLITE-BASED MAXIMUM
! SNOW ALBEDO FIELDS PROVIDED BY D. ROBINSON AND G. KUKLA
! (1985, JCAM, VOL 24, 402-411)
! ----------------------------------------------------------------------
      REAL, INTENT(IN)  ::  ALB, SNOALB, EMBRD, SHDFAC, SHDMIN, SNCOVR, TSNOW
      REAL, INTENT(IN)  :: DT
      LOGICAL, INTENT(IN) :: SNOWNG
      REAL, INTENT(INOUT):: SNOTIME1
      REAL, INTENT(OUT) ::  ALBEDO, EMISSI
      REAL              :: SNOALB2
      REAL              :: TM,SNOALB1
      REAL, INTENT(IN)  :: LVCOEF
      REAL, PARAMETER   :: SNACCA=0.94,SNACCB=0.58,SNTHWA=0.82,SNTHWB=0.46
! turn of vegetation effect
!      ALBEDO = ALB + (1.0- (SHDFAC - SHDMIN))* SNCOVR * (SNOALB - ALB)
!      ALBEDO = (1.0-SNCOVR)*ALB + SNCOVR*SNOALB !this is equivalent to below
      ALBEDO = ALB + SNCOVR*(SNOALB-ALB)
      EMISSI = EMBRD + SNCOVR*(EMISSI_S - EMBRD)

!     BASE FORMULATION (DICKINSON ET AL., 1986, COGLEY ET AL., 1990)
!          IF (TSNOW.LE.263.16) THEN
!            ALBEDO=SNOALB
!          ELSE
!            IF (TSNOW.LT.273.16) THEN
!              TM=0.1*(TSNOW-263.16)
!              SNOALB1=0.5*((0.9-0.2*(TM**3))+(0.8-0.16*(TM**3)))
!            ELSE
!              SNOALB1=0.67
!             IF(SNCOVR.GT.0.95) SNOALB1= 0.6
!             SNOALB1 = ALB + SNCOVR*(SNOALB-ALB)
!            ENDIF
!          ENDIF
!            ALBEDO = ALB + SNCOVR*(SNOALB1-ALB)

!     ISBA FORMULATION (VERSEGHY, 1991; BAKER ET AL., 1990)
!          SNOALB1 = SNOALB+COEF*(0.85-SNOALB)
!          SNOALB2=SNOALB1
!!m          LSTSNW=LSTSNW+1
!          SNOTIME1 = SNOTIME1 + DT
!          IF (SNOWNG) THEN
!             SNOALB2=SNOALB
!!m             LSTSNW=0
!             SNOTIME1 = 0.0
!          ELSE
!            IF (TSNOW.LT.273.16) THEN
!!              SNOALB2=SNOALB-0.008*LSTSNW*DT/86400
!!m              SNOALB2=SNOALB-0.008*SNOTIME1/86400
!              SNOALB2=(SNOALB2-0.65)*EXP(-0.05*DT/3600)+0.65
!!              SNOALB2=(ALBEDO-0.65)*EXP(-0.01*DT/3600)+0.65
!            ELSE
!              SNOALB2=(SNOALB2-0.5)*EXP(-0.0005*DT/3600)+0.5
!!              SNOALB2=(SNOALB-0.5)*EXP(-0.24*LSTSNW*DT/86400)+0.5
!!m              SNOALB2=(SNOALB-0.5)*EXP(-0.24*SNOTIME1/86400)+0.5
!            ENDIF
!          ENDIF
!
!!               print*,'SNOALB2',SNOALB2,'ALBEDO',ALBEDO,'DT',DT
!          ALBEDO = ALB + SNCOVR*(SNOALB2-ALB)
!          IF (ALBEDO .GT. SNOALB2) ALBEDO=SNOALB2
!!m          LSTSNW1=LSTSNW
!!          SNOTIME = SNOTIME1

! formulation by Livneh
! ----------------------------------------------------------------------
! SNOALB IS CONSIDERED AS THE MAXIMUM SNOW ALBEDO FOR NEW SNOW, AT
! A VALUE OF 85%. SNOW ALBEDO CURVE DEFAULTS ARE FROM BRAS P.263. SHOULD
! NOT BE CHANGED EXCEPT FOR SERIOUS PROBLEMS WITH SNOW MELT.
! TO IMPLEMENT ACCUMULATIN PARAMETERS, SNACCA AND SNACCB, ASSERT THAT IT
! IS INDEED ACCUMULATION SEASON. I.E. THAT SNOW SURFACE TEMP IS BELOW
! ZERO AND THE DATE FALLS BETWEEN OCTOBER AND FEBRUARY
! ----------------------------------------------------------------------
         SNOALB1 = SNOALB+LVCOEF*(0.85-SNOALB)
         SNOALB2=SNOALB1
! ---------------- Initial LSTSNW --------------------------------------
          IF (SNOWNG) THEN
             SNOTIME1 = 0.
          ELSE
           SNOTIME1=SNOTIME1+DT
!               IF (TSNOW.LT.273.16) THEN
                   SNOALB2=SNOALB1*(SNACCA**((SNOTIME1/86400.0)**SNACCB))
!               ELSE
!                  SNOALB2 =SNOALB1*(SNTHWA**((SNOTIME1/86400.0)**SNTHWB))
!               ENDIF
          ENDIF
!
           SNOALB2 = MAX ( SNOALB2, ALB )
           ALBEDO = ALB + SNCOVR*(SNOALB2-ALB)
           IF (ALBEDO .GT. SNOALB2) ALBEDO=SNOALB2

!          IF (TSNOW.LT.273.16) THEN
!            ALBEDO=SNOALB-0.008*DT/86400
!          ELSE
!            ALBEDO=(SNOALB-0.5)*EXP(-0.24*DT/86400)+0.5
!          ENDIF

!      IF (ALBEDO > SNOALB) ALBEDO = SNOALB

! ----------------------------------------------------------------------
  END SUBROUTINE ALCALC
! ----------------------------------------------------------------------

      SUBROUTINE CANRES (SOLAR,CH,SFCTMP,Q2,SFCPRS,SMC,ZSOIL,NSOIL,       &
                         SMCWLT,SMCREF,RSMIN,RC,PC,NROOT,Q2SAT,DQSDT2,    &
                         TOPT,RSMAX,RGL,HS,XLAI,                          &
                         RCS,RCT,RCQ,RCSOIL,EMISSI)

! ----------------------------------------------------------------------
! SUBROUTINE CANRES
! ----------------------------------------------------------------------
! CALCULATE CANOPY RESISTANCE WHICH DEPENDS ON INCOMING SOLAR RADIATION,
! AIR TEMPERATURE, ATMOSPHERIC WATER VAPOR PRESSURE DEFICIT AT THE
! LOWEST MODEL LEVEL, AND SOIL MOISTURE (PREFERABLY UNFROZEN SOIL
! MOISTURE RATHER THAN TOTAL)
! ----------------------------------------------------------------------
! SOURCE:  JARVIS (1976), NOILHAN AND PLANTON (1989, MWR), JACQUEMIN AND
! NOILHAN (1990, BLM)
! SEE ALSO:  CHEN ET AL (1996, JGR, VOL 101(D3), 7251-7268), EQNS 12-14
! AND TABLE 2 OF SEC. 3.1.2
! ----------------------------------------------------------------------
! INPUT:
!   SOLAR   INCOMING SOLAR RADIATION
!   CH      SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!   SFCTMP  AIR TEMPERATURE AT 1ST LEVEL ABOVE GROUND
!   Q2      AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!   Q2SAT   SATURATION AIR HUMIDITY AT 1ST LEVEL ABOVE GROUND
!   DQSDT2  SLOPE OF SATURATION HUMIDITY FUNCTION WRT TEMP
!   SFCPRS  SURFACE PRESSURE
!   SMC     VOLUMETRIC SOIL MOISTURE
!   ZSOIL   SOIL DEPTH (NEGATIVE SIGN, AS IT IS BELOW GROUND)
!   NSOIL   NO. OF SOIL LAYERS
!   NROOT   NO. OF SOIL LAYERS IN ROOT ZONE (1.LE.NROOT.LE.NSOIL)
!   XLAI    LEAF AREA INDEX
!   SMCWLT  WILTING POINT
!   SMCREF  REFERENCE SOIL MOISTURE (WHERE SOIL WATER DEFICIT STRESS
!             SETS IN)
! RSMIN, RSMAX, TOPT, RGL, HS ARE CANOPY STRESS PARAMETERS SET IN
!   SURBOUTINE REDPRM
! OUTPUT:
!   PC  PLANT COEFFICIENT
!   RC  CANOPY RESISTANCE
! ----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NROOT,NSOIL
      INTEGER  K
      REAL,    INTENT(IN) :: CH,DQSDT2,HS,Q2,Q2SAT,RSMIN,RGL,RSMAX,        &
                             SFCPRS,SFCTMP,SMCREF,SMCWLT, SOLAR,TOPT,XLAI, &
                             EMISSI
      REAL,DIMENSION(1:NSOIL), INTENT(IN) :: SMC,ZSOIL
      REAL,    INTENT(OUT):: PC,RC,RCQ,RCS,RCSOIL,RCT
      REAL                :: DELTA,FF,GX,P,RR
      REAL, DIMENSION(1:NSOIL) ::  PART
      REAL, PARAMETER     :: SLV = 2.501000E6


! ----------------------------------------------------------------------
! INITIALIZE CANOPY RESISTANCE MULTIPLIER TERMS.
! ----------------------------------------------------------------------
      RCS = 0.0
      RCT = 0.0
      RCQ = 0.0
      RCSOIL = 0.0

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO INCOMING SOLAR RADIATION
! ----------------------------------------------------------------------
      RC = 0.0
      FF = 0.55*2.0* SOLAR / (RGL * XLAI)
      RCS = (FF + RSMIN / RSMAX) / (1.0+ FF)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO AIR TEMPERATURE AT FIRST MODEL LEVEL ABOVE GROUND
! RCT EXPRESSION FROM NOILHAN AND PLANTON (1989, MWR).
! ----------------------------------------------------------------------
      RCS = MAX (RCS,0.0001)
      RCT = 1.0- 0.0016* ( (TOPT - SFCTMP)**2.0)

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO VAPOR PRESSURE DEFICIT AT FIRST MODEL LEVEL.
! RCQ EXPRESSION FROM SSIB
! ----------------------------------------------------------------------
      RCT = MAX (RCT,0.0001)
      RCQ = 1.0/ (1.0+ HS * (Q2SAT - Q2))

! ----------------------------------------------------------------------
! CONTRIBUTION DUE TO SOIL MOISTURE AVAILABILITY.
! DETERMINE CONTRIBUTION FROM EACH SOIL LAYER, THEN ADD THEM UP.
! ----------------------------------------------------------------------
      RCQ = MAX (RCQ,0.01)
      GX = (SMC (1) - SMCWLT) / (SMCREF - SMCWLT)
      IF (GX  >  1.) GX = 1.
      IF (GX  <  0.) GX = 0.

! ----------------------------------------------------------------------
! USE SOIL DEPTH AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
!      PART(1) = RTDIS(1) * GX
! ----------------------------------------------------------------------
      PART (1) = (ZSOIL (1)/ ZSOIL (NROOT)) * GX
      DO K = 2,NROOT
         GX = (SMC (K) - SMCWLT) / (SMCREF - SMCWLT)
         IF (GX >  1.) GX = 1.
         IF (GX <  0.) GX = 0.
! ----------------------------------------------------------------------
! USE SOIL DEPTH AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! USE ROOT DISTRIBUTION AS WEIGHTING FACTOR
!        PART(K) = RTDIS(K) * GX
! ----------------------------------------------------------------------
         PART (K) = ( (ZSOIL (K) - ZSOIL (K -1))/ ZSOIL (NROOT)) * GX
      END DO
      DO K = 1,NROOT
         RCSOIL = RCSOIL + PART (K)
      END DO

! ----------------------------------------------------------------------
! DETERMINE CANOPY RESISTANCE DUE TO ALL FACTORS.  CONVERT CANOPY
! RESISTANCE (RC) TO PLANT COEFFICIENT (PC) TO BE USED WITH POTENTIAL
! EVAP IN DETERMINING ACTUAL EVAP.  PC IS DETERMINED BY:
!   PC * LINERIZED PENMAN POTENTIAL EVAP =
!   PENMAN-MONTEITH ACTUAL EVAPORATION (CONTAINING RC TERM).
! ----------------------------------------------------------------------
      RCSOIL = MAX (RCSOIL,0.0001)

      RC = RSMIN / (XLAI * RCS * RCT * RCQ * RCSOIL)
!      RR = (4.* SIGMA * RD / CP)* (SFCTMP **4.)/ (SFCPRS * CH) + 1.0
      RR = (4.* EMISSI *SIGMA * RD / CP)* (SFCTMP **4.)/ (SFCPRS * CH) &
             + 1.0

      DELTA = (SLV / CP)* DQSDT2

      PC = (RR + DELTA)/ (RR * (1. + RC * CH) + DELTA)

! ----------------------------------------------------------------------
  END SUBROUTINE CANRES
! ----------------------------------------------------------------------

      SUBROUTINE CSNOW (SNCOND,DSNOW)

! ----------------------------------------------------------------------
! SUBROUTINE CSNOW
! FUNCTION CSNOW
! ----------------------------------------------------------------------
! CALCULATE SNOW TERMAL CONDUCTIVITY
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN) :: DSNOW
      REAL, INTENT(OUT):: SNCOND
      REAL             :: C
      REAL, PARAMETER  :: UNIT = 0.11631

! ----------------------------------------------------------------------
! SNCOND IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)
! CSNOW IN UNITS OF CAL/(CM*HR*C), RETURNED IN W/(M*C)
! BASIC VERSION IS DYACHKOVA EQUATION (1960), FOR RANGE 0.1-0.4
! ----------------------------------------------------------------------
      C = 0.328*10** (2.25* DSNOW)
!      CSNOW=UNIT*C

! ----------------------------------------------------------------------
! DE VAUX EQUATION (1933), IN RANGE 0.1-0.6
! ----------------------------------------------------------------------
!      SNCOND=0.0293*(1.+100.*DSNOW**2)
!      CSNOW=0.0293*(1.+100.*DSNOW**2)

! ----------------------------------------------------------------------
! E. ANDERSEN FROM FLERCHINGER
! ----------------------------------------------------------------------
!      SNCOND=0.021+2.51*DSNOW**2
!      CSNOW=0.021+2.51*DSNOW**2

!      SNCOND = UNIT * C
! double snow thermal conductivity
      SNCOND = 2.0 * UNIT * C

! ----------------------------------------------------------------------
  END SUBROUTINE CSNOW
! ----------------------------------------------------------------------
      SUBROUTINE DEVAP (EDIR,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,         &
                        DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)

! ----------------------------------------------------------------------
! SUBROUTINE DEVAP
! FUNCTION DEVAP
! ----------------------------------------------------------------------
! CALCULATE DIRECT SOIL EVAPORATION
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN) :: ETP1,SMC,BEXP,DKSAT,DWSAT,FXEXP,              &
                          SHDFAC,SMCDRY,SMCMAX,ZSOIL,SMCREF,SMCWLT
      REAL, INTENT(OUT):: EDIR
      REAL             :: FX, SRATIO


! ----------------------------------------------------------------------
! DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR
! WHEN FXEXP=1.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! FX > 1 REPRESENTS DEMAND CONTROL
! FX < 1 REPRESENTS FLUX CONTROL
! ----------------------------------------------------------------------

      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO > 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF

! ----------------------------------------------------------------------
! ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
! ----------------------------------------------------------------------
      EDIR = FX * ( 1.0- SHDFAC ) * ETP1

! ----------------------------------------------------------------------
  END SUBROUTINE DEVAP

      SUBROUTINE DEVAP_hydro (EDIR,ETP1,SMC,ZSOIL,SHDFAC,SMCMAX,BEXP,         &
                        DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP,         &
                        SFHEAD1RT,ETPND1,DT)

! ----------------------------------------------------------------------
! SUBROUTINE DEVAP
! FUNCTION DEVAP
! ----------------------------------------------------------------------
! CALCULATE DIRECT SOIL EVAPORATION
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN) :: ETP1,SMC,BEXP,DKSAT,DWSAT,FXEXP,              &
                          SHDFAC,SMCDRY,SMCMAX,ZSOIL,SMCREF,SMCWLT
      REAL, INTENT(OUT):: EDIR
      REAL             :: FX, SRATIO

      REAL, INTENT(INOUT) :: SFHEAD1RT,ETPND1
      REAL, INTENT(IN   ) :: DT
       REAL             :: EDIRTMP



! ----------------------------------------------------------------------
! DIRECT EVAP A FUNCTION OF RELATIVE SOIL MOISTURE AVAILABILITY, LINEAR
! WHEN FXEXP=1.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! FX > 1 REPRESENTS DEMAND CONTROL
! FX < 1 REPRESENTS FLUX CONTROL
! ----------------------------------------------------------------------

      SRATIO = (SMC - SMCDRY) / (SMCMAX - SMCDRY)
      IF (SRATIO > 0.) THEN
        FX = SRATIO**FXEXP
        FX = MAX ( MIN ( FX, 1. ) ,0. )
      ELSE
        FX = 0.
      ENDIF

!DJG NDHMS/WRF-Hydro edits... Adjustment for ponded surface water : Reduce ETP1
      EDIRTMP = 0.
      ETPND1 = 0.

!DJG NDHMS/WRF-Hydro edits...  Calc Max Potential Dir Evap. (ETP1 units: }=m/s)

!DJG NDHMS/WRF-Hydro...currently set ponded water evap to 0.0 until further notice...11/5/2012
!EDIRTMP = ( 1.0- SHDFAC ) * ETP1

! Convert all units to (m)
! Convert EDIRTMP  from (kg m{-2} s{-1}=m/s) to (m) ...
      EDIRTMP = EDIRTMP * DT

!DJG NDHMS/WRF-Hydro edits... Convert SFHEAD from (mm) to (m) ...
      SFHEAD1RT=SFHEAD1RT * 0.001



!DJG NDHMS/WRF-Hydro edits... Calculate ETPND as reduction in EDIR(TMP)...
      IF (EDIRTMP > 0.) THEN
       IF ( EDIRTMP > SFHEAD1RT ) THEN
         ETPND1 = SFHEAD1RT
         SFHEAD1RT=0.
         EDIRTMP = EDIRTMP - ETPND1
       ELSE
         ETPND1 = EDIRTMP
         EDIRTMP = 0.
         SFHEAD1RT = SFHEAD1RT - ETPND1
       END IF
      END IF

!DJG NDHMS/WRF-Hydro edits... Convert SFHEAD units back to (mm)
      IF ( SFHEAD1RT /= 0.) SFHEAD1RT=SFHEAD1RT * 1000.

!DJG NDHMS/WRF-Hydro edits...Convert ETPND and EDIRTMP back to (mm/s=kg m{-2} s{-1})
      ETPND1 = ETPND1 / DT
      EDIRTMP = EDIRTMP / DT
!DEBUG    print *, "After DEVAP...SFCHEAD+ETPND1",SFHEAD1RT+ETPND1*DT


! ----------------------------------------------------------------------
! ALLOW FOR THE DIRECT-EVAP-REDUCING EFFECT OF SHADE
! ----------------------------------------------------------------------
!DJG NDHMS/WRF-Hydro edits...
!       EDIR = FX * ( 1.0- SHDFAC ) * ETP1
       EDIR = FX * EDIRTMP




! ----------------------------------------------------------------------
  END SUBROUTINE DEVAP_hydro
! ----------------------------------------------------------------------

      SUBROUTINE EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,               &
                         SH2O,                                          &
                         SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,             &
                         SMCREF,SHDFAC,CMCMAX,                          &
                         SMCDRY,CFACTR,                                 &
                         EDIR,EC,ET,ETT,SFCTMP,Q2,NROOT,RTDIS,FXEXP,    &
                         SFHEAD1RT,ETPND1)

! ----------------------------------------------------------------------
! SUBROUTINE EVAPO
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)   :: NSOIL, NROOT
      INTEGER               :: I,K
      REAL,    INTENT(IN)   :: BEXP, CFACTR,CMC,CMCMAX,DKSAT,           &
                                 DT,DWSAT,ETP1,FXEXP,PC,Q2,SFCTMP,           &
                                 SHDFAC,SMCDRY,SMCMAX,SMCREF,SMCWLT
      REAL,    INTENT(OUT)  :: EC,EDIR,ETA1,ETT
      REAL                  :: CMC2MS
      REAL,DIMENSION(1:NSOIL), INTENT(IN) :: RTDIS, SMC, SH2O, ZSOIL
      REAL,DIMENSION(1:NSOIL), INTENT(OUT) :: ET

      REAL,   INTENT(INOUT) :: SFHEAD1RT,ETPND1

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE IF THE POTENTIAL EVAPOTRANSPIRATION IS
! GREATER THAN ZERO.
! ----------------------------------------------------------------------
      EDIR = 0.
      EC = 0.
      ETT = 0.
      DO K = 1,NSOIL
         ET (K) = 0.
      END DO

! ----------------------------------------------------------------------
! RETRIEVE DIRECT EVAPORATION FROM SOIL SURFACE.  CALL THIS FUNCTION
! ONLY IF VEG COVER NOT COMPLETE.
! FROZEN GROUND VERSION:  SH2O STATES REPLACE SMC STATES.
! ----------------------------------------------------------------------
      IF (ETP1 > 0.0) THEN
         IF (SHDFAC <  1.) THEN
#ifdef WRF_HYDRO
!            CALL DEVAP_hydro (EDIR,ETP1,SMC (1),ZSOIL (1),SHDFAC,SMCMAX,      &
!                        BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP,    &
!                         SFHEAD1RT,ETPND1,DT)
!DJG Reduce ETP1 by EDIR & ETPND1...
!                ETP1=ETP1-EDIR-ETPND1

! following is the temparay setting ...
             CALL DEVAP (EDIR,ETP1,SMC (1),ZSOIL (1),SHDFAC,SMCMAX,      &
                         BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)
!            ETP1=ETP1-EDIR
#else
             CALL DEVAP (EDIR,ETP1,SMC (1),ZSOIL (1),SHDFAC,SMCMAX,      &
                         BEXP,DKSAT,DWSAT,SMCDRY,SMCREF,SMCWLT,FXEXP)
#endif
         END IF
! ----------------------------------------------------------------------
! INITIALIZE PLANT TOTAL TRANSPIRATION, RETRIEVE PLANT TRANSPIRATION,
! AND ACCUMULATE IT FOR ALL SOIL LAYERS.
! ----------------------------------------------------------------------

         IF (SHDFAC > 0.0) THEN
            CALL TRANSP (ET,NSOIL,ETP1,SH2O,CMC,ZSOIL,SHDFAC,SMCWLT,     &
                          CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,RTDIS)
            DO K = 1,NSOIL
               ETT = ETT + ET ( K )
            END DO
! ----------------------------------------------------------------------
! CALCULATE CANOPY EVAPORATION.
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR CMC=0.0.
! ----------------------------------------------------------------------
            IF (CMC > 0.0) THEN
               EC = SHDFAC * ( ( CMC / CMCMAX ) ** CFACTR ) * ETP1
            ELSE
               EC = 0.0
            END IF
! ----------------------------------------------------------------------
! EC SHOULD BE LIMITED BY THE TOTAL AMOUNT OF AVAILABLE WATER ON THE
! CANOPY.  -F.CHEN, 18-OCT-1994
! ----------------------------------------------------------------------
            CMC2MS = CMC / DT
            EC = MIN ( CMC2MS, EC )
         END IF
      END IF
! ----------------------------------------------------------------------
! TOTAL UP EVAP AND TRANSP TYPES TO OBTAIN ACTUAL EVAPOTRANSP
! ----------------------------------------------------------------------
      ETA1 = EDIR + ETT + EC

! ----------------------------------------------------------------------
  END SUBROUTINE EVAPO
! ----------------------------------------------------------------------

  SUBROUTINE FAC2MIT(SMCMAX,FLIMIT)
    IMPLICIT NONE		
    REAL, INTENT(IN)  :: SMCMAX
    REAL, INTENT(OUT) :: FLIMIT

    FLIMIT = 0.90

    IF ( SMCMAX == 0.395 ) THEN
       FLIMIT = 0.59
    ELSE IF ( ( SMCMAX == 0.434 ) .OR. ( SMCMAX == 0.404 ) ) THEN
       FLIMIT = 0.85
    ELSE IF ( ( SMCMAX == 0.465 ) .OR. ( SMCMAX == 0.406 ) ) THEN
       FLIMIT = 0.86
    ELSE IF ( ( SMCMAX == 0.476 ) .OR. ( SMCMAX == 0.439 ) ) THEN
       FLIMIT = 0.74
    ELSE IF ( ( SMCMAX == 0.200 ) .OR. ( SMCMAX == 0.464 ) ) THEN
       FLIMIT = 0.80
    ENDIF

! ----------------------------------------------------------------------
  END SUBROUTINE FAC2MIT
! ----------------------------------------------------------------------

      SUBROUTINE FRH2O (FREE,TKELV,SMC,SH2O,SMCMAX,BEXP,PSIS)

! ----------------------------------------------------------------------
! SUBROUTINE FRH2O
! ----------------------------------------------------------------------
! CALCULATE AMOUNT OF SUPERCOOLED LIQUID SOIL WATER CONTENT IF
! TEMPERATURE IS BELOW 273.15K (T0).  REQUIRES NEWTON-TYPE ITERATION TO
! SOLVE THE NONLINEAR IMPLICIT EQUATION GIVEN IN EQN 17 OF KOREN ET AL
! (1999, JGR, VOL 104(D16), 19569-19585).
! ----------------------------------------------------------------------
! NEW VERSION (JUNE 2001): MUCH FASTER AND MORE ACCURATE NEWTON
! ITERATION ACHIEVED BY FIRST TAKING LOG OF EQN CITED ABOVE -- LESS THAN
! 4 (TYPICALLY 1 OR 2) ITERATIONS ACHIEVES CONVERGENCE.  ALSO, EXPLICIT
! 1-STEP SOLUTION OPTION FOR SPECIAL CASE OF PARAMETER CK=0, WHICH
! REDUCES THE ORIGINAL IMPLICIT EQUATION TO A SIMPLER EXPLICIT FORM,
! KNOWN AS THE "FLERCHINGER EQN". IMPROVED HANDLING OF SOLUTION IN THE
! LIMIT OF FREEZING POINT TEMPERATURE T0.
! ----------------------------------------------------------------------
! INPUT:

!   TKELV.........TEMPERATURE (Kelvin)
!   SMC...........TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SH2O..........LIQUID SOIL MOISTURE CONTENT (VOLUMETRIC)
!   SMCMAX........SATURATION SOIL MOISTURE CONTENT (FROM REDPRM)
!   B.............SOIL TYPE "B" PARAMETER (FROM REDPRM)
!   PSIS..........SATURATED SOIL MATRIC POTENTIAL (FROM REDPRM)

! OUTPUT:
!   FRH2O.........SUPERCOOLED LIQUID WATER CONTENT
!   FREE..........SUPERCOOLED LIQUID WATER CONTENT
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN)     :: BEXP,PSIS,SH2O,SMC,SMCMAX,TKELV
      REAL, INTENT(OUT)    :: FREE
      REAL                 :: BX,DENOM,DF,DSWL,FK,SWL,SWLK
      INTEGER              :: NLOG,KCOUNT
!      PARAMETER(CK = 0.0)
      REAL, PARAMETER      :: CK = 8.0, BLIM = 5.5, ERROR = 0.005,       &
                              HLICE = 3.335E5, GS = 9.81,DICE = 920.0,   &
                              DH2O = 1000.0, T0 = 273.15

! ----------------------------------------------------------------------
! LIMITS ON PARAMETER B: B < 5.5  (use parameter BLIM)
! SIMULATIONS SHOWED IF B > 5.5 UNFROZEN WATER CONTENT IS
! NON-REALISTICALLY HIGH AT VERY LOW TEMPERATURES.
! ----------------------------------------------------------------------
      BX = BEXP

! ----------------------------------------------------------------------
! INITIALIZING ITERATIONS COUNTER AND ITERATIVE SOLUTION FLAG.
! ----------------------------------------------------------------------
      IF (BEXP >  BLIM) BX = BLIM
      NLOG = 0

! ----------------------------------------------------------------------
!  IF TEMPERATURE NOT SIGNIFICANTLY BELOW FREEZING (T0), SH2O = SMC
! ----------------------------------------------------------------------
      KCOUNT = 0
!      FRH2O = SMC
      IF (TKELV > (T0- 1.E-3)) THEN
          FREE = SMC
      ELSE

! ----------------------------------------------------------------------
! OPTION 1: ITERATED SOLUTION FOR NONZERO CK
! IN KOREN ET AL, JGR, 1999, EQN 17
! ----------------------------------------------------------------------
! INITIAL GUESS FOR SWL (frozen content)
! ----------------------------------------------------------------------
         IF (CK /= 0.0) THEN
            SWL = SMC - SH2O
! ----------------------------------------------------------------------
! KEEP WITHIN BOUNDS.
! ----------------------------------------------------------------------
            IF (SWL > (SMC -0.02)) SWL = SMC -0.02

! ----------------------------------------------------------------------
!  START OF ITERATIONS
! ----------------------------------------------------------------------
            IF (SWL < 0.) SWL = 0.
 1001       Continue
              IF (.NOT.( (NLOG < 10) .AND. (KCOUNT == 0)))   goto 1002
              NLOG = NLOG +1
              DF = ALOG ( ( PSIS * GS / HLICE ) * ( ( 1. + CK * SWL )**2.) * &
                   ( SMCMAX / (SMC - SWL) )** BX) - ALOG ( - (               &
                   TKELV - T0)/ TKELV)
              DENOM = 2. * CK / ( 1. + CK * SWL ) + BX / ( SMC - SWL )
              SWLK = SWL - DF / DENOM
! ----------------------------------------------------------------------
! BOUNDS USEFUL FOR MATHEMATICAL SOLUTION.
! ----------------------------------------------------------------------
              IF (SWLK > (SMC -0.02)) SWLK = SMC - 0.02
              IF (SWLK < 0.) SWLK = 0.

! ----------------------------------------------------------------------
! MATHEMATICAL SOLUTION BOUNDS APPLIED.
! ----------------------------------------------------------------------
              DSWL = ABS (SWLK - SWL)

! ----------------------------------------------------------------------
! IF MORE THAN 10 ITERATIONS, USE EXPLICIT METHOD (CK=0 APPROX.)
! WHEN DSWL LESS OR EQ. ERROR, NO MORE ITERATIONS REQUIRED.
! ----------------------------------------------------------------------
              SWL = SWLK
              IF ( DSWL <= ERROR ) THEN
                    KCOUNT = KCOUNT +1
              END IF
! ----------------------------------------------------------------------
!  END OF ITERATIONS
! ----------------------------------------------------------------------
! BOUNDS APPLIED WITHIN DO-BLOCK ARE VALID FOR PHYSICAL SOLUTION.
! ----------------------------------------------------------------------
!          FRH2O = SMC - SWL
           goto 1001
 1002   continue
           FREE = SMC - SWL
         END IF
! ----------------------------------------------------------------------
! END OPTION 1
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! OPTION 2: EXPLICIT SOLUTION FOR FLERCHINGER EQ. i.e. CK=0
! IN KOREN ET AL., JGR, 1999, EQN 17
! APPLY PHYSICAL BOUNDS TO FLERCHINGER SOLUTION
! ----------------------------------------------------------------------
         IF (KCOUNT == 0) THEN
             PRINT *,'Flerchinger USEd in NEW version. Iterations=',NLOG
                  FK = ( ( (HLICE / (GS * ( - PSIS)))*                    &
                       ( (TKELV - T0)/ TKELV))** ( -1/ BX))* SMCMAX
!            FRH2O = MIN (FK, SMC)
             IF (FK < 0.02) FK = 0.02
             FREE = MIN (FK, SMC)
! ----------------------------------------------------------------------
! END OPTION 2
! ----------------------------------------------------------------------
         END IF
      END IF
! ----------------------------------------------------------------------
  END SUBROUTINE FRH2O
! ----------------------------------------------------------------------

      SUBROUTINE HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,          &
                       TBOT,ZBOT,PSISAT,SH2O,DT,BEXP,SOILTYP,OPT_THCND, &
                       F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP,ISURBAN      &
                      ,HCPCT_FASDAS                                     ) !fasdas

! ----------------------------------------------------------------------
! SUBROUTINE HRT
! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! THERMAL DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL              :: ITAVG
      INTEGER, INTENT(IN)  :: OPT_THCND
      INTEGER, INTENT(IN)  :: NSOIL, VEGTYP, SOILTYP
      INTEGER, INTENT(IN)  :: ISURBAN
      INTEGER              :: I, K

      REAL, INTENT(IN)     :: BEXP, CSOIL, DF1, DT,F1,PSISAT,QUARTZ,     &
                              SMCMAX ,TBOT,YY,ZZ1, ZBOT
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: SMC,STC,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: SH2O
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: RHSTS
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: AI, BI,CI
      REAL                 :: DDZ, DDZ2, DENOM, DF1N, DF1K, DTSDZ,       &
                              DTSDZ2,HCPCT,QTOT,SSOIL,SICE,TAVG,TBK,     &
                              TBK1,TSNSR,TSURF,CSOIL_LOC
      REAL, PARAMETER      :: T0 = 273.15, CAIR = 1004.0, CICE = 2.106E6,&
                              CH2O = 4.2E6

!
! FASDAS
!
      REAL, INTENT(  OUT)       :: HCPCT_FASDAS
!
! END FASDAS
!

!urban
        IF( VEGTYP == ISURBAN ) then
            CSOIL_LOC=3.0E6
        ELSE
            CSOIL_LOC=CSOIL
        ENDIF

! ----------------------------------------------------------------------
! INITIALIZE LOGICAL FOR SOIL LAYER TEMPERATURE AVERAGING.
! ----------------------------------------------------------------------
       ITAVG = .TRUE.
! ----------------------------------------------------------------------
! BEGIN SECTION FOR TOP SOIL LAYER
! ----------------------------------------------------------------------
! CALC THE HEAT CAPACITY OF THE TOP SOIL LAYER
! ----------------------------------------------------------------------
      HCPCT = SH2O (1)* CH2O + (1.0- SMCMAX)* CSOIL_LOC + (SMCMAX - SMC (1))&
       * CAIR                                                           &
              + ( SMC (1) - SH2O (1) )* CICE
!
! FASDAS
!
      HCPCT_FASDAS = HCPCT
!
! END FASDAS
!
! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
      DDZ = 1.0 / ( -0.5 * ZSOIL (2) )
      AI (1) = 0.0
      CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)

! ----------------------------------------------------------------------
! CALCULATE THE VERTICAL SOIL TEMP GRADIENT BTWN THE 1ST AND 2ND SOIL
! LAYERS.  THEN CALCULATE THE SUBSURFACE HEAT FLUX. USE THE TEMP
! GRADIENT AND SUBSFC HEAT FLUX TO CALC "RIGHT-HAND SIDE TENDENCY
! TERMS", OR "RHSTS", FOR TOP SOIL LAYER.
! ----------------------------------------------------------------------
      BI (1) = - CI (1) + DF1 / (0.5 * ZSOIL (1) * ZSOIL (1)* HCPCT *    &
       ZZ1)
      DTSDZ = (STC (1) - STC (2)) / ( -0.5 * ZSOIL (2))
      SSOIL = DF1 * (STC (1) - YY) / (0.5 * ZSOIL (1) * ZZ1)
!      RHSTS(1) = (DF1 * DTSDZ - SSOIL) / (ZSOIL(1) * HCPCT)
      DENOM = (ZSOIL (1) * HCPCT)

! ----------------------------------------------------------------------
! NEXT CAPTURE THE VERTICAL DIFFERENCE OF THE HEAT FLUX AT TOP AND
! BOTTOM OF FIRST SOIL LAYER FOR USE IN HEAT FLUX CONSTRAINT APPLIED TO
! POTENTIAL SOIL FREEZING/THAWING IN ROUTINE SNKSRC.
! ----------------------------------------------------------------------
!      QTOT = SSOIL - DF1*DTSDZ
      RHSTS (1) = (DF1 * DTSDZ - SSOIL) / DENOM

! ----------------------------------------------------------------------
! CALCULATE FROZEN WATER CONTENT IN 1ST SOIL LAYER.
! ----------------------------------------------------------------------
      QTOT = -1.0* RHSTS (1)* DENOM

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):
! SET TEMP "TSURF" AT TOP OF SOIL COLUMN (FOR USE IN FREEZING SOIL
! PHYSICS LATER IN FUNCTION SUBROUTINE SNKSRC).  IF SNOWPACK CONTENT IS
! ZERO, THEN TSURF EXPRESSION BELOW GIVES TSURF = SKIN TEMP.  IF
! SNOWPACK IS NONZERO (HENCE ARGUMENT ZZ1=1), THEN TSURF EXPRESSION
! BELOW YIELDS SOIL COLUMN TOP TEMPERATURE UNDER SNOWPACK.  THEN
! CALCULATE TEMPERATURE AT BOTTOM INTERFACE OF 1ST SOIL LAYER FOR USE
! LATER IN FUNCTION SUBROUTINE SNKSRC
! ----------------------------------------------------------------------
      SICE = SMC (1) - SH2O (1)
      IF (ITAVG) THEN
         TSURF = (YY + (ZZ1-1) * STC (1)) / ZZ1
! ----------------------------------------------------------------------
! IF FROZEN WATER PRESENT OR ANY OF LAYER-1 MID-POINT OR BOUNDING
! INTERFACE TEMPERATURES BELOW FREEZING, THEN CALL SNKSRC TO
! COMPUTE HEAT SOURCE/SINK (AND CHANGE IN FROZEN WATER CONTENT)
! DUE TO POSSIBLE SOIL WATER PHASE CHANGE
! ----------------------------------------------------------------------
         CALL TBND (STC (1),STC (2),ZSOIL,ZBOT,1,NSOIL,TBK)
         IF ( (SICE > 0.) .OR. (STC (1) < T0) .OR.                         &
            (TSURF < T0) .OR. (TBK < T0) ) THEN
!          TSNSR = SNKSRC (TAVG,SMC(1),SH2O(1),
            CALL TMPAVG (TAVG,TSURF,STC (1),TBK,ZSOIL,NSOIL,1)
            CALL SNKSRC (TSNSR,TAVG,SMC (1),SH2O (1),                      &
                          ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)
!          RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )
            RHSTS (1) = RHSTS (1) - TSNSR / DENOM
         END IF
      ELSE
!          TSNSR = SNKSRC (STC(1),SMC(1),SH2O(1),
         IF ( (SICE > 0.) .OR. (STC (1) < T0) ) THEN
            CALL SNKSRC (TSNSR,STC (1),SMC (1),SH2O (1),                   &
                          ZSOIL,NSOIL,SMCMAX,PSISAT,BEXP,DT,1,QTOT)
!          RHSTS(1) = RHSTS(1) - TSNSR / ( ZSOIL(1) * HCPCT )
            RHSTS (1) = RHSTS (1) - TSNSR / DENOM
         END IF
! ----------------------------------------------------------------------
! THIS ENDS SECTION FOR TOP SOIL LAYER.
! ----------------------------------------------------------------------
      END IF

! INITIALIZE DDZ2
! ----------------------------------------------------------------------

      DDZ2 = 0.0
      DF1K = DF1

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
! (EXCEPT SUBSFC OR "GROUND" HEAT FLUX NOT REPEATED IN LOWER LAYERS)
! ----------------------------------------------------------------------
! CALCULATE HEAT CAPACITY FOR THIS SOIL LAYER.
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
         HCPCT = SH2O (K)* CH2O + (1.0- SMCMAX)* CSOIL_LOC + (SMCMAX - SMC (  &
                K))* CAIR + ( SMC (K) - SH2O (K) )* CICE
! ----------------------------------------------------------------------
! THIS SECTION FOR LAYER 2 OR GREATER, BUT NOT LAST LAYER.
! ----------------------------------------------------------------------
! CALCULATE THERMAL DIFFUSIVITY FOR THIS LAYER.
! ----------------------------------------------------------------------
         IF (K /= NSOIL) THEN

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER
! ----------------------------------------------------------------------
            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K),BEXP, PSISAT, SOILTYP, OPT_THCND)

!urban
       IF ( VEGTYP == ISURBAN ) DF1N = 3.24

            DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
! ----------------------------------------------------------------------
            DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM
            DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
! TEMP AT BOTTOM OF LAYER.
! ----------------------------------------------------------------------
            CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K)) *      &
       HCPCT)
            IF (ITAVG) THEN
               CALL TBND (STC (K),STC (K +1),ZSOIL,ZBOT,K,NSOIL,TBK1)
            END IF

         ELSE
! ----------------------------------------------------------------------
! SPECIAL CASE OF BOTTOM SOIL LAYER:  CALCULATE THERMAL DIFFUSIVITY FOR
! BOTTOM LAYER.
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU BOTTOM LAYER.
! ----------------------------------------------------------------------
            CALL TDFCND (DF1N,SMC (K),QUARTZ,SMCMAX,SH2O (K),BEXP, PSISAT, SOILTYP, OPT_THCND)


!urban
       IF ( VEGTYP == ISURBAN ) DF1N = 3.24

            DENOM = .5 * (ZSOIL (K -1) + ZSOIL (K)) - ZBOT

! ----------------------------------------------------------------------
! SET MATRIX COEF, CI TO ZERO IF BOTTOM LAYER.
! ----------------------------------------------------------------------
            DTSDZ2 = (STC (K) - TBOT) / DENOM

! ----------------------------------------------------------------------
! IF TEMPERATURE AVERAGING INVOKED (ITAVG=TRUE; ELSE SKIP):  CALCULATE
! TEMP AT BOTTOM OF LAST LAYER.
! ----------------------------------------------------------------------
            CI (K) = 0.
            IF (ITAVG) THEN
               CALL TBND (STC (K),TBOT,ZSOIL,ZBOT,K,NSOIL,TBK1)
            END IF
! ----------------------------------------------------------------------
! THIS ENDS SPECIAL LOOP FOR BOTTOM LAYER.
         END IF
! ----------------------------------------------------------------------
! CALCULATE RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
! ----------------------------------------------------------------------
         DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT
         RHSTS (K) = ( DF1N * DTSDZ2- DF1K * DTSDZ ) / DENOM
         QTOT = -1.0* DENOM * RHSTS (K)

         SICE = SMC (K) - SH2O (K)
         IF (ITAVG) THEN
            CALL TMPAVG (TAVG,TBK,STC (K),TBK1,ZSOIL,NSOIL,K)
!            TSNSR = SNKSRC(TAVG,SMC(K),SH2O(K),ZSOIL,NSOIL,
            IF ( (SICE > 0.) .OR. (STC (K) < T0) .OR.                   &
               (TBK .lt. T0) .OR. (TBK1 .lt. T0) ) THEN
               CALL SNKSRC (TSNSR,TAVG,SMC (K),SH2O (K),ZSOIL,NSOIL,    &
                             SMCMAX,PSISAT,BEXP,DT,K,QTOT)
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM
            END IF
         ELSE
!            TSNSR = SNKSRC(STC(K),SMC(K),SH2O(K),ZSOIL,NSOIL,
            IF ( (SICE > 0.) .OR. (STC (K) < T0) ) THEN
               CALL SNKSRC (TSNSR,STC (K),SMC (K),SH2O (K),ZSOIL,NSOIL, &
                             SMCMAX,PSISAT,BEXP,DT,K,QTOT)
               RHSTS (K) = RHSTS (K) - TSNSR / DENOM
            END IF
         END IF

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
! ----------------------------------------------------------------------
         AI (K) = - DF1K * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)

! ----------------------------------------------------------------------
! RESET VALUES OF DF1, DTSDZ, DDZ, AND TBK FOR LOOP TO NEXT SOIL LAYER.
! ----------------------------------------------------------------------
         BI (K) = - (AI (K) + CI (K))
         TBK = TBK1
         DF1K = DF1N
         DTSDZ = DTSDZ2
         DDZ = DDZ2
      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE HRT
! ----------------------------------------------------------------------

      SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)

! ----------------------------------------------------------------------
! SUBROUTINE HSTEP
! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)  :: NSOIL
      INTEGER              :: K

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: STCIN
      REAL, DIMENSION(1:NSOIL), INTENT(OUT):: STCOUT
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: RHSTS
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: AI,BI,CI
      REAL, DIMENSION(1:NSOIL) :: RHSTSin
      REAL, DIMENSION(1:NSOIL) :: CIin
      REAL                 :: DT

! ----------------------------------------------------------------------
! CREATE FINITE DIFFERENCE VALUES FOR USE IN ROSR12 ROUTINE
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTS (K) = RHSTS (K) * DT
         AI (K) = AI (K) * DT
         BI (K) = 1. + BI (K) * DT
         CI (K) = CI (K) * DT
      END DO
! ----------------------------------------------------------------------
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTSin (K) = RHSTS (K)
      END DO
      DO K = 1,NSOIL
         CIin (K) = CI (K)
      END DO
! ----------------------------------------------------------------------
! SOLVE THE TRI-DIAGONAL MATRIX EQUATION
! ----------------------------------------------------------------------
      CALL ROSR12 (CI,AI,BI,CIin,RHSTSin,RHSTS,NSOIL)
! ----------------------------------------------------------------------
! CALC/UPDATE THE SOIL TEMPS USING MATRIX SOLUTION
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         STCOUT (K) = STCIN (K) + CI (K)
      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE HSTEP
! ----------------------------------------------------------------------

      SUBROUTINE NOPAC (ETP,ETA,PRCP,SMC,SMCMAX,SMCWLT,                 &
                         SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,SHDFAC,      &
                         SBETA,Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,EMISSI,    &
                         SSOIL,                                         &
                         STC,EPSCA,BEXP,PC,RCH,RR,CFACTR,               &
                         SH2O,SLOPE,KDT,FRZFACT,PSISAT,ZSOIL,           &
                         DKSAT,DWSAT,TBOT,ZBOT,RUNOFF1,RUNOFF2,         &
                         RUNOFF3,EDIR,EC,ET,ETT,NROOT,RTDIS,            &
                         QUARTZ,FXEXP,CSOIL,                            &
                         BETA,DRIP,DEW,FLX1,FLX3,VEGTYP,ISURBAN,        &
                         SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND    &
                        ,XSDA_QFX,QFX_PHY,XQNORM,fasdas,HCPCT_FASDAS    &
                        ,IRRIGATION_CHANNEL    ) !fasdas

! ----------------------------------------------------------------------
! SUBROUTINE NOPAC
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES AND UPDATE SOIL MOISTURE
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN NO SNOW PACK IS
! PRESENT.
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: OPT_THCND
      INTEGER, INTENT(IN)  :: NROOT,NSOIL,VEGTYP,SOILTYP
      INTEGER, INTENT(IN)  :: ISURBAN
      INTEGER              :: K

      REAL, INTENT(IN)     :: BEXP,CFACTR, CMCMAX,CSOIL,DKSAT,DT,DWSAT, &
                              EPSCA,ETP,FDOWN,F1,FXEXP,FRZFACT,KDT,PC,  &
                              PRCP,PSISAT,Q2,QUARTZ,RCH,RR,SBETA,SFCTMP,&
                              SHDFAC,SLOPE,SMCDRY,SMCMAX,SMCREF,SMCWLT, &
                              T24,TBOT,TH2,ZBOT,EMISSI
      REAL, INTENT(INOUT)  :: CMC,BETA,T1
      REAL, INTENT(OUT)    :: DEW,DRIP,EC,EDIR,ETA,ETT,FLX1,FLX3,       &
                              RUNOFF1,RUNOFF2,RUNOFF3,SSOIL
!DJG NDHMS/WRF-Hydro edit...
      REAL, INTENT(INOUT)  :: SFHEAD1RT,INFXS1RT,ETPND1

      REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: RTDIS,ZSOIL
      REAL, DIMENSION(1:NSOIL),INTENT(OUT)    :: ET
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC,SH2O,STC
      REAL, DIMENSION(1:NSOIL) :: ET1
      REAL                 :: EC1,EDIR1,ETT1,DF1,ETA1,ETP1,PRCP1,YY,    &
                              YYNUM,ZZ1
!
! FASDAS
!
      REAL                       ::  XSDA_QFX, QFX_PHY, XQNORM
      INTEGER                    ::  fasdas
      REAL , DIMENSION(1:NSOIL)  ::  EFT(NSOIL), wetty(1:NSOIL)
      REAL                       ::  EFDIR, EFC, EALL_now
      REAL, INTENT(  OUT)        ::  HCPCT_FASDAS
      REAL, INTENT(IN),OPTIONAL  ::  IRRIGATION_CHANNEL
!
! END FASDAS
!
! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! CONVERT ETP Fnd PRCP FROM KG M-2 S-1 TO M S-1 AND INITIALIZE DEW.
! ----------------------------------------------------------------------
      PRCP1 = PRCP * 0.001
      ETP1 = ETP * 0.001
      DEW = 0.0
! ----------------------------------------------------------------------
! INITIALIZE EVAP TERMS.
! ----------------------------------------------------------------------
!
! FASDAS
!
      QFX_PHY = 0.0
!
! END FASDAS
!
      EDIR = 0.
      EDIR1 = 0.
      EC1 = 0.
      EC = 0.
      DO K = 1,NSOIL
        ET(K) = 0.
        ET1(K) = 0.
!
! FASDAS
!
        wetty(K) = 1.0
!
! END FASDAS
!
      END DO
      ETT = 0.
      ETT1 = 0.

!DJG NDHMS/WRF-Hydro edit...
      ETPND1 = 0.


      IF (ETP > 0.0) THEN
         CALL EVAPO (ETA1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,                  &
                      SH2O,                                             &
                      SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,                &
                      SMCREF,SHDFAC,CMCMAX,                             &
                      SMCDRY,CFACTR,                                    &
                       EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,FXEXP,  &
                      SFHEAD1RT,ETPND1 )
!
! FASDAS
!
      IF( fasdas == 1 ) THEN
        DO K=1,NSOIL
        QFX_PHY = QFX_PHY + ET1(K)   ! m/s
! dont add moisture fluxes if soil moisture is = or > smcref
        IF(SMC(K).GE.SMCREF.and.XSDA_QFX.gt.0.0) wetty(K)=0.0
        END DO
       QFX_PHY = EDIR1+EC1+QFX_PHY    ! m/s
       EALL_now = QFX_PHY           ! m/s
       QFX_PHY = QFX_PHY*1000.0     ! Kg/m2/s

       if(EALL_now.ne.0.0) then
       EFDIR = (EDIR1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
       EFDIR = EFDIR * wetty(1)
          !TWG2015 Bugfix Flip Sign to conform to Net upward Flux
           EDIR1 = EDIR1 + EFDIR    ! new value
       
       EFC = (EC1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
       !TWG2015 Bugfix Flip Sign to conform to Net upward Flux
       EC1 = EC1 + EFC         ! new value


       DO K=1,NSOIL
        EFT(K) = (ET1(K)/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFT(K) =  EFT(K) * wetty(K)
        !TWG2015 Bugfix Flip Sign to conform to Net upward Flux
        ET1(K) = ET1(K) + EFT(K) ! new value
       END DO


       END IF ! for non-zero eall_now
      ELSE
        QFX_PHY = 0.0
      ENDIF
!
! END FASDAS
!
         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                      SH2O,SLOPE,KDT,FRZFACT,                           &
                      SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                      SHDFAC,CMCMAX,                                    &
                      RUNOFF1,RUNOFF2,RUNOFF3,                          &
                      EDIR1,EC1,ET1,                                    &
                      DRIP, SFHEAD1RT,INFXS1RT,IRRIGATION_CHANNEL)

! ----------------------------------------------------------------------
! CONVERT MODELED EVAPOTRANSPIRATION FROM  M S-1  TO  KG M-2 S-1.
! ----------------------------------------------------------------------

         ETA = ETA1 * 1000.0

! ----------------------------------------------------------------------
! IF ETP < 0, ASSUME DEW FORMS (TRANSFORM ETP1 INTO DEW AND REINITIALIZE
! ETP1 TO ZERO).
! ----------------------------------------------------------------------
      ELSE
         DEW = - ETP1

! ----------------------------------------------------------------------
! CONVERT PRCP FROM 'KG M-2 S-1' TO 'M S-1' AND ADD DEW AMOUNT.
! ----------------------------------------------------------------------

         PRCP1 = PRCP1+ DEW
!
! FASDAS
!
     IF( fasdas == 1 ) THEN
       DO K=1,NSOIL
        QFX_PHY = QFX_PHY + ET1(K)   ! m/s
! dont add moisture fluxes if soil moisture is = or > smcref
        IF(SMC(K).GE.SMCREF.and.XSDA_QFX.gt.0.0) wetty(K)=0.0
       END DO
       QFX_PHY = EDIR1+EC1+QFX_PHY    ! m/s
        EALL_now = QFX_PHY     ! m/s
       QFX_PHY = QFX_PHY*1000.0    ! Kg/m2/s

       IF(EALL_now.ne.0.0) then
       EFDIR = (EDIR1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFDIR =  EFDIR * wetty(1)
          !TWG2015 Bugfix Flip Sign to conform to Net Upward Flux
           EDIR1 = EDIR1 + EFDIR    ! new value

        EFC = (EC1/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        !TWG2015 Bugfix Flip Sign to conform to Net Upward Flux
        EC1 = EC1+ EFC         ! new value

       DO K=1,NSOIL
        EFT(K) = (ET1(K)/EALL_now)*XSDA_QFX*1.0E-03*XQNORM
        EFT(K) = EFT(K) * wetty(K)
        !TWG2015 Bugfix Flip Sign to conform to Net Upward Flux
        ET1(K) = ET1(K) + EFT(K)   ! new value
       END DO

        END IF ! for non-zero eall_now
      ELSE
        QFX_PHY = 0.0
      ENDIF
!
! END FASDAS
!
         CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                      SH2O,SLOPE,KDT,FRZFACT,                           &
                      SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                      SHDFAC,CMCMAX,                                    &
                      RUNOFF1,RUNOFF2,RUNOFF3,                          &
                      EDIR1,EC1,ET1,                                    &
                      DRIP, SFHEAD1RT,INFXS1RT,IRRIGATION_CHANNEL)

! ----------------------------------------------------------------------
! CONVERT MODELED EVAPOTRANSPIRATION FROM 'M S-1' TO 'KG M-2 S-1'.
! ----------------------------------------------------------------------
!         ETA = ETA1 * 1000.0
      END IF

! ----------------------------------------------------------------------
! BASED ON ETP AND E VALUES, DETERMINE BETA
! ----------------------------------------------------------------------

      IF ( ETP <= 0.0 ) THEN
         BETA = 0.0
         ETA = ETP
         IF ( ETP < 0.0 ) THEN
            BETA = 1.0
         END IF
      ELSE
         BETA = ETA / ETP
      END IF

! ----------------------------------------------------------------------
! CONVERT MODELED EVAPOTRANSPIRATION COMPONENTS 'M S-1' TO 'KG M-2 S-1'.
! ----------------------------------------------------------------------
      EDIR = EDIR1*1000.
      EC = EC1*1000.
      DO K = 1,NSOIL
        ET(K) = ET1(K)*1000.
      END DO
      ETT = ETT1*1000.

! ----------------------------------------------------------------------
! GET SOIL THERMAL DIFFUXIVITY/CONDUCTIVITY FOR TOP SOIL LYR,
! CALC. ADJUSTED TOP LYR SOIL TEMP AND ADJUSTED SOIL FLUX, THEN
! CALL SHFLX TO COMPUTE/UPDATE SOIL HEAT FLUX AND SOIL TEMPS.
! ----------------------------------------------------------------------

      CALL TDFCND (DF1,SMC (1),QUARTZ,SMCMAX,SH2O (1),BEXP, PSISAT, SOILTYP, OPT_THCND)

!urban
      IF ( VEGTYP == ISURBAN ) DF1=3.24
!

! ----------------------------------------------------------------------
! VEGETATION GREENNESS FRACTION REDUCTION IN SUBSURFACE HEAT FLUX
! VIA REDUCTION FACTOR, WHICH IS CONVENIENT TO APPLY HERE TO THERMAL
! DIFFUSIVITY THAT IS LATER USED IN HRT TO COMPUTE SUB SFC HEAT FLUX
! (SEE ADDITIONAL COMMENTS ON VEG EFFECT SUB-SFC HEAT FLX IN
! ROUTINE SFLX)
! ----------------------------------------------------------------------
      DF1 = DF1 * EXP (SBETA * SHDFAC)
! ----------------------------------------------------------------------
! COMPUTE INTERMEDIATE TERMS PASSED TO ROUTINE HRT (VIA ROUTINE
! SHFLX BELOW) FOR USE IN COMPUTING SUBSURFACE HEAT FLUX IN HRT
! ----------------------------------------------------------------------
      YYNUM = FDOWN - EMISSI*SIGMA * T24
      YY = SFCTMP + (YYNUM / RCH + TH2- SFCTMP - BETA * EPSCA) / RR

      ZZ1 = DF1 / ( -0.5 * ZSOIL (1) * RCH * RR ) + 1.0

!urban
      CALL SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL,       &
                  TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,            &
                  QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND        &
                 ,HCPCT_FASDAS                                         ) !fasdas

! ----------------------------------------------------------------------
! SET FLX1 AND FLX3 (SNOPACK PHASE CHANGE HEAT FLUXES) TO ZERO SINCE
! THEY ARE NOT USED HERE IN SNOPAC.  FLX2 (FREEZING RAIN HEAT FLUX) WAS
! SIMILARLY INITIALIZED IN THE PENMAN ROUTINE.
! ----------------------------------------------------------------------
      FLX1 = CPH2O * PRCP * (T1- SFCTMP)
      FLX3 = 0.0

! ----------------------------------------------------------------------
  END SUBROUTINE NOPAC
! ----------------------------------------------------------------------

      SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,T2V,TH2,PRCP,FDOWN,T24,SSOIL, &
     &                   Q2,Q2SAT,ETP,RCH,EPSCA,RR,SNOWNG,FRZGRA,       &
     &                   DQSDT2,FLX2,EMISSI_IN,SNEQV,T1,SNCOVR,AOASIS,  &
		         ALBEDO,SOLDN,FVB,GAMA,STC1,ETPN,FLX4,UA_PHYS)

! ----------------------------------------------------------------------
! SUBROUTINE PENMAN
! ----------------------------------------------------------------------
! CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.  VARIOUS
! PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE
! CALLING ROUTINE FOR LATER USE.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL, INTENT(IN)     :: SNOWNG, FRZGRA
      REAL, INTENT(IN)        :: CH, DQSDT2,FDOWN,PRCP,                 &
                                 Q2, Q2SAT,SSOIL, SFCPRS, SFCTMP,       &
                                 T2V, TH2,EMISSI_IN,SNEQV,AOASIS
      REAL, INTENT(IN)        :: T1 , SNCOVR
      REAL, INTENT(IN)        :: ALBEDO,SOLDN,FVB,GAMA,STC1
      LOGICAL, INTENT(IN)     :: UA_PHYS
!
      REAL, INTENT(OUT)       :: EPSCA,ETP,FLX2,RCH,RR,T24
      REAL, INTENT(OUT)       :: FLX4,ETPN
      REAL                    :: A, DELTA, FNET,RAD,RHO,EMISSI,ELCP1,LVS
      REAL                    :: TOTABS,UCABS,SIGNCK,FNETN,RADN,EPSCAN

      REAL, PARAMETER      :: ELCP = 2.4888E+3, LSUBC = 2.501000E+6,CP = 1004.6
      REAL, PARAMETER      :: LSUBS = 2.83E+6
      REAL, PARAMETER      :: ALGDSN = 0.5, ALVGSN = 0.13

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
! ----------------------------------------------------------------------
        EMISSI=EMISSI_IN
        ELCP1  = (1.0-SNCOVR)*ELCP  + SNCOVR*ELCP*LSUBS/LSUBC
        LVS    = (1.0-SNCOVR)*LSUBC + SNCOVR*LSUBS

      FLX2 = 0.0
!      DELTA = ELCP * DQSDT2
      DELTA = ELCP1 * DQSDT2
      T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP
!      RR = T24 * 6.48E-8 / (SFCPRS * CH) + 1.0
      RR = EMISSI*T24 * 6.48E-8 / (SFCPRS * CH) + 1.0
      RHO = SFCPRS / (RD * T2V)

! ----------------------------------------------------------------------
! ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
! EFFECTS CAUSED BY FALLING PRECIPITATION.
! ----------------------------------------------------------------------
      RCH = RHO * CP * CH
      IF (.NOT. SNOWNG) THEN
         IF (PRCP >  0.0) RR = RR + CPH2O * PRCP / RCH
      ELSE
         RR = RR + CPICE * PRCP / RCH
      END IF

! ----------------------------------------------------------------------
! INCLUDE THE LATENT HEAT EFFECTS OF FRZNG RAIN CONVERTING TO ICE ON
! IMPACT IN THE CALCULATION OF FLX2 AND FNET.
! ----------------------------------------------------------------------
!      FNET = FDOWN - SIGMA * T24- SSOIL
      FNET = FDOWN -  EMISSI*SIGMA * T24- SSOIL

      FLX4 = 0.0
      IF(UA_PHYS) THEN
        IF(SNEQV > 0. .AND. FNET > 0. .AND. SOLDN > 0. ) THEN
         TOTABS = (1.-ALBEDO)*SOLDN*FVB           ! solar radiation absorbed 
                                                  ! by vegetated fraction
         UCABS = MIN(TOTABS,((1.0-ALGDSN)*(1.0-ALVGSN)*SOLDN*GAMA)*FVB)
!         print*,'penman',UCABS,TOTABS,SOLDN,GAMA,FVB
!         UCABS = MIN(TOTABS,(0.44*SOLDN*GAMA)*FVB)  
                                                  ! UCABS -> solar radiation
						  ! absorbed under canopy
         FLX4 = MIN(TOTABS - UCABS, MIN(250., 0.5*(1.-ALBEDO)*SOLDN))
        ENDIF

        SIGNCK = (STC1-273.15)*(SFCTMP-273.15)

        IF(FLX4 > 0. .AND. (SIGNCK <= 0. .OR. STC1 < 273.15)) THEN
          IF(FNET >= FLX4) THEN
           FNETN = FNET - FLX4
          ELSE
           FLX4 = FNET
           FNETN = 0.
          ENDIF
        ELSE
          FLX4 = 0.0
          FNETN = 0.
        ENDIF
      ENDIF

      IF (FRZGRA) THEN
         FLX2 = - LSUBF * PRCP
         FNET = FNET - FLX2
         IF(UA_PHYS) FNETN = FNETN - FLX2
! ----------------------------------------------------------------------
! FINISH PENMAN EQUATION CALCULATIONS.
! ----------------------------------------------------------------------
      END IF
      RAD = FNET / RCH + TH2- SFCTMP
!      A = ELCP * (Q2SAT - Q2)
      A = ELCP1 * (Q2SAT - Q2)
      EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)
! Fei-Mike
      IF (EPSCA>0.) EPSCA = EPSCA * AOASIS
!      ETP = EPSCA * RCH / LSUBC
      ETP = EPSCA * RCH / LVS

      IF(UA_PHYS) THEN
        RADN = FNETN / RCH + TH2- SFCTMP
        EPSCAN = (A * RR + RADN * DELTA) / (DELTA + RR)
        ETPN = EPSCAN * RCH / LVS
      END IF
! ----------------------------------------------------------------------
  END SUBROUTINE PENMAN
! ----------------------------------------------------------------------

      SUBROUTINE REDPRM (VEGTYP,SOILTYP,SLOPETYP,CFACTR,CMCMAX,RSMAX,      &
                         TOPT,                                             &
                         REFKDT,KDT,SBETA, SHDFAC,RSMIN,RGL,HS,ZBOT,FRZX,  &
                         PSISAT,SLOPE,SNUP,SALP,BEXP,DKSAT,DWSAT,          &
                         SMCMAX,SMCWLT,SMCREF,SMCDRY,F1,QUARTZ,FXEXP,      &
                         RTDIS,SLDPTH,ZSOIL, NROOT,NSOIL,CZIL,             &
                         LAIMIN, LAIMAX, EMISSMIN, EMISSMAX, ALBEDOMIN,    &
                         ALBEDOMAX, Z0MIN, Z0MAX, CSOIL, PTU, LLANDUSE,    &
                         LSOIL, LOCAL,LVCOEF,ZTOPV,ZBOTV)

      IMPLICIT NONE
! ----------------------------------------------------------------------
! Internally set (default valuess)
! all soil and vegetation parameters required for the execusion oF
! the Noah lsm are defined in VEGPARM.TBL, SOILPARM.TB, and GENPARM.TBL.
! ----------------------------------------------------------------------
!     Vegetation parameters:
!             ALBBRD: SFC background snow-free albedo
!             CMXTBL: MAX CNPY Capacity
!              Z0BRD: Background roughness length
!             SHDFAC: Green vegetation fraction
!              NROOT: Rooting depth
!              RSMIN: Mimimum stomatal resistance
!              RSMAX: Max. stomatal resistance
!                RGL: Parameters used in radiation stress function
!                 HS: Parameter used in vapor pressure deficit functio
!               TOPT: Optimum transpiration air temperature.
!             CMCMAX: Maximum canopy water capacity
!             CFACTR: Parameter used in the canopy inteception calculation
!               SNUP: Threshold snow depth (in water equivalent m) that
!                     implies 100 percent snow cover
!                LAI: Leaf area index
!
! ----------------------------------------------------------------------
!      Soil parameters:
!        SMCMAX: MAX soil moisture content (porosity)
!        SMCREF: Reference soil moisture  (field capacity)
!        SMCWLT: Wilting point soil moisture
!        SMCWLT: Air dry soil moist content limits
!       SSATPSI: SAT (saturation) soil potential
!         DKSAT: SAT soil conductivity
!          BEXP: B parameter
!        SSATDW: SAT soil diffusivity
!           F1: Soil thermal diffusivity/conductivity coef.
!        QUARTZ: Soil quartz content
!  Modified by F. Chen (12/22/97)  to use the STATSGO soil map
!  Modified By F. Chen (01/22/00)  to include PLaya, Lava, and White San
!  Modified By F. Chen (08/05/02)  to include additional parameters for the Noah
! NOTE: SATDW = BB*SATDK*(SATPSI/MAXSMC)
!         F11 = ALOG10(SATPSI) + BB*ALOG10(MAXSMC) + 2.0
!       REFSMC1=MAXSMC*(5.79E-9/SATDK)**(1/(2*BB+3)) 5.79E-9 m/s= 0.5 mm
!       REFSMC=REFSMC1+1./3.(MAXSMC-REFSMC1)
!       WLTSMC1=MAXSMC*(200./SATPSI)**(-1./BB)    (Wetzel and Chang, 198
!       WLTSMC=WLTSMC1-0.5*WLTSMC1
! Note: the values for playa is set for it to have a thermal conductivit
! as sand and to have a hydrulic conductivity as clay
!
! ----------------------------------------------------------------------
! Class parameter 'SLOPETYP' was included to estimate linear reservoir
! coefficient 'SLOPE' to the baseflow runoff out of the bottom layer.
! lowest class (slopetyp=0) means highest slope parameter = 1.
! definition of slopetyp from 'zobler' slope type:
! slope class  percent slope
! 1            0-8
! 2            8-30
! 3            > 30
! 4            0-30
! 5            0-8 & > 30
! 6            8-30 & > 30
! 7            0-8, 8-30, > 30
! 9            GLACIAL ICE
! BLANK        OCEAN/SEA
!       SLOPE_DATA: linear reservoir coefficient
!       SBETA_DATA: parameter used to caluculate vegetation effect on soil heat
!       FXEXP_DAT:  soil evaporation exponent used in DEVAP
!       CSOIL_DATA: soil heat capacity [J M-3 K-1]
!       SALP_DATA: shape parameter of  distribution function of snow cover
!       REFDK_DATA and REFKDT_DATA: parameters in the surface runoff parameteriz
!       FRZK_DATA: frozen ground parameter
!       ZBOT_DATA: depth[M] of lower boundary soil temperature
!       CZIL_DATA: calculate roughness length of heat
!       SMLOW_DATA and MHIGH_DATA: two soil moisture wilt, soil moisture referen
!                 parameters
! Set maximum number of soil-, veg-, and slopetyp in data statement.
! ----------------------------------------------------------------------
      INTEGER, PARAMETER     :: MAX_SLOPETYP=30,MAX_SOILTYP=30,MAX_VEGTYP=30
      LOGICAL                :: LOCAL
      CHARACTER (LEN=256), INTENT(IN)::  LLANDUSE, LSOIL

! Veg parameters
      INTEGER, INTENT(IN)    :: VEGTYP
      INTEGER, INTENT(OUT)   :: NROOT
      REAL, INTENT(INOUT)    :: SHDFAC
      REAL, INTENT(OUT)      :: HS,RSMIN,RGL,SNUP,                          &
                                CMCMAX,RSMAX,TOPT,                          &
                                EMISSMIN,  EMISSMAX,                        &
                                LAIMIN,    LAIMAX,                          &
                                Z0MIN,     Z0MAX,                           &
                                ALBEDOMIN, ALBEDOMAX, ZTOPV, ZBOTV
! Soil parameters
      INTEGER, INTENT(IN)    :: SOILTYP
      REAL, INTENT(OUT)      :: BEXP,DKSAT,DWSAT,F1,QUARTZ,SMCDRY,          &
                                SMCMAX,SMCREF,SMCWLT,PSISAT
! General parameters
      INTEGER, INTENT(IN)    :: SLOPETYP,NSOIL
      INTEGER                :: I

      REAL,    INTENT(OUT)   :: SLOPE,CZIL,SBETA,FXEXP,                     &
                                CSOIL,SALP,FRZX,KDT,CFACTR,      &
                                ZBOT,REFKDT,PTU
      REAL,    INTENT(OUT)   :: LVCOEF
      REAL,DIMENSION(1:NSOIL),INTENT(IN) :: SLDPTH,ZSOIL
      REAL,DIMENSION(1:NSOIL),INTENT(OUT):: RTDIS
      REAL                   :: FRZFACT,FRZK,REFDK

!      SAVE
! ----------------------------------------------------------------------
!
               IF (SOILTYP .gt. SLCATS) THEN
                        FATAL_ERROR( 'Warning: too many input soil types' )
               END IF
               IF (VEGTYP .gt. LUCATS) THEN
                     FATAL_ERROR( 'Warning: too many input landuse types' )
               END IF
               IF (SLOPETYP .gt. SLPCATS) THEN
                     FATAL_ERROR( 'Warning: too many input slope types' )
               END IF

! ----------------------------------------------------------------------
!  SET-UP SOIL PARAMETERS
! ----------------------------------------------------------------------
      CSOIL = CSOIL_DATA
      BEXP = BB (SOILTYP)
      DKSAT = SATDK (SOILTYP)
      DWSAT = SATDW (SOILTYP)
      F1 = F11 (SOILTYP)
      PSISAT = SATPSI (SOILTYP)
      QUARTZ = QTZ (SOILTYP)
      SMCDRY = DRYSMC (SOILTYP)
      SMCMAX = MAXSMC (SOILTYP)
      SMCREF = REFSMC (SOILTYP)
      SMCWLT = WLTSMC (SOILTYP)
! ----------------------------------------------------------------------
! Set-up universal parameters (not dependent on SOILTYP, VEGTYP or
! SLOPETYP)
! ----------------------------------------------------------------------
      ZBOT = ZBOT_DATA
      SALP = SALP_DATA
      SBETA = SBETA_DATA
      REFDK = REFDK_DATA
      FRZK = FRZK_DATA
      FXEXP = FXEXP_DATA
      REFKDT = REFKDT_DATA
      PTU = 0.    ! (not used yet) to satisify intent(out)
      KDT = REFKDT * DKSAT / REFDK
      CZIL = CZIL_DATA
      SLOPE = SLOPE_DATA (SLOPETYP)
      LVCOEF = LVCOEF_DATA

! ----------------------------------------------------------------------
! TO ADJUST FRZK PARAMETER TO ACTUAL SOIL TYPE: FRZK * FRZFACT
! ----------------------------------------------------------------------
      FRZFACT = (SMCMAX / SMCREF) * (0.412 / 0.468)
      FRZX = FRZK * FRZFACT

! ----------------------------------------------------------------------
! SET-UP VEGETATION PARAMETERS
! ----------------------------------------------------------------------
      TOPT = TOPT_DATA
      CMCMAX = CMCMAX_DATA
      CFACTR = CFACTR_DATA
      RSMAX = RSMAX_DATA
      NROOT = NROTBL (VEGTYP)
      SNUP = SNUPTBL (VEGTYP)
      RSMIN = RSTBL (VEGTYP)
      RGL = RGLTBL (VEGTYP)
      HS = HSTBL (VEGTYP)
      EMISSMIN  = EMISSMINTBL  (VEGTYP)
      EMISSMAX  = EMISSMAXTBL  (VEGTYP)
      LAIMIN    = LAIMINTBL    (VEGTYP)
      LAIMAX    = LAIMAXTBL    (VEGTYP)
      Z0MIN     = Z0MINTBL     (VEGTYP)
      Z0MAX     = Z0MAXTBL     (VEGTYP)
      ALBEDOMIN = ALBEDOMINTBL (VEGTYP)
      ALBEDOMAX = ALBEDOMAXTBL (VEGTYP)
      ZTOPV     = ZTOPVTBL     (VEGTYP)
      ZBOTV     = ZBOTVTBL     (VEGTYP)

               IF (VEGTYP .eq. BARE) SHDFAC = 0.0
               IF (NROOT .gt. NSOIL) THEN
                  WRITE (err_message,*) 'Error: too many root layers ',  &
                                                 NSOIL,NROOT
                  FATAL_ERROR( err_message )
! ----------------------------------------------------------------------
! CALCULATE ROOT DISTRIBUTION.  PRESENT VERSION ASSUMES UNIFORM
! DISTRIBUTION BASED ON SOIL LAYER DEPTHS.
! ----------------------------------------------------------------------
               END IF
               DO I = 1,NROOT
                  RTDIS (I) = - SLDPTH (I)/ ZSOIL (NROOT)
! ----------------------------------------------------------------------
!  SET-UP SLOPE PARAMETER
! ----------------------------------------------------------------------
               END DO

!        print*,'end of PRMRED'
!       print*,'VEGTYP',VEGTYP,'SOILTYP',SOILTYP,'SLOPETYP',SLOPETYP,    &
!    & 'CFACTR',CFACTR,'CMCMAX',CMCMAX,'RSMAX',RSMAX,'TOPT',TOPT,        &
!    & 'REFKDT',REFKDT,'KDT',KDT,'SBETA',SBETA, 'SHDFAC',SHDFAC,         &
!    &  'RSMIN',RSMIN,'RGL',RGL,'HS',HS,'ZBOT',ZBOT,'FRZX',FRZX,         &
!    &  'PSISAT',PSISAT,'SLOPE',SLOPE,'SNUP',SNUP,'SALP',SALP,'BEXP',    &
!    &   BEXP,                                                           &
!    &  'DKSAT',DKSAT,'DWSAT',DWSAT,                                     &
!    &  'SMCMAX',SMCMAX,'SMCWLT',SMCWLT,'SMCREF',SMCREF,'SMCDRY',SMCDRY, &
!    &  'F1',F1,'QUARTZ',QUARTZ,'FXEXP',FXEXP,                           &
!    &  'RTDIS',RTDIS,'SLDPTH',SLDPTH,'ZSOIL',ZSOIL, 'NROOT',NROOT,      &
!    &  'NSOIL',NSOIL,'Z0',Z0,'CZIL',CZIL,'LAI',LAI,                     &
!    &  'CSOIL',CSOIL,'PTU',PTU,                                         &
!    &  'LOCAL', LOCAL

      END  SUBROUTINE REDPRM

      SUBROUTINE ROSR12 (P,A,B,C,D,DELTA,NSOIL)

! ----------------------------------------------------------------------
! SUBROUTINE ROSR12
! ----------------------------------------------------------------------
! INVERT (SOLVE) THE TRI-DIAGONAL MATRIX PROBLEM SHOWN BELOW:
! ###                                            ### ###  ###   ###  ###
! #B(1), C(1),  0  ,  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! #A(2), B(2), C(2),  0  ,  0  ,   . . .  ,    0   # #      #   #      #
! # 0  , A(3), B(3), C(3),  0  ,   . . .  ,    0   # #      #   # D(3) #
! # 0  ,  0  , A(4), B(4), C(4),   . . .  ,    0   # # P(4) #   # D(4) #
! # 0  ,  0  ,  0  , A(5), B(5),   . . .  ,    0   # # P(5) #   # D(5) #
! # .                                          .   # #  .   # = #   .  #
! # .                                          .   # #  .   #   #   .  #
! # .                                          .   # #  .   #   #   .  #
! # 0  , . . . , 0 , A(M-2), B(M-2), C(M-2),   0   # #P(M-2)#   #D(M-2)#
! # 0  , . . . , 0 ,   0   , A(M-1), B(M-1), C(M-1)# #P(M-1)#   #D(M-1)#
! # 0  , . . . , 0 ,   0   ,   0   ,  A(M) ,  B(M) # # P(M) #   # D(M) #
! ###                                            ### ###  ###   ###  ###
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: NSOIL
      INTEGER               :: K, KK

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: A, B, D
      REAL, DIMENSION(1:NSOIL),INTENT(INOUT):: C,P,DELTA

! ----------------------------------------------------------------------
! INITIALIZE EQN COEF C FOR THE LOWEST SOIL LAYER
! ----------------------------------------------------------------------
      C (NSOIL) = 0.0
      P (1) = - C (1) / B (1)
! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR THE 1ST SOIL LAYER
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! SOLVE THE COEFS FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
      DELTA (1) = D (1) / B (1)
      DO K = 2,NSOIL
         P (K) = - C (K) * ( 1.0 / (B (K) + A (K) * P (K -1)) )
         DELTA (K) = (D (K) - A (K)* DELTA (K -1))* (1.0/ (B (K) + A (K)&
                    * P (K -1)))
      END DO
! ----------------------------------------------------------------------
! SET P TO DELTA FOR LOWEST SOIL LAYER
! ----------------------------------------------------------------------
      P (NSOIL) = DELTA (NSOIL)

! ----------------------------------------------------------------------
! ADJUST P FOR SOIL LAYERS 2 THRU NSOIL
! ----------------------------------------------------------------------
      DO K = 2,NSOIL
         KK = NSOIL - K + 1
         P (KK) = P (KK) * P (KK +1) + DELTA (KK)
      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE ROSR12
! ----------------------------------------------------------------------


      SUBROUTINE SHFLX (SSOIL,STC,SMC,SMCMAX,NSOIL,T1,DT,YY,ZZ1,ZSOIL, &
                         TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,     &
                         QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND &
                        ,HCPCT_FASDAS                                  ) ! fasdas

! ----------------------------------------------------------------------
! SUBROUTINE SHFLX
! ----------------------------------------------------------------------
! UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
! DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
! ON THE TEMPERATURE.
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: OPT_THCND
      INTEGER, INTENT(IN)   :: NSOIL, VEGTYP, ISURBAN, SOILTYP
      INTEGER               :: I

      REAL, INTENT(IN)      :: BEXP,CSOIL,DF1,DT,F1,PSISAT,QUARTZ,     &
                               SMCMAX, SMCWLT, TBOT,YY, ZBOT,ZZ1
      REAL, INTENT(INOUT)   :: T1
      REAL, INTENT(OUT)     :: SSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SMC,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SH2O
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
      REAL, DIMENSION(1:NSOIL)             :: AI, BI, CI, STCF,RHSTS
      REAL, PARAMETER       :: T0 = 273.15

!
! FASDAS
!
      REAL, INTENT(  OUT)     :: HCPCT_FASDAS
!
! END FASDAS
!
! ----------------------------------------------------------------------
! HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
! ----------------------------------------------------------------------

      ! Land case

      CALL HRT (RHSTS,STC,SMC,SMCMAX,NSOIL,ZSOIL,YY,ZZ1,TBOT,     &
                ZBOT,PSISAT,SH2O,DT,BEXP,SOILTYP,OPT_THCND,       &
                F1,DF1,QUARTZ,CSOIL,AI,BI,CI,VEGTYP,ISURBAN       &
               ,HCPCT_FASDAS                                      ) !fasdas

      CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

      DO I = 1,NSOIL
         STC (I) = STCF (I)
      ENDDO

! ----------------------------------------------------------------------
! IN THE NO SNOWPACK CASE (VIA ROUTINE NOPAC BRANCH,) UPDATE THE GRND
! (SKIN) TEMPERATURE HERE IN RESPONSE TO THE UPDATED SOIL TEMPERATURE
! PROFILE ABOVE.  (NOTE: INSPECTION OF ROUTINE SNOPAC SHOWS THAT T1
! BELOW IS A DUMMY VARIABLE ONLY, AS SKIN TEMPERATURE IS UPDATED
! DIFFERENTLY IN ROUTINE SNOPAC)
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! CALCULATE SURFACE SOIL HEAT FLUX
! ----------------------------------------------------------------------
      T1 = (YY + (ZZ1- 1.0) * STC (1)) / ZZ1
      SSOIL = DF1 * (STC (1) - T1) / (0.5 * ZSOIL (1))

! ----------------------------------------------------------------------
  END SUBROUTINE SHFLX
! ----------------------------------------------------------------------

      SUBROUTINE SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                   &
     &                   SH2O,SLOPE,KDT,FRZFACT,                        &
     &                   SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                &
     &                   SHDFAC,CMCMAX,                                 &
     &                   RUNOFF1,RUNOFF2,RUNOFF3,                       &
     &                   EDIR,EC,ET,                                    &
     &                   DRIP, SFHEAD1RT,INFXS1RT,                      &
                         IRRIGATION_CHANNEL )

! ----------------------------------------------------------------------
! SUBROUTINE SMFLX
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE FLUX.  THE SOIL MOISTURE CONTENT (SMC - A PER
! UNIT VOLUME MEASUREMENT) IS A DEPENDENT VARIABLE THAT IS UPDATED WITH
! PROGNOSTIC EQNS. THE CANOPY MOISTURE CONTENT (CMC) IS ALSO UPDATED.
! FROZEN GROUND VERSION:  NEW STATES ADDED: SH2O, AND FROZEN GROUND
! CORRECTION FACTOR, FRZFACT AND PARAMETER SLOPE.
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: NSOIL
      INTEGER               :: I,K

      REAL, INTENT(IN)      :: BEXP, CMCMAX, DKSAT,DWSAT, DT, EC, EDIR,  &
                               KDT, PRCP1, SHDFAC, SLOPE, SMCMAX, SMCWLT
      REAL, INTENT(OUT)                      :: DRIP, RUNOFF1, RUNOFF2, RUNOFF3
      REAL, INTENT(INOUT)   :: CMC
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ET,ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT):: SMC, SH2O
      REAL, DIMENSION(1:NSOIL)             :: AI, BI, CI, STCF,RHSTS, RHSTT, &
                                              SICE, SH2OA, SH2OFG, SH2OIN
      REAL                  :: DUMMY, EXCESS,FRZFACT,PCPDRP,RHSCT,TRHSCT
      REAL :: FAC2
      REAL :: FLIMIT
      REAL, INTENT(IN),OPTIONAL      :: IRRIGATION_CHANNEL
      REAL,    INTENT(INOUT)                 :: SFHEAD1RT,INFXS1RT

! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! COMPUTE THE RIGHT HAND SIDE OF THE CANOPY EQN TERM ( RHSCT )
! ----------------------------------------------------------------------
      DUMMY = 0.

! ----------------------------------------------------------------------
! CONVERT RHSCT (A RATE) TO TRHSCT (AN AMOUNT) AND ADD IT TO EXISTING
! CMC.  IF RESULTING AMT EXCEEDS MAX CAPACITY, IT BECOMES DRIP AND WILL
! FALL TO THE GRND.
! ----------------------------------------------------------------------
      RHSCT = SHDFAC * PRCP1- EC
      DRIP = 0.
      TRHSCT = DT * RHSCT
      EXCESS = CMC + TRHSCT

! ----------------------------------------------------------------------
! PCPDRP IS THE COMBINED PRCP1 AND DRIP (FROM CMC) THAT GOES INTO THE
! SOIL
! ----------------------------------------------------------------------
      IF (EXCESS > CMCMAX) DRIP = EXCESS - CMCMAX
      PCPDRP = (1. - SHDFAC) * PRCP1+ DRIP / DT
      IF(PRESENT(IRRIGATION_CHANNEL)) THEN
         IF (IRRIGATION_CHANNEL.NE.0.)PCPDRP =PCPDRP+ 0.001*IRRIGATION_CHANNEL !conversion of units
      END IF
! ----------------------------------------------------------------------
! STORE ICE CONTENT AT EACH SOIL LAYER BEFORE CALLING SRT and SSTEP
!
      DO I = 1,NSOIL
         SICE (I) = SMC (I) - SH2O (I)
      END DO
! ----------------------------------------------------------------------
! CALL SUBROUTINES SRT AND SSTEP TO SOLVE THE SOIL MOISTURE
! TENDENCY EQUATIONS.
! IF THE INFILTRATING PRECIP RATE IS NONTRIVIAL,
!   (WE CONSIDER NONTRIVIAL TO BE A PRECIP TOTAL OVER THE TIME STEP
!    EXCEEDING ONE ONE-THOUSANDTH OF THE WATER HOLDING CAPACITY OF
!    THE FIRST SOIL LAYER)
! THEN CALL THE SRT/SSTEP SUBROUTINE PAIR TWICE IN THE MANNER OF
!   TIME SCHEME "F" (IMPLICIT STATE, AVERAGED COEFFICIENT)
!   OF SECTION 2 OF KALNAY AND KANAMITSU (1988, MWR, VOL 116,
!   PAGES 1945-1958)TO MINIMIZE 2-DELTA-T OSCILLATIONS IN THE
!   SOIL MOISTURE VALUE OF THE TOP SOIL LAYER THAT CAN ARISE BECAUSE
!   OF THE EXTREME NONLINEAR DEPENDENCE OF THE SOIL HYDRAULIC
!   DIFFUSIVITY COEFFICIENT AND THE HYDRAULIC CONDUCTIVITY ON THE
!   SOIL MOISTURE STATE
! OTHERWISE CALL THE SRT/SSTEP SUBROUTINE PAIR ONCE IN THE MANNER OF
!   TIME SCHEME "D" (IMPLICIT STATE, EXPLICIT COEFFICIENT)
!   OF SECTION 2 OF KALNAY AND KANAMITSU
! PCPDRP IS UNITS OF KG/M**2/S OR MM/S, ZSOIL IS NEGATIVE DEPTH IN M
! ----------------------------------------------------------------------
!  According to Dr. Ken Mitchell's suggestion, add the second contraint
!  to remove numerical instability of runoff and soil moisture
!  FLIMIT is a limit value for FAC2
      FAC2=0.0
      DO I=1,NSOIL
         FAC2=MAX(FAC2,SH2O(I)/SMCMAX)
      ENDDO
      CALL FAC2MIT(SMCMAX,FLIMIT)

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! SMC STATES REPLACED BY SH2O STATES IN SRT SUBR.  SH2O & SICE STATES
! INC&UDED IN SSTEP SUBR.  FROZEN GROUND CORRECTION FACTOR, FRZFACT
! ADDED.  ALL WATER BALANCE CALCULATIONS USING UNFROZEN WATER
! ----------------------------------------------------------------------

#ifdef WRF_HYDRO
!DJG NDHMS/WRF-Hydro edit... Add previous ponded water to new precip drip...
    PCPDRP = PCPDRP + SFHEAD1RT/1000./DT   ! convert SFHEAD1RT to (m/s)
#endif


      IF ( ( (PCPDRP * DT) > (0.0001*1000.0* (- ZSOIL (1))* SMCMAX) )   &
           .OR. (FAC2 > FLIMIT) ) THEN
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                    SFHEAD1RT,INFXS1RT)
         CALL SSTEP (SH2OFG,SH2O,DUMMY,RHSTT,RHSCT,DT,NSOIL,SMCMAX,     &
                        CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)
         DO K = 1,NSOIL
            SH2OA (K) = (SH2O (K) + SH2OFG (K)) * 0.5
         END DO
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,ZSOIL,         &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                    SFHEAD1RT,INFXS1RT)
         SH2OIN=SH2O
         CALL SSTEP (SH2O,SH2OIN,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,       &
                        CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)

      ELSE
         CALL SRT (RHSTT,EDIR,ET,SH2O,SH2O,NSOIL,PCPDRP,ZSOIL,          &
                    DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,                    &
                    RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZFACT,SICE,AI,BI,CI,  &
                   SFHEAD1RT,INFXS1RT)
         SH2OIN=SH2O
         CALL SSTEP (SH2O,SH2OIN,CMC,RHSTT,RHSCT,DT,NSOIL,SMCMAX,       &
                     CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,AI,BI,CI,INFXS1RT)
!      RUNOF = RUNOFF

      END IF

! ----------------------------------------------------------------------
  END SUBROUTINE SMFLX
! ----------------------------------------------------------------------


      SUBROUTINE SNFRAC (SNEQV,SNUP,SALP,SNOWH,SNCOVR, &
                         XLAI,SHDFAC,FVB,GAMA,FBUR,    &
                         FGSN,ZTOPV,ZBOTV,UA_PHYS)

! ----------------------------------------------------------------------
! SUBROUTINE SNFRAC
! ----------------------------------------------------------------------
! CALCULATE SNOW FRACTION (0 -> 1)
! SNEQV   SNOW WATER EQUIVALENT (M)
! SNUP    THRESHOLD SNEQV DEPTH ABOVE WHICH SNCOVR=1
! SALP    TUNING PARAMETER
! SNCOVR  FRACTIONAL SNOW COVER
! ----------------------------------------------------------------------
      IMPLICIT NONE

      REAL, INTENT(IN)     :: SNEQV,SNUP,SALP,SNOWH
      REAL, INTENT(OUT)    :: SNCOVR
      REAL                 :: RSNOW, Z0N
      LOGICAL, INTENT(IN)  :: UA_PHYS  ! UA: flag for UA option
      REAL, INTENT(IN)     :: ZTOPV    ! UA: height of canopy top
      REAL, INTENT(IN)     :: ZBOTV    ! UA: height of canopy bottom
      REAL, INTENT(IN)     :: SHDFAC   ! UA: vegetation fraction
      REAL, INTENT(INOUT)  :: XLAI     ! UA: LAI modified by snow
      REAL, INTENT(OUT)    :: FVB      ! UA: frac. veg. w/snow beneath
      REAL, INTENT(OUT)    :: GAMA     ! UA: = EXP(-1.* XLAI)
      REAL, INTENT(OUT)    :: FBUR     ! UA: fraction of canopy buried
      REAL, INTENT(OUT)    :: FGSN     ! UA: ground snow cover fraction

      REAL ::  SNUPGRD = 0.02          ! UA: SWE limit for ground cover

! ----------------------------------------------------------------------
! SNUP IS VEG-CLASS DEPENDENT SNOWDEPTH THRESHHOLD (SET IN ROUTINE
! REDPRM) ABOVE WHICH SNOCVR=1.
! ----------------------------------------------------------------------
      IF (SNEQV < SNUP) THEN
         RSNOW = SNEQV / SNUP
         SNCOVR = 1. - ( EXP ( - SALP * RSNOW) - RSNOW * EXP ( - SALP))
      ELSE
         SNCOVR = 1.0
      END IF

!     FORMULATION OF DICKINSON ET AL. 1986
!     Z0N = 0.035

!        SNCOVR=SNOWH/(SNOWH + 5*Z0N)

!     FORMULATION OF MARSHALL ET AL. 1994
!        SNCOVR=SNEQV/(SNEQV + 2*Z0N)

      IF(UA_PHYS) THEN

!---------------------------------------------------------------------
! FGSN: FRACTION OF SOIL COVERED WITH SNOW
!---------------------------------------------------------------------
        IF (SNEQV < SNUPGRD) THEN
         FGSN = SNEQV / SNUPGRD
        ELSE
         FGSN = 1.0
        END IF
!------------------------------------------------------------------
! FBUR: VERTICAL FRACTION OF VEGETATION COVERED BY SNOW
! GRASS, CROP, AND SHRUB: MULTIPLY 0.4 BY ZTOPV AND ZBOTV BECAUSE 
! THEY WILL BE PRESSED DOWN BY THE SNOW.
! FOREST: DON'T NEED TO CHANGE ZTOPV AND ZBOTV.

        IF(ZBOTV > 0. .AND. SNOWH > ZBOTV) THEN
          IF(ZBOTV <= 0.5) THEN
            FBUR = (SNOWH - 0.4*ZBOTV) / (0.4*(ZTOPV-ZBOTV)) ! short veg.
          ELSE
            FBUR = (SNOWH - ZBOTV) / (ZTOPV-ZBOTV)           !  tall veg.
          ENDIF
        ELSE
          FBUR = 0.
        ENDIF

        FBUR = MIN(MAX(FBUR,0.0),1.0)

! XLAI IS ADJUSTED FOR VERTICAL BURYING BY SNOW
        XLAI = XLAI * (1.0 - FBUR)
! ----------------------------------------------------------------------
! SNOW-COVERED SOIL: (1-SHDFAC)*FGSN
! VEGETATION WITH SNOW ABOVE DUE TO BURIAL FVEG_SN_AB = SHDFAC*FBUR
! SNOW ON THE GROUND THAT CAN BE "SEEN" BY SATELLITE
! (IF XLAI GOES TO ZERO): GAMA*FVB
! Where GAMA = exp(-XLAI)
! ----------------------------------------------------------------------

! VEGETATION WITH SNOW BELOW
        FVB = SHDFAC * FGSN * (1.0 - FBUR)

! GAMA IS USED TO DIVIDE FVB INTO TWO PARTS:
! GAMA=1 FOR XLAI=0 AND GAMA=0 FOR XLAI=6
        GAMA = EXP(-1.* XLAI)
      ELSE
        ! Define intent(out) terms for .NOT. UA_PHYS case
        FVB  = 0.0
        GAMA = 0.0
        FBUR = 0.0
        FGSN = 0.0
      END IF    ! UA_PHYS

! ----------------------------------------------------------------------
  END SUBROUTINE SNFRAC
! ----------------------------------------------------------------------

      SUBROUTINE SNKSRC (TSNSR,TAVG,SMC,SH2O,ZSOIL,NSOIL,               &
     &                      SMCMAX,PSISAT,BEXP,DT,K,QTOT)
! ----------------------------------------------------------------------
! SUBROUTINE SNKSRC
! ----------------------------------------------------------------------
! CALCULATE SINK/SOURCE TERM OF THE TERMAL DIFFUSION EQUATION. (SH2O) IS
! AVAILABLE LIQUED WATER.
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: K,NSOIL
      REAL, INTENT(IN)      :: BEXP, DT, PSISAT, QTOT, SMC, SMCMAX,    &
                               TAVG
      REAL, INTENT(INOUT)   :: SH2O

      REAL, DIMENSION(1:NSOIL), INTENT(IN):: ZSOIL

      REAL                  :: DF, DZ, DZH, FREE, TSNSR,               &
                               TDN, TM, TUP, TZ, X0, XDN, XH2O, XUP

      REAL, PARAMETER       :: DH2O = 1.0000E3, HLICE = 3.3350E5,      &
                               T0 = 2.7315E2

      IF (K == 1) THEN
         DZ = - ZSOIL (1)
      ELSE
         DZ = ZSOIL (K -1) - ZSOIL (K)
      END IF
! ----------------------------------------------------------------------
! VIA FUNCTION FRH2O, COMPUTE POTENTIAL OR 'EQUILIBRIUM' UNFROZEN
! SUPERCOOLED FREE WATER FOR GIVEN SOIL TYPE AND SOIL LAYER TEMPERATURE.
! FUNCTION FRH20 INVOKES EQN (17) FROM V. KOREN ET AL (1999, JGR, VOL.
! 104, PG 19573).  (ASIDE:  LATTER EQN IN JOURNAL IN CENTIGRADE UNITS.
! ROUTINE FRH2O USE FORM OF EQN IN KELVIN UNITS.)
! ----------------------------------------------------------------------
!      FREE = FRH2O(TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)

! ----------------------------------------------------------------------
! IN NEXT BLOCK OF CODE, INVOKE EQN 18 OF V. KOREN ET AL (1999, JGR,
! VOL. 104, PG 19573.)  THAT IS, FIRST ESTIMATE THE NEW AMOUNTOF LIQUID
! WATER, 'XH2O', IMPLIED BY THE SUM OF (1) THE LIQUID WATER AT THE BEGIN
! OF CURRENT TIME STEP, AND (2) THE FREEZE OF THAW CHANGE IN LIQUID
! WATER IMPLIED BY THE HEAT FLUX 'QTOT' PASSED IN FROM ROUTINE HRT.
! SECOND, DETERMINE IF XH2O NEEDS TO BE BOUNDED BY 'FREE' (EQUIL AMT) OR
! IF 'FREE' NEEDS TO BE BOUNDED BY XH2O.
! ----------------------------------------------------------------------
      CALL FRH2O (FREE,TAVG,SMC,SH2O,SMCMAX,BEXP,PSISAT)

! ----------------------------------------------------------------------
! FIRST, IF FREEZING AND REMAINING LIQUID LESS THAN LOWER BOUND, THEN
! REDUCE EXTENT OF FREEZING, THEREBY LETTING SOME OR ALL OF HEAT FLUX
! QTOT COOL THE SOIL TEMP LATER IN ROUTINE HRT.
! ----------------------------------------------------------------------
      XH2O = SH2O + QTOT * DT / (DH2O * HLICE * DZ)
      IF ( XH2O < SH2O .AND. XH2O < FREE) THEN
         IF ( FREE > SH2O ) THEN
            XH2O = SH2O
         ELSE
            XH2O = FREE
         END IF
      END IF
! ----------------------------------------------------------------------
! SECOND, IF THAWING AND THE INCREASE IN LIQUID WATER GREATER THAN UPPER
! BOUND, THEN REDUCE EXTENT OF THAW, THEREBY LETTING SOME OR ALL OF HEAT
! FLUX QTOT WARM THE SOIL TEMP LATER IN ROUTINE HRT.
! ----------------------------------------------------------------------
      IF ( XH2O > SH2O .AND. XH2O > FREE ) THEN
         IF ( FREE < SH2O ) THEN
            XH2O = SH2O
         ELSE
            XH2O = FREE
         END IF
      END IF

! ----------------------------------------------------------------------
! CALCULATE PHASE-CHANGE HEAT SOURCE/SINK TERM FOR USE IN ROUTINE HRT
! AND UPDATE LIQUID WATER TO REFLCET FINAL FREEZE/THAW INCREMENT.
! ----------------------------------------------------------------------
!      SNKSRC = -DH2O*HLICE*DZ*(XH2O-SH2O)/DT
      IF (XH2O < 0.) XH2O = 0.
      IF (XH2O > SMC) XH2O = SMC
      TSNSR = - DH2O * HLICE * DZ * (XH2O - SH2O)/ DT
      SH2O = XH2O

! ----------------------------------------------------------------------
  END SUBROUTINE SNKSRC
! ----------------------------------------------------------------------

      SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,SMC,SMCMAX,SMCWLT,   &
                          SMCREF,SMCDRY,CMC,CMCMAX,NSOIL,DT,            &
                          SBETA,DF1,                                    &
                          Q2,T1,SFCTMP,T24,TH2,FDOWN,F1,SSOIL,STC,EPSCA,&
                         SFCPRS,BEXP,PC,RCH,RR,CFACTR,SNCOVR,ESD,SNDENS,&
                          SNOWH,SH2O,SLOPE,KDT,FRZFACT,PSISAT,          &
                          ZSOIL,DWSAT,DKSAT,TBOT,ZBOT,SHDFAC,RUNOFF1,   &
                          RUNOFF2,RUNOFF3,EDIR,EC,ET,ETT,NROOT,SNOMLT,  &
                          RTDIS,QUARTZ,FXEXP,CSOIL,                     &
                          BETA,DRIP,DEW,FLX1,FLX2,FLX3,ESNOW,ETNS,EMISSI,&
                          RIBB,SOLDN,                                   &
                          ISURBAN,                                      &
                          VEGTYP,                                       &
                          ETPN,FLX4,UA_PHYS,                            &
                          SFHEAD1RT,INFXS1RT,ETPND1,SOILTYP,OPT_THCND   &
                         ,QFX_PHY,fasdas,HCPCT_FASDAS                   ) !fasdas
! ----------------------------------------------------------------------
! SUBROUTINE SNOPAC
! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS
! PRESENT.
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER, INTENT(IN)   :: OPT_THCND
      INTEGER, INTENT(IN)   :: NROOT, NSOIL,VEGTYP,SOILTYP
      INTEGER, INTENT(IN)   :: ISURBAN
      INTEGER               :: K
!
! kmh 09/03/2006 add IT16 for surface temperature iteration
!
      INTEGER               :: IT16
      LOGICAL, INTENT(IN)   :: SNOWNG

!DJG NDHMS/WRF-Hydro edit...
       REAL, INTENT(INOUT)    ::  SFHEAD1RT,INFXS1RT,ETPND1

      REAL, INTENT(IN)      :: BEXP,CFACTR, CMCMAX,CSOIL,DF1,DKSAT,     &
                               DT,DWSAT, EPSCA,FDOWN,F1,FXEXP,          &
                               FRZFACT,KDT,PC, PRCP,PSISAT,Q2,QUARTZ,   &
                               RCH,RR,SBETA,SFCPRS, SFCTMP, SHDFAC,     &
                               SLOPE,SMCDRY,SMCMAX,SMCREF,SMCWLT, T24,  &
                               TBOT,TH2,ZBOT,EMISSI,SOLDN
      REAL, INTENT(INOUT)   :: CMC, BETA, ESD,FLX2,PRCPF,SNOWH,SNCOVR,  &
                               SNDENS, T1, RIBB, ETP
      REAL, INTENT(OUT)     :: DEW,DRIP,EC,EDIR, ETNS, ESNOW,ETT,       &
                               FLX1,FLX3, RUNOFF1,RUNOFF2,RUNOFF3,      &
                               SSOIL,SNOMLT
      REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: RTDIS,ZSOIL
      REAL, DIMENSION(1:NSOIL),INTENT(OUT)    :: ET
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: SMC,SH2O,STC
      REAL, DIMENSION(1:NSOIL) :: ET1
      REAL                  :: DENOM,DSOIL,DTOT,EC1,EDIR1,ESDFLX,ETA,   &
                               ETT1, ESNOW1, ESNOW2, ETA1,ETP1,ETP2,    &
                               ETP3, ETNS1, ETANRG, ETAX, EX, FLX3X,    &
                               FRCSNO,FRCSOI, PRCP1, QSAT,RSNOW, SEH,   &
                               SNCOND,SSOIL1, T11,T12, T12A, T12AX,     &
                               T12B, T14, YY, ZZ1
!                               T12B, T14, YY, ZZ1,EMISSI_S
!
! kmh 01/11/2007 add T15, T16, and DTOT2 for SFC T iteration and snow heat flux
!
      REAL                  :: T15, T16, DTOT2
      REAL, PARAMETER       :: ESDMIN = 1.E-6, LSUBC = 2.501000E+6,     &
                               LSUBS = 2.83E+6, TFREEZ = 273.15,        &
                               SNOEXP = 2.0
      LOGICAL, INTENT(IN)   :: UA_PHYS  ! UA: flag for UA option
      REAL, INTENT(INOUT)   :: FLX4     ! UA: energy removed by canopy
      REAL, INTENT(IN)      :: ETPN     ! UA: adjusted pot. evap. [mm/s]
      REAL                  :: ETP1N    ! UA: adjusted pot. evap. [m/s]

!
!  FASDAS
!
      REAL                  :: QFX_PHY
      INTEGER               :: fasdas
      REAL, INTENT(  OUT)   :: HCPCT_FASDAS
!
!  END FASDAS
!
! ----------------------------------------------------------------------
! EXECUTABLE CODE BEGINS HERE:
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! INITIALIZE EVAP TERMS.
! ----------------------------------------------------------------------
! conversions:
! ESNOW [KG M-2 S-1]
! ESDFLX [KG M-2 S-1] .le. ESNOW
! ESNOW1 [M S-1]
! ESNOW2 [M]
! ETP [KG M-2 S-1]
! ETP1 [M S-1]
! ETP2 [M]
! ----------------------------------------------------------------------
      DEW = 0.
      EDIR = 0.
      EDIR1 = 0.
      EC1 = 0.
      EC = 0.
!      EMISSI_S=0.95    ! For snow

      DO K = 1,NSOIL
         ET (K) = 0.
         ET1 (K) = 0.
      END DO
      ETT = 0.
      ETT1 = 0.

!DJG NDHMS/WRF-Hydro edit...
      ETPND1 = 0.


      ETNS = 0.
      ETNS1 = 0.
      ESNOW = 0.
      ESNOW1 = 0.
      ESNOW2 = 0.

! ----------------------------------------------------------------------
! CONVERT POTENTIAL EVAP (ETP) FROM KG M-2 S-1 TO ETP1 IN M S-1
! ----------------------------------------------------------------------
      PRCP1 = PRCPF *0.001
! ----------------------------------------------------------------------
! IF ETP<0 (DOWNWARD) THEN DEWFALL (=FROSTFALL IN THIS CASE).
! ----------------------------------------------------------------------
      BETA = 1.0
      IF (ETP <= 0.0) THEN
         IF ( ( RIBB >= 0.1 ) .AND. ( FDOWN > 150.0 ) ) THEN
            ETP=(MIN(ETP*(1.0-RIBB),0.)*SNCOVR/0.980 + ETP*(0.980-SNCOVR))/0.980
         ENDIF
         IF(ETP == 0.) BETA = 0.0
         ETP1 = ETP * 0.001
         IF(UA_PHYS) ETP1N = ETPN * 0.001
         DEW = -ETP1
         ESNOW2 = ETP1*DT
         ETANRG = ETP*((1.-SNCOVR)*LSUBC + SNCOVR*LSUBS)
      ELSE
         ETP1 = ETP * 0.001
         IF(UA_PHYS) ETP1N = ETPN * 0.001
         !  LAND CASE
         IF (SNCOVR <  1.) THEN
            CALL EVAPO (ETNS1,SMC,NSOIL,CMC,ETP1,DT,ZSOIL,           &
                         SH2O,                                       &
                         SMCMAX,BEXP,PC,SMCWLT,DKSAT,DWSAT,          &
                         SMCREF,SHDFAC,CMCMAX,                       &
                         SMCDRY,CFACTR,                              &
                         EDIR1,EC1,ET1,ETT1,SFCTMP,Q2,NROOT,RTDIS,   &
                         FXEXP, SFHEAD1RT,ETPND1)
! ----------------------------------------------------------------------------
            EDIR1 = EDIR1* (1. - SNCOVR)
            EC1 = EC1* (1. - SNCOVR)
            DO K = 1,NSOIL
               ET1 (K) = ET1 (K)* (1. - SNCOVR)
            END DO
            ETT1 = ETT1*(1.-SNCOVR)
!            ETNS1 = EDIR1+ EC1+ ETT1
            ETNS1 = ETNS1*(1.-SNCOVR)
! ----------------------------------------------------------------------------
            EDIR = EDIR1*1000.
            EC = EC1*1000.
            DO K = 1,NSOIL
               ET (K) = ET1 (K)*1000.
            END DO
!
! FASDAS
!
            if( fasdas ==  1 ) then
              QFX_PHY = EDIR + EC
              DO K=1,NSOIL
                 QFX_PHY = QFX_PHY + ET(K)
              END DO
            endif
!
! END FASDAS
!
            ETT = ETT1*1000.
            ETNS = ETNS1*1000.


!DJG NDHMS/WRF-Hydro edit...
               ETPND1 = ETPND1*1000.


! ----------------------------------------------------------------------

         ENDIF
         ESNOW = ETP*SNCOVR
         IF(UA_PHYS) ESNOW = ETPN*SNCOVR   ! USE ADJUSTED ETP
         ESNOW1 = ESNOW*0.001
         ESNOW2 = ESNOW1*DT
         ETANRG = ESNOW*LSUBS + ETNS*LSUBC
      ENDIF

! ----------------------------------------------------------------------
! IF PRECIP IS FALLING, CALCULATE HEAT FLUX FROM SNOW SFC TO NEWLY
! ACCUMULATING PRECIP.  NOTE THAT THIS REFLECTS THE FLUX APPROPRIATE FOR
! THE NOT-YET-UPDATED SKIN TEMPERATURE (T1).  ASSUMES TEMPERATURE OF THE
! SNOWFALL STRIKING THE GROUND IS =SFCTMP (LOWEST MODEL LEVEL AIR TEMP).
! ----------------------------------------------------------------------
      FLX1 = 0.0
      IF (SNOWNG) THEN
         FLX1 = CPICE * PRCP * (T1- SFCTMP)
      ELSE
         IF (PRCP >  0.0) FLX1 = CPH2O * PRCP * (T1- SFCTMP)
! ----------------------------------------------------------------------
! CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES
! BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.
! INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)
! FLUXES.  FLX1 FROM ABOVE, FLX2 BROUGHT IN VIA COMMOM BLOCK RITE.
! FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING T1 CALCULATED IN
! PENMAN.
! ----------------------------------------------------------------------
      END IF
      DSOIL = - (0.5 * ZSOIL (1))
      DTOT = SNOWH + DSOIL
      DENOM = 1.0+ DF1 / (DTOT * RR * RCH)
! surface emissivity weighted by snow cover fraction
!      T12A = ( (FDOWN - FLX1 - FLX2 -                                   &
!     &       ((SNCOVR*EMISSI_S)+EMISSI*(1.0-SNCOVR))*SIGMA *T24)/RCH    &
!     &       + TH2 - SFCTMP - ETANRG/RCH ) / RR
      T12A = ( (FDOWN - FLX1- FLX2- EMISSI * SIGMA * T24)/ RCH                    &
                + TH2- SFCTMP - ETANRG / RCH ) / RR

      T12B = DF1 * STC (1) / (DTOT * RR * RCH)

! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS AT OR BELOW FREEZING, NO SNOW
! MELT WILL OCCUR.  SET THE SKIN TEMP TO THIS EFFECTIVE TEMP.  REDUCE
! (BY SUBLIMINATION ) OR INCREASE (BY FROST) THE DEPTH OF THE SNOWPACK,
! DEPENDING ON SIGN OF ETP.
! UPDATE SOIL HEAT FLUX (SSOIL) USING NEW SKIN TEMPERATURE (T1)
! SINCE NO SNOWMELT, SET ACCUMULATED SNOWMELT TO ZERO, SET 'EFFECTIVE'
! PRECIP FROM SNOWMELT TO ZERO, SET PHASE-CHANGE HEAT FLUX FROM SNOWMELT
! TO ZERO.
! ----------------------------------------------------------------------
! SUB-FREEZING BLOCK
! ----------------------------------------------------------------------
      T12 = (SFCTMP + T12A + T12B) / DENOM
      IF (T12 <=  TFREEZ) THEN
         T1 = T12
         SSOIL = DF1 * (T1- STC (1)) / DTOT
!        ESD = MAX (0.0, ESD- ETP2)
         ESD = MAX(0.0, ESD-ESNOW2)
         FLX3 = 0.0
         EX = 0.0

         SNOMLT = 0.0
         IF(UA_PHYS) FLX4 = 0.0
! ----------------------------------------------------------------------
! IF THE 'EFFECTIVE SNOW-GRND SFC TEMP' IS ABOVE FREEZING, SNOW MELT
! WILL OCCUR.  CALL THE SNOW MELT RATE,EX AND AMT, SNOMLT.  REVISE THE
! EFFECTIVE SNOW DEPTH.  REVISE THE SKIN TEMP BECAUSE IT WOULD HAVE CHGD
! DUE TO THE LATENT HEAT RELEASED BY THE MELTING. CALC THE LATENT HEAT
! RELEASED, FLX3. SET THE EFFECTIVE PRECIP, PRCP1 TO THE SNOW MELT RATE,
! EX FOR USE IN SMFLX.  ADJUSTMENT TO T1 TO ACCOUNT FOR SNOW PATCHES.
! CALCULATE QSAT VALID AT FREEZING POINT.  NOTE THAT ESAT (SATURATION
! VAPOR PRESSURE) VALUE OF 6.11E+2 USED HERE IS THAT VALID AT FRZZING
! POINT.  NOTE THAT ETP FROM CALL PENMAN IN SFLX IS IGNORED HERE IN
! FAVOR OF BULK ETP OVER 'OPEN WATER' AT FREEZING TEMP.
! UPDATE SOIL HEAT FLUX (S) USING NEW SKIN TEMPERATURE (T1)
! ----------------------------------------------------------------------
! ABOVE FREEZING BLOCK
! ----------------------------------------------------------------------
      ELSE
!     From V3.9 original code (commented) replaced to allow complete melting of small snow amounts
!        T1 = TFREEZ * SNCOVR ** SNOEXP + T12 * (1.0- SNCOVR ** SNOEXP)
         T1 = TFREEZ * max(0.01,SNCOVR ** SNOEXP) + T12 * (1.0- max(0.01,SNCOVR ** SNOEXP))
         BETA = 1.0

! ----------------------------------------------------------------------
! IF POTENTIAL EVAP (SUBLIMATION) GREATER THAN DEPTH OF SNOWPACK.
! BETA<1
! SNOWPACK HAS SUBLIMATED AWAY, SET DEPTH TO ZERO.
! ----------------------------------------------------------------------
         SSOIL = DF1 * (T1- STC (1)) / DTOT
         IF (ESD-ESNOW2 <= ESDMIN) THEN
            ESD = 0.0
            EX = 0.0
            SNOMLT = 0.0
            FLX3 = 0.0
            IF(UA_PHYS) FLX4 = 0.0
! ----------------------------------------------------------------------
! SUBLIMATION LESS THAN DEPTH OF SNOWPACK
! SNOWPACK (ESD) REDUCED BY ESNOW2 (DEPTH OF SUBLIMATED SNOW)
! ----------------------------------------------------------------------
         ELSE
            ESD = ESD-ESNOW2
            ETP3 = ETP * LSUBC
            SEH = RCH * (T1- TH2)
            T14 = T1* T1
            T14 = T14* T14
!           FLX3 = FDOWN - FLX1 - FLX2 -                                 &
!                  ((SNCOVR*EMISSI_S)+EMISSI*(1-SNCOVR))*SIGMA*T14 -     &
!                    SSOIL - SEH - ETANRG
            FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL - SEH - ETANRG
            IF (FLX3 <= 0.0) FLX3 = 0.0

            IF(UA_PHYS .AND. FLX4 > 0. .AND. FLX3 > 0.) THEN
              IF(FLX3 >= FLX4) THEN
                FLX3 = FLX3 - FLX4
              ELSE
                FLX4 = FLX3
                FLX3 = 0.
              ENDIF
            ELSE
              FLX4 = 0.0
            ENDIF

! ----------------------------------------------------------------------
! SNOWMELT REDUCTION DEPENDING ON SNOW COVER
! ----------------------------------------------------------------------
            EX = FLX3*0.001/ LSUBF

! ----------------------------------------------------------------------
! ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE
! CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.
! ----------------------------------------------------------------------
            SNOMLT = EX * DT
            IF (ESD- SNOMLT >=  ESDMIN) THEN
               ESD = ESD- SNOMLT
! ----------------------------------------------------------------------
! SNOWMELT EXCEEDS SNOW DEPTH
! ----------------------------------------------------------------------
            ELSE
               EX = ESD / DT
               FLX3 = EX *1000.0* LSUBF
               SNOMLT = ESD

               ESD = 0.0
! ----------------------------------------------------------------------
! END OF 'ESD .LE. ETP2' IF-BLOCK
! ----------------------------------------------------------------------
            END IF
         END IF

! ----------------------------------------------------------------------
! END OF 'T12 .LE. TFREEZ' IF-BLOCK
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! IF NON-GLACIAL LAND, ADD SNOWMELT RATE (EX) TO PRECIP RATE TO BE USED
! IN SUBROUTINE SMFLX (SOIL MOISTURE EVOLUTION) VIA INFILTRATION.
!
! RUNOFF/BASEFLOW LATER NEAR THE END OF SFLX (AFTER RETURN FROM CALL TO
! SUBROUTINE SNOPAC)
! ----------------------------------------------------------------------
         PRCP1 = PRCP1+ EX

! ----------------------------------------------------------------------
! SET THE EFFECTIVE POTNL EVAPOTRANSP (ETP1) TO ZERO SINCE THIS IS SNOW
! CASE, SO SURFACE EVAP NOT CALCULATED FROM EDIR, EC, OR ETT IN SMFLX
! (BELOW).
! SMFLX RETURNS UPDATED SOIL MOISTURE VALUES FOR NON-GLACIAL LAND.
! ----------------------------------------------------------------------
      END IF
      CALL SMFLX (SMC,NSOIL,CMC,DT,PRCP1,ZSOIL,                      &
                   SH2O,SLOPE,KDT,FRZFACT,                           &
                   SMCMAX,BEXP,SMCWLT,DKSAT,DWSAT,                   &
                   SHDFAC,CMCMAX,                                    &
                   RUNOFF1,RUNOFF2,RUNOFF3,                          &
                   EDIR1,EC1,ET1,                                    &
                   DRIP, SFHEAD1RT,INFXS1RT)
! ----------------------------------------------------------------------
! BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO
! SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX
! MATCHES THAT ALREADY COMPUTER FOR BELOW THE SNOWPACK, THUS THE SFC
! HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE
! SNOW TOP SURFACE.  T11 IS A DUMMY ARGUEMENT SO WE WILL NOT USE THE
! SKIN TEMP VALUE AS REVISED BY SHFLX.
! ----------------------------------------------------------------------
      ZZ1 = 1.0
      YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1

! ----------------------------------------------------------------------
! SHFLX WILL CALC/UPDATE THE SOIL TEMPS.  NOTE:  THE SUB-SFC HEAT FLUX
! (SSOIL1) AND THE SKIN TEMP (T11) OUTPUT FROM THIS SHFLX CALL ARE NOT
! USED  IN ANY SUBSEQUENT CALCULATIONS. RATHER, THEY ARE DUMMY VARIABLES
! HERE IN THE SNOPAC CASE, SINCE THE SKIN TEMP AND SUB-SFC HEAT FLUX ARE
! UPDATED INSTEAD NEAR THE BEGINNING OF THE CALL TO SNOPAC.
! ----------------------------------------------------------------------
      T11 = T1
      CALL SHFLX (SSOIL1,STC,SMC,SMCMAX,NSOIL,T11,DT,YY,ZZ1,ZSOIL,     &
                   TBOT,ZBOT,SMCWLT,PSISAT,SH2O,BEXP,F1,DF1,           &
                   QUARTZ,CSOIL,VEGTYP,ISURBAN,SOILTYP,OPT_THCND       &
                  ,HCPCT_FASDAS                                        ) !fasdas

! ----------------------------------------------------------------------
! SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS
! ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.
! ----------------------------------------------------------------------
      ! LAND
      IF (ESD >  0.) THEN
         CALL SNOWPACK (ESD,DT,SNOWH,SNDENS,T1,YY,SNOMLT,UA_PHYS)
      ELSE
         ESD = 0.
         SNOWH = 0.
         SNDENS = 0.
         SNCOND = 1.
         SNCOVR = 0.
      END IF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOPAC
! ----------------------------------------------------------------------


      SUBROUTINE SNOWPACK (ESD,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL,SNOMLT,UA_PHYS)

! ----------------------------------------------------------------------
! SUBROUTINE SNOWPACK
! ----------------------------------------------------------------------
! CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW
! DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S
! DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR
! KOREN, 03/25/95.
! ----------------------------------------------------------------------
! ESD     WATER EQUIVALENT OF SNOW (M)
! DTSEC   TIME STEP (SEC)
! SNOWH   SNOW DEPTH (M)
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
! TSNOW   SNOW SURFACE TEMPERATURE (K)
! TSOIL   SOIL SURFACE TEMPERATURE (K)

! SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER                :: IPOL, J
      REAL, INTENT(IN)       :: ESD, DTSEC,TSNOW,TSOIL
      REAL, INTENT(INOUT)    :: SNOWH, SNDENS
      REAL                   :: BFAC,DSX,DTHR,DW,SNOWHC,PEXP,           &
                                TAVGC,TSNOWC,TSOILC,ESDC,ESDCX
      REAL, PARAMETER        :: C1 = 0.01, C2 = 21.0, G = 9.81,         &
                                KN = 4000.0
      LOGICAL, INTENT(IN)    :: UA_PHYS  ! UA: flag for UA option
      REAL, INTENT(IN)       :: SNOMLT   ! UA: snow melt [m]
      REAL                   :: SNOMLTC  ! UA: snow melt [cm]
! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS
! ----------------------------------------------------------------------
      SNOWHC = SNOWH *100.
      ESDC = ESD *100.
      IF(UA_PHYS) SNOMLTC = SNOMLT *100.
      DTHR = DTSEC /3600.
      TSNOWC = TSNOW -273.15
      TSOILC = TSOIL -273.15

! ----------------------------------------------------------------------
! CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
!  SNDENS=DS0*(EXP(BFAC*ESD)-1.)/(BFAC*ESD)
!  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)
! NOTE: BFAC*ESD IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED
! NUMERICALLY BELOW:
!   C1 IS THE FRACTIONAL INCREASE IN DENSITY (1/(CM*HR))
!   C2 IS A CONSTANT (CM3/G) KOJIMA ESTIMATED AS 21 CMS/G
! ----------------------------------------------------------------------
      TAVGC = 0.5* (TSNOWC + TSOILC)
      IF (ESDC >  1.E-2) THEN
         ESDCX = ESDC
      ELSE
         ESDCX = 1.E-2
      END IF

!      DSX = SNDENS*((DEXP(BFAC*ESDC)-1.)/(BFAC*ESDC))
! ----------------------------------------------------------------------
! THE FUNCTION OF THE FORM (e**x-1)/x EMBEDDED IN ABOVE EXPRESSION
! FOR DSX WAS CAUSING NUMERICAL DIFFICULTIES WHEN THE DENOMINATOR "x"
! (I.E. BFAC*ESDC) BECAME ZERO OR APPROACHED ZERO (DESPITE THE FACT THAT
! THE ANALYTICAL FUNCTION (e**x-1)/x HAS A WELL DEFINED LIMIT AS
! "x" APPROACHES ZERO), HENCE BELOW WE REPLACE THE (e**x-1)/x
! EXPRESSION WITH AN EQUIVALENT, NUMERICALLY WELL-BEHAVED
! POLYNOMIAL EXPANSION.

! NUMBER OF TERMS OF POLYNOMIAL EXPANSION, AND HENCE ITS ACCURACY,
! IS GOVERNED BY ITERATION LIMIT "IPOL".
!      IPOL GREATER THAN 9 ONLY MAKES A DIFFERENCE ON DOUBLE
!            PRECISION (RELATIVE ERRORS GIVEN IN PERCENT %).
!       IPOL=9, FOR REL.ERROR <~ 1.6 E-6 % (8 SIGNIFICANT DIGITS)
!       IPOL=8, FOR REL.ERROR <~ 1.8 E-5 % (7 SIGNIFICANT DIGITS)
!       IPOL=7, FOR REL.ERROR <~ 1.8 E-4 % ...
! ----------------------------------------------------------------------
      BFAC = DTHR * C1* EXP (0.08* TAVGC - C2* SNDENS)
      IPOL = 4
      PEXP = 0.
!        PEXP = (1. + PEXP)*BFAC*ESDC/REAL(J+1)
      DO J = IPOL,1, -1
         PEXP = (1. + PEXP)* BFAC * ESDCX / REAL (J +1)
      END DO

      PEXP = PEXP + 1.
! ----------------------------------------------------------------------
! ABOVE LINE ENDS POLYNOMIAL SUBSTITUTION
! ----------------------------------------------------------------------
!     END OF KOREAN FORMULATION

!     BASE FORMULATION (COGLEY ET AL., 1990)
!     CONVERT DENSITY FROM G/CM3 TO KG/M3
!       DSM=SNDENS*1000.0

!       DSX=DSM+DTSEC*0.5*DSM*G*ESD/
!    &      (1E7*EXP(-0.02*DSM+KN/(TAVGC+273.16)-14.643))

!  &   CONVERT DENSITY FROM KG/M3 TO G/CM3
!       DSX=DSX/1000.0

!     END OF COGLEY ET AL. FORMULATION

! ----------------------------------------------------------------------
! SET UPPER/LOWER LIMIT ON SNOW DENSITY
! ----------------------------------------------------------------------
      DSX = SNDENS * (PEXP)
      IF (DSX > 0.40) DSX = 0.40
      IF (DSX < 0.05) DSX = 0.05
! ----------------------------------------------------------------------
! UPDATE OF SNOW DEPTH AND DENSITY DEPENDING ON LIQUID WATER DURING
! SNOWMELT.  ASSUMED THAT 13% OF LIQUID WATER CAN BE STORED IN SNOW PER
! DAY DURING SNOWMELT TILL SNOW DENSITY 0.40.
! ----------------------------------------------------------------------
      SNDENS = DSX
      IF (TSNOWC >=  0.) THEN
         DW = 0.13* DTHR /24.
         IF ( UA_PHYS .AND. TSOILC >= 0.) THEN
             DW = MIN (DW, 0.13*SNOMLTC/(ESDCX+0.13*SNOMLTC))
         ENDIF
         SNDENS = SNDENS * (1. - DW) + DW
         IF (SNDENS >=  0.40) SNDENS = 0.40
! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.
! CHANGE SNOW DEPTH UNITS TO METERS
! ----------------------------------------------------------------------
      END IF
      SNOWHC = ESDC / SNDENS
      SNOWH = SNOWHC *0.01

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWPACK
! ----------------------------------------------------------------------

      SUBROUTINE SNOWZ0 (SNCOVR,Z0, Z0BRD, SNOWH,FBUR,FGSN,SHDMAX,UA_PHYS)

! ----------------------------------------------------------------------
! SUBROUTINE SNOWZ0
! ----------------------------------------------------------------------
! CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW
! SNCOVR  FRACTIONAL SNOW COVER
! Z0      ROUGHNESS LENGTH (m)
! Z0S     SNOW ROUGHNESS LENGTH:=0.001 (m)
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN)        :: SNCOVR, Z0BRD
      REAL, INTENT(OUT)       :: Z0
      REAL, PARAMETER         :: Z0S=0.001
      REAL, INTENT(IN)        :: SNOWH
      REAL                    :: BURIAL
      REAL                    :: Z0EFF
      LOGICAL, INTENT(IN)     :: UA_PHYS   ! UA: flag for UA option
      REAL, INTENT(IN)        :: FBUR      ! UA: fraction of canopy buried
      REAL, INTENT(IN)        :: FGSN      ! UA: ground snow cover fraction
      REAL, INTENT(IN)        :: SHDMAX    ! UA: maximum vegetation fraction
      REAL, PARAMETER         :: Z0G=0.01  ! UA: soil roughness
      REAL                    :: FV,A1,A2

      IF(UA_PHYS) THEN

          FV = SHDMAX * (1.-FBUR)
          A1 = (1.-FV)**2*((1.-FGSN**2)*LOG(Z0G) + (FGSN**2)*LOG(Z0S))
          A2 = (1.-(1.-FV)**2)*LOG(Z0BRD)
          Z0 = EXP(A1+A2)

      ELSE

!m        Z0 = (1.- SNCOVR)* Z0BRD + SNCOVR * Z0S
          BURIAL = 7.0*Z0BRD - SNOWH
          IF(BURIAL.LE.0.0007) THEN
              Z0EFF = Z0S
          ELSE      
              Z0EFF = BURIAL/7.0
          ENDIF
      
          Z0 = (1.- SNCOVR)* Z0BRD + SNCOVR * Z0EFF

      ENDIF
! ----------------------------------------------------------------------
  END SUBROUTINE SNOWZ0
! ----------------------------------------------------------------------


      SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)

! ----------------------------------------------------------------------
! SUBROUTINE SNOW_NEW
! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH AND DENSITY TO ACCOUNT FOR THE NEW SNOWFALL.
! NEW VALUES OF SNOW DEPTH & DENSITY RETURNED.

! TEMP    AIR TEMPERATURE (K)
! NEWSN   NEW SNOWFALL (M)
! SNOWH   SNOW DEPTH (M)
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL, INTENT(IN)        :: NEWSN, TEMP
      REAL, INTENT(INOUT)     :: SNDENS, SNOWH
      REAL                    :: DSNEW, HNEWC, SNOWHC,NEWSNC,TEMPC

! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS
! ----------------------------------------------------------------------
      SNOWHC = SNOWH *100.
      NEWSNC = NEWSN *100.

! ----------------------------------------------------------------------
! CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
! EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
! AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
! VEMADOLEN, SWEDEN, 1980, 172-177PP.
!-----------------------------------------------------------------------
      TEMPC = TEMP -273.15
      IF (TEMPC <=  -15.) THEN
         DSNEW = 0.05
      ELSE
         DSNEW = 0.05+0.0017* (TEMPC +15.)**1.5
      END IF
! ----------------------------------------------------------------------
! ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL
! ----------------------------------------------------------------------
      HNEWC = NEWSNC / DSNEW
      IF (SNOWHC + HNEWC .LT. 1.0E-3) THEN
         SNDENS = MAX(DSNEW,SNDENS)
      ELSE
         SNDENS = (SNOWHC * SNDENS + HNEWC * DSNEW)/ (SNOWHC + HNEWC)
      ENDIF
      SNOWHC = SNOWHC + HNEWC
      SNOWH = SNOWHC *0.01

! ----------------------------------------------------------------------
  END SUBROUTINE SNOW_NEW
! ----------------------------------------------------------------------

      SUBROUTINE SRT (RHSTT,EDIR,ET,SH2O,SH2OA,NSOIL,PCPDRP,            &
                       ZSOIL,DWSAT,DKSAT,SMCMAX,BEXP,RUNOFF1,           &
                     RUNOFF2,DT,SMCWLT,SLOPE,KDT,FRZX,SICE,AI,BI,CI,  &
                     SFHEAD1RT,INFXS1RT )

! ----------------------------------------------------------------------
! SUBROUTINE SRT
! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! WATER DIFFUSION EQUATION.  ALSO TO COMPUTE ( PREPARE ) THE MATRIX
! COEFFICIENTS FOR THE TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: IALP1, IOHINF, J, JJ,  K, KS

!DJG NDHMS/WRF-Hydro edit... Variables used in OV routing infiltration calcs
       REAL, INTENT(INOUT)     :: SFHEAD1RT, INFXS1RT
       REAL                    :: SFCWATR,chcksm



      REAL, INTENT(IN)          :: BEXP, DKSAT, DT, DWSAT, EDIR, FRZX,  &
                                   KDT, PCPDRP, SLOPE, SMCMAX, SMCWLT
      REAL, INTENT(OUT)         :: RUNOFF1, RUNOFF2
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ET, SH2O, SH2OA, SICE,  &
                                                ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: RHSTT
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)  :: AI, BI, CI
      REAL, DIMENSION(1:NSOIL)  :: DMAX
      REAL                      :: ACRT, DD, DDT, DDZ, DDZ2, DENOM,     &
                                   DENOM2,DICE, DSMDZ, DSMDZ2, DT1,     &
                                   FCR,INFMAX,MXSMC,MXSMC2,NUMER,PDDUM, &
                                   PX, SICEMAX,SLOPX, SMCAV, SSTT,      &
                                   SUM, VAL, WCND, WCND2, WDF, WDF2
      INTEGER, PARAMETER        :: CVFRZ = 3

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! REFERENCE FROZEN GROUND PARAMETER, CVFRZ, IS A SHAPE PARAMETER OF
! AREAL DISTRIBUTION FUNCTION OF SOIL ICE CONTENT WHICH EQUALS 1/CV.
! CV IS A COEFFICIENT OF SPATIAL VARIATION OF SOIL ICE CONTENT.  BASED
! ON FIELD DATA CV DEPENDS ON AREAL MEAN OF FROZEN DEPTH, AND IT CLOSE
! TO CONSTANT = 0.6 IF AREAL MEAN FROZEN DEPTH IS ABOVE 20 CM.  THAT IS
! WHY PARAMETER CVFRZ = 3 (INT{1/0.6*0.6}).
! CURRENT LOGIC DOESN'T ALLOW CVFRZ BE BIGGER THAN 3
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF.  INCLUDE THE
! INFILTRATION FORMULE FROM SCHAAKE AND KOREN MODEL.
! MODIFIED BY Q DUAN
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! LET SICEMAX BE THE GREATEST, IF ANY, FROZEN WATER CONTENT WITHIN SOIL
! LAYERS.
! ----------------------------------------------------------------------
      IOHINF = 1
      SICEMAX = 0.0
      DO KS = 1,NSOIL
         IF (SICE (KS) >  SICEMAX) SICEMAX = SICE (KS)
! ----------------------------------------------------------------------
! DETERMINE RAINFALL INFILTRATION RATE AND RUNOFF
! ----------------------------------------------------------------------
      END DO

#ifdef WRF_HYDRO
!DJG NDHMS/WRF-Hydro edit...
!DJG Use previously merged Precip and Sfchead for infil. cap. calc.
      SFCWATR = PCPDRP
      PDDUM = SFCWATR
!DJG original   PDDUM = PCPDRP
      RUNOFF1 = 0.0
      INFXS1RT = 0.0
#else
      PDDUM = PCPDRP
      RUNOFF1 = 0.0
#endif



! ----------------------------------------------------------------------
! MODIFIED BY Q. DUAN, 5/16/94
! ----------------------------------------------------------------------
!        IF (IOHINF == 1) THEN

#ifdef WRF_HYDRO
!DJG NDHMS/WRF-Hydro edit...
!DJG IF (PCPDRP /=  0.0) THEN
      IF (SFCWATR /=  0.0) THEN
#else
     IF (PCPDRP /=  0.0) THEN
#endif
         DT1 = DT /86400.
         SMCAV = SMCMAX - SMCWLT

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! ----------------------------------------------------------------------
         DMAX (1)= - ZSOIL (1)* SMCAV

         DICE = - ZSOIL (1) * SICE (1)
         DMAX (1)= DMAX (1)* (1.0- (SH2OA (1) + SICE (1) - SMCWLT)/      &
                    SMCAV)

         DD = DMAX (1)

! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! ----------------------------------------------------------------------
         DO KS = 2,NSOIL

            DICE = DICE+ ( ZSOIL (KS -1) - ZSOIL (KS) ) * SICE (KS)
            DMAX (KS) = (ZSOIL (KS -1) - ZSOIL (KS))* SMCAV
            DMAX (KS) = DMAX (KS)* (1.0- (SH2OA (KS) + SICE (KS)        &
                        - SMCWLT)/ SMCAV)
            DD = DD+ DMAX (KS)
! ----------------------------------------------------------------------
! VAL = (1.-EXP(-KDT*SQRT(DT1)))
! IN BELOW, REMOVE THE SQRT IN ABOVE
! ----------------------------------------------------------------------
         END DO
         VAL = (1. - EXP ( - KDT * DT1))
         DDT = DD * VAL
#ifdef WRF_HYDRO
!DJG NDHMS/WRF-Hydro edit...
!DJG        PX = PCPDRP * DT
         PX = SFCWATR * DT
#else
         PX = PCPDRP * DT
#endif
         IF (PX <  0.0) PX = 0.0



! ----------------------------------------------------------------------
! FROZEN GROUND VERSION:
! REDUCTION OF INFILTRATION BASED ON FROZEN GROUND PARAMETERS
! ----------------------------------------------------------------------
         INFMAX = (PX * (DDT / (PX + DDT)))/ DT
         FCR = 1.
         IF (DICE >  1.E-2) THEN
            ACRT = CVFRZ * FRZX / DICE
            SUM = 1.
            IALP1 = CVFRZ - 1
            DO J = 1,IALP1
               K = 1
               DO JJ = J +1,IALP1
                  K = K * JJ
               END DO
               SUM = SUM + (ACRT ** ( CVFRZ - J)) / FLOAT (K)
            END DO
            FCR = 1. - EXP ( - ACRT) * SUM
         END IF

! ----------------------------------------------------------------------
! CORRECTION OF INFILTRATION LIMITATION:
! IF INFMAX .LE. HYDROLIC CONDUCTIVITY ASSIGN INFMAX THE VALUE OF
! HYDROLIC CONDUCTIVITY
! ----------------------------------------------------------------------
!         MXSMC = MAX ( SH2OA(1), SH2OA(2) )
         INFMAX = INFMAX * FCR

         MXSMC = SH2OA (1)
         CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,           &
                         SICEMAX)
         INFMAX = MAX (INFMAX,WCND)

         INFMAX = MIN (INFMAX,PX/DT)
#ifdef WRF_HYDRO 
!DJG NDHMS/WRF-Hydro edit...
!DJG       IF (PCPDRP >  INFMAX) THEN
         IF (SFCWATR > INFMAX) THEN
!DJG          RUNOFF1 = PCPDRP - INFMAX
          RUNOFF1 = SFCWATR - INFMAX
#else
         IF (PCPDRP >  INFMAX) THEN
            RUNOFF1 = PCPDRP - INFMAX
#endif
          INFXS1RT = RUNOFF1*DT*1000.
          PDDUM = INFMAX
         END IF

! ----------------------------------------------------------------------
! TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN LINE
! BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
! 'MXSMC = MAX(SH2OA(1), SH2OA(2))'
! ----------------------------------------------------------------------
      END IF

      MXSMC = SH2OA (1)
      CALL WDFCND (WDF,WCND,MXSMC,SMCMAX,BEXP,DKSAT,DWSAT,              &
                    SICEMAX)
! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
      DDZ = 1. / ( - .5 * ZSOIL (2) )
      AI (1) = 0.0
      BI (1) = WDF * DDZ / ( - ZSOIL (1) )

! ----------------------------------------------------------------------
! CALC RHSTT FOR THE TOP LAYER AFTER CALC'NG THE VERTICAL SOIL MOISTURE
! GRADIENT BTWN THE TOP AND NEXT TO TOP LAYERS.
! ----------------------------------------------------------------------
      CI (1) = - BI (1)
      DSMDZ = ( SH2O (1) - SH2O (2) ) / ( - .5 * ZSOIL (2) )
      RHSTT (1) = (WDF * DSMDZ + WCND- PDDUM + EDIR + ET (1))/ ZSOIL (1)

! ----------------------------------------------------------------------
! INITIALIZE DDZ2
! ----------------------------------------------------------------------
      SSTT = WDF * DSMDZ + WCND+ EDIR + ET (1)

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABV PROCESS
! ----------------------------------------------------------------------
      DDZ2 = 0.0
      DO K = 2,NSOIL
         DENOM2 = (ZSOIL (K -1) - ZSOIL (K))
         IF (K /= NSOIL) THEN

! ----------------------------------------------------------------------
! AGAIN, TO AVOID SPURIOUS DRAINAGE BEHAVIOR, 'UPSTREAM DIFFERENCING' IN
! LINE BELOW REPLACED WITH NEW APPROACH IN 2ND LINE:
! 'MXSMC2 = MAX (SH2OA(K), SH2OA(K+1))'
! ----------------------------------------------------------------------
            SLOPX = 1.

            MXSMC2 = SH2OA (K)
            CALL WDFCND (WDF2,WCND2,MXSMC2,SMCMAX,BEXP,DKSAT,DWSAT,     &
                          SICEMAX)
! -----------------------------------------------------------------------
! CALC SOME PARTIAL PRODUCTS FOR LATER USE IN CALC'NG RHSTT
! ----------------------------------------------------------------------
            DENOM = (ZSOIL (K -1) - ZSOIL (K +1))

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT
! ----------------------------------------------------------------------
            DSMDZ2 = (SH2O (K) - SH2O (K +1)) / (DENOM * 0.5)
            DDZ2 = 2.0 / DENOM
            CI (K) = - WDF2 * DDZ2 / DENOM2

         ELSE
! ----------------------------------------------------------------------
! SLOPE OF BOTTOM LAYER IS INTRODUCED
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! RETRIEVE THE SOIL WATER DIFFUSIVITY AND HYDRAULIC CONDUCTIVITY FOR
! THIS LAYER
! ----------------------------------------------------------------------
            SLOPX = SLOPE
          CALL WDFCND (WDF2,WCND2,SH2OA (NSOIL),SMCMAX,BEXP,DKSAT,DWSAT,     &
                            SICEMAX)

! ----------------------------------------------------------------------
! CALC A PARTIAL PRODUCT FOR LATER USE IN CALC'NG RHSTT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! SET MATRIX COEF CI TO ZERO
! ----------------------------------------------------------------------
            DSMDZ2 = 0.0
            CI (K) = 0.0
! ----------------------------------------------------------------------
! CALC RHSTT FOR THIS LAYER AFTER CALC'NG ITS NUMERATOR
! ----------------------------------------------------------------------
         END IF
         NUMER = (WDF2 * DSMDZ2) + SLOPX * WCND2- (WDF * DSMDZ)         &
                 - WCND+ ET (K)

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER
! ----------------------------------------------------------------------
         RHSTT (K) = NUMER / ( - DENOM2)
         AI (K) = - WDF * DDZ / DENOM2

! ----------------------------------------------------------------------
! RESET VALUES OF WDF, WCND, DSMDZ, AND DDZ FOR LOOP TO NEXT LYR
! RUNOFF2:  SUB-SURFACE OR BASEFLOW RUNOFF
! ----------------------------------------------------------------------
         BI (K) = - ( AI (K) + CI (K) )
         IF (K .eq. NSOIL) THEN
            RUNOFF2 = SLOPX * WCND2
         END IF
         IF (K .ne. NSOIL) THEN
            WDF = WDF2
            WCND = WCND2
            DSMDZ = DSMDZ2
            DDZ = DDZ2
         END IF
      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SRT
! ----------------------------------------------------------------------

      SUBROUTINE SSTEP (SH2OOUT,SH2OIN,CMC,RHSTT,RHSCT,DT,              &
                        NSOIL,SMCMAX,CMCMAX,RUNOFF3,ZSOIL,SMC,SICE,     &
                        AI,BI,CI, INFXS1RT)

! ----------------------------------------------------------------------
! SUBROUTINE SSTEP
! ----------------------------------------------------------------------
! CALCULATE/UPDATE SOIL MOISTURE CONTENT VALUES AND CANOPY MOISTURE
! CONTENT VALUES.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: I, K, KK11

!!DJG NDHMS/WRF-Hydro edit...
      REAL, INTENT(INOUT)       :: INFXS1RT
      REAL                      :: AVAIL

      REAL, INTENT(IN)          :: CMCMAX, DT, SMCMAX
      REAL, INTENT(OUT)         :: RUNOFF3
      REAL, INTENT(INOUT)       :: CMC
      REAL, DIMENSION(1:NSOIL), INTENT(IN)     :: SH2OIN, SICE, ZSOIL
      REAL, DIMENSION(1:NSOIL), INTENT(OUT)    :: SH2OOUT
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT)  :: RHSTT, SMC
      REAL, DIMENSION(1:NSOIL), INTENT(INOUT)  :: AI, BI, CI
      REAL, DIMENSION(1:NSOIL)  :: RHSTTin
      REAL, DIMENSION(1:NSOIL)  :: CIin
      REAL                      :: DDZ, RHSCT, STOT, WPLUS

! ----------------------------------------------------------------------
! CREATE 'AMOUNT' VALUES OF VARIABLES TO BE INPUT TO THE
! TRI-DIAGONAL MATRIX ROUTINE.
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTT (K) = RHSTT (K) * DT
         AI (K) = AI (K) * DT
         BI (K) = 1. + BI (K) * DT
         CI (K) = CI (K) * DT
      END DO
! ----------------------------------------------------------------------
! COPY VALUES FOR INPUT VARIABLES BEFORE CALL TO ROSR12
! ----------------------------------------------------------------------
      DO K = 1,NSOIL
         RHSTTin (K) = RHSTT (K)
      END DO
      DO K = 1,NSOIL
         CIin (K) = CI (K)
      END DO
! ----------------------------------------------------------------------
! CALL ROSR12 TO SOLVE THE TRI-DIAGONAL MATRIX
! ----------------------------------------------------------------------
      CALL ROSR12 (CI,AI,BI,CIin,RHSTTin,RHSTT,NSOIL)
! ----------------------------------------------------------------------
! SUM THE PREVIOUS SMC VALUE AND THE MATRIX SOLUTION TO GET A
! NEW VALUE.  MIN ALLOWABLE VALUE OF SMC WILL BE 0.02.
! RUNOFF3: RUNOFF WITHIN SOIL LAYERS
! ----------------------------------------------------------------------
      WPLUS = 0.0
      RUNOFF3 = 0.

      DDZ = - ZSOIL (1)
      DO K = 1,NSOIL
         IF (K /= 1) DDZ = ZSOIL (K - 1) - ZSOIL (K)
         SH2OOUT (K) = SH2OIN (K) + CI (K) + WPLUS / DDZ
         STOT = SH2OOUT (K) + SICE (K)
         IF (STOT > SMCMAX) THEN
            IF (K .eq. 1) THEN
               DDZ = - ZSOIL (1)
            ELSE
               KK11 = K - 1
               DDZ = - ZSOIL (K) + ZSOIL (KK11)
            END IF
            WPLUS = (STOT - SMCMAX) * DDZ
         ELSE
            WPLUS = 0.
         END IF
         SMC (K) = MAX ( MIN (STOT,SMCMAX),0.02 )
         SH2OOUT (K) = MAX ( (SMC (K) - SICE (K)),0.0)
      END DO
#ifdef WRF_HYDRO
!DJG NDHMS/WRF-Hydro edit...
!DJG Modifications to redstribute WPLUS/RUNOFF3 (soil moisture closure error) to soil profile
!DJG beginning at bottom layer (NSOIL)
         IF (WPLUS > 0.) THEN
           DO K=NSOIL,2,-1

               IF (K .eq. 2) THEN     !Assign soil depths
                 DDZ = -ZSOIL(1)
               ELSE
                 DDZ = ZSOIL(K-2)-ZSOIL(K-1)
               END IF

               AVAIL = (SMCMAX - SMC(K-1)) * DDZ    !Det. Avail. Stor.

!               print *, "ZZZZZ", K,DDZ,AVAIL,WPLUS,SMC(K),SMC(K-1),SMCMAX

               IF (WPLUS <= AVAIL) THEN
                 SMC(K-1) = SMC(K-1) + WPLUS/DDZ
                 WPLUS = 0.
               ELSE
                 SMC(K-1) = SMCMAX
                 WPLUS = WPLUS - AVAIL
                 IF (K-1 .eq. 1) THEN
                   INFXS1RT = INFXS1RT + WPLUS*1000
                   WPLUS = 0.
                 END IF
               END IF

!             SMC (K) = MAX ( MIN (STOT,SMCMAX),0.02 )
             SH2OOUT (K) = MAX ( (SMC (K) - SICE (K)),0.0)

           END DO
         END IF
!DJG NDHMS/WRF-Hydro edit...End of modification
#endif


! ----------------------------------------------------------------------
! UPDATE CANOPY WATER CONTENT/INTERCEPTION (CMC).  CONVERT RHSCT TO
! AN 'AMOUNT' VALUE AND ADD TO PREVIOUS CMC VALUE TO GET NEW CMC.
! ----------------------------------------------------------------------
      RUNOFF3 = WPLUS
      CMC = CMC + DT * RHSCT
      IF (CMC < 1.E-20) CMC = 0.0
      CMC = MIN (CMC,CMCMAX)

! ----------------------------------------------------------------------
  END SUBROUTINE SSTEP
! ----------------------------------------------------------------------

      SUBROUTINE TBND (TU,TB,ZSOIL,ZBOT,K,NSOIL,TBND1)

! ----------------------------------------------------------------------
! SUBROUTINE TBND
! ----------------------------------------------------------------------
! CALCULATE TEMPERATURE ON THE BOUNDARY OF THE LAYER BY INTERPOLATION OF
! THE MIDDLE LAYER TEMPERATURES
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: NSOIL
      INTEGER                   :: K
      REAL, INTENT(IN)          :: TB, TU, ZBOT
      REAL, INTENT(OUT)         :: TBND1
      REAL, DIMENSION(1:NSOIL), INTENT(IN)   :: ZSOIL
      REAL                      :: ZB, ZUP
      REAL, PARAMETER           :: T0 = 273.15

! ----------------------------------------------------------------------
! USE SURFACE TEMPERATURE ON THE TOP OF THE FIRST LAYER
! ----------------------------------------------------------------------
     IF (K == 1) THEN
         ZUP = 0.
      ELSE
         ZUP = ZSOIL (K -1)
      END IF
! ----------------------------------------------------------------------
! USE DEPTH OF THE CONSTANT BOTTOM TEMPERATURE WHEN INTERPOLATE
! TEMPERATURE INTO THE LAST LAYER BOUNDARY
! ----------------------------------------------------------------------
      IF (K ==  NSOIL) THEN
         ZB = 2.* ZBOT - ZSOIL (K)
      ELSE
         ZB = ZSOIL (K +1)
      END IF
! ----------------------------------------------------------------------
! LINEAR INTERPOLATION BETWEEN THE AVERAGE LAYER TEMPERATURES
! ----------------------------------------------------------------------

      TBND1 = TU + (TB - TU)* (ZUP - ZSOIL (K))/ (ZUP - ZB)
! ----------------------------------------------------------------------
  END SUBROUTINE TBND
! ----------------------------------------------------------------------


      SUBROUTINE TDFCND ( DF, SMC, QZ, SMCMAX, SH2O, BEXP, PSISAT, SOILTYP, OPT_THCND)

! ----------------------------------------------------------------------
! SUBROUTINE TDFCND
! ----------------------------------------------------------------------
! CALCULATE THERMAL DIFFUSIVITY AND CONDUCTIVITY OF THE SOIL FOR A GIVEN
! POINT AND TIME.
! ----------------------------------------------------------------------
! PETERS-LIDARD APPROACH (PETERS-LIDARD et al., 1998)
! June 2001 CHANGES: FROZEN SOIL CONDITION.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(IN)       :: SOILTYP, OPT_THCND  
      REAL, INTENT(IN)          :: QZ,  SMC, SMCMAX, SH2O, BEXP, PSISAT
      REAL, INTENT(OUT)         :: DF
      REAL                      :: AKE, GAMMD, THKDRY, THKICE, THKO,    &
                                   THKQTZ,THKSAT,THKS,THKW,SATRATIO,XU, &
                                   XUNFROZ,AKEI,AKEL,PSIF,PF  

! ----------------------------------------------------------------------
! WE NOW GET QUARTZ AS AN INPUT ARGUMENT (SET IN ROUTINE REDPRM):
!      DATA QUARTZ /0.82, 0.10, 0.25, 0.60, 0.52,
!     &             0.35, 0.60, 0.40, 0.82/
! ----------------------------------------------------------------------
! IF THE SOIL HAS ANY MOISTURE CONTENT COMPUTE A PARTIAL SUM/PRODUCT
! OTHERWISE USE A CONSTANT VALUE WHICH WORKS WELL WITH MOST SOILS
! ----------------------------------------------------------------------
!  THKW ......WATER THERMAL CONDUCTIVITY
!  THKQTZ ....THERMAL CONDUCTIVITY FOR QUARTZ
!  THKO ......THERMAL CONDUCTIVITY FOR OTHER SOIL COMPONENTS
!  THKS ......THERMAL CONDUCTIVITY FOR THE SOLIDS COMBINED(QUARTZ+OTHER)
!  THKICE ....ICE THERMAL CONDUCTIVITY
!  SMCMAX ....POROSITY (= SMCMAX)
!  QZ .........QUARTZ CONTENT (SOIL TYPE DEPENDENT)
! ----------------------------------------------------------------------
! USE AS IN PETERS-LIDARD, 1998 (MODIF. FROM JOHANSEN, 1975).

!                                  PABLO GRUNMANN, 08/17/98
! REFS.:
!      FAROUKI, O.T.,1986: THERMAL PROPERTIES OF SOILS. SERIES ON ROCK
!              AND SOIL MECHANICS, VOL. 11, TRANS TECH, 136 PP.
!      JOHANSEN, O., 1975: THERMAL CONDUCTIVITY OF SOILS. PH.D. THESIS,
!              UNIVERSITY OF TRONDHEIM,
!      PETERS-LIDARD, C. D., ET AL., 1998: THE EFFECT OF SOIL THERMAL
!              CONDUCTIVITY PARAMETERIZATION ON SURFACE ENERGY FLUXES
!              AND TEMPERATURES. JOURNAL OF THE ATMOSPHERIC SCIENCES,
!              VOL. 55, PP. 1209-1224.
! ----------------------------------------------------------------------

IF ( OPT_THCND == 1 .OR. ( OPT_THCND == 2 .AND. (SOILTYP /= 4 .AND. SOILTYP /= 3)) )THEN

! NEEDS PARAMETERS
! POROSITY(SOIL TYPE):
!      POROS = SMCMAX
! SATURATION RATIO:
! PARAMETERS  W/(M.K)
      SATRATIO = SMC / SMCMAX
! ICE CONDUCTIVITY:
      THKICE = 2.2
! WATER CONDUCTIVITY:
      THKW = 0.57
! THERMAL CONDUCTIVITY OF "OTHER" SOIL COMPONENTS
!      IF (QZ .LE. 0.2) THKO = 3.0
      THKO = 2.0
! QUARTZ' CONDUCTIVITY
      THKQTZ = 7.7
! SOLIDS' CONDUCTIVITY
      THKS = (THKQTZ ** QZ)* (THKO ** (1. - QZ))

! UNFROZEN FRACTION (FROM 1., i.e., 100%LIQUID, TO 0. (100% FROZEN))
      XUNFROZ = SH2O / SMC
! UNFROZEN VOLUME FOR SATURATION (POROSITY*XUNFROZ)
      XU = XUNFROZ * SMCMAX

! SATURATED THERMAL CONDUCTIVITY
      THKSAT = THKS ** (1. - SMCMAX)* THKICE ** (SMCMAX - XU)* THKW **   &
         (XU)

! DRY DENSITY IN KG/M3
      GAMMD = (1. - SMCMAX)*2700.

! DRY THERMAL CONDUCTIVITY IN W.M-1.K-1
      THKDRY = (0.135* GAMMD+ 64.7)/ (2700. - 0.947* GAMMD)
! FROZEN
         AKEI = SATRATIO
! UNFROZEN
! RANGE OF VALIDITY FOR THE KERSTEN NUMBER (AKE)

! KERSTEN NUMBER (USING "FINE" FORMULA, VALID FOR SOILS CONTAINING AT
! LEAST 5% OF PARTICLES WITH DIAMETER LESS THAN 2.E-6 METERS.)
! (FOR "COARSE" FORMULA, SEE PETERS-LIDARD ET AL., 1998).

         IF ( SATRATIO >  0.1 ) THEN

            AKEL = LOG10 (SATRATIO) + 1.0

! USE K = KDRY
         ELSE

            AKEL = 0.0
         END IF
      AKE = ((SMC-SH2O)*AKEI + SH2O*AKEL)/SMC
!  THERMAL CONDUCTIVITY


      DF = AKE * (THKSAT - THKDRY) + THKDRY

    ELSE
 
! use the Mccumber and Pielke approach for silt loam (4), sandy loam (3)

         PSIF = PSISAT*100.*(SMCMAX/(SMC))**BEXP
!--- PSIF should be in [CM] to compute PF
           PF=log10(abs(PSIF))
!--- HK is for McCumber thermal conductivity
         IF(PF.LE.5.1) THEN
           DF=420.*EXP(-(PF+2.7))
         ELSE
           DF=.1744
         END IF

    ENDIF  ! for OPT_THCND OPTIONS             
! ----------------------------------------------------------------------
  END SUBROUTINE TDFCND
! ----------------------------------------------------------------------

      SUBROUTINE TMPAVG (TAVG,TUP,TM,TDN,ZSOIL,NSOIL,K)

! ----------------------------------------------------------------------
! SUBROUTINE TMPAVG
! ----------------------------------------------------------------------
! CALCULATE SOIL LAYER AVERAGE TEMPERATURE (TAVG) IN FREEZING/THAWING
! LAYER USING UP, DOWN, AND MIDDLE LAYER TEMPERATURES (TUP, TDN, TM),
! WHERE TUP IS AT TOP BOUNDARY OF LAYER, TDN IS AT BOTTOM BOUNDARY OF
! LAYER.  TM IS LAYER PROGNOSTIC STATE TEMPERATURE.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  K

      INTEGER  NSOIL
      REAL     DZ
      REAL     DZH
      REAL     T0
      REAL     TAVG
      REAL     TDN
      REAL     TM
      REAL     TUP
      REAL     X0
      REAL     XDN
      REAL     XUP

      REAL     ZSOIL (NSOIL)

! ----------------------------------------------------------------------
      PARAMETER (T0 = 2.7315E2)
      IF (K .eq. 1) THEN
         DZ = - ZSOIL (1)
      ELSE
         DZ = ZSOIL (K -1) - ZSOIL (K)
      END IF

      DZH = DZ *0.5
      IF (TUP .lt. T0) THEN
         IF (TM .lt. T0) THEN
! ----------------------------------------------------------------------
! TUP, TM, TDN < T0
! ----------------------------------------------------------------------
            IF (TDN .lt. T0) THEN
               TAVG = (TUP + 2.0* TM + TDN)/ 4.0
! ----------------------------------------------------------------------
! TUP & TM < T0,  TDN .ge. T0
! ----------------------------------------------------------------------
            ELSE
               X0 = (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (TUP * DZH + TM * (DZH + X0) + T0* (        &
     &               2.* DZH - X0)) / DZ
            END IF
         ELSE
! ----------------------------------------------------------------------
! TUP < T0, TM .ge. T0, TDN < T0
! ----------------------------------------------------------------------
            IF (TDN .lt. T0) THEN
               XUP = (T0- TUP) * DZH / (TM - TUP)
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP - XDN)       &
     &                + TDN * XDN) / DZ
! ----------------------------------------------------------------------
! TUP < T0, TM .ge. T0, TDN .ge. T0
! ----------------------------------------------------------------------
            ELSE
               XUP = (T0- TUP) * DZH / (TM - TUP)
               TAVG = 0.5 * (TUP * XUP + T0* (2.* DZ - XUP)) / DZ
            END IF
         END IF
      ELSE
         IF (TM .lt. T0) THEN
! ----------------------------------------------------------------------
! TUP .ge. T0, TM < T0, TDN < T0
! ----------------------------------------------------------------------
            IF (TDN .lt. T0) THEN
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)
               TAVG = 0.5 * (T0* (DZ - XUP) + TM * (DZH + XUP)          &
     &                + TDN * DZH) / DZ
! ----------------------------------------------------------------------
! TUP .ge. T0, TM < T0, TDN .ge. T0
! ----------------------------------------------------------------------
            ELSE
               XUP = DZH - (T0- TUP) * DZH / (TM - TUP)
               XDN = (T0- TM) * DZH / (TDN - TM)
               TAVG = 0.5 * (T0* (2.* DZ - XUP - XDN) + TM *            &
     & (XUP + XDN)) / DZ
            END IF
         ELSE
! ----------------------------------------------------------------------
! TUP .ge. T0, TM .ge. T0, TDN < T0
! ----------------------------------------------------------------------
            IF (TDN .lt. T0) THEN
               XDN = DZH - (T0- TM) * DZH / (TDN - TM)
               TAVG = (T0* (DZ - XDN) +0.5* (T0+ TDN)* XDN) / DZ
! ----------------------------------------------------------------------
! TUP .ge. T0, TM .ge. T0, TDN .ge. T0
! ----------------------------------------------------------------------
            ELSE
               TAVG = (TUP + 2.0* TM + TDN) / 4.0
            END IF
         END IF
      END IF
! ----------------------------------------------------------------------
  END SUBROUTINE TMPAVG
! ----------------------------------------------------------------------

      SUBROUTINE TRANSP (ET,NSOIL,ETP1,SMC,CMC,ZSOIL,SHDFAC,SMCWLT,     &
     &                      CMCMAX,PC,CFACTR,SMCREF,SFCTMP,Q2,NROOT,    &
     &                      RTDIS)

! ----------------------------------------------------------------------
! SUBROUTINE TRANSP
! ----------------------------------------------------------------------
! CALCULATE TRANSPIRATION FOR THE VEG CLASS.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  I
      INTEGER  K
      INTEGER  NSOIL

      INTEGER  NROOT
      REAL     CFACTR
      REAL     CMC
      REAL     CMCMAX
      REAL     DENOM
      REAL     ET (NSOIL)
      REAL     ETP1
      REAL     ETP1A
!.....REAL PART(NSOIL)
      REAL     GX (NROOT)
      REAL     PC
      REAL     Q2
      REAL     RTDIS (NSOIL)
      REAL     RTX
      REAL     SFCTMP
      REAL     SGX
      REAL     SHDFAC
      REAL     SMC (NSOIL)
      REAL     SMCREF
      REAL     SMCWLT

! ----------------------------------------------------------------------
! INITIALIZE PLANT TRANSP TO ZERO FOR ALL SOIL LAYERS.
! ----------------------------------------------------------------------
      REAL     ZSOIL (NSOIL)
      DO K = 1,NSOIL
         ET (K) = 0.
! ----------------------------------------------------------------------
! CALCULATE AN 'ADJUSTED' POTENTIAL TRANSPIRATION
! IF STATEMENT BELOW TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
! NOTE: GX AND OTHER TERMS BELOW REDISTRIBUTE TRANSPIRATION BY LAYER,
! ET(K), AS A FUNCTION OF SOIL MOISTURE AVAILABILITY, WHILE PRESERVING
! TOTAL ETP1A.
! ----------------------------------------------------------------------
      END DO
      IF (CMC .ne. 0.0) THEN
         ETP1A = SHDFAC * PC * ETP1 * (1.0- (CMC / CMCMAX) ** CFACTR)
      ELSE
         ETP1A = SHDFAC * PC * ETP1
      END IF
      SGX = 0.0
      DO I = 1,NROOT
         GX (I) = ( SMC (I) - SMCWLT ) / ( SMCREF - SMCWLT )
         GX (I) = MAX ( MIN ( GX (I), 1. ), 0. )
         SGX = SGX + GX (I)
      END DO

      SGX = SGX / NROOT
      DENOM = 0.
      DO I = 1,NROOT
         RTX = RTDIS (I) + GX (I) - SGX
         GX (I) = GX (I) * MAX ( RTX, 0. )
         DENOM = DENOM + GX (I)
      END DO

      IF (DENOM .le. 0.0) DENOM = 1.
      DO I = 1,NROOT
         ET (I) = ETP1A * GX (I) / DENOM
! ----------------------------------------------------------------------
! ABOVE CODE ASSUMES A VERTICALLY UNIFORM ROOT DISTRIBUTION
! CODE BELOW TESTS A VARIABLE ROOT DISTRIBUTION
! ----------------------------------------------------------------------
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * GX * ETP1A
!      ET(1) = ( ZSOIL(1) / ZSOIL(NROOT) ) * ETP1A
! ----------------------------------------------------------------------
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
!      ET(1) = RTDIS(1) * ETP1A
!      ET(1) = ETP1A * PART(1)
! ----------------------------------------------------------------------
! LOOP DOWN THRU THE SOIL LAYERS REPEATING THE OPERATION ABOVE,
! BUT USING THE THICKNESS OF THE SOIL LAYER (RATHER THAN THE
! ABSOLUTE DEPTH OF EACH LAYER) IN THE FINAL CALCULATION.
! ----------------------------------------------------------------------
!      DO K = 2,NROOT
!        GX = ( SMC(K) - SMCWLT ) / ( SMCREF - SMCWLT )
!        GX = MAX ( MIN ( GX, 1. ), 0. )
! TEST CANOPY RESISTANCE
!        GX = 1.0
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*GX*ETP1A
!        ET(K) = ((ZSOIL(K)-ZSOIL(K-1))/ZSOIL(NROOT))*ETP1A
! ----------------------------------------------------------------------
! USING ROOT DISTRIBUTION AS WEIGHTING FACTOR
! ----------------------------------------------------------------------
!        ET(K) = RTDIS(K) * ETP1A
!        ET(K) = ETP1A*PART(K)
!      END DO
      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE TRANSP
! ----------------------------------------------------------------------

      SUBROUTINE WDFCND (WDF,WCND,SMC,SMCMAX,BEXP,DKSAT,DWSAT,          &
     &                      SICEMAX)

! ----------------------------------------------------------------------
! SUBROUTINE WDFCND
! ----------------------------------------------------------------------
! CALCULATE SOIL WATER DIFFUSIVITY AND SOIL HYDRAULIC CONDUCTIVITY.
! ----------------------------------------------------------------------
      IMPLICIT NONE
      REAL     BEXP
      REAL     DKSAT
      REAL     DWSAT
      REAL     EXPON
      REAL     FACTR1
      REAL     FACTR2
      REAL     SICEMAX
      REAL     SMC
      REAL     SMCMAX
      REAL     VKwgt
      REAL     WCND

! ----------------------------------------------------------------------
!     CALC THE RATIO OF THE ACTUAL TO THE MAX PSBL SOIL H2O CONTENT
! ----------------------------------------------------------------------
      REAL     WDF
      FACTR1 = 0.05 / SMCMAX

! ----------------------------------------------------------------------
! PREP AN EXPNTL COEF AND CALC THE SOIL WATER DIFFUSIVITY
! ----------------------------------------------------------------------
      FACTR2 = SMC / SMCMAX
      FACTR1 = MIN(FACTR1,FACTR2)
      EXPON = BEXP + 2.0

! ----------------------------------------------------------------------
! FROZEN SOIL HYDRAULIC DIFFUSIVITY.  VERY SENSITIVE TO THE VERTICAL
! GRADIENT OF UNFROZEN WATER. THE LATTER GRADIENT CAN BECOME VERY
! EXTREME IN FREEZING/THAWING SITUATIONS, AND GIVEN THE RELATIVELY
! FEW AND THICK SOIL LAYERS, THIS GRADIENT SUFFERES SERIOUS
! TRUNCTION ERRORS YIELDING ERRONEOUSLY HIGH VERTICAL TRANSPORTS OF
! UNFROZEN WATER IN BOTH DIRECTIONS FROM HUGE HYDRAULIC DIFFUSIVITY.
! THEREFORE, WE FOUND WE HAD TO ARBITRARILY CONSTRAIN WDF
! --
! VERSION D_10CM: ........  FACTR1 = 0.2/SMCMAX
! WEIGHTED APPROACH...................... PABLO GRUNMANN, 28_SEP_1999.
! ----------------------------------------------------------------------
      WDF = DWSAT * FACTR2 ** EXPON
      IF (SICEMAX .gt. 0.0) THEN
         VKWGT = 1./ (1. + (500.* SICEMAX)**3.)
         WDF = VKWGT * WDF + (1. - VKWGT)* DWSAT * FACTR1** EXPON
! ----------------------------------------------------------------------
! RESET THE EXPNTL COEF AND CALC THE HYDRAULIC CONDUCTIVITY
! ----------------------------------------------------------------------
      END IF
      EXPON = (2.0 * BEXP) + 3.0
      WCND = DKSAT * FACTR2 ** EXPON

! ----------------------------------------------------------------------
  END SUBROUTINE WDFCND
! ----------------------------------------------------------------------

      SUBROUTINE SFCDIF_off (ZLM,Z0,THZ0,THLM,SFCSPD,CZIL,AKMS,AKHS)

! ----------------------------------------------------------------------
! SUBROUTINE SFCDIF (renamed SFCDIF_off to avoid clash with Eta PBL)
! ----------------------------------------------------------------------
! CALCULATE SURFACE LAYER EXCHANGE COEFFICIENTS VIA ITERATIVE PROCESS.
! SEE CHEN ET AL (1997, BLM)
! ----------------------------------------------------------------------

      IMPLICIT NONE
      REAL     WWST, WWST2, G, VKRM, EXCM, BETA, BTG, ELFC, WOLD, WNEW
      REAL     PIHF, EPSU2, EPSUST, EPSIT, EPSA, ZTMIN, ZTMAX, HPBL,     &
     & SQVISC
      REAL     RIC, RRIC, FHNEU, RFC, RFAC, ZZ, PSLMU, PSLMS, PSLHU,     &
     & PSLHS
      REAL     XX, PSPMU, YY, PSPMS, PSPHU, PSPHS, ZLM, Z0, THZ0, THLM
      REAL     SFCSPD, CZIL, AKMS, AKHS, ZILFC, ZU, ZT, RDZ, CXCH
      REAL     DTHV, DU2, BTGH, WSTAR2, USTAR, ZSLU, ZSLT, RLOGU, RLOGT
      REAL     RLMO, ZETALT, ZETALU, ZETAU, ZETAT, XLU4, XLT4, XU4, XT4
!CC   ......REAL ZTFC

      REAL     XLU, XLT, XU, XT, PSMZ, SIMM, PSHZ, SIMH, USTARK, RLMN,  &
     &         RLMA

      INTEGER  ITRMX, ILECH, ITR
      PARAMETER                                                         &
     &        (WWST = 1.2,WWST2 = WWST * WWST,G = 9.8,VKRM = 0.40,      &
     &         EXCM = 0.001                                             &
     &        ,BETA = 1./270.,BTG = BETA * G,ELFC = VKRM * BTG          &
     &                  ,WOLD =.15,WNEW = 1. - WOLD,ITRMX = 05,         &
     &                   PIHF = 3.14159265/2.)
      PARAMETER                                                         &
     &         (EPSU2 = 1.E-4,EPSUST = 0.07,EPSIT = 1.E-4,EPSA = 1.E-8  &
     &         ,ZTMIN = -5.,ZTMAX = 1.,HPBL = 1000.0                    &
     &          ,SQVISC = 258.2)
      PARAMETER                                                         &
     &       (RIC = 0.183,RRIC = 1.0/ RIC,FHNEU = 0.8,RFC = 0.191       &
     &        ,RFAC = RIC / (FHNEU * RFC * RFC))

! ----------------------------------------------------------------------
! NOTE: THE TWO CODE BLOCKS BELOW DEFINE FUNCTIONS
! ----------------------------------------------------------------------
! LECH'S SURFACE FUNCTIONS
! ----------------------------------------------------------------------
      PSLMU (ZZ)= -0.96* log (1.0-4.5* ZZ)
      PSLMS (ZZ)= ZZ * RRIC -2.076* (1. -1./ (ZZ +1.))
      PSLHU (ZZ)= -0.96* log (1.0-4.5* ZZ)

! ----------------------------------------------------------------------
! PAULSON'S SURFACE FUNCTIONS
! ----------------------------------------------------------------------
      PSLHS (ZZ)= ZZ * RFAC -2.076* (1. -1./ (ZZ +1.))
      PSPMU (XX)= -2.* log ( (XX +1.)*0.5) - log ( (XX * XX +1.)*0.5)   &
     &        +2.* ATAN (XX)                                            &
     &- PIHF
      PSPMS (YY)= 5.* YY
      PSPHU (XX)= -2.* log ( (XX * XX +1.)*0.5)

! ----------------------------------------------------------------------
! THIS ROUTINE SFCDIF CAN HANDLE BOTH OVER OPEN WATER (SEA, OCEAN) AND
! OVER SOLID SURFACE (LAND, SEA-ICE).
! ----------------------------------------------------------------------
      PSPHS (YY)= 5.* YY

! ----------------------------------------------------------------------
!     ZTFC: RATIO OF ZOH/ZOM  LESS OR EQUAL THAN 1
!     C......ZTFC=0.1
!     CZIL: CONSTANT C IN Zilitinkevich, S. S.1995,:NOTE ABOUT ZT
! ----------------------------------------------------------------------
      ILECH = 0

! ----------------------------------------------------------------------
      ZILFC = - CZIL * VKRM * SQVISC
!     C.......ZT=Z0*ZTFC
      ZU = Z0
      RDZ = 1./ ZLM
      CXCH = EXCM * RDZ
      DTHV = THLM - THZ0

! ----------------------------------------------------------------------
! BELJARS CORRECTION OF USTAR
! ----------------------------------------------------------------------
      DU2 = MAX (SFCSPD * SFCSPD,EPSU2)
!cc   If statements to avoid TANGENT LINEAR problems near zero
      BTGH = BTG * HPBL
      IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
         WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
      ELSE
         WSTAR2 = 0.0
      END IF

! ----------------------------------------------------------------------
! ZILITINKEVITCH APPROACH FOR ZT
! ----------------------------------------------------------------------
      USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

! ----------------------------------------------------------------------
      ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
      ZSLU = ZLM + ZU
!     PRINT*,'ZSLT=',ZSLT
!     PRINT*,'ZLM=',ZLM
!     PRINT*,'ZT=',ZT

      ZSLT = ZLM + ZT
      RLOGU = log (ZSLU / ZU)

      RLOGT = log (ZSLT / ZT)
!     PRINT*,'RLMO=',RLMO
!     PRINT*,'ELFC=',ELFC
!     PRINT*,'AKHS=',AKHS
!     PRINT*,'DTHV=',DTHV
!     PRINT*,'USTAR=',USTAR

      RLMO = ELFC * AKHS * DTHV / USTAR **3
! ----------------------------------------------------------------------
! 1./MONIN-OBUKKHOV LENGTH-SCALE
! ----------------------------------------------------------------------
      DO ITR = 1,ITRMX
         ZETALT = MAX (ZSLT * RLMO,ZTMIN)
         RLMO = ZETALT / ZSLT
         ZETALU = ZSLU * RLMO
         ZETAU = ZU * RLMO

         ZETAT = ZT * RLMO
         IF (ILECH .eq. 0) THEN
            IF (RLMO .lt. 0.)THEN
               XLU4 = 1. -16.* ZETALU
               XLT4 = 1. -16.* ZETALT
               XU4 = 1. -16.* ZETAU

               XT4 = 1. -16.* ZETAT
               XLU = SQRT (SQRT (XLU4))
               XLT = SQRT (SQRT (XLT4))
               XU = SQRT (SQRT (XU4))

               XT = SQRT (SQRT (XT4))
!     PRINT*,'-----------1------------'
!     PRINT*,'PSMZ=',PSMZ
!     PRINT*,'PSPMU(ZETAU)=',PSPMU(ZETAU)
!     PRINT*,'XU=',XU
!     PRINT*,'------------------------'
               PSMZ = PSPMU (XU)
               SIMM = PSPMU (XLU) - PSMZ + RLOGU
               PSHZ = PSPHU (XT)
               SIMH = PSPHU (XLT) - PSHZ + RLOGT
            ELSE
               ZETALU = MIN (ZETALU,ZTMAX)
               ZETALT = MIN (ZETALT,ZTMAX)
!     PRINT*,'-----------2------------'
!     PRINT*,'PSMZ=',PSMZ
!     PRINT*,'PSPMS(ZETAU)=',PSPMS(ZETAU)
!     PRINT*,'ZETAU=',ZETAU
!     PRINT*,'------------------------'
               PSMZ = PSPMS (ZETAU)
               SIMM = PSPMS (ZETALU) - PSMZ + RLOGU
               PSHZ = PSPHS (ZETAT)
               SIMH = PSPHS (ZETALT) - PSHZ + RLOGT
            END IF
! ----------------------------------------------------------------------
! LECH'S FUNCTIONS
! ----------------------------------------------------------------------
         ELSE
            IF (RLMO .lt. 0.)THEN
!     PRINT*,'-----------3------------'
!     PRINT*,'PSMZ=',PSMZ
!     PRINT*,'PSLMU(ZETAU)=',PSLMU(ZETAU)
!     PRINT*,'ZETAU=',ZETAU
!     PRINT*,'------------------------'
               PSMZ = PSLMU (ZETAU)
               SIMM = PSLMU (ZETALU) - PSMZ + RLOGU
               PSHZ = PSLHU (ZETAT)
               SIMH = PSLHU (ZETALT) - PSHZ + RLOGT
            ELSE
               ZETALU = MIN (ZETALU,ZTMAX)

               ZETALT = MIN (ZETALT,ZTMAX)
!     PRINT*,'-----------4------------'
!     PRINT*,'PSMZ=',PSMZ
!     PRINT*,'PSLMS(ZETAU)=',PSLMS(ZETAU)
!     PRINT*,'ZETAU=',ZETAU
!     PRINT*,'------------------------'
               PSMZ = PSLMS (ZETAU)
               SIMM = PSLMS (ZETALU) - PSMZ + RLOGU
               PSHZ = PSLHS (ZETAT)
               SIMH = PSLHS (ZETALT) - PSHZ + RLOGT
            END IF
! ----------------------------------------------------------------------
! BELJAARS CORRECTION FOR USTAR
! ----------------------------------------------------------------------
         END IF

! ----------------------------------------------------------------------
! ZILITINKEVITCH FIX FOR ZT
! ----------------------------------------------------------------------
         USTAR = MAX (SQRT (AKMS * SQRT (DU2+ WSTAR2)),EPSUST)

         ZT = EXP (ZILFC * SQRT (USTAR * Z0))* Z0
         ZSLT = ZLM + ZT
!-----------------------------------------------------------------------
         RLOGT = log (ZSLT / ZT)
         USTARK = USTAR * VKRM
         AKMS = MAX (USTARK / SIMM,CXCH)
!-----------------------------------------------------------------------
! IF STATEMENTS TO AVOID TANGENT LINEAR PROBLEMS NEAR ZERO
!-----------------------------------------------------------------------
         AKHS = MAX (USTARK / SIMH,CXCH)
         IF (BTGH * AKHS * DTHV .ne. 0.0) THEN
            WSTAR2 = WWST2* ABS (BTGH * AKHS * DTHV)** (2./3.)
         ELSE
            WSTAR2 = 0.0
         END IF
!-----------------------------------------------------------------------
         RLMN = ELFC * AKHS * DTHV / USTAR **3
!-----------------------------------------------------------------------
!     IF(ABS((RLMN-RLMO)/RLMA).LT.EPSIT)    GO TO 110
!-----------------------------------------------------------------------
         RLMA = RLMO * WOLD+ RLMN * WNEW
!-----------------------------------------------------------------------
         RLMO = RLMA
!     PRINT*,'----------------------------'
!     PRINT*,'SFCDIF OUTPUT !  ! ! ! ! ! ! ! !  !   !    !'

!     PRINT*,'ZLM=',ZLM
!     PRINT*,'Z0=',Z0
!     PRINT*,'THZ0=',THZ0
!     PRINT*,'THLM=',THLM
!     PRINT*,'SFCSPD=',SFCSPD
!     PRINT*,'CZIL=',CZIL
!     PRINT*,'AKMS=',AKMS
!     PRINT*,'AKHS=',AKHS
!     PRINT*,'----------------------------'

      END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SFCDIF_off
! ----------------------------------------------------------------------

END MODULE module_sf_noahlsm
