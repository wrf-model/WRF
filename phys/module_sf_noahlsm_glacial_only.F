MODULE module_sf_noahlsm_glacial_only
  USE module_model_constants
  USE module_sf_noahlsm, ONLY : RD, SIGMA, CPH2O, CPICE, LSUBF, EMISSI_S, ROSR12
  USE module_sf_noahlsm, ONLY : LVCOEF_DATA

  PRIVATE :: ALCALC
  PRIVATE :: CSNOW
  PRIVATE :: HRTICE
  PRIVATE :: HSTEP
  PRIVATE :: PENMAN
  PRIVATE :: SHFLX
  PRIVATE :: SNOPAC
  PRIVATE :: SNOWPACK
  PRIVATE :: SNOWZ0
  PRIVATE :: SNOW_NEW

  integer, private :: iloc, jloc
!$omp threadprivate(iloc, jloc)

CONTAINS

  SUBROUTINE SFLX_GLACIAL (IILOC,JJLOC,ISICE,FFROZP,DT,ZLVL,NSOIL,SLDPTH,     &    !C
       &                   LWDN,SOLNET,SFCPRS,PRCP,SFCTMP,Q2,           &    !F
       &                   TH2,Q2SAT,DQSDT2,                            &    !I
       &                   ALB, SNOALB,TBOT, Z0BRD, Z0, EMISSI, EMBRD,  &    !S
       &                   T1,STC,SNOWH,SNEQV,ALBEDO,CH,                &    !H
! ----------------------------------------------------------------------
! OUTPUTS, DIAGNOSTICS, PARAMETERS BELOW GENERALLY NOT NECESSARY WHEN
! COUPLED WITH E.G. A NWP MODEL (SUCH AS THE NOAA/NWS/NCEP MESOSCALE ETA
! MODEL).  OTHER APPLICATIONS MAY REQUIRE DIFFERENT OUTPUT VARIABLES.
! ----------------------------------------------------------------------
       &                   ETA,SHEAT, ETA_KINEMATIC,FDOWN,              &    !O
       &                   ESNOW,DEW,                                   &    !O
       &                   ETP,SSOIL,                                   &    !O
       &                   FLX1,FLX2,FLX3,                              &    !O
       &                   SNOMLT,SNCOVR,                               &    !O
       &                   RUNOFF1,                                     &    !O
       &                   Q1,                                          &    !D
       &                   SNOTIME1,                                    &
       &                   RIBB)
! ----------------------------------------------------------------------
! SUB-DRIVER FOR "Noah LSM" FAMILY OF PHYSICS SUBROUTINES FOR A
! SOIL/VEG/SNOWPACK LAND-SURFACE MODEL TO UPDATE ICE TEMPERATURE, SKIN 
! TEMPERATURE, SNOWPACK WATER CONTENT, SNOWDEPTH, AND ALL TERMS OF THE 
! SURFACE ENERGY BALANCE (EXCLUDING INPUT ATMOSPHERIC FORCINGS OF 
! DOWNWARD RADIATION AND PRECIP)
! ----------------------------------------------------------------------
! SFLX ARGUMENT LIST KEY:
! ----------------------------------------------------------------------
!  C  CONFIGURATION INFORMATION
!  F  FORCING DATA
!  I  OTHER (INPUT) FORCING DATA
!  S  SURFACE CHARACTERISTICS
!  H  HISTORY (STATE) VARIABLES
!  O  OUTPUT VARIABLES
!  D  DIAGNOSTIC OUTPUT
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
! 3. FORCING DATA (F):
! ----------------------------------------------------------------------
!   LWDN       LW DOWNWARD RADIATION (W M-2; POSITIVE, NOT NET LONGWAVE)
!   SOLNET     NET DOWNWARD SOLAR RADIATION ((W M-2; POSITIVE)
!   SFCPRS     PRESSURE AT HEIGHT ZLVL ABOVE GROUND (PASCALS)
!   PRCP       PRECIP RATE (KG M-2 S-1) (NOTE, THIS IS A RATE)
!   SFCTMP     AIR TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   TH2        AIR POTENTIAL TEMPERATURE (K) AT HEIGHT ZLVL ABOVE GROUND
!   Q2         MIXING RATIO AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
!   FFROZP     FRACTION OF FROZEN PRECIPITATION
! ----------------------------------------------------------------------
! 4. OTHER FORCING (INPUT) DATA (I):
! ----------------------------------------------------------------------
!   Q2SAT      SAT SPECIFIC HUMIDITY AT HEIGHT ZLVL ABOVE GROUND (KG KG-1)
!   DQSDT2     SLOPE OF SAT SPECIFIC HUMIDITY CURVE AT T=SFCTMP
!                (KG KG-1 K-1)
! ----------------------------------------------------------------------
! 5. CANOPY/SOIL CHARACTERISTICS (S):
! ----------------------------------------------------------------------
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
!   EMBRD      Background surface emissivity (between 0 and 1)
!   EMISSI     Surface emissivity (between 0 and 1)
! ----------------------------------------------------------------------
! 6. HISTORY (STATE) VARIABLES (H):
! ----------------------------------------------------------------------
!  T1          GROUND/CANOPY/SNOWPACK) EFFECTIVE SKIN TEMPERATURE (K)
!  STC(NSOIL)  SOIL TEMP (K)
!  SNOWH       ACTUAL SNOW DEPTH (M)
!  SNEQV       LIQUID WATER-EQUIVALENT SNOW DEPTH (M)
!                NOTE: SNOW DENSITY = SNEQV/SNOWH
!  ALBEDO      SURFACE ALBEDO INCLUDING SNOW EFFECT (UNITLESS FRACTION)
!                =SNOW-FREE ALBEDO (ALB) WHEN SNEQV=0, OR
!                =FCT(MSNOALB,ALB,SHDFAC,SHDMIN) WHEN SNEQV>0
!  CH          SURFACE EXCHANGE COEFFICIENT FOR HEAT AND MOISTURE
!                (M S-1); NOTE: CH IS TECHNICALLY A CONDUCTANCE SINCE
!                IT HAS BEEN MULTIPLIED BY WIND SPEED.
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
!   SHEAT      SENSIBLE HEAT FLUX (W M-2: NEGATIVE, IF UPWARD FROM
!              SURFACE)
!   FDOWN      Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
! ----------------------------------------------------------------------
!   ESNOW      SUBLIMATION FROM (OR DEPOSITION TO IF <0) SNOWPACK
!                (W m-2)
!   DEW        DEWFALL (OR FROSTFALL FOR T<273.15) (M)
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! 8. DIAGNOSTIC OUTPUT (D):
! ----------------------------------------------------------------------
!   Q1         Effective mixing ratio at surface (kg kg-1), used for
!              diagnosing the mixing ratio at 2 meter for coupled model
!  Documentation for SNOTIME1 and SNOABL2 ?????
!  What categories of arguments do these variables fall into ????
!  Documentation for RIBB ?????
!  What category of argument does RIBB fall into ?????
! ----------------------------------------------------------------------

    IMPLICIT NONE
! ----------------------------------------------------------------------
    integer, intent(in) ::  iiloc, jjloc
    INTEGER, INTENT(IN) ::  ISICE
! ----------------------------------------------------------------------
    LOGICAL             ::  FRZGRA, SNOWNG

! ----------------------------------------------------------------------
! 1. CONFIGURATION INFORMATION (C):
! ----------------------------------------------------------------------
    INTEGER, INTENT(IN) ::  NSOIL
    INTEGER             ::  KZ

! ----------------------------------------------------------------------
! 2. LOGICAL:
! ----------------------------------------------------------------------

    REAL, INTENT(IN)   :: DT,DQSDT2,LWDN,PRCP,     &
         &                Q2,Q2SAT,SFCPRS,SFCTMP, SNOALB,          &
         &                SOLNET,TBOT,TH2,ZLVL,FFROZP
    REAL, INTENT(OUT)  :: EMBRD, ALBEDO
    REAL, INTENT(INOUT):: CH,SNEQV,SNCOVR,SNOWH,T1,Z0BRD,EMISSI,ALB
    REAL, INTENT(INOUT):: SNOTIME1
    REAL, INTENT(INOUT):: RIBB
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: SLDPTH
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) ::  STC
    REAL, DIMENSION(1:NSOIL) ::   ZSOIL

    REAL,INTENT(OUT)   :: ETA_KINEMATIC,DEW,ESNOW,ETA,  &
         &                ETP,FLX1,FLX2,FLX3,SHEAT,RUNOFF1,    &
         &                SSOIL,SNOMLT,FDOWN,Q1
    REAL               :: DF1,DSOIL,DTOT,FRCSNO,FRCSOI,          &
         &                PRCP1,RCH,RR,RSNOW,SNDENS,SNCOND,SN_NEW,     &
         &                T1V,T24,T2V,TH2V,TSNOW,Z0,PRCPF,RHO

! ----------------------------------------------------------------------
! DECLARATIONS - PARAMETERS
! ----------------------------------------------------------------------
    REAL, PARAMETER :: TFREEZ = 273.15
    REAL, PARAMETER :: LVH2O = 2.501E+6
    REAL, PARAMETER :: LSUBS = 2.83E+6
    REAL, PARAMETER :: R = 287.04

! ----------------------------------------------------------------------
    iloc = iiloc
    jloc = jjloc
! ----------------------------------------------------------------------
    ZSOIL (1) = - SLDPTH (1)
    DO KZ = 2,NSOIL
       ZSOIL (KZ) = - SLDPTH (KZ) + ZSOIL (KZ -1)
    END DO

! ----------------------------------------------------------------------
! IF S.W.E. (SNEQV) BELOW THRESHOLD LOWER BOUND (0.10 M FOR GLACIAL 
! ICE), THEN SET AT LOWER BOUND
! ----------------------------------------------------------------------
    IF ( SNEQV < 0.10 ) THEN
       SNEQV = 0.10
       SNOWH = 0.50
    ENDIF
! ----------------------------------------------------------------------
! IF INPUT SNOWPACK IS NONZERO, THEN COMPUTE SNOW DENSITY "SNDENS" AND
! SNOW THERMAL CONDUCTIVITY "SNCOND"
! ----------------------------------------------------------------------
    SNDENS = SNEQV / SNOWH
    IF(SNDENS > 1.0) THEN
       CALL wrf_error_fatal ( 'Physical snow depth is less than snow water equiv.' )
    ENDIF

    CALL CSNOW (SNCOND,SNDENS)
! ----------------------------------------------------------------------
! DETERMINE IF IT'S PRECIPITATING AND WHAT KIND OF PRECIP IT IS.
! IF IT'S PRCPING AND THE AIR TEMP IS COLDER THAN 0 C, IT'S SNOWING!
! IF IT'S PRCPING AND THE AIR TEMP IS WARMER THAN 0 C, BUT THE GRND
! TEMP IS COLDER THAN 0 C, FREEZING RAIN IS PRESUMED TO BE FALLING.
! ----------------------------------------------------------------------

    SNOWNG = .FALSE.
    FRZGRA = .FALSE.
    IF (PRCP > 0.0) THEN
! ----------------------------------------------------------------------
! Snow defined when fraction of frozen precip (FFROZP) > 0.5,
! passed in from model microphysics.
! ----------------------------------------------------------------------
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

! ----------------------------------------------------------------------
! kmh 09/04/2006 set Snow Density at 0.2 g/cm**3
! for "cold permanent ice" or new "dry" snow
! if soil temperature less than 268.15 K, treat as typical 
! Antarctic/Greenland snow firn
! ----------------------------------------------------------------------
       IF ( SNCOVR .GT. 0.99 ) THEN
          IF ( STC(1) .LT. (TFREEZ - 5.) ) SNDENS = 0.2
          IF ( SNOWNG .AND. (T1.LT.273.) .AND. (SFCTMP.LT.273.) ) SNDENS=0.2
       ENDIF

       CALL CSNOW (SNCOND,SNDENS)

! ----------------------------------------------------------------------
! PRECIP IS LIQUID (RAIN), HENCE SAVE IN THE PRECIP VARIABLE THAT
! LATER CAN WHOLELY OR PARTIALLY INFILTRATE THE SOIL
! ----------------------------------------------------------------------
    ELSE
       PRCPF = PRCP
    ENDIF

! ----------------------------------------------------------------------
! DETERMINE SNOW FRACTIONAL COVERAGE.
!      KWM:  Set SNCOVR to 1.0 because SNUP is set small in VEGPARM.TBL,
!      and SNEQV is at least 0.1 (as set above)
! ----------------------------------------------------------------------
    SNCOVR = 1.0 

! ----------------------------------------------------------------------
! DETERMINE SURFACE ALBEDO MODIFICATION DUE TO SNOWDEPTH STATE.
! ----------------------------------------------------------------------

    CALL ALCALC (ALB,SNOALB,EMBRD,T1,ALBEDO,EMISSI,   &
         &       DT,SNOWNG,SNOTIME1) 

! ----------------------------------------------------------------------
! THERMAL CONDUCTIVITY 
! ----------------------------------------------------------------------
    DF1 = SNCOND

    DSOIL = - (0.5 * ZSOIL (1))
    DTOT = SNOWH + DSOIL
    FRCSNO = SNOWH / DTOT

! 1. HARMONIC MEAN (SERIES FLOW)
!        DF1 = (SNCOND*DF1)/(FRCSOI*SNCOND+FRCSNO*DF1)
    FRCSOI = DSOIL / DTOT

! 3. GEOMETRIC MEAN (INTERMEDIATE BETWEEN HARMONIC AND ARITHMETIC MEAN)
!        DF1 = (SNCOND**FRCSNO)*(DF1**FRCSOI)
    DF1 = FRCSNO * SNCOND + FRCSOI * DF1

! ----------------------------------------------------------------------
! CALCULATE SUBSURFACE HEAT FLUX, SSOIL, FROM FINAL THERMAL DIFFUSIVITY
! OF SURFACE MEDIUMS, DF1 ABOVE, AND SKIN TEMPERATURE AND TOP
! MID-LAYER SOIL TEMPERATURE
! ----------------------------------------------------------------------
    IF ( DTOT .GT. 2.*DSOIL ) then
       DTOT = 2.*DSOIL
    ENDIF
    SSOIL = DF1 * ( T1 - STC(1) ) / DTOT

! ----------------------------------------------------------------------
! DETERMINE SURFACE ROUGHNESS OVER SNOWPACK USING SNOW CONDITION FROM
! THE PREVIOUS TIMESTEP.
! ----------------------------------------------------------------------

    CALL SNOWZ0 (Z0,Z0BRD,SNOWH)

! ----------------------------------------------------------------------
! CALCULATE TOTAL DOWNWARD RADIATION (SOLAR PLUS LONGWAVE) NEEDED IN
! PENMAN EP SUBROUTINE THAT FOLLOWS
! ----------------------------------------------------------------------

    FDOWN = SOLNET + LWDN

! ----------------------------------------------------------------------
! CALC VIRTUAL TEMPS AND VIRTUAL POTENTIAL TEMPS NEEDED BY SUBROUTINES
! PENMAN.
! ----------------------------------------------------------------------

    T2V = SFCTMP * (1.0+ 0.61 * Q2 )
    RHO = SFCPRS / (RD * T2V)
    RCH = RHO * 1004.6 * CH
    T24 = SFCTMP * SFCTMP * SFCTMP * SFCTMP

! ----------------------------------------------------------------------
! CALL PENMAN SUBROUTINE TO CALCULATE POTENTIAL EVAPORATION (ETP), AND
! OTHER PARTIAL PRODUCTS AND SUMS SAVE IN COMMON/RITE FOR LATER
! CALCULATIONS.
! ----------------------------------------------------------------------

    ! PENMAN returns ETP, FLX2, and RR
    CALL PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL,     &
         &       Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,             &
         &       DQSDT2,FLX2,EMISSI,T1)

    CALL SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,NSOIL,DT,DF1,        &
         &       Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,          &
         &       SFCPRS,RCH,RR,SNEQV,SNDENS,SNOWH,ZSOIL,TBOT,   &
         &       SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,RIBB)

!   ETA_KINEMATIC =  ESNOW
    ETA_KINEMATIC =  ETP

! ----------------------------------------------------------------------
! Effective mixing ratio at grnd level (skin)
! ----------------------------------------------------------------------
    Q1=Q2+ETA_KINEMATIC*CP/RCH

! ----------------------------------------------------------------------
! DETERMINE SENSIBLE HEAT (H) IN ENERGY UNITS (W M-2)
! ----------------------------------------------------------------------
    SHEAT = - (CH * CP * SFCPRS)/ (R * T2V) * ( TH2- T1 )

! ----------------------------------------------------------------------
! CONVERT EVAP TERMS FROM KINEMATIC (KG M-2 S-1) TO ENERGY UNITS (W M-2)
! ----------------------------------------------------------------------
    ESNOW = ESNOW * LSUBS
    ETP   = ETP   * LSUBS
    IF (ETP .GT. 0.) THEN
       ETA = ESNOW
    ELSE
       ETA = ETP
    ENDIF

! ----------------------------------------------------------------------
! CONVERT THE SIGN OF SOIL HEAT FLUX SO THAT:
!   SSOIL>0: WARM THE SURFACE  (NIGHT TIME)
!   SSOIL<0: COOL THE SURFACE  (DAY TIME)
! ----------------------------------------------------------------------
    SSOIL = -1.0* SSOIL

! ----------------------------------------------------------------------
! FOR THE CASE OF GLACIAL-ICE, ADD ANY SNOWMELT DIRECTLY TO SURFACE 
! RUNOFF (RUNOFF1) SINCE THERE IS NO SOIL MEDIUM
! ----------------------------------------------------------------------
    RUNOFF1 = SNOMLT / DT

! ----------------------------------------------------------------------
  END SUBROUTINE SFLX_GLACIAL
! ----------------------------------------------------------------------

  SUBROUTINE ALCALC (ALB,SNOALB,EMBRD,TSNOW,ALBEDO,EMISSI,   &
       &             DT,SNOWNG,SNOTIME1)

! ----------------------------------------------------------------------
! CALCULATE ALBEDO INCLUDING SNOW EFFECT (0 -> 1)
!   ALB     SNOWFREE ALBEDO
!   SNOALB  MAXIMUM (DEEP) SNOW ALBEDO
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
    REAL,    INTENT(IN)    :: ALB, SNOALB, EMBRD, TSNOW
    REAL,    INTENT(IN)    :: DT
    LOGICAL, INTENT(IN)    :: SNOWNG
    REAL,    INTENT(INOUT) :: SNOTIME1
    REAL,    INTENT(OUT)   :: ALBEDO, EMISSI
    REAL                   :: SNOALB2
    REAL                   :: TM,SNOALB1
    REAL,    PARAMETER     :: SNACCA=0.94,SNACCB=0.58,SNTHWA=0.82,SNTHWB=0.46
! turn off vegetation effect
!      ALBEDO = ALB + (1.0- (SHDFAC - SHDMIN))* SNCOVR * (SNOALB - ALB)
!      ALBEDO = (1.0-SNCOVR)*ALB + SNCOVR*SNOALB !this is equivalent to below
    ALBEDO = ALB + (SNOALB-ALB)
    EMISSI = EMBRD + (EMISSI_S - EMBRD)

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
    SNOALB1 = SNOALB+LVCOEF_DATA*(0.85-SNOALB)
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

    SNOALB2 = MAX ( SNOALB2, ALB )
    ALBEDO = ALB + (SNOALB2-ALB)
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

  SUBROUTINE CSNOW (SNCOND,DSNOW)

! ----------------------------------------------------------------------
! CALCULATE SNOW TERMAL CONDUCTIVITY
! ----------------------------------------------------------------------
    IMPLICIT NONE
    REAL, INTENT(IN)  :: DSNOW
    REAL, INTENT(OUT) :: SNCOND
    REAL              :: C
    REAL, PARAMETER   :: UNIT = 0.11631

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

  SUBROUTINE HRTICE (RHSTS,STC,TBOT,NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

! ----------------------------------------------------------------------
! CALCULATE THE RIGHT HAND SIDE OF THE TIME TENDENCY TERM OF THE SOIL
! THERMAL DIFFUSION EQUATION IN THE CASE OF SEA-ICE (ICE=1) OR GLACIAL
! ICE (ICE=-1). COMPUTE (PREPARE) THE MATRIX COEFFICIENTS FOR THE
! TRI-DIAGONAL MATRIX OF THE IMPLICIT TIME SCHEME.
!
! (NOTE:  THIS SUBROUTINE ONLY CALLED FOR SEA-ICE OR GLACIAL ICE, BUT
! NOT FOR NON-GLACIAL LAND (ICE = 0).
! ----------------------------------------------------------------------
    IMPLICIT NONE


    INTEGER,                  INTENT(IN)  :: NSOIL
    REAL,                     INTENT(IN)  :: DF1,YY,ZZ1
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: AI, BI,CI
    REAL, DIMENSION(1:NSOIL), INTENT(IN)  :: STC, ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(OUT) :: RHSTS
    REAL,                     INTENT(IN)  :: TBOT
    INTEGER                :: K
    REAL                   :: DDZ,DDZ2,DENOM,DTSDZ,DTSDZ2,SSOIL,HCPCT
    REAL                   :: DF1K,DF1N
    REAL                   :: ZMD
    REAL, PARAMETER        :: ZBOT = -25.0

! ----------------------------------------------------------------------
! SET A NOMINAL UNIVERSAL VALUE OF GLACIAL-ICE SPECIFIC HEAT CAPACITY,
!   HCPCT = 2100.0*900.0 = 1.89000E+6 (SOURCE:  BOB GRUMBINE, 2005)
!   TBOT PASSED IN AS ARGUMENT, VALUE FROM GLOBAL DATA SET
    !
    ! A least-squares fit for the four points provided by
    ! Keith Hines for the Yen (1981) values for Antarctic
    ! snow firn.
    !
    HCPCT = 1.E6 * (0.8194 - 0.1309*0.5*ZSOIL(1))
    DF1K = DF1

! ----------------------------------------------------------------------
! THE INPUT ARGUMENT DF1 IS A UNIVERSALLY CONSTANT VALUE OF SEA-ICE
! THERMAL DIFFUSIVITY, SET IN ROUTINE SNOPAC AS DF1 = 2.2.
! ----------------------------------------------------------------------
! SET ICE PACK DEPTH.  USE TBOT AS ICE PACK LOWER BOUNDARY TEMPERATURE
! (THAT OF UNFROZEN SEA WATER AT BOTTOM OF SEA ICE PACK).  ASSUME ICE
! PACK IS OF N=NSOIL LAYERS SPANNING A UNIFORM CONSTANT ICE PACK
! THICKNESS AS DEFINED BY ZSOIL(NSOIL) IN ROUTINE SFLX.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! CALC THE MATRIX COEFFICIENTS AI, BI, AND CI FOR THE TOP LAYER
! ----------------------------------------------------------------------
    DDZ = 1.0 / ( -0.5 * ZSOIL (2) )
    AI (1) = 0.0
    CI (1) = (DF1 * DDZ) / (ZSOIL (1) * HCPCT)

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT BTWN THE TOP AND 2ND SOIL LAYERS.
! RECALC/ADJUST THE SOIL HEAT FLUX.  USE THE GRADIENT AND FLUX TO CALC
! RHSTS FOR THE TOP SOIL LAYER.
! ----------------------------------------------------------------------
    BI (1) = - CI (1) + DF1/ (0.5 * ZSOIL (1) * ZSOIL (1) * HCPCT *    &
         &   ZZ1)
    DTSDZ = ( STC (1) - STC (2) ) / ( -0.5 * ZSOIL (2) )
    SSOIL = DF1 * ( STC (1) - YY ) / ( 0.5 * ZSOIL (1) * ZZ1 )

! ----------------------------------------------------------------------
! INITIALIZE DDZ2
! ----------------------------------------------------------------------
    RHSTS (1) = ( DF1 * DTSDZ - SSOIL ) / ( ZSOIL (1) * HCPCT )

! ----------------------------------------------------------------------
! LOOP THRU THE REMAINING SOIL LAYERS, REPEATING THE ABOVE PROCESS
! ----------------------------------------------------------------------
    DDZ2 = 0.0
    DF1K = DF1
    DF1N = DF1
    DO K = 2,NSOIL

       ZMD = 0.5 * (ZSOIL(K)+ZSOIL(K-1))
       ! For the land-ice case
! kmh 09/03/2006 use Yen (1981)'s values for Antarctic snow firn
!         IF ( K .eq. 2 ) HCPCT = 0.855108E6
!         IF ( K .eq. 3 ) HCPCT = 0.922906E6
!         IF ( K .eq. 4 ) HCPCT = 1.009986E6

       ! Least squares fit to the four points supplied by Keith Hines
       ! from Yen (1981) for Antarctic snow firn.  Not optimal, but
       ! probably better than just a constant.
       HCPCT = 1.E6 * ( 0.8194 - 0.1309*ZMD )

!         IF ( K .eq. 2 ) DF1N = 0.345356
!         IF ( K .eq. 3 ) DF1N = 0.398777
!         IF ( K .eq. 4 ) DF1N = 0.472653

       ! Least squares fit to the three points supplied by Keith Hines
       ! from Yen (1981) for Antarctic snow firn.  Not optimal, but
       ! probably better than just a constant.
       DF1N = 0.32333 - ( 0.10073 * ZMD )
! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THIS LAYER.
! ----------------------------------------------------------------------
       IF (K /= NSOIL) THEN
          DENOM = 0.5 * ( ZSOIL (K -1) - ZSOIL (K +1) )

! ----------------------------------------------------------------------
! CALC THE MATRIX COEF, CI, AFTER CALC'NG ITS PARTIAL PRODUCT.
! ----------------------------------------------------------------------
          DTSDZ2 = ( STC (K) - STC (K +1) ) / DENOM
          DDZ2 = 2. / (ZSOIL (K -1) - ZSOIL (K +1))
          CI (K) = - DF1N * DDZ2 / ( (ZSOIL (K -1) - ZSOIL (K))*HCPCT)

! ----------------------------------------------------------------------
! CALC THE VERTICAL SOIL TEMP GRADIENT THRU THE LOWEST LAYER.
! ----------------------------------------------------------------------
       ELSE

! ----------------------------------------------------------------------
! SET MATRIX COEF, CI TO ZERO.
! ----------------------------------------------------------------------
          DTSDZ2 = (STC (K) - TBOT)/ (.5 * (ZSOIL (K -1) + ZSOIL (K)) &
               &   - ZBOT)
          CI (K) = 0.
! ----------------------------------------------------------------------
! CALC RHSTS FOR THIS LAYER AFTER CALC'NG A PARTIAL PRODUCT.
! ----------------------------------------------------------------------
       END IF
       DENOM = ( ZSOIL (K) - ZSOIL (K -1) ) * HCPCT

! ----------------------------------------------------------------------
! CALC MATRIX COEFS, AI, AND BI FOR THIS LAYER.
! ----------------------------------------------------------------------
       RHSTS (K) = ( DF1N * DTSDZ2- DF1K * DTSDZ ) / DENOM
       AI (K) = - DF1K * DDZ / ( (ZSOIL (K -1) - ZSOIL (K)) * HCPCT)

! ----------------------------------------------------------------------
! RESET VALUES OF DTSDZ AND DDZ FOR LOOP TO NEXT SOIL LYR.
! ----------------------------------------------------------------------
       BI (K) = - (AI (K) + CI (K))
       DF1K = DF1N
       DTSDZ = DTSDZ2
       DDZ = DDZ2
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE HRTICE
! ----------------------------------------------------------------------

  SUBROUTINE HSTEP (STCOUT,STCIN,RHSTS,DT,NSOIL,AI,BI,CI)

! ----------------------------------------------------------------------
! CALCULATE/UPDATE THE SOIL TEMPERATURE FIELD.
! ----------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: STCIN
    REAL, DIMENSION(1:NSOIL), INTENT(OUT)   :: STCOUT
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: RHSTS
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: AI,BI,CI
    REAL, DIMENSION(1:NSOIL) :: RHSTSin
    REAL, DIMENSION(1:NSOIL) :: CIin
    REAL                     :: DT
    INTEGER                  :: K

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

  SUBROUTINE PENMAN (SFCTMP,SFCPRS,CH,TH2,PRCP,FDOWN,T24,SSOIL, &
       &             Q2,Q2SAT,ETP,RCH,RR,SNOWNG,FRZGRA,       &
       &             DQSDT2,FLX2,EMISSI,T1)

! ----------------------------------------------------------------------
! CALCULATE POTENTIAL EVAPORATION FOR THE CURRENT POINT.  VARIOUS
! PARTIAL SUMS/PRODUCTS ARE ALSO CALCULATED AND PASSED BACK TO THE
! CALLING ROUTINE FOR LATER USE.
! ----------------------------------------------------------------------
    IMPLICIT NONE
    LOGICAL, INTENT(IN)  :: SNOWNG, FRZGRA
    REAL,    INTENT(IN)  :: CH, DQSDT2,FDOWN,PRCP,Q2,Q2SAT,SSOIL,SFCPRS, &
         &                  SFCTMP,TH2,EMISSI,T1,RCH,T24
    REAL,    INTENT(OUT) :: ETP,FLX2,RR

    REAL                 :: A, DELTA, FNET,RAD,ELCP1,LVS,EPSCA

    REAL, PARAMETER      :: ELCP = 2.4888E+3, LSUBC = 2.501000E+6
    REAL, PARAMETER      :: LSUBS = 2.83E+6

! ----------------------------------------------------------------------
! PREPARE PARTIAL QUANTITIES FOR PENMAN EQUATION.
! ----------------------------------------------------------------------
    IF ( T1 > 273.15 ) THEN
       ELCP1 = ELCP
       LVS   = LSUBC
    ELSE
       ELCP1 = ELCP*LSUBS/LSUBC
       LVS   = LSUBS
    ENDIF
    DELTA = ELCP1 * DQSDT2
    A = ELCP1 * (Q2SAT - Q2)
    RR = EMISSI*T24 * 6.48E-8 / (SFCPRS * CH) + 1.0

! ----------------------------------------------------------------------
! ADJUST THE PARTIAL SUMS / PRODUCTS WITH THE LATENT HEAT
! EFFECTS CAUSED BY FALLING PRECIPITATION.
! ----------------------------------------------------------------------
    IF (.NOT. SNOWNG) THEN
       IF (PRCP >  0.0) RR = RR + CPH2O * PRCP / RCH
    ELSE
       RR = RR + CPICE * PRCP / RCH
    END IF

! ----------------------------------------------------------------------
! INCLUDE THE LATENT HEAT EFFECTS OF FREEZING RAIN CONVERTING TO ICE ON
! IMPACT IN THE CALCULATION OF FLX2 AND FNET.
! ----------------------------------------------------------------------
    IF (FRZGRA) THEN
       FLX2 = - LSUBF * PRCP
    ELSE
       FLX2 = 0.0
    ENDIF
    FNET = FDOWN -  ( EMISSI * SIGMA * T24 ) - SSOIL - FLX2

! ----------------------------------------------------------------------
! FINISH PENMAN EQUATION CALCULATIONS.
! ----------------------------------------------------------------------
    RAD = FNET / RCH + TH2 - SFCTMP
    EPSCA = (A * RR + RAD * DELTA) / (DELTA + RR)
    ETP = EPSCA * RCH / LVS

! ----------------------------------------------------------------------
  END SUBROUTINE PENMAN
! ----------------------------------------------------------------------

  SUBROUTINE SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)
! ----------------------------------------------------------------------
! UPDATE THE TEMPERATURE STATE OF THE SOIL COLUMN BASED ON THE THERMAL
! DIFFUSION EQUATION AND UPDATE THE FROZEN SOIL MOISTURE CONTENT BASED
! ON THE TEMPERATURE.
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER,                  INTENT(IN)    :: NSOIL
    REAL,                     INTENT(IN)    :: DF1,DT,TBOT,YY, ZZ1
    REAL, DIMENSION(1:NSOIL), INTENT(IN)    :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC

    REAL, DIMENSION(1:NSOIL) :: AI, BI, CI, STCF,RHSTS
    INTEGER                  :: I
    REAL, PARAMETER          :: T0 = 273.15

! ----------------------------------------------------------------------
! HRT ROUTINE CALCS THE RIGHT HAND SIDE OF THE SOIL TEMP DIF EQN
! ----------------------------------------------------------------------

    CALL HRTICE (RHSTS,STC,TBOT, NSOIL,ZSOIL,YY,ZZ1,DF1,AI,BI,CI)

    CALL HSTEP (STCF,STC,RHSTS,DT,NSOIL,AI,BI,CI)

    DO I = 1,NSOIL
       STC (I) = STCF (I)
    END DO
! ----------------------------------------------------------------------
  END SUBROUTINE SHFLX
! ----------------------------------------------------------------------

  SUBROUTINE SNOPAC (ETP,ETA,PRCP,PRCPF,SNOWNG,NSOIL,DT,DF1,           &
       &             Q2,T1,SFCTMP,T24,TH2,FDOWN,SSOIL,STC,             &
       &             SFCPRS,RCH,RR,SNEQV,SNDENS,SNOWH,ZSOIL,TBOT,      &
       &             SNOMLT,DEW,FLX1,FLX2,FLX3,ESNOW,EMISSI,RIBB)

! ----------------------------------------------------------------------
! CALCULATE SOIL MOISTURE AND HEAT FLUX VALUES & UPDATE SOIL MOISTURE
! CONTENT AND SOIL HEAT CONTENT VALUES FOR THE CASE WHEN A SNOW PACK IS
! PRESENT.
! ----------------------------------------------------------------------
    IMPLICIT NONE

    INTEGER, INTENT(IN)   :: NSOIL
    LOGICAL, INTENT(IN)   :: SNOWNG
    REAL,    INTENT(IN)   :: DF1,DT,FDOWN,PRCP,Q2,RCH,RR,SFCPRS,SFCTMP, &
         &                   T24,TBOT,TH2,EMISSI
    REAL, INTENT(INOUT)   :: SNEQV,FLX2,PRCPF,SNOWH,SNDENS,T1,RIBB,ETP
    REAL, INTENT(OUT)     :: DEW,ESNOW,FLX1,FLX3,SSOIL,SNOMLT
    REAL, DIMENSION(1:NSOIL),INTENT(IN)     :: ZSOIL
    REAL, DIMENSION(1:NSOIL), INTENT(INOUT) :: STC
    REAL, DIMENSION(1:NSOIL) :: ET1
    INTEGER               :: K
    REAL                  :: DENOM,DSOIL,DTOT,ESDFLX,ETA,        &
         &                   ESNOW1,ESNOW2,ETA1,ETP1,ETP2,       &
         &                   ETP3,ETANRG,EX,                     &
         &                   FRCSNO,FRCSOI,PRCP1,QSAT,RSNOW,SEH, &
         &                   SNCOND,T12,T12A,T12B,T14,YY,ZZ1

    REAL, PARAMETER       :: ESDMIN = 1.E-6, LSUBC = 2.501000E+6,     &
         &                   LSUBS = 2.83E+6, TFREEZ = 273.15,        &
         &                   SNOEXP = 2.0

! ----------------------------------------------------------------------
! FOR GLACIAL-ICE, SNOWCOVER FRACTION = 1.0, AND SUBLIMATION IS AT THE 
! POTENTIAL RATE.
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
    SNOMLT = 0.0
    DEW = 0.
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
    IF (ETP <= 0.0) THEN
       IF ( ( RIBB >= 0.1 ) .AND. ( FDOWN > 150.0 ) ) THEN
          ETP=(MIN(ETP*(1.0-RIBB),0.)/0.980 + ETP*(0.980-1.0))/0.980
       ENDIF
       ETP1 = ETP * 0.001
       DEW = -ETP1
       ESNOW2 = ETP1*DT
       ETANRG = ETP*LSUBS
    ELSE
       ETP1 = ETP * 0.001
       ESNOW = ETP
       ESNOW1 = ESNOW*0.001
       ESNOW2 = ESNOW1*DT
       ETANRG = ESNOW*LSUBS
    END IF

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
    END IF
! ----------------------------------------------------------------------
! CALCULATE AN 'EFFECTIVE SNOW-GRND SFC TEMP' (T12) BASED ON HEAT FLUXES
! BETWEEN THE SNOW PACK AND THE SOIL AND ON NET RADIATION.
! INCLUDE FLX1 (PRECIP-SNOW SFC) AND FLX2 (FREEZING RAIN LATENT HEAT)
! FLUXES.  FLX1 FROM ABOVE, FLX2 BROUGHT IN VIA COMMOM BLOCK RITE.
! FLX2 REFLECTS FREEZING RAIN LATENT HEAT FLUX USING T1 CALCULATED IN
! PENMAN.
! ----------------------------------------------------------------------
    DSOIL = - (0.5 * ZSOIL (1))
    DTOT = SNOWH + DSOIL
    DENOM = 1.0+ DF1 / (DTOT * RR * RCH)
    T12A = ( (FDOWN - FLX1- FLX2- EMISSI * SIGMA * T24)/ RCH                    &
         + TH2- SFCTMP - ETANRG / RCH ) / RR
    T12B = DF1 * STC (1) / (DTOT * RR * RCH)

    T12 = (SFCTMP + T12A + T12B) / DENOM
    IF (T12 <=  TFREEZ) THEN
! ----------------------------------------------------------------------
! SUB-FREEZING BLOCK
! ----------------------------------------------------------------------
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
       T1 = T12
       SSOIL = DF1 * (T1- STC (1)) / DTOT
       SNEQV = MAX(0.0, SNEQV-ESNOW2)
       FLX3 = 0.0
       EX = 0.0
       SNOMLT = 0.0
    ELSE
! ----------------------------------------------------------------------
! ABOVE FREEZING BLOCK
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
       T1 = TFREEZ
       IF ( DTOT .GT. 2.0*DSOIL ) THEN
          DTOT = 2.0*DSOIL
       ENDIF
       SSOIL = DF1 * (T1- STC (1)) / DTOT
       IF (SNEQV-ESNOW2 <= ESDMIN) THEN
          SNEQV = 0.0
          EX = 0.0
          SNOMLT = 0.0
          FLX3 = 0.0
! ----------------------------------------------------------------------
! SUBLIMATION LESS THAN DEPTH OF SNOWPACK
! SNOWPACK (SNEQV) REDUCED BY ESNOW2 (DEPTH OF SUBLIMATED SNOW)
! ----------------------------------------------------------------------
       ELSE
          SNEQV = SNEQV-ESNOW2
          ETP3 = ETP * LSUBC
          SEH = RCH * (T1- TH2)
          T14 = ( T1 * T1 ) * ( T1 * T1 )
          FLX3 = FDOWN - FLX1- FLX2- EMISSI*SIGMA * T14- SSOIL - SEH - ETANRG
          IF (FLX3 <= 0.0) FLX3 = 0.0
          EX = FLX3*0.001/ LSUBF
          SNOMLT = EX * DT
! ----------------------------------------------------------------------
! ESDMIN REPRESENTS A SNOWPACK DEPTH THRESHOLD VALUE BELOW WHICH WE
! CHOOSE NOT TO RETAIN ANY SNOWPACK, AND INSTEAD INCLUDE IT IN SNOWMELT.
! ----------------------------------------------------------------------
          IF (SNEQV- SNOMLT >=  ESDMIN) THEN
             SNEQV = SNEQV- SNOMLT
          ELSE
! ----------------------------------------------------------------------
! SNOWMELT EXCEEDS SNOW DEPTH
! ----------------------------------------------------------------------
             EX = SNEQV / DT
             FLX3 = EX *1000.0* LSUBF
             SNOMLT = SNEQV

             SNEQV = 0.0
          ENDIF
       ENDIF

! ----------------------------------------------------------------------
! FOR GLACIAL ICE, THE SNOWMELT WILL BE ADDED TO SUBSURFACE
! RUNOFF/BASEFLOW LATER NEAR THE END OF SFLX (AFTER RETURN FROM CALL TO
! SUBROUTINE SNOPAC)
! ----------------------------------------------------------------------

    ENDIF

! ----------------------------------------------------------------------
! BEFORE CALL SHFLX IN THIS SNOWPACK CASE, SET ZZ1 AND YY ARGUMENTS TO
! SPECIAL VALUES THAT ENSURE THAT GROUND HEAT FLUX CALCULATED IN SHFLX
! MATCHES THAT ALREADY COMPUTED FOR BELOW THE SNOWPACK, THUS THE SFC
! HEAT FLUX TO BE COMPUTED IN SHFLX WILL EFFECTIVELY BE THE FLUX AT THE
! SNOW TOP SURFACE.  
! ----------------------------------------------------------------------
    ZZ1 = 1.0
    YY = STC (1) -0.5* SSOIL * ZSOIL (1)* ZZ1/ DF1

! ----------------------------------------------------------------------
! SHFLX WILL CALC/UPDATE THE SOIL TEMPS.
! ----------------------------------------------------------------------
    CALL SHFLX (STC,NSOIL,DT,YY,ZZ1,ZSOIL,TBOT,DF1)

! ----------------------------------------------------------------------
! SNOW DEPTH AND DENSITY ADJUSTMENT BASED ON SNOW COMPACTION.  YY IS
! ASSUMED TO BE THE SOIL TEMPERTURE AT THE TOP OF THE SOIL COLUMN.
! ----------------------------------------------------------------------
    IF (SNEQV .GE. 0.10) THEN
       CALL SNOWPACK (SNEQV,DT,SNOWH,SNDENS,T1,YY)
    ELSE
       SNEQV = 0.10
       SNOWH = 0.50
!KWM???? SNDENS =
!KWM???? SNCOND =
    ENDIF
! ----------------------------------------------------------------------
  END SUBROUTINE SNOPAC
! ----------------------------------------------------------------------

  SUBROUTINE SNOWPACK (SNEQV,DTSEC,SNOWH,SNDENS,TSNOW,TSOIL)

! ----------------------------------------------------------------------
! CALCULATE COMPACTION OF SNOWPACK UNDER CONDITIONS OF INCREASING SNOW
! DENSITY, AS OBTAINED FROM AN APPROXIMATE SOLUTION OF E. ANDERSON'S
! DIFFERENTIAL EQUATION (3.29), NOAA TECHNICAL REPORT NWS 19, BY VICTOR
! KOREN, 03/25/95.
! ----------------------------------------------------------------------
! SNEQV     WATER EQUIVALENT OF SNOW (M)
! DTSEC   TIME STEP (SEC)
! SNOWH   SNOW DEPTH (M)
! SNDENS  SNOW DENSITY (G/CM3=DIMENSIONLESS FRACTION OF H2O DENSITY)
! TSNOW   SNOW SURFACE TEMPERATURE (K)
! TSOIL   SOIL SURFACE TEMPERATURE (K)

! SUBROUTINE WILL RETURN NEW VALUES OF SNOWH AND SNDENS
! ----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER                :: IPOL, J
      REAL, INTENT(IN)       :: SNEQV, DTSEC,TSNOW,TSOIL
      REAL, INTENT(INOUT)    :: SNOWH, SNDENS
      REAL                   :: BFAC,DSX,DTHR,DW,SNOWHC,PEXP,           &
                                TAVGC,TSNOWC,TSOILC,ESDC,ESDCX
      REAL, PARAMETER        :: C1 = 0.01, C2 = 21.0, G = 9.81,         &
                                KN = 4000.0
! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS
! ----------------------------------------------------------------------
      SNOWHC = SNOWH *100.
      ESDC   = SNEQV *100.
      DTHR   = DTSEC /3600.
      TSNOWC = TSNOW -273.15
      TSOILC = TSOIL -273.15

! ----------------------------------------------------------------------
! CALCULATING OF AVERAGE TEMPERATURE OF SNOW PACK
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! CALCULATING OF SNOW DEPTH AND DENSITY AS A RESULT OF COMPACTION
!  SNDENS=DS0*(EXP(BFAC*SNEQV)-1.)/(BFAC*SNEQV)
!  BFAC=DTHR*C1*EXP(0.08*TAVGC-C2*DS0)
! NOTE: BFAC*SNEQV IN SNDENS EQN ABOVE HAS TO BE CAREFULLY TREATED
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
! THE FUNCTION OF THE FORM (e**x-1)/x IMBEDDED IN ABOVE EXPRESSION
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

!       DSX=DSM+DTSEC*0.5*DSM*G*SNEQV/
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
         SNDENS = SNDENS * (1. - DW) + DW
         IF (SNDENS >=  0.40) SNDENS = 0.40
! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH (CM) FROM SNOW WATER EQUIVALENT AND SNOW DENSITY.
! CHANGE SNOW DEPTH UNITS TO METERS
! ----------------------------------------------------------------------
      END IF
      SNOWHC = ESDC / SNDENS
      SNOWH = SNOWHC * 0.01

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWPACK
! ----------------------------------------------------------------------

  SUBROUTINE SNOWZ0 (Z0, Z0BRD, SNOWH)
! ----------------------------------------------------------------------
! CALCULATE TOTAL ROUGHNESS LENGTH OVER SNOW
! Z0      ROUGHNESS LENGTH (m)
! Z0S     SNOW ROUGHNESS LENGTH:=0.001 (m)
! ----------------------------------------------------------------------
    IMPLICIT NONE
    REAL, INTENT(IN)        :: Z0BRD
    REAL, INTENT(OUT)       :: Z0
    REAL, PARAMETER         :: Z0S=0.001
    REAL, INTENT(IN)        :: SNOWH
    REAL                    :: BURIAL
    REAL                    :: Z0EFF

    BURIAL = 7.0*Z0BRD - SNOWH
    IF(BURIAL.LE.0.0007) THEN
       Z0EFF = Z0S
    ELSE      
       Z0EFF = BURIAL/7.0
    ENDIF

    Z0 = Z0EFF

! ----------------------------------------------------------------------
  END SUBROUTINE SNOWZ0
! ----------------------------------------------------------------------

  SUBROUTINE SNOW_NEW (TEMP,NEWSN,SNOWH,SNDENS)

! ----------------------------------------------------------------------
! CALCULATE SNOW DEPTH AND DENSITY TO ACCOUNT FOR THE NEW SNOWFALL.
! UPDATED VALUES OF SNOW DEPTH AND DENSITY ARE RETURNED.

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
! CALCULATING NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
! EQUATION FROM GOTTLIB L. 'A GENERAL RUNOFF MODEL FOR SNOWCOVERED
! AND GLACIERIZED BASIN', 6TH NORDIC HYDROLOGICAL CONFERENCE,
! VEMADOLEN, SWEDEN, 1980, 172-177PP.
!-----------------------------------------------------------------------
    TEMPC = TEMP - 273.15
    IF ( TEMPC <=  -15. ) THEN
       DSNEW = 0.05
    ELSE
       DSNEW = 0.05 + 0.0017 * ( TEMPC + 15. ) ** 1.5
    ENDIF

! ----------------------------------------------------------------------
! CONVERSION INTO SIMULATION UNITS
! ----------------------------------------------------------------------
    SNOWHC = SNOWH * 100.
    NEWSNC = NEWSN * 100.

! ----------------------------------------------------------------------
! ADJUSTMENT OF SNOW DENSITY DEPENDING ON NEW SNOWFALL
! ----------------------------------------------------------------------
    HNEWC = NEWSNC / DSNEW
    IF ( SNOWHC + HNEWC < 1.0E-3 ) THEN
       SNDENS = MAX ( DSNEW , SNDENS )
    ELSE
       SNDENS = ( SNOWHC * SNDENS + HNEWC * DSNEW ) / ( SNOWHC + HNEWC )
    ENDIF
    SNOWHC = SNOWHC + HNEWC
    SNOWH = SNOWHC * 0.01

! ----------------------------------------------------------------------
  END SUBROUTINE SNOW_NEW
! ----------------------------------------------------------------------

END MODULE module_sf_noahlsm_glacial_only
