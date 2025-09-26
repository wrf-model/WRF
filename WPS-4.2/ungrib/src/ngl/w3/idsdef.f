      SUBROUTINE IDSDEF(IPTV,IDS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: IDSDEF         SETS DEFAULT DECIMAL SCALINGS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: SETS DECIMAL SCALINGS DEFAULTS FOR VARIOUS PARAMETERS.
C   A DECIMAL SCALING OF -3 MEANS DATA IS PACKED IN KILO-SI UNITS.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C
C USAGE:    CALL IDSDEF(IPTV,IDS)
C   INPUT ARGUMENTS:
C     IPTV         PARAMTER TABLE VERSION (ONLY 1 OR 2 IS RECOGNIZED)
C   OUTPUT ARGUMENTS:
C     IDS          INTEGER (255) DECIMAL SCALINGS
C                  (UNKNOWN DECIMAL SCALINGS WILL NOT BE SET)
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      DIMENSION IDS(255)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IF(IPTV.EQ.1.OR.IPTV.EQ.2) THEN
        IDS(001)=-1     ! PRESSURE (PA)
        IDS(002)=-1     ! SEA-LEVEL PRESSURE (PA)
        IDS(003)=3      ! PRESSURE TENDENCY (PA/S)
                        !
                        !
        IDS(006)=-1     ! GEOPOTENTIAL (M2/S2)
        IDS(007)=0      ! GEOPOTENTIAL HEIGHT (M)
        IDS(008)=0      ! GEOMETRIC HEIGHT (M)
        IDS(009)=0      ! STANDARD DEVIATION OF HEIGHT (M)
                        !
        IDS(011)=1      ! TEMPERATURE (K)
        IDS(012)=1      ! VIRTUAL TEMPERATURE (K)
        IDS(013)=1      ! POTENTIAL TEMPERATURE (K)
        IDS(014)=1      ! PSEUDO-ADIABATIC POTENTIAL TEMPERATURE (K)
        IDS(015)=1      ! MAXIMUM TEMPERATURE (K)
        IDS(016)=1      ! MINIMUM TEMPERATURE (K)
        IDS(017)=1      ! DEWPOINT TEMPERATURE (K)
        IDS(018)=1      ! DEWPOINT DEPRESSION (K)
        IDS(019)=4      ! TEMPERATURE LAPSE RATE (K/M)
        IDS(020)=0      ! VISIBILITY (M)
                        ! RADAR SPECTRA 1 ()
                        ! RADAR SPECTRA 2 ()
                        ! RADAR SPECTRA 3 ()
                        !
        IDS(025)=1      ! TEMPERATURE ANOMALY (K)
        IDS(026)=-1     ! PRESSURE ANOMALY (PA)
        IDS(027)=0      ! GEOPOTENTIAL HEIGHT ANOMALY (M)
                        ! WAVE SPECTRA 1 ()
                        ! WAVE SPECTRA 2 ()
                        ! WAVE SPECTRA 3 ()
        IDS(031)=0      ! WIND DIRECTION (DEGREES)
        IDS(032)=1      ! WIND SPEED (M/S)
        IDS(033)=1      ! ZONAL WIND (M/S)
        IDS(034)=1      ! MERIDIONAL WIND (M/S)
        IDS(035)=-4     ! STREAMFUNCTION (M2/S)
        IDS(036)=-4     ! VELOCITY POTENTIAL (M2/S)
        IDS(037)=-1     ! MONTGOMERY STREAM FUNCTION (M2/S2)
        IDS(038)=8      ! SIGMA VERTICAL VELOCITY (1/S)
        IDS(039)=3      ! PRESSURE VERTICAL VELOCITY (PA/S)
        IDS(040)=4      ! GEOMETRIC VERTICAL VELOCITY (M/S)
        IDS(041)=6      ! ABSOLUTE VORTICITY (1/S)
        IDS(042)=6      ! ABSOLUTE DIVERGENCE (1/S)
        IDS(043)=6      ! RELATIVE VORTICITY (1/S)
        IDS(044)=6      ! RELATIVE DIVERGENCE (1/S)
        IDS(045)=4      ! VERTICAL U SHEAR (1/S)
        IDS(046)=4      ! VERTICAL V SHEAR (1/S)
        IDS(047)=0      ! DIRECTION OF CURRENT (DEGREES)
                        ! SPEED OF CURRENT (M/S)
                        ! U OF CURRENT (M/S)
                        ! V OF CURRENT (M/S)
        IDS(051)=4      ! SPECIFIC HUMIDITY (KG/KG)
        IDS(052)=0      ! RELATIVE HUMIDITY (PERCENT)
        IDS(053)=4      ! HUMIDITY MIXING RATIO (KG/KG)
        IDS(054)=1      ! PRECIPITABLE WATER (KG/M2)
        IDS(055)=-1     ! VAPOR PRESSURE (PA)
        IDS(056)=-1     ! SATURATION DEFICIT (PA)
        IDS(057)=1      ! EVAPORATION (KG/M2)
        IDS(058)=1      ! CLOUD ICE (KG/M2)
        IDS(059)=6      ! PRECIPITATION RATE (KG/M2/S)
        IDS(060)=0      ! THUNDERSTORM PROBABILITY (PERCENT)
        IDS(061)=1      ! TOTAL PRECIPITATION (KG/M2)
        IDS(062)=1      ! LARGE-SCALE PRECIPITATION (KG/M2)
        IDS(063)=1      ! CONVECTIVE PRECIPITATION (KG/M2)
        IDS(064)=6      ! WATER EQUIVALENT SNOWFALL RATE (KG/M2/S)
        IDS(065)=0      ! WATER EQUIVALENT OF SNOW DEPTH (KG/M2)
        IDS(066)=2      ! SNOW DEPTH (M)
                        ! MIXED-LAYER DEPTH (M)
                        ! TRANSIENT THERMOCLINE DEPTH (M)
                        ! MAIN THERMOCLINE DEPTH (M)
                        ! MAIN THERMOCLINE ANOMALY (M)
        IDS(071)=0      ! TOTAL CLOUD COVER (PERCENT)
        IDS(072)=0      ! CONVECTIVE CLOUD COVER (PERCENT)
        IDS(073)=0      ! LOW CLOUD COVER (PERCENT)
        IDS(074)=0      ! MIDDLE CLOUD COVER (PERCENT)
        IDS(075)=0      ! HIGH CLOUD COVER (PERCENT)
        IDS(076)=1      ! CLOUD WATER (KG/M2)
                        !
        IDS(078)=1      ! CONVECTIVE SNOW (KG/M2)
        IDS(079)=1      ! LARGE SCALE SNOW (KG/M2)
        IDS(080)=1      ! WATER TEMPERATURE (K)
        IDS(081)=0      ! SEA-LAND MASK ()
                        ! DEVIATION OF SEA LEVEL FROM MEAN (M)
        IDS(083)=5      ! ROUGHNESS (M)
        IDS(084)=1      ! ALBEDO (PERCENT)
        IDS(085)=1      ! SOIL TEMPERATURE (K)
        IDS(086)=0      ! SOIL WETNESS (KG/M2)
        IDS(087)=0      ! VEGETATION (PERCENT)
                        ! SALINITY (KG/KG)
        IDS(089)=4      ! DENSITY (KG/M3)
        IDS(090)=1      ! RUNOFF (KG/M2)
        IDS(091)=0      ! ICE CONCENTRATION ()
                        ! ICE THICKNESS (M)
        IDS(093)=0      ! DIRECTION OF ICE DRIFT (DEGREES)
                        ! SPEED OF ICE DRIFT (M/S)
                        ! U OF ICE DRIFT (M/S)
                        ! V OF ICE DRIFT (M/S)
                        ! ICE GROWTH (M)
                        ! ICE DIVERGENCE (1/S)
        IDS(099)=1      ! SNOW MELT (KG/M2)
                        ! SIG HEIGHT OF WAVES AND SWELL (M)
        IDS(101)=0      ! DIRECTION OF WIND WAVES (DEGREES)
                        ! SIG HEIGHT OF WIND WAVES (M)
                        ! MEAN PERIOD OF WIND WAVES (S)
        IDS(104)=0      ! DIRECTION OF SWELL WAVES (DEGREES)
                        ! SIG HEIGHT OF SWELL WAVES (M)
                        ! MEAN PERIOD OF SWELL WAVES (S)
        IDS(107)=0      ! PRIMARY WAVE DIRECTION (DEGREES)
                        ! PRIMARY WAVE MEAN PERIOD (S)
        IDS(109)=0      ! SECONDARY WAVE DIRECTION (DEGREES)
                        ! SECONDARY WAVE MEAN PERIOD (S)
        IDS(111)=0      ! NET SOLAR RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(112)=0      ! NET LONGWAVE RADIATIVE FLUX AT SURFACE (W/M2)
        IDS(113)=0      ! NET SOLAR RADIATIVE FLUX AT TOP (W/M2)
        IDS(114)=0      ! NET LONGWAVE RADIATIVE FLUX AT TOP (W/M2)
        IDS(115)=0      ! NET LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(116)=0      ! NET SOLAR RADIATIVE FLUX (W/M2)
        IDS(117)=0      ! TOTAL RADIATIVE FLUX (W/M2)
                        !
                        !
                        !
        IDS(121)=0      ! LATENT HEAT FLUX (W/M2)
        IDS(122)=0      ! SENSIBLE HEAT FLUX (W/M2)
        IDS(123)=0      ! BOUNDARY LAYER DISSIPATION (W/M2)
        IDS(124)=3      ! U WIND STRESS (N/M2)
        IDS(125)=3      ! V WIND STRESS (N/M2)
                        ! WIND MIXING ENERGY (J)
                        ! IMAGE DATA ()
        IDS(128)=-1     ! MEAN SEA-LEVEL PRESSURE (STDATM) (PA)
        IDS(129)=-1     ! MEAN SEA-LEVEL PRESSURE (MAPS) (PA)
        IDS(130)=-1     ! MEAN SEA-LEVEL PRESSURE (ETA) (PA)
        IDS(131)=1      ! SURFACE LIFTED INDEX (K)
        IDS(132)=1      ! BEST LIFTED INDEX (K)
        IDS(133)=1      ! K INDEX (K)
        IDS(134)=1      ! SWEAT INDEX (K)
        IDS(135)=10     ! HORIZONTAL MOISTURE DIVERGENCE (KG/KG/S)
        IDS(136)=4      ! SPEED SHEAR (1/S)
        IDS(137)=3      ! 3-HR PRESSURE TENDENCY (PA/S)
        IDS(138)=6      ! BRUNT-VAISALA FREQUENCY SQUARED (1/S2)
        IDS(139)=11     ! POTENTIAL VORTICITY (MASS-WEIGHTED) (1/S/M)
        IDS(140)=0      ! RAIN MASK ()
        IDS(141)=0      ! FREEZING RAIN MASK ()
        IDS(142)=0      ! ICE PELLETS MASK ()
        IDS(143)=0      ! SNOW MASK ()
        IDS(144)=3      ! VOLUMETRIC SOIL MOISTURE CONTENT (FRACTION)
        IDS(145)=0      ! POTENTIAL EVAPORATION RATE (W/M2)
        IDS(146)=0      ! CLOUD WORKFUNCTION (J/KG)
        IDS(147)=3      ! U GRAVITY WAVE STRESS (N/M2)
        IDS(148)=3      ! V GRAVITY WAVE STRESS (N/M2)
        IDS(149)=10     ! POTENTIAL VORTICITY (M2/S/KG)
                        ! COVARIANCE BETWEEN V AND U (M2/S2)
                        ! COVARIANCE BETWEEN U AND T (K*M/S)
                        ! COVARIANCE BETWEEN V AND T (K*M/S)
                        !
                        !
        IDS(155)=0      ! GROUND HEAT FLUX (W/M2)
        IDS(156)=0      ! CONVECTIVE INHIBITION (W/M2)
        IDS(157)=0      ! CONVECTIVE APE (J/KG)
        IDS(158)=0      ! TURBULENT KE (J/KG)
        IDS(159)=-1     ! CONDENSATION PRESSURE OF LIFTED PARCEL (PA)
        IDS(160)=0      ! CLEAR SKY UPWARD SOLAR FLUX (W/M2)
        IDS(161)=0      ! CLEAR SKY DOWNWARD SOLAR FLUX (W/M2)
        IDS(162)=0      ! CLEAR SKY UPWARD LONGWAVE FLUX (W/M2)
        IDS(163)=0      ! CLEAR SKY DOWNWARD LONGWAVE FLUX (W/M2)
        IDS(164)=0      ! CLOUD FORCING NET SOLAR FLUX (W/M2)
        IDS(165)=0      ! CLOUD FORCING NET LONGWAVE FLUX (W/M2)
        IDS(166)=0      ! VISIBLE BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(167)=0      ! VISIBLE DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
        IDS(168)=0      ! NEAR IR BEAM DOWNWARD SOLAR FLUX (W/M2)
        IDS(169)=0      ! NEAR IR DIFFUSE DOWNWARD SOLAR FLUX (W/M2)
                        !
                        !
        IDS(172)=3      ! MOMENTUM FLUX (N/M2)
        IDS(173)=0      ! MASS POINT MODEL SURFACE ()
        IDS(174)=0      ! VELOCITY POINT MODEL SURFACE ()
        IDS(175)=0      ! SIGMA LAYER NUMBER ()
        IDS(176)=2      ! LATITUDE (DEGREES)
        IDS(177)=2      ! EAST LONGITUDE (DEGREES)
                        !
                        !
                        !
        IDS(181)=9      ! X-GRADIENT LOG PRESSURE (1/M)
        IDS(182)=9      ! Y-GRADIENT LOG PRESSURE (1/M)
        IDS(183)=5      ! X-GRADIENT HEIGHT (M/M)
        IDS(184)=5      ! Y-GRADIENT HEIGHT (M/M)
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
                        !
        IDS(201)=0      ! ICE-FREE WATER SURCACE (PERCENT)
                        !
                        !
        IDS(204)=0      ! DOWNWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(205)=0      ! DOWNWARD LONGWAVE RADIATIVE FLUX (W/M2)
                        !
        IDS(207)=0      ! MOISTURE AVAILABILITY (PERCENT)
                        ! EXCHANGE COEFFICIENT (KG/M2/S)
        IDS(209)=0      ! NUMBER OF MIXED LAYER NEXT TO SFC ()
                        !
        IDS(211)=0      ! UPWARD SOLAR RADIATIVE FLUX (W/M2)
        IDS(212)=0      ! UPWARD LONGWAVE RADIATIVE FLUX (W/M2)
        IDS(213)=0      ! NON-CONVECTIVE CLOUD COVER (PERCENT)
        IDS(214)=6      ! CONVECTIVE PRECIPITATION RATE (KG/M2/S)
        IDS(215)=7      ! TOTAL DIABATIC HEATING RATE (K/S)
        IDS(216)=7      ! TOTAL RADIATIVE HEATING RATE (K/S)
        IDS(217)=7      ! TOTAL DIABATIC NONRADIATIVE HEATING RATE (K/S)
        IDS(218)=2      ! PRECIPITATION INDEX (FRACTION)
        IDS(219)=1      ! STD DEV OF IR T OVER 1X1 DEG AREA (K)
        IDS(220)=4      ! NATURAL LOG OF SURFACE PRESSURE OVER 1 KPA ()
                        !
        IDS(222)=0      ! 5-WAVE GEOPOTENTIAL HEIGHT (M)
        IDS(223)=1      ! PLANT CANOPY SURFACE WATER (KG/M2)
                        !
                        !
                        ! BLACKADARS MIXING LENGTH (M)
                        ! ASYMPTOTIC MIXING LENGTH (M)
        IDS(228)=1      ! POTENTIAL EVAPORATION (KG/M2)
        IDS(229)=0      ! SNOW PHASE-CHANGE HEAT FLUX (W/M2)
                        !
        IDS(231)=3      ! CONVECTIVE CLOUD MASS FLUX (PA/S)
        IDS(232)=0      ! DOWNWARD TOTAL RADIATION FLUX (W/M2)
        IDS(233)=0      ! UPWARD TOTAL RADIATION FLUX (W/M2)
        IDS(224)=1      ! BASEFLOW-GROUNDWATER RUNOFF (KG/M2)
        IDS(225)=1      ! STORM SURFACE RUNOFF (KG/M2)
                        !
                        !
        IDS(238)=0      ! SNOW COVER (PERCENT)
        IDS(239)=1      ! SNOW TEMPERATURE (K)
                        !
        IDS(241)=7      ! LARGE SCALE CONDENSATION HEATING RATE (K/S)
        IDS(242)=7      ! DEEP CONVECTIVE HEATING RATE (K/S)
        IDS(243)=10     ! DEEP CONVECTIVE MOISTENING RATE (KG/KG/S)
        IDS(244)=7      ! SHALLOW CONVECTIVE HEATING RATE (K/S)
        IDS(245)=10     ! SHALLOW CONVECTIVE MOISTENING RATE (KG/KG/S)
        IDS(246)=7      ! VERTICAL DIFFUSION HEATING RATE (KG/KG/S)
        IDS(247)=7      ! VERTICAL DIFFUSION ZONAL ACCELERATION (M/S/S)
        IDS(248)=7      ! VERTICAL DIFFUSION MERID ACCELERATION (M/S/S)
        IDS(249)=10     ! VERTICAL DIFFUSION MOISTENING RATE (KG/KG/S)
        IDS(250)=7      ! SOLAR RADIATIVE HEATING RATE (K/S)
        IDS(251)=7      ! LONGWAVE RADIATIVE HEATING RATE (K/S)
                        ! DRAG COEFFICIENT ()
                        ! FRICTION VELOCITY (M/S)
                        ! RICHARDSON NUMBER ()
                        !
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
