! WRF-TEB (https://github.com/teb-model/wrf-teb).
! Copyright 2018-2020 D. Meyer. Licensed under the MIT License.

! WRF-TEB incorporates functions from PsychroLib (Meyer and Thevenard, 2019)
! version 2.4.0 (see https://github.com/psychrometrics/psychrolib).
! These functions (GetSpecificHumFromHumRatio/GetHumRatioFromSpecificHum) were
! only modified to relax conditions when the parameter is out of bound.
!
! The original copyright notices and the licenses are included below:
! 
! %% psychrometrics/psychrolib NOTICES AND INFORMATION BEGIN HERE
! =========================================
! The MIT License (MIT)
! 
! Copyright (c) 2018-2020 The PsychroLib Contributors.
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
! =========================================
! END OF psychrometrics/psychrolib NOTICES AND INFORMATION
! 
! References
! ===============
! Meyer, D. and Thevenard, D. (2019). PsychroLib: a library of psychrometric
! functions to calculate thermodynamic properties of air. Journal of Open
! Source Software, 4(33), 1137, https://doi.org/10.21105/joss.01137.

#ifdef WRF_TEB
MODULE MODULE_SF_TEB

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: TEB

  real, parameter  :: MIN_HUM_RATIO = 1e-7
    !+ Minimum acceptable humidity ratio used/returned by any functions.
    !+ Any value above 0 or below the MIN_HUM_RATIO will be reset to this value.

  CONTAINS

    SUBROUTINE TEB (   &
                  ims ,ime, jms, jme, kms, kme,                           &
                  its ,ite, jts, jte, kts, kte,                           &
                  TEB_TEST_INTEGRATION,                                   &
                  TEB_NUM_ROOF_LAYERS, TEB_NUM_WALL_LAYERS, TEB_NUM_ROAD_LAYERS, &
                  TEB_NUM_SNOW_LAYERS, TEB_NUM_FLOOR_LAYERS,              &
                  XLAT, XLONG,                                            &
                  JULDAY, JULYR, GMT, XTIME, XTSTEP_SURF,                 &
                  U_PHY, V_PHY,                                           &
                  DZ8W, P8W3D, T3D, QV3D, RHO,                            &
                  SWDOWN, SW_DIFFUSE_FRAC, GLW,                           &
                  RAINBL, SR, FRAC_URB2D, UTYPE_URB2D,                    &
                  IVGTYP, ISURBAN,                                        &
                  CQS2, CHS2,                                             &
                  ! Input from noah
                  ALB_RURAL, EMISSI_RURAL, HFX_RURAL, QFX_RURAL,          &
                  LH_RURAL, GRDFLX_RURAL,                                 &
                  TEB_TSK_RURAL, QSFC_RURAL,                                  &
                  ! End input from noah
                  TEB_TI_BLD,                                             &
                  TEB_T_CANYON, TEB_Q_CANYON,                             &
                  TEB_WS_ROOF, TEB_WS_ROAD,                               &
                  TEB_WSNOW_ROOF, TEB_TSNOW_ROOF,                         &
                  TEB_RSNOW_ROOF, TEB_ASNOW_ROOF,                         &
                  TEB_TSSNOW_ROOF, TEB_ESNOW_ROOF,                        &
                  TEB_WSNOW_ROAD, TEB_TSNOW_ROAD,                         &
                  TEB_RSNOW_ROAD, TEB_ASNOW_ROAD,                         &
                  TEB_TSSNOW_ROAD, TEB_ESNOW_ROAD,                        &
                  TEB_T_WIN1, TEB_T_WIN2,                                 &
                  TEB_AUX_MAX, TEB_THER_PRODC_DAY,                        &
                  TEB_QI_BLD,                                             &
                  TEB_T_FLOOR, TEB_T_MASS,                                &
                  TEB_T_ROAD, TEB_T_ROOF,                                 &
                  TEB_T_WALL_A, TEB_T_WALL_B,                             &
                  TEB_HVAC_COOL, TEB_HVAC_HEAT,                           &
                  TEB_THER_PROD_PANEL, TEB_PHOT_PROD_PANEL,               &
                  RN_URB2D, U10_URB2D, V10_URB2D,                         &
                  ! Output Noah + TEB
                  ALBEDO, HFX, QFX, LH, GRDFLX, TSK, EMISS, QSFC, UST,    &
                  TH2_URB2D, Q2_URB2D,                                    &
                  ! End output Noah + TEB
                  !
                  ! BEGIN: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE
                  TEB_INPUT_TA,                                       &
                  TEB_INPUT_PS,                                       &
                  TEB_INPUT_QA,                                       &
                  TEB_INPUT_WIND,                                     &
                  TEB_INPUT_DIR,                                      &
                  TEB_INPUT_DIR_SW,                                   &
                  TEB_INPUT_SCA_SW,                                   &
                  TEB_INPUT_LW,                                       &
                  TEB_INPUT_RAIN,                                     &
                  TEB_INPUT_SNOW,                                     &
                  TEB_INPUT_DIR_CO2)
                  ! END: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE

      USE module_sf_noahlsm, only : LOW_DENSITY_RESIDENTIAL, HIGH_DENSITY_RESIDENTIAL, HIGH_INTENSITY_INDUSTRIAL

      USE module_sf_urban, only:  TEB_SOLAR_PANEL_TBL,                  &
        TEB_USE_NOAH, TEB_USE_ZGARDEN_FROM_TBL,  &
        TEB_LGARDEN_TBL, TEB_GREENROOF_TBL, TEB_HROAD_DIR_TBL, TEB_ZROAD_DIR_TBL, TEB_WALL_OPT_TBL,                 &
        TEB_COOL_COIL_TBL, TEB_HEAT_COIL_TBL, TEB_F_WATER_COND_TBL, TEB_HNATVENT_TBL, TEB_ZNATVENT_TBL,             &
        TEB_F_WASTE_CAN_TBL, TEB_QIN_TBL, TEB_QIN_FRAD_TBL, TEB_QIN_FLAT_TBL, TEB_GR_TBL, TEB_EFF_HEAT_TBL,             &
        TEB_INF_TBL, TEB_TCOOL_TARGET_TBL, TEB_THEAT_TARGET_TBL, TEB_HR_TARGET_TBL, TEB_V_VENT_TBL,                 &
        TEB_CAP_SYS_HEAT_TBL, TEB_CAP_SYS_RAT_TBL, TEB_T_ADP_TBL, TEB_M_SYS_RAT_TBL, TEB_COP_RAT_TBL,               &
        TEB_SHGC_TBL, TEB_SHGC_SH_TBL, TEB_U_WIN_TBL, TEB_LSHADE_TBL, TEB_ZSHADE_TBL, TEB_FLOOR_HEIGHT_TBL,             &
        TEB_CH_BEM_TBL, TEB_ROUGH_ROOF_TBL, TEB_ROUGH_WALL_TBL, TEB_PAR_RD_IRRIG_TBL, TEB_RD_START_MONTH_TBL,       &
        TEB_RD_END_MONTH_TBL, TEB_RD_START_HOUR_TBL, TEB_RD_END_HOUR_TBL, TEB_RD_24H_IRRIG_TBL, TEB_EMIS_PANEL_TBL, &
        TEB_ALB_PANEL_TBL, TEB_EFF_PANEL_TBL, TEB_FRAC_PANEL_TBL, TEB_RESIDENTIAL_TBL, TEB_DT_RES_TBL, TEB_DT_OFF_TBL,  &
        TEB_CBEM_TBL, TEB_Z0H_TBL, TEB_OPT_ZOH_TBL, TEB_SNOW_ROOF_TBL, TEB_SNOW_SCH_ROOF_TBL, TEB_SNOW_ROAD_TBL,    &
        TEB_SNOW_SCH_ROAD_TBL, TEB_ZGARDEN_TBL, TEB_FRAC_GR_TBL, TEB_Z0_TOWN_TBL, TEB_BLD_HEIGHT_TBL, TEB_FRAC_BLD_TBL,         &
        TEB_WALL_O_TBL, TEB_H_TRAFFIC_TBL, TEB_LE_TRAFFIC_TBL, TEB_H_INDUSTRY_TBL, TEB_LE_INDUSTRY_TBL,         &
        TEB_ALB_ROOF_TBL, TEB_EMIS_ROOF_TBL, TEB_ALB_ROAD_TBL, TEB_EMIS_ROAD_TBL,                   &
        TEB_ALB_WALL_TBL, TEB_EMIS_WALL_TBL,                                        &
        TEB_HC_ROOF_TBL, TEB_TC_ROOF_TBL,  TEB_D_ROOF_TBL, &
        TEB_HC_ROAD_TBL, TEB_TC_ROAD_TBL,  TEB_D_ROAD_TBL, &
        TEB_HC_WALL_TBL, TEB_TC_WALL_TBL,  TEB_D_WALL_TBL, &
        TEB_HC_FLOOR_TBL, TEB_TC_FLOOR_TBL,  TEB_D_FLOOR_TBL

      USE module_ra_gfdleta, only: cal_mon_day

      USE modd_wrf_teb_driver, only: TEB_DRIVER

      ! TEB imports
      USE MODD_CSTS,     ONLY : XCPD, & ! Cpd (dry air)
                                XRV,  & ! gaz constant for vapor
                                XRD,  & ! Gaz constant for dry air
                                XG      ! Gravity constant
      USE MODD_SURF_PAR, ONLY : XUNDEF  ! "no-data" value
      USE MODI_ADD_FORECAST_TO_DATE_SURF

      IMPLICIT NONE

      INTEGER,  INTENT(IN) :: TEB_TEST_INTEGRATION

      ! array dimension sizes
      INTEGER,  INTENT(IN) :: ims, ime, jms, jme, kms, kme
      INTEGER,  INTENT(IN) :: its, ite, jts, jte, kts, kte
      INTEGER,  INTENT(IN) :: TEB_NUM_ROOF_LAYERS
      INTEGER,  INTENT(IN) :: TEB_NUM_WALL_LAYERS
      INTEGER,  INTENT(IN) :: TEB_NUM_ROAD_LAYERS
      INTEGER,  INTENT(IN) :: TEB_NUM_SNOW_LAYERS
      INTEGER,  INTENT(IN) :: TEB_NUM_FLOOR_LAYERS

      ! spatial coordinates
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)      :: XLAT
      REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)      :: XLONG

      ! temporal coordinates
      INTEGER,  INTENT(IN) :: JULDAY, JULYR       ! date at simulation start
      REAL,     INTENT(IN) :: GMT                 ! hours since midnight at simulation start
      REAL,     INTENT(IN) :: XTIME               ! minutes since simulation start
      REAL,     INTENT(IN) :: XTSTEP_SURF         ! duration of time step (seconds)

      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: U_PHY                ! zonal speed
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: V_PHY                ! meridional speed
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: DZ8W                 ! thickness of each atmospheric model layer
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: P8W3D                ! 3D pressure (Pa) This is the pressure at full levels (Pa)
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: T3D                  ! 3D temperature (K)
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: QV3D                 ! 3D mixing ratio
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme),              INTENT(IN)    :: RHO                  ! density of the lowest level (kg/m^3)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: SWDOWN               ! downward short wave flux at ground surface (W/m^2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: SW_DIFFUSE_FRAC      ! diffuse fraction of surface shortwave irradiance
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: GLW                  ! downward long wave flux at ground surface (W/m^2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: RAINBL               ! in mm (Accumulation between PBL calls)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: SR                   ! fraction of frozen precipitation
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: FRAC_URB2D           ! urban fraction
      INTEGER, DIMENSION(ims:ime, jms:jme),                    INTENT(IN)    :: UTYPE_URB2D          ! urban type
      INTEGER, DIMENSION(ims:ime, jms:jme),                    INTENT(IN)    :: IVGTYP               ! dominant vegetation category
      INTEGER,                                                 INTENT(IN)    :: ISURBAN              ! vegetation category for urban
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: CQS2                 ! 2m surface exchange coefficient for moisture
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: CHS2                 ! 2m surface exchange coefficient for heat
      ! input from Noah
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: ALB_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: EMISSI_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: HFX_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: QFX_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: LH_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: GRDFLX_RURAL
      REAL, DIMENSION(its:ite, jts:jte),                       INTENT(IN)    :: QSFC_RURAL  ! mixing ratio from Noah
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(IN)    :: TEB_TSK_RURAL
      ! end input from Noah
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_TI_BLD      ! indoor air temperature
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_T_CANYON    ! air canyon temperature
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_Q_CANYON    ! canyon air humidity ratio [kg kg-1]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_WS_ROOF     ! roof water content (kg/m2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_WS_ROAD     ! road water content (kg/m2)
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_WSNOW_ROOF  ! snow layers reservoir
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_TSNOW_ROOF  ! snow layers temperature
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_RSNOW_ROOF  ! snow layers density
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_ASNOW_ROOF  ! snow albedo
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_ESNOW_ROOF  ! snow emissivity
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_TSSNOW_ROOF ! snow surface temperature
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_WSNOW_ROAD  ! snow layers reservoir
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_TSNOW_ROAD  ! snow layers temperature
      REAL, DIMENSION(ims:ime, TEB_NUM_SNOW_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_RSNOW_ROAD  ! snow layers density
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_ASNOW_ROAD  ! snow albedo
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_ESNOW_ROAD  ! snow emissivity
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_TSSNOW_ROAD ! snow surface temperature
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_T_WIN1      ! outdoor window temperature [K]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_T_WIN2      ! Indoor window temperature [K]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_AUX_MAX     ! Auxiliar variable for autosize calcs
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_THER_PRODC_DAY ! Present day integrated thermal production of energy (J/m2 panel)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(INOUT) :: TEB_QI_BLD      ! Indoor air specific humidity [kg kg-1]
      REAL, DIMENSION(ims:ime, TEB_NUM_FLOOR_LAYERS, jms:jme), INTENT(INOUT) :: TEB_T_FLOOR     ! Floor layers temperatures [K]
      REAL, DIMENSION(ims:ime, TEB_NUM_FLOOR_LAYERS, jms:jme), INTENT(INOUT) :: TEB_T_MASS      ! Internal mass layers temperatures [K]
      REAL, DIMENSION(ims:ime, TEB_NUM_ROAD_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_T_ROAD      ! road layers temperatures
      REAL, DIMENSION(ims:ime, TEB_NUM_ROOF_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_T_ROOF      ! roof layers temperatures
      REAL, DIMENSION(ims:ime, TEB_NUM_WALL_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_T_WALL_A    ! wall layers temperatures (wall 'A')
      REAL, DIMENSION(ims:ime, TEB_NUM_WALL_LAYERS, jms:jme),  INTENT(INOUT) :: TEB_T_WALL_B    ! wall layers temperatures (wall 'B')
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TEB_HVAC_COOL   ! Energy consumption of the cooling system [W m-2(bld)]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TEB_HVAC_HEAT   ! Energy consumption of the heating system [W m-2(bld)]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TEB_THER_PROD_PANEL ! Thermal energy production of solar panel on roofs [W m-2(panel)]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TEB_PHOT_PROD_PANEL ! Photovoltaic energy production of solar panel on roofs [W m-2(panel)]
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: RN_URB2D
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: U10_URB2D
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: V10_URB2D
      ! output Noah + TEB
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: ALBEDO
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: HFX    ! upward heat flux at the surface (W m-2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: QFX    ! upward moisture flux at the surface (kg m-2 s-1)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: LH     ! latent heat flux at the surface (W m-2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: GRDFLX ! ground heat flux (W m-2)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TSK    ! surface skin temperature (K)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: EMISS  ! surface emissivity
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: QSFC   ! mixing ratio
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: UST    ! U* in similarity theory (m s-1)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: TH2_URB2D ! 2 m temperature (not potential)
      REAL, DIMENSION(ims:ime, jms:jme),                       INTENT(OUT)   :: Q2_URB2D  ! 2 m mixing ratio
      ! end output Noah + TEB

      ! BEGIN: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_TA
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_PS
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_QA
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_WIND
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_DIR
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_DIR_SW
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_SCA_SW
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_LW
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_RAIN
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_SNOW
      REAL, DIMENSION(ims:ime, jms:jme),                     INTENT(OUT)   :: TEB_INPUT_DIR_CO2
      ! END: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! LOCAL
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      INTEGER                :: IYEAR, IMONTH, IDAY ! date at current time step
      REAL                   :: ZTIME_START  ! time at beginning of time step, since midnight (UTC, s)
      INTEGER                :: I, J ,L
      ! Index in the vertical dimension for quantities with half levels.
      ! 1 is at the centre of the grid cell
      INTEGER, PARAMETER     :: KHL = 1
      ! Index in the vertical dimension for quantities with full levels.
      ! 1 is at the surface
      INTEGER, PARAMETER     :: KFL = 1
      REAL                   :: FRAC_NOAH  ! Noah fraction to use during averaging
      INTEGER                :: URBTYPE    ! urban type
      REAL                   :: QA         ! specific humidity at the lowest level
      REAL                   :: Q2_RURAL   ! 2m specific humidity for vegetation (Noah)
      REAL                   :: T2_RURAL   ! 2m temperature for vegetation (Noah)
      REAL                   :: RHO_TSK
      REAL                   :: EMISS_TEMP ! emissivity (Noah + TEB)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! LOCAL for TEB 1D
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! IN
      REAL, DIMENSION(1)    :: LAT        ! latitude
      REAL, DIMENSION(1)    :: LON        ! longitude
      REAL, DIMENSION(1)    :: ZGARDEN    ! fraction of GARDEN areas
      REAL, DIMENSION(1)    :: LW         ! atmospheric infrared radiation
      REAL, DIMENSION(1)    :: TA         ! air temperature forcing (K)
      REAL, DIMENSION(1)    :: WIND       ! wind speed
      REAL, DIMENSION(1)    :: DIR        ! wind direction
      REAL, DIMENSION(1)    :: EVAP_TOWN  ! evaporation flux (kg/m2/s)
      REAL, DIMENSION(1)    :: RAIN       ! rain rate (kg/m2/s)
      REAL, DIMENSION(1)    :: SNOW       ! snow rate (kg/s/m2 of snow)
      REAL, DIMENSION(1)    :: PA         ! pressure at the first atmospheric level
      REAL, DIMENSION(1)    :: PS         ! pressure at the surface
      REAL, DIMENSION(1)    :: RHOA       ! air density at the lowest level
      REAL, DIMENSION(1,1)  :: DIR_SW     ! incoming direct solar radiation on an horizontal surface
      REAL, DIMENSION(1,1)  :: SCA_SW     ! scattered incoming solar rad.
      REAL, DIMENSION(1)    :: ZREF       ! reference height of the first atmospheric level (temperature)
      REAL ,DIMENSION(1)    :: QA_KGKG    ! air humidity at forcing level (kg/kg)
      REAL ,DIMENSION(1)    :: BLD        ! Horizontal building area density
      ! TODO: make into actual input (ims:ime, jms:jme) from file
      ! This is not critical for now as we are not considering CO2 in the evaluation.
      REAL, DIMENSION(1)    :: CO2 = 0.   ! CO2 concentration in the air    (kg/m3)

      ! OUT
      REAL, DIMENSION(1)    :: EMIS_TOWN  ! town equivalent emissivity
      REAL, DIMENSION(1)    :: H_TOWN     ! sensible heat flux over town
      REAL, DIMENSION(1)    :: LE_TOWN    ! latent heat flux over town
      REAL, DIMENSION(1)    :: U_CANYON   ! canyon hor. wind
      REAL, DIMENSION(1)    :: ALB_TOWN   ! Town eqivalent albedo
      REAL, DIMENSION(1)    :: DIR_CANYON ! Canyon wind direction
      REAL, DIMENSION(1)    :: Q_TOWN     ! Town eqivalent specific humidity
      REAL, DIMENSION(1)    :: USTAR_TOWN ! friction velocity over town
      REAL, DIMENSION(1)    :: GFLUX_TOWN ! flux through the ground
      REAL, DIMENSION(1)    :: RN_TOWN    ! net radiation over town
      REAL, DIMENSION(1)    :: TS_TOWN    ! town surface temperature

      ! Date at simulation start.
      CALL cal_mon_day(JULDAY, JULYR, IMONTH, IDAY)

      ! TEB needs year, month, day, GMT time for beginning of current timestep.
      IYEAR = JULYR
      ZTIME_START = GMT*3600 + XTIME*60 ! seconds since midnight from simulation start
      CALL ADD_FORECAST_TO_DATE_SURF(IYEAR, IMONTH, IDAY, ZTIME_START)
      ! ZTIME_START is now seconds since midnight at current timestep.
      ! IYEAR, IMONTH, IDAY have been adjusted as well to current timestep.

      ! 1. Grid loop
      ! ============
      ! Loop over all grid cells and identify whether they are urban.
      ! If urban call TEB, otherwise pass through values as given by the LSM or
      ! zero in the case of variables that apply to TEB only.
      DO J = jts, jte
        DO I = its, ite

          ! 2. Run TEB
          ! ==========
          ! If the cell is of urban type prepare parameters and run TEB.
          !
          IF ( IVGTYP(I,J) == ISURBAN .or. IVGTYP(I,J) == LOW_DENSITY_RESIDENTIAL .or. &
               IVGTYP(I,J) == HIGH_DENSITY_RESIDENTIAL .or. IVGTYP(I,J) == HIGH_INTENSITY_INDUSTRIAL ) THEN

            URBTYPE = UTYPE_URB2D(I,J)

            ! Initialise TEB inputs
            ! =========================================
            !
            LAT(1) = XLAT(I,J)
            LON(1) = XLONG(I,J)
            LW(1) = GLW(I,J)
            TA(1) = T3D(I,KHL,J)

            if (TEB_USE_NOAH) then
              ! In TEB, `ROAD` is used to account for other impervious surfaces other than buildings.
              ! This is defined as: `ROAD = 1 - BLD - GARDEN`
              ! When using the Noah LSM to compute the fraction of gardens, we want to run TEB without any gardens
              ! as gardens will be accounted by the the Noah LSM.
              ! Since `FRAC_URB2D` represents the total impervious surface in a grid box, i.e. buildings + roads in TEB
              ! We can obrain the fraction of buildings over the total impervious surface (FRAC_URB2D) as:
              BLD(1) = TEB_FRAC_BLD_TBL(URBTYPE) * FRAC_URB2D(I,J)
              ZGARDEN(1) = 0.
              FRAC_NOAH = 1 - FRAC_URB2D(I,J)
            else
              FRAC_NOAH = 0
              if (TEB_USE_ZGARDEN_FROM_TBL) then
                BLD(1) = TEB_FRAC_BLD_TBL(URBTYPE)
                ZGARDEN(1) = TEB_ZGARDEN_TBL(URBTYPE)
              else
                ! See why this in comment above when TEB_USE_NOAH is ON.
                BLD(1) = TEB_FRAC_BLD_TBL(URBTYPE) * FRAC_URB2D(I,J)
                ZGARDEN(1) = 1 - FRAC_URB2D(I,J)
              endif
            endif

            !
            ! Reference height of the first atmospheric level.
            ! Mass quantities and horizontal windspeed are at the same height.
            ZREF(1) = (0.5 * DZ8W(I,KHL,J))
            IF (ZREF(1) < (TEB_Z0_TOWN_TBL(URBTYPE) * 2)) THEN
              CALL WRF_ERROR_FATAL('The first vertical grid spacing must be set to at least ' // &
                                   'twice the value of roughness lenght for momentum (TEB_Z0_TOWN) ' // &
                                   'in the urban parameter table (URBPARM.TBL).')
            END IF
            !
            ! Atmospheric pressure at the surface.
            ! NOTE: WRF surface pressure is same as pressure at the roof level in TEB.
            !       the pressure is considered to be the same throughout the canyon.
            PS(1) = P8W3D(I,KFL,J)
            !
            ! Atmospheric pressure at forcing level.
            PA(1)   = 0.5 * (P8W3D(I,KFL,J) + P8W3D(I,KFL+1,J))
            !
            ! Wind Speed
            WIND(1) = CALC_WIND_SPEED_FROM_U_V(U_PHY(I,KHL,J), V_PHY(I,KHL,J))
            !
            ! Wind direction
            DIR(1) = CALC_WIND_DIR_FROM_U_V(U_PHY(I,KHL,J), V_PHY(I,KHL,J))
            !
            ! WRF uses humidity in terms of mixing ratio (kg/kg) but TEB require the
            ! humidity in terms of specific humidity (kg/kg).
            ! Convert from mixing ratio to specific humidity.
            QA = GetSpecificHumFromHumRatio(QV3D(I,KHL,J))
            QA_KGKG(1) = GetSpecificHumFromHumRatio(QV3D(I,KHL,J))
            !
            RHOA(1) = RHO(I,KHL,J)
            !
            ! RAINBL contains liquid and solid precipitation.
            RAIN(1) = 0.
            SNOW(1) = 0.
            IF (RAINBL(I,J) > 0.) THEN
              ! Snow is defined when fraction of frozen precipitation SR(I,J) > 0.5.
              IF (SR(I,J) > 0.5 .AND. T3D(I,1,J) <= 273.15) THEN
                SNOW(1) = RAINBL(I,J)/XTSTEP_SURF
              ELSE
                RAIN(1) = RAINBL(I,J)/XTSTEP_SURF
              END IF
            END IF
            !
            ! Radiation
            SCA_SW(1,1) = SWDOWN(I,J) * SW_DIFFUSE_FRAC(I,J)
            DIR_SW(1,1) = SWDOWN(I,J) - SCA_SW(1,1)


          !=======================USED IN TECHNICAL EVALUATION===========================================
          if (TEB_TEST_INTEGRATION == 1) then
            ! BEGIN: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE
            TEB_INPUT_TA(I,J) = T3D(I,KHL,J)
            TEB_INPUT_PS(I,J) = PS(1)
            TEB_INPUT_QA(I,J) = QA_KGKG(1)
            TEB_INPUT_WIND(I,J) = WIND(1)
            TEB_INPUT_DIR(I,J) = DIR(1)
            TEB_INPUT_DIR_SW(I,J) = DIR_SW(1,1)
            TEB_INPUT_SCA_SW(I,J) = SCA_SW(1,1)
            TEB_INPUT_LW(I,J) = GLW(I,J)
            TEB_INPUT_RAIN(I,J) = RAIN(1)
            TEB_INPUT_SNOW(I,J) = SNOW(1)
            TEB_INPUT_DIR_CO2(I,J) = CO2(1)
            ! END: INPUT quantities used for evaluating WRF-TEB with TEB OFFLINE
            !
            ZREF(1) = 33.
            RHOA(1) = PS(1) / (T3D(I,KHL,J) * XRD * (1. + ((XRV/XRD) - 1.) * QA) + ZREF(1) * XG)
            ! Humidity in kg/m3
            QA = QA * RHOA(1) ! Taken from: src\driver\ol_time_interp_atm.F90
            ! Forcing level pressure from hydrostatism
            PA(1) = PS(1) - RHOA(1) * XG * ZREF(1) ! Taken from: src\driver\ol_time_interp_atm.F90
            ! specific humidity (conversion from kg/m3 to kg/kg)
            QA_KGKG(1) = QA / RHOA(1)
          endif
          !=========================USED IN TECHNICAL EVALUATION========================================

    CALL TEB_DRIVER ( &
                     ! inputs
                     TEB_NUM_ROOF_LAYERS, TEB_NUM_WALL_LAYERS, TEB_NUM_ROAD_LAYERS, &
                     TEB_NUM_SNOW_LAYERS, TEB_NUM_FLOOR_LAYERS,                     &
                     LON, LAT,                                                      &
                     IYEAR, IMONTH, IDAY, ZTIME_START, XTSTEP_SURF,                 &
                     PS, PA,                                                        &
                     TA, QA_KGKG, RHOA, CO2,                                        &
                     LW,                                                            &
                     RAIN, SNOW,                                                    &
                     ZREF,                                                          &
                     DIR_SW, SCA_SW, WIND, DIR,                                     &
                     ! inputs / outputs
                     TEB_T_CANYON(I,J), TEB_Q_CANYON(I,J),                          &
                     TEB_TI_BLD(I,J),                                               &
                     TEB_T_ROOF(I,:,J), TEB_T_ROAD(I,:,J),                          &
                     TEB_T_WALL_A(I,:,J), TEB_T_WALL_B(I,:,J),                      &
                     TEB_WS_ROOF(I,J), TEB_WS_ROAD(I,J),                            &
                     TEB_WSNOW_ROOF(I,:,J), TEB_TSNOW_ROOF(I,:,J),                  &
                     TEB_RSNOW_ROOF(I,:,J), TEB_ASNOW_ROOF(I,J),                    &
                     TEB_TSSNOW_ROOF(I,J), TEB_ESNOW_ROOF(I,J),                     &
                     TEB_WSNOW_ROAD(I,:,J), TEB_TSNOW_ROAD(I,:,J),                  &
                     TEB_RSNOW_ROAD(I,:,J), TEB_ASNOW_ROAD(I,J),                    &
                     TEB_TSSNOW_ROAD(I,J), TEB_ESNOW_ROAD(I,J),                     &
                     TEB_AUX_MAX(I,J), TEB_T_FLOOR(I,:,J),                          &
                     TEB_T_MASS(I,:,J),                                             &
                     TEB_T_WIN1(I,J), TEB_T_WIN2(I,J), TEB_QI_BLD(I,J),             &
                     TEB_THER_PRODC_DAY(I,J),                                       &
                     ! parameters
                     TEB_LGARDEN_TBL(URBTYPE), TEB_GREENROOF_TBL(URBTYPE), TEB_SOLAR_PANEL_TBL(URBTYPE),                &
                     TEB_Z0H_TBL(TEB_OPT_ZOH_TBL(URBTYPE)), TEB_HROAD_DIR_TBL(URBTYPE), TEB_WALL_OPT_TBL(URBTYPE),       &
                     TEB_SNOW_ROAD_TBL(TEB_SNOW_SCH_ROAD_TBL(URBTYPE)),                                              &
                     TEB_SNOW_ROOF_TBL(TEB_SNOW_SCH_ROOF_TBL(URBTYPE)),                                              &
                     TEB_H_TRAFFIC_TBL(URBTYPE), TEB_LE_TRAFFIC_TBL(URBTYPE), &
                     TEB_H_INDUSTRY_TBL(URBTYPE), TEB_LE_INDUSTRY_TBL(URBTYPE),      &
                     TEB_Z0_TOWN_TBL(URBTYPE),                                                     &
                     BLD, ZGARDEN, &
                     TEB_ZROAD_DIR_TBL(URBTYPE), TEB_FRAC_GR_TBL(URBTYPE),                   &
                     TEB_BLD_HEIGHT_TBL(URBTYPE), TEB_WALL_O_TBL(URBTYPE),                   &
                     TEB_ALB_ROOF_TBL(URBTYPE), TEB_EMIS_ROOF_TBL(URBTYPE),                                   &
                     TEB_HC_ROOF_TBL(:, URBTYPE), TEB_TC_ROOF_TBL(:, URBTYPE), TEB_D_ROOF_TBL(:, URBTYPE),    &
                     TEB_ALB_ROAD_TBL(URBTYPE), TEB_EMIS_ROAD_TBL(URBTYPE),                         &
                     TEB_HC_ROAD_TBL(:, URBTYPE), TEB_TC_ROAD_TBL(:, URBTYPE), TEB_D_ROAD_TBL(:, URBTYPE),    &
                     TEB_ALB_WALL_TBL(URBTYPE), TEB_EMIS_WALL_TBL(URBTYPE),                        &
                     TEB_HC_WALL_TBL(:, URBTYPE), TEB_TC_WALL_TBL(:, URBTYPE), TEB_D_WALL_TBL(:, URBTYPE),    &
                     TEB_COOL_COIL_TBL(URBTYPE), TEB_F_WATER_COND_TBL(URBTYPE), TEB_HEAT_COIL_TBL(URBTYPE),  &
                     TEB_HNATVENT_TBL(URBTYPE), TEB_ZNATVENT_TBL(URBTYPE), &
                     TEB_F_WASTE_CAN_TBL(URBTYPE), &
                     TEB_QIN_TBL(URBTYPE), TEB_QIN_FRAD_TBL(URBTYPE),   &
                     TEB_QIN_FLAT_TBL(URBTYPE), TEB_GR_TBL(URBTYPE), TEB_EFF_HEAT_TBL(URBTYPE), TEB_INF_TBL(URBTYPE),                         &
                     TEB_TCOOL_TARGET_TBL(URBTYPE), TEB_THEAT_TARGET_TBL(URBTYPE), TEB_HR_TARGET_TBL(URBTYPE),        &
                     TEB_V_VENT_TBL(URBTYPE), TEB_CAP_SYS_HEAT_TBL(URBTYPE), TEB_CAP_SYS_RAT_TBL(URBTYPE), TEB_T_ADP_TBL(URBTYPE),   &
                     TEB_M_SYS_RAT_TBL(URBTYPE), TEB_COP_RAT_TBL(URBTYPE), &
                     TEB_HC_FLOOR_TBL(:, URBTYPE), &
                     TEB_TC_FLOOR_TBL(:, URBTYPE), &
                     TEB_D_FLOOR_TBL(:, URBTYPE), &
                     TEB_SHGC_TBL(URBTYPE),      &
                     TEB_SHGC_SH_TBL(URBTYPE),     &
                     TEB_LSHADE_TBL(URBTYPE), TEB_ZSHADE_TBL(URBTYPE),       &
                     TEB_CBEM_TBL(URBTYPE),                                                    &
                     TEB_CH_BEM_TBL(URBTYPE), TEB_ROUGH_ROOF_TBL(URBTYPE), TEB_ROUGH_WALL_TBL(URBTYPE), &
                     TEB_PAR_RD_IRRIG_TBL(URBTYPE), TEB_RD_START_MONTH_TBL(URBTYPE), TEB_RD_END_MONTH_TBL(URBTYPE),           &
                     TEB_RD_START_HOUR_TBL(URBTYPE), TEB_RD_END_HOUR_TBL(URBTYPE), TEB_RD_24H_IRRIG_TBL(URBTYPE), &
                     TEB_EMIS_PANEL_TBL(URBTYPE), TEB_ALB_PANEL_TBL(URBTYPE), TEB_EFF_PANEL_TBL(URBTYPE), TEB_FRAC_PANEL_TBL(URBTYPE),        &
                     TEB_RESIDENTIAL_TBL(URBTYPE),                                            &
                     TEB_DT_RES_TBL(URBTYPE), TEB_DT_OFF_TBL(URBTYPE), &
                     TEB_FLOOR_HEIGHT_TBL(URBTYPE), TEB_U_WIN_TBL(URBTYPE), &
                     ! outputs
                     TEB_HVAC_COOL(I,J), TEB_HVAC_HEAT(I,J),             &
                     TEB_THER_PROD_PANEL(I,J), TEB_PHOT_PROD_PANEL(I,J), &
                     U_CANYON,                                           &
                     RN_TOWN, H_TOWN, LE_TOWN, GFLUX_TOWN, EVAP_TOWN,    &
                     USTAR_TOWN,                                         &
                     TS_TOWN, EMIS_TOWN,                                 &
                     ALB_TOWN, DIR_CANYON, Q_TOWN                        &
                     )


            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! Update of INOUT variables
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! TEB does not provide 10 m wind diagnostics, instead windspeed and direction
            ! are provided at half the height of the canyon.
            ! TEB only accounts for a reduction in windspeed. Here we recalculate the U and V
            ! components with the updated windspeed as returned by TEB. The wind direction remains
            ! unchanged, i.e. DIR_CANYON == DIR.
            ! Note: U10/V10 is set to U10_URB2D/V10_URB2D in module_surface_driver.
            U10_URB2D(I,J) = CALC_U_FROM_WIND_SPEED_AND_DIR(U_CANYON(1), DIR_CANYON(1))
            V10_URB2D(I,J) = CALC_V_FROM_WIND_SPEED_AND_DIR(U_CANYON(1), DIR_CANYON(1))

            ! Net all-wave radiation (RN_TOWN) does not exist as general WRF variable,
            ! only as RN_URB2D, so use that for diagnostic purposes.
            RN_URB2D(I,J)  = RN_TOWN(1)

            ! Calculate 2 meter temperature and specific humidity for vegetation
            IF (CQS2(I,J) < 1.E-5) THEN
              Q2_RURAL = QSFC_RURAL(I,J)
            ELSE
              Q2_RURAL = QSFC_RURAL(I,J) - QFX_RURAL(I,J) / (RHO(I,KHL,J) * CQS2(I,J))
            ENDIF
            IF (CHS2(I,J) < 1.E-5) THEN
              T2_RURAL = TEB_TSK_RURAL(I,J)
            ELSE
              T2_RURAL = TEB_TSK_RURAL(I,J) - HFX_RURAL(I,J)/(RHO(I,KHL,J) * XCPD * CHS2(I,J))
            ENDIF

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! OUT: Town + Vegetation (TEB + Noah)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! TEB does not provide 2 m air temperature and humidity diagnostics,
            ! instead air temperature and humidity are provided at half the height of the canyon.
            ! TH2_URB2D/Q2_URB2D are assigned to Q2/T2/TH2 in the surface driver.
            ! Writing directly to Q2 etc. is not possible here as they get overridden.
            TH2_URB2D(I,J) = (1.-FRAC_NOAH)   * TEB_T_CANYON(I,J) + &
                             FRAC_NOAH        * T2_RURAL
            Q2_URB2D(I,J)  = (1.-FRAC_NOAH)   * GetHumRatioFromSpecificHum(TEB_Q_CANYON(I,J)) + &
                             FRAC_NOAH        * GetHumRatioFromSpecificHum(Q2_RURAL)
            EMISS_TEMP     = (1.-FRAC_NOAH)   * EMIS_TOWN(1) + &
                             FRAC_NOAH        * EMISSI_RURAL(I,J)
            EMISS(I,J)     = MAX(EMISS_TEMP, 1.E-10)
            ALBEDO(I,J)    = (1.-FRAC_NOAH)   * ALB_TOWN(1) + &
                             FRAC_NOAH        * ALB_RURAL(I,J)
            HFX(I,J)       = (1.-FRAC_NOAH)   * H_TOWN(1) + &
                             FRAC_NOAH        * HFX_RURAL(I,J)
            QFX(I,J)       = (1.-FRAC_NOAH)   * EVAP_TOWN(1) + &
                             FRAC_NOAH        * QFX_RURAL(I,J)
            LH(I,J)        = (1.-FRAC_NOAH)   * LE_TOWN(1) + &
                             FRAC_NOAH        * LH_RURAL(I,J)
            GRDFLX(I,J)    = (1.-FRAC_NOAH)   * (-1)*GFLUX_TOWN(1) + &
                             FRAC_NOAH        * GRDFLX_RURAL(I,J)
            TSK(I,J)       = (((1.-FRAC_NOAH) * EMIS_TOWN(1) * TS_TOWN(1)**4 + &
                               FRAC_NOAH      * EMISSI_RURAL(I,J) * TEB_TSK_RURAL(I,J)**4) &
                              / EMISS(I,J))**0.25
            QSFC(I,J)      = (1.-FRAC_NOAH)   * GetHumRatioFromSpecificHum(Q_TOWN(1)) + &
                             FRAC_NOAH        * QSFC_RURAL(I,J)
            UST(I,J)       = (1.-FRAC_NOAH)   * USTAR_TOWN(1) + &
                             FRAC_NOAH        * UST(I,J)

          ! 3. Pass-through condition
          ! =========================
          ! For non-urban cells, return the same conditions as given by the
          ! land surface model.
          ELSE
            ! Calculate 2 meter temperature and specific humidity for vegetation
            IF (CQS2(I,J) < 1.E-5) THEN
              Q2_RURAL = QSFC_RURAL(I,J)
            ELSE
              Q2_RURAL = QSFC_RURAL(I,J) - QFX_RURAL(I,J) / (RHO(I,KHL,J) * CQS2(I,J))
            ENDIF
            IF (CHS2(I,J) < 1.E-5) THEN
              T2_RURAL = TEB_TSK_RURAL(I,J)
            ELSE
              T2_RURAL = TEB_TSK_RURAL(I,J) - HFX_RURAL(I,J)/(RHO(I,KHL,J) * XCPD * CHS2(I,J))
            ENDIF

            ! TH2_URB2D/Q2_URB2D are assigned to Q2/T2/TH2 in the surface driver.
            ! Writing directly to Q2 etc. is not possible here as they get overridden.
            Q2_URB2D(I,J) = Q2_RURAL
            TH2_URB2D(I,J) = T2_RURAL

            ! No change
            ! Note: UST does not have a _RURAL variant, so no copy needed.
            ! Note: TSK is not updated for non-urban cells as done in BEP/BEM.
            EMISS(I,J)      = EMISSI_RURAL(I,J)
            ALBEDO(I,J)     = ALB_RURAL(I,J)
            HFX(I,J)        = HFX_RURAL(I,J)
            QFX(I,J)        = QFX_RURAL(I,J)
            LH(I,J)         = LH_RURAL(I,J)
            GRDFLX(I,J)     = GRDFLX_RURAL(I,J)
            QSFC(I,J)       = QSFC_RURAL(I,J)
            ! Only valid for urban-type grid points
            U10_URB2D(I,J)            = XUNDEF
            V10_URB2D(I,J)            = XUNDEF
            RN_URB2D(I,J)             = XUNDEF
            TEB_HVAC_COOL(I,J)        = XUNDEF
            TEB_HVAC_HEAT(I,J)        = XUNDEF
            TEB_THER_PROD_PANEL(I,J)  = XUNDEF
            TEB_PHOT_PROD_PANEL(I,J)  = XUNDEF
            !
          ENDIF
        END DO
      END DO
    END SUBROUTINE TEB


FUNCTION CALC_WIND_SPEED_FROM_U_V(U, V) RESULT(WIND_SPEED)
  real, intent(in)  :: U, V
  real              :: WIND_SPEED

  WIND_SPEED = SQRT(U**2. + V**2.)
END FUNCTION CALC_WIND_SPEED_FROM_U_V


FUNCTION CALC_WIND_DIR_FROM_U_V(U, V) RESULT(WIND_DIR)
  USE MODD_CSTS, ONLY: XPI
  real, intent(in)  :: U, V
  real              :: WIND_DIR

  WIND_DIR = 180. + (ATAN2(U, V) * 180./XPI)
END FUNCTION CALC_WIND_DIR_FROM_U_V


FUNCTION CALC_U_FROM_WIND_SPEED_AND_DIR(WIND_SPEED, WIND_DIR) RESULT(U)
  USE MODD_CSTS, ONLY: XPI
  real, intent(in)  :: WIND_SPEED, WIND_DIR
  real              :: U

  U = -WIND_SPEED * SIN(WIND_DIR * XPI/180.)
END FUNCTION CALC_U_FROM_WIND_SPEED_AND_DIR


FUNCTION CALC_V_FROM_WIND_SPEED_AND_DIR(WIND_SPEED, WIND_DIR) RESULT(V)
  USE MODD_CSTS, ONLY: XPI
  real, intent(in)  :: WIND_SPEED, WIND_DIR
  real              :: V

  V = -WIND_SPEED * COS(WIND_DIR * XPI/180.)
END FUNCTION CALC_V_FROM_WIND_SPEED_AND_DIR

function GetSpecificHumFromHumRatio(HumRatio) result(SpecificHum)
  !+ Return the specific humidity from humidity ratio (aka mixing ratio).
  !+ Reference:
  !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 9b

  real, intent(in) :: HumRatio
    !+ Humidity ratio in lb_H₂O lb_Dry_Air⁻¹ [IP] or kg_H₂O kg_Dry_Air⁻¹ [SI]
  real             :: SpecificHum
    !+ Specific humidity in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
  real              ::  BoundedHumRatio
    !+ Humidity ratio bounded to MIN_HUM_RATIO

  if (HumRatio < 0.0) then
    print *, "Warning: humidity ratio cannot be negative"
  end if
  BoundedHumRatio = max(HumRatio, MIN_HUM_RATIO)

  SpecificHum = BoundedHumRatio / (1.0 + BoundedHumRatio)
end function GetSpecificHumFromHumRatio

function GetHumRatioFromSpecificHum(SpecificHum) result(HumRatio)
  !+ Return the humidity ratio (aka mixing ratio) from specific humidity.
  !+ Reference:
  !+ ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 9b (solved for humidity ratio)

  real, intent(in)  :: SpecificHum
    !+ Specific humidity in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
  real              :: HumRatio
    !+ Humidity ratio in lb_H₂O lb_Dry_Air⁻¹ [IP] or kg_H₂O kg_Dry_Air⁻¹ [SI]

  if (SpecificHum < 0.0 .or. SpecificHum >= 1.0) then
    print *, "Warning: specific humidity is outside range [0, 1["
  end if

  HumRatio = SpecificHum / (1.0 - SpecificHum)

  ! Validity check.
  HumRatio = max(HumRatio, MIN_HUM_RATIO)
end function GetHumRatioFromSpecificHum

END MODULE MODULE_SF_TEB
#endif