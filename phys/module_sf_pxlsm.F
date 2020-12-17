MODULE module_sf_pxlsm
 
  USE module_model_constants
  USE module_sf_pxlsm_data

  INTEGER, PARAMETER   :: NSOLD=20
  REAL, PARAMETER      :: RD     = 287.04,   CPD   = 1004.67,             &
                          CPH2O  = 4.218E+3, CPICE = 2.106E+3,            &
                          LSUBF  = 3.335E+5, SIGMA = 5.67E-8,             &  
                          ROVCP  = RD / CPD
                          
  REAL, PARAMETER      :: CRANKP = 0.5                    ! CRANK-NIC PARAMETER
  REAL, PARAMETER      :: RIC    = 0.25                   ! critical Richardson number
  REAL, PARAMETER      :: DENW   = 1000.0                 ! water density in KG/M3                  
  REAL, PARAMETER      :: TAUINV = 1.0 / 86400.0          ! 1/1DAY(SEC)
  REAL, PARAMETER      :: T2TFAC = 1.0 / 10.0             ! Bottom soil temp response factor
  REAL, PARAMETER      :: PI = 3.1415926
  REAL, PARAMETER      :: PR0 = 0.95
  REAL, PARAMETER      :: CZO    = 0.032
  REAL, PARAMETER      :: OZO    = 1.E-4

CONTAINS
!

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
   SUBROUTINE pxlsm(U3D, V3D, DZ8W, QV3D,T3D,TH3D, RHO,         &   
                    PSFC, GSW, GLW, RAINBL, EMISS,              &
                    ITIMESTEP,CURR_SECS,NSOIL,DT,ANAL_INTERVAL, &            
                    XLAND, XICE, ALBBCK, ALBEDO,                &
                    SNOALB, SMOIS, TSLB, MAVAIL, TA2,           &
                    QA2, QSFC, ZS,DZS, PSIH,                          &
                    LANDUSEF,SOILCTOP,SOILCBOT,VEGFRA,VEGF_PX,  &
                    ISLTYP,RA,RS,LAI,IMPERV,CANFRA,NLCAT,NSCAT, & 
                    HFX,QFX,LH,TSK,SST,ZNT,CANWAT,              &
                    GRDFLX,SHDMIN,SHDMAX,                       &
                    SNOWC,PBLH,RMOL,UST,CAPG,DTBL,              &
                    T2_NDG_OLD, T2_NDG_NEW,                     &     
                    Q2_NDG_OLD, Q2_NDG_NEW,                     & 
                    SN_NDG_OLD, SN_NDG_NEW, SNOW, SNOWH,SNOWNCV,&
                    T2OBS, Q2OBS, PXLSM_SMOIS_INIT,             &           
                    PXLSM_SOIL_NUDGE,                           &
                    pxlsm_modis_veg,                            &
                    LAI_PX,                                     &
                    WWLT_PX, WFC_PX, WSAT_PX,                   &
                    CLAY_PX, CSAND_PX, FMSAND_PX,               &
                    ids,ide, jds,jde, kds,kde,                  &
                    ims,ime, jms,jme, kms,kme,                  &
                    its,ite, jts,jte, kts,kte                    )

!-------------------------------------------------------------------------
!   THIS MODULE CONTAINS THE PLEIM-XIU LAND-SURFACE MODEL (PX-LSM).
!   IT IS DESIGNED TO SIMULATE CHARACTERISTICS OF THE LAND SURFACE AND
!   VEGETATION AND EXCHANGE WITH THE PLANETARY BOUNDARY LAYER (PBL). THE
!   SOIL MOISTURE MODEL IS BASED ON THE ISBA SCHEME DEVELOPED BY NOILHAN
!   AND PLANTON (1989) AND JACQUEMIN AND NOILHAN (1990) AND INCLUDES
!   PROGNOSTIC EQUATIONS FOR SOIL MOISTURE AND SOIL TEMPERATURE IN TWO
!   LAYERS (1 CM AND 1 M) AS WELL AS CANOPY WATER CONTENT.  SURFACE
!   MOISTURE FLUXES ARE MODELED BY 3 PATHWAYS: SOIL EVAPORATION, CANOPY
!   EVAPORATION, AND VEGETATIVE EVAPOTRANSPIRRATION.
!   EVAPOTRANSPIRATION DIRECTLY FROM THE ROOT ZONE SOIL LAYER IS MODELED
!   VIA A CANOPY RESISTANCE ANALOG ALGORITHM WHERE STOMATAL CONDUCTANCE
!   IS CONTROLLED BY SOLAR RADIATION, AIR TEMPERATURE, AIR HUMIDITY, AND
!   ROOT ZONE SOIL MOISTURE. REQUIRED VEGETATION CHARACTERISTICS DERIVED
!   FROM THE USGS LANDUSE DATA INCLUDE: LEAF AREA INDEX, FRACTIONAL VEGETATION
!   COVERAGE, ROUGHNESS LENGTH, AND MINIMUM STOMATAL RESISTANCE. AN INDIRECT
!   NUDGING SCHEME ADJUSTS SOIL MOISTURE ACCORDING TO DIFFERENCES BETWEEN
!   MODELED TEMPERATURE AND HUMIDITY AND ANALYSED SURFACE FIELDS.
!
! References:
! Pleim and Xiu, 1995: Development and testing of a surface flux and planetary 
!                      boundary layer model for application in mesoscale models.
!                      J. Appl. Meteoro., Vol. 34, 16-32.
! Xiu and Pleim, 2001: Development of a land surface model. Part I: Application
!                      in a mesoscale meteorological model. J. Appl. Meteoro.,
!                      Vol. 40, 192-209.
! Pleim and Xiu, 2003: Development of a land surface model. Part II: Data
!                      assimilation. J. Appl. Meteoro., Vol. 42, 1811-1822.
! 
! Pleim and Gilliam, 2009: An Indirect Data Assimilation Scheme for Deep Soil Temperature in the 
!                          Pleim-Xiu Land Surface Model. J. Appl. Meteor. Climatol., 48, 1362-1376.
!
! Gilliam and Pleim, 2010: Performance assessment of new land-surface and planetary boundary layer 
!                          physics in the WRF-ARW. Journal of Applied Meteorology and Climatology, 49, 760-774. 
! REVISION HISTORY:
!     AX     4/2005  - developed the initial WRF version based on the MM5 PX LSM
!     RG     2/2008  - Completed testing of the intial working version of PX LSM, released in WRFV3.0 early 2008
!     DW     8/2011  - Landuse specific versions of PX (USGS/NLCD/MODIS) were unified into a single code with
!                      landuse characteristics defined in module_sf_pxsfclay.F.
!     RG     12/2011 - Basic code clean, removed commented out debug statements, lined up columns, etc.
!     RG     01/2012 - Removed FIRSTIME Logic that computes PX Landuse characteristics at first time step only. This resulted 
!                      in different solutions when OpenMP was used and would not work with moving domains.
!     RG     08/2012 - Added CURR_SECS variable in argument list as replacement for PX internal CURRTIME internally comp var.
!                      This is neccessary for PX to correctly interpolate analyses for soil nudging. In this same calculation
!                      logic was added for cases where user does not specify the analysis interval, or no analysis interval is
!                      relevant as in the no PX soil nudging via namelist (pxlsm_soil_nudge = 0). Prior to this fix the default
!                      analysis interval was zero, so if not speficied a divide by zero was the result. Also, changes were made to 
!                      ensure PX LSM will work with not only MODIS and USGS, but also both the 40 and 50 class NLCD-MODIS data.
!                      Also, coupled module_sf_pxlsm_data.F was updated so landuse characteristics across datasets are more
!                      consistent. Albedo for NLCD two grassland categories were lowered from 23 and 25 to 18 and 19.
!                      For the NLCD40 and NLCD50 roughness and leaf area were made consistent between the US NLCD and
!                      outside US MODIS datasets. Prior, US boundaries created boundaries of roughness and LAI.
!
!     RG     10/2014 - Wetlands soil moisture treatment. Grid cell soil moisture cannot fall less than fraction of a grid
!                      cells wetland area * soil saturation (e.g., SMOIS of cell with 50% wetlands cannot fall below 50% of WSAT)
!                    - Both soil levels are initialized using MVAVAIL (Soil moisture availability) instead of just layer 2.
!                    - Veg Cv (heat capacity) changed from 8x10-6 to 1.2x10-5 (K-M2/J)
!                    - Alternate empirical stomatal function of PAR (F1) to better replicate photosynthesis-conductance models.
!                      The main effect is to reduce stomatal conductance for low PAR.
!                    - Snow albedo is now computed using fractional land-use weighting. Values for each land-use class
!                      are defined like other PX landuse parameters in module_sf_pxlsm_data.F. These are based on values
!                      used by NOAH LSM MODIS in VEGPARM.TBL (MAXALB), but tuned to better match satellite values in maxsnowalb 
!                      dataset. Tuning reduced the MAXALB for all forest classes from values in the 50-60% range to 30-40% range.
!                      These static values are more representative of albedo after snow has melted of fallen from trees. These
!                      values were also cross verified with http://www.globalbedo.org/global.php
!                    - USGS 28 category added as an option
!                    - Impervious surface and canopy fraction data can be used if processed (otherwise 0% so no impact)
!                      to alter surface heat capacity (See SURFPX subroutine for details) in urban areas and refine
!                      LAI and VEGF_PX estimations (see VEGLAND subroutine).
!
!     JP     12/2015 - Surface water vapor mixing ratio calculation added for land surface, which is passed to PX-SFCLAY 
!                      for use over all non-water and non-frozen surfaces.
!                    - PAR function and impact on transpiration modified according to Echer et al.(2015). See P-X LSM documentation
!                      for full reference. These act to reduce moisture bias near surface during PBL transition.
!
!    JP     11/2017 -  Updated vegetation table for different land cover types. Added in WRFv4.0.
!
!    LR     11/2017 -  Update for MODIS vegetation: many changes in soil properties. Added in WRFv4.1. 
!                      (Ran et al., 2016 JGR-atmosphere, Ran et al. 2017 in preparation)
!    JP     12/2018 -  revised soil type categories (ISTI) to conform to WRF soil type input data 
!                      soil types Sand through Clay are now 1-12 rather than 1-11

!
!--------------------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------------------
!   ARGUMENT LIST:
!
!... Inputs:
!-- U3D          3D u-velocity interpolated to theta points (m/s)
!-- V3D          3D v-velocity interpolated to theta points (m/s)
!-- DZ8W         dz between full levels (m)
!-- QV3D	       3D mixing ratio
!-- T3D          Temperature (K)
!-- TH3D         Theta (K)
!-- RHO          3D dry air density (kg/m^3)

!-- PSFC         surface pressure (Pa)
!-- GSW          downward short wave flux at ground surface (W/m^2)      
!-- GLW          downward long wave flux at ground surface (W/m^2)
!-- RAINBL       Timestep rainfall 
!-- EMISS        surface emissivity (between 0 and 1)

!-- ITIMESTEP    time step number 
!-- NSOIL        number of soil layers
!-- DT           time step (second)
!-- CURR_SECS    time on model domain in seconds, universal WRF variable
!-- ANAL_INTERVAL Interval of analyses used for soil moisture and temperature nudging

!-- XLAND        land mask (1 for land, 2 for water)
!-- XICE         Sea ice
!-- ALBBCK       Background Albedo
!-- ALBEDO       surface albedo with snow cover effects
!-- SNOALB       Albedo of snow

!-- SMOIS        total soil moisture content (volumetric fraction)
!-- TSLB         soil temp (k)
!-- MAVAIL       Moisture availibility of soil
!-- TA2          2-m temperature
!-- QA2          2-m mixing ratio

!-- SVPT0        constant for saturation vapor pressure (K)
!-- SVP1         constant for saturation vapor pressure (kPa)
!-- SVP2         constant for saturation vapor pressure (dimensionless)
!-- SVP3         constant for saturation vapor pressure (K)

!-- ZS           depths of centers of soil layers
!-- DZS          thicknesses of soil layers
!-- PSIH         similarity stability function for heat

!-- LANDUSEF     Landuse fraction		
!-- SOILCTOP     Top soil fraction		
!-- SOILCBOT     Bottom soil fraction		
!-- VEGFRA       Vegetation fraction                   
!-- VEGF_PX      Veg fraction recomputed and used by PX LSM
!-- ISLTYP       Soil type

!-- RA           Aerodynamic resistence			
!-- RS           Stomatal resistence			
!-- LAI          read in Leaf area index (weighted according to fractional landuse)
!-- ZNT          rougness length
!-- QSFC         Sat. water vapor mixing ratio at the surface interface

!-- IMPERV       Fraction (percent) of grid cell that is impervious surface (concrete/road/non-veg)
!-- CANFRA       Fraction (percent) of grid cell that is covered with tree canopy

!-- NLCAT        Number of landuse categories		
!-- NSCAT        Number of soil categories		

!-- HFX          net upward heat flux at the surface (W/m^2)
!-- QFX          net upward moisture flux at the surface (kg/m^2/s)
!-- LH           net upward latent heat flux at surface (W/m^2)
!-- TSK          surface skin temperature (K)
!-- SST          sea surface temperature
!-- CANWAT       Canopy water (mm)

!-- GRDFLX       Ground heat flux
!-- SFCEVP       Evaportation from surface
!-- SHDMIN       Minimum annual vegetation fraction for each grid cell
!-- SHDMAX       Maximum annual vegetation fraction for each grid cell

!-- SNOWC        flag indicating snow coverage (1 for snow cover)
!-- PBLH         PBL height (m)
!-- RMOL         1/L Reciprocal of Monin-Obukhov length
!-- UST          u* in similarity theory (m/s)
!-- CAPG         heat capacity for soil (J/K/m^3)
!-- DTBL	       time step of boundary layer calls

!-- T2_NDG_OLD   Analysis temperature prior to current time
!-- T2_NDG_NEW   Analysis temperature ahead of current time
!-- Q2_NDG_OLD   Analysis mixing ratio prior to current time
!-- Q2_NDG_NEW   Analysis mixing ratio ahead of current time

!-- SN_NDG_OLD   Analysis snow water prior to current time
!-- SN_NDG_NEW   Analysis snow water ahead of current time
!-- SNOW         Snow water equivalent
!-- SNOWH        Physical snow depth
!-- SNOWNCV      Time step accumulated snow

!-- T2OBS             Analysis temperature interpolated from prior and next in time analysese
!-- Q2OBS             Analysis moisture interpolated from prior and next in time analysese
!-- PXLSM_SMOIS_INIT  Flag to intialize deep soil moisture to a value derived from moisture availiability.
!-- PXLSM_SOIL_NUDGE  Flag to use soil moisture and temperature nudging in the PX LSM
!                     This is typically done for the first simulation.
!-- pxlsm_modis_veg   Use MODIS vegetation option: 1 yes, 0 no
!-- LAI_PX            LAI used for PX (m^2/m^2)
!-- WWLT_PX           Computed soil wilting point for PX (m^3/m^3)
!-- WFC_PX            Computed soil field capacity for PX (m^3/m^3)
!-- WSAT_PX           Computed soil saturation for PX (m^3/m^3) 
!-- CLAY_PX           Aggregated soil clay fraction for PX (%)
!-- CSAND_PX          Aggregated soil coarse sand fraction for PX (%)
!-- FMSAND_PX         Aggregated soil fine-medium sand fraction for PX (%)
!
!-- ids             start index for i in domain
!-- ide             end index for i in domain
!-- jds             start index for j in domain
!-- jde             end index for j in domain
!-- kds             start index for k in domain
!-- kde             end index for k in domain
!-- ims             start index for i in memory
!-- ime             end index for i in memory
!-- jms             start index for j in memory
!-- jme             end index for j in memory
!-- kms             start index for k in memory
!-- kme             end index for k in memory
!-- jts             start index for j in tile
!-- jte             end index for j in tile
!-- kts             start index for k in tile
!-- kte             end index for k in tile
!... Outputs:

     IMPLICIT NONE

!.......Arguments
! DECLARATIONS - INTEGER
    INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                   ims,ime, jms,jme, kms,kme, &
                                   its,ite, jts,jte, kts,kte 

   INTEGER,   INTENT(IN)  ::      NSOIL, ITIMESTEP, NLCAT, NSCAT,             &
                                  ANAL_INTERVAL, PXLSM_SMOIS_INIT, PXLSM_SOIL_NUDGE

   REAL,       INTENT(IN   ),OPTIONAL    ::     curr_secs

   REAL,     INTENT(IN )  ::      DT,DTBL 

   INTEGER,   DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::   ISLTYP

! DECLARATIONS - REAL
    REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ),  INTENT(IN) :: U3D, V3D, RHO, &
                                                                    T3D, TH3D, DZ8W, QV3D           

   REAL,     DIMENSION(1:NSOIL), INTENT(IN)::ZS,DZS
   REAL,     DIMENSION( ims:ime , 1:NSOIL, jms:jme ),  INTENT(INOUT)   ::  SMOIS, TSLB     

   REAL,     DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: RA, RS, LAI, ZNT, QSFC
   REAL,     DIMENSION( ims:ime , jms:jme ), INTENT(OUT):: GRDFLX, TSK, TA2, QA2

   REAL,     DIMENSION( ims:ime , 1:NLCAT, jms:jme ), INTENT(IN):: LANDUSEF       
   REAL,     DIMENSION( ims:ime , 1:NSCAT, jms:jme ), INTENT(IN):: SOILCTOP, SOILCBOT


   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(IN) ::                            PSFC, GSW, GLW, RAINBL,   &                
                                                     ALBBCK, SHDMIN, SHDMAX,   &
                                                     PBLH, RMOL, SNOWNCV,      &
                                                     UST, MAVAIL, SST, EMISS
                                                      
   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(IN) ::                            T2_NDG_OLD, T2_NDG_NEW,   &                
                                                     Q2_NDG_OLD, Q2_NDG_NEW,   & 
                                                     SN_NDG_OLD, SN_NDG_NEW   

   REAL,    DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: T2OBS, Q2OBS                
                                                     
   REAL,    DIMENSION( ims:ime, jms:jme ),                                     &
            INTENT(INOUT) ::           CAPG,CANWAT, QFX, HFX, LH,              &
                                       PSIH,VEGFRA, VEGF_PX, SNOW, SNOALB,     &
                                       SNOWH, SNOWC, ALBEDO, XLAND, XICE,      &
                                       IMPERV, CANFRA

   INTEGER, OPTIONAL, INTENT(IN)  :: pxlsm_modis_veg

   REAL,    DIMENSION( ims:ime,  jms:jme ),                                    &
            OPTIONAL, INTENT(OUT)  :: LAI_PX, WWLT_PX, WFC_PX, WSAT_PX,        &
                                      CLAY_PX, CSAND_PX, FMSAND_PX
   INTEGER  ::  KWAT

   LOGICAL :: radiation

!-------------------------------------------------------------------------
!     ---------- Local Variables --------------------------------


      !----  PARAMETERS
      INTEGER, PARAMETER  :: NSTPS  = 11     ! max. soil types
      REAL, PARAMETER     :: DTPBLX = 40.0   ! Max PX timestep = 40 sec

      
      !----  INTEGERS
      INTEGER, DIMENSION( 1: NSTPS ) :: JP
      INTEGER:: J, I, NS, NUDGE, ISTI, WEIGHT
      INTEGER:: NTSPS, IT

      !----  REALS
      REAL,     DIMENSION( ims:ime, jms:jme ) :: XLAI, XLAIMN, RSTMIN, &
                                                 XVEG, XVEGMN, XSNUP, &
                                                 XALB, XSNOALB, WETFRA
                                                  
      REAL,     DIMENSION( ims:ime, jms:jme ) :: RADNET, EG, ER, ETR, QST

      REAL:: SFCPRS,TA1,DENS1,QV1,ZLVL,SOLDN,LWDN,    &
            EMISSI,PRECIP,THETA1,VAPPRS,QSBT,         &
            WG,W2,WR,TG,T2,USTAR,MOLX,Z0,             &
            RAIR,CPAIR,IFLAND,ISNOW,                  &
            ES,QSS,BETAP,                             &
            RH2_OLD, RH2_NEW, T2_OLD, T2_NEW,         &
            CORE, CORB, TIME_BETWEEN_ANALYSIS,        &
            G1000, ALN10,RH2OBS, HU, SNOBS,           &
            FWSAT,FWFC,FWWLT,FB,FCGSAT,FJP,FAS,       &
            FWRES, FC3, FCLAY, FCSAND, FFMSAND,       &   ! Soil model updates - JEP 12/14, LR 04/2017
            FSEAS, T2I, HC_SNOW, SNOW_FRA,SNOWALB,    &
            QST12,ZFUNC,ZF1,ZA2,QV2, DT_FDDA,         &
            FC2R,FC1SAT, DTPBL, RAW


      CHARACTER (LEN = 6) :: LAND_USE_TYPE

!-------------------------------------------------------------------------
!-------------------------------Executable starts here--------------------
!
      ALN10  = ALOG(10.0)
      G1000  = g*1.0E-3            ! G/1000
      WEIGHT = 0
      ! Determine Landuse Dataset by the number of categories
      IF (NLCAT == 50) THEN
         LAND_USE_TYPE = 'NLCD50'
      ELSE IF (NLCAT == 40) THEN
         LAND_USE_TYPE = 'NLCD40'
      ELSE IF (NLCAT == 20) THEN
         LAND_USE_TYPE = 'MODIS'
      ELSE IF (NLCAT == 21) THEN
         LAND_USE_TYPE = 'MODIS'
      ELSE IF (NLCAT == 24) THEN
         LAND_USE_TYPE = 'USGS'
      ELSE IF (NLCAT == 28) THEN
         LAND_USE_TYPE = 'USGS28'
      ELSE
         call wrf_error_fatal("Error: Unknown Land Use Category")
      END IF 
      
      IF (ITIMESTEP .EQ. 1) THEN
       CALL wrf_message( 'PX LSM will use the ' // TRIM(LAND_USE_TYPE) // ' landuse tables' )
       PRINT *, 'The analysis interval for surface soil and temp nudging = ',ANAL_INTERVAL,'sec.'
      ENDIF

      !-----------------------------------------------------------------------------------
      ! Kill WRF if user specifies soil nudging but provides no analysis interval, then provide helpful message.
      IF (ANAL_INTERVAL .LE. 0.0 .AND. PXLSM_SOIL_NUDGE .EQ. 1) THEN
       CALL wrf_message('PX LSM Error: The User specified analysis interval is zero or negative.')
       CALL wrf_message('If the PX LSM is used with soil nudging (pxlsm_soil_nudge=1) a wrfsfdda_d0* file is required.')
       CALL wrf_message('Make sure these files are present and')
       CALL wrf_message('Check the namelist to ensure sgfdda_interval_m is set to proper sfc analysis interval')
       STOP
      ENDIF                                                                    
      !-----------------------------------------------------------------------------------
      !--- Compute time relatve to old and new analysis time for timestep interpolation
      IF(PXLSM_SOIL_NUDGE .EQ. 1) THEN
        DT_FDDA = ANAL_INTERVAL * 1.0    ! Convert DT of Analysis to real
        TIME_BETWEEN_ANALYSIS = MOD(CURR_SECS,DT_FDDA)
        IF (TIME_BETWEEN_ANALYSIS .EQ. 0.0) THEN
          CORB = 1.0
          CORE = 0.0    
        ELSE
          CORE = TIME_BETWEEN_ANALYSIS  / DT_FDDA
          CORB = 1.0 - CORE
        ENDIF 
      ENDIF 
      !-----------------------------------------------------------------------------------
      ! Compute vegetation and land-use characteristics by land-use fraction weighting
      ! These parameters include LAI, VEGF, ZNT, ALBEDO, SNOALB, RS, etc.
      CALL VEGELAND(LANDUSEF, VEGFRA, SHDMIN, SHDMAX,         &
                    SOILCTOP, SOILCBOT, NLCAT, NSCAT,         &
                    ZNT,XLAI,XLAIMN,RSTMIN,XVEG,XVEGMN,XSNUP, &
                    XLAND, XALB,XSNOALB,WETFRA,IMPERV,CANFRA, &
                    ids,ide, jds,jde, kds,kde,                &
                    ims,ime, jms,jme, kms,kme,                &
                    its,ite, jts,jte, kts,kte, LAND_USE_TYPE, &
                    KWAT )
      !-----------------------------------------------------------------------------------
      

      !-----------------------------------------------------------------------------------
      ! Main loop over individual grid cells
      DO J = jts,jte    !-- J LOOP
       DO I = its,ite   !-- I LOOP
          
          IFLAND = XLAND(I,J)

          ! Compute soil properties via weighting of fractional components
          IF (IFLAND .LT. 1.5 ) THEN 

          !---------------------------------------------------------
            CALL SOILPROP (SOILCBOT(I,:,J), WEIGHT,               &
                         ITIMESTEP, MAVAIL(I,J),                  &
                         PXLSM_SMOIS_INIT,                        &
                         FWSAT,FWFC,FWWLT,FCLAY,FCSAND,FFMSAND,   &
                         FB,FCGSAT,                               &  
                         FJP,FAS,FC2R,FC1SAT,FWRES, FC3, ISTI,    & 
                         SMOIS(I,1,J),  SMOIS(I,2,J)    )
          !----------------------------------------------------------
          !----------------------------------------------------------
            ISLTYP(I,J) = ISTI                      
          ELSE
            ISLTYP(I,J) = 14                    ! STATSGO type for water

            !-- aded for MODIS model
            FWWLT = 0.1
            FWFC = 1.0
            FWSAT = 1.0

            FCLAY = 0.0
            FCSAND = 0.0
            FFMSAND = 0.0
            !-- end

          ENDIF

          !-- aded for MODIS model
          WWLT_PX(I,J) = FWWLT
          WFC_PX(I,J) = FWFC
          WSAT_PX(I,J) = FWSAT

          CLAY_PX(I,J) = FCLAY * 0.01   ! percent to fraction
          CSAND_PX(I,J) = FCSAND * 0.01
          FMSAND_PX(I,J) = FFMSAND * 0.01
          !-- end

          !--  Variables Sub. SURFPX needs
          SFCPRS = PSFC(i,j) / 1000.0                 ! surface pressure in cb
          TA1    = T3D(i,1,j)                         ! air temperature at first layer
          DENS1  = RHO(I,1,J)                         ! air density at first layer
          QV1    = QV3D(i,1,j)                        ! water vapor mixing ratio at first layer
          QV2    = QV3D(i,2,j)
          ZLVL   = 0.5 * DZ8W(i,1,j)                  ! thickness of lowest half level
          ZF1    = DZ8W(i,1,j)
          ZA2    = ZF1 + 0.5 * DZ8W(i,2,j)

          LWDN   = GLW(I,J)                           ! longwave radiation
          EMISSI = EMISS(I,J)                         ! emissivity
          PRECIP = MAX ( 1.0E-3*RAINBL(i,j)/DTBL,0.0) ! accumulated precip. rate during DT (=dtpbl)
                                                      ! convert RAINBL from mm to m for PXLSM
          WR     = 1.0E-3*CANWAT(I,J)                 ! convert CANWAT from mm to m for PXLSM
          THETA1 = TH3D(i,1,j)                        ! potential temp at first layer
          SNOBS  = SNOW(I,J)                          ! Set snow cover to existing model value
                                                      !   this is overwritten below if snow analysis is availiable
                                                      !   otherwise snow cover remains constant through simulation
                   
          IF(PXLSM_SOIL_NUDGE .EQ. 1) THEN                   
            !-- 2 m Temp and RH for Nudging
            T2_OLD = T2_NDG_OLD(I,J)
            T2_NEW     = T2_NDG_NEW(I,J)
            VAPPRS     = SVP1 * EXP(SVP2 * (T2_OLD - SVPT0) / ( T2_OLD - SVP3))
            QSBT       = EP_2 * VAPPRS / (SFCPRS - VAPPRS)          
            RH2_OLD    = Q2_NDG_OLD(I,J) / QSBT
            VAPPRS     = SVP1 * EXP(SVP2 * (T2_NEW - SVPT0) / (T2_NEW - SVP3))
            QSBT       = EP_2 * VAPPRS / (SFCPRS - VAPPRS)          
            RH2_NEW    = Q2_NDG_NEW(I,J) / QSBT
            RH2OBS     = CORB * RH2_OLD +  CORE * RH2_NEW  
            T2OBS(I,J) = CORB * T2_OLD +  CORE * T2_NEW
            Q2OBS(I,J) = CORB * Q2_NDG_OLD(I,J) +  CORE * Q2_NDG_NEW(I,J)
            SNOBS = CORB * SN_NDG_OLD(I,J) +  CORE * SN_NDG_NEW(I,J)  
          ENDIF

          USTAR  = MAX(UST(I,J),0.005)
          
          IF (IFLAND .GE. 1.5) THEN ! if over water                            
            ZNT(I,J) = CZO * UST(I,J) * UST(I,J) / G + OZO
          ENDIF                                                              
          
          Z0       = ZNT(I,J)
          CPAIR    = CPD * (1.0 + 0.84 * QV1)            ! J/(K KG)

          ! Set WRF Snow albedo to PX snow albedo based on fractional landuse
          ! Snow albedo for each landuse class is defined in module_sf_pxlsm_data.F
          SNOALB(I,J) = XSNOALB(I,J)
          !-------------------------------------------------------------
          ! Compute fractional snow area and snow albedo
          CALL PXSNOW (ITIMESTEP, SNOBS, SNOWNCV(I,J), SNOW(I,J),  &
                       SNOWH(I,J), XSNUP(I,J),  XALB(i,j),         &
                       SNOALB(I,J),VEGF_PX(I,J), SHDMIN(I,J),      &
                       HC_SNOW, SNOW_FRA, SNOWC(I,J),  ALBEDO(I,J) ) 
          !-------------------------------------------------------------
                                 
          !-------------------------------------------------------------
          ! Sea Ice from analysis and water cells that are very cold, but more than 50% water
          ! are converted to ice/snow for more reasonable treatment.
          IF( (XICE(I,J).GE.0.5) .OR.   &
              (SST(I,J).LE.270.0.AND.XLAND(I,J).GE.1.50) ) THEN
              XLAND(I,J) = 1.0
              IFLAND = 1.0
              ZNT(I,J) = 0.001  !  Ice
              SMOIS(I,1,J) = 1.0     ! FWSAT
              SMOIS(I,2,J) = 1.0     ! FWSAT
              XICE(I,J) = 1.0
              ALBEDO(I,J) = 0.7
              SNOWC(I,J) = 1.0
              SNOW_FRA = 1.0
              VEGF_PX(I,J) = 0.0
              LAI(I,J) = 0.0
          ENDIF
          !-------------------------------------------------------------

          !-------------------------------------------------------------
          !-- Note that when IFGROW = 0 is selected in Vegeland then max and min           
          !-- LAI and Veg are the same                                                     
          T2I = TSLB(I,2,J)                                            
!         FSEAS = AMAX1(1.0 - 0.0016 * (298.0 - T2I) ** 2,0.0)  ! BATS            
          FSEAS = AMAX1(1.0 - 0.015625 * (290.0 - T2I) ** 2,0.0) ! JP97            
          IF (T2I .GE. 290.0) FSEAS = 1.0                                          
          
          !get PX table vegetation
          LAI_PX(I,J)    = XLAIMN(I,J) + FSEAS*(XLAI(I,J) - XLAIMN(I,J))
          VEGF_PX(I,J)   = XVEGMN(I,J) + FSEAS*(XVEG(I,J) - XVEGMN(I,J))                       
          
!... use MODIS LAI and VEGFRA from wrflowinp
          IF ( pxlsm_modis_veg .EQ. 1 ) THEN

!               IF ( I .EQ. 300 .AND. J .EQ. 120 ) THEN
!                    print*, "  I=",I," J=",J," LAI_PX=",LAI_PX(I,J)," VEGF_PX=",VEGF_PX(I,J), &
!                            " LAI=",LAI(I,J)," VEGFRA=",VEGFRA(I,J)
!               ENDIF

                ! get LAI for vegetated area
                IF ( VEGFRA(I,J) .GT. 0.0 )  THEN
                   LAI_PX(I,J) = LAI(I,J) / ( VEGFRA(I,J) / 100.0)
                ELSE
                   LAI_PX(I,J) = 0.0
                ENDIF

                VEGF_PX(I,J) = VEGFRA(I,J) / 100.0

                !vegF is just for the land
                IF ( LANDUSEF(I,KWAT,J) .LT. 1.0 )  THEN
                   VEGF_PX(I,J) = VEGF_PX(I,J) / (1.0 - LANDUSEF(I,KWAT,J))
                ELSE
                   VEGF_PX(I,J) = 0.0
                ENDIF

          ENDIF

          LAI_PX(I,J) = MIN(LAI_PX(I,J), 8.0)
          LAI_PX(I,J) = MAX(LAI_PX(I,J), 0.0001)

          VEGF_PX(I,J) = MIN(VEGF_PX(I,J), 1.0)
          VEGF_PX(I,J) = MAX(VEGF_PX(I,J), 0.0001)

!... END OF MODIS LAI and FPAR


          ! Ensure veg algorithms not used for water 
          IF (IFLAND .GE. 1.5) THEN                        
             VEGF_PX(I,J) = 0.0
             LAI_PX(I,J) = 0.0         
          ENDIF                                                                   
          !-------------------------------------------------------------


          SOLDN  = GSW(I,J) / (1.0 - ALBEDO(I,J))     ! downward shortwave radiaton
          ISNOW = SNOWC(I,J)


          NUDGE=PXLSM_SOIL_NUDGE
          IF ( J .LE. 2 .OR. J .GE. (jde-1) ) NUDGE=0
          IF ( I .LE. 2 .OR. I .GE. (ide-1) ) NUDGE=0
          
          IF ( RMOL(I,J) .GT. 0.0 )  THEN
              MOLX = AMIN1(1/RMOL(I,J),1000.0)
          ELSE IF ( RMOL(I,J) .LT. 0.0 ) THEN
              MOLX = AMAX1(1/RMOL(I,J),-1000.0)
          ELSE
              MOLX = 1000
          ENDIF   
 
          ZFUNC = ZF1 * (1.0 - ZF1 / MAX(100.,PBLH(I,J))) ** 2
          QST12 = KARMAN * ZFUNC*(QV2-QV1) / (ZA2-ZLVL)
               

          !-------------------------------------------------------------
          !-- LSM sub-time loop too prevent dt > 40 sec       
          NTSPS = INT(DT / (DTPBLX + 0.000001) + 1.0)                                
          DTPBL = DT / NTSPS                                                       

          DO IT=1,NTSPS                                                     
          
            !... SATURATION VAPOR PRESSURE (MB) 
            IF ( TSLB(I,1,J) .LE. SVPT0 ) THEN        ! For ground that is below freezing
              ES = SVP1 * EXP(22.514 - 6.15E3 / TSLB(I,1,J))      ! cb
            ELSE
              ES = SVP1 * EXP(SVP2 * (TSLB(I,1,J) - SVPT0) /  (TSLB(I,1,J) - SVP3))
            ENDIF
            QSS  = ES * 0.622 / (SFCPRS - ES)
          
            !... beta method, Lee & Pielke (JAM,May1992)
            BETAP = 1.0
            IF (IFLAND .LT. 1.5 .AND. ISNOW .LT. 0.5 .AND. SMOIS(I,1,J) .LE. FWFC) THEN       
              BETAP = 0.25 * (1.0 - COS(SMOIS(I,1,J) / FWFC * PI)) ** 2
            ENDIF
            
            !-------------------------------------------------------------------------
            CALL SURFPX (DTPBL, IFLAND, SNOWC(I,J),  NUDGE, XICE(I,J),          & !in
                      SOLDN,  GSW(I,J), LWDN,   EMISSI, ZLVL,                   & !in
                      MOLX,    Z0,    USTAR,                                    & !in
                      SFCPRS, DENS1,  QV1,    QSS,   TA1,                       & !in
                      THETA1,   PRECIP,                                         & !in
                      CPAIR, PSIH(I,J),                                         & !in
                      RH2OBS,T2OBS(I,J),                                        & !in
                      VEGF_PX(I,J), ISTI, LAI_PX(I,J), IMPERV(I,J), CANFRA(I,J),   & !in
                      BETAP, RSTMIN(I,J), HC_SNOW, SNOW_FRA, WETFRA(I,J),       & !in          
                      FWWLT, FWFC, FWRES, FCGSAT,  FWSAT, FB,                   & !in    ! Soil model updates - JEP 12/14
                      FC1SAT,FC2R,FAS,FJP,FC3,DZS(1),DZS(2),QST12,              & !in
                      RADNET(I,J), GRDFLX(I,J), HFX(I,J), QFX(I,J), LH(I,J),    & !out
                      EG(I,J), ER(I,J), ETR(I,J),                               & !out
                      QST(I,J), CAPG(I,J), RS(I,J), RA(I,J),                    & !out
                      TSLB(I,1,J), TSLB(I,2,J),                                 & !out
                      SMOIS(I,1,J), SMOIS(I,2,J), WR,                           &
                      TA2(I,J), QA2(I,J), LAND_USE_TYPE,I,J )
            !-------------------------------------------------------------------------
          
          END DO                        ! Time internal PX time loop   

          TSK(I,J)    = TSLB(I,1,J)     ! Skin temp set to 1 cm soil temperature in PX for now
          CANWAT(I,J) = WR * 1000.      ! convert WR back to mm for CANWAT
          RAW = RA(I,J) + 4.503 / USTAR
          QSFC(I,J) = QFX(I,J) * RAW / DENS1 + QV1
          
       ENDDO !  END MIAN I LOOP
      ENDDO  !  END MAIN J LOOP
      
!------------------------------------------------------
   END SUBROUTINE pxlsm
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE VEGELAND( landusef, vegfra,                            & 
                            shdmin, shdmax,                             &                          
                            soilctop, soilcbot, nlcat, nscat,znt, xlai, &
                            xlaimn, rstmin, xveg, xvegmn, xsnup, xland, &
                            xalb, xsnoalb, wetfra, imperv, canfra,      &
                            ids,ide, jds,jde, kds,kde,                  &
                            ims,ime, jms,jme, kms,kme,                  &
                            its,ite, jts,jte, kts,kte,                  &
                            LAND_USE_TYPE, KWAT_OUT )
!-------------------------------------------------------------------------
!           
!   CALLED FROM Sub. bl_init in module_physics.init.F
!           
!   THIS PROGRAM PROCESSES THE USGS LANDUSE DATA
!   WHICH HAS BEEN GRIDDED BY THE WPS SYSTEM
!   AND PRODUCES 2-D FIELDS OF LU RELATED PARAMETERS
!   FOR USE IN THE PX SURFACE MODEL
!
!
! REVISION HISTORY:
!  AX      Oct 2004 - developed WRF version based on VEGELAND in the MM5 PX LSM
!  RG      Dec 2006 - revised for WRFV2.1.2
!  JP      Dec 2007 - revised for WRFV3 - removed IFGROW options
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
                              
      IMPLICIT NONE
!... 
      INTEGER,  INTENT(IN)        ::  ids,ide, jds,jde, kds,kde,  &
                                      ims,ime, jms,jme, kms,kme,  &
                                      its,ite, jts,jte, kts,kte
                                                                            
      INTEGER , INTENT(IN)        ::  NSCAT, NLCAT

      REAL,    DIMENSION( ims:ime , 1:NLCAT, jms:jme ),  INTENT(IN) :: LANDUSEF                        
      REAL,    DIMENSION( ims:ime , 1:NSCAT, jms:jme ),  INTENT(IN) :: SOILCTOP, SOILCBOT
                                                                   
      REAL,    DIMENSION( ims:ime, jms:jme ),            INTENT(IN) ::  VEGFRA, SHDMIN, SHDMAX 

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ZNT, IMPERV, CANFRA

      REAL,     DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: XLAI, XLAIMN, RSTMIN, XALB, &
                                                              XVEG, XVEGMN, XSNUP, XLAND, &
                                                              WETFRA, XSNOALB 

      INTEGER, INTENT(OUT)      ::  KWAT_OUT

      CHARACTER (LEN = 6), INTENT(IN) :: LAND_USE_TYPE

                                     
!... local variables

      INTEGER :: ITF, JTF, K, J, I
      REAL    :: SUMLAI, SUMLMN, SUMRSI, SUMLZ0, SUMVEG, SUMVMN, &
                 ALAI, VEGF, SUMSNUP, SUMALB, SUMSNOALB
                 
      REAL    :: VFMX, VFMN, VSEAS, FAREA, FWAT, ZNOTC, FCAN, FIMP, FORFRA, EXTFOR
      REAL,   DIMENSION( NLCAT )  :: LAIMX, LAIMN, Z0, VEG, VEGMN, SNUP, ALB, SNOALB

      REAL, PARAMETER :: ZNOTCMN = 5.0  ! CM, MIN Zo FOR CROPS
      REAL, PARAMETER :: ZNOTCMX = 15.0 ! CM, MAX Zo FOR CROPS

      REAL, SAVE, DIMENSION(:), POINTER  :: RSMIN, Z00, VEG0, VEGMN0, LAI0, &
                                            LAIMN0, SNUP0, ALBF, SNOALBF

      !---- INITIALIZE PARAMETERS
      INTEGER, SAVE :: KWAT, LIMIT1, LIMIT2

     ! Initialize LU characteristics by LU Dataset
      IF (LAND_USE_TYPE == 'USGS') THEN
         KWAT = 16
         RSMIN  => RSMIN_USGS
         Z00    => Z00_USGS  
         VEG0   => VEG0_USGS 
         VEGMN0 => VEGMN0_USGS 
         LAI0   => LAI0_USGS 
         LAIMN0 => LAIMN0_USGS 
         SNUP0  => SNUP0_USGS 
         ALBF   => ALBF_USGS
         SNOALBF=> SNOALB_USGS
         LIMIT1 = 2
         LIMIT1 = 6
      ELSE IF (LAND_USE_TYPE == 'USGS28') THEN
         KWAT = 16
         RSMIN  => RSMIN_USGS28
         Z00    => Z00_USGS28  
         VEG0   => VEG0_USGS28 
         VEGMN0 => VEGMN0_USGS28 
         LAI0   => LAI0_USGS28 
         LAIMN0 => LAIMN0_USGS28 
         SNUP0  => SNUP0_USGS28 
         ALBF   => ALBF_USGS28
         SNOALBF=> SNOALB_USGS28
         LIMIT1 = 2
         LIMIT1 = 6
      ELSE IF (LAND_USE_TYPE == 'NLCD50') THEN
         KWAT = 1
         RSMIN  => RSMIN_NLCD50
         Z00    => Z00_NLCD50  
         VEG0   => VEG0_NLCD50 
         VEGMN0 => VEGMN0_NLCD50 
         LAI0   => LAI0_NLCD50 
         LAIMN0 => LAIMN0_NLCD50 
         SNUP0  => SNUP0_NLCD50 
         ALBF   => ALBF_NLCD50
         SNOALBF=> SNOALB_NLCD50
         LIMIT1 = 20
         LIMIT1 = 43
      ELSE IF (LAND_USE_TYPE == 'NLCD40') THEN
         KWAT = 17
         RSMIN  => RSMIN_NLCD40
         Z00    => Z00_NLCD40  
         VEG0   => VEG0_NLCD40 
         VEGMN0 => VEGMN0_NLCD40 
         LAI0   => LAI0_NLCD40 
         LAIMN0 => LAIMN0_NLCD40 
         SNUP0  => SNUP0_NLCD40 
         ALBF   => ALBF_NLCD40
         SNOALBF=> SNOALB_NLCD40
         LIMIT1 = 20
         LIMIT1 = 43
      ELSE IF (LAND_USE_TYPE == 'MODIS') THEN
         KWAT = 17
         RSMIN  => RSMIN_MODIS
         Z00    => Z00_MODIS  
         VEG0   => VEG0_MODIS 
         VEGMN0 => VEGMN0_MODIS 
         LAI0   => LAI0_MODIS 
         LAIMN0 => LAIMN0_MODIS 
         SNUP0  => SNUP0_MODIS 
         ALBF   => ALBF_MODIS
         SNOALBF=> SNOALB_MODIS
         LIMIT1 = 12
         LIMIT1 = 14
      END IF

      KWAT_OUT = KWAT
      !--------------------------------------------------------------------
      DO J = jts,jte
        DO I = its,ite
          XLAI(I,J)   = 0.0                                                        
          XLAIMN(I,J) = 0.0                                                     
          RSTMIN(I,J) = 9999.0                                                    
          XVEG(I,J)   = 0.0                                                       
          XVEGMN(I,J) = 0.0
          XSNUP(I,J)  = 0.0                                                            
          XALB(I,J)   = 0.0                                                            
          XSNOALB(I,J)= 0.0                                                            

          ! Code that may be needed in case these arrays are not intialized
          ! with zero by real.exe when not defined by GEOGRID or present 
          ! in met_em* files
          !IMPERV(I,J) = AMAX1(0.0001,IMPERV(I,J))                                                           
          !CANFRA(I,J) = AMAX1(0.0001,CANFRA(I,J))                                                           

        ENDDO   ! END LOOP THROUGH GRID CELLS
      ENDDO     ! END LOOP THROUGH GRID CELLS
      !--------------------------------------------------------------------

      DO J = jts,jte
        DO I = its,ite
          !-- Initialize 2 and 3-D veg parameters to be caculated
          DO K=1,NLCAT
            LAIMX(K) = LAI0(K)                                                       
            LAIMN(K) = LAIMN0(K)                                                   
            Z0(K)    = Z00(K)                                                   
            VEG(K)   = VEG0(K)                                              
            VEGMN(K) = VEGMN0(K)
            SNUP(K)  = SNUP0(K)                                            
            ALB(K)   = ALBF(K)                                            
            SNOALB(K)= SNOALBF(K)                                            
          ENDDO                                              

          !--  INITIALIZE SUMS
          SUMLAI    = 0.0
          SUMLMN    = 0.0
          SUMRSI    = 0.0
          SUMLZ0    = 0.0
          SUMVEG    = 0.0
          SUMVMN    = 0.0
          ALAI      = 0.0
          SUMSNUP   = 0.0
          SUMALB    = 0.0
          SUMSNOALB = 0.0

          !--   ESTIMATE CROP EMERGANCE DATE FROM VEGFRAC
          VFMX   = SHDMAX(I,J)
          VFMN   = SHDMIN(I,J)
          VEGF   = VEGFRA(I,J) 
                
          !-- Computations for VEGETATION CELLS ONLY
          IF(VFMX.GT.0.0.AND.LANDUSEF(I,KWAT,J).LT.1.00) THEN
            VSEAS = VEGF/VFMX
            IF(VSEAS.GT.1.0.OR.VSEAS.LT.0.0) THEN
              VSEAS = MIN(VSEAS,1.0)
              VSEAS = MAX(VSEAS,0.0)
            ENDIF

            ZNOTC = ZNOTCMN * (1-VSEAS) + ZNOTCMX * VSEAS  ! Zo FOR CROPS
            DO K = 1, NLCAT
              IF (LAND_USE_TYPE == 'MODIS') THEN
                !-- USE THE VEGFRAC DATA ONLY FOR CROPS
                IF (K.EQ.12 .OR. K.EQ.14) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   !-- SEASONALLY VARY Zo FOR MODIS DryCrop (k=12)
                   IF (K .EQ. 12) THEN
                      Z0(K) = ZNOTC
                   !-- CrGrM (k=14) USE AVG WITH GRASS AND FOREST
                   ELSE IF (K .EQ.14) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                   ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'NLCD50') THEN
                !-- USE THE VEGFRAC DATA ONLY FOR CROPS
                IF (K.EQ.20 .OR. K.EQ.43 .OR. K.EQ.45) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   !-- SEASONALLY VARY Zo FOR DryCrop (k=20) OR Irigated Crop (k=43) OR  Mix Crop (k=4)
                   IF (K.EQ.20 .OR. K.EQ.43) THEN
                      Z0(K) = ZNOTC
                   !-- CrNatM (k=45) USE AVG WITH GRASS AND FOREST
                  ELSE IF (K.EQ.45) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                  ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'NLCD40') THEN
                !-- USE THE VEGFRAC DATA ONLY FOR CROPS
                IF (K.EQ.12 .OR. K.EQ.14 .OR. K.EQ.38) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   !-- SEASONALLY VARY Zo FOR Crop (k=12 for MODIS or 38 for NLCD)
                   IF (K.EQ.12 .OR. K.EQ.38) THEN
                      Z0(K) = ZNOTC
                   !-- CrNatM (k=14) USE AVG WITH GRASS AND FOREST
                   ELSE IF (K.EQ.14) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                  ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'USGS') THEN
                !-- USE THE VEGFRAC DATA ONLY FOR CROPS 
                IF (K .GE. 2 .AND. K .LE. 6) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   !-- SEASONALLY VARY Zo FOR DryCrop (k=2) OR Irigated Crop (k=3) OR  Mix Crop (k=4)
                   IF (K .GE. 2 .AND. K .LE. 4) THEN
                      Z0(K) = ZNOTC
                   !-- CrGrM (k=5) or CrWdM (k=6) USE AVG WITH GRASS AND FOREST
                   ELSE IF (K .GE.5 .AND. K .LE. 6) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                   ENDIF
                ENDIF
              ELSE IF (LAND_USE_TYPE == 'USGS28') THEN
                !-- USE THE VEGFRAC DATA ONLY FOR CROPS 
                IF (K .GE. 2 .AND. K .LE. 6) THEN
                   LAIMX(K) = LAIMN0(K) * (1-VSEAS) + LAI0(K) * VSEAS
                   LAIMN(K) = LAIMX(K)
                   VEG(K)   = VEGMN0(K) * (1-VSEAS) + VEG0(K) * VSEAS
                   VEGMN(K) = VEG(K)
                   !-- SEASONALLY VARY Zo FOR DryCrop (k=2) OR Irigated Crop (k=3) OR  Mix Crop (k=4)
                   IF (K .GE. 2 .AND. K .LE. 4) THEN
                      Z0(K) = ZNOTC
                   !-- CrGrM (k=5) or CrWdM (k=6) USE AVG WITH GRASS AND FOREST
                   ELSE IF (K .GE.5 .AND. K .LE. 6) THEN
                      Z0(K)  = 0.5 * (ZNOTC + Z00(K))
                   ENDIF
                ENDIF

              END IF

            ENDDO

          ENDIF       !-- IF cell is vegetation

          !-------------------------------------
          !-- LOOP THROUGH LANDUSE Fraction and compute totals
          DO K = 1, NLCAT 
            FAREA    = LANDUSEF(I,K,J)
            SUMLAI   = SUMLAI + LAIMX(K) * FAREA
            SUMLMN   = SUMLMN + LAIMN(K) * FAREA
            ALAI     = ALAI + FAREA
            SUMRSI   = SUMRSI + FAREA * LAIMX(K) / RSMIN(K)
            SUMLZ0   = SUMLZ0 + FAREA * ALOG(Z0(K))
            SUMVEG   = SUMVEG + FAREA * VEG(K)
            SUMVMN   = SUMVMN + FAREA * VEGMN(K)
            SUMSNUP  = SUMSNUP+ FAREA * SNUP(K)
            SUMALB   = SUMALB + FAREA * ALB(K)
            SUMSNOALB= SUMSNOALB + FAREA * SNOALB(K)
          ENDDO

          FWAT = LANDUSEF(I,KWAT,J)
          !-- CHECK FOR WATER
          IF (FWAT .GE. 0.50) THEN        ! Changed WRFV3.7
!          IF (FWAT .GE. 0.9999) THEN
            XLAI(I,J)   = LAIMX(KWAT)
            XLAIMN(I,J) = LAIMN(KWAT)
            RSTMIN(I,J) = RSMIN(KWAT)
            ZNT(I,J)    = Z0(KWAT)
            XVEG(I,J)   = VEG(KWAT)
            XVEGMN(I,J) = VEGMN(KWAT)
            XSNUP(I,J)  = SNUP(KWAT)
            XALB(I,J)   = ALB(KWAT)
            XSNOALB(I,J)= SNOALB(KWAT)
          ELSE
            IF (FWAT .GT. 0.10) THEN
              ALAI   = ALAI - FWAT
              SUMLZ0 = SUMLZ0 - FWAT * ALOG(Z0(KWAT))
            ENDIF
            XLAI(I,J)   = SUMLAI / ALAI
            XLAIMN(I,J) = SUMLMN / ALAI
            RSTMIN(I,J) = SUMLAI / SUMRSI
            ZNT(I,J)    = EXP(SUMLZ0/ALAI)
            XVEG(I,J)   = SUMVEG / ALAI
            XVEGMN(I,J) = SUMVMN / ALAI
            XSNUP(I,J)  = SUMSNUP
            XALB(I,J)   = SUMALB
            XSNOALB(I,J)= SUMSNOALB
          ENDIF

          !!!!!!!!!!!!!!!!!!!Qestion Limei Ran, deleted in wrf37
          IF (FWAT .GT. 0.50) THEN
            ZNT(I,J)    = Z0(KWAT)
            XALB(I,J)   = ALB(KWAT)
            XSNOALB(I,J)= SNOALB(KWAT)
          ENDIF

          !-- Compute wetlands fraction for proper MMLUIN data set
          !-- Note: if LU categories change, these hard coded indicies must be updated
          IF (LAND_USE_TYPE == 'USGS') THEN
            WETFRA(I,J)  = LANDUSEF(I,17,J)+LANDUSEF(I,18,J)
          ELSE IF (LAND_USE_TYPE == 'USGS28') THEN
            WETFRA(I,J)  = LANDUSEF(I,17,J)+LANDUSEF(I,18,J)
          ELSE IF (LAND_USE_TYPE == 'NLCD50') THEN
            WETFRA(I,J)  = LANDUSEF(I,22,J)+LANDUSEF(I,23,J)+LANDUSEF(I,27,J)+LANDUSEF(I,28,J)+LANDUSEF(I,42,J)
          ELSE IF (LAND_USE_TYPE == 'NLCD40') THEN
            WETFRA(I,J)  = LANDUSEF(I,39,J)+LANDUSEF(I,40,J)+LANDUSEF(I,11,J)
          ELSE IF (LAND_USE_TYPE == 'MODIS') THEN
            WETFRA(I,J)  = LANDUSEF(I,11,J)
          END IF

          ZNT(I,J)    = ZNT(I,J) * 0.01           !CONVERT TO M
          XVEG(I,J)   = XVEG(I,J) * 0.01          !CONVERT TO FRAC
          XVEGMN(I,J) = XVEGMN(I,J) * 0.01
          XLAND(I,J)  = 1.0 + FWAT
          XALB(I,J)   = XALB(I,J) * 0.01
          XSNOALB(I,J)= XSNOALB(I,J) * 0.01
        
          !-------Adjustment according to CANFRA and IMPERV fo NLCD40 only -----------
          FIMP = IMPERV(I,J) * 0.01
          FCAN = CANFRA(I,J) * 0.01
          IF (LAND_USE_TYPE == 'NLCD40') THEN
             XVEG(I,J) = MIN(XVEG(I,J),1.0-FIMP)
             XVEGMN(I,J) = MIN(XVEGMN(I,J),1.0-FIMP)
             XVEG(I,J) = MAX(XVEG(I,J),FCAN)
             XVEGMN(I,J) = MAX(XVEGMN(I,J),FCAN)
           
             FORFRA = LANDUSEF(I,39,J)+LANDUSEF(I,30,J)+LANDUSEF(I,29,J)+LANDUSEF(I,28,J)
             EXTFOR =  FCAN - FORFRA
             IF (EXTFOR.GE.0.01) THEN
                XLAI(I,J) = LAIMX(30) * EXTFOR + XLAI(I,J) * (1-EXTFOR)
                XLAIMN(I,J) = LAIMN(30) * EXTFOR + XLAIMN(I,J) * (1-EXTFOR)
             ENDIF
          ENDIF
          !--------------------------------------------------------------------------

        ENDDO     ! END LOOP THROUGH GRID CELLS
      ENDDO       ! END LOOP THROUGH GRID CELLS
      !--------------------------------------------------------------------

  END SUBROUTINE vegeland

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      SUBROUTINE SURFPX(DTPBL, IFLAND, ISNOW, NUDGEX, XICE1, SOLDN, GSW,     & !in
                        LWDN, EMISSI, Z1, MOL, ZNT, UST, PSURF, DENS1,       & !in
                        QV1, QSS, TA1, THETA1, PRECIP, CPAIR, PSIH,          & !in
                        RH2OBS, T2OBS, VEGFRC, ISTI,LAI,IMPERV,CANFRA,BETAP, & !in
                        RSTMIN, HC_SNOW, SNOW_FRA, WETFRA, WWLT, WFC,        & !in
                        WRES, CGSAT, WSAT, B, C1SAT, C2R, AS, JP, C3, DS1,   & !in 
                        DS2, QST12,                                          & !in
                        RADNET, GRDFLX, HFX, QFX, LH, EG, ER, ETR,           & !out
                        QST, CAPG, RS, RA, TG, T2, WG, W2, WR,               & !out
                        TA2, QA2, LAND_USE_TYPE, I, J )                        !out

!------------------------------------------------------------------------------
! 
!  FUNCTION:
!    THIS SUBROUTINE COMPUTES SOIL MOISTURE AND TEMPERATURE TENDENCIES
!    BY SOLVING THE PROGNOSTIC EQUATIONS IN PX95.
!
!  SUBROUTINES CALLED:
!    SUB. QFLUX   compute the soil and canopy evaporation, and transpiration
!    SUB. SMASS   compute nudging coefficients for soil moisture and temp nudging
!
!  ARGUMENTS:
!    DTPBL:   TIME STEP OF THE MINOR LOOP FOR THE LAND-SURFACE/PBL MODEL
!    IFLAND:  INDEX WHICH INDICATES THE TYPE OF SURFACE,=1,LAND;=2,SEA
!    ISNOW:   SNOW (=1) OR NOT (=0)
!    NUDGE:   SWITCH FOR SOIL MOISTURE NUDGING
!    SOLDN:   SHORT-WAVE RADIATION
!    LWDN:    LONG-WAVE RADIATION
!    EMISSI:  EMISSIVITY
!    Z1:      HEIGHT OF THE LOWEST HALF LAYER
!    MOL:     MONIN-OBUKOV LENGH (M)
!    ZNT:     ROUGHNESS LENGTH (M)
!    UST:     FRICTION VELOCITY (M/S)
!    TST:     Turbulent moisture scale
!    RA:      AERODYNAMIC RESISTENCE
!    PSURF:   P AT SURFACE (CB)
!    DENS1:   AIR DENSITY AT THE FIRST HALF LAYER
!    QV1:     Air humidity at first half layer
!    QSS:     Saturation mixing ratio at ground
!    TA1:     Air temperature at first half layer
!    THETA1:  Potential temperature at first half layer
!    PRECIP:  Precipitation rate in m/s
!    STBOLT:  STEFAN BOLTZMANN'S CONSTANT
!    KARMAN:  VON KARMAN CONSTANT
!    CPAIR:   Specific heat of moist air (M^2 S^-2 K^-1)
!    TAUINV:  1/1DAY(SEC)
!    VEGFRC:  Vegetation coverage
!    ISTI:    soil type
!    LAI:     Leaf area index
!    IMPERV:  Percentage of IMPERVIOUS Fraction 
!    CANFRA:  Percentage of Canopy/Tree Fraction 
!    BETAP:   Coefficient for bare soil evaporation
!    WETFRA:  Fraction of Wetlands area 
!    THZ1OB:  Observed TEMP FROM ANAL OF OBS TEMPS AT SCREEN HT
!    RHOBS:   Observed relative humidity at SCREEN HT
!    RSTMIN   Minimum Stomatol resistence
!... Outputs from SURFPX
!    RADNET: Net radiation
!    HFX:    SENSIBLE HEAT FLUX (W / M^2)
!    QFX:    TOTAL EVAP FLUX (KG / M^2 S)
!    GRDFLX: Ground heat flux (W / M^2)
!    QST:    Turbulent moisture scale
!    CAPG:   THERMAL CAPACITY OF GROUND SLAB (J/M^2/K)
!    RS:     Surface resistence
!    RA:     Surface aerodynamic resistence
!    EG:     evaporation from ground (bare soil)
!    ER:     evaporation from canopy
!    ETR:    transpiration from vegetation
!    TA2:    diagnostic 2-m temperature from surface layer and lsm
!
!... Updated variables in this subroutine
!    TG:     Soil temperature at first soil layer
!    T2:     Soil temperature in root zone
!    WG:     Soil moisture at first soil layer
!    W2:     Soil moisture at root zone
!    WR:     Canopy water content in m
!
!  REVISION HISTORY:
!     AX     2/2005 - developed WRF version based on the MM5 PX LSM
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
      IMPLICIT NONE

!.......Arguments

!.. Integer
      INTEGER , INTENT(IN)  :: ISTI, NUDGEX, I, J

!... Real
      REAL ,    INTENT(IN)  :: DTPBL, DS1, DS2
      REAL ,    INTENT(IN)  :: IFLAND, ISNOW, XICE1
      REAL ,    INTENT(IN)  :: SOLDN, GSW, LWDN, EMISSI, Z1
      REAL ,    INTENT(IN)  :: ZNT
      REAL ,    INTENT(IN)  :: PSURF, DENS1, QV1, QSS, TA1,  THETA1, PRECIP
      REAL ,    INTENT(IN)  :: CPAIR
      REAL ,    INTENT(IN)  :: VEGFRC, LAI, IMPERV, CANFRA
      REAL ,    INTENT(IN)  :: RSTMIN, HC_SNOW, SNOW_FRA, WETFRA 
      REAL ,    INTENT(IN)  :: WWLT, WFC, WRES, CGSAT, WSAT, B, C1SAT, C2R, AS, JP, C3
      REAL ,    INTENT(IN)  :: RH2OBS,T2OBS
      REAL ,    INTENT(IN)  :: QST12

      REAL ,    INTENT(OUT) :: RADNET, EG, ER, ETR
      REAL ,    INTENT(OUT) :: QST, CAPG, RS, TA2, QA2

      REAL ,    INTENT(INOUT) :: TG, T2, WG, W2, WR, UST, RA, BETAP 
      REAL ,    INTENT(INOUT) :: GRDFLX, QFX, HFX, LH, PSIH, MOL

      CHARACTER (LEN = 6), INTENT(IN) :: LAND_USE_TYPE      ! Check Limei Ran, wrf38 set to 5

!... Local Variables

!... Real
      REAL        :: HF, LV, CQ4, WETSAT, SM2 
      REAL        :: RAH, RAW, ET, W2CG, CG, CT, SOILFLX, CPOT, THETAG
      REAL        :: ZOL, ZOBOL, ZNTOL, Y, Y0, PSIH15, YNT
      REAL        :: WGNUDG, W2NUDG, ALPH1, ALPH2, BET1, BET2, T1P5
      REAL        :: CQ1, CQ2, CQ3, COEFFNP1, COEFFN, TSNEW, TSHLF, T2NEW
      REAL        :: ROFF, WRMAX, PC, DWR, PNET, TENDWR, WRNEW
      REAL        :: COF1, CFNP1WR, CFNWR, PG, FASS
      REAL        :: TENDW2, W2NEW, W2HLF, W2REL, C1, C2, WEQ, CFNP1, CFN, WGNEW
      REAL        :: ALN10, TMP1, TMP2, TMP3, AA, AB, TST, RBH, CTVEG
      REAL        :: QST1,PHIH,PSIOB
      REAL        :: T2NUD, T2NUDF
      REAL        :: VAPPRS, QSBT, RH2MOD, IMF, VEGF, SOILF
      REAL        :: RSOIL, LDRY, DP       ! Soil model updates - JEP 12/14
      REAL        :: C1MAX,ZZA,ZZB,ZDEL,ZLY,ZA,ZB,ZY2

!... Parameters
      REAL        :: ZOBS, GAMAH, BETAH, SIGF, BH, CT_SNOW, CT_IMPERV

!      REAL, PARAMETER :: CV = 2.0E-5   !1.2E-5   ! K-M2/J Note: Update from 8E-6 10/14 Jon Pleim
      REAL, PARAMETER :: CV = 1.2E-5   ! K-M2/J Note: Update from 8E-6 10/14 Jon Pleim

      PARAMETER (ZOBS  = 1.5)    ! height for observed screen temp., (m)
      PARAMETER (BH    = 15.7)
      PARAMETER (GAMAH = 16. )   !11.6)
      PARAMETER (BETAH = 5.0 )   !8.21)
      PARAMETER (SIGF  = 0.5)    ! rain interception see LSM (can be 0-1)
      REAL, PARAMETER  :: DWAT   = 0.2178  ! [cm^2 / s] at 273.15K
      !--------------------------------------------------------------------
      !  OLD PX legacy value from MM5 ... unknown origin PARAMETER (CT_SNOW  = 5.54E-5)   
      ! New value of CT_SNOW calibrated using multilayer soil model where csnow=6.9E5 J/(m3 K) 
      ! from NCAR (WRFV3.2 -WRFV3.6.1) CSM PARAMETER (CT_SNOW  = 2.0E-5) 
      PARAMETER (CT_SNOW  = 2.0E-5)  

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! CS for concrete/asphalt/road material (http://www.engineeringtoolbox.com/specific-heat-capacity-d_391.html)
      !                                        cs_imperv = 920 J kg-1 K-1 
      ! CS for asphalt and concrete from 0.1785 to 0.20 cal g-1 K-1 (http://pages.towson.edu/morgan/files/Impervious.pdf)
      ! the values above translate to ~750 to 850 J kg-1 K-1
      !
      ! CAPG used for WRF urban physics ranges from 1.0E6 for roof and building walls to 1.4E6 J m-3 K-1 for roads/urban ground
      ! Using these values to back out CS along with 0.15 m thickness 1.4E6 J m-3 K-1 * 0.15 m = 2.1E5 J m-2 K-1
      ! inverse of the value above gives CT_IMPERV value of 1/0.000021 = 4.762E-6 K m2 J-1

      ! The middle value will be used for now. 850 J kg-1 K-1. This needs to be converted from J/K per kg to area using 
      ! the approxiate concrete/asphalt density and layer thickness or represenative thickness. For starters (12/2011)
      ! well not use the PX first layer thickness, but representative thickness of most roads/parkinglots/buildings.
      ! for now welll use 6 inches or about 15 cm or 0.15 m. Density of concrete (normal) from 
      ! Dorf, Richard. Engineering Handbook. New York: CRC Press, 1996. is ~2400 kg m-3.
      ! Using these values 850 J kg-1 K-1 * 2400 kg m-3 * 0.15 m = 3.06E5 J m-2 K-1 or in CT form (inverse) 3.268E-6 K m2 J-1
      ! If you look at the range of possible values considering density differences of concrete/asphalt/etc
      ! Values can range from 2.5 to 6.0 E-6                                 
      PARAMETER (CT_IMPERV  = 3.268E-6)  
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ALN10  = ALOG(10.0)
        RADNET = SOLDN - (EMISSI *(STBOLT *TG **4 - LWDN))        ! NET RADIATION                             
        !--------------------------------------------------------------------
        CPOT= (100.0 / PSURF) ** ROVCP       ! rcp is global constant(module_model_constants)                            
        THETAG = TG * CPOT

        ZOL   = Z1/MOL                                                       
        ZOBOL = ZOBS/MOL                                                      
        ZNTOL = ZNT/MOL                              

        !-----------------------------------------------------------------------------------------
        IF (MOL .LT. 0.0) THEN
          Y      = ( 1.0 - GAMAH * ZOL ) ** 0.5
          Y0     = ( 1.0 - GAMAH * ZOBOL ) ** 0.5
          YNT    = ( 1.0 - GAMAH * ZNTOL ) ** 0.5
          PSIH15 =  2.0 * ALOG((Y + 1.0) / (Y0 + 1.0))
          PSIH   =  2.0 * ALOG((Y + 1.0) / (YNT + 1.0))
          PSIOB  =  2.0 * ALOG((Y0 + 1.0) / (YNT + 1.0))
          PHIH = 1.0 / Y
        ELSE
          IF((ZOL-ZNTOL).LE.1.0) THEN                                         
            PSIH = -BETAH*(ZOL-ZNTOL)                                        
          ELSE                                                               
            PSIH = 1.-BETAH-(ZOL-ZNTOL)                                      
          ENDIF             
          IF ((ZOBOL - ZNTOL) .LE. 1.0) THEN
            PSIOB = -BETAH * (ZOBOL - ZNTOL)
          ELSE
            PSIOB = 1.0 - BETAH - (ZOBOL - ZNTOL)
          ENDIF
          PSIH15 =  PSIH - PSIOB
          IF(ZOL.LE.1.0) THEN
            PHIH = 1.0 + BETAH * ZOL
          ELSE
            PHIH = BETAH + ZOL
          ENDIF
        ENDIF
        !-----------------------------------------------------------------------------------------
        !-- ADD RA AND RB FOR HEAT AND MOISTURE
        !... RB FOR HEAT = 5 /UST
        !... RB FOR WATER VAPOR =  5*(0.599/0.709)^2/3 /UST = 4.503/UST
        RA=PR0* ( ALOG(Z1/ZNT) - PSIH )/(KARMAN*UST)
        RAH = RA + 5.0 / UST
        RAW = RA + 4.503 / UST
        IF (IFLAND .LT. 1.5.AND. XICE1.LT.0.5) THEN
          LDRY = 1.75*DS1*(EXP((1.-WG/WSAT)**5)-1.)/1.718       ! 1.75 cm is the layer thickness used by S&Z09
          DP  = DWAT*1.E-4 * WSAT**2 * (1.-WRES/WSAT)**(2.+3./B)
          !DP  = DWAT*1.E-4 * 0.1 * ( 2.0*(1.-WRES/WSAT)**3 + 0.04*(1.-WRES/WSAT) )   !Deepagoda et al. 2010
          RSOIL=LDRY/DP

          !Merlin et al. 2016 ECMWF H-TESSEL
          !IF ( WG > WRES ) THEN
          !   RSOIL = 50.0 * (WFC - WRES ) / (WG - WRES) 
          !ELSE
          !   RSOIL = 8000.0
          !ENDIF

        ELSE
          RSOIL = 0.0
        ENDIF
        !--------------------------------------------------------------------
        ! Compute soil moisture layer 2 that considers fraction of saturated 
        ! wetlands. If 100% of cell is wetland, soil moisture can be no lower
        ! than full soil saturation. If half wetland, no less than half saturated 
        IF (IFLAND .LT. 1.5 ) THEN
          WETSAT = 1.00 * WSAT                           ! Wetlands soil moisture
          SM2    = (WETFRA * WETSAT)                     ! 
          W2     = AMAX1(SM2, W2)                        ! In case that W2 > Field capacity (heavy precip), use wetter W2
        ENDIF

        !--------------------------------------------------------------------
        !--  COMPUTE MOISTURE FLUX
        CALL QFLUX( DENS1,  QV1,    TA1,  SOLDN,  RAW, QSS,            &
                    VEGFRC, ISNOW,  ISTI, IFLAND, LAI, BETAP,          &
                    WG,     W2,     WR,                                &
                    RSTMIN, WWLT, WFC, RSOIL,                          &     !  Soil model updates - JEP 12/14
                    EG,     ER,     ETR,  CQ4,    RS,  FASS)
        !--------------------------------------------------------------------

        !--------------------------------------------------------------------
        !    Compute Total evaporation (ET) from various modes of moisture flux
        ET  = EG + ER + ETR
        QST = -ET / (DENS1 * UST)
 
        LV  = 2.83E6                         !-- LATENT HEAT OF SUBLIMATION AT 0C FROM STULL(1988)
        IF (ISNOW .LT. 0.5.AND.TG.GT.273.15)                            &                                                                               
                    LV = (2.501 - 0.00237 * (TG - 273.15)) * 1.E6  !-- FROM STULL(1988) in J/KG
        
        ! IF (IFLAND .LT. 1.5 )  QFX = ET     !-- Recaculate QFX over land to account for P-X LSM EG, ER and ETR
        QFX = ET
        LH  = LV * QFX
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Surface sensible heat flux
        TST = (THETA1 - THETAG ) / (UST*RAH)
        HF  = UST * TST           
        HFX = AMAX1(-DENS1 * CPAIR * HF, -250.0)  ! using -250. from MM5                                   
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        ! Compute the diagnosed 2m Q and T consistent with PX LSM
        QST1 = 0.5*(QST+QST12/PHIH)
        TA2  = (THETAG + TST * (PR0 / KARMAN * (ALOG(ZOBS / ZNT) - PSIOB)+5.))/CPOT
        QA2  = QV1 - QST1 * PR0/ KARMAN *  (ALOG(Z1 / ZOBS) - PSIH15)

        IF (QA2.LE.0.0) QA2 = QV1

        !--  Relative humidity
        VAPPRS = SVP1 * EXP(SVP2 * (TA2 - SVPT0) / (TA2 - SVP3))
        QSBT   = EP_2 * VAPPRS / (PSURF - VAPPRS)
        RH2MOD = QA2 / QSBT
        !-----------------------------------------------------------------------------------------
        IF (IFLAND .LT. 1.5 ) THEN
          W2CG = AMAX1(W2,WWLT)
          CG   = CGSAT * 1.0E-6 * (WSAT/ W2CG) **    &  
                 (0.5 * B / ALN10)
          ! IMPERVIOUS weighting scheme -- Subtract highly accurate impervious fraction from cell
          ! remainder is split between ground and vegetation. CT is a weighted fractional average.
          ! Snow CT is then applied for final heat capacity
          IMF  = AMAX1(0.0,IMPERV/100.0)
          VEGF = (1.0 - IMF) * VEGFRC
          SOILF= (1.0 - IMF) * (1.0 - VEGFRC)
          CT   = 1./( IMF/CT_IMPERV + VEGF/CV + SOILF/CG)                        
          CT   = 1./(SNOW_FRA/CT_SNOW + (1-SNOW_FRA)/CT)
          CAPG = 1.0/CT          

          SOILFLX = 2.0 * PI * TAUINV * (TG - T2)
          GRDFLX  = SOILFLX / CT
        ENDIF
        !-----------------------------------------------------------------------------------------

        !--------------------------------------------------------------------
        !-- ASSIMILATION --- COMPUTE SOIL MOISTURE NUDGING FROM TA2 and RH2               
        !-------COMPUTE ASSIMILATION COEFFICIENTS FOR ALL I                            
        IF (IFLAND .LT. 1.5) THEN
          IF (NUDGEX .EQ. 0) THEN                                          !-- NO NUDGING CASE                
            WGNUDG = 0.0                                                        
            W2NUDG = 0.0    
            T2NUD  = 0.0                                    
          ELSE                                                               !-- NUDGING CASE        
            
            CALL SMASS (ISTI,  FASS,  SOLDN,   VEGFRC, RA, WWLT, WFC,   &                       
                        ALPH1, ALPH2, BET1, BET2, T2NUDF)                             

            !--COMPUTE SOIL MOISTURE NUDGING
            WGNUDG = ALPH1 * (T2OBS - TA2) + ALPH2 * (RH2OBS - RH2MOD) * 100  !NUDGING W2 FOR NON-VEG
            W2NUDG = BET1  * (T2OBS - TA2) + BET2  * (RH2OBS - RH2MOD) * 100
            IF (W2 .GE. WFC)  W2NUDG = AMIN1(W2NUDG,0.0)
            IF (W2 .LE. WWLT) W2NUDG = AMAX1(W2NUDG,0.0)
            IF (W2 .GE. WFC)  WGNUDG = AMIN1(WGNUDG,0.0)
            IF (W2 .LE. WWLT) WGNUDG = AMAX1(WGNUDG,0.0)
            T2NUD = T2NUDF * (T2OBS - TA2)
          ENDIF
        ENDIF
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
        !-- Compute new values for TS,T2,WG,W2 and WR. No change over ice or water (XLAND > 1)
        IF (IFLAND .LT. 1.5) THEN
          !-- SOLVE BY CRANK-NIC --   TENDTS=CT*(RADNET-HFX-QFX)-SOILFLX 
          !-- Calculate the coefficients for implicit calculation of TG
          CQ1      = (1.0 - 0.622 * LV * CRANKP / (r_d * TG)) * QSS
          CQ2      = 0.622 * LV * QSS * CRANKP / (r_d * TG * TG)
          CQ3      = DENS1 * (1.0 - VEGFRC) / (RAW + RSOIL)
!          CQ3      = DENS1 * BETAP * (1.0 - VEGFRC) / RAW
          COEFFNP1 = 1.0 + DTPBL * CRANKP * (4.0 * EMISSI *  STBOLT * TG ** 3    &
                     * CT + DENS1 * CPAIR / RAH * CPOT * CT + 2.0 * PI           &
                     * TAUINV ) + DTPBL * (CT * LV * CQ2 * (CQ3 + CQ4))
          COEFFN   = CT * (GSW + EMISSI * (STBOLT * (4.0 * CRANKP - 1.0)         &
                     * TG*TG*TG*TG + LWDN)                                       & !NET RAD
                     + DENS1 * CPAIR / RAH * (THETA1 - (1.0 - CRANKP) * THETAG)  &
                     - LV * (CQ3 * (CQ1 - QV1) + CQ4 * (CQ1 - QV1)))             & !SFC HEAT FLUX 
                     - 2.0 * PI * TAUINV * ((1.0 - CRANKP) * TG - T2)              !SOIL FLUX
          TSNEW    = (TG + DTPBL * COEFFN) / COEFFNP1
          !-- FOR SNOW COVERED SURFACE TEMPERATURE IS NOT MORE THAN ZERO
          IF (XICE1 .GT. 0.5) TSNEW = AMIN1(TSNEW,273.15)                         ! Re-added Jan 2010 to keep ice surface at or below freezing (J. Pleim)
          TSHLF = 0.5 * ( TSNEW + TG)
          T2NEW = (T2 + DTPBL * TAUINV * T2TFAC * (TSHLF - (1 - CRANKP) * T2)     &
                + DTPBL*T2NUD)  &              ! Added deep temperature nudging 
                  / (1.0 + DTPBL * TAUINV * T2TFAC * CRANKP)
          !-- REPLACE OLD with NEW Value
          TG = TSNEW
          T2 = T2NEW             
        ENDIF
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
      ! Compute new subsurface soil and canopy moisture values DENS1. No change required over ocean.
        IF (IFLAND .LT. 1.5.AND. XICE1.LT.0.5) THEN  
          !-- Compute WR
          ROFF  = 0.0
          WRMAX = 0.2E-3 * VEGFRC * LAI                     ! max. WRMAX IN m
          
          IF(WRMAX.GT.0.0) THEN
            !--  PC is precip. intercepted by veg.(M/S)
            PC    = VEGFRC * SIGF * PRECIP
            DWR   = (WRMAX - WR) / DTPBL                       !the tendency to reach max.
            PNET  = PC - ER/ DENW                              ! residual of precip. and evap.
            IF (PNET .GT. DWR) THEN
              ROFF = PNET - DWR
              PC   = PC - ROFF
            ENDIF
            IF (QSS .LT. QV1) THEN
              TENDWR = PC - ER / DENW
              WRNEW  = WR + DTPBL * TENDWR
            ELSE
              COF1    = DENS1 / DENW * VEGFRC * (QSS - QV1) / RAW
              !-- using delta=wr/wrmax
              CFNP1WR = 1.0 + DTPBL * COF1 * CRANKP / WRMAX  
              CFNWR   = PC - COF1 * (1.0 - CRANKP) * WR / WRMAX
              WRNEW   = (WR + DTPBL * CFNWR) / CFNP1WR
            ENDIF
          ELSE
            PC=0.0
            WRNEW=0.0  
          ENDIF
          !---------------------------------------------
          !-- Compute W2
          PG     = DENW * (PRECIP - PC)               ! PG is precip. reaching soil (PC already including ROFF)
          TENDW2 = 1.0 / (DENW * DS2) * (PG - EG - ETR) &
                   - C3/DS2 * TAUINV * AMAX1(0.0,(W2 - WFC)) &
                   + (W2NUDG + WGNUDG) / DS2                    ! NUDGING
          W2NEW  = W2 + DTPBL * TENDW2
          W2NEW  = AMIN1(W2NEW,WSAT)
          W2NEW  = AMAX1(W2NEW,WRES) !0.05)  !Limei 08/02/2017
          W2HLF  = 0.5 * (W2 + W2NEW)
          !.. new values
          W2     = W2NEW
          WR     = AMIN1(WRMAX,WRNEW)
        ENDIF
        !-----------------------------------------------------------------------------------------

        !-----------------------------------------------------------------------------------------
      ! Compute new surface soil moisture values (WR).
        IF (IFLAND .LT. 1.5.AND. XICE1.LT.0.5) THEN  ! over ocean no change to wg w2,wr
        !--    FOR SNOW COVERED SURFACE, ASSUME SURFACE IS SATURATED AND
        !      WG AND W2 ARE NOT CHANGED
          IF (ISNOW .GT.0.5) THEN
            WG = WSAT
          ELSE
            W2REL = W2HLF / WSAT
            IF (WG .GT. WWLT) THEN
              C1 = DS1*C1SAT * (WSAT / WG) ** (0.5 * B + 1.0) 
            ELSE   ! revise C1 for wg < wilting point Noilhan & Mahfouf (1996)
              ZY2   = C1SAT * (WSAT / WWLT) ** (0.5 * B + 1.0)
              C1MAX = (1.19*WWLT - 5.09)*TG - 146.*WWLT + 1786.
              C1MAX = MAX(MAX(C1MAX,ZY2),10.)
!* Giard-Bazile formulation (resolution of a second order equation)
!
              ZLY =   LOG( C1MAX/10.)
              ZZA = - LOG( ZY2  /10.)
              ZZB = 2. * WWLT * ZLY
              ZDEL = 4. * (ZLY+ZZA) * ZLY * WWLT**2
              ZA  = (-ZZB+SQRT(ZDEL)) / (2.*ZZA)
              ZB  = ZA**2 / ZLY
              C1 = DS1*C1MAX * EXP(-(WG-ZA)**2/ZB)
            ENDIF

            C2 = C2R * W2HLF / (WSAT - W2HLF + 1.E-11) 
            IF (W2HLF .GE. WSAT) THEN
              WEQ = WSAT
            ELSE
              WEQ = W2HLF - AS * WSAT * W2REL ** JP *         &
                   (1.0 - W2REL ** (8.0 * JP))
            ENDIF

            !.... The diffusion method in Sakaguchi and Zeng [2009] (JGR-Atmos.)
            CFNP1 = 1.0 + DTPBL * C2 * TAUINV * CRANKP
            CFN   = C1 / (DENW * DS1) * (PG - EG) - C2 * TAUINV *               &
                    ((1.0 - CRANKP) * WG - WEQ) + WGNUDG/ DS1

            WGNEW = AMAX1((WG + DTPBL * CFN) / CFNP1, WRES ) !0.001)  ! Limei 08/02/2017
            !-- NEW VALUES
            WG    = AMIN1(WGNEW,WSAT)
            
          ENDIF                  !endif for ISNOW
        ENDIF                    !endif for XLAND
            
      END SUBROUTINE surfpx
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
      SUBROUTINE QFLUX (DENS1,  QV1,   TA1,  RG,     RAW, QSS,           & ! in
                        VEGFRC, ISNOW, ISTI, IFLAND, LAI, BETAP,         & ! in
                        WG,     W2,    WR,                               & ! in
                        RSTMIN, WWLT,  WFC,  RSOIL,                      & ! in  !Soil model updates - JEP 12/14
                        EG,     ER,    ETR,  CQ4,    RS,  FASS)            ! out

!-------------------------------------------------------------------------
!                                                              
!  FUNCTION:                                                          
!    THIS SUBROUTINE COMPUTES EVAPORATION FROM BARE SOIL (EG) AND FROM
!    THE WET PART OF CANOPY (ER) AND TRANSPIRATION FROM THE DRY PART OF
!    CANOPY (ETR).                       
!                                        
!  REVISION HISTORY: 
!    A. Xiu       Oct  2004  - adapted from the PX LSM in MM5 for the WRF system                            
!    R. Gilliam   Dec  2006  - Completed WRF V2.1.2 implementation                                 
!                                                               
!-------------------------------------------------------------------------
!  QFLUX ARGUMENT LIST:                                                        
!-------------------------------------------------------------------------
! INPUTS:
!-- DENS1        air density at first layer
!-- QV1          air humidity at first layer
!-- TA1          air temperature at first layer
!-- RG           shortwave radition reaching the ground
!-- RAW          RA+RB for moisture
!-- QSS          saturation mixing ratio at ground
!-- VEGFRC       vegetation coverage
!-- ISNOW        if snow on the ground 
!-- ISTI         soil type
!-- IFLAND       if land (=1) or water (=2)
!-- LAI          leaf area index
!-- BETAP        
!-- WG           soil moisture at first soil layer
!-- W2           soil moisture at root zone
!-- WR           Canopy water
!
!  OUTPUTS FROM QFLUX:
!-- EG           evaporation from ground (bare soil)
!-- ER           evaporation from canopy
!-- ETR          transpiration from vegetation
!-- CQ4
!-- RS           surface resistence
!-- FASS         parameter for soil moisture nudging
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

      IMPLICIT NONE

! DECLARATIONS - INTEGER
      INTEGER , INTENT(IN)  :: ISTI

! DECLARATIONS - REAL
      REAL ,    INTENT(IN)  :: ISNOW, IFLAND
      REAL ,    INTENT(IN)  :: DENS1, QV1, TA1, RG, RAW, QSS,            &
                               VEGFRC, LAI,                              &
                               WG, W2, WR, RSTMIN   
      REAL ,    INTENT(INOUT)  :: BETAP, RSOIL
      REAL,     INTENT(IN)   :: WWLT, WFC

      REAL ,    INTENT(OUT) :: EG, ER, ETR, CQ4, RS, FASS

!... Local Variables

!... Real
      REAL    :: WRMAX, DELTA, SIGG, RADL, RADF, W2AVAIL, W2MXAV
      REAL    :: FTOT, F1, F2, F3,  F4
      REAL    :: FSHELT, GS, GA, FX
      REAL    :: PAR, F1MAX


!... Parameters
      REAL, PARAMETER :: RSMAX = 5000.0                ! s/m
      REAL, PARAMETER :: FTMIN = 0.0000001             ! m/s
      REAL, PARAMETER :: F3MIN = 0.25

!
!... for water surface, no canopy evaporation and transpiration
      ER  = 0.0
      ETR = 0.0
      CQ4 = 0.0
      
!... GROUND EVAPORATION (DEPOSITION)
!      IF (QSS .LT. QV1) BETAP = 1.0  
      IF (QSS .LT. QV1) RSOIL = 0.0
!      EG = DENS1 * (1.0 - VEGFRC) * BETAP * (QSS - QV1) / RAW
      EG = DENS1 * (1.0 - VEGFRC) * (QSS - QV1) / (RAW + RSOIL)

      !!---------------------------------------------------------------------
!... CANOPY
      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        WRMAX = 0.2E-3 * VEGFRC * LAI   ! in unit m
        IF (WR .LE. 0.0) THEN
          DELTA = 0.0
        ELSE
!         DELTA = (WR / WRMAX) ** 0.66667
          DELTA = WR / WRMAX           ! referred to SiB model
        ENDIF
        
        IF (QSS .GE. QV1) THEN
          SIGG = DELTA
        ELSE
          SIGG = 1.0
        ENDIF

        ER = DENS1 * VEGFRC * SIGG * (QSS - QV1) / RAW  
      ENDIF
      !!---------------------------------------------------------------------

      !-- TRANSPIRATION
      !!---------------------------------------------------------------------
      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        
        !!!-RADIATION
        IF (RSTMIN .GT. 130.0) THEN
!          RADL = 30.0                                            ! W/M2
!          F1MAX = 1.-0.03*LAI
          F1MAX = 1.-0.02*LAI    !Echer2015  Trees
        ELSE
!          RADL = 100.0                                           ! W/M2
!          F1MAX = 1.-0.05*LAI
          F1MAX = 1.-0.07*LAI    !Echer2015 crops/grass
        ENDIF
!        RADF = 1.1 * RG / (RADL * LAI)                  ! NP89 - EQN34
!        F1   = (RSTMIN / RSMAX + RADF) / (1.0 + RADF)
!        PAR = 0.45 * RG
        PAR = 0.45 * RG * 4.566  ! converted from W/m2 to umoles/m2/s  (1/.219)  Echer2015
!        F1 = F1MAX*(2./(1.+EXP(-0.014*PAR))-1.)
        F1 = F1MAX*(1.0-exp(-0.0017*PAR))   !Echer2015
        F1 = AMAX1(F1,RSTMIN / RSMAX)

        !!!-SOIL MOISTURE
        W2AVAIL = W2 - WWLT
        W2MXAV  = WFC - WWLT
        F2      = 1.0 / (1.0 + EXP(-5.0 * ( W2AVAIL / W2MXAV -               &
                  (W2MXAV / 3.0 + WWLT))))    ! according JP, 9/94

        !-AIR TEMP
        !... according to Avissar (1985) and AX 7/95
        IF (TA1 .LE. 302.15) THEN
          F4 = 1.0 / (1.0 + EXP(-0.41 * (TA1 - 282.05)))
        ELSE
          F4 = 1.0 / (1.0 + EXP(0.5 * (TA1 - 314.0)))
        ENDIF

        FTOT = LAI * F1 * F2 * F4
      ENDIF

      !!---------------------------------------------------------------------
      IF (IFLAND .LT. 1.5 .AND. VEGFRC .GT. 0.0)  THEN
        FSHELT = 1.0   ! go back to NP89
        GS     = FTOT / (RSTMIN * FSHELT)
        GA     = 1.0 / RAW
        !-- Compute humidity effect according to RH at leaf surf
        F3 = 0.5 * (GS - GA + SQRT(GA * GA + GA * GS *                       &
             (4.0 * QV1 / QSS - 2.0) + GS * GS)) / GS
        F3 = AMIN1(AMAX1(F3,F3MIN),1.0)
        RS = 1.0 / (GS * F3)
        
        !--- Compute Assimilation factor for soil moisture nudging - jp 12/94
        !--  Note that the 30 coef is to result in order 1 factor for max
        IF (RG .LT. 0.00001) THEN   ! do not nudge during night
          FX = 0.0
        ELSE
          FX = 30.0 * F1 * F4 * LAI / (RSTMIN * FSHELT)
        ENDIF

        FASS = FX
        ETR = DENS1 * VEGFRC * (1.0 - SIGG) * (QSS - QV1) / (RAW + RS)
        !..... CQ4 is used for the implicit calculation of TG in SURFACE
        CQ4 = DENS1 * VEGFRC * ((1.0 - SIGG) / (RAW + RS) + SIGG / RAW)
      ENDIF         

    END SUBROUTINE qflux
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
      SUBROUTINE SMASS (ISTI,  FASS,  RG,   VEGFRC, RA,              & !in                      
                        WWLT, WFC,                                   & !in
                        ALPH1, ALPH2, BET1, BET2, T2NUDF )             !out  
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
      IMPLICIT NONE
!------------------------------------------------------------------------------------------
!     SMASS COMPUTES SOIL MOISTURE NUDGING COEFFICIENTS
!------------------------------------------------------------------------------------------
!
!.........Arguments
      INTEGER, PARAMETER             :: NSCAT    = 16     ! max. soil types
      
      INTEGER,                   INTENT(IN)   :: ISTI 
      REAL,                      INTENT(IN)   :: FASS, RG, VEGFRC, RA
      REAL,                      INTENT(IN)   :: WWLT, WFC
      REAL,                      INTENT(OUT)  :: ALPH1, ALPH2, BET1, BET2, T2NUDF


!........Local variables

!... Real
      REAL    :: FBET, FALPH, FRA, FTEXT
      
!... Parameters
      REAL, PARAMETER  :: A1MAX = -10.E-5, A2MAX = 1.E-5  ! m/K, m for 6hr period
      REAL, PARAMETER  :: B1MAX = -10.E-3, B2MAX = 1.E-3  ! m/K, m (Bouttier et al 1993)
      REAL, PARAMETER  :: TASSI = 4.6296E-5               ! 1/6hr in 1/sec
      REAL, PARAMETER  :: RAMIN = 10.0                    ! 0.1 s/cm
      REAL, PARAMETER  :: WFCX  = 0.243                   ! middle of WFC range 
      REAL, PARAMETER  :: WWLTX = 0.169                   ! middle of WWLT range 

!
      FBET  = FASS
      FALPH = RG / 1370.0
!--TEXTURE FACTOR NORMALIZED BY LOAM (IST=5)
      FRA   = RAMIN / RA            ! scale by aerodynamic resistance
      FTEXT = TASSI * (WWLT + WFC) / (WWLTX + WFCX) * FRA
!        write(6,*) ' ftot, fbet=',ftot, fbet,' ftext=',ftext/tassi
!
      ALPH1 = A1MAX * FALPH * (1.0 - VEGFRC) * FTEXT
      ALPH2 = A2MAX * FALPH * (1.0 - VEGFRC) * FTEXT
      BET1  = B1MAX * FBET  *        VEGFRC  * FTEXT
      BET2  = B2MAX * FBET  *        VEGFRC  * FTEXT
      !T2NUDF = 1.0E-5 * MAX((1.0 - 5.0 * FALPH),0.0)   ! T2 Nudging at night
      T2NUDF = 1.0E-5 * ( VEGFRC*MAX((1.0 - 5.0 * FALPH),0.0) + (1-VEGFRC) )  !  T2 Nudging at night and day for non-veg frac - jp 10/30/14

    END SUBROUTINE smass
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

      SUBROUTINE SOILPROP (SOILCBOT,WEIGHT, ITIMESTEP, MAVAIL, &  ! IN
                           PXLSM_SMOIS_INIT,                   &  ! IN
                           FWSAT,FWFC,FWWLT,FCLAY,FCSAND,      &  ! OUT
                           FFMSAND,FB,FCGSAT,                  &  ! OUT
                           FJP,FAS,FC2R,FC1SAT,FWRES,FC3,ISTI, &  ! OUT
                           WG, W2    )                            ! OUT
     !------------------------------------------------------------------------
     !     SOILPROP COMPUTES SOIL PARAMETERS FOR BOTH BOTTOM AND TOP LAYERS
     !              USING FRACTIONAL SOIL TYPE. A HARD CODED OPTION IS AVAILIABLE
     !              TO COMPUTE THE SOIL PARAMETERS USING FRACTIONAL INFORMATION, OR
     !              TO JUST USE SOIL PARAMETERS OF THE DOMINANT SOIL TYPE
     !------------------------------------------------------------------------
     !-- SOIL PARAMETERS ARE SPECIFIED BY SOIL TYPE:
     !   #  SOIL TYPE  WSAT  WFC  WWLT    B   CGSAT   JP   AS   C2R  C1SAT WRES
     !   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____ ____
     !   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082  0.020
     !   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098  0.035
     !   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132  0.041
     !   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153  0.015
     !   5  SILT       .485 .255  .179  5.30  4.418    6  .105  0.8  .153  0.015
     !   6  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191  0.027
     !   7  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213  0.068
     !   8  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385  0.040
     !   9  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227  0.075
     !  10  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421  0.109
     !  11  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375  0.056
     !  12  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342  0.090
     !------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
      IMPLICIT NONE

     !.........Arguments
      INTEGER, PARAMETER             :: NSCAT    = 16     ! max. soil types
      INTEGER, PARAMETER             :: NSCATMIN = 16     ! min soil types

      INTEGER,                      INTENT(IN)   :: WEIGHT, ITIMESTEP, PXLSM_SMOIS_INIT
      REAL,                         INTENT(IN)   :: MAVAIL
      REAL,     DIMENSION(1:NSCAT), INTENT(IN)   :: SOILCBOT
      REAL,                         INTENT(OUT)  :: FWSAT,FWFC,FWWLT,FCLAY,       &
                                                    FCSAND,FFMSAND,FB,FCGSAT,     &
                                                    FJP,FAS,FC2R,FC1SAT,FWRES,FC3
      REAL,                         INTENT(INOUT)  :: W2, WG


      INTEGER,                      INTENT(OUT)  :: ISTI
!........Local variables
      CHARACTER*4, AVCLASS
      CHARACTER*4, DIMENSION( 1: NSCAT ) ::  TEXID 
!... Integer
      INTEGER:: S
!... Real
      REAL:: TFRACBOT, CFRAC, SUMCSND, SUMFMSND, SUMCLY,        &
             AVS, AVCS, AVFMS, AVC, AVSLT,                      &
             SSMPOT, DSMPOT         ! saturated and air-dry soil matric potential

      REAL,    DIMENSION( 1: NSCAT ) :: WSAT, WFC, WWLT, B, CGSAT, AS,  &
                                        JP, C2R, C1SAT, WRES

      REAL,    DIMENSION( 1: NSCATMIN ) ::  CSAND,FMSAND, CLAY
!.......... DATA statement for SOIL PARAMETERS for the 11 soil types
!...........Follow Menut et al., 2013 JGR
      DATA CSAND  /46.0,40.0,29.0, 0.0, 0.0,                        &
                    0.0,29.0, 0.0, 0.0, 0.0,                        &
                    0.0, 0.0, 0.0, 0.0,46.0,                        &
                    0.0/
      DATA FMSAND /46.0,40.0,29.0,17.0,10.0,                        &
                   43.0,29.0,10.0,32.0,52.0,                        &
                    6.0,22.0,43.0,43.0,46.0,                        &
                   32.0/

      DATA CLAY   / 3.0, 4.0,10.0,13.0, 5.0,                        &
                   18.0,27.0,34.0,34.0,42.0,                        &
                   47.0,58.0,18.0,18.0, 3.0,                        &
                   34.0/

      DATA TEXID/'Sand','Lsan','Sloa','Sill','Silt',                &
                 'Loam','Sclo','Sicl','Cllo','Sacl',                &
                 'Sicy','Clay','Ormt','Wate','Bedr',                &
                 'Othe'/                                         
!-- Removed soil parameter lookup table data since these parameters are now computed analytically (Noilhan and Mahfouf 1996)

      DSMPOT  =  -1.0E7          ! mm air-dry water matric potential
!   
!-------------------------------Exicutable starts here--------------------

!     Compute soil characteristics by sand (coarse and fine-medium) and clay fraction
    
      CFRAC      = 0.0                                                               
      SUMCSND    = 0.0
      SUMFMSND   = 0.0
      SUMCLY     = 0.0                                                               
      TFRACBOT   = 0.0                                                              
                                                                                
      DO S = 1,NSCAT                                                        

        TFRACBOT  = TFRACBOT  + SOILCBOT(S)
        SUMCSND   = SUMCSND   + CSAND(S)  * SOILCBOT(S)
        SUMFMSND  = SUMFMSND  + FMSAND(S) * SOILCBOT(S)
        SUMCLY    = SUMCLY    + CLAY(S)   * SOILCBOT(S)
        
        IF(SOILCBOT(S).GE.CFRAC) THEN   ! Find Dominant Category and fraction
          ISTI=S
          CFRAC=SOILCBOT(S)
        ENDIF

      ENDDO

         
      IF(TFRACBOT.GT.0.001) THEN                                                  
        AVCS  = SUMCSND / TFRACBOT
        AVFMS = SUMFMSND / TFRACBOT
        AVS   = AVCS + AVFMS                                               

        AVC   = SUMCLY / TFRACBOT                                           
        AVSLT = 100.0 - AVS - AVC                                   
        
        IF(AVS.GT.(85.+ 0.5*AVC)) THEN
          AVCLASS= 'Sand'                                                   
          ISTI = 1
        ELSE IF(AVS.GT.(70.+ AVC)) THEN
          AVCLASS= 'Lsan'                                             
          ISTI = 2
        ELSE IF((AVC.LT.20..AND.AVS.GT.52.) &
                .OR.(AVC.LE.7.5.AND.AVSLT.LT.50.)) THEN                
          AVCLASS= 'Sloa'                                             
          ISTI = 3
        ELSE IF(AVC.LT.35..AND.AVS.GT.45..AND.AVSLT.LT.28.) THEN      
          AVCLASS= 'Sclo'                                                  
          ISTI = 7
        ELSE IF(AVC.GE.35..AND.AVS.GT.45.) THEN                           
          AVCLASS = 'Sacl'                                                  
          ISTI = 10
        ELSE IF(AVC.LT.27.0.AND.AVSLT.LT.50.) THEN                        
          AVCLASS= 'Loam'                                                
          ISTI = 6
        ELSE IF(AVC.LT.12..AND.AVSLT.GT.80.) THEN                
          AVCLASS = 'Silt'                                         
          ISTI = 5
        ELSE IF(AVC.LT.27.) THEN                                                 
          AVCLASS = 'Sill'                                                       
          ISTI = 4
        ELSE IF(AVC.LT.40..AND.AVS.GT.20.) THEN                                  
          AVCLASS = 'Cllo'                                                       
          ISTI = 9 
        ELSE IF(AVC.LT.40.) THEN                                                 
          AVCLASS = 'Sicl'                                                       
          ISTI = 8       
        ELSE IF(AVSLT.GE.40.) THEN                                               
          AVCLASS = 'Sicy'                                                       
          ISTI = 11            
        ELSE                                                                     
          AVCLASS = 'Clay'                                                       
          ISTI = 12                        
        ENDIF                                                                          
      ELSE
        ! set no soil to 9 - clay loam
        ISTI = 9
        AVCLASS = TEXID(ISTI)  

        AVCS  = CSAND(ISTI)
        AVFMS = FMSAND(ISTI)
        AVS   = AVCS + AVFMS

        AVC   = CLAY(ISTI)
        AVSLT = 100.0 - AVS - AVC

      ENDIF

      FCSAND  = AVCS
      FFMSAND = AVFMS
      FCLAY   = AVC
  
      ! Continous formulation of secondary soil parmeters (Noilhan and Mahfouf 1996)
      FWSAT = (-1.08 * AVS + 494.305) * 1.0E-3
      FWWLT = 37.1342E-3 * SQRT(AVC)
      FWFC  = 89.0467E-3 * AVC**0.3496
      FB    = 0.137 * AVC + 3.501
      FCGSAT= -1.557E-2 * AVS - 1.441E-2 * AVC + 4.7021
      FC1SAT= (5.58 * AVC + 84.88) * 1.0E-2
      FC2R  = 13.815 * AVC**(-0.954)
      FC3   = 5.327 * AVC **(-1.043)
      FAS   = 732.42E-3 * AVC **(-0.539)
      FJP   = 0.134 * AVC + 3.4
      FWRES = 0.00123 * AVC - 0.00066 * AVSLT + 0.0405  !J. Pleim fitted function
      FWRES = AMAX1(FWRES, 0.01)                        !L. Ran set minimum

      !SSMPOT = -10.0 * 10.0**(1.88 - 0.0131 * AVS)   !compute saturated mineral soil matric potential CLM4.5
      !FWRES = FWSAT * (SSMPOT/DSMPOT)**(1.0 / FB)    !Swenson and Lawrence 2014, Dingman 2002
                            
    
    ! Compute W2 using soil moisture availiability if pxlsm_smois_init (in namelist) is not zero
    IF (ITIMESTEP .EQ. 1 .AND. PXLSM_SMOIS_INIT .GT. 0) THEN
       WG = FWWLT + (0.5*(FWSAT+FWFC) - FWWLT)  * MAVAIL 
       W2 = FWWLT + (0.5*(FWSAT+FWFC) - FWWLT)  * MAVAIL 
    ENDIF                                                                    
    
    END SUBROUTINE soilprop
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
      SUBROUTINE PXSNOW (ITIMESTEP, ASNOW, CSNOW, SNOW,       &
                         SNOWH, SNUP,                         &
                         ALB, SNOALB, SHDFAC, SHDMIN,         & 
                         HC_SNOW, SNOW_FRA, SNOWC, SNOWALB)                   
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------
!     Pleim-Xiu LSM Simple Snow model
!---------------------------------------------------------------------------------------------------
!     ITIMESTEP -- Model time step index
!     ASNOW --  Analyzed snow water equivalent (mm)
!     CSNOW --  Current snow water equivalent (mm)
!     SNOW  --  Snow water equivalent in model (mm)
!     SNOWH --  Physical snow depth (m)
!     SNUP --   Physical snow depth (landuse dependent) where when below, snow cover is fractional
!
!     HC_SNOW -- Heat capacity of snow as a function of depth
!     SNOW_FRA-- Factional snow area 
!     IFSNOW  -- Snow flag
!     SNOWALB -- Snow albedo
!---------------------------------------------------------------------------------------------------

      IMPLICIT NONE

!---  Arguments
      REAL, PARAMETER  :: W2SN_CONV   =   10.0
      REAL, PARAMETER  :: CS_SNOWPACK = 2092.0
      REAL, PARAMETER  :: SALP        =    2.6
!-----------------------------------------------
      INTEGER,       INTENT(IN)    :: ITIMESTEP
      REAL,          INTENT(IN)    :: ASNOW, CSNOW, SNUP, ALB, SNOALB, SHDFAC, SHDMIN
      REAL,          INTENT(INOUT) :: SNOW, SNOWH, SNOWC
      REAL,          INTENT(OUT)   :: HC_SNOW, SNOW_FRA, SNOWALB
!------------------------------------------------------------------------------------

                                                   
!-----------------------------------------------
!    Local variables
     !... Real
     REAL:: CONV_WAT2SNOW, CSNOWW, RHO_SNOWPACK,   &
            LIQSN_RATIO, SNEQV, RSNOW
!-----------------------------------------------
             
     SNEQV=ASNOW*0.001              ! Snow water in meters
     RHO_SNOWPACK = 450                   ! Snowpack density (kg/m3), this should be computed in the future
     LIQSN_RATIO  = DENW/RHO_SNOWPACK     ! Ratio of water density and snowpack density
     
     CONV_WAT2SNOW = LIQSN_RATIO/1000     ! Conversion factor for snow liquid equiv. (mm) to snowpack (m)
      
     SNOW = ASNOW                         ! Set snow water to analysis value for now, implement a nudging scheme later
     SNOWH= SNOW * CONV_WAT2SNOW          ! Conversion of snow water (mm) to snow depth (m)     
     
     
     ! Is snow cover now present. The limit is 0.45 mm of water eqivalent or about 2 inches snow depth 
     SNOWC = 0.0
     IF (SNOWH .GT. 0.005) SNOWC = 1.0  
     
     HC_SNOW = RHO_SNOWPACK * CS_SNOWPACK * SNOWH
     
      IF (SNEQV < SNUP) THEN                                                  
         RSNOW = SNEQV / SNUP                                                    
         SNOW_FRA = 1. - ( EXP ( - SALP * RSNOW) - RSNOW * EXP ( - SALP))          
      ELSE                                                                       
         SNOW_FRA = 1.0                                                            
      END IF   
      
      SNOWC  = SNOW_FRA

      SNOWALB = ALB + SNOWC*(SNOALB-ALB)

    END SUBROUTINE pxsnow
!------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------

END MODULE module_sf_pxlsm

