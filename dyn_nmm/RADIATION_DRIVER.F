!WRF:MODEL_LAYER:PHYSICS
!
   SUBROUTINE radiation_driver (itimestep,dt,                            &
                         RTHRATENLW,RTHRATENSW,RTHRATEN,                 &
                         GLW,GSW,                                        &
                         XLAT,XLONG,ALBEDO,CLDFRA,EMISS,                 &
                         rho_phy,moist,n_moist,                          &
                         p8w,p_phy,Pb,pi_phy,dz8w,t_phy,t8w,GMT,         &
!#$                      JULDAY,config_flags,RADT,STEPRA,ICLOUD,         &
                         JULDAY,RADT,                                    &
                         NRADS,NRADL,RA_SW_PHYSICS,RA_LW_PHYSICS,        &
                         ICLOUD,                                         &
                         taucldi,taucldc,warm_rain,                      &
! for eta radiation
                         XLAND,TSK,HTOP,HBOT,CUPPT,VEGFRA,SNOW,LTOP,     &
                         julyr,NPHS,                                     &
                         RSWTOA,RLWTOA,CZMEAN,                           & !Added
                         CFRACL,CFRACM,CFRACH,                           & !Added
                         ACFRST,NCFRST,ACFRCV,NCFRCV,                    & !Added
                         ids,ide, jds,jde, kds,kde,                      &
                         ims,ime, jms,jme, kms,kme,                      &
                         its,ite, jts,jte, kts,kte                       )        
!-------------------------------------------------------------------------

!#$USE module_bc
!#$USE module_state_description
   USE module_model_constants
!#$USE module_wrf_error

! *** add new modules of schemes here

!#$USE module_ra_sw
!#$USE module_ra_gsfcsw
!#$USE module_ra_rrtm
   USE module_ra_gfdleta

   !  This driver calls subroutines for the radiation parameterizations.
   !
   !  short wave radiation choices:
   !  1. swrad (19??)
   !
   !  long wave radiation choices:
   !  1. rrtmlwrad
   !
!----------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------
! In WRF, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  In your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         .
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Grid structure in physics part of WRF
!-------------------------------------
! The horizontal velocities used in the physics are unstaggered 
! relative to temperature/moisture variables. All predicted 
! variables are carried at half levels except w, which is at full 
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!==================================================================
! Definitions
!-----------
! Rho_d      dry density (kg/m^3)
! Theta_m    moist potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- RTHRATEN      Rho_dTheta_m tendency 
!                 due to radiation (kg/m^3 . K)
!-- RTHRATENLW    Rho_dTheta_m tendency 
!                 due to long wave radiation (kg/m^3 . K)
!-- RTHRATENSW    Rho_dTheta_m temperature tendency 
!                 due to short wave radiation (kg/m^3 . K)
!-- dt            time step (s)
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           downward short wave flux at ground surface (W/m^2)
!-- XLAT          latitude, south is negative (degree)
!-- XLONG         longitude, west is negative (degree)
!-- ALBEDO                albedo (between 0 and 1)
!-- CLDFRA        cloud fraction (between 0 and 1)
!-- EMISS         surface emissivity (between 0 and 1)
!-- rho_phy       density (kg/m^3)
!-- rr            dry air density (kg/m^3)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- n_moist       number of moisture species
!-- p8w           pressure at full levels (Pa)
!-- p_phy         pressure (Pa)
!-- Pb            base-state pressure (Pa)
!-- pi_phy        exner function (dimensionless)
!-- dz8w          dz between full levels (m)
!-- t_phy         temperature (K)
!-- t8w           temperature at full levels (K)
!-- GMT           Greenwich Mean Time Hour of model start (hour)
!-- JULDAY        the initial day (Julian day)
!-- config_flags  boundary condition flag
!-- RADT          time for calling radiation (min)
!-- DEGRAD        conversion factor for 
!                 degrees to radians (pi/180.) (rad/deg)
!-- DPD           degrees per day for earth's 
!                 orbital position (deg/day)
!-- R_d           gas constant for dry air (J/kg/K)
!-- CP            heat capacity at constant pressure for dry air (J/kg/K)
!-- G             acceleration due to gravity (m/s^2)
!-- rvovrd        R_v divided by R_d (dimensionless)
!-- XTIME         time since simulation start (min)
!-- DECLIN        solar declination angle (deg)
!-- SOLCON        solar constant (W/m^2)
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QS          species index for snow
!-- P_QG          species index for graupel
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!==================================================================
!#$TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
!
   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte, &
                                       n_moist
   INTEGER,      INTENT(IN   )    ::   NRADS,NRADL,ICLOUD
   CHARACTER(20),INTENT(IN) :: RA_LW_PHYSICS,RA_SW_PHYSICS
   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  HTOP, &
                                                            HBOT, &
                                                           CUPPT

   INTEGER, INTENT(IN   )  ::   julyr,NPHS
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                                     dz8w, &
                                                             p8w, &
                                                           p_phy, &
                                                              Pb, &
                                                          pi_phy, &
                                                           t_phy, &
                                                             t8w, &
                                                         rho_phy, &
                                                          CLDFRA
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, n_moist ),         &
         INTENT(IN ) ::                                    moist
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN, &
                                                      RTHRATENLW, &
                                                      RTHRATENSW

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                       taucldi,taucldc
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                          ALBEDO, &
                                                           EMISS
!
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(OUT) :: CFRACH,     & !Added
                                                      CFRACL,     & !Added
                                                      CFRACM,     & !Added
                                                      CZMEAN        !Added
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, &
                                                             GLW, &
                                                          RLWTOA, &  !Added
                                                          RSWTOA, &  !Added
                                                          ACFRST, &  !Added
                                                          ACFRCV     !Added
!
   REAL, INTENT(IN  )   ::                                GMT,dt
!
   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep
!
   INTEGER,DIMENSION(3),INTENT(IN) :: LTOP
!
   INTEGER,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT) :: NCFRST, &  !Added
                                                          NCFRCV     !Added
 
! LOCAL  VAR

   REAL, DIMENSION( its:ite, jts:jte ) ::             GLAT,GLON

   REAL    ::    XTIME,DECLIN,SOLCON 
   INTEGER ::    i,j,k
!#$LOGICAL ::    gfdl_lw,gfdl_sw
   LOGICAL ::    longwave,shortwave

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC

   INTEGER :: P_QV=1
   INTEGER :: P_QC=2
   CHARACTER(20),PARAMETER :: RRTMSCHEME='RRTMSCHEME'
   CHARACTER(20),PARAMETER :: GFDLLWSCHEME='GFDLLWSCHEME'
   CHARACTER(20),PARAMETER :: SWRADSCHEME='SWRADSCHEME'
   CHARACTER(20),PARAMETER :: GSFCSWSCHEME='GSFCSWSCHEME'
   CHARACTER(20),PARAMETER :: GFDLSWSCHEME='GFDLSWSCHEME'
!------------------------------------------------------------------

!#$if (config_flags%ra_lw_physics .eq. 0 .and. &
!#$    config_flags%ra_sw_physics .eq. 0)         return

!#$gfdl_lw = .false.
!#$gfdl_sw = .false.

   longwave=.false.
   shortwave=.false.

!#@IF (itimestep .eq. 1 .or. mod(itimestep-1,STEPRA) .eq. 0) THEN
!---------------
! initialize data

   DO j=jts,jte
   DO i=its,ite
      GSW(I,J)=0.
      GLW(I,J)=0.
      GLAT(I,J)=XLAT(I,J)*DEGRAD
      GLON(I,J)=XLONG(I,J)*DEGRAD
   ENDDO
   ENDDO

   DO j=jts,jte
   DO k=kts,kte
   DO i=its,ite
      RTHRATEN(I,K,J)=0.
   ENDDO
   ENDDO
   ENDDO
!---------------
! Calculate constant for short wave radiation

   CALL radconst(XTIME,DECLIN,SOLCON,GMT,JULDAY,           &
                 DEGRAD,DPD,itimestep,dt                   )

!  CALL cal_cldfra(CLDFRA,moist(ims,kms,jms,P_QC),         &
!                  moist(ims,kms,jms,P_QI),P_QI,           &
!                  ids,ide, jds,jde, kds,kde,              &
!                  ims,ime, jms,jme, kms,kme,              &
!                  its,ite, jts,jte, kts,kte               )

!#$WRITE(wrf_err_message,*)'SOLCON=',SOLCON,DECLIN,XTIME
!#$CALL wrf_debug(50,wrf_err_message)

!
   IF(mod(itimestep-1,NRADL).eq.0)THEN
     longwave=.true.
   ENDIF
!
   IF(LONGWAVE)THEN
!#$lwrad_select: SELECT CASE(config_flags%ra_lw_physics)
   lwrad_select: SELECT CASE(ra_lw_physics)

        CASE (RRTMSCHEME)
!#$          CALL wrf_debug (100, 'CALL rrtm')

!            CALL RRTMLWRAD(RTHRATEN,GLW,EMISS,                    &
!                    moist(ims,kms,jms,P_QV),                      &
!                    moist(ims,kms,jms,P_QC),                      &
!                    moist(ims,kms,jms,P_QR),                      &
!                    moist(ims,kms,jms,P_QI),                      &
!                    moist(ims,kms,jms,P_QS),                      &
!                    moist(ims,kms,jms,P_QG),                      &
!                    p8w,p_phy,pi_phy,dz8w,t_phy,                  &
!                    t8w, rho_phy, CLDFRA,                         &
!                    R_d,G,P_QI,P_QS,P_QG,ICLOUD,                  &
!                    PARAM_FIRST_SCALAR,warm_rain,                 &
!                    ids,ide, jds,jde, kds,kde,                    &     
!                    ims,ime, jms,jme, kms,kme,                    &
!                    its,ite, jts,jte, kts,kte                     )

       CASE (GFDLLWSCHEME)

!#$          CALL wrf_debug (100, 'CALL gfdllw')

!#$          gfdl_lw  = .true.

             CALL  ETARA(DT,RTHRATEN,RTHRATENLW,RTHRATENSW,pi_phy, &
                      XLAND,p8w,dz8w,rho_phy,p_phy,t_phy,          &
                      moist(ims,kms,jms,P_QV),                     &
                      moist(ims,kms,jms,P_QC),TSK,GLW,GSW,         &
                      RLWTOA,RSWTOA,CZMEAN,                        & !Added
                      GLAT,GLON,HTOP,HBOT,ALBEDO,CUPPT,            &
                      VEGFRA,SNOW,G,GMT,                           & !Modified
                      SHORTWAVE,LONGWAVE,NRADS,NRADL,NPHS,itimestep, & !Modified
!#$                   julyr,julday,gfdl_lw,gfdl_sw,                &
                      julyr,julday,LTOP,                           & !Modified
                      CFRACL,CFRACM,CFRACH,                        &
                      ACFRST,NCFRST,ACFRCV,NCFRCV,                 &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      its,ite, jts,jte, kts,kte                    )

        CASE DEFAULT
  
!#$          CALL wrf_message ('The long wave scheme does not exist')
           
   END SELECT lwrad_select    


!#$IF (config_flags%ra_lw_physics .gt. 0 ) THEN
      DO j=jts,jte
      DO k=kts,kte
      DO i=its,ite
         RTHRATENLW(I,K,J)=RTHRATEN(I,K,J)
      ENDDO
      ENDDO
      ENDDO
!#$ENDIF

      longwave=.false.

   ENDIF  !End LONGWAVE block

!
   IF(mod(itimestep-1,NRADS).eq.0)THEN
     shortwave=.true.
   ENDIF
!
   IF(SHORTWAVE)THEN
!#$swrad_select: SELECT CASE(config_flags%ra_sw_physics)
   swrad_select: SELECT CASE(ra_sw_physics)

        CASE (SWRADSCHEME)
!#$          CALL wrf_debug(100, 'CALL swrad')
!            CALL SWRAD(dt,RTHRATEN,GSW,XLAT,XLONG,ALBEDO,         &
!                    rho_phy,t_phy,                                &
!                    moist(ims,kms,jms,P_QV),                      &
!                    moist(ims,kms,jms,P_QC),                      &
!                    moist(ims,kms,jms,P_QR),                      &
!                    moist(ims,kms,jms,P_QI),                      &
!                    moist(ims,kms,jms,P_QS),                      &
!                    moist(ims,kms,jms,P_QG),                      &
!                    p_phy,pi_phy,dz8w,GMT,                        &
!                    R_d,CP,G,JULDAY,                              &
!                    XTIME,DECLIN,SOLCON,                          &
!                    P_QI,P_QS,P_QG,RADT,ICLOUD,DEGRAD,            &
!                    PARAM_FIRST_SCALAR,warm_rain,                 &
!                    ids,ide, jds,jde, kds,kde,                    &    
!                    ims,ime, jms,jme, kms,kme,                    & 
!                    its,ite, jts,jte, kts,kte                     ) 

        CASE (GSFCSWSCHEME)
!            CALL GSFCSWRAD(RTHRATEN,GSW,XLAT,XLONG,               &
!                    ALBEDO,t_phy,                                 &
!                    moist(ims,kms,jms,P_QV),                      &
!                    moist(ims,kms,jms,P_QC),                      &
!                    moist(ims,kms,jms,P_QR),                      &
!                    moist(ims,kms,jms,P_QI),                      &
!                    moist(ims,kms,jms,P_QS),                      &
!                    moist(ims,kms,jms,P_QG),                      &
!                    p_phy,p8w,pi_phy,CLDFRA,                      &
!                    GMT,CP,G,JULDAY,XTIME,DECLIN,SOLCON,          &
!                    P_QI,P_QS,P_QG,RADT,DEGRAD,                   &
!                    taucldi,taucldc,PARAM_FIRST_SCALAR,warm_rain, &
!                    ids,ide, jds,jde, kds,kde,                    &
!                    ims,ime, jms,jme, kms,kme,                    &
!                    its,ite, jts,jte, kts,kte                     )

       CASE (GFDLSWSCHEME)

!#$          CALL wrf_debug (100, 'CALL gfdlsw')

!#$          gfdl_sw = .true.

             CALL  ETARA(DT,RTHRATEN,RTHRATENLW,RTHRATENSW,pi_phy, &
                      XLAND,p8w,dz8w,rho_phy,p_phy,t_phy,          &
                      moist(ims,kms,jms,P_QV),                     &
                      moist(ims,kms,jms,P_QC),TSK,GLW,GSW,         &
                      RLWTOA,RSWTOA,CZMEAN,                        & !Added
                      GLAT,GLON,HTOP,HBOT,ALBEDO,CUPPT,            &
                      VEGFRA,SNOW,G,GMT,                           & !Modified
                      SHORTWAVE,LONGWAVE,NRADS,NRADL,NPHS,itimestep, & !Modified
!#$                   julyr,julday,gfdl_lw,gfdl_sw,                &
                      julyr,julday,LTOP,                           & !Modified
                      CFRACL,CFRACM,CFRACH,                        &
                      ACFRST,NCFRST,ACFRCV,NCFRCV,                 &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      its,ite, jts,jte, kts,kte                    )

        CASE DEFAULT

!#$          CALL wrf_message('The short wave scheme does not exist')

   END SELECT swrad_select    

!#$IF (config_flags%ra_sw_physics .gt. 0) THEN
      DO j=jts,jte
      DO k=kts,kte
      DO i=its,ite
         RTHRATENSW(I,K,J)=RTHRATEN(I,K,J)-RTHRATENLW(I,K,J)
      ENDDO
      ENDDO
      ENDDO
!#$ENDIF

      shortwave=.false.

   ENDIF  !End SHORTWAVE block

   END SUBROUTINE radiation_driver

!---------------------------------------------------------------------
   SUBROUTINE radconst(XTIME,DECLIN,SOLCON,GMT,JULDAY,               &
                       DEGRAD,DPD,step,dt                            )
!---------------------------------------------------------------------
!#$USE module_wrf_error
   IMPLICIT NONE
!---------------------------------------------------------------------

   INTEGER, INTENT(IN   )   ::       JULDAY, step
   REAL, INTENT(IN   )      ::       GMT,dt,DEGRAD,DPD
   REAL, INTENT(OUT  )      ::       XTIME,DECLIN,SOLCON
   REAL                     ::       OBECL,SINOB,SXLONG,ARG,JULIAN,  &
                                     DECDEG,DJUL,RJUL,ECCFAC

! for short wave radiation

   DECLIN=0.
   SOLCON=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.
        
   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)
   XTIME=float(step)*dt/60.
        
!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:
        
   JULIAN=FLOAT(JULDAY-1)+(XTIME/60.+GMT)/24.
   IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)
   IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)
   SXLONG=SXLONG*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)
   DECDEG=DECLIN/DEGRAD
!----SOLAR CONSTANT ECCENTRICITY FACTOR (PALTRIDGE AND PLATT 1976)
   DJUL=JULIAN*360./365.
   RJUL=DJUL*DEGRAD
   ECCFAC=1.000110+0.034221*COS(RJUL)+0.001280*SIN(RJUL)+0.000719*  &
          COS(2*RJUL)+0.000077*SIN(2*RJUL)
   SOLCON=1370.*ECCFAC
   
!#$write(wrf_err_message,10)DECDEG,SOLCON
10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',     &
        ' SOLAR CONSTANT = ',F8.2,' W/M**2 ***')
!#$CALL wrf_debug (50, wrf_err_message)

   END SUBROUTINE radconst

!---------------------------------------------------------------------
   SUBROUTINE cal_cldfra(CLDFRA,QC,QI,P_QI,                          &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte

   INTEGER,  INTENT(IN   )   ::           P_QI
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QI, &
                                                                 QC

   REAL thresh
   INTEGER:: i,j,k
!---------------------------------------------------------------------
     thresh=1.0e-6

     IF (P_QI .gt. 0) THEN
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j)+QI(I,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ELSE
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           IF ( QC(i,k,j) .gt. thresh) THEN
              CLDFRA(i,k,j)=1.
           ELSE
              CLDFRA(i,k,j)=0.
           ENDIF
        ENDDO
        ENDDO
        ENDDO
     ENDIF

   END SUBROUTINE cal_cldfra

