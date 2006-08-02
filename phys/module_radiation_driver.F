!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_radiation_driver
   integer, parameter :: r8=8
   integer, parameter :: ntau=7    ! number of optical depth ranges
   integer, parameter :: npres=7   ! number of pressure ranges
   integer invtau(-20:45000)            !  ISCCP table for converting optical thickness
                                        !  to count value
   real(r8) tautab(0:255)               !  ISCCP table for converting count value to
                                        !  optical thickness
CONTAINS
!BOP
! !IROUTINE: radiation_driver - interface to radiation physics options

! !INTERFACE:
   SUBROUTINE radiation_driver (                                         &
                       itimestep,dt,restart                              &
                      ,lw_physics,sw_physics,NPHS                        &
                      ,AER_DRY,AER_WATER                                 &
                      ,RTHRATENLW,RTHRATENSW,RTHRATEN                    &
                      ,SWUP,SWDN,SWUPCLEAR,SWDNCLEAR                     &      ! added to output upward and downward clearsky
                      ,LWUP,LWDN,LWUPCLEAR,LWDNCLEAR                     &      ! and total shortwave and longwave fluxes
                      ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC                 &
                      ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC                 &
                      ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC                 &
                      ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC                 &
                      ,GLW,GSW,LWCF,SWCF,OLR,SWDOWN,XLAT,XLONG,ALBEDO    &
!                     ,GLW,GSW,SWDOWN,XLAT,XLONG,ALBEDO                  &
                      ,EMISS ,rho ,p8w,p ,pi,dz8w,t,t8w,GMT              &
                      ,XLAND,XICE,TSK,HTOP,HBOT,CUPPT,VEGFRA,SNOW        &
                      ,julyr,JULDAY,JULIAN,RADT,STEPRA,ICLOUD ,warm_rain &
                      ,RSWTOA,RLWTOA,CZMEAN                              &
                      ,CFRACL,CFRACM,CFRACH                              &
                      ,ACFRST,NCFRST,ACFRCV,NCFRCV                       &
                      ,z,znu,znw                                         &
                      ,  ozmixm,pin,levsiz,n_ozmixm                      &
                      ,  m_ps_1,m_ps_2,aerosolc_1,aerosolc_2,m_hybi0     &
                      ,  n_aerosolc, paerlev                             &
                      ,  cptop,ctaucld,cttop                             &
                      ,abstot, absnxt, emstot                            &
                      ,ids,ide, jds,jde, kds,kde                         &
                      ,ims,ime, jms,jme, kms,kme                         &
                      ,i_start,i_end,j_start,j_end,kts,kte,num_tiles     &
                    ! Optional
                      ,taucldi ,taucldc                                  &
                      ,qv,qc,qr,qi,qs,qg                                 &
                      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg                     &
                      ,SWDOWNC,CLDFRA ,concld, Pb                        &
                      ,f_ice_phy,f_rain_phy                              &
                                                                         )

!-------------------------------------------------------------------------

! !USES:
   USE module_state_description, ONLY : RRTMSCHEME, GFDLLWSCHEME        &
                                       ,SWRADSCHEME, GSFCSWSCHEME       &
                                       ,GFDLSWSCHEME, CAMLWSCHEME, CAMSWSCHEME
   USE module_model_constants
   USE module_wrf_error

! *** add new modules of schemes here

   USE module_ra_sw
   USE module_ra_gsfcsw
   USE module_ra_rrtm
   USE module_ra_cam
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
!<DESCRIPTION>
!
! Radiation_driver is the WRF mediation layer routine that provides the interface to
! to radiation physics packages in the WRF model layer. The radiation
! physics packages to call are chosen by setting the namelist variable
! (Rconfig entry in Registry) to the integer value assigned to the 
! particular package (package entry in Registry). For example, if the
! namelist variable ra_lw_physics is set to 1, this corresponds to the
! Registry Package entry for swradscheme.  Note that the Package
! names in the Registry are defined constants (frame/module_state_description.F)
! in the CASE statements in this routine.
!
! Among the arguments is moist, a four-dimensional scalar array storing
! a variable number of moisture tracers, depending on the physics 
! configuration for the WRF run, as determined in the namelist.  The
! highest numbered index of active moisture tracers the integer argument
! n_moist (note: the number of tracers at run time is the quantity
! <tt>n_moist - PARAM_FIRST_SCALAR + 1</tt> , not n_moist. Individual tracers
! may be indexed from moist by the Registry name of the tracer prepended
! with P_; for example P_QC is the index of cloud water. An index 
! represents a valid, active field only if the index is greater than
! or equal to PARAM_FIRST_SCALAR.  PARAM_FIRST_SCALAR and the individual
! indices for each tracer is defined in module_state_description and
! set in <a href=set_scalar_indices_from_config.html>set_scalar_indices_from_config</a> defined in frame/module_configure.F.
!
! Physics drivers in WRF 2.0 and higher, originally model-layer 
! routines, have been promoted to mediation layer routines and they
! contain OpenMP threaded loops over tiles.  Thus, physics drivers
! are called from single-threaded regions in the solver. The physics
! routines that are called from the physics drivers are model-layer
! routines and fully tile-callable and thread-safe.
!</DESCRIPTION>
! 
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
! 
!-------------------------------------
! The horizontal velocities used in the physics are unstaggered 
! relative to temperature/moisture variables. All predicted 
! variables are carried at half levels except w, which is at full 
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!==================================================================
! Definitions
!-----------
! Theta      potential temperature (K)
! Qv         water vapor mixing ratio (kg/kg)
! Qc         cloud water mixing ratio (kg/kg)
! Qr         rain water mixing ratio (kg/kg)
! Qi         cloud ice mixing ratio (kg/kg)
! Qs         snow mixing ratio (kg/kg)
!-----------------------------------------------------------------
!-- RTHRATEN      Theta tendency 
!                 due to radiation (K/s)
!-- RTHRATENLW    Theta tendency 
!                 due to long wave radiation (K/s)
!-- RTHRATENSW    Theta temperature tendency 
!                 due to short wave radiation (K/s)
!-- dt            time step (s)
!-- itimestep     number of time steps
!-- GLW           downward long wave flux at ground surface (W/m^2)
!-- GSW           net short wave flux at ground surface (W/m^2)
!-- SWDOWN        downward short wave flux at ground surface (W/m^2)
!-- SWDOWNC       clear-sky downward short wave flux at ground surface (W/m^2; optional; for AQ)
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
!-- DECLIN        solar declination angle (rad)
!-- SOLCON        solar constant (W/m^2)
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
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!==================================================================
!
   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                                         kts,kte, &
                                       num_tiles

   INTEGER, INTENT(IN)            :: lw_physics, sw_physics

!  INTEGER,      INTENT(IN   )    ::   nisccp

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   STEPRA,ICLOUD
   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc

   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 
   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ),      &
          INTENT(IN   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), INTENT(IN )  ::                   PIN

   REAL,  DIMENSION(ims:ime,jms:jme), INTENT(IN )  ::      m_ps_1,m_ps_2
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ),      &
          INTENT(IN   ) ::                       aerosolc_1, aerosolc_2
   REAL,  DIMENSION(paerlev) ::                           m_hybi0

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  HTOP, &
                                                            HBOT, &
                                                           CUPPT

   INTEGER, INTENT(IN   )  ::   julyr
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::                                     dz8w, &
                                                               z, &
                                                             p8w, &
                                                               p, &
                                                              pi, &
                                                               t, &
                                                             t8w, &
                                                             rho, &
                                                         aer_dry, &
                                                       aer_water
!
   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: znu,znw

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN, &
                                                      RTHRATENLW, &
                                                      RTHRATENSW

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                                  SWUP, &
                                                            SWDN, &
                                                       SWUPCLEAR, &
                                                       SWDNCLEAR, &
                                                            LWUP, &
                                                            LWDN, &
                                                       LWUPCLEAR, &
                                                       LWDNCLEAR

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  SWCF, &
                                                            LWCF, &
                                                             OLR 
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::          &
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          & 
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC      
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                          ALBEDO, &
                                                           EMISS
!
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                   GSW, &
                                                             GLW

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(OUT)  ::   SWDOWN
!
   REAL, INTENT(IN  )   ::                                GMT,dt
!
   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep
   REAL,    INTENT(IN  ) ::                    JULIAN
   LOGICAL , INTENT(IN)  ::                              restart

   INTEGER,INTENT(IN)                                       :: NPHS
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(OUT)          ::    &
                                                      CFRACH,     & !Added
                                                      CFRACL,     & !Added
                                                      CFRACM,     & !Added
                                                      CZMEAN        !Added
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                        &
                                                      RLWTOA,     & !Added
                                                      RSWTOA,     & !Added
                                                      ACFRST,     & !Added
                                                      ACFRCV        !Added

   INTEGER,DIMENSION( ims:ime, jms:jme ),INTENT(INOUT)        ::  &
                                                          NCFRST, &  !Added
                                                          NCFRCV     !Added
! Perhaps should be optional (only used by CAM lw scheme)
   REAL, DIMENSION( ims:ime, kms:kme, kms:kme, jms:jme ),           &
         INTENT(INOUT)  ::                                  abstot
   REAL, DIMENSION( ims:ime, kms:kme, 4      , jms:jme ),           &
         INTENT(INOUT)  ::                                  absnxt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),           &
         INTENT(INOUT)  ::                                  emstot

!
! Optional 
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) ::                                 CLDFRA, &
                                                          concld     !convective cloud fraction
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL,                                                &
         INTENT(OUT) ::                                   SWDOWNC
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN ) ::                                           &
                                                               pb &
                                               ,qv,qc,qr,qi,qs,qg

   LOGICAL, OPTIONAL ::             f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       taucldi,taucldc

!  REAL, DIMENSION( ims:ime, jms:jme, nisccp ),INTENT(OUT)          :: fq_isccp              ! isccp output
   REAL, DIMENSION( ims:ime, jms:jme ),INTENT(INOUT)                :: cptop,ctaucld,cttop   ! isccp output
 
! LOCAL  VAR

   REAL, DIMENSION( ims:ime, jms:jme ) ::             GLAT,GLON
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    CEMISS
   REAL, DIMENSION( ims:ime, jms:jme ) ::             coszr
!  REAL, DIMENSION( ims:ime, jms:jme ) ::             swcf,lwcf

   REAL    ::    XTIME,DECLIN,SOLCON 
   INTEGER ::    i,j,k,its,ite,jts,jte,ij,isccp_unit
   INTEGER ::    STEP6HR
   LOGICAL ::    gfdl_lw,gfdl_sw
   LOGICAL ::    doabsems
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC
!------------------------------------------------------------------

   if (lw_physics .eq. 0 .and. sw_physics .eq. 0)         return

!!!   IF (itimestep .eq. 1 .or. mod(itimestep,STEPRA) .eq. 0) THEN
   IF (mod(itimestep,STEPRA) .eq. 1) THEN
     STEP6HR = nint(21600./(dt*STEPRA))*STEPRA
!!!     IF (itimestep .eq. 1 .or. mod(itimestep,STEP6HR) .eq. 0) THEN
     IF (mod(itimestep,STEP6HR) .eq. 1) THEN
       doabsems = .true.
     ELSE
       doabsems = .false.
     ENDIF

   gfdl_lw = .false.
   gfdl_sw = .false.

!-- read in input data for isccp simulator
! This should go in an isccp module
                                                                                                                                                           
!!!   if (itimestep .eq. 1.or.restart) then
   if (invtau(-20) .ne. 1 .or. tautab(0) .gt. -99. ) then
print *,'reading isccp data'
      isccp_unit = 27
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(isccp_unit, FILE='tautab.formatted',FORM='FORMATTED',STATUS='OLD')
      ENDIF
      if ( wrf_dm_on_monitor() ) then
        do i = 0,255
        READ (isccp_unit,*)tautab(i)
        end do
      endif
      CALL wrf_dm_bcast_bytes (tautab,  512*RWORDSIZE )
      close(27)
                                                                                                                                                           
      IF ( wrf_dm_on_monitor() ) THEN
        OPEN(isccp_unit, FILE='invtau.formatted',FORM='FORMATTED',STATUS='OLD')
      ENDIF
      if ( wrf_dm_on_monitor() ) then
        do i = -20,45000
        READ (isccp_unit,*)invtau(i)
        end do
      endif
      CALL wrf_dm_bcast_bytes (invtau,  45021*RWORDSIZE )
      close(27)
   endif

!---------------
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

! initialize data

     DO j=jts,jte
     DO i=its,ite
        GSW(I,J)=0.
        GLW(I,J)=0.
        SWDOWN(I,J)=0.
        GLAT(I,J)=XLAT(I,J)*DEGRAD
        GLON(I,J)=XLONG(I,J)*DEGRAD
     ENDDO
     ENDDO

     DO j=jts,jte
     DO k=kts,kte+1
     DO i=its,ite
        RTHRATEN(I,K,J)=0.
        SWUP(I,K,J) = 0.0
        SWDN(I,K,J) = 0.0
        SWUPCLEAR(I,K,J) = 0.0
        SWDNCLEAR(I,K,J) = 0.0
        LWUP(I,K,J) = 0.0
        LWDN(I,K,J) = 0.0
        LWUPCLEAR(I,K,J) = 0.0
        LWDNCLEAR(I,K,J) = 0.0
        CEMISS(I,K,J)=0.0
     ENDDO
     ENDDO
     ENDDO

!---------------
! Calculate constant for short wave radiation

     CALL radconst(XTIME,DECLIN,SOLCON,GMT,JULDAY,JULIAN,    &
                   DEGRAD,DPD,itimestep,dt                   )

     lwrad_gfdl_select: SELECT CASE(lw_physics)

        CASE (GFDLLWSCHEME)

!-- Do nothing, since cloud fractions (with partial cloudiness effects) 
!-- are defined in GFDL LW/SW schemes and do not need to be initialized.

        CASE DEFAULT

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
!      CALL cal_cldfra(CLDFRA,qc,qi,F_QC,F_QI,               &
!                      ids,ide, jds,jde, kds,kde,            &
!                      ims,ime, jms,jme, kms,kme,            &
!                      its,ite, jts,jte, kts,kte             )

! Call to cloud fraction routine based on Randall 1994 (Hong Pan 1998)
                                                                                                                                                      
   CALL cal_cldfra(CLDFRA,qv,qc,qi,                        &
                   F_QV,F_QC,F_QI,t,p,                     &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF

     END SELECT lwrad_gfdl_select    

!pjj/cray  Cray X1 cannot print from threaded region
#ifndef crayx1
     WRITE(wrf_err_message,*)'SOLCON=',SOLCON,DECLIN,XTIME
     CALL wrf_debug(50,wrf_err_message)
#endif

     lwrad_select: SELECT CASE(lw_physics)

        CASE (RRTMSCHEME)
             CALL wrf_debug (100, 'CALL rrtm')

             CALL RRTMLWRAD(                                        &
                  RTHRATEN=RTHRATEN,LWUP=LWUP,LWDN=LWDN             &
                 ,LWUPCLEAR=LWUPCLEAR,LWDNCLEAR=LWDNCLEAR           &
                 ,GLW=GLW,LWCF=LWCF,OLR=OLR,EMISS=EMISS,CEMISS=CEMISS       &
                 ,QV3D=QV                                           &
                 ,QC3D=QC                                           &
                 ,QR3D=QR                                           &
                 ,QI3D=QI                                           &
                 ,QS3D=QS                                           &
                 ,QG3D=QG                                           &
                 ,P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,T3D=t             &
                 ,T8W=t8w,RHO3D=rho, CLDFRA3D=CLDFRA,R=R_d,G=G      &
                 ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR                     &
                 ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG                     &
                 ,ICLOUD=icloud,WARM_RAIN=warm_rain                 &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )

        CASE (GFDLLWSCHEME)

             CALL wrf_debug (100, 'CALL gfdllw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND. PRESENT(F_QI) .AND. &
                  PRESENT(qv)   .AND. PRESENT(qc)   .AND. PRESENT(qi)  ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QI ) THEN
                 gfdl_lw  = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc,QI=qi                                 &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal('Can not call ETARA (1a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal('Can not call ETARA (1b). Missing moisture fields.')
             ENDIF
        CASE (CAMLWSCHEME)
             CALL wrf_debug(100, 'CALL camrad lw')
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     SWUP=SWUP,SWUPCLEAR=SWUPCLEAR,SWDN=SWDN,SWDNCLEAR=SWDNCLEAR,&
                     LWUP=LWUP,LWUPCLEAR=LWUPCLEAR,LWDN=LWDN,LWDNCLEAR=LWDNCLEAR,&
                     SWCF=SWCF,LWCF=LWCF,OLR=OLR,CEMISS=CEMISS,TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,  &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,f_ice_phy=f_ice_phy,f_rain_phy=f_rain_phy         &
                    ,p_phy=p,p8w=p8w,z=z,pi_phy=pi,rho_phy=rho,        &
                     dz8w=dz8w,znu=znu,znw=znw,                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
        CASE DEFAULT
  
             WRITE( wrf_err_message , * ) 'The longwave option does not exist: lw_physics = ', lw_physics
             CALL wrf_error_fatal ( wrf_err_message )
           
     END SELECT lwrad_select    

     IF (lw_physics .gt. 0 ) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENLW(I,K,J)=RTHRATEN(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     ENDIF
!

     swrad_select: SELECT CASE(sw_physics)

        CASE (SWRADSCHEME)
             CALL wrf_debug(100, 'CALL swrad')
             CALL SWRAD(                                               &
                     DT=dt,RTHRATEN=rthraten,GSW=gsw                   &
                    ,XLAT=xlat,XLONG=xlong,ALBEDO=albedo               &
                    ,AER_DRY=aer_dry,AER_WATER=aer_water               &
                    ,RHO_PHY=rho,T3D=t                                 &
                    ,P3D=p,PI3D=pi,DZ8W=dz8w,GMT=gmt                   &
                    ,R=r_d,CP=cp,G=g,JULDAY=julday                     &
                    ,XTIME=xtime,DECLIN=declin,SOLCON=solcon           &
                    ,RADFRQ=radt,ICLOUD=icloud,DEGRAD=degrad           &
                    ,warm_rain=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                                                                       )

        CASE (GSFCSWSCHEME)
             CALL wrf_debug(100, 'CALL gsfcswrad')
             CALL GSFCSWRAD(                                           &
                     RTHRATEN=rthraten,SWUP=swup,SWDN=swdn             &
                    ,SWUPCLEAR=swupclear,SWDNCLEAR=swdnclear           &
                    ,GSW=gsw,SWCF=swcf,XLAT=xlat,XLONG=xlong           &
                    ,ALB=albedo,T3D=t,P3D=p,P8W3D=p8w,pi3D=pi          &
                    ,DZ8W=dz8w,RHO_PHY=rho                             &
                    ,CLDFRA3D=cldfra                                   &
                    ,GMT=gmt,CP=cp,G=g                                 &
                    ,JULDAY=julday,XTIME=xtime                         &
                    ,DECLIN=declin,SOLCON=solcon                       &
                    ,RADFRQ=radt,DEGRAD=degrad                         &
                    ,COSZR=coszr                                       &
                    ,TAUCLDI=taucldi,TAUCLDC=taucldc                   &
                    ,WARM_RAIN=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                                                                       )
        CASE (CAMSWSCHEME)
! Temporarily lw switch already calculates sw CAM tendency, so inactive here

      DO j=jts,jte
      DO k=kts,kte
      DO i=its,ite
         RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
      ENDDO
      ENDDO
      ENDDO

        CASE (GFDLSWSCHEME)

             CALL wrf_debug (100, 'CALL gfdlsw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND. PRESENT(F_QI) .AND. &
                  PRESENT(qv)   .AND. PRESENT(qc)   .AND. PRESENT(qi)  ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QI ) THEN
                 gfdl_sw = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc,QI=qi                                 &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,JULYR=julyr,JULDAY=julday                         &
                 ,GFDL_LW=gfdl_lw,GFDL_SW=gfdl_sw                   &
                 ,CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach         &
                 ,ACFRST=acfrst,NCFRST=ncfrst                       &
                 ,ACFRCV=acfrcv,NCFRCV=ncfrcv                       &
                 ,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean         &
                 ,THRATEN=rthraten,THRATENLW=rthratenlw             &
                 ,THRATENSW=rthratensw                              &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
               ELSE
                 CALL wrf_error_fatal('Can not call ETARA (2a). Missing moisture fields.')
               ENDIF
             ELSE
               CALL wrf_error_fatal('Can not call ETARA (2b). Missing moisture fields.')
             ENDIF

        CASE DEFAULT

             WRITE( wrf_err_message , * ) 'The shortwave option does not exist: sw_physics = ', sw_physics
             CALL wrf_error_fatal ( wrf_err_message )

     END SELECT swrad_select    

     IF (sw_physics .gt. 0) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENSW(I,K,J)=RTHRATEN(I,K,J)-RTHRATENLW(I,K,J)
        ENDDO
        ENDDO
        ENDDO

        DO j=jts,jte
        DO i=its,ite
           SWDOWN(I,J)=GSW(I,J)/(1.-ALBEDO(I,J))
        ENDDO
        ENDDO

     ENDIF

! call isccp simulator

! set convective cloud fraction to zero (may be calculated by convection schemes)

        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           concld(I,K,J)=0.0
        ENDDO
        ENDDO
        ENDDO

       call ccm_isccp(p,p8w,t,qv,tsk,coszr,cldfra,concld,             &
                      emiss,cemiss,taucldi,taucldc,cptop,ctaucld,cttop,&
                         ids,ide, jds,jde, kds,kde,                   &
                         ims,ime, jms,jme, kms,kme,                   &
                         its,ite, jts,jte, kts,kte                    )

   ENDDO
   !$OMP END PARALLEL DO

   ENDIF

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACSWUPT(I,J) = ACSWUPT(I,J) + SWUP(I,KTE+1,J)*DT
! if(i.eq.100.and.j.eq.30)print *,'swup ',i,j,kte+1,SWUP(I,KTE+1,J),DT,ACSWUPT(I,J)
           ACSWUPTC(I,J) = ACSWUPTC(I,J) + SWUPCLEAR(I,KTE+1,J)*DT
           ACSWDNT(I,J) = ACSWDNT(I,J) + SWDN(I,KTE+1,J)*DT
           ACSWDNTC(I,J) = ACSWDNTC(I,J) + SWDNCLEAR(I,KTE+1,J)*DT
           ACLWUPT(I,J) = ACLWUPT(I,J) + LWUP(I,KTE+1,J)*DT
           ACLWUPTC(I,J) = ACLWUPTC(I,J) + LWUPCLEAR(I,KTE+1,J)*DT
           ACLWDNT(I,J) = ACLWDNT(I,J) + LWDN(I,KTE+1,J)*DT
           ACLWDNTC(I,J) = ACLWDNTC(I,J) + LWDNCLEAR(I,KTE+1,J)*DT
           ACSWUPB(I,J) = ACSWUPB(I,J) + SWUP(I,KTS,J)*DT
           ACSWUPBC(I,J) = ACSWUPBC(I,J) + SWUPCLEAR(I,KTS,J)*DT
           ACSWDNB(I,J) = ACSWDNB(I,J) + SWDN(I,KTS,J)*DT
           ACSWDNBC(I,J) = ACSWDNBC(I,J) + SWDNCLEAR(I,KTS,J)*DT
           ACLWUPB(I,J) = ACLWUPB(I,J) + LWUP(I,KTS,J)*DT
           ACLWUPBC(I,J) = ACLWUPBC(I,J) + LWUPCLEAR(I,KTS,J)*DT
           ACLWDNB(I,J) = ACLWDNB(I,J) + LWDN(I,KTS,J)*DT
           ACLWDNBC(I,J) = ACLWDNBC(I,J) + LWDNCLEAR(I,KTS,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
 
   END SUBROUTINE radiation_driver

!---------------------------------------------------------------------
!BOP
! !IROUTINE: radconst - compute radiation terms
! !INTERFAC:
   SUBROUTINE radconst(XTIME,DECLIN,SOLCON,GMT,JULDAY,JULIAN_in,        &
                       DEGRAD,DPD,step,dt                            )
!---------------------------------------------------------------------
   USE module_wrf_error
   IMPLICIT NONE
!---------------------------------------------------------------------

! !ARGUMENTS:
   INTEGER, INTENT(IN   )   ::       JULDAY, step
   REAL, INTENT(IN   )      ::       GMT,dt,DEGRAD,DPD
   REAL, INTENT(IN   )      ::       JULIAN_in
   REAL, INTENT(OUT  )      ::       XTIME,DECLIN,SOLCON
   REAL                     ::       OBECL,SINOB,SXLONG,ARG,oldJULIAN,  &
                                     DECDEG,DJUL,RJUL,ECCFAC,JULIAN
!
! !DESCRIPTION:
! Compute terms used in radiation physics 
!EOP

! for short wave radiation

   DECLIN=0.
   SOLCON=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.
        
   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)
   XTIME=float(step)*dt/60.
        
!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:
        
   JULIAN = (JULIAN_in - 1.) + (dt/(60.*60.*24.))    ! offset by one timestep
   oldJULIAN=FLOAT(JULDAY-1)+(XTIME/60.+GMT)/24.
   IF ( oldJULIAN .NE. JULIAN ) THEN
     WRITE( wrf_err_message , * ) 'radiation_driver:  JULIAN DATE MISMATCH:  JULIAN = ',JULIAN, &
       '  oldJULIAN,JULDAY,XTIME,GMT,step,dt = ',oldJULIAN,JULDAY,XTIME,GMT,step,dt
     CALL wrf_debug ( 50, TRIM(wrf_err_message) )
   ENDIF
!$$$DEBUG
!JULIAN = oldJULIAN
!$$$END DEBUG
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
   
!pjj/cray  Cray X1 cannot print from threaded region
#ifndef crayx1
   write(wrf_err_message,10)DECDEG,SOLCON
10 FORMAT(1X,'*** SOLAR DECLINATION ANGLE = ',F6.2,' DEGREES.',     &
        ' SOLAR CONSTANT = ',F8.2,' W/M**2 ***')
   CALL wrf_debug (50, wrf_err_message)
#endif

   END SUBROUTINE radconst

!---------------------------------------------------------------------
!BOP
! !IROUTINE: cal_cldfra - Compute cloud fraction
! !INTERFACE:
!  SUBROUTINE cal_cldfra(CLDFRA,QC,QI,F_QC,F_QI,                     &
! cal_cldfra_xr - Compute cloud fraction.
! Code adapted from that in module_ra_gfdleta.F in WRF_v2.0.3 by James Done
!!
!!---  Cloud fraction parameterization follows Randall, 1994
!!     (see Hong et al., 1998)
!!     (modified by Ferrier, Feb '02)
!
   SUBROUTINE cal_cldfra(CLDFRA, QV, QC, QI,                         &
                         F_QV, F_QC, F_QI, t_phy, p_phy,             &
                         F_ICE_PHY,F_RAIN_PHY,                       &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte

!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QV, &
                                                                 QI, &
                                                                 QC, &
                                                              t_phy, &
                                                              p_phy, &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   LOGICAL,INTENT(IN) :: F_QC,F_QI,F_QV

!  REAL thresh
   INTEGER:: i,j,k
   REAL    :: RHUM, tc, esw, esi, weight, qvsw, qvsi, qvs_weight, QIMID, QWMID, QCLD, DENOM, ARG, SUBSAT
                                                                                                                                                 
   REAL    ,PARAMETER :: ALPHA0=100., GAMMA=0.49, QCLDMIN=1.E-12,    &
                                        PEXP=0.25, RHGRID=1.0
   REAL    , PARAMETER ::  SVP1=0.61078
   REAL    , PARAMETER ::  SVP2=17.2693882
   REAL    , PARAMETER ::  SVPI2=21.8745584
   REAL    , PARAMETER ::  SVP3=35.86
   REAL    , PARAMETER ::  SVPI3=7.66
   REAL    , PARAMETER ::  SVPT0=273.15
   REAL    , PARAMETER ::  r_d = 287.
   REAL    , PARAMETER ::  r_v = 461.6
   REAL    , PARAMETER ::  ep_2=r_d/r_v
! !DESCRIPTION:
! Compute cloud fraction from input ice and cloud water fields
! if provided.
!
! Whether QI or QC is active or not is determined from the indices of
! the fields into the 4D scalar arrays in WRF. These indices are 
! P_QI and P_QC, respectively, and they are passed in to the routine
! to enable testing to see if QI and QC represent active fields in
! the moisture 4D scalar array carried by WRF.
! 
! If a field is active its index will have a value greater than or
! equal to PARAM_FIRST_SCALAR, which is also an input argument to 
! this routine.
!EOP
!---------------------------------------------------------------------
                                                                                                                                                      
                                                                                                                                                      
!-----------------------------------------------------------------------
!---  COMPUTE GRID-SCALE CLOUD COVER FOR RADIATION
!     (modified by Ferrier, Feb '02)
!
!---  Cloud fraction parameterization follows Randall, 1994
!     (see Hong et al., 1998)
!-----------------------------------------------------------------------
! Note: ep_2=287./461.6 Rd/Rv
! Note: R_D=287.
                                                                                                                                                      
! Alternative calculation for critical RH for grid saturation
!     RHGRID=0.90+.08*((100.-DX)/95.)**.5
                                                                                                                                                      
! Calculate saturation mixing ratio weighted according to the fractions of
! water and ice. 
! Following:
! Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure''  J. Appl. Meteor.  6 p.204
!    es (in mb) = 6.1078 · exp[ a · (T-273.16)/ (T-b) ]
!
!       over ice        over water
! a = 	21.8745584	17.2693882
! b = 	7.66	        35.86

    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      tc         = t_phy(i,k,j) - SVPT0
      esw     = 1000.0 * SVP1 * EXP( SVP2  * tc / ( t_phy(i,k,j) - SVP3  ) )
      esi     = 1000.0 * SVP1 * EXP( SVPI2 * tc / ( t_phy(i,k,j) - SVPI3 ) )
      QVSW = EP_2 * esw / ( p_phy(i,k,j) - esw )
      QVSI = EP_2 * esi / ( p_phy(i,k,j) - esi )

      IF ( F_QI .and. F_QC ) THEN
        QCLD=QI(i,k,j)+QC(i,k,j)
        IF (QCLD .LT. QCLDMIN) THEN
          weight = 0.
        ELSE
          weight = QI(i,k,j) / QCLD 
        ENDIF
      ELSE IF ( F_QC ) THEN
 
! Mixing ratios of cloud water & total ice (cloud ice + snow).
! Mixing ratios of rain are not considered in this scheme.
! F_ICE is fraction of ice
! F_RAIN is fraction of rain

      QIMID=QC(i,k,j)*F_ICE_PHY(i,k,j)
      QWMID=(QC(i,k,j)-QIMID)*(1.-F_RAIN_PHY(i,k,j))

!
!--- Total "cloud" mixing ratio, QCLD.  Rain is not part of cloud,
!    only cloud water + cloud ice + snow
!
      QCLD=QWMID+QIMID
        IF (QCLD .LT. QCLDMIN) THEN
          weight = 0.
        ELSE
          weight = F_ICE_PHY(i,k,j)
        ENDIF

      ELSE
        CLDFRA(i,k,j)=0.
      ENDIF !  IF ( F_QI .and. F_QC )


      QVS_WEIGHT = (1-weight)*QVSW + weight*QVSI
      RHUM=QV(i,k,j)/QVS_WEIGHT   !--- Relative humidity
!
!--- Determine cloud fraction (modified from original algorithm)
!
      IF (QCLD .LT. QCLDMIN) THEN
!
!--- Assume zero cloud fraction if there is no cloud mixing ratio
!
        CLDFRA(i,k,j)=0.
      ELSEIF(RHUM.GE.RHGRID)THEN
!
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
!
        CLDFRA(i,k,j)=1.
      ELSE
!
!--- Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
!
        SUBSAT=MAX(1.E-10,RHGRID*QVS_WEIGHT-QV(i,k,j))
        DENOM=(SUBSAT)**GAMMA
        ARG=MAX(-6.9, -ALPHA0*QCLD/DENOM)    ! <-- EXP(-6.9)=.001
! prevent negative values  (new)
        RHUM=MAX(1.E-10, RHUM)
        CLDFRA(i,k,j)=(RHUM/RHGRID)**PEXP*(1.-EXP(ARG))
!!              ARG=-1000*QCLD/(RHUM-RHGRID)
!!              ARG=MAX(ARG, ARGMIN)
!!              CLDFRA(i,k,j)=(RHUM/RHGRID)*(1.-EXP(ARG))
        IF (CLDFRA(i,k,j) .LT. .01) CLDFRA(i,k,j)=0.
      ENDIF          !--- End IF (QCLD .LT. QCLDMIN) ...
    ENDDO          !--- End DO i
    ENDDO          !--- End DO k
    ENDDO          !--- End DO j

   END SUBROUTINE cal_cldfra

! This should go in an isccp module
!  subroutine ccm_isccp (lchnk, ncol, pmid, pint, q, t, ts, concld,   &
!          cld, cliqwp, cicewp, rel, rei, emis, cltot, coszrs)

!  need to add convective cloud cover

   subroutine ccm_isccp(p,pmid,t,qv,tsk,cosz,cldfra,concld,          &
                        emiss,cemiss,taucldi,taucldc,cptop,ctaucld,cttop,          &
                        ids,ide, jds,jde, kds,kde,                   &
                        ims,ime, jms,jme, kms,kme,                   &
                        its,ite, jts,jte, kts,kte                    )

                                                                                                                                                           
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte
!  INTEGER,  INTENT(IN   )   ::           nisccp
                                                                                                                                                           
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) ::        emiss, &
                                                                tsk, cosz
                                                                                                                                                           
                                                                                                                                                           
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                             CLDFRA, &
                                                             concld, &
                                                                 QV, &
                                                             cemiss, &
                                                            taucldi, & 
                                                            taucldc, & 
                                                                  t, &
                                                                  p, &
                                                               pmid
                                                                                                                                                           
!  REAL,  DIMENSION( ims:ime, jms:jme, nisccp ), INTENT(INOUT   ) :: fq_isccp
   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(INOUT   )     :: cptop, ctaucld, cttop

                                                                                                                                                           
! Local variables:
                                                                                                                                                           
   REAL(r8), DIMENSION(kms:kme-1)                                :: t1d, & ! temperature
                                                                    q1d, & ! specific humidity
                                                                  pfull, & ! in pascals
                                                                  cld1d, & ! cloud fraction
                                                               concld1d, & ! convective cloud fraction
                                                                  tau1d, & ! cloud optical depth
                                                                 emis1d    ! cloud emissivity
   REAL(r8), DIMENSION(kms:kme)                                  :: phalf
   REAL(r8), DIMENSION(ntau,npres)                                :: fq_isccp_1d
   REAL(r8) emsfc_lw,ts,gcldarea,gcptop,gctaucld,gcttop
                                                                                                                                                           
   INTEGER mkx,top_height,overlap,seed,ibox,itau,it,ip,itp,i,j,k,kk
   REAL thresh,fillvalue
!   REAL cldcol,cldmin
   REAL cldmin
   REAL(r8) cldcol

!
!  real(r8)    emsfc_lw   !emsfc_lw    longwave emissivity of surface at 10.5 microns
   integer nsubcol!nsubcol  # of columns each grid box is subdivided into
!  PARAMETER(nsubcol=50,emsfc_lw=0.99)
   PARAMETER(nsubcol=50)
!  real(r8), intent(in) :: concld  (pcols,pver)  !convective cloud cover
!  real(r8), intent(in) :: cld  (pcols,pver)  !cloud cover
!  real(r8), intent(in) :: cliqwp(pcols,pver) ! in-cloud liquid water path
!  real(r8), intent(in) :: cicewp(pcols,pver) ! in-cloud ice water path
!  real(r8), intent(in) :: rel(pcols,pver)   ! Liquid cloud particle effective radius
!  real(r8), intent(in) :: rei(pcols,pver)   ! Ice effective drop size (microns)
!  real(r8), intent(in) :: emis(pcols,pver)  ! Cloud longwave emissivity
!  real(r8), intent(in) :: cltot(pcols)  ! Total cloud amount
!  real(r8), intent(in) :: coszrs(pcols)  ! cosine solar zenith angle (to tell if day or night)
                                                                                                                                                           
!  real(r8)  fq_isccp_s1(pcols,ntau*npres)  !accumulated fq_isccp
                                                                                                                                                           
!  real(r8) tau(pcols,pver)
!  real(r8) dtau_s(pver),dtau_c(pver), dem_s(pver),dem_c(pver)
!  real(r8) totalcldarea(pcols), meantaucld(pcols), meanptop(pcols), meanttop(pcols), cloudy(pcols)
                                                                                                                                                           
!  integer itau,top_height,overlap,ibox,seed
!  integer i,k
!  REAL(r8) fq_isccp(ntau,npres)
   real(r8) boxtau(nsubcol), boxptop(nsubcol)
!  real abarl         ! A coefficient for extinction optical depth
!  real bbarl         ! b coefficient for extinction optical depth
!  real abari         ! A coefficient for extinction optical depth
!  real bbari         ! b coefficient for extinction optical depth
!  real(r8) cldmin    ! the value taken from radcswmx.F90, note: cldmin much less than cldmin from what on cldnrh
!  real(r8) cldeps    ! the value taken from radcswmx.F90
!  real(r8) pmid1(pver), pint1(pverp), t1(pver), q1(pver), ts1,  &
!           cld1(pver), concld1(pver),sumf
!  integer itaunpres,it,ip
   character*1 cc
!
!JR Fake fields added for code checking
!
!  real(r8) :: fakefld1(pcols)
!  real(r8) :: fakefld2(pcols)
                                                                                                                                                           
!  call t_startf ('ccm_isccp')
                                                                                                                                                           
!  cldmin = 1.0e-80_r8
!  cldeps = 0.0_r8
                                                                                                                                                           
!  abarl = 2.817e-02
!  bbarl = 1.305
!  abari = 3.448e-03
!  bbari = 2.431

   !compute the optical depth

!   do k=1,pver
!      do i=1,ncol
!         if(cld(i,k) >= cldmin .and. cld(i,k) >= cldeps) then
!            tau(i,k) = (abarl + bbarl/rel(i,k)) * cliqwp(i,k) + &
!                       (abari + bbari/rei(i,k)) * cicewp(i,k)
!         else
!            tau(i,k) = 0.0
!         endif
!      end do
!   end do

! isccp vertical levels go from 1 at the top
! wrf vertical levels go from 1 at the surface

   mkx=kte-kts+1
   top_height=1
   overlap=3
   thresh=1.0e-6
   cldmin=1.0e-6
   fillvalue=-9999.0

!
!JR Add coszrs logic to only call the simulator during daytime
!
   DO J=jts,jte
   DO I=its,ite
      DO K=kts,kte
         kk=kte-k+kts
         t1d(K)=t(I,KK,J)
if(t1d(k).lt.10.)print *,t1d(k),i,j,k,' cold t1d'
         q1d(K)=qv(I,KK,J)/(1.+qv(I,KK,J))
         pfull(K)=p(I,KK,J)
         phalf(K)=pmid(I,KK+1,J)
         cld1d(K)=cldfra(I,KK,J)
         concld1d(K)=concld(I,KK,J)
         tau1d(K)=taucldi(I,KK,J)+taucldc(I,KK,J)   
        emis1d(K)=cemiss(I,KK,J)
      ENDDO
         phalf(kte+1)=pmid(I,1,J)

       cldcol=0.
       do k=kts,kte
          cldcol=max(cldcol,cld1d(k))
       end do
               ts=tsk(I,J)
         emsfc_lw=emiss(I,J)
  
      if(cosz(i,j).ge.thresh) then    ! if daytime

         if(cldcol.gt.cldmin) then    !if cloudy

!           cloudy = 1.0       !cloudy flag, which may be used to derived mean values under cloudy
!                              !conditions. The cloud flag itself is freuency of cloudy condition
!                              !when average over an accumulation period.
            seed=(phalf(k)-int(phalf(k)))*100+1
            call ISCCP_CLOUD_TYPES(mkx,nsubcol,seed,pfull,phalf,q1d,cld1d,concld1d,  &
                                   tau1d,tau1d,top_height,overlap, ts,emsfc_lw, &
                                   t1d,emis1d, emis1d,fq_isccp_1d,gcldarea,       &
                                   gcptop, gctaucld,boxtau,boxptop,gcttop)
                                                                                                                                                           
!save standard ISCCP type of 7x7 clouds
!           sumf=0
!           do ip=1,npres
!              do it=1,ntau
!                 itp = (ip-1)*ntau+it
!                 fq_isccp(i,j,itp) = fq_isccp_1d(it,ip)
!                 sumf=sumf+fq_isccp_1d(it,ip)
!              end do
!           end do
                  cptop(i,j)=gcptop
                  ctaucld(i,j)=gctaucld
                  cttop(i,j)=gcttop
!           if(sumf<-0.9)then
!              print *,'sum of fq_isccp<0.9 in ccm_isccp',sumf
!              print *,'totalcldarea',totalcldarea(i)
!              do ip=1,7
!                 write(6,'(a,i,10f8.5)')'ip=',ip,(fq_isccp(it,ip),it=1,ntau)
!              enddo
!              write(6,*)'cloud fraction'
!              write(6,'(12f8.5)')(cld(i,k),k=1,pver)
!              write(6,*)'tau'
!              write(6,'(12f8.5)')(tau(i,k),k=1,pver)
!           endif
! extra index:
!     23-> totalcldarea
!     24-> meanptop
!     25-> meantaucld
!     26-> meanttop
!     27-> cloudy flag
!     print *,lchnk,itaunpres-pverp+1   != 23
!JR got rid of this and wrote them out as separate fields
!           fakefld1(i)      = 1.
!           fakefld2(i)      = 1.
         else                             !cloud free in the (daytime) grid box
!           do itp=1,nisccp
!           fq_isccp(i,j,itp) = 0.
!           end do
            cptop(I,J)=0.
            ctaucld(I,J)=0.
            cttop(I,J)=0.
!           cloudy(i,j)= 0.
!           fakefld1(i)= 0.
!           fakefld2(i)= fillvalue
         endif
      else                                ! nighttime
!        do itp=1,nisccp
!        fq_isccp(i,j,itp) = fillvalue
!        end do
         cptop(I,J)=fillvalue
         ctaucld(I,J)=fillvalue
         cttop(I,J)=fillvalue
!        cloudy(i,j)= fillvalue
!        fakefld1(i,j)      = fillvalue
!        fakefld2(i,j)      = fillvalue
      end if

   END DO    ! i_loop

   END DO    ! j_loop

!  call t_stopf ('ccm_isccp')

   return
   end subroutine ccm_isccp

                                                                                                                                                           
!#######################################################################
                                                                                                                                                           
subroutine isccp_cloud_types(nlev,ncol,seed,pfull,phalf,qv,       &
           cc,conv,dtau_s,dtau_c,top_height,overlap,              &
           skt,emsfc_lw,at,dem_s,dem_c,fq_isccp,                  &
           totalcldarea,meanptop,meantaucld,boxtau,boxptop,meanttop)
                                                                                                                                                           
!$Id$
                                                                                                                                                           
! Copyright Steve Klein and Mark Webb 2002 - all rights reserved.
!
! This code is available without charge with the following conditions:
!
!  1. The code is available for scientific purposes and is not for
!     commercial use.
!  2. Any improvements you make to the code should be made available
!     to the authors for incorporation into a future release.
!  3. The code should not be used in any way that brings the authors
!     or their employers into disrepute.
                                                                                                                                                           
                                                                                                                                                           
!     NOTE:   the maximum number of levels and columns is set by
!             the following parameter statement
                                                                                                                                                           
      INTEGER ncolmax,nlevmax,ncolprint
                                                                                                                                                           
!     parameter(ncolmax=100,nlevmax=pver)
      parameter(ncolmax=100)
                                                                                                                                                           
!     -----
!     Input
!     -----
                                                                                                                                                           
      INTEGER nlev                      !  number of model levels in column
      INTEGER ncol                      !  number of subcolumns
                                                                                                                                                           
      INTEGER seed                      !  seed value for random number generator
                                        !  ( see Numerical Recipes Chapter 7)
                                        !  It is recommended that the seed is set
                                        !  to a different value for each model
                                        !  gridbox it is called on, as it is
                                        !  possible that the choice of the same
                                        !  seed value every time may introduce some
                                        !  statistical bias in the results, particularly
                                        !  for low values of NCOL.
                                                                                                                                                           
      REAL(r8) pfull(nlev)              !  pressure of full model levels (Pascals)
                                        !  pfull(1)    is    top level of model
                                        !  pfull(nlev) is bottom level of model
                                                                                                                                                           
      REAL(r8) phalf(nlev+1)            !  pressure of half model levels (Pascals)
                                        !  phalf(1)    is    top       of model
                                        !  phalf(nlev+1) is the surface pressure
                                                                                                                                                           
      REAL(r8) qv(nlev)                 !  water vapor specific humidity (kg vapor/ kg air)
                                        !         on full model levels
                                                                                                                                                           
      REAL(r8) cc(nlev)                 !  input cloud cover in each model level (fraction)
                                        !  NOTE:  This is the HORIZONTAL area of each
                                        !         grid box covered by clouds
                                                                                                                                                           
      REAL(r8) conv(nlev)               !  input convective cloud cover in each model level (fraction)
                                        !  NOTE:  This is the HORIZONTAL area of each
                                        !         grid box covered by convective clouds
                                                                                                                                                           
      REAL(r8) dtau_s(nlev)             !  mean 0.67 micron optical depth of stratiform
                                        !  clouds in each model level
                                        !  NOTE:  this the cloud optical depth of only the
                                        !         cloudy part of the grid box, it is not weighted
                                        !         with the 0 cloud optical depth of the clear
                                        !         part of the grid box
                                                                                                                                                           
      REAL(r8) dtau_c(nlev)             !  mean 0.67 micron optical depth of convective
                                        !  clouds in each
                                        !  model level.  Same note applies as in dtau_s.
                                                                                                                                                           
      INTEGER overlap                   !  overlap type
                                        !  1=max
                                        !  2=rand
                                        !  3=max/rand
                                                                                                                                                           
      INTEGER top_height                !  1 = adjust top height using both a computed
                                        !  infrared brightness temperature and the visible
                                        !  optical depth to adjust cloud top pressure. Note
                                        !  that this calculation is most appropriate to compare
                                        !  to ISCCP data during sunlit hours.
                                        !  2 = do not adjust top height, that is cloud top
                                        !  pressure is the actual cloud top pressure
                                        !  in the model
                                        !  3 = adjust top height using only the computed
                                        !  infrared brightness temperature. Note that this
                                        !  calculation is most appropriate to compare to ISCCP
                                        !  IR only algortihm (i.e. you can compare to nighttime
                                        !  ISCCP data with this option)
                                                                                                                                                           
!
!     The following input variables are used only if top_height = 1 or top_height = 3
!
      REAL(r8) skt                      !  skin Temperature (K)
      REAL(r8) emsfc_lw                 !  10.5 micron emissivity of surface (fraction)
      REAL(r8) at(nlev)                 !  temperature in each model level (K)
      REAL(r8) dem_s(nlev)              !  10.5 micron longwave emissivity of stratiform
                                        !  clouds in each
                                        !  model level.  Same note applies as in dtau_s.
      REAL(r8) dem_c(nlev)              !  10.5 micron longwave emissivity of convective
                                        !  clouds in each
                                        !  model level.  Same note applies as in dtau_s.
!     ------
!     Output
!     ------
                                                                                                                                                           
      REAL(r8) fq_isccp(ntau,npres)     !  the fraction of the model grid box covered by
                                        !  each of the 49 ISCCP D level cloud types
                                                                                                                                                           
      REAL(r8) totalcldarea             !  the fraction of model grid box columns
                                        !  with cloud somewhere in them.  This should
                                        !  equal the sum over all entries of fq_isccp
                                                                                                                                                           
                                                                                                                                                           
      ! The following three means are averages over the cloudy areas only.  If no
      ! clouds are in grid box all three quantities should equal zero.
                                                                                                                                                           
      REAL(r8) meanptop                 !  mean cloud top pressure (mb) - linear averaging
                                        !  in cloud top pressure.
      REAL(r8) meanttop                 !  mean cloud top temp (k) - linear averaging
                                                                                                                                                           
      REAL(r8) meantaucld               !  mean optical thickness (dimensionless)
                                        !  linear averaging in albedo performed.
                                                                                                                                                           
                                                                                                                                                           
      REAL(r8) boxtau(ncol)             !  optical thickness in each column
                                                                                                                                                           
      REAL(r8) boxptop(ncol)            !  cloud top pressure (mb) in each column
                                                                                                                                                           
                                                                                                                                                           
!
!     ------
!     Working variables added when program updated to mimic Mark Webb's PV-Wave code
!     ------
                                                                                                                                                           
      REAL frac_out(ncolmax,nlev)       ! boxes gridbox divided up into
                                        ! Equivalent of BOX in original version, but
                                        ! indexed by column then row, rather than
                                        ! by row then column
                                                                                                                                                           
      REAL tca(ncolmax,0:nlev)          ! total cloud cover in each model level (fraction)
                                        ! with extra layer of zeroes on top
                                        ! in this version this just contains the values input
                                        ! from cc but replicated across ncol
      REAL cca(ncolmax,nlev)            ! convective cloud cover in each model level (fraction)
                                        ! from conv but replicated across ncol
                                                                                                                                                           
      REAL threshold(ncolmax)           ! pointer to position in gridbox
      REAL maxocc(ncolmax)              ! Flag for max overlapped conv cld
      REAL maxosc(ncolmax)              ! Flag for max overlapped strat cld
                                                                                                                                                           
      REAL boxpos(ncolmax)              ! ordered pointer to position in gridbox
                                                                                                                                                           
      REAL threshold_min(ncolmax)       ! minimum value to define range in with new threshold
                                        ! is chosen
                                                                                                                                                           
      REAL dem(ncolmax),bb              !  working variables for 10.5 micron longwave
                                        !  emissivity in part of
                                        !  gridbox under consideration
                                                                                                                                                           
!     REAL    ran0                      ! type for random number function
      REAL    dtautmp(ncolmax)          ! temporary variable for dtau of layer
      REAL ptrop,attrop,atmax,atmin,btcmin,transmax
      INTEGER ilev,ibox,itrop,ipres,itau,ilev2
      INTEGER acc(nlev,ncolmax)
      INTEGER match(nlev-1),nmatch,levmatch(ncolmax)
                                                                                                                                                           
      !variables needed for water vapor continuum absorption
      real fluxtop_clrsky,trans_layers_above_clrsky,taumin
      real dem_wv(nlev), wtmair, wtmh20, Navo, grav, pstd, t0
      real press, dpress, atmden, rvh20, wk, rhoave, rh20s, rfrgn
      real tmpexp,tauwv
                                                                                                                                                           
      character*1 cchar(6),cchar_realtops(6)
      integer icycle
      REAL tau(ncolmax),tb(ncolmax),ptop(ncolmax),emcld(ncolmax)
      real ttop(ncolmax)
      real fluxtop(ncolmax),trans_layers_above(ncolmax)
      real              fluxtopinit,tauir
      real meanalbedocld
      REAL albedocld(ncolmax)
      real boxarea

!JR   DATA isccp_taumin / 0.3 /
      DATA cchar / ' ','-','1','+','I','+'/
      DATA cchar_realtops / ' ',' ','1','1','I','I'/

   real(r8) :: prlim(npres+1) = (/0., 180., 310., 440., 560., 680., 800., 1000./)
!
!JR The commented out line is what the documentation says
!JR The uncomment line is what the code does
!   real(r8) :: taulim(ntau+1)  = (/0., 0.1, 1.3, 3.6, 9.4, 23., 60., 379./)
   real(r8) :: taulim(ntau+1)  = (/0., 0.3, 1.3, 3.6, 9.4, 23., 60., 379./)

!JR Test for validity of array dimensioning

      nlevmax = nlev

      if (ncol > ncolmax .or. nlev > nlevmax) then
         write(6,*)'ISCCP_CLOUD_TYPES: nlevmax and/or ncolmax too small'
         write(6,*)'nlev, nlevmax=', nlev, nlevmax
         write(6,*)'ncol, ncolmax=', ncol, ncolmax
!        call endrun ()
      end if
                                                                                                                                                           
!     call t_startf ('isccp_cloud_types')
                                                                                                                                                           
      ncolprint=0
                                                                                                                                                           
                                                                                                                                                           
!     assign 2d tca array using 1d input array cc
                                                                                                                                                           
      do ilev=0,nlev
        do ibox=1,ncol
          if (ilev.eq.0) then
            tca(ibox,ilev)=0
          else
            tca(ibox,ilev)=cc(ilev)
          endif
        enddo
      enddo
                                                                                                                                                           
!     assign 2d cca array using 1d input array conv
                                                                                                                                                           
      do ilev=1,nlev
        do ibox=1,ncol
          cca(ibox,ilev)=conv(ilev)
        enddo
      enddo
                                                                                                                                                           
      if (ncolprint.ne.0) then
        write (6,'(a)') 'seed:'
        write (6,'(I3.2)') seed
                                                                                                                                                           
        write (6,'(a)') 'tca_pp_rev:'
        write (6,'(8f5.2)')  &
         ((tca(ibox,ilev),ibox=1,ncolprint),ilev=1,nlev)
                                                                                                                                                           
        write (6,'(a)') 'cca_pp_rev:'
        write (6,'(8f5.2)')  &
         ((cca(ibox,ilev),ibox=1,ncolprint),ilev=1,nlev)
      endif
                                                                                                                                                           
      if (top_height .eq. 1 .or. top_height .eq. 3) then
                                                                                                                                                           
      ptrop=5000.
      atmin = 400.
      atmax = 0.
      attrop = at(1)
      do 12 ilev=1,nlev-1
           if ((pfull(ilev)/phalf(nlev+1)) .lt. 0.4 .and.  &
                at(ilev) .gt. at(ilev+1)) then
                ptrop = pfull(ilev+1)
                attrop = at(ilev+1)
                itrop=ilev+1
           end if
           if (at(ilev) .gt. atmax) atmax=at(ilev)
           if (at(ilev) .lt. atmin) atmin=at(ilev)
12    continue
      end if


!     find unpermittable data.....
!
      do 13 ilev=1,nlev
            if (cc(ilev) .lt. 0.) then
                write(6,*)  ' error = cloud fraction less than zero'
!                CALL ENDRUN ()
            else if (cc(ilev) .gt. 1.) then
                write(6,*)  ' error = cloud fraction greater than 1'
!                CALL ENDRUN ()
            end if
            if (conv(ilev) .lt. 0.) then
                write(6,*) &
                     ' error = convective cloud fraction less than zero'
!                CALL ENDRUN ()
            else if (conv(ilev) .gt. 1.) then
                WRITE(6,*)'CONV ilev =',conv,ilev
                CALL wrf_debug(50,6)
                write(6,*) &
                     ' error = convective cloud fraction greater than 1'
!                CALL ENDRUN ()
            end if
                                                                                                                                                           
            if (dtau_s(ilev) .lt. 0.) then
                write(6,*)   &
                 ' error = stratiform cloud opt. depth less than zero'
!                CALL ENDRUN ()
            end if
            if (dem_s(ilev) .lt. 0.) then
                write(6,*)  &
                 ' error = stratiform cloud emissivity less than zero'
!                CALL ENDRUN ()
            else if (dem_s(ilev) .gt. 1.) then
                write(6,*)  &
                 ' error = stratiform cloud emissivity greater than 1'
!                CALL ENDRUN ()
            end if
                                                                                                                                                           
            if (dtau_c(ilev) .lt. 0.) then
                write(6,*)  &
                 ' error = convective cloud opt. depth less than zero'
!                CALL ENDRUN ()
            end if
            if (dem_c(ilev) .lt. 0.) then
                write(6,*)  &
                 ' error = convective cloud emissivity less than zero'
!                CALL ENDRUN ()
            else if (dem_c(ilev) .gt. 1.) then
                write(6,*)  &
                 ' error = convective cloud emissivity greater than 1'
!                CALL ENDRUN ()
            end if
13    continue
                                                                                                                                                           
!     ---------------------------------------------------!
!     Initialise working variables
!     ---------------------------------------------------!
                                                                                                                                                           
!     Initialised frac_out to zero
                                                                                                                                                           
      do ibox=1,ncol
        do ilev=1,nlev
          frac_out(ibox,ilev)=0.0
        enddo
      enddo
                                                                                                                                                           
      if (ncolprint.ne.0) then
        write (6,'(a)') 'frac_out_pp_rev:'
        write (6,'(8f5.2)')  &
        ((frac_out(ibox,ilev),ibox=1,ncolprint),ilev=1,nlev)
                                                                                                                                                           
        write (6,'(a)') 'ncol:'
        write (6,'(I3)') ncol
      endif
                                                                                                                                                           
      do ibox=1,ncol
        boxpos(ibox)=(ibox-.5)/ncol
      enddo
                                                                                                                                                           
      if (ncolprint.ne.0) then
        write (6,'(a)') 'last_frac_pp:'
        write (6,'(8f5.2)') (tca(ibox,1),ibox=1,ncolprint)
      endif
                                                                                                                                                           
!     ---------------------------------------------------!
!     ALLOCATE CLOUD INTO BOXES, FOR NCOLUMNS, NLEVELS
!     frac_out is the array that contains the information
!     where 0 is no cloud, 1 is a stratiform cloud and 2 is a
!     convective cloud
                                                                                                                                                           
      !loop over vertical levels
      DO 200 ilev = 1,nlev
                                                                                                                                                           
!     Initialise threshold
                                                                                                                                                           
        IF (ilev.eq.1) then
          DO ibox=1,ncol
            ! If max overlap
            IF (overlap.eq.1) then
              ! select pixels spread evenly
              ! across the gridbox
              threshold(ibox)=boxpos(ibox)
            ELSE
              ! select random pixels from the non-convective
              ! part the gridbox ( some will be converted into
              ! convective pixels below )
              threshold(ibox)=   &
                 cca(ibox,ilev)+(1-cca(ibox,ilev))*ran0(seed)
            ENDIF
          ENDDO
          if(ncolprint .ne. 0) then
             write (6,'(a)') 'threshold_nsf2:'
             write (6,'(8f5.2)') (threshold(ibox),ibox=1,ncolprint)
          endif
        ENDIF
                                                                                                                                                           
        IF (ncolprint.ne.0) then
            write (6,'(a)') 'ilev:'
            write (6,'(I2)') ilev
        ENDIF
                                                                                                                                                           
        DO ibox=1,ncol
                                                                                                                                                           
          ! All versions
          if (boxpos(ibox).le.cca(ibox,ilev)) then
              maxocc(ibox) = 1
          else
              maxocc(ibox) = 0
          end if
                                                                                                                                                           
          ! Max overlap
          if (overlap.eq.1) then
            threshold_min(ibox)=cca(ibox,ilev)
            maxosc(ibox)=1
          endif
                                                                                                                                                           
          ! Random overlap
          if (overlap.eq.2) then
            threshold_min(ibox)=cca(ibox,ilev)
            maxosc(ibox)=0
          endif
                                                                                                                                                           
          ! Max/Random overlap
          if (overlap.eq.3) then
            threshold_min(ibox)=max(cca(ibox,ilev),   &
               min(tca(ibox,ilev-1),tca(ibox,ilev)))
            if (threshold(ibox).lt.min(tca(ibox,ilev-1),tca(ibox,ilev))  &
                  .and.(threshold(ibox).gt.cca(ibox,ilev))) then
                   maxosc(ibox)= 1
            else
                   maxosc(ibox)= 0
            end if
                                                                                                                                                           
          endif
                                                                                                                                                           
          ! Reset threshold
                                                                                                                                                           
          threshold(ibox)=                                        &
              !if max overlapped conv cloud
              maxocc(ibox) * (                                    &
                  boxpos(ibox)                                    &
              ) +                                                 &
              !else
              (1-maxocc(ibox)) * (                                &
                  !if max overlapped strat cloud
                  (maxosc(ibox)) * (                              &
                      !threshold=boxpos
                      threshold(ibox)                             &
                  ) +                                             &
                  !else
                  (1-maxosc(ibox)) * (                            &
                      !threshold_min=random[thrmin,1]
                      threshold_min(ibox)+                        &
                        (1-threshold_min(ibox))*ran0(seed)        &
                 )                                                &
              )
                                                                                                                                                           
           ENDDO
                                                                                                                                                           
!          Fill frac_out with 1's where tca is greater than the threshold
                                                                                                                                                           
           DO ibox=1,ncol
               if (tca(ibox,ilev).gt.threshold(ibox)) then
               frac_out(ibox,ilev)=1
               else
               frac_out(ibox,ilev)=0
               end if
           ENDDO
                                                                                                                                                           
!          Code to partition boxes into startiform and convective parts
!          goes here
                                                                                                                                                           
           DO ibox=1,ncol
                if (threshold(ibox).le.cca(ibox,ilev)) then
                    ! = 2 IF threshold le cca(ibox)
                    frac_out(ibox,ilev) = 2
                else
                    ! = the same IF NOT threshold le cca(ibox)
                    frac_out(ibox,ilev) = frac_out(ibox,ilev)
                end if
           ENDDO
                                                                                                                                                           
!         Set last_frac to tca at this level, so as to be tca
!         from last level next time round
                                                                                                                                                           
          if (ncolprint.ne.0) then
                                                                                                                                                           
            write (6,'(a)') 'last_frac:'
            write (6,'(8f5.2)') (tca(ibox,ilev-1),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'cca:'
            write (6,'(8f5.2)') (cca(ibox,ilev),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'max_overlap_cc:'
            write (6,'(8f5.2)') (maxocc(ibox),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'max_overlap_sc:'
            write (6,'(8f5.2)') (maxosc(ibox),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'threshold_min_nsf2:'
            write (6,'(8f5.2)') (threshold_min(ibox),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'threshold_nsf2:'
            write (6,'(8f5.2)') (threshold(ibox),ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'frac_out_pp_rev:'
            write (6,'(8f5.2)')    &
             ((frac_out(ibox,ilev2),ibox=1,ncolprint),ilev2=1,nlev)
          endif
                                                                                                                                                           
200   CONTINUE    !loop over nlev
                                                                                                                                                           
!
!     ---------------------------------------------------!
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
!     COMPUTE CLOUD OPTICAL DEPTH FOR EACH COLUMN and
!     put into vector tau
                                                                                                                                                           
      !initialize tau and albedocld to zero
      do 15 ibox=1,ncol
            tau(ibox)=0.
            albedocld(ibox)=0.
15    continue
                                                                                                                                                           
      !compute total cloud optical depth for each column
      do 26 ilev=1,nlev
                                                                                                                                                           
                                                                                                                                                           
            !increment tau for each of the boxes
            do 16 ibox=1,ncol
                                                                                                                                                           
                 if (frac_out(ibox,ilev).eq.1) then
                        dtautmp(ibox)= dtau_s(ilev)
                 else if (frac_out(ibox,ilev).eq.2) then
                        dtautmp(ibox)= dtau_c(ilev)
                 else
                        dtautmp(ibox)= 0.
                 end if
                                                                                                                                                           
                 tau(ibox)=tau(ibox)+dtautmp(ibox)
                                                                                                                                                           
16          continue
                                                                                                                                                           
            if (ncolprint.ne.0) then
                     write(6,'(i2,1X,8(f7.2,1X))')   &
                     ilev,(dtautmp(ibox),ibox=1,ncolprint)
            endif
                                                                                                                                                           
26    continue
                                                                                                                                                           
          if (ncolprint.ne.0) then
                                                                                                                                                           
              write(6,'(i2,1X,8(f7.2,1X))')          &
                ilev, (tau(ibox),ibox=1,ncolprint)
          endif
!
!     ---------------------------------------------------!
                                                                                                                                                           
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
!     COMPUTE INFRARED BRIGHTNESS TEMPERUATRES
!     AND CLOUD TOP TEMPERATURE SATELLITE SHOULD SEE
!
!     again this is only done if top_height = 1 or 3
!
!     fluxtop is the 10.5 micron radiance at the top of the
!              atmosphere
!     trans_layers_above is the total transmissivity in the layers
!             above the current layer
!     fluxtop_clrsky and trans_layers_above_clrsky are the clear
!             sky versions of these quantities.
                                                                                                                                                           
                                                                                                                                                           
      if (top_height .eq. 1 .or. top_height .eq. 3) then
                                                                                                                                                           
                                                                                                                                                           
        !----------------------------------------------------------------------
        !
        !             DO CLEAR SKY RADIANCE CALCULATION FIRST
        !
        !compute water vapor continuum emissivity
        !this treatment follows Schwarkzopf and Ramasamy
        !JGR 1999,vol 104, pages 9467-9499.
        !the emissivity is calculated at a wavenumber of 955 cm-1,
        !or 10.47 microns
        wtmair = 28.9644
        wtmh20 = 18.01534
        Navo = 6.023E+23
        grav = 9.806650E+02
        pstd = 1.013250E+06
        t0 = 296.
        if (ncolprint .ne. 0)  &
               write(6,*)  'ilev   pw (kg/m2)   tauwv      dem_wv'
        do 125 ilev=1,nlev
               !press and dpress are dyne/cm2 = Pascals *10
               press = pfull(ilev)*10.
               dpress = (phalf(ilev+1)-phalf(ilev))*10
               !atmden = g/cm2 = kg/m2 / 10
               atmden = dpress/grav
               rvh20 = qv(ilev)*wtmair/wtmh20
               wk = rvh20*Navo*atmden/wtmair
               rhoave = (press/pstd)*(t0/at(ilev))
               rh20s = rvh20*rhoave
               rfrgn = rhoave-rh20s
               tmpexp = exp(-0.02*(at(ilev)-t0))
               tauwv = wk*1.e-20*( (0.0224697*rh20s*tmpexp) +      &
                      (3.41817e-7*rfrgn)         )*0.98
               dem_wv(ilev) = 1. - exp( -1. * tauwv)
               if (ncolprint .ne. 0)                               &
               write(6,'(i2,1X,3(f8.3,3X))') ilev,                 &
                 qv(ilev)*(phalf(ilev+1)-phalf(ilev))/(grav/100.), &
                 tauwv,dem_wv(ilev)
125     continue
                                                                                                                                                           
                                                                                                                                                           
        !initialize variables
        fluxtop_clrsky = 0.
        trans_layers_above_clrsky=1.
                                                                                                                                                           
                                                                                                                                                           
        do ilev=1,nlev
                                                                                                                                                           
            ! Black body emission at temperature of the layer
                                                                                                                                                           
                bb=1 / ( exp(1307.27/at(ilev)) - 1. )
                !bb= 5.67e-8*at(ilev)**4
                                                                                                                                                           
                ! increase TOA flux by flux emitted from layer
                ! times total transmittance in layers above
                                                                                                                                                           
                fluxtop_clrsky = fluxtop_clrsky                      &
                  + dem_wv(ilev) * bb * trans_layers_above_clrsky
                                                                                                                                                           
                ! update trans_layers_above with transmissivity
                ! from this layer for next time around loop
                                                                                                                                                           
                trans_layers_above_clrsky=                           &
                  trans_layers_above_clrsky*(1.-dem_wv(ilev))
                                                                                                                                                           
                                                                                                                                                           
            if (ncolprint.ne.0) then
              write (6,'(a)') 'ilev:'
              write (6,'(I2)') ilev
                                                                                                                                                           
              write (6,'(a)') 'emiss_layer,100.*bb,100.*f,total_trans:'
              write (6,'(4(f7.2,1X))') dem_wv(ilev),100.*bb,         &
                   100.*fluxtop_clrsky,trans_layers_above_clrsky
            endif
                                                                                                                                                           
        enddo   !loop over level
                                                                                                                                                           
        !add in surface emission
        bb=1/( exp(1307.27/skt) - 1. )
        !bb=5.67e-8*skt**4
                                                                                                                                                           
        fluxtop_clrsky = fluxtop_clrsky + emsfc_lw * bb              &
           * trans_layers_above_clrsky
                                                                                                                                                           
        if (ncolprint.ne.0) then
          write (6,'(a)') 'id:'
          write (6,'(a)') 'surface'
                                                                                                                                                           
          write (6,'(a)') 'emsfc,100.*bb,100.*f,total_trans:'
          write (6,'(4(f7.2,1X))') emsfc_lw,100.*bb,                 &
            100.*fluxtop_clrsky,trans_layers_above_clrsky
        endif
                                                                                                                                                           
                                                                                                                                                           
        !
        !           END OF CLEAR SKY CALCULATION
        !
        !----------------------------------------------------------------
                                                                                                                                                           
                                                                                                                                                           
                                                                                                                                                           
        if (ncolprint.ne.0) then
                                                                                                                                                           
            write (6,'(a)') 'ts:'
            write (6,'(8f7.2)') (skt,ibox=1,ncolprint)
                                                                                                                                                           
            write (6,'(a)') 'ta_rev:'
            write (6,'(8f7.2)')                                      &
             ((at(ilev2),ibox=1,ncolprint),ilev2=1,nlev)
                                                                                                                                                           
        endif
        !loop over columns
        do ibox=1,ncol
                                                                                                                                                           
            fluxtop(ibox)=0.
            trans_layers_above(ibox)=1.
                                                                                                                                                           
        enddo
                                                                                                                                                           
        do ilev=1,nlev
                                                                                                                                                           
            do ibox=1,ncol
                                                                                                                                                           
                ! Black body emission at temperature of the layer
                                                                                                                                                           
                bb=1 / ( exp(1307.27/at(ilev)) - 1. )
                !bb= 5.67e-8*at(ilev)**4
                                                                                                                                                           
                ! emissivity for point in this layer
                if (frac_out(ibox,ilev).eq.1) then
                dem(ibox)= 1. -                                     &
                           ( (1. - dem_wv(ilev)) * (1. -  dem_s(ilev)) )
                else if (frac_out(ibox,ilev).eq.2) then
                dem(ibox)= 1. -                                     &
                           ( (1. - dem_wv(ilev)) * (1. -  dem_c(ilev)) )
                else
                dem(ibox)=  dem_wv(ilev)
                end if
                                                                                                                                                           
                                                                                                                                                           
                ! increase TOA flux by flux emitted from layer
                ! times total transmittance in layers above
                                                                                                                                                           
                fluxtop(ibox) = fluxtop(ibox)                       &
                  + dem(ibox) * bb                                  &
                  * trans_layers_above(ibox)
                                                                                                                                                           
                ! update trans_layers_above with transmissivity
                ! from this layer for next time around loop
                                                                                                                                                           
                trans_layers_above(ibox)=                           &
                  trans_layers_above(ibox)*(1.-dem(ibox))
                                                                                                                                                           
            enddo ! ibox
                                                                                                                                                           
            if (ncolprint.ne.0) then
                                                                                                                                                           
              write (6,'(a)') 'ilev:'
              write (6,'(I2)') ilev
                                                                                                                                                           
              write (6,'(a)') 'emiss_layer:'
              write (6,'(8f7.2)') (dem(ibox),ibox=1,ncolprint)
                                                                                                                                                           
              write (6,'(a)') '100.*bb:'
              write (6,'(8f7.2)') (100.*bb,ibox=1,ncolprint)
                                                                                                                                                           
              write (6,'(a)') '100.*f:'
              write (6,'(8f7.2)') (100.*fluxtop(ibox),ibox=1,ncolprint)
                                                                                                                                                           
              write (6,'(a)') 'total_trans:'
              write (6,'(8f7.2)')                                   &
                (trans_layers_above(ibox),ibox=1,ncolprint)
          endif
                                                                                                                                                           
        enddo ! ilev
                                                                                                                                                           
        do ibox=1,ncol
                                                                                                                                                           
            !add in surface emission
                                                                                                                                                           
            bb=1/( exp(1307.27/skt) - 1. )
            !bb=5.67e-8*skt**4
                                                                                                                                                           
            fluxtop(ibox) = fluxtop(ibox)                           &
               + emsfc_lw * bb                                      &
               * trans_layers_above(ibox)
                                                                                                                                                           
        end do
                                                                                                                                                           
        if (ncolprint.ne.0) then
                                                                                                                                                           
          write (6,'(a)') 'id:'
          write (6,'(a)') 'surface'
                                                                                                                                                           
          write (6,'(a)') 'emiss_layer:'
                                                                                                                                                                write (6,'(8f7.2)') (dem(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') '100.*bb:'
          write (6,'(8f7.2)') (100.*bb,ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') '100.*f:'
          write (6,'(8f7.2)') (100.*fluxtop(ibox),ibox=1,ncolprint)
        endif
                                                                                                                                                           
        do ibox=1,ncol
                                                                                                                                                           
            !now that you have the top of atmosphere radiance account
            !for ISCCP procedures to determine cloud top temperature
                                                                                                                                                           
            !account for partially transmitting cloud recompute flux
            !ISCCP would see assuming a single layer cloud
            !note choice here of 2.13, as it is primarily ice
            !clouds which have partial emissivity and need the
            !adjustment performed in this section
            !
            !If it turns out that the cloud brightness temperature
            !is greater than 260K, then the liquid cloud conversion
            !factor of 2.56 is used.
            !
            !Note that this is discussed on pages 85-87 of
            !the ISCCP D level documentation (Rossow et al. 1996)
                                                                                                                                                           
                                                                                                                                                           
            !compute minimum brightness temperature and optical depth
            btcmin = 1. /  ( exp(1307.27/(attrop-5.)) - 1. )
            transmax = (fluxtop(ibox)-btcmin)/(fluxtop_clrsky-btcmin)
            taumin = -1. * log(max(min(transmax,0.9999999),0.001))
                                                                                                                                                           
            !note that the initial setting of tauir is needed so that
            !tauir has a realistic value should the next if block be
            !bypassed
            tauir = tau(ibox) / 2.13
                                                                                                                                                           
            if (top_height .eq. 1 .and. transmax .gt. 0.001 .and.   &
                transmax .le. 0.9999999) then
                    icycle = 1
                    fluxtopinit = fluxtop(ibox)
                    tauir = tau(ibox) / 2.13
10                  emcld(ibox) = 1. - exp(-1. * tauir  )
                    fluxtop(ibox) = fluxtopinit -                   &
                               ((1.-emcld(ibox))*fluxtop_clrsky)
                    emcld(ibox)=max(emcld(ibox),1.e-10)
                    fluxtop(ibox)=max(1.E-06,                       &
                               (fluxtop(ibox)/emcld(ibox)))
                    tb(ibox)= 1307.27/ (log(1. + (1./fluxtop(ibox))))
                    if (icycle .eq. 1 .and. tb(ibox) .gt. 260.) then
                         tauir = tau(ibox) / 2.56
                         icycle = 2
                         go to 10
                    end if
            end if
                                                                                                                                                           
            if (tau(ibox) .gt.  (-1.*log(0.9999999))) then
                                                                                                                                                           
                !cloudy box
                tb(ibox)= 1307.27/ (log(1. + (1./fluxtop(ibox))))
                                                                                                                                                           
                if (top_height .eq. 1 .and. tauir .lt. taumin) then
                         tb(ibox) = attrop - 5.
                         tau(ibox) = 2.13*taumin
                end if
                                                                                                                                                           
            else
                                                                                                                                                           
                !clear sky brightness temperature
                tb(ibox) = 1307.27/(log(1.+(1./fluxtop_clrsky)))
                                                                                                                                                           
            end if
                                                                                                                                                           
        enddo ! ibox
                                                                                                                                                           
        if (ncolprint.ne.0) then
                                                                                                                                                           
          write (6,'(a)') '100.*f_adj:'
          write (6,'(8f7.2)') (100.*fluxtop(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'tau:'
          write (6,'(8f7.2)') (tau(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'emcld:'
          write (6,'(8f7.2)') (emcld(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'total_trans:'
          write (6,'(8f7.2)')                                      &
          (trans_layers_above(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'total_emiss:'
          write (6,'(8f7.2)')                                      &
          (1.0-trans_layers_above(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'total_trans:'
          write (6,'(8f7.2)')                                      &
          (trans_layers_above(ibox),ibox=1,ncolprint)
                                                                                                                                                           
          write (6,'(a)') 'ppout:'
          write (6,'(8f7.2)') (tb(ibox),ibox=1,ncolprint)
        endif
                                                                                                                                                           
      end if
!
!     ---------------------------------------------------!
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
!     DETERMINE CLOUD TOP PRESSURE
!
!     again the 2 methods differ according to whether
!     or not you use the physical cloud top pressure (top_height = 2)
!     or the radiatively determined cloud top pressure (top_height = 1 or 3)
!
                                                                                                                                                           
                                                                                                                                                           
                                                                                                                                                           
      !compute cloud top pressure
      do 30 ibox=1,ncol
                                                                                                                                                           
               !segregate according to optical thickness
               if (tau(ibox) .le. (-1.*log(0.9999999))) then
                                                                                                                                                           
                         ptop(ibox)=0.
                         ttop(ibox)=0.
                         levmatch(ibox)=0
                                                                                                                                                           
               else
                                                                                                                                                           
                     if (top_height .eq. 1 .or. top_height .eq. 3) then
                                                                                                                                                           
                        !find level whose temperature
                        !most closely matches brightness temperature
                        nmatch=0
                        do 29 ilev=1,nlev-1
                                                                                                                                                           
                            if ((at(ilev)   .ge. tb(ibox) .and.      &
                                 at(ilev+1) .lt. tb(ibox)) .or.      &
                                (at(ilev) .le. tb(ibox) .and.        &
                                 at(ilev+1) .gt. tb(ibox))) then
                                                                                                                                                           
                                  nmatch=nmatch+1
                                  if(abs(at(ilev)-tb(ibox)) .lt.     &
                                     abs(at(ilev+1)-tb(ibox))) then
                                         match(nmatch)=ilev
                                  else
                                         match(nmatch)=ilev+1
                                  end if
                            end if
29                      continue
                                                                                                                                                           
                        if (nmatch .ge. 1) then
                                                                                                                                                           
                            ptop(ibox)=pfull(match(nmatch))
                            ttop(ibox)=at(match(nmatch))
                            levmatch(ibox)=match(nmatch)
                        else
                                                                                                                                                           
                            if (tb(ibox) .lt. atmin) then
                                 ptop(ibox)=ptrop
                                 ttop(ibox)=atmin
                                 levmatch(ibox)=itrop
                            end if
                            if (tb(ibox) .gt. atmax) then
                                 ptop(ibox)=pfull(nlev)
                                 ttop(ibox)=atmax
                                 levmatch(ibox)=nlev
                            end if
                                                                                                                                                           
                        end if
                                                                                                                                                           
                     else
                          ptop(ibox)=0.
                          ttop(ibox)=0.
                          ilev=1
                          do while(ptop(ibox) .eq. 0.                 &
                                    .and. ilev .lt. nlev+1)
                                if (frac_out(ibox,ilev) .ne. 0) then
                                   ptop(ibox)=pfull(ilev)
                                   levmatch(ibox)=ilev
                                end if
                                ilev=ilev+1
                          end do
                     end if
               end if
                                                                                                                                                           
30    continue
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
!     DETERMINE ISCCP CLOUD TYPE FREQUENCIES
!
!     Now that ptop and tau have been determined,
!     determine amount of each of the 49 ISCCP cloud
!     types
!
!     Also compute grid box mean cloud top pressure and
!     optical thickness.  The mean cloud top pressure and
!     optical thickness are averages over the cloudy
!     area only. The mean cloud top pressure is a linear
!     average of the cloud top pressures.  The mean cloud
!     optical thickness is computed by converting optical
!     thickness to an albedo, averaging in albedo units,
!     then converting the average albedo back to a mean
!     optical thickness.
!
                                                                                                                                                           
      !compute isccp frequencies
                                                                                                                                                           
      !reset frequencies
      do 38 itau=1,ntau
      do 38 ipres=1,npres
             fq_isccp(itau,ipres)=0.
38    continue
                                                                                                                                                           
      !reset variables need for averaging cloud properties
      totalcldarea = 0.
      meanalbedocld = 0.
      meanptop = 0.
      meanttop = 0.
      meantaucld = 0.
      boxarea = 1./real(ncol)
                                                                                                                                                           
      do 39 ibox=1,ncol
                                                                                                                                                           
            !convert ptop to millibars
            ptop(ibox)=ptop(ibox) / 100.
                                                                                                                                                           
            !save for output cloud top pressure and optical thickness
            boxtau(ibox) = tau(ibox)
            boxptop(ibox) = ptop(ibox)
                                                                                                                                                           
      if (tau(ibox) .gt. (-1.*log(0.9999999))                 &
           .and. ptop(ibox) .gt. 0.) then
                                                                                                                                                           
            !convert optical thickness to albedo
            albedocld(ibox)=real(invtau(min(nint(100.*tau(ibox)),45000)))
                                                                                                                                                           
            !contribute to averaging
            totalcldarea = totalcldarea + boxarea
            meanalbedocld = meanalbedocld + albedocld(ibox)*boxarea
            meanptop = meanptop + ptop(ibox)*boxarea
            meanttop = meanttop + ttop(ibox)*boxarea
                                                                                                                                                           
            !reset itau, ipres
            itau = 0
            ipres = 0
                                                                                                                                                           
            !determine optical depth category
            if (tau(ibox) .lt. taulim(2)) then
                itau=1
            else if (tau(ibox) .ge. taulim(2)                  &
                                        .and. tau(ibox) .lt. taulim(3)) then
                itau=2
            else if (tau(ibox) .ge. taulim(3) .and. tau(ibox) .lt. taulim(4)) then
                itau=3
            else if (tau(ibox) .ge. taulim(4) .and. tau(ibox) .lt. taulim(5)) then
                itau=4
            else if (tau(ibox) .ge. taulim(5) .and. tau(ibox) .lt. taulim(6)) then
                itau=5
            else if (tau(ibox) .ge. taulim(6) .and. tau(ibox) .lt. taulim(7)) then
                itau=6
            else if (tau(ibox) .ge. taulim(7)) then
                itau=7
            end if
                                                                                                                                                           
            !determine cloud top pressure category
            if (    ptop(ibox) .gt. prlim(1)  .and.ptop(ibox) .lt. prlim(2)) then
                ipres=1
            else if(ptop(ibox) .ge. prlim(2).and.ptop(ibox) .lt. prlim(3)) then
                ipres=2
            else if(ptop(ibox) .ge. prlim(3).and.ptop(ibox) .lt. prlim(4)) then
                ipres=3
            else if(ptop(ibox) .ge. prlim(4).and.ptop(ibox) .lt. prlim(5)) then
                ipres=4
            else if(ptop(ibox) .ge. prlim(5).and.ptop(ibox) .lt. prlim(6)) then
                ipres=5
            else if(ptop(ibox) .ge. prlim(6).and.ptop(ibox) .lt. prlim(7)) then
                ipres=6
            else if(ptop(ibox) .ge. prlim(7)) then
                ipres=7
            end if
                                                                                                                                                           
            !update frequencies
            if(ipres .gt. 0.and.itau .gt. 0) then
            fq_isccp(itau,ipres)=fq_isccp(itau,ipres)+ boxarea
            end if
                                                                                                                                                           
      end if
                                                                                                                                                           
39    continue
                                                                                                                                                           
                                                                                                                                                           
      !compute mean cloud properties
      if (totalcldarea .gt. 0.) then
         meanalbedocld = meanalbedocld / totalcldarea
         meanptop = meanptop / totalcldarea
         meanttop = meanttop / totalcldarea
         meantaucld = tautab(min(255,max(1,nint(meanalbedocld))))
      else
!JR New code added here to prevent zeros getting accumulated into 2-d output arrays
         meanptop = fillvalue
         meanttop = fillvalue
         meantaucld = fillvalue
      end if
                                                                                                                                                           
!      call endisccp
!
!     ---------------------------------------------------!
                                                                                                                                                           
                                                                                                                                                           
!
!     ---------------------------------------------------!
!     OPTIONAL PRINTOUT OF DATA TO CHECK PROGRAM
!
!     to see info replace '3 .eq. 4' with '3 .eq. 3'
!
                                                                                                                                                           
      if (3 .eq. 4) then
                                                                                                                                                           
            !produce character output
            do 4031 ilev=1,nlev
            do 4031 ibox=1,ncol
                   acc(ilev,ibox)=frac_out(ibox,ilev)*2
                   if (levmatch(ibox) .eq. ilev)                      &
                       acc(ilev,ibox)=acc(ilev,ibox)+1
4031         continue
                                                                                                                                                           
             !print test
                                                                                                                                                           
             write (6,*) 'Gridbox decomposition written to unit 9'
                                                                                                                                                           
             write(9,'(a1)') ' '
             write(9,'(10i5)') (ilev,ilev=5,nlev,5)
             write(9,'(a1)') ' '
                                                                                                                                                           
             do ibox=1,ncol
                    write(9,'(40(a1),1x,40(a1))')                     &
                        (cchar_realtops(acc(ilev,ibox)+1),ilev=1,nlevmax)  &
                       ,(cchar(acc(ilev,ibox)+1),ilev=1,nlevmax)
             end do
                                                                                                                                                           
             if (ncolprint.ne.0) then
               write(6,'(a1)') ' '
                    write(6,'(a2,1X,5(a7,1X),a50)')                   &
                        'ilev',                                       &
                        'pfull','at',                                 &
                        'cc*100','dem_s','dtau_s',                    &
                        'cchar'
                                                                                                                                                           
               do 4012 ilev=1,nlev
!                    write(6,'(60i2)') (box(i,ilev),i=1,ncolprint)
                   write(6,'(i2,1X,5(f7.2,1X),50(a1))')               &
                        ilev,                                         &
                        pfull(ilev)/100.,at(ilev),                    &
                        cc(ilev)*100.0,dem_s(ilev),dtau_s(ilev),      &
                        (cchar(acc(ilev,ibox)+1),ibox=1,ncolprint)
4012           continue
               write (6,'(a)') 'skt:'
               write (6,'(8f7.2)') skt
                                                                                                                                                           
               write (6,'(8I7)') (ibox,ibox=1,ncolprint)
                                                                                                                                                           
               write (6,'(a)') 'tau:'
               write (6,'(8f7.2)') (tau(ibox),ibox=1,ncolprint)
                                                                                                                                                           
               write (6,'(a)') 'tb:'
               write (6,'(8f7.2)') (tb(ibox),ibox=1,ncolprint)
                                                                                                                                                           
               write (6,'(a)') 'ptop:'
               write (6,'(8f7.2)') (ptop(ibox),ibox=1,ncolprint)
             endif
      end if
!      call t_stopf ('isccp_cloud_types')
      return
end subroutine isccp_cloud_types

real(r8) function ran0(idum)
                                                                                                                                                           
!     $Id$
!     Platform independent random number generator from
!     Numerical Recipies
!     Mark Webb July 1999
                                                                                                                                                           
      integer idum,IA,IM,IQ,IR,k
      real(r8) AM
                                                                                                                                                           
      parameter (IA=16807, IM=2147483647, AM=1.0/IM, IQ=127773, IR=2836)
                                                                                                                                                           
      if (idum.eq.0) then
        write(6,*) 'idum=',idum
        write(6,*) 'ZERO seed not allowed'
!       call endrun ()
      endif
                                                                                                                                                           
      k=idum/IQ
      idum=ia*(idum-k*iq)-ir*k
      if (idum.lt.0) idum=idum+im
      ran0=am*idum
      return
end function ran0

END MODULE module_radiation_driver
