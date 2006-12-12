!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_radiation_driver
CONTAINS
!BOP
! !IROUTINE: radiation_driver - interface to radiation physics options

! !INTERFACE:
   SUBROUTINE radiation_driver (                                          &
               itimestep,dt ,lw_physics,sw_physics ,NPHS                  &
              ,RTHRATENLW ,RTHRATENSW ,RTHRATEN                           &
              ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC                          & ! Optional
              ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC                          & ! Optional
              ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC                          & ! Optional
              ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC                          & ! Optional
              ,  SWUPT,  SWUPTC,  SWDNT,  SWDNTC                          & ! Optional
              ,  SWUPB,  SWUPBC,  SWDNB,  SWDNBC                          & ! Optional
              ,  LWUPT,  LWUPTC,  LWDNT,  LWDNTC                          & ! Optional
              ,  LWUPB,  LWUPBC,  LWDNB,  LWDNBC                          & ! Optional
              ,LWCF,SWCF,OLR                                              & ! Optional
              ,GLW, GSW, SWDOWN, XLAT, XLONG, ALBEDO                      &
              ,EMISS, rho, p8w, p , pi , dz8w ,t, t8w, GMT                &
              ,XLAND, XICE, TSK, HTOP,HBOT,HTOPR,HBOTR, CUPPT, VEGFRA, SNOW     &
              ,julyr, JULDAY, julian, xtime, RADT, STEPRA, ICLOUD, warm_rain     &
              ,declin_urb,COSZ_URB2D, omg_urb2d                           & !Optional urban
              ,ra_call_offset,RSWTOA,RLWTOA, CZMEAN                       &
              ,CFRACL, CFRACM, CFRACH                                     &
              ,ACFRST,NCFRST,ACFRCV,NCFRCV,SWDOWNC                        &
              ,z                                                          &
              ,levsiz, n_ozmixm, n_aerosolc, paerlev                      &
              ,cam_abs_dim1, cam_abs_dim2, cam_abs_freq_s                 &
              ,ozmixm,pin                                                 & ! Optional
              ,m_ps_1,m_ps_2,aerosolc_1,aerosolc_2,m_hybi0                & ! Optional
              ,abstot, absnxt, emstot                                     & ! Optional
              ,taucldi, taucldc                                           & ! Optional
              ,ids, ide, jds, jde, kds, kde                               &
              ,ims, ime, jms, jme, kms, kme                               &
              ,i_start, i_end                                             &
              ,j_start, j_end                                             &
              ,kts, kte                                                   &
              ,num_tiles                                                  &
              ,qv,qc,qr,qi,qs,qg                                          &
              ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg                              &
              ,CLDFRA ,Pb                                                 &
              ,f_ice_phy,f_rain_phy                                       &
              ,pm2_5_dry, pm2_5_water, pm2_5_dry_ec                       &
              ,tauaer300, tauaer400, tauaer600, tauaer999                 & ! jcb
              ,gaer300, gaer400, gaer600, gaer999                         & ! jcb
              ,waer300, waer400, waer600, waer999                         & ! jcb
              ,qc_adjust ,qi_adjust                                       & ! jm
              ,cu_rad_feedback                                            & ! jm

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
!-- PM2_5_DRY     Dry PM2.5 aerosol mass for all species (ug m^-3)
!-- PM2_5_WATER   PM2.5 water mass (ug m^-3)
!-- PM2_5_DRY_EC  Dry PM2.5 elemental carbon aersol mass (ug m^-3)
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
!-- ra_call_offset -1 (old) means usually just before output, 0 after
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

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   STEPRA,ICLOUD,ra_call_offset
   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc, cam_abs_dim1, cam_abs_dim2
   REAL,      INTENT(IN   )       ::   cam_abs_freq_s

   LOGICAL,      INTENT(IN   )    ::   warm_rain

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 
   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  OZMIXM

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(IN )  ::     PIN

   REAL,  DIMENSION(ims:ime,jms:jme), OPTIONAL, INTENT(IN )  ::      m_ps_1,m_ps_2
   REAL,  DIMENSION( ims:ime, paerlev, jms:jme, n_aerosolc ), OPTIONAL, &
          INTENT(IN   ) ::                       aerosolc_1, aerosolc_2
   REAL,  DIMENSION(paerlev), OPTIONAL, &
          INTENT(IN   ) ::                           m_hybi0

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(INOUT)  ::                                  HTOP, &
                                                            HBOT, &
                                                           HTOPR, &
                                                           HBOTR, &
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
                                                             rho
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaer300,tauaer400,tauaer600,tauaer999, & ! jcb
                                 gaer300,gaer400,gaer600,gaer999, & ! jcb
                                 waer300,waer400,waer600,waer999, & ! jcb
                                 qc_adjust, qi_adjust

   LOGICAL, OPTIONAL :: cu_rad_feedback

!
! variables for aerosols (only if running with chemistry)
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::                                pm2_5_dry, &
                                                     pm2_5_water, &
                                                    pm2_5_dry_ec
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                              RTHRATEN, &
                                                      RTHRATENLW, &
                                                      RTHRATENSW

!  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
!        INTENT(INOUT)  ::                                  SWUP, &
!                                                           SWDN, &
!                                                      SWUPCLEAR, &
!                                                      SWDNCLEAR, &
!                                                           LWUP, &
!                                                           LWDN, &
!                                                      LWUPCLEAR, &
!                                                      LWDNCLEAR

   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                        SWUPT,  SWUPTC,  SWDNT,  SWDNTC,          &
                        SWUPB,  SWUPBC,  SWDNB,  SWDNBC,          &
                        LWUPT,  LWUPTC,  LWDNT,  LWDNTC,          &
                        LWUPB,  LWUPBC,  LWDNB,  LWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(INOUT)  ::                                  SWCF, &
                                                            LWCF, &
                                                             OLR


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
   REAL, INTENT(IN  )   ::                                GMT,dt, &
                                                   julian, xtime
!
   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep

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
! Optional (only used by CAM lw scheme)

   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim2, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  abstot
   REAL, DIMENSION( ims:ime, kms:kme, cam_abs_dim1, jms:jme ), OPTIONAL ,&
         INTENT(INOUT)  ::                                  absnxt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               OPTIONAL ,&
         INTENT(INOUT)  ::                                  emstot

!
! Optional 
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) ::                                 CLDFRA

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
         INTENT(INOUT ) ::                                        &
                                                               pb &
                                               ,qv,qc,qr,qi,qs,qg

   LOGICAL, OPTIONAL ::             f_qv,f_qc,f_qr,f_qi,f_qs,f_qg
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       taucldi,taucldc
 
! LOCAL  VAR

   REAL, DIMENSION( ims:ime, jms:jme ) ::             GLAT,GLON
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    CEMISS
   REAL, DIMENSION( ims:ime, jms:jme ) ::             coszr

   REAL    ::    DECLIN,SOLCON 
   INTEGER ::    i,j,k,its,ite,jts,jte,ij
   INTEGER ::    STEPABS
   LOGICAL ::    gfdl_lw,gfdl_sw
   LOGICAL ::    doabsems
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_temp,qc_temp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_save,qc_save

!------------------------------------------------------------------
! urban related variables are added to declaration
!-------------------------------------------------
   REAL, OPTIONAL, INTENT(OUT) :: DECLIN_URB  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: COSZ_URB2D  !urban
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: omg_urb2d   !urban
!------------------------------------------------------------------

   if (lw_physics .eq. 0 .and. sw_physics .eq. 0)         return

! ra_call_offset = -1 gives old method where radiation may be called just before output
! ra_call_offset =  0 gives new method where radiation may be called just after output
!                     and is also consistent with removal of offset in new XTIME
   Radiation_step: IF (itimestep .eq. 1 .or. mod(itimestep,STEPRA) .eq. 1 + ra_call_offset) THEN

! CAM-specific additional radiation frequency - cam_abs_freq_s (=21600s by default)
     STEPABS = nint(cam_abs_freq_s/(dt*STEPRA))*STEPRA
     IF (itimestep .eq. 1 .or. mod(itimestep,STEPABS) .eq. 1 + ra_call_offset) THEN
       doabsems = .true.
     ELSE
       doabsems = .false.
     ENDIF

   gfdl_lw = .false.
   gfdl_sw = .false.

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
!        SWUP(I,K,J) = 0.0
!        SWDN(I,K,J) = 0.0
!        SWUPCLEAR(I,K,J) = 0.0
!        SWDNCLEAR(I,K,J) = 0.0
!        LWUP(I,K,J) = 0.0
!        LWDN(I,K,J) = 0.0
!        LWUPCLEAR(I,K,J) = 0.0
!        LWDNCLEAR(I,K,J) = 0.0
        CEMISS(I,K,J)=0.0
     ENDDO
     ENDDO
     ENDDO

! temporarily modify hydrometeors (currently only done for GD scheme and WRF-Chem)
!
     IF ( PRESENT( cu_rad_feedback ) ) THEN
       IF ( PRESENT( qc ) .AND. PRESENT( qc_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qc_save(i,k,j) = qc(i,k,j)
            qc(i,k,j) = qc(i,k,j) + qc_adjust(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
       IF ( PRESENT( qi ) .AND. PRESENT( qi_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qi_save(i,k,j) = qi(i,k,j)
            qi(i,k,j) = qi(i,k,j) + qi_adjust(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
     ENDIF


! Fill temporary water variable depending on micro package (tgs 25 Apr 2006)
     if(PRESENT(qc) .and. PRESENT(F_QC)) then
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=qc(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     else
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J)=0.
        ENDDO
        ENDDO
        ENDDO
     endif
     if(PRESENT(qr) .and. PRESENT(F_QR)) then
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           qc_temp(I,K,J) = qc_temp(I,K,J) + qr(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     endif

!---------------
! Calculate constant for short wave radiation

     CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                   DEGRAD,DPD                                )

     if(present(DECLIN_URB))DECLIN_URB=DECLIN  ! urban

     lwrad_cldfra_select: SELECT CASE(lw_physics)

        CASE (GFDLLWSCHEME)

!-- Do nothing, since cloud fractions (with partial cloudiness effects) 
!-- are defined in GFDL LW/SW schemes and do not need to be initialized.

        CASE (CAMLWSCHEME)

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
! Call to cloud fraction routine based on Randall 1994 (Hong Pan 1998)

   CALL cal_cldfra2(CLDFRA,qv,qc,qi,qs,                     &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )
     ENDIF
 
        CASE DEFAULT

     IF ( PRESENT ( CLDFRA ) .AND.                           &
          PRESENT(F_QC) .AND. PRESENT ( F_QI ) ) THEN
       CALL cal_cldfra(CLDFRA,qc,qi,F_QC,F_QI,               &
                       ids,ide, jds,jde, kds,kde,            &
                       ims,ime, jms,jme, kms,kme,            &
                       its,ite, jts,jte, kts,kte             )
     ENDIF

     END SELECT lwrad_cldfra_select    

!pjj/cray  Cray X1 cannot print from threaded region
#ifndef crayx1
     WRITE(wrf_err_message,*)'SOLCON=',SOLCON,DECLIN,XTIME
     CALL wrf_debug(50,wrf_err_message)
#endif

     lwrad_select: SELECT CASE(lw_physics)

        CASE (RRTMSCHEME)
             CALL wrf_debug (100, 'CALL rrtm')

             CALL RRTMLWRAD(                                        &
                  RTHRATEN=RTHRATEN,GLW=GLW,EMISS=EMISS             &
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

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc)   ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS) THEN
                 gfdl_lw  = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,COSZ_URB2D=COSZ_URB2D  ,OMG_URB2D=omg_urb2d       &
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
             IF(cam_abs_dim1 .ne. 4 .or. cam_abs_dim2 .ne. kde .or.  &
                paerlev .ne. 29 .or. levsiz .ne. 59 )THEN
               WRITE( wrf_err_message , * ) &
'set paerlev=29, levsiz=59, cam_abs_dim1=4, and cam_abs_dim2=number of levels (e_vert) in physics namelist for CAM radiation'
               CALL wrf_error_fatal ( wrf_err_message )
             ENDIF
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2) ) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=OLR,CEMISS=CEMISS,        &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
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
                     dz8w=dz8w,                                        &
                     CLDFRA=CLDFRA,XLAND=XLAND,XICE=XICE,SNOW=SNOW,    &
                     ozmixm=ozmixm,pin0=pin,levsiz=levsiz,             &
                     num_months=n_ozmixm,                              &
                     m_psp=m_ps_1,m_psn=m_ps_2,aerosolcp=aerosolc_1,   &
                     aerosolcn=aerosolc_2,m_hybi0=m_hybi0,             &
                     paerlev=paerlev, naer_c=n_aerosolc,               &
                     cam_abs_dim1=cam_abs_dim1, cam_abs_dim2=cam_abs_dim2, &
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling cam radiation' )
             ENDIF
        CASE DEFAULT
  
             WRITE( wrf_err_message , * ) 'The longwave option does not exist: lw_physics = ', lw_physics
             CALL wrf_error_fatal ( wrf_err_message )
           
     END SELECT lwrad_select    

     IF (lw_physics .gt. 0 .and. .not.gfdl_lw) THEN
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
#ifdef WRF_CHEM
                    ,PM2_5_DRY=pm2_5_dry,PM2_5_WATER=pm2_5_water       &
                    ,PM2_5_DRY_EC=pm2_5_dry_ec                         &
#endif
                    ,RHO_PHY=rho,T3D=t                                 &
                    ,P3D=p,PI3D=pi,DZ8W=dz8w,GMT=gmt                   &
                    ,R=r_d,CP=cp,G=g,JULDAY=julday                     &
                    ,XTIME=xtime,DECLIN=declin,SOLCON=solcon           &
!                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d            & !urban
                    ,RADFRQ=radt,ICLOUD=icloud,DEGRAD=degrad           &
                    ,warm_rain=warm_rain                               &
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        & !urban
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
                     RTHRATEN=rthraten,GSW=gsw,XLAT=xlat,XLONG=xlong   &
                    ,ALB=albedo,T3D=t,P3D=p,P8W3D=p8w,pi3D=pi          &
                    ,DZ8W=dz8w,RHO_PHY=rho                             &
                    ,CLDFRA3D=cldfra                                   &
                    ,GMT=gmt,CP=cp,G=g                                 &
!                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d            & !urban
                    ,JULDAY=julday,XTIME=xtime                         &
                    ,DECLIN=declin,SOLCON=solcon                       &
                    ,RADFRQ=radt,DEGRAD=degrad                         &
                    ,TAUCLDI=taucldi,TAUCLDC=taucldc                   &
                    ,WARM_RAIN=warm_rain                               &
#ifdef WRF_CHEM
                    ,TAUAER300=tauaer300,TAUAER400=tauaer400           & ! jcb
                    ,TAUAER600=tauaer600,TAUAER999=tauaer999           & ! jcb
                    ,GAER300=gaer300,GAER400=gaer400                   & ! jcb
                    ,GAER600=gaer600,GAER999=gaer999                   & ! jcb
                    ,WAER300=waer300,WAER400=waer400                   & ! jcb
                    ,WAER600=waer600,WAER999=waer999                   & ! jcb
#endif
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        & !urban
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

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QS) .AND. PRESENT(qs)   .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc) ) THEN
               IF ( F_QV .AND. F_QC .AND. F_QS ) THEN
                 gfdl_sw = .true.
                 CALL ETARA(                                        &
                  DT=dt,XLAND=xland                                 &
                 ,P8W=p8w,DZ8W=dz8w,RHO_PHY=rho,P_PHY=p,T=t         &
                 ,QV=qv,QW=qc_temp,QI=qi,QS=qs                      &
                 ,TSK2D=tsk,GLW=GLW,RSWIN=SWDOWN,GSW=GSW            &
                 ,RSWINC=SWDOWNC,CLDFRA=CLDFRA,PI3D=pi              &
                 ,GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot           &
                 ,HBOTR=hbotr, HTOPR=htopr                          &
                 ,ALBEDO=albedo,CUPPT=cuppt                         &
                 ,VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt               &
                 ,NSTEPRA=stepra,NPHS=nphs,ITIMESTEP=itimestep      &
                 ,XTIME=xtime,JULIAN=julian                         &
                 ,COSZ_URB2D=COSZ_URB2D ,OMG_URB2D=omg_urb2d        &
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

     IF (sw_physics .gt. 0 .and. .not.gfdl_sw) THEN
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

   ENDDO
   !$OMP END PARALLEL DO

   ENDIF Radiation_step

     accumulate_lw_select: SELECT CASE(lw_physics)

     CASE (CAMLWSCHEME)
   IF(PRESENT(LWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACLWUPT(I,J) = ACLWUPT(I,J) + LWUPT(I,J)*DT
           ACLWUPTC(I,J) = ACLWUPTC(I,J) + LWUPTC(I,J)*DT
           ACLWDNT(I,J) = ACLWDNT(I,J) + LWDNT(I,J)*DT
           ACLWDNTC(I,J) = ACLWDNTC(I,J) + LWDNTC(I,J)*DT
           ACLWUPB(I,J) = ACLWUPB(I,J) + LWUPB(I,J)*DT
           ACLWUPBC(I,J) = ACLWUPBC(I,J) + LWUPBC(I,J)*DT
           ACLWDNB(I,J) = ACLWDNB(I,J) + LWDNB(I,J)*DT
           ACLWDNBC(I,J) = ACLWDNBC(I,J) + LWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF
     CASE DEFAULT
     END SELECT accumulate_lw_select

     accumulate_sw_select: SELECT CASE(sw_physics)

     CASE (CAMSWSCHEME)
   IF(PRESENT(SWUPTC))THEN
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACSWUPT(I,J) = ACSWUPT(I,J) + SWUPT(I,J)*DT
           ACSWUPTC(I,J) = ACSWUPTC(I,J) + SWUPTC(I,J)*DT
           ACSWDNT(I,J) = ACSWDNT(I,J) + SWDNT(I,J)*DT
           ACSWDNTC(I,J) = ACSWDNTC(I,J) + SWDNTC(I,J)*DT
           ACSWUPB(I,J) = ACSWUPB(I,J) + SWUPB(I,J)*DT
           ACSWUPBC(I,J) = ACSWUPBC(I,J) + SWUPBC(I,J)*DT
           ACSWDNB(I,J) = ACSWDNB(I,J) + SWDNB(I,J)*DT
           ACSWDNBC(I,J) = ACSWDNBC(I,J) + SWDNBC(I,J)*DT
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF
     CASE DEFAULT
     END SELECT accumulate_sw_select
!
!*** Restore the saved values of the input Q arrays before exiting

     IF ( PRESENT( cu_rad_feedback ) ) THEN
       IF ( PRESENT( qc  ) .AND. PRESENT( qc_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qc(i,k,j) = qc_save(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
       IF ( PRESENT( qi  ) .AND. PRESENT( qi_adjust ) .AND. cu_rad_feedback ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
            qi(i,k,j) = qi_save(i,k,j)
          ENDDO
          ENDDO
          ENDDO
       ENDIF
     ENDIF

   END SUBROUTINE radiation_driver

!---------------------------------------------------------------------
!BOP
! !IROUTINE: radconst - compute radiation terms
! !INTERFAC:
   SUBROUTINE radconst(XTIME,DECLIN,SOLCON,JULIAN,                   &
                       DEGRAD,DPD                                    )
!---------------------------------------------------------------------
   USE module_wrf_error
   IMPLICIT NONE
!---------------------------------------------------------------------

! !ARGUMENTS:
   REAL, INTENT(IN   )      ::       DEGRAD,DPD,XTIME,JULIAN
   REAL, INTENT(OUT  )      ::       DECLIN,SOLCON
   REAL                     ::       OBECL,SINOB,SXLONG,ARG,  &
                                     DECDEG,DJUL,RJUL,ECCFAC
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
        
!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:
        
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
   SUBROUTINE cal_cldfra(CLDFRA,QC,QI,F_QC,F_QI,                     &
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
                                                                 QI, &
                                                                 QC

   LOGICAL,INTENT(IN) :: F_QC,F_QI

   REAL thresh
   INTEGER:: i,j,k
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
     thresh=1.0e-6

     IF ( f_qi .AND. f_qc ) THEN
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
     ELSE IF ( f_qc ) THEN
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
     ELSE
        DO j = jts,jte
        DO k = kts,kte
        DO i = its,ite
           CLDFRA(i,k,j)=0.
        ENDDO
        ENDDO
        ENDDO
     ENDIF

   END SUBROUTINE cal_cldfra

!BOP
! !IROUTINE: cal_cldfra2 - Compute cloud fraction
! !INTERFACE:
! cal_cldfra_xr - Compute cloud fraction.
! Code adapted from that in module_ra_gfdleta.F in WRF_v2.0.3 by James Done
!!
!!---  Cloud fraction parameterization follows Randall, 1994
!!     (see Hong et al., 1998)
!!     (modified by Ferrier, Feb '02)
!
   SUBROUTINE cal_cldfra2(CLDFRA, QV, QC, QI, QS,                     &
                         F_QV, F_QC, F_QI, F_QS, t_phy, p_phy,       &
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
                                                                 QS, &
                                                              t_phy, &
                                                              p_phy, &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY

   LOGICAL,INTENT(IN) :: F_QC,F_QI,F_QV,F_QS

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
!    es (in mb) = 6.1078 . exp[ a . (T-273.16)/ (T-b) ]
!
!       over ice        over water
! a =   21.8745584      17.2693882
! b =   7.66            35.86

!---------------------------------------------------------------------

    DO j = jts,jte
    DO k = kts,kte
    DO i = its,ite
      tc         = t_phy(i,k,j) - SVPT0
      esw     = 1000.0 * SVP1 * EXP( SVP2  * tc / ( t_phy(i,k,j) - SVP3  ) )
      esi     = 1000.0 * SVP1 * EXP( SVPI2 * tc / ( t_phy(i,k,j) - SVPI3 ) )
      QVSW = EP_2 * esw / ( p_phy(i,k,j) - esw )
      QVSI = EP_2 * esi / ( p_phy(i,k,j) - esi )

      IF ( F_QI .and. F_QC .and. F_QS) THEN
        QCLD=QI(i,k,j)+QC(i,k,j)+QS(I,k,j)
        IF (QCLD .LT. QCLDMIN) THEN
          weight = 0.
        ELSE
          weight = (QI(i,k,j)+QS(I,k,j)) / QCLD
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

   END SUBROUTINE cal_cldfra2

END MODULE module_radiation_driver
