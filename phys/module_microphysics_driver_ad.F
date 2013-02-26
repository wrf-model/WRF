!WRF:MEDIATION_LAYER:PHYSICS
! *** add new modules of schemes here
!
MODULE a_module_microphysics_driver
CONTAINS

SUBROUTINE a_microphysics_driver(   th, a_th, rho, a_rho                 & 
                      ,pi_phy, a_pi_phy,  p , a_p                        &

                      ,ht, dz8w,a_dz8w, p8w,a_p8w                   &
                      ,dt,dx,dy                                          &
                      ,mp_physics, spec_zone                             &
                      ,specified, channel_switch                         &
                      ,warm_rain                                         &
                      ,t8w                                               &
                      ,chem_opt, progn                                   &
                      ,cldfra, cldfra_old, exch_h, nsource               &
                      ,qlsink, precr, preci, precs, precg                &
                      ,xland,itimestep                                   &
                      ,f_ice_phy,f_rain_phy,f_rimef_phy                  &
                      ,lowlyr,sr, id                                     &
                      ,ids,ide, jds,jde, kds,kde                         &
                      ,ims,ime, jms,jme, kms,kme                         &
                      ,ips,ipe, jps,jpe, kps,kpe                         &
                      ,i_start,i_end,j_start,j_end,kts,kte               &
                      ,num_tiles, naer                                   &
                      ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr   &
                      ,a_qv_curr,a_qc_curr,a_qr_curr,a_qi_curr,a_qs_curr,a_qg_curr   &
                      ,qndrop_curr,qni_curr,qh_curr,a_qh_curr, qnh_curr             &
                      ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr      &
                      ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr      &
                      ,qvolg_curr                                        &
                      ,f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni      &
                      ,f_qns,f_qnr,f_qng,f_qnc,f_qnn,f_qh,f_qnh          &
                      ,            f_qzr,f_qzi,f_qzs,f_qzg,f_qzh         &
                      ,f_qvolg                                           &
                      ,qrcuten, qscuten, qicuten, mu                     &
                      ,qt_curr,f_qt                                      &
                      ,mp_restart_state,tbpvs_state,tbpvs0_state         & ! for etampnew
                      ,hail,ice2                                         & ! for mp_gsfcgce
!                     ,ccntype                                           & ! for mp_milbrandt2mom
                      ,w ,z                                              &
                      ,a_w, a_z                                          &
                      ,rainnc,    rainncv                                &
                      ,a_rainnc,  a_rainncv                              &
                      ,snownc,    snowncv                                &
                      ,hailnc,    hailncv                                &
                      ,graupelnc, graupelncv                             &
#ifdef WRF_CHEM
                      ,rainprod, evapprod                                &
                      ,qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp                &
#endif
                      ,refl_10cm                                         & ! HM, 9/22/09, add for refl
! YLIN
! Added the RI_CURR array to the call
                      ,ri_curr                                           &
                      ,diagflag,   do_radar_ref                          &
                                                   )
! Framework
#if(NMM_CORE==1)
   USE module_state_description, ONLY :                                  &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME    &
                    ,WSM6SCHEME, ETAMPNEW, etamp_HWRF,THOMPSON, MORR_TWO_MOMENT     &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME  &
                    ,MILBRANDT2MOM !,MILBRANDT3MOM
#else
   USE module_state_description, ONLY :                                  &
                     KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME    &
                    ,WSM6SCHEME, ETAMPNEW,THOMPSON, MORR_TWO_MOMENT     &
                    ,GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME, NSSL_2MOM, NSSL_2MOMCCN &
                    ,MILBRANDT2MOM, LSCONDSCHEME, MKESSLERSCHEME !,MILBRANDT3MOM
#endif

! Model Layer
   USE module_model_constants
   USE module_wrf_error

! *** add new modules of schemes here

   USE module_mp_mkessler
   USE a_module_mp_mkessler
   USE module_mp_lin
   USE module_mp_sbu_ylin
   USE module_mp_wsm3
   USE module_mp_wsm5
   USE module_mp_wsm6
   USE module_mp_etanew
   USE module_mp_thompson
   USE module_mp_gsfcgce
   USE module_mp_morr_two_moment
   USE module_mp_wdm5
   USE module_mp_wdm6
   USE module_mp_milbrandt2mom
!  USE module_mp_milbrandt3mom
#if (EM_CORE==1)
   USE module_mp_nssl_2mom
#endif

   USE module_mp_HWRF
   USE module_mixactivate, only: prescribe_aerosol_mixactivate

   USE module_mp_nconvp 
   USE a_module_mp_nconvp 

! For checking model timestep is history time (for radar reflectivity)
   USE module_utility, ONLY: WRFU_Clock, WRFU_Alarm
   USE module_domain, ONLY : HISTORY_ALARM, Is_alarm_tstep

!----------------------------------------------------------------------
   ! This driver calls subroutines for the microphys.
   !
   ! Schemes
   !
   ! Kessler scheme
   ! Lin et al. (1983), Rutledge and Hobbs (1984)
   ! WRF Single-Moment 3-class, Hong, Dudhia and Chen (2004)
   ! WRF Single-Moment 5-class, Hong, Dudhia and Chen (2004)
   ! WRF Single-Moment 6-class, Lim and Hong (2003 WRF workshop)
   ! Eta Grid-scale Cloud and Precipitation scheme (EGCP01, Ferrier)
   ! Milbrandt and Yau (2005)

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
! Definitions
!-----------
! Rho_d      dry density (kg/m^3)
! Theta_m    moist potential temperature (K)
! Qv         water vapor    mixing ratio (kg/kg)
! Qc         cloud water    mixing ratio (kg/kg)
! Qr         rain water     mixing ratio (kg/kg)
! Qi         cloud ice      mixing ratio (kg/kg)
! Qs         snow           mixing ratio (kg/kg)
! Qg         graupel        mixing ratio (kg/kg)
! Qh         hail           mixing ratio (kg/kg)
! Qndrop     droplet number mixing ratio (#/kg)
! Qni        cloud ice number concentration (#/kg)
! Qns        snow      number concentration (#/kg)
! Qnr        rain      number concentration (#/kg)
! Qng        graupel   number concentration (#/kg)
! Qnh        hail      number concentration (#/kg)

! Qzr        rain             reflectivity (m6/kg)
! Qzi        ice              reflectivity (m6/kg)
! Qzs        snow             reflectivity (m6/kg)
! Qzg        graupel          reflectivity (m6/kg)
! Qzh        hail             reflectivity (m6/kg)

! Qvolg        graupel   particle volume (m3/kg)

!
!----------------------------------------------------------------------
!-- th        potential temperature    (K)
!-- moist_new     updated moisture array   (kg/kg)
!-- moist_old     Old moisture array       (kg/kg)
!-- rho           density of air           (kg/m^3)
!-- pi_phy        exner function           (dimensionless)
!-- p             pressure                 (Pa)
!-- RAINNC        grid scale precipitation (mm)
!-- RAINNCV       one time step grid scale precipitation (mm/step)
!-- SNOWNC        grid scale snow and ice (mm)
!-- SNOWNCV       one time step grid scale snow and ice (mm/step)
!-- GRAUPELNC     grid scale graupel (mm)
!-- GRAUPELNCV    one time step grid scale graupel (mm/step)
!-- HAILNC        grid scale hail (mm)
!-- HAILNCV       one time step grid scale hail (mm/step)
!-- SR            one time step mass ratio of snow to total precip
!-- z             Height above sea level   (m)
!-- dt            Time step              (s)
!-- G             acceleration due to gravity  (m/s^2)
!-- CP            heat capacity at constant pressure for dry air (J/kg/K)
!-- R_d           gas constant for dry air (J/kg/K)
!-- R_v           gas constant for water vapor (J/kg/K)
!-- XLS           latent heat of sublimation   (J/kg)
!-- XLV           latent heat of vaporization  (J/kg)
!-- XLF           latent heat of melting       (J/kg)
!-- rhowater      water density                      (kg/m^3)
!-- rhosnow       snow density               (kg/m^3)
!-- F_ICE_PHY     Fraction of ice.
!-- F_RAIN_PHY    Fraction of rain.
!-- F_RIMEF_PHY   Mass ratio of rimed ice (rime factor)
!-- t8w           temperature at layer interfaces
!-- cldfra, cldfra_old, current, previous cloud fraction
!-- exch_h        vertical diffusivity (m2/s)
!-- qlsink        Fractional cloud water sink (/s)
!-- precr         rain precipitation rate at all levels (kg/m2/s)
!-- preci         ice precipitation rate at all levels (kg/m2/s)
!-- precs         snow precipitation rate at all levels (kg/m2/s)
!-- precg         graupel precipitation rate at all levels (kg/m2/s)                             &
!-- P_QV          species index for water vapor
!-- P_QC          species index for cloud water
!-- P_QR          species index for rain water
!-- P_QI          species index for cloud ice
!-- P_QS          species index for snow
!-- P_QG          species index for graupel
!-- P_QH          species index for hail
!-- P_QNDROP      species index for cloud drop mixing ratio
!-- P_QNR         species index for rain number concentration,
!-- P_QNI         species index for cloud ice number concentration
!-- P_QNS         species index for snow number concentration,
!-- P_QNG         species index for graupel number concentration,
!-- P_QNH         species index for hail number concentration,
!-- P_QZR         species index for rain    reflectivity
!-- P_QZI         species index for ice     reflectivity
!-- P_QZS         species index for snow    reflectivity
!-- P_QZG         species index for graupel reflectivity
!-- P_QZH         species index for hail    reflectivity
!-- P_QVOLG       species index for graupel particle volume,
!-- id            grid id number
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
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!-- diagflag      Logical to tell us when to produce diagnostics for history or restart
!
!======================================================================

   INTEGER,    INTENT(IN   )    :: mp_physics
   LOGICAL,    INTENT(IN   )    :: specified
   INTEGER, OPTIONAL, INTENT(IN   )    :: chem_opt, progn
   INTEGER, OPTIONAL, INTENT(IN   )    :: hail, ice2 !, ccntype
!
   INTEGER,      INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
   INTEGER,      INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
   INTEGER, OPTIONAL, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe
   INTEGER,      INTENT(IN   )    ::                         kts,kte
   INTEGER,      INTENT(IN   )    ::     itimestep,num_tiles,spec_zone
   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
     &           i_start,i_end,j_start,j_end

   LOGICAL,      INTENT(IN   )    ::   warm_rain
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(INOUT) ::                                    th, a_th
!

!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                             &
                                                                 rho, &
                                                                dz8w, &
                                                                 p8w, &
                                                              pi_phy, &
                                                                   p

   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ::                  &
                                                               a_rho, &
                                                              a_dz8w, &
                                                               a_p8w, &
                                                            a_pi_phy, &
                                                                 a_p

   REAL, INTENT(INOUT),  DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
                                     F_ICE_PHY,F_RAIN_PHY,F_RIMEF_PHY
!!$#ifdef WRF_CHEM
!  REAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
   REAL, OPTIONAL, INTENT(OUT), DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
!!$#else
!!$  REAL, DIMENSION(ims:ime, kms:kme, jms:jme ) ::     &
!!$#endif
         qlsink, & ! cloud water sink (/s)
         precr, & ! rain precipitation rate at all levels (kg/m2/s)
         preci, & ! ice precipitation rate at all levels (kg/m2/s)
         precs, & ! snow precipitation rate at all levels (kg/m2/s)
         precg    ! graupel precipitation rate at all levels (kg/m2/s)

!

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: XLAND

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(OUT)   :: SR

   REAL, INTENT(IN   ) :: dt,dx,dy

   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: LOWLYR

!
! Optional
!
   REAL, OPTIONAL, DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(OUT) :: refl_10cm

   LOGICAL,  OPTIONAL,   INTENT(IN   )    :: channel_switch
   REAL, OPTIONAL,  INTENT(INOUT   ) :: naer  ! aerosol number concentration (/kg)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                  w, z, t8w                                       &
                 ,a_w,a_z                                         &
                 ,cldfra, cldfra_old, exch_h                      &
                 ,qv_curr,qc_curr,qr_curr,qi_curr,qs_curr,qg_curr &
                 ,a_qv_curr,a_qc_curr,a_qr_curr,a_qi_curr,a_qs_curr,a_qg_curr &
                 ,qt_curr,qndrop_curr,qni_curr,qh_curr,a_qh_curr,qnh_curr   &
                 ,qns_curr,qnr_curr,qng_curr,qnn_curr,qnc_curr    &
                 ,qzr_curr,qzi_curr,qzs_curr,qzg_curr,qzh_curr    &
                 ,qvolg_curr

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN) :: qrcuten, qscuten, qicuten
#ifdef WRF_CHEM
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT) :: rainprod, evapprod
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT) :: qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp
#endif
   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL,                                                &
         INTENT(IN) :: mu
! YLIN
! Added RI_CURR similar to microphysics fields above
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT) :: ri_curr


   REAL, DIMENSION(ims:ime, kms:kme, jms:jme ),                   &
         OPTIONAL,                                                &
         INTENT(OUT ) ::                                          &
                  nsource

!
   REAL, DIMENSION( ims:ime , jms:jme ),                          &
         INTENT(INOUT),                                           &
         OPTIONAL   ::                                            &
                                                  RAINNC,a_RAINNC &
                                               ,RAINNCV,a_RAINNCV &
                                                          ,SNOWNC &
                                                         ,SNOWNCV &
                                                       ,GRAUPELNC &
                                                      ,GRAUPELNCV &
                                                          ,HAILNC &
                                                         ,HAILNCV
   INTEGER,OPTIONAL,INTENT(IN   )    ::                        id

   REAL , DIMENSION( ims:ime , jms:jme ) , OPTIONAL ,             &
         INTENT(IN)   ::                                       ht

   REAL, DIMENSION (:), OPTIONAL, INTENT(INOUT) :: mp_restart_state &
                                         ,tbpvs_state,tbpvs0_state
!

   LOGICAL, OPTIONAL :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt    &
                       ,f_qns,f_qnr,f_qng,f_qnn,f_qnc,f_qh,f_qnh,f_qzr       &
                       ,f_qzi,f_qzs,f_qzg,f_qzh,f_qvolg

   LOGICAL, OPTIONAL, INTENT(IN) :: diagflag
   INTEGER, OPTIONAL, INTENT(IN) :: do_radar_ref

! LOCAL  VAR

   INTEGER :: i,j,k,its,ite,jts,jte,ij,sz,n
   LOGICAL :: channel

!---------------------------------------------------------------------
!  check for microphysics type.  We need a clean way to
!  specify these things!
!---------------------------------------------------------------------

   channel = .FALSE.
   IF ( PRESENT ( channel_switch ) ) channel = channel_switch

   if (mp_physics .eq. 0) return
   IF( specified ) THEN
     sz = spec_zone
   ELSE
     sz = 0
   ENDIF

#ifndef RUN_ON_GPU
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij, its, ite, jts, jte, i,j,k,n )

   DO ij = 1 , num_tiles
       IF (channel) THEN
         its = max(i_start(ij),ids)
         ite = min(i_end(ij),ide-1)
       ELSE
         its = max(i_start(ij),ids+sz)
         ite = min(i_end(ij),ide-1-sz)
       ENDIF
       jts = max(j_start(ij),jds+sz)
       jte = min(j_end(ij),jde-1-sz)
#else
   DO ij = 1 , 1
       IF (channel) THEN
         its = max(ips,ids)
         ite = min(ipe,ide-1)
       ELSE
         its = max(ips,ids+sz)
         ite = min(ipe,ide-1-sz)
       ENDIF
       jts = max(jps,jds+sz)
       jte = min(jpe,jde-1-sz)
#endif


! 2009-06009 rce - zero all these for safety
       IF( PRESENT(qlsink) ) qlsink(its:ite,kts:kte,jts:jte) = 0.
       IF( PRESENT(precr ) ) precr(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(preci ) ) preci(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precs ) ) precs(its:ite,kts:kte,jts:jte)  = 0.
       IF( PRESENT(precg ) ) precg(its:ite,kts:kte,jts:jte)  = 0.

!-----------

     adj_micro_select: SELECT CASE(mp_physics)

        CASE (MKESSLERSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling a_mkessler' )
             IF ( PRESENT( QV_CURR ) .AND. PRESENT( QC_CURR ) .AND.  &
                                           PRESENT( QR_CURR ) .AND.  &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV ) .AND.  &
                                           PRESENT( Z       ))  THEN
               CALL a_mkessler(                                        &
                  T=th,TB=a_th                                              &
                 ,QV=qv_curr,QVB=a_qv_curr                                        &
                 ,QC=qc_curr,QCB=a_qc_curr                                        &
                 ,QR=qr_curr,QRB=a_qr_curr                                        &
!whl P is not recomputed in the HL' Kessler; need be added here
                 ,P=p,PB=a_p                                               &
!end whl
                 ,RHO=rho, RHOB=a_rho, PII=pi_phy,PIIB=a_pi_phy, DT_IN=dt, Z=z, XLV=xlv, CP=cp &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0,RHOWATER=rhowater           &
                 ,DZ8W=dz8w                                         &
                 ,RAINNC=rainnc,RAINNCV=rainncv                     &
                 ,RAINNCB=a_rainnc,RAINNCVB=a_rainncv                     &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling a_mkessler' )
             ENDIF

        CASE (LSCONDSCHEME)
             CALL wrf_debug ( 100 , 'microphysics_driver: calling a_lscond' )
             IF ( PRESENT( QV_CURR ) .AND.                          &
                  PRESENT( RAINNC  ) .AND. PRESENT ( RAINNCV )) THEN

               CALL a_lscond(                                         &
                  TH=th,  A_TH=a_th                                  &
                 ,P=p, A_P=a_p                                            &
                 ,QV=qv_curr, A_QV=a_qv_curr                              &
                 ,RHO=rho,A_RHO=a_rho, PII=pi_phy, A_PII=a_pi_phy   &
                 , XLV=xlv, CP=cp                                   &
                 ,EP2=ep_2,SVP1=svp1,SVP2=svp2                      &
                 ,SVP3=svp3,SVPT0=svpt0                             &
                 ,R_V= R_v                                          & ! added
                 ,DZ8W=dz8w,A_DZ8W=a_dz8w                             &
                 ,RAINNC=rainnc, A_RAINNC=a_rainnc                    &
                 ,RAINNCV=rainncv,A_RAINNCV=a_rainncv                 &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )
                                          
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling a_lscond' )
             ENDIF

      CASE DEFAULT

         WRITE( wrf_err_message , * ) 'The microphysics option does not exist: mp_physics = ', mp_physics
         CALL wrf_error_fatal ( wrf_err_message )

      END SELECT adj_micro_select

   ENDDO
#ifndef RUN_ON_GPU
   !$OMP END PARALLEL DO
#endif

   CALL wrf_debug ( 200 , 'microphysics_driver: returning from' )

   RETURN

   END SUBROUTINE a_microphysics_driver

END MODULE a_module_microphysics_driver
