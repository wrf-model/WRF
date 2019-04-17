
!WRF:MEDIATION_LAYER:PHYSICS
!
MODULE module_radiation_driver
CONTAINS
!BOP
! !IROUTINE: radiation_driver - interface to radiation physics options

! !INTERFACE:
   SUBROUTINE radiation_driver (                                  &
               ACFRCV ,ACFRST ,ALBEDO  &
              ,CFRACH ,CFRACL ,CFRACM  &
              ,CUPPT  ,CZMEAN ,DT      &
              ,DZ8W   ,EMISS  ,GLW     &
              ,GMT    ,GSW    ,HBOT    &
              ,HTOP   ,HBOTR  ,HTOPR   &
              ,ICLOUD                  &
              ,cldovrlp                 & 
              ,ITIMESTEP,JULDAY, JULIAN &
              ,JULYR  ,LW_PHYSICS       &
              ,NCFRCV ,NCFRST ,NPHS     &
              ,O3INPUT, O3RAD           &
              ,AER_OPT, aerod           &
              ,swint_opt                &
              ,P8W    ,P ,PI            &
              ,p_top                    &
              ,RADT   ,RA_CALL_OFFSET   &
              ,RHO    ,RLWTOA           &
              ,RSWTOA ,RTHRATEN         &
              ,RTHRATENLW      ,RTHRATENSW    &
              ,SNOW   ,STEPRA ,SWDOWN  &
              ,SWDOWNC ,SW_PHYSICS     &
              ,T8W     ,T ,TAUCLDC &
              ,TAUCLDI ,TSK ,VEGFRA  &
              ,WARM_RAIN ,XICE ,XLAND   &
              ,XLAT ,XLONG ,YR          &
!Optional solar variables
              ,DECLINX ,SOLCONX ,COSZEN ,HRANG    &
              , CEN_LAT                                      &
              ,Z                                             &
              ,ALEVSIZ, no_src_types               &
              ,LEVSIZ, N_OZMIXM                    &
              ,N_AEROSOLC                                    &
              ,PAERLEV   ,ID                                 &
              ,CAM_ABS_DIM1, CAM_ABS_DIM2 &
              ,CAM_ABS_FREQ_S                         &
              ,XTIME                                           &
              ,CURR_SECS, ADAPT_STEP_FLAG       &
              !BSINGH - For WRFCuP scheme (optional args)
              ,cu_physics,shallowcu_forced_ra   & !CuP, wig 1-Oct-2006
              ,cubot,cutop,cldfra_cup           & !CuP, wig 1-Oct-2006
              ,shall                            & !CuP, wig 4-Feb-2008
              !BSINGH - ENDS
            ! indexes
              ,IDS,IDE, JDS,JDE, KDS,KDE          &
              ,IMS,IME, JMS,JME, KMS,KME          &
              ,i_start,i_end          &
              ,j_start,j_end          &
              ,kts, kte                          &
              ,num_tiles                                   &
            ! Optional
              , TLWDN, TLWUP                        & ! goddard schemes
              , SLWDN, SLWUP                        & ! goddard schemes
              , TSWDN, TSWUP                        & ! goddard schemes
              , SSWDN, SSWUP                        & ! goddard schemes
              , RE_CLOUD_GSFC                       & ! goddard
              , RE_RAIN_GSFC                        & ! goddard
              , RE_ICE_GSFC                         & ! goddard
              , RE_SNOW_GSFC                        & ! goddard
              , RE_GRAUPEL_GSFC                     & ! goddard
              , RE_HAIL_GSFC                        & ! goddard
              , COD2D_OUT                           & ! goddardrad
              , CTOP2D_OUT                          & ! goddardrad
              , CLDFRA,CLDFRA_MP_ALL,CLDT,ZNU       &
#if (EM_CORE == 1)
              , lradius,iradius                     &
#endif
              , cldfra_dp, cldfra_sh                & ! ckay for sub-grid cloud fraction
              , re_cloud, re_ice, re_snow           & ! G. Thompson
              , has_reqc, has_reqi, has_reqs        & ! G. Thompson
              , PB                                                &
              , F_ICE_PHY,F_RAIN_PHY       &
              , F_QNC                      &
              , QNC_CURR                   &
              , QV, F_QV                     &
              , QC, F_QC                     &
              , QR, F_QR                     &
              , QI, F_QI                     &
              , QI2, F_QI2                   & ! for P3
              , QI3, F_QI3                   & ! for Jensen ISHMAEL
              , QS, F_QS                     &
              , QG, F_QG                     &
              , QH, F_QH                     &  !goddardrad
              , QNDROP, F_QNDROP    &
              ,QNIFA,F_QNIFA                  &   ! trude
              ,QNWFA,F_QNWFA                  &   ! trude
              ,ACSWUPT   ,ACSWUPTC            &
              ,ACSWDNT   ,ACSWDNTC            &
              ,ACSWUPB   ,ACSWUPBC            &
              ,ACSWDNB   ,ACSWDNBC            &
              ,ACLWUPT   ,ACLWUPTC            &
              ,ACLWDNT   ,ACLWDNTC            &
              ,ACLWUPB   ,ACLWUPBC            &
              ,ACLWDNB   ,ACLWDNBC            &
              ,SWUPT ,SWUPTC, SWUPTCLN        &
              ,SWDNT ,SWDNTC, SWDNTCLN        &
              ,SWUPB ,SWUPBC, SWUPBCLN        &
              ,SWDNB ,SWDNBC, SWDNBCLN        &
              ,LWUPT ,LWUPTC, LWUPTCLN        &
              ,LWDNT ,LWDNTC, LWDNTCLN        &
              ,LWUPB ,LWUPBC, LWUPBCLN        &
              ,LWDNB ,LWDNBC, LWDNBCLN        &
              ,LWCF                           &
              ,SWCF                           &
              ,OLR                            &
              ,aerodm, PINA, aodtot           &
              ,OZMIXM, PIN                    &
              ,M_PS_1, M_PS_2, AEROSOLC_1     &
              ,AEROSOLC_2, M_HYBI0            &
              ,ABSTOT, ABSNXT, EMSTOT         &
              ,ICLOUD_CU                      &
              ,CALC_CLEAN_ATM_DIAG            &
              ,AER_RA_FEEDBACK                &
              ,QC_CU , QI_CU                  &
              ,icloud_bl,qc_bl,cldfra_bl     & !JOE-subgrid bl clouds
              ,PM2_5_DRY, PM2_5_WATER         &
              ,PM2_5_DRY_EC                   &
              ,TAUAER300, TAUAER400 & ! jcb
              ,TAUAER600, TAUAER999 & ! jcb
              ,GAER300, GAER400, GAER600, GAER999 & ! jcb
              ,WAER300, WAER400, WAER600, WAER999 & ! jcb
              ,TAUAERlw1,  TAUAERlw2  &
              ,TAUAERlw3,  TAUAERlw4  &
              ,TAUAERlw5,  TAUAERlw6  &
              ,TAUAERlw7,  TAUAERlw8  &
              ,TAUAERlw9,  TAUAERlw10   &
              ,TAUAERlw11, TAUAERlw12   &
              ,TAUAERlw13, TAUAERlw14   &
              ,TAUAERlw15, TAUAERlw16  &
              ,progn                                            &
              ,slope_rad,topo_shading     &
              ,shadowmask,ht,dx,dy                 &
              ,dxkm                                                       &
              ,diffuse_frac               &
              ,SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC                          & ! Optional
              ,LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC                          & ! Optional
              ,radtacttime                                                &
              ,ALSWVISDIR, ALSWVISDIF, ALSWNIRDIR, ALSWNIRDIF             & !fds ssib alb comp (06/2010)
              ,SWVISDIR, SWVISDIF, SWNIRDIR, SWNIRDIF                     & !fds ssib swr comp (06/2010)
              ,SF_SURFACE_PHYSICS, IS_CAMMGMP_USED                        & !fds
              ,EXPLICIT_CONVECTION                                        & ! .true.=no conv. scheme
              ,swddir,swddni,swddif                                       & ! jararias 2013/08
              ,swddirc,swddnic                                            & ! PAJ: clear-sky direct irradiance
              ,swdown_ref,swddir_ref,coszen_ref,Gx,gg,Bx,bb               &
              ,aer_type                                                   & ! jararias 2013/11
              ,aer_aod550_opt, aer_aod550_val                             &
              ,aer_angexp_opt, aer_angexp_val                             &
              ,aer_ssa_opt, aer_ssa_val                                   &
              ,aer_asy_opt, aer_asy_val                                   &
              ,aod5502d, angexp2d, aerssa2d, aerasy2d                     &
              ,aod5503d                                                   &
              ,cw_rad, shcu_physics                      &
              ,taod5502d, taod5503d                                       & !  Trude
              ,mp_physics                                                 &
              ,EFCG,EFCS,EFIG,EFIS,EFSG,aercu_opt                         &
              ,EFSS,QS_CU                                                 &
#if (WRF_CHEM == 1)
              ,chem                                                       & ! inline gocart
              ,aod_out                                                    & ! optional gocart
              , AOD2D_OUT                                                 & ! goddardrad
              , ATOP2D_OUT                                                & ! goddardrad
              ,chem_opt                                                   &
              ,gsfcrad_gocart_coupling                                    &
#endif
                                                                          )


!-------------------------------------------------------------------------

! !USES:
   USE module_state_description, ONLY : RRTMSCHEME, GFDLLWSCHEME        &
                                       ,RRTMG_LWSCHEME, RRTMG_SWSCHEME  &
#if( BUILD_RRTMG_FAST == 1)
                                       ,RRTMG_LWSCHEME_FAST, RRTMG_SWSCHEME_FAST  &
#endif
                                       ,RRTMK_LWSCHEME, RRTMK_SWSCHEME  & 
                                       ,SWRADSCHEME, GSFCSWSCHEME       &
                                       ,GFDLSWSCHEME, CAMLWSCHEME, CAMSWSCHEME &
                                       ,HELDSUAREZ                      &
#if ( HWRF == 1 )
                                       ,HWRFSWSCHEME, HWRFLWSCHEME  &
#endif
                                       ,goddardswscheme             & !NUWRF
                                       ,goddardlwscheme             & !NUWRF
# if (EM_CORE == 1)
                                       ,CAMMGMPSCHEME                   &
#if (WRF_CHEM == 1)

                                       ,num_chem             & !NUWRF
                                       ,p_bc1, p_bc2, p_oc1, p_oc2  & !NUWRF
                                       ,p_dust_1, p_dust_2, p_dust_3 & !NUWRF
                                       ,p_dust_4, p_dust_5          & !NUWRF
                                       ,p_sulf, p_seas_1, p_seas_2  & !NUWRF
                                       ,p_seas_3, p_seas_4          & !NUWRF
#endif
#endif
                                       ,KFCUPSCHEME                     & !BSINGH - Added KFCUPSCHEME for WRFCuP scheme
                                       ,FLGLWSCHEME, FLGSWSCHEME

   USE module_model_constants
#ifndef HWRF
   USE module_wrf_error , ONLY : wrf_err_message
#endif
   USE module_state_description, ONLY : nuwrf4icescheme

! *** add new modules of schemes here

   USE module_ra_sw         , ONLY : swrad
   USE module_ra_gsfcsw     , ONLY : gsfcswrad
   USE module_ra_rrtm       , ONLY : rrtmlwrad
   USE module_ra_rrtmg_lw   , ONLY : rrtmg_lwrad
   USE module_ra_rrtmg_sw   , ONLY : rrtmg_swrad
#if( BUILD_RRTMG_FAST == 1)
   USE module_ra_rrtmg_lwf  , ONLY : rrtmg_lwrad_fast
   USE module_ra_rrtmg_swf  , ONLY : rrtmg_swrad_fast
#endif
   USE module_ra_rrtmg_swk  , ONLY : rad_rrtmg_driver
   USE module_ra_cam        , ONLY : camrad
   USE module_ra_gfdleta    , ONLY : etara
#if ( HWRF == 1 )
   USE module_ra_hwrf
#endif
   USE module_ra_hs         , ONLY : hsrad
   
   USE module_ra_goddard    , ONLY : goddardrad
   USE module_ra_flg        , ONLY : RAD_FLG
   
   USE module_ra_aerosol    , ONLY : calc_aerosol_goddard_sw, &
                                     calc_aerosol_rrtmg_sw

   !  This driver calls subroutines for the radiation parameterizations.
   !
   !  short wave radiation choices:
   !  1. swrad (19??)
   !  4. rrtmg_sw - Added November 2008, MJIacono, AER, Inc.
   !
   !  long wave radiation choices:
   !  1. rrtmlwrad
   !  4. rrtmg_lw - Added November 2008, MJIacono, AER, Inc.
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
!-- RLWTOA        upward long wave at top of atmosphere (w/m2)
!-- RSWTOA        upward short wave at top of atmosphere (w/m2)
!-- XLAT          latitude, south is negative (degree)
!-- XLONG         longitude, west is negative (degree)
!-- ALBEDO        albedo (between 0 and 1)
!-- CLDFRA        cloud fraction (between 0 and 1)
!-- CLDFRA_DP     cloud fraction from deep cloud in a cumulus scheme
!-- CLDFRA_SH     cloud fraction from shallow cloud in a cumulus scheme
!-- CLDFRA_MP_ALL cloud fraction from CAMMGMP microphysics scheme
!-- EMISS         surface emissivity (between 0 and 1)
!-- rho_phy       density (kg/m^3)
!-- rr            dry air density (kg/m^3)
!-- moist         moisture array (4D - last index is species) (kg/kg)
!-- n_moist       number of moisture species
!-- qndrop        Cloud droplet number (#/kg)
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
   LOGICAL, OPTIONAL, INTENT(IN) :: explicit_convection
   LOGICAL :: expl_conv
   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                                         kts,kte, &
                                       num_tiles

   INTEGER, INTENT(IN)            :: lw_physics, sw_physics, mp_physics
   INTEGER, INTENT(IN)            :: o3input, aer_opt
   INTEGER, INTENT(IN)            :: id
   integer, intent(in)            :: swint_opt

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                       &
                i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   STEPRA,ICLOUD,ra_call_offset
   INTEGER,      INTENT(IN   )    ::   cldovrlp                  ! J. Henderson AER: cldovrlp namelist value
   INTEGER,      INTENT(IN   )    ::   alevsiz, no_src_types
   INTEGER,      INTENT(IN   )    ::   levsiz, n_ozmixm
   INTEGER,      INTENT(IN   )    ::   paerlev, n_aerosolc, cam_abs_dim1, cam_abs_dim2
   REAL,      INTENT(IN   )       ::   cam_abs_freq_s

   LOGICAL,      INTENT(IN   )    ::   warm_rain
   INTEGER,      INTENT(IN   )    ::   cu_physics                !CuP, wig 5-Oct-2006 !BSINGH - For WRFCuP scheme
   !BSINGH - For WRFCuP scheme
   LOGICAL, OPTIONAL, INTENT(IN)  :: shallowcu_forced_ra          !CuP, wig
   !BSINGH -ENDS
   LOGICAL,      INTENT(IN   )    ::   is_CAMMGMP_used !BSINGH:01/31/2013: Added for CAM5 RRTMG

   REAL,      INTENT(IN   )       ::   RADT

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                 XLAND, &
                                                            XICE, &
                                                             TSK, &
                                                          VEGFRA, &
                                                            SNOW 
   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  OZMIXM
   REAL,  DIMENSION( ims:ime, alevsiz, jms:jme, no_src_types, n_ozmixm-1 ),  OPTIONAL,    &
          INTENT(IN   ) ::                                  AERODM
   REAL,  DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types ),  OPTIONAL,    &
          INTENT(INOUT) ::                                  AEROD
   REAL,  DIMENSION( ims:ime, jms:jme ), OPTIONAL,                &
          INTENT(INOUT) ::                                  AODTOT

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, n_ozmixm ) :: OZFLG

   REAL,  DIMENSION(levsiz), OPTIONAL, INTENT(IN )  ::     PIN
   REAL,  DIMENSION(alevsiz), OPTIONAL, INTENT(IN )  ::     PINA

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
   !BSINGH - For WRFCuP scheme
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL,                 &
        INTENT(INOUT)  ::                                         &
                                                           cutop, & !CuP, wig 1-Oct-2006
                                                           cubot, & !CuP, wig 1-Oct-2006
                                                           shall    !CuP, wig 4-Feb-2008
   !BSINGH -ENDS


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

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(IN   )  ::                       cw_rad
   INTEGER, OPTIONAL,                INTENT(IN) ::  shcu_physics

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,        &
        INTENT(IN)     ::                            EFCG,       & 
                                                     EFCS,       &
                                                     EFIG,       &
                                                     EFIS,       &
                                                     EFSG,       &
                                                     EFSS

   !BSINGH - For WRFCuP scheme
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,        &
        INTENT(INOUT ) ::                            cldfra_cup     !CuP, wig 1-Oct-2006


      !BSINGH -ENDS

!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaer300,tauaer400,tauaer600,tauaer999, & ! jcb
                                 gaer300,gaer400,gaer600,gaer999, & ! jcb
                                 waer300,waer400,waer600,waer999

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(IN ) ::          qc_cu, qi_cu

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::          qc_bl, qs_cu

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL ,       &
         INTENT(IN ) ::  tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4, & ! czhao 
                         tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8, & ! czhao 
                         tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12, & ! czhao 
                         tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16

   INTEGER, INTENT(IN) :: icloud_cu

   INTEGER, INTENT(IN   ), OPTIONAL  ::   icloud_bl
   INTEGER, INTENT(IN   ), OPTIONAL  ::   aer_ra_feedback
   INTEGER, INTENT(IN   )  ::   calc_clean_atm_diag

!jdfcz   INTEGER, OPTIONAL, INTENT(IN   )    :: progn,prescribe
   INTEGER, OPTIONAL, INTENT(IN   )    :: progn
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

! TOA and surface, upward and downward, total, clear (no cloud), and clean (no aerosol) fluxes
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
              SWUPT,  SWUPTC, SWUPTCLN,  SWDNT,  SWDNTC, SWDNTCLN,&
              SWUPB,  SWUPBC, SWUPBCLN,  SWDNB,  SWDNBC, SWDNBCLN,&
              LWUPT,  LWUPTC, LWUPTCLN,  LWDNT,  LWDNTC, LWDNTCLN,&
              LWUPB,  LWUPBC, LWUPBCLN,  LWDNB,  LWDNBC, LWDNBCLN


! Upward and downward, total and clear sky layer fluxes (W m-2)
   REAL, DIMENSION( ims:ime, kms:kme+2, jms:jme ),                &
         OPTIONAL, INTENT(INOUT) ::                               &
                               SWUPFLX,SWUPFLXC,SWDNFLX,SWDNFLXC, &
                               LWUPFLX,LWUPFLXC,LWDNFLX,LWDNFLXC

   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(INOUT)  ::                                  SWCF, &
                                                            LWCF, &
                                                             OLR
! ---- fds (06/2010) ssib alb components ------------
   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(IN   )  ::                            ALSWVISDIR, &
                                                      ALSWVISDIF, &
                                                      ALSWNIRDIR, &
                                                      ALSWNIRDIF
! ---- fds (06/2010) ssib swr components ------------
   REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL ,       &
         INTENT(OUT  )  ::                              SWVISDIR, &
                                                        SWVISDIF, &
                                                        SWNIRDIR, &
                                                        SWNIRDIF
   INTEGER,    OPTIONAL, INTENT(IN   )    ::  sf_surface_physics
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

! ------------------------------------------------------------------------------ jararias 2013/08/10 -----------
   REAL, DIMENSION( ims:ime, jms:jme ),  INTENT(OUT) :: swddir, & ! All-sky SW broadband surface direct irradiance
                                                        swddni, & ! All-sky SW broadband surface direct normal irradiance
                                                        swddif    ! All-sky SW broadband surface diffuse irradiance
   REAL, DIMENSION( ims:ime, jms:jme ),  INTENT(INOUT) :: Gx,Bx,gg,bb, & ! For SW sza-interpolation
                                                          swdown_ref,  &
                                                          swddir_ref,  &
                                                          coszen_ref
! ------------------------------------------------------------------------------ jararias 2013/11    -----------
    INTEGER,                             INTENT(IN)    :: aer_type,       & ! rural, urban, maritime, ...
                                                          aer_aod550_opt, & ! input option for AOD at 550 nm
                                                          aer_angexp_opt, & ! input option for aerosol Angstrom exponent
                                                          aer_ssa_opt,    & ! input option for aerosol ssa
                                                          aer_asy_opt,    & ! input option for aerosol asy
                                                          aercu_opt         !
    REAL,                                INTENT(IN)    :: aer_aod550_val, & ! AOD at 550 nm if aer_aod550_opt=1
                                                          aer_angexp_val, & ! aerosol Angstrom exponent if aer_angexp_opt=1
                                                          aer_ssa_val,    & ! aerosol ssa if aer_ssa_opt=1
                                                          aer_asy_val       ! aerosol asy if aer_asy_opt=1
    REAL, DIMENSION( ims:ime, jms:jme ),          OPTIONAL,               &
          INTENT(INOUT)                                :: aod5502d,       & ! gridded AOD at 550 nm from auxinput
                                                          angexp2d,       & ! gridded aerosol Angstrom exponent from auxinput
                                                          aerssa2d,       & ! gridded aerosol ssa from auxinput
                                                          aerasy2d          ! gridded aerosol asy from auxinput
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL,               &
          INTENT(OUT)                                  :: aod5503d   ! 3D AOD at 550 nm

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme), OPTIONAL:: taod5503d         !  Trude
    REAL, DIMENSION(ims:ime,jms:jme), OPTIONAL:: taod5502d                 !  Trude
!
   REAL, INTENT(IN  )   ::                                GMT,dt, &
                                                   julian, xtime
   INTEGER, INTENT(IN  ),OPTIONAL ::                          YR
!
   INTEGER, INTENT(IN  ) ::                    JULDAY, itimestep
   REAL, INTENT(IN ),OPTIONAL     ::                    CURR_SECS
   LOGICAL, INTENT(IN ),OPTIONAL  ::              ADAPT_STEP_FLAG

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

! NUWRF JJS 20101021 vvvvv
! for inline Gocart coupling
#if( WRF_CHEM == 1)

 REAL, OPTIONAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem), &
         INTENT(IN)               ::  chem
 real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(out) :: aod_out  !Aeorosol Optical Depth

 real, dimension( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT) :: &   !goddardrad
      aod2d_out    ,&  ! column aerosol optical depth
      atop2d_out       ! aerosol top pressure [mb]  


 integer :: i24h
 INTEGER, PARAMETER :: num_go = 14  ! number of the gocart aerosol species
 REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_go) :: aero
 REAL, PARAMETER :: frac(4)=(/ 0.01053,0.08421,0.25263,0.65263 /) !fraction for fine dust
 integer,intent(in) :: chem_opt ! EMK
 integer,intent(in) :: gsfcrad_gocart_coupling ! EMK

#endif
! NUWRF JJS 20101021 ^^^^^

! JJS 20090623  vvvvv
! Optional, only for Goddard LW and SW
   REAL, DIMENSION(IMS:IME, JMS:JME, 1:8)       :: ERBE_out  !extra output for SDSU
   REAL, DIMENSION(IMS:IME, JMS:JME), OPTIONAL, INTENT(INOUT) ::   & !BSINGH(PNNL)- Lahey compiler forced this specification to be intent-inout
                                               TLWDN, TLWUP,     &
                                               SLWDN, SLWUP,     &
                                               TSWDN, TSWUP,     &
                                               SSWDN, SSWUP        ! for Goddard schemes
! NUWRF JJS 20090623  ^^^^^

! NUWRF JJS 20140225  vvvvv
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),               OPTIONAL ,&
      INTENT(INOUT)  :: re_cloud_gsfc, re_rain_gsfc, re_ice_gsfc,        &
                        re_snow_gsfc, re_graupel_gsfc, re_hail_gsfc 
! NUWRF JJS 20140225  ^^^^^

  real, dimension( ims:ime, jms:jme, 1:4 )  :: sflxd    !NUWRF SW only for LIS

!   REAL, DIMENSION(IMS:IME, JMS:JME, 1:4)    :: flxd  !NUWRF extra radiation output for LIS (CLM)
       ! 1-surface downward UV+VIS beam radiation [W/m2]
       ! 2-surface downward UV+VIS diffuse radiation [W/m2]
       ! 3-surface downward NIR beam radiation [W/m2]
       ! 4-surface downward NIR diffuse radiation [W/m2]


 real, dimension( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) :: &   !goddardrad
      cod2d_out    ,&  ! column optical depth
      ctop2d_out       ! cloud top pressure [mb]  

! Added by ZCX for low and total cloud fraction
   REAL, DIMENSION( kms:kme ), OPTIONAL, INTENT(IN)   :: znu      ! eta values on half (mass)levels
   REAL, DIMENSION( ims:ime, jms:jme ),  OPTIONAL, INTENT(INOUT) ::         &
                                                         cldt

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

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  & ! ckay for sub-grid cloud fraction
         OPTIONAL,                                                &
         INTENT(INOUT) ::                              cldfra_dp, &
                                                       cldfra_sh, &
                                                       cldfra_bl

!..G. Thompson
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT):: re_cloud, re_ice, re_snow
   INTEGER, INTENT(INOUT):: has_reqc, has_reqi, has_reqs

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY, &
                                                      CLDFRA_MP_ALL

#if (EM_CORE == 1)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                            LRADIUS, &
                                                            IRADIUS
#endif

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         OPTIONAL,                                                &
         INTENT(OUT) ::                                   SWDOWNC, SWDDIRC, SWDDNIC
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT ) ::                                        &
                                                               pb &
                                        ,qv,qc,qr,qi,qs,qg,qh,qndrop,      &
                                                      qnifa,qnwfa,      & ! Trude
                                                      qi2,qi3        ! for P3

   LOGICAL, OPTIONAL ::     f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qh,f_qndrop,&
                                                f_qnifa,f_qnwfa,        &  ! trude
                                                f_qi2,f_qi3            ! for P3
!shbaek
   real, dimension ( ims:ime, kms:kme, jms:jme ), optional, intent(in) :: qnc_curr
   LOGICAL, OPTIONAL :: f_qnc
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         OPTIONAL,                                                &
         INTENT(INOUT)  ::                       taucldi,taucldc

     REAL, OPTIONAL, INTENT(IN) :: dxkm

! Variables for slope-dependent radiation

     REAL, OPTIONAL, INTENT(IN) :: dx,dy
     INTEGER, OPTIONAL, INTENT(IN) :: slope_rad,topo_shading
     REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN)  :: ht
     INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN) :: shadowmask
     REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(OUT) :: diffuse_frac

   REAL , OPTIONAL, INTENT(INOUT) ::    radtacttime ! Storing the time in s when radiation is called next
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
         INTENT(INOUT)  ::                       o3rad

   ! vert nesting
   REAL, OPTIONAL , INTENT(IN   ) :: p_top
   REAL                           :: p_top_dummy

! LOCAL  VAR
   INTEGER, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    cldfra1_flag
   REAL, DIMENSION( ims:ime, jms:jme ) ::             GLAT,GLON
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    CEMISS
   REAL, DIMENSION( ims:ime, jms:jme ) ::             coszr
   REAL, DIMENSION( ims:ime, levsiz, jms:jme )  ::    ozmixt
   REAL, DIMENSION( ims:ime, alevsiz, jms:jme, 1:no_src_types )  ::    aerodt

   REAL    ::    DECLIN,SOLCON,XXLAT,TLOCTM,XT24, CEN_LAT, cldfra_cup_mod
   INTEGER ::    i,j,k,its,ite,jts,jte,ij
   INTEGER ::    STEPABS
   LOGICAL ::    gfdl_lw,gfdl_sw, compute_cldfra_cup
   LOGICAL ::    doabsems
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   INTEGER ::    s

   REAL    ::    OBECL,SINOB,SXLONG,ARG,DECDEG,                  &
                 DJUL,RJUL,ECCFAC
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qc_temp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_save,qc_save
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qs_save

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qc_cu_weight
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qi_cu_weight
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: qs_cu_weight

   REAL    ::    gridkm, Wice,Wh2o
   REAL, DIMENSION(kms:kme):: t_1d, p_1d, Dz_1d, qv_1d, qc_1d, qi_1d, qs_1d, cf_1d

   REAL    ::    next_rad_time, DTaccum
   LOGICAL ::    run_param , doing_adapt_dt , decided
   LOGICAL ::    flg_lw, flg_sw
!ZCX
   REAL    ::    cldji,cldlji
!ckay
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) ::    cldfra_cu
!------------------------------------------------------------------
! solar related variables are added to declaration
!-------------------------------------------------
   REAL, OPTIONAL, INTENT(OUT) :: DECLINX,SOLCONX
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: COSZEN
   REAL, OPTIONAL, DIMENSION( ims:ime, jms:jme), INTENT(OUT) :: HRANG
!------------------------------------------------------------------

! jararias, 2013/08/10
   real :: ioh,kt,airmass,kd
   real, dimension(ims:ime,jms:jme) :: coszen_loc,hrang_loc
! jararias 2013/11 but modified on 2016/2/10
!  the following three arrays may be dimensioned by (ims,ime,kms,kme,jms,jme,aerosol_vars)
   real, dimension(:,:,:,:), pointer :: tauaer_sw=>null(), ssaaer_sw=>null(), asyaer_sw=>null()

! Trude AOD variables
     INTEGER, PARAMETER:: taer_type = 1                                  !  rural, urban, maritime, ...
     INTEGER, PARAMETER:: taer_aod550_opt = 2                            !  input option for AOD at 550 nm
     INTEGER, PARAMETER:: taer_angexp_opt = 3                            !  input option for aerosol Angstrom exponent
     INTEGER, PARAMETER:: taer_ssa_opt = 3                               !  input option for aerosol ssa
     INTEGER, PARAMETER:: taer_asy_opt = 3                               !  input option for aerosol asy


#if ( HWRF == 1 )
   CHARACTER(len=255) :: wrf_err_message
#endif

!---------- local test vars 
!   real, dimension(ims:ime, kms:kme, jms:jme) ::  hlw, hsw

   ! This allows radiation schemes (mainly HWRF) to correctly
   ! interface with the convection scheme when convection is only
   ! enabled in some domains:
   if(present(explicit_convection)) then
      expl_conv=explicit_convection
   else
      expl_conv=.true. ! backward compatibility for ARW
   endif

   IF ( ICLOUD == 3 ) THEN
      IF (PRESENT(dxkm)) then
         gridkm = 1.414*SQRT(dxkm*dxkm + dy*0.001*dy*0.001)
      ELSE IF (PRESENT(dx)) then
         gridkm = SQRT(dx*0.001*dx*0.001 + dy*0.001*dy*0.001)
      endif

      if (itimestep .LE. 100) then
        WRITE ( wrf_err_message , * ) 'Grid spacing in km ', dx, dy, gridkm
        CALL wrf_debug (100, wrf_err_message)
      endif
   END IF

   CALL wrf_debug (1, 'Top of Radiation Driver')
!  WRITE ( wrf_err_message , * ) 'itimestep = ',itimestep,', dt = ',dt,', lw and sw options = ',lw_physics,sw_physics
!  CALL wrf_debug (1, wrf_err_message )
   if (lw_physics .eq. 0 .and. sw_physics .eq. 0)         return

! amontornes-bcodina (2014-05-02) :: improving the namelist settings consistency for the FLG scheme
!  if (lw_physics .ne. FLGLWSCHEME .and. sw_physics .eq. FLGSWSCHEME)  then
!      call wrf_error_fatal('SW and LW schemes are in conflict. SW is FLG and LW is a different scheme!')
!  end if
!  if (lw_physics .eq. FLGLWSCHEME .and. sw_physics .ne. FLGSWSCHEME) then
!      call wrf_error_fatal('SW and LW schemes are in conflict. LW is FLG and SW is a different scheme!')
!  end if

! ra_call_offset = -1 gives old method where radiation may be called just before output
! ra_call_offset =  0 gives new method where radiation may be called just after output
!                     and is also consistent with removal of offset in new XTIME
! also need to account for stepra=1 which always has zero modulo output

   doing_adapt_dt = .FALSE.
   IF ( PRESENT(adapt_step_flag) ) THEN
      IF ( adapt_step_flag ) THEN
         doing_adapt_dt = .TRUE.
         IF ( radtacttime .eq. 0. ) THEN
            radtacttime = CURR_SECS + radt*60.
         END IF
      END IF
   END IF

!  Do we run through this scheme or not?

!    Test 1:  If this is the initial model time, then yes.
!                ITIMESTEP=1
!    Test 2:  If the user asked for the radiation to be run every time step, then yes.
!                RADT=0 or STEPRA=1
!    Test 3:  If not adaptive dt, and this is on the requested radiation frequency, then yes.
!                MOD(ITIMESTEP,STEPRA)=0 (or 1, depending on the offset setting)
!    Test 4:  If using adaptive dt and the current time is past the last requested activate radiation time, then yes.
!                CURR_SECS >= RADTACTTIME

!  If we do run through the scheme, we set the flag run_param to TRUE and we set the decided flag
!  to TRUE.  The decided flag says that one of these tests was able to say "yes", run the scheme.
!  We only proceed to other tests if the previous tests all have left decided as FALSE.

!  If we set run_param to TRUE and this is adaptive time stepping, we set the time to the next
!  radiation run.

   run_param = .FALSE.
   decided = .FALSE.
   IF ( ( .NOT. decided ) .AND. &
        ( itimestep .EQ. 1 ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( ( radt .EQ. 0. ) .OR. ( stepra .EQ. 1 ) ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( .NOT. doing_adapt_dt ) .AND. &
        ( MOD(itimestep,stepra) .EQ. 1+ra_call_offset ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
   END IF

   IF ( ( .NOT. decided ) .AND. &
        ( doing_adapt_dt ) .AND. &
        ( curr_secs .GE. radtacttime ) ) THEN
      run_param   = .TRUE.
      decided     = .TRUE.
      radtacttime = curr_secs + radt*60
   END IF

   if(swint_opt.eq.1) then
      DO ij = 1 , num_tiles
         its = i_start(ij)
         ite = i_end(ij)
         jts = j_start(ij)
         jte = j_end(ij)
         CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                       DEGRAD,DPD                                )
         call calc_coszen(ims,ime,jms,jme,its,ite,jts,jte, &
                          julian,xtime,gmt,declin,degrad,  &
                          xlong,xlat,coszen_loc,hrang_loc)
      end do
   end if

   Radiation_step: IF ( run_param ) then

! CAM-specific additional radiation frequency - cam_abs_freq_s (=21600s by default)
     STEPABS = nint(cam_abs_freq_s/(dt*STEPRA))*STEPRA
     IF (itimestep .eq. 1 .or. mod(itimestep,STEPABS) .eq. 1 + ra_call_offset &
                                        .or. STEPABS .eq. 1 ) THEN
       doabsems = .true.
     ELSE
       doabsems = .false.
     ENDIF
   IF (PRESENT(adapt_step_flag)) THEN
     IF ((adapt_step_flag)) THEN
       IF ( (itimestep .EQ. 1) .OR. (cam_abs_freq_s .EQ. 0) .OR. &
           ( CURR_SECS + dt >= ( INT( CURR_SECS / ( cam_abs_freq_s ) + 1 ) * cam_abs_freq_s) ) ) THEN
         doabsems = .true.
       ELSE
         doabsems = .false.
       ENDIF
     ENDIF
   ENDIF

   gfdl_lw = .false.
   gfdl_sw = .false.
   flg_lw = .false.
   flg_sw = .false.

! Allocate aerosol arrays used by aer_opt = 2 option
   IF ( PRESENT( AOD5502D ) ) THEN
     ! jararias, 2013/11
     IF ( aer_opt .EQ. 2 ) THEN
        swrad_aerosol_select: select case(sw_physics)

           case(GODDARDSWSCHEME)
              allocate(tauaer_sw(ims:ime, kms:kme, jms:jme, 1:11))
              allocate(ssaaer_sw(ims:ime, kms:kme, jms:jme, 1:11))
              allocate(asyaer_sw(ims:ime, kms:kme, jms:jme, 1:11))

           case(RRTMG_SWSCHEME,&
#if( BUILD_RRTMG_FAST == 1)
                RRTMG_SWSCHEME_FAST,&
#endif
                RRTMK_SWSCHEME)
              allocate(tauaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
              allocate(ssaaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
              allocate(asyaer_sw(ims:ime, kms:kme, jms:jme, 1:14))

        end select swrad_aerosol_select
     ENDIF
   ENDIF

! Allocate aerosol arrays used by aer_opt = 3 option, and explicit AOD from QNWFA+QNIFA   (Trude)
   IF (PRESENT(f_qnwfa) .AND. PRESENT(f_qnifa) .AND. PRESENT(taod5503d) .AND.  PRESENT(taod5502d)) THEN
      IF (F_QNWFA .AND. aer_opt.eq.3 .AND.                              &
                 (sw_physics.eq.RRTMG_SWSCHEME .OR.                     &
#if( BUILD_RRTMG_FAST == 1)
                  sw_physics.eq.RRTMG_SWSCHEME_FAST .OR.                &
#endif
                  sw_physics.eq.RRTMK_SWSCHEME )) THEN
         CALL wrf_debug (150, 'DEBUG-GT:  computing 3D AOD from QNWFA+QNIFA')

         allocate(tauaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
         allocate(ssaaer_sw(ims:ime, kms:kme, jms:jme, 1:14))
         allocate(asyaer_sw(ims:ime, kms:kme, jms:jme, 1:14))

         !$OMP PARALLEL DO   &
         !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)
         DO ij = 1 , num_tiles
           its = i_start(ij)
           ite = i_end(ij)
           jts = j_start(ij)
           jte = j_end(ij)

           do j=jts,jte
              do i=its,ite
                 taod5502d(i,j) = 0.0
              end do
           end do

           call gt_aod (p, DZ8W, t, qv, qnwfa, qnifa, taod5503d,        &
                    ims,ime, jms,jme, kms,kme,its,ite, jts,jte, kts,kte) 

           do j=jts,jte
              do i=its,ite
                 do k=kts,kte
                    taod5502d(i,j) = taod5502d(i,j) + taod5503d(i,k,j)
                 end do
              end do
           end do
         ENDDO
         !$OMP END PARALLEL DO
      ENDIF
   ENDIF

!---------------
! Calculate constant for short wave radiation
! moved up and out of OMP loop because it only needs to be computed once
! and because it is not entirely thread-safe (XT24, TOLOCTM and XXLAT need
! their thread-privacy)  JM 20100217
   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)
     CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,               &
                   DEGRAD,DPD                                )

     IF(PRESENT(declinx).AND.PRESENT(solconx))THEN
! saved to state arrays used in surface driver
       declinx=declin
       solconx=solcon
     ENDIF

! added coszen subroutine : jararias, 2013/08/10
!   outputs: coszen, hrang
     call calc_coszen(ims,ime,jms,jme,its,ite,jts,jte,  &
                      julian,xtime+radt*0.5,gmt, &
                      declin,degrad,xlong,xlat,coszen,hrang)
   ENDDO

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

! initialize data

     if ((itimestep.eq.1).and.(swint_opt.eq.1)) then
        do j=jts,jte
           do i=its,ite
              Bx(i,j)=0.
              bb(i,j)=0.
              Gx(i,j)=0.
              gg(i,j)=0.
           end do
        end do
     end if

     DO j=jts,jte
     DO i=its,ite
        GSW(I,J)=0.
        GLW(I,J)=0.
        SWDOWN(I,J)=0.
        swddir(i,j)=0.  ! jararias, 2013/08/10
        swddni(i,j)=0.  ! jararias, 2013/08/10
        swddif(i,j)=0.  ! jararias, 2013/08/10
        GLAT(I,J)=XLAT(I,J)*DEGRAD
        GLON(I,J)=XLONG(I,J)*DEGRAD
     ENDDO
     ENDDO
 
     DO j=jts,jte
     DO k=kts,kte+1
     DO i=its,ite
        RTHRATEN(I,K,J)=0.
        RTHRATENLW(I,K,J)=0.
        RTHRATENSW(I,K,J)=0.
        CEMISS(I,K,J)=0.0
     ENDDO
     ENDDO
     ENDDO

     IF ( PRESENT( SWUPFLX ) ) THEN
        DO j=jts,jte
        DO k=kts,kte+2
        DO i=its,ite
           SWUPFLX(I,K,J) = 0.0
           SWDNFLX(I,K,J) = 0.0
           SWUPFLXC(I,K,J) = 0.0
           SWDNFLXC(I,K,J) = 0.0
           LWUPFLX(I,K,J) = 0.0
           LWDNFLX(I,K,J) = 0.0
           LWUPFLXC(I,K,J) = 0.0
           LWDNFLXC(I,K,J) = 0.0
        ENDDO
        ENDDO
        ENDDO
     ENDIF

! backup the incoming hydrometeors

     IF ( F_QC ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qc_save(i,k,j) = qc(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF
     IF ( F_QI ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qi_save(i,k,j) = qi(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF

   IF(aercu_opt.gt.0.0) THEN
     IF ( F_QI ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qs_save(i,k,j) = qs(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF
   END IF


! Fill temporary water variable depending on micro package (tgs 25 Apr 2006)
     if( F_QC ) then
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
! Remove this - to match NAM operational (affects GFDL and HWRF schemes)
!    if( F_QR ) then
!       DO j=jts,jte
!       DO k=kts,kte
!       DO i=its,ite
!          qc_temp(I,K,J) = qc_temp(I,K,J) + qr(I,K,J)
!       ENDDO
!       ENDDO
!       ENDDO
!    endif
!
! temporarily modify hydrometeors (this is for GD scheme and WRF-Chem)
!
     IF ( F_QC .AND. icloud_cu .EQ. 1 ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qc(i,k,j) = qc(i,k,j) + qc_cu(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF
     IF ( F_QI .AND. icloud_cu .EQ. 1 ) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qi(i,k,j) = qi(i,k,j) + qi_cu(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF
#if (EM_CORE == 1)
! temporarily modify hydrometeors (for P3, if 2 cat then add ice from both categories)
!
     IF ( F_QI2) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qi(i,k,j) = qi(i,k,j) + qi2(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF

! for Jensen ISHMAEL, add the third ice species
     IF ( F_QI3) THEN
          DO j=jts,jte
          DO k=kts,kte
          DO i=its,ite
             qi(i,k,j) = qi(i,k,j) + qi3(i,k,j)
          ENDDO
          ENDDO
          ENDDO
     ENDIF
#endif

! these are half level parameters.....so, it should start from kts to kte - 1
!NUWRF JJS 20101021 vvvvv
#if ( WRF_CHEM == 1)
! Pack gocart aerosol species
! All aerosol species in chem are in "ug/kg-dryair"
!  and conerted to (g/m**3)

   aero(:,:,:,:) = 0.
   if ( (chem_opt == 300 .or. chem_opt == 301 .or. &
         chem_opt == 302 .or. chem_opt == 303) .and. &
         (gsfcrad_gocart_coupling == 1) ) then
      do j = jts, jte
         do k = kts, kte  !corrected memory order
            do i = its, ite
               ! aero(i,k,j, 1) = max(0.0, chem(i,k,j,p_sulf)*1.0e-6*rho(i,k,j))             !  1 = SO4
               aero(i,k,j, 1) = max(0.0, chem(i,k,j,p_sulf)*1.0e-6*p(i,k,j)*96.0/(8.314*t(i,k,j)))  ! 1 = SO4
               aero(i,k,j, 2) = max(0.0, (chem(i,k,j,p_bc1)+chem(i,k,j,p_bc2))*1.0e-6*rho(i,k,j))  ! 2 = BC1+BC2
               aero(i,k,j, 3) = max(0.0, chem(i,k,j,p_oc1)*1.0e-6*rho(i,k,j)*1.4e0)          !  3 = OC1
               aero(i,k,j, 4) = max(0.0, chem(i,k,j,p_oc2)*1.0e-6*rho(i,k,j)*1.4e0)          !  4 = OC2
               aero(i,k,j, 5) = max(0.0, chem(i,k,j,p_seas_1)*1.0e-6*rho(i,k,j))           !  5 = SS1
               aero(i,k,j, 6) = max(0.0, (chem(i,k,j,p_seas_2)+chem(i,k,j,p_seas_3)+  &
                    chem(i,k,j,p_seas_4))*1.0e-6*rho(i,k,j))          !  6 = SS2+SS3+SS4
               aero(i,k,j, 7) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(1))   !  7 = DU1 dust mode 1
               aero(i,k,j, 8) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(2))   !  8 = DU1 dust mode 2
               aero(i,k,j, 9) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(3))   !  9 = DU1 dust mode 3
               aero(i,k,j,10) = max(0.0, chem(i,k,j,p_dust_1)*1.0e-6*rho(i,k,j)*frac(4))   ! 10 = DU1 dust mode 4
               aero(i,k,j,11) = max(0.0, chem(i,k,j,p_dust_2)*1.0e-6*rho(i,k,j))           ! 11 = DU2 dust mode 5
               aero(i,k,j,12) = max(0.0, chem(i,k,j,p_dust_3)*1.0e-6*rho(i,k,j))           ! 11 = DU3 dust mode 6
               aero(i,k,j,13) = max(0.0, chem(i,k,j,p_dust_4)*1.0e-6*rho(i,k,j))           ! 11 = DU4 dust mode 7
               aero(i,k,j,14) = max(0.0, chem(i,k,j,p_dust_5)*1.0e-6*rho(i,k,j))           ! 11 = DU5 dust mode 8
            enddo ! i
         enddo ! k
      enddo ! j
   end if ! if (gsfcrad_gocart_coupling == 1)
#endif
!NUWRF JJS 20101021 ^^^^^

! Choose how to compute cloud fraction (since 3.6)
! Initialize to zero 
     DO j=jts,jte
     DO k=kts,kte
     DO i=its,ite
        CLDFRA(i,k,j) = 0.
     END DO
     END DO
     END DO

!---------------
! Calculate constant for short wave radiation

     IF ( ICLOUD == 1 ) THEN

     IF ( F_QC .OR. F_QI ) THEN
! Call to cloud fraction routine based on Randall 1994 (Hong Pan 1998)

        CALL wrf_debug (1, 'CALL cldfra1')
        CALL cal_cldfra1(CLDFRA,qv,qc,qi,qs,               &
                   F_QV,F_QC,F_QI,F_QS,t,p,                &
                   F_ICE_PHY,F_RAIN_PHY,                   &
                   mp_physics,cldfra1_flag,                &
                   ids,ide, jds,jde, kds,kde,              &
                   ims,ime, jms,jme, kms,kme,              &
                   its,ite, jts,jte, kts,kte               )

        IF ( PRESENT ( CLDFRA_DP ) ) THEN
! this is for Kain-Fritsch schemes
          IF ( icloud_cu .EQ. 2 .OR. aercu_opt .GT. 0 ) THEN
             CALL wrf_debug (1, 'use kf cldfra')
             DO j = jts,jte
             DO k = kts,kte
             DO i = its,ite
                cldfra_cu(i,k,j)=cldfra_dp(i,k,j)+cldfra_sh(i,k,j) ! Cu cloud fraction
                CLDFRA(i,k,j)=(1.-cldfra_cu(i,k,j))*CLDFRA(i,k,j)  ! Update resolved cloud fraction for Cu punch-through
                CLDFRA(i,k,j)=CLDFRA(i,k,j)+cldfra_cu(i,k,j)       ! New total cloud fraction
                CLDFRA(i,k,j)=AMIN1(1.0,CLDFRA(i,k,j))
                qc(i,k,j) = qc(i,k,j)+qc_cu(i,k,j)*cldfra_cu(i,k,j)
                qi(i,k,j) = qi(i,k,j)+qi_cu(i,k,j)*cldfra_cu(i,k,j)
             ENDDO
             ENDDO
             ENDDO

             IF (aercu_opt.gt.0.0) THEN
             DO j = jts,jte
             DO k = kts,kte
             DO i = its,ite
                IF (qc(i,k,j).eq.0.0.and.qc_cu(i,k,j).gt.0.0) THEN
                    qc_cu_weight(i,k,j) = 1.0
                ELSE IF (qc(i,k,j).gt.0.0.and.qc_cu(i,k,j).eq.0.0) THEN
                    qc_cu_weight(i,k,j) = 0.0
                ELSE IF (qc(i,k,j).eq.0.0.and.qc_cu(i,k,j).eq.0.0) THEN
                    qc_cu_weight(i,k,j) = 0.0
                ELSE
                    qc_cu_weight(i,k,j) = (qc_cu(i,k,j)*cldfra_cu(i,k,j))/(qc(i,k,j) + qc_cu(i,k,j)*cldfra_cu(i,k,j))
                END IF
                IF (qi(i,k,j).eq.0.0.and.qi_cu(i,k,j).gt.0.0) THEN
                    qi_cu_weight(i,k,j) = 1.0
                ELSE IF (qi(i,k,j).gt.0.0.and.qi_cu(i,k,j).eq.0.0) THEN
                    qi_cu_weight(i,k,j) = 0.0
                ELSE IF (qi(i,k,j).eq.0.0.and.qi_cu(i,k,j).eq.0.0) THEN
                    qi_cu_weight(i,k,j) = 0.0
                ELSE
                    qi_cu_weight(i,k,j) =(qi_cu(i,k,j)*cldfra_cu(i,k,j))/(qi(i,k,j) + qi_cu(i,k,j)*cldfra_cu(i,k,j))
                END IF
                IF (qs(i,k,j).eq.0.0.and.qs_cu(i,k,j).gt.0.0) THEN
                    qs_cu_weight(i,k,j) = 1.0
                ELSE IF (qs(i,k,j).gt.0.0.and.qs_cu(i,k,j).eq.0.0) THEN
                    qs_cu_weight(i,k,j) = 0.0
                ELSE IF (qs(i,k,j).eq.0.0.and.qs_cu(i,k,j).eq.0.0) THEN
                    qs_cu_weight(i,k,j) = 0.0
                ELSE
                    qs_cu_weight(i,k,j)=(qs_cu(i,k,j)*cldfra_cu(i,k,j))/(qs(i,k,j) + qs_cu(i,k,j)*cldfra_cu(i,k,j))
                END IF

! use re_cloud, re_ice and re_snow to store combined effective radii from MSKF and Morrison microphysics
                re_cloud(i,k,j) = EFCS(I,K,J)*qc_cu_weight(I,K,J) &
                                + EFCG(I,K,J)*(1-qc_cu_weight(I,K,J))
                re_cloud(i,k,j) = re_cloud(i,k,j) * 1.E-6
                re_ice(i,k,j)   = EFIS(I,K,J)*qi_cu_weight(I,K,J) &
                                + EFIG(I,K,J)*(1-qi_cu_weight(I,K,J))
                re_ice(i,k,j)   = re_ice(i,k,j) * 1.E-6
                re_snow(i,k,j)  = EFSS(I,K,J)*qs_cu_weight(I,K,J) &
                                + EFSG(I,K,J)*(1-qs_cu_weight(I,K,J))
                re_snow(i,k,j)  = re_snow(i,k,j) * 1.E-6
                has_reqc = 1
                has_reqi = 1
                has_reqs = 1
                qs(i,k,j) = qs(i,k,j)+qs_cu(i,k,j)*cldfra_cu(i,k,j)

             ENDDO
             ENDDO
             ENDDO
             END IF
          ENDIF
        ENDIF

        IF ( PRESENT ( CLDFRA_BL ) .AND.  PRESENT ( QC_BL ) ) THEN
           IF ( icloud_bl > 0 ) THEN
           CALL wrf_debug (1, 'in rad driver; use BL clouds')
           DO j = jts,jte
           DO i = its,ite
           DO k = kts,kte
              IF (qc(i,k,j) < 1.E-6 .AND. qi(i,k,j) < 1.E-6 .AND. CLDFRA_BL(i,k,j)>0.001) THEN
               !Partition the BL clouds into water & ice according to a linear
               !approximation of Hobbs et al. (1974). This allows us to only use
               !one 3D array for both cloud water & ice.
!              Wice = 1. - MIN(1., MAX(0., (t(i,k,j)-254.)/15.))
!              Wh2o = 1. - Wice
               CLDFRA(i,k,j)=MAX(CLDFRA(i,k,j),CLDFRA_BL(i,k,j))
               CLDFRA(i,k,j)=MAX(0.0,MIN(1.0,CLDFRA(i,k,j)))
               qc(i,k,j)=qc(i,k,j) + QC_BL(i,k,j)*(MIN(1., MAX(0., (t(i,k,j)-254.)/15.)))*CLDFRA_BL(i,k,j)
               qi(i,k,j)=qi(i,k,j) + QC_BL(i,k,j)*(1. - MIN(1., MAX(0., (t(i,k,j)-254.)/15.)))*CLDFRA_BL(i,k,j)
              ENDIF
           ENDDO
           ENDDO
           ENDDO
           ENDIF
        ENDIF

        IF ( PRESENT (cldfra_mp_all) ) THEN
          IF (is_CAMMGMP_used) THEN
            !BSINGH: cloud fraction from CAMMGMP is being used (Mods by Po-Lun)
        CALL wrf_debug (1, 'use cammgmp')
            IF (itimestep .NE. 1) THEN
               DO j=jts,jte
               DO k=kts,kte
               DO i=its,ite
                  CLDFRA(i,k,j) = CLDFRA_MP_ALL(I,K,J) !PMA
                  if (CLDFRA(i,k,j) .lt. 0.01) CLDFRA(i,k,j) = 0.
               ENDDO
               ENDDO
               ENDDO
            ENDIF 
          ENDIF
        ENDIF
     ENDIF
 
     ELSE IF ( ICLOUD == 2 ) THEN

     IF ( F_QC .OR. F_QI ) THEN
       CALL wrf_debug (1, 'CALL cldfra2')
       CALL cal_cldfra2(CLDFRA,qc,qi,F_QC,F_QI,              &
                       ids,ide, jds,jde, kds,kde,            &
                       ims,ime, jms,jme, kms,kme,            &
                       its,ite, jts,jte, kts,kte             )
     ENDIF

!+---+-----------------------------------------------------------------+ 
!..New cloud fraction scheme added by G. Thompson (2014Oct31)
!+---+-----------------------------------------------------------------+ 

     ELSEIF (ICLOUD == 3) THEN
        IF ( F_QC .AND. F_QI ) THEN

           CALL wrf_debug (150, 'DEBUG: using gthompsn cloud fraction scheme')

           DO j = jts,jte
           DO i = its,ite

             DO k = kts,kte
                p_1d(k) = p(i,k,j)
                t_1d(k) = t(i,k,j)
                qv_1d(k) = qv(i,k,j)
                qc_1d(k) = qc(i,k,j)
                qi_1d(k) = qi(i,k,j)
                qs_1d(k) = qs(i,k,j)
                Dz_1d(k) = dz8w(i,k,j)
                cf_1d(k) = cldfra(i,k,j)
             ENDDO

             WRITE (wrf_err_message,*) 'DEBUG: calling cal_cldfra3 at (i,j): ', i,j, kms,kme,kts,kte
             CALL wrf_debug (150, wrf_err_message)

             CALL cal_cldfra3(cf_1d, qv_1d, qc_1d, qi_1d, qs_1d, Dz_1d, &
     &                          p_1d, t_1d, XLAND(i,j), gridkm,         &
     &                          .false., 1.5, kts, kte, .false.)

             DO k = kts,kte
                qc(i,k,j) = qc_1d(k)
                qi(i,k,j) = qi_1d(k)
                qs(i,k,j) = qs_1d(k)
                cldfra(i,k,j) = cf_1d(k)
             ENDDO

           ENDDO
           ENDDO

        ELSE
           CALL wrf_error_fatal('Can not use icloud = 3 option, missing QC or QI field.')
        ENDIF

     END IF

!     Interpolating climatological ozone and aerosol to model time and levels
!     Adapted from camrad code
#if (EM_CORE==1)
     IF ( o3input .EQ. 2 .AND. id .EQ. 1 ) THEN
#else
     IF ( o3input .EQ. 2 ) THEN
#endif
!       ! Find the current month (adapted from Cavallo)
!       CALL cam_time_interp( ozmixm, pin, levsiz, date_str, &
!                             ids , ide , jds , jde , kds , kde , &
!                             ims , ime , jms , jme , kms , kme , &
!                             its , ite , jts , jte , kts , kte )
! this is the CAM version
! interpolate to model time-step
        call ozn_time_int(julday,julian,ozmixm,ozmixt,levsiz,n_ozmixm,    &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

! interpolate to model model levels
        call ozn_p_int(p ,pin, levsiz, ozmixt, o3rad, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )
     ENDIF

     IF ( PRESENT( AEROD ) ) THEN
     IF ( aer_opt .EQ. 1 .AND. id .EQ. 1 ) THEN
        call aer_time_int(julday,julian,aerodm,aerodt,alevsiz,n_ozmixm-1,no_src_types,    &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

        call aer_p_int(p ,pina, alevsiz, aerodt, aerod, no_src_types, p8w, AODTOT, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )
     ENDIF
     ENDIF

     !Modify CLDFRA and QC for kfcupscheme cumulus scheme
     if(present(cldfra_cup)) then
        !BSINGH - overwrite cldfra with the cloud fraction computed in module_cu_kfcup.F        
        !Force cloud fraction if cumulus triggered.
        if( cu_physics == KFCUPSCHEME ) then
           do j = jts,jte
              do k = kts,kte
                 do i = its,ite

                    !Find whether to overwrite cldfra or not (ONLY if ICLOUD == 1)
                    compute_cldfra_cup = .true. 
                    if (icloud == 1 ) then
                       compute_cldfra_cup = .false.  !-- LK Berg, 4/26/17     
                       if(cldfra1_flag(i,k,j) == 1 .and. shall(i,j) .gt. 1) then
                          CLDFRA(i,k,j)=0.
                       elseif(cldfra1_flag(i,k,j) == 1 .and. shall(i,j) .le. 1) then
                          CLDFRA(i,k,j)=0.
                          compute_cldfra_cup = .true.    ! No resolved clouds, but check of shallow clouds.  -- LK Berg, 4/26/17
                       elseif(cldfra1_flag(i,k,j) == 2 .and. shall(i,j) .gt. 1) then
                          CLDFRA(i,k,j)=1.
                       elseif(cldfra1_flag(i,k,j) == 3) then
                          compute_cldfra_cup = .true.
                       endif
                    endif


                    if(compute_cldfra_cup) then
                       if( (int(shall(i,j)) .le.1) .and. k >= int(cubot(i,j)) .and. k <= int(cutop(i,j)) ) then  ! LD Mar232012
                          CLDFRA(i,k,j) = cldfra_cup(i,k,j)
                       else if( shall(i,j) .gt. 1) then       !!LD
                          cldfra_cup(i,k,j) = 0.0
                       end if
                    endif
                    if( shall(i,j) <= 1 .and. k >= cubot(i,j)  .and. k <= cutop(i,j)  ) then  ! 1=Shallow Cu  -- Changed to use for both deep and shallow  -- LK Berg 4/26/17
                       ! Begin: wig, 4-Feb-2008
                       !
                       ! Override the cloud condensate values if shallow convection triggered.
                       ! For shallow convection, use a representative condensate value based on
                       ! observations from CHAPS (Oklahoma area) and Florida (Blyth et al. 2005)
                       ! or the predicted value if it is greater.

                       cldfra_cup_mod = cldfra_cup(i,k,j) * 1.0e-3 !modified cloud fraction, assume QCLOUD is 1 g/kg -- LK Berg 4/26/17                       
                       qc(i,k,j) = max(cldfra_cup_mod, qc(i,k,j) )!DE+LB 2012-Feb

                       ! Override the cloud fraction values calculated above if shallow
                       ! convection triggered. For shallow convection, use a representative
                       ! cloud fraction. In this case, the median value for shallow Cu cases
                       ! at the ARM SGP site, 36%, Berg et al. 2008, J. Clim.
                       if( shallowcu_forced_ra )cldfra(i,k,j) = max(0.36, cldfra(i,k,j)) ! Median shallow Cu frac at ARM SGP
                    endif
                 ENDDO
              ENDDO
           ENDDO
        end if
     endif

#if (EM_CORE==1)
     IF( shcu_physics == 5 ) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           cldfra(I,K,J) = max(cldfra_sh(I,K,J), cldfra(I,K,J))
           qc_save(I,K,J)=qc(I,K,J)
           qc(I,K,J)=cw_rad(I,K,J)+qc(I,K,J)
        ENDDO
        ENDDO
        ENDDO
     ENDIF
#endif

        

     lwrad_select: SELECT CASE(lw_physics)

        CASE (RRTMSCHEME)
             CALL wrf_debug (100, 'CALL rrtm')

             IF ( PRESENT(p_top) ) THEN
                p_top_dummy = p_top
             ELSE
                p_top_dummy = -1. ! not used by NMM
             END IF
             CALL RRTMLWRAD(                                        &
                  P_TOP=p_top_dummy                                 & 
                 ,RTHRATEN=RTHRATEN,GLW=GLW,OLR=RLWTOA,EMISS=EMISS  &
                 ,QV3D=QV                                           &
                 ,QC3D=QC                                           &
                 ,QR3D=QR                                           &
                 ,QI3D=QI                                           &
                 ,QS3D=QS                                           &
                 ,QG3D=QG                                           &
                 ,P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t     &
                 ,T8W=t8w,RHO3D=rho, CLDFRA3D=CLDFRA,R=R_d,G=G      &
                 ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR                     &
                 ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG                     &
                 ,ICLOUD=icloud,WARM_RAIN=warm_rain                 &
!ccc Added for time-varying trace gases.
                 ,YR=YR,JULIAN=JULIAN                               &
!ccc
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                                                                    )

! NUWRF Version by Toshihisa Matsui and Jainn Shi 20090623
        case (goddardlwscheme)

             CALL wrf_debug(100, 'CALL NUWRF goddard longwave radiation scheme 2017')

            IF ( mp_physics .NE. nuwrf4icescheme ) THEN
                IF ( has_reqc .EQ. 1 ) THEN
                   DO j=jts,jte
                   DO k=kts,kte
                   DO i=its,ite
                      re_cloud_gsfc(i,k,j) = re_cloud(i,k,j) * 1.E+6
                      re_ice_gsfc(i,k,j) = re_ice(i,k,j) * 1.E+6
                      re_snow_gsfc(i,k,j) = re_snow(i,k,j) * 1.E+6
                      re_rain_gsfc(i,k,j) = 0.
                      re_graupel_gsfc(i,k,j) = 0.
                      re_hail_gsfc(i,k,j) = 0.
                   ENDDO
                   ENDDO
                   ENDDO
                ELSE
                   WRITE ( wrf_err_message , * ) 'Must choose a microphysics that provides effective radii.'
                   CALL wrf_debug (0, wrf_err_message)
                END IF
             END IF

             CALL goddardrad(sw_or_lw='lw',dx=dx                  &
                    ,rthraten=rthraten,gsf=glw,xlat=xlat,xlong=xlong   &
                    ,alb=albedo,t3d=t,p3d=p,p8w3d=p8w,pi3d=pi          &
                    ,dz8w=dz8w,rho_phy=rho,emiss=emiss,tsk=tsk         &
                    ,cldfra3d=cldfra                                   &
                    ,gmt=gmt,cp=cp,g=g,t8w=t8w                         &
                    ,julday=julday,xtime=xtime                         &
                    ,declin=declin,solcon=solcon                       &
                    ,radfrq=radt,degrad=degrad                         &
                    ,warm_rain=warm_rain                               &
                    ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde &
                    ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme &
                    ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte &
                    ,qv=qv,qc=qc,qi=qi,qr=qr,qs=qs,qg=qg,qh=qh         &
                    ,f_qv=f_qv,f_qc=f_qc,f_qr=f_qr                     &
                    ,f_qi=f_qi,f_qs=f_qs,f_qg=f_qg ,f_qh=f_qh          &
                    ,rec3d=re_cloud_gsfc,rei3d=re_ice_gsfc             &
                    ,rer3d=re_rain_gsfc,res3d=re_snow_gsfc             & !optional
                    ,reg3d=re_graupel_gsfc,reh3d=re_hail_gsfc          & !optional 
                    ,erbe_out=erbe_out                                 &
                    ,itimestep=itimestep, dt_in = dt                   &
#if (WRF_CHEM == 1)
                    ,AERO=aero                                         &
                    ,CHEM_OPT=chem_opt                                 &
                    ,GSFCRAD_GOCART_COUPLING=gsfcrad_gocart_coupling   &
#endif
                                                                       )

                                                                       
        CASE (GFDLLWSCHEME)

             CALL wrf_debug (100, 'CALL gfdllw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QI) .AND. (PRESENT(qi) .OR. PRESENT(qs))  .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc)   ) THEN
               IF ( F_QV .AND. F_QC .AND. (F_QI .OR. F_QS)) THEN
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

#if ( HWRF == 1 )
       CASE (HWRFLWSCHEME)

             CALL wrf_debug (100, 'CALL hwrflw')

             gfdl_lw  = .true.

               CALL HWRFRA(explicit_convection=expl_conv, &
                    DT=dt,thraten=RTHRATEN,thratenlw=RTHRATENLW,thratensw=RTHRATENSW,pi3d=pi, &
                        XLAND=xland,P8w=p8w,DZ8w=dz8w,RHO_PHY=rho,P_PHY=p,T=t,          &
                        QV=qv,QW=qc_temp,QI=Qi,                      &
                        TSK2D=tsk,GLW=GLW,GSW=GSW,                                 &
                        TOTSWDN=swdown,TOTLWDN=glw,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean,        & !Added
                        GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot,htopr=htopr,hbotr=hbotr,ALBEDO=albedo,CUPPT=cuppt,&
                        VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt,                           & !Modified
                        NSTEPRA=stepra,NPHS=nphs,itimestep=itimestep,                       & !Modified
                        julyr=julyr,julday=julday,gfdl_lw=gfdl_lw,gfdl_sw=gfdl_sw,                &
                        CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach,                        & !Added
                        ACFRST=acfrst,NCFRST=ncfrst,ACFRCV=acfrcv,NCFRCV=ncfrcv,                 & !Added
                        ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,                   &
                        ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,                   &
                        its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte                    )


#endif

        CASE (CAMLWSCHEME)

             CALL wrf_debug(100, 'CALL camrad lw')

             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2).AND. PRESENT(ALSWVISDIR) ) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.true.,dosw=.false.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif      &  !fds ssib alb comp (06/2010)
                    ,ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif      &  !fds ssib alb comp (06/2010)
                    ,SWVISDIR=swvisdir ,SWVISDIF=swvisdif              &  !fds ssib swr comp (06/2010)
                    ,SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif              &  !fds ssib swr comp (06/2010)
                    ,SF_SURFACE_PHYSICS=sf_surface_physics             &  !fds
                    ,SWDDIR=swddir,SWDDIF=swddif,SWDDNI=swddni         &  !amontornes-bcodina (2014-04-20)
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
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,coszen=coszen                                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling cam radiation' )
             ENDIF

        CASE (RRTMG_LWSCHEME)

             CALL wrf_debug (100, 'CALL rrtmg_lw')
             IF ( mp_physics .EQ. nuwrf4icescheme ) THEN
                DO j=jts,jte
                DO k=kts,kte
                DO i=its,ite
                   re_cloud(i,k,j) = re_cloud_gsfc(i,k,j) * 1.E-6
                   re_ice(i,k,j) = re_ice_gsfc(i,k,j) * 1.E-6
                   re_snow(i,k,j) = re_snow_gsfc(i,k,j) * 1.E-6
                ENDDO
                ENDDO
                ENDDO
             END IF

             CALL RRTMG_LWRAD(                                      &
                  RTHRATENLW=RTHRATEN,                              &
                  LWUPT=LWUPT,LWUPTC=LWUPTC,LWUPTCLN=LWUPTCLN,      &
                  LWDNT=LWDNT,LWDNTC=LWDNTC,LWDNTCLN=LWDNTCLN,      &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,LWUPBCLN=LWUPBCLN,      &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,LWDNBCLN=LWDNBCLN,      &
                  GLW=GLW,OLR=RLWTOA,LWCF=LWCF,                     &
                  EMISS=EMISS,                                      &
                  P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t,    &
                  T8W=t8w,RHO3D=rho,R=R_d,G=G,                      &
                  ICLOUD=icloud,WARM_RAIN=warm_rain,CLDFRA3D=CLDFRA,&
                  cldovrlp=cldovrlp,                                & ! J. Henderson AER: cldovrlp namelist value
#if (EM_CORE == 1)
                  LRADIUS=lradius, IRADIUS=iradius,                 &
#endif
                  IS_CAMMGMP_USED=is_cammgmp_used,                  &

!ckay
!                 CLDFRA_KF3D=cldfra_KF,QC_KF3D=qc_KF,QI_KF3D=qi_KF,&
                  F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                  XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                  QV3D=QV,QC3D=QC,QR3D=QR,                          &
                  QI3D=QI,QS3D=QS,QG3D=QG,                          &
                  O3INPUT=O3INPUT,O33D=O3RAD,                       &
                  F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,                    &
                  F_QI=F_QI,F_QS=F_QS,F_QG=F_QG,                    &
                  RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & ! G. Thompson
                  has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & ! G. Thompson
#if ( WRF_CHEM == 1 )
                  TAUAERLW1=tauaerlw1,TAUAERLW2=tauaerlw2,          & ! jcb
                  TAUAERLW3=tauaerlw3,TAUAERLW4=tauaerlw4,          & ! jcb
                  TAUAERLW5=tauaerlw5,TAUAERLW6=tauaerlw6,          & ! jcb
                  TAUAERLW7=tauaerlw7,TAUAERLW8=tauaerlw8,          & ! jcb
                  TAUAERLW9=tauaerlw9,TAUAERLW10=tauaerlw10,        & ! jcb
                  TAUAERLW11=tauaerlw11,TAUAERLW12=tauaerlw12,      & ! jcb
                  TAUAERLW13=tauaerlw13,TAUAERLW14=tauaerlw14,      & ! jcb
                  TAUAERLW15=tauaerlw15,TAUAERLW16=tauaerlw16,      & ! jcb
                  aer_ra_feedback=aer_ra_feedback,                  &
!jdfcz            progn=progn,prescribe=prescribe,                   &
                  progn=progn,                                      &
#endif
                  calc_clean_atm_diag=calc_clean_atm_diag,          &
                  QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &
!ccc Added for time-varying trace gases.
                  YR=YR,JULIAN=JULIAN,                              &
!ccc
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,                &
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC,                &
                  mp_physics=mp_physics                             )

        CASE (RRTMK_LWSCHEME)

           IF ( PRESENT(F_QNC) .AND. PRESENT(QNC_CURR) ) THEN
             call rad_rrtmg_driver(                                 &
                           RTHRATEN, RTHRATENSW,                    &
                           lwupflx, lwupflxc, lwdnflx, lwdnflxc,    &
                           swupflx, swupflxc, swdnflx, swdnflxc,    &
                           lwupt, lwuptc, lwdnt, lwdntc,            &
                           lwupb, lwupbc, lwdnb, lwdnb,             &
                           glw, olr, lwcf,                          &
                           swupt, swuptc, swdnt, swdntc,            &
                           swupb, swupbc, swdnb, swdnb,             &
                           gsw, swcf,                               &
                           coszen, solcon, albedo, emiss,           &
                           t,t8w, tsk, rho, p, p8w, cldfra,         &
                           r_d, g, qnc_curr, xland,                 &
                           f_qnc, f_qv, f_qc, f_qr, f_qi,           &
                           f_qs, f_qg,                              &
                           qv, qc, qr, qi, qs, qg,                  &
                           o3input, o3rad,                          &
                           aer_opt, aerod, no_src_types,            &
                           ids,ide, jds,jde, kds,kde,               &
                           ims,ime, jms,jme, kms,kme,               &
                           its,ite, jts,jte, kts,kte)
             ELSE
                call wrf_error_fatal('Can not call RRTMG-K. Missing QNC field.')
             ENDIF


#if( BUILD_RRTMG_FAST == 1)
        CASE (RRTMG_LWSCHEME_FAST)
             CALL wrf_debug (100, 'CALL rrtmg_lw')

             CALL RRTMG_LWRAD_FAST(                                 &
                  RTHRATENLW=RTHRATEN,                              &
                  LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                  LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                  LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                  LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                  GLW=GLW,OLR=RLWTOA,LWCF=LWCF,                     &
                  EMISS=EMISS,                                      &
                  P8W=p8w,P3D=p,PI3D=pi,DZ8W=dz8w,TSK=tsk,T3D=t,    &
                  T8W=t8w,RHO3D=rho,R=R_d,G=G,                      &
                  ICLOUD=icloud,WARM_RAIN=warm_rain,CLDFRA3D=CLDFRA,&
#if (EM_CORE == 1)
                  LRADIUS=lradius, IRADIUS=iradius,                 &
#endif
                  IS_CAMMGMP_USED=is_cammgmp_used,                  &

!ckay
!                 CLDFRA_KF3D=cldfra_KF,QC_KF3D=qc_KF,QI_KF3D=qi_KF,&
                  F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                  XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                  QV3D=QV,QC3D=QC,QR3D=QR,                          &
                  QI3D=QI,QS3D=QS,QG3D=QG,                          &
                  O3INPUT=O3INPUT,O33D=O3RAD,                       &
                  F_QV=F_QV,F_QC=F_QC,F_QR=F_QR,                    &
                  F_QI=F_QI,F_QS=F_QS,F_QG=F_QG,                    &
                  RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & ! G. Thompson
                  has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & ! G. Thompson
#if ( WRF_CHEM == 1 )
                  TAUAERLW1=tauaerlw1,TAUAERLW2=tauaerlw2,          & ! jcb
                  TAUAERLW3=tauaerlw3,TAUAERLW4=tauaerlw4,          & ! jcb
                  TAUAERLW5=tauaerlw5,TAUAERLW6=tauaerlw6,          & ! jcb
                  TAUAERLW7=tauaerlw7,TAUAERLW8=tauaerlw8,          & ! jcb
                  TAUAERLW9=tauaerlw9,TAUAERLW10=tauaerlw10,        & ! jcb
                  TAUAERLW11=tauaerlw11,TAUAERLW12=tauaerlw12,      & ! jcb
                  TAUAERLW13=tauaerlw13,TAUAERLW14=tauaerlw14,      & ! jcb
                  TAUAERLW15=tauaerlw15,TAUAERLW16=tauaerlw16,      & ! jcb
                  aer_ra_feedback=aer_ra_feedback,                  &
!jdfcz            progn=progn,prescribe=prescribe,                   &
                  progn=progn,                                      &
#endif
                  QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &
!ccc Added for time-varying trace gases.
                  YR=YR,JULIAN=JULIAN,                              &
!ccc
                  IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                  IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                  ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                  LWUPFLX=LWUPFLX,LWUPFLXC=LWUPFLXC,                &
                  LWDNFLX=LWDNFLX,LWDNFLXC=LWDNFLXC                 &
                                                                    )
#endif

        CASE (HELDSUAREZ)
             CALL wrf_debug (100, 'CALL heldsuarez')

             CALL HSRAD(RTHRATEN,p8w,p,pi,dz8w,t,          &
                     t8w, rho, R_d,G,CP, dt, xlat, degrad, &
                     ids,ide, jds,jde, kds,kde,            &
                     ims,ime, jms,jme, kms,kme,            &
                     its,ite, jts,jte, kts,kte            )

! -- add by Jin Kong 10/2011
        CASE (FLGLWSCHEME)
          CALL wrf_debug (100, 'CALL Fu-Liou-Gu')
          flg_lw  = .true.
!test Jin Kong 11/01/2011 for ozone
          ozflg = 0.
!test over
          CALL RAD_FLG(                               &
               peven=p8w,podd=p,t8w=t8w,degrees=t     &
              ,pi3d=pi,o3=ozflg                       &
              ,G=G,CP=CP                              &
              ,albedo=ALBEDO,tskin=tsk                &
              ,h2o=QV,cld_iccld=QI,cld_wlcld=QC       &
              ,cld_prwc=QR,cld_pgwc=QG,cld_snow=QS    &
              ,F_QV=F_QV,F_QC=F_QC,F_QR=F_QR          &
              ,F_QI=F_QI,F_QS=F_QS,F_QG=F_QG          &
              ,warm_rain=warm_rain                    &
              ,cloudstrf=CLDFRA                       &
              ,emiss=EMISS                            &
              ,air_den=rho                            &
              ,dz3d=dz8w                              &
              ,SOLCON=SOLCON                          &
              ,declin=DECLIN                          &
              ,xtime=xtime, xlong=xlong, xlat=xlat    &
              ,JULDAY=JULDAY                          &
              ,gmt=gmt, radt=radt, degrad=degrad      &
              ,dtcolumn=dt                            &
              ,ids=ids,ide=ide,jds=jds,jde=jde        &
              ,kds=kds,kde=kde                        &     
              ,ims=ims,idim=ime,jms=jms,jdim=jme      &
              ,kms=kms,kmax=kme                       &
              ,its=its,ite=ite,jts=jts,jte=jte        &
              ,kts=kts,kte=kte                        &
	      ,uswtop=RSWTOA,ulwtop=RLWTOA            &
	      ,NETSWBOT=GSW,DLWBOT=GLW                &
	      ,DSWBOT=SWDOWN,deltat=RTHRATEN          &
	      ,dtshort=RTHRATENSW,dtlongwv=RTHRATENLW &
              ,SWDDIR=swddir,SWDDIF=swddif,SWDDNI=swddni &  ! amontornes-bcodina (2014-04-20)
              )

          CALL wrf_debug(100, 'a4 Fu_Liou-Gu')
! -- end 

        CASE DEFAULT
  
             WRITE( wrf_err_message , * ) 'The longwave option does not exist: lw_physics = ', lw_physics
             CALL wrf_error_fatal ( wrf_err_message )
           
     END SELECT lwrad_select    

     IF (lw_physics .gt. 0 .and. .not.gfdl_lw .and. .not.flg_lw) THEN
        DO j=jts,jte
        DO k=kts,kte
        DO i=its,ite
           RTHRATENLW(I,K,J)=RTHRATEN(I,K,J)
! OLR ALSO WILL CONTAIN OUTGOING LONGWAVE FOR RRTM (NMM HAS NO OLR ARRAY)
           IF(PRESENT(OLR) .AND. K .EQ. 1)OLR(I,J)=RLWTOA(I,J)
        ENDDO
        ENDDO
        ENDDO
     ENDIF

!NUWRF JJS 20090623 vvvvv
     IF (lw_physics .eq. goddardlwscheme) THEN
          IF ( PRESENT (tlwdn) ) THEN
        DO j=jts,jte
        DO i=its,ite
           tlwdn(i,j) = erbe_out(i,j,1)    ! TOA LW downwelling flux [W/m2]
           tlwup(i,j) = erbe_out(i,j,2)    ! TOA LW upwelling flux [W/m2]
           slwdn(i,j) = erbe_out(i,j,3)    ! surface LW downwelling flux [W/m2]
           slwup(i,j) = erbe_out(i,j,4)    ! surface LW upwelling flux [W/m2]
           olr(i,j)   = -erbe_out(i,j,2)
        ENDDO    
        ENDDO    
          ENDIF
     ENDIF       
!NUWRF JJS 20090623 ^^^^^

     IF ( PRESENT( AOD5502D ) ) THEN
     ! jararias, 2013/11
     IF ( aer_opt .EQ. 2 ) THEN
     swrad_aerosol_select2: select case(sw_physics)

        case(RRTMG_SWSCHEME,&
#if( BUILD_RRTMG_FAST == 1)
             RRTMG_SWSCHEME_FAST,&
#endif
             RRTMK_SWSCHEME)
           call wrf_debug(100, 'call calc_aerosol_rrtmg_sw')
           call calc_aerosol_rrtmg_sw(ht,dz8w,p,t,qv,aer_type,aer_aod550_opt,aer_angexp_opt,    &
                                      aer_ssa_opt,aer_asy_opt,aer_aod550_val,aer_angexp_val,    &
                                      aer_ssa_val,aer_asy_val,aod5502d,angexp2d,aerssa2d,       &
                                      aerasy2d,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte, &
                                      tauaer_sw,ssaaer_sw,asyaer_sw                             )
           do j=jts,jte
              do i=its,ite
                 do k=kts,kte
                    aod5503d(i,k,j)=tauaer_sw(i,k,j,10) ! band at 550 nm
                 end do
              end do
           end do

        case default
     end select swrad_aerosol_select2
     ENDIF
     ENDIF

     !..Different treatment for aer_opt=3 using QNWFA+QNIFA aerosol species (Trude)

     IF (PRESENT(f_qnwfa) .AND. PRESENT(f_qnifa)) THEN
       IF (F_QNWFA .AND. aer_opt.eq.3 .AND.                             &
                             (sw_physics.eq.RRTMG_SWSCHEME .OR.         &
#if( BUILD_RRTMG_FAST == 1)
                              sw_physics.eq.RRTMG_SWSCHEME_FAST .OR.    &
#endif
                              sw_physics.eq.RRTMK_SWSCHEME )) THEN
         call wrf_debug(100, 'call calc_aerosol_rrtmg_sw with 3D AOD values')
         call calc_aerosol_rrtmg_sw(ht,dz8w,p,t,qv,taer_type,taer_aod550_opt,taer_angexp_opt,  &
                                    taer_ssa_opt,taer_asy_opt,aer_aod550_val,aer_angexp_val,   &
                                    aer_ssa_val,aer_asy_val,taod5502d,angexp2d,aerssa2d,       &
                                    aerasy2d,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte,  &
                                    tauaer_sw,ssaaer_sw,asyaer_sw, taod5503d)
       ENDIF
     ENDIF

     swrad_select: SELECT CASE(sw_physics)

        CASE (SWRADSCHEME)
             CALL wrf_debug(100, 'CALL swrad')
             CALL SWRAD(                                               &
                     DT=dt,RTHRATEN=rthraten,GSW=gsw                   &
                    ,XLAT=xlat,XLONG=xlong,ALBEDO=albedo               &
#if ( WRF_CHEM == 1 )
                    ,PM2_5_DRY=pm2_5_dry,PM2_5_WATER=pm2_5_water       &
                    ,PM2_5_DRY_EC=pm2_5_dry_ec                         &
#endif
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
                    ,coszen=coszen,julian=julian                       )

        CASE (GSFCSWSCHEME)
             CALL wrf_debug(100, 'CALL gsfcswrad')
             CALL GSFCSWRAD(                                           &
                     RTHRATEN=rthraten,GSW=gsw                         & ! PAJ: xlat and xlong removed.
                    ,ALB=albedo,T3D=t,P3D=p,P8W3D=p8w,pi3D=pi          &
                    ,DZ8W=dz8w,RHO_PHY=rho                             &
                    ,CLDFRA3D=cldfra,RSWTOA=rswtoa                     &
                    ,CP=cp,G=g                                         & ! PAJ: GMT removed.
                    ,JULDAY=julday                                     & ! PAJ: XTIME removed.
                    ,SOLCON=solcon                                     & ! PAJ: declin removed
!                    ,RADFRQ=radt,DEGRAD=degrad                         & ! PAJ: degrad and radfrq removed
                    ,TAUCLDI=taucldi,TAUCLDC=taucldc                   &
                    ,WARM_RAIN=warm_rain                               &

#if ( WRF_CHEM == 1 )
                    ,TAUAER300=tauaer300,TAUAER400=tauaer400           & ! jcb
                    ,TAUAER600=tauaer600,TAUAER999=tauaer999           & ! jcb
                    ,GAER300=gaer300,GAER400=gaer400                   & ! jcb
                    ,GAER600=gaer600,GAER999=gaer999                   & ! jcb
                    ,WAER300=waer300,WAER400=waer400                   & ! jcb
                    ,WAER600=waer600,WAER999=waer999                   & ! jcb
                    ,aer_ra_feedback=aer_ra_feedback                   &
#endif
                    ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                    ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                    ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,QNDROP3D=qndrop                                   &
                    ,F_QV=f_qv,F_QC=f_qc,F_QR=f_qr                     &
                    ,F_QI=f_qi,F_QS=f_qs,F_QG=f_qg                     &
                    ,F_QNDROP=f_qndrop                                 &
                    ,COSZEN=coszen                                     &
                                                                       )

        case (goddardswscheme)

             CALL wrf_debug(100, 'CALL NUWRF goddard shortwave radiation scheme 2017')

             IF ( mp_physics .NE. nuwrf4icescheme ) THEN
                IF ( has_reqc .EQ. 1 ) THEN
                   DO j=jts,jte
                   DO k=kts,kte
                   DO i=its,ite
                      re_cloud_gsfc(i,k,j) = re_cloud(i,k,j) * 1.E+6
                      re_ice_gsfc(i,k,j) = re_ice(i,k,j) * 1.E+6
                      re_snow_gsfc(i,k,j) = re_snow(i,k,j) * 1.E+6
                      re_rain_gsfc(i,k,j) = 0.
                      re_graupel_gsfc(i,k,j) = 0.
                      re_hail_gsfc(i,k,j) = 0.
                   ENDDO
                   ENDDO
                   ENDDO
                ELSE
                   WRITE ( wrf_err_message , * ) 'Must choose a microphysics that provides effective radii.'
                   CALL wrf_debug (0, wrf_err_message)
                END IF
             END IF

             CALL goddardrad(sw_or_lw='sw',dx=dx                  &
                    ,rthraten=rthraten,gsf=gsw,xlat=xlat,xlong=xlong   &
                    ,alb=albedo,t3d=t,p3d=p,p8w3d=p8w,pi3d=pi          &
                    ,dz8w=dz8w,rho_phy=rho,emiss=emiss,tsk=tsk         &
                    ,cldfra3d=cldfra                                   &
                    ,gmt=gmt,cp=cp,g=g,t8w=t8w                         &
                    ,julday=julday,xtime=xtime                         &
                    ,declin=declin,solcon=solcon                       &
                    ,radfrq=radt,degrad=degrad                         &
                    ,warm_rain=warm_rain                               &
                    ,ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde &
                    ,ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme &
                    ,its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte &
                    ,qv=qv,qc=qc,qr=qr,qi=qi,qs=qs,qg=qg,qh=qh         & !optional
                    ,f_qv=f_qv,f_qc=f_qc,f_qr=f_qr                     & !optional
                    ,f_qi=f_qi,f_qs=f_qs,f_qg=f_qg ,f_qh=f_qh          & !optional
                    ,rec3d=re_cloud_gsfc,rei3d=re_ice_gsfc             & !optional
                    ,rer3d=re_rain_gsfc,res3d=re_snow_gsfc             & !optional
                    ,reg3d=re_graupel_gsfc,reh3d=re_hail_gsfc          & !optional 
                    ,erbe_out=erbe_out                                 &
                    ,cod2d_out=cod2d_out,ctop2d_out=ctop2d_out         & !optional
                    ,sflxd=sflxd                                       & !optional
                    ,swddir=swddir,swddni=swddni,swddif=swddif         & !optional 
                    ,coszen=coszen                                     & !optional
                    ,itimestep=itimestep, dt_in = dt                   &
#if (WRF_CHEM == 1)
                    ,aod2d_out=aod2d_out, atop2d_out=atop2d_out        & ! optional
                    ,AERO=aero                                         &
                    ,CHEM_OPT=chem_opt                                 &
                    ,GSFCRAD_GOCART_COUPLING=gsfcrad_gocart_coupling   &
#endif
                                                                       )







        CASE (CAMSWSCHEME)
             CALL wrf_debug(100, 'CALL camrad sw')
             IF ( PRESENT( OZMIXM ) .AND. PRESENT( PIN ) .AND. &
                  PRESENT(M_PS_1) .AND. PRESENT(M_PS_2) .AND.  &
                  PRESENT(M_HYBI0) .AND. PRESENT(AEROSOLC_1)    &
                  .AND. PRESENT(AEROSOLC_2) .AND. PRESENT(ALSWVISDIR)) THEN
             CALL CAMRAD(RTHRATENLW=RTHRATEN,RTHRATENSW=RTHRATENSW,    &
                     dolw=.false.,dosw=.true.,                         &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     LWUPT=LWUPT,LWUPTC=LWUPTC,                        &
                     LWDNT=LWDNT,LWDNTC=LWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     LWUPB=LWUPB,LWUPBC=LWUPBC,                        &
                     LWDNB=LWDNB,LWDNBC=LWDNBC,                        &
                     SWCF=SWCF,LWCF=LWCF,OLR=RLWTOA,CEMISS=CEMISS,     &
                     TAUCLDC=TAUCLDC,TAUCLDI=TAUCLDI,COSZR=COSZR,      &
                     GSW=GSW,GLW=GLW,XLAT=XLAT,XLONG=XLONG,            &
                     ALBEDO=ALBEDO,t_phy=t,TSK=TSK,EMISS=EMISS         &
                    ,QV3D=qv                                           &
                    ,QC3D=qc                                           &
                    ,QR3D=qr                                           &
                    ,QI3D=qi                                           &
                    ,QS3D=qs                                           &
                    ,QG3D=qg                                           &
                    ,ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif      &  !fds ssib alb comp (06/2010)
                    ,ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif      &  !fds ssib alb comp (06/2010)
                    ,SWVISDIR=swvisdir ,SWVISDIF=swvisdif              &  !fds ssib swr comp (06/2010)
                    ,SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif              &  !fds ssib swr comp (06/2010)
                    ,SF_SURFACE_PHYSICS=sf_surface_physics             &  !fds
                    ,SWDDIR=swddir,SWDDIF=swddif,SWDDNI=swddni         &  !amontornes-bcodina (2014-04-20)
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
                     GMT=GMT,JULDAY=JULDAY,JULIAN=JULIAN,YR=YR,DT=DT,XTIME=XTIME,DECLIN=DECLIN,  &
                     SOLCON=SOLCON,RADT=RADT,DEGRAD=DEGRAD,n_cldadv=3  &
                   ,abstot_3d=abstot,absnxt_3d=absnxt,emstot_3d=emstot &
                   ,doabsems=doabsems                               &
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde &     
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte &
                 ,coszen=coszen                                     )
             ELSE
                CALL wrf_error_fatal ( 'arguments not present for calling cam radiation' )
             ENDIF
             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (RRTMG_SWSCHEME)

             CALL wrf_debug(100, 'CALL rrtmg_sw')
             IF ( mp_physics .EQ. nuwrf4icescheme ) THEN
                DO j=jts,jte
                DO k=kts,kte
                DO i=its,ite
                   re_cloud(i,k,j) = re_cloud_gsfc(i,k,j) * 1.E-6
                   re_ice(i,k,j) = re_ice_gsfc(i,k,j) * 1.E-6
                   re_snow(i,k,j) = re_snow_gsfc(i,k,j) * 1.E-6
                ENDDO
                ENDDO
                ENDDO
             END IF

             CALL RRTMG_SWRAD(                                         &
                     RTHRATENSW=RTHRATENSW,                            &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,SWUPTCLN=SWUPTCLN,      &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,SWDNTCLN=SWDNTCLN,      &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,SWUPBCLN=SWUPBCLN,      &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,SWDNBCLN=SWDNBCLN,      &
                     SWCF=SWCF,GSW=GSW,                                &
                     XTIME=XTIME,GMT=GMT,XLAT=XLAT,XLONG=XLONG,        &
                     RADT=RADT,DEGRAD=DEGRAD,DECLIN=DECLIN,            &
                     COSZR=COSZR,JULDAY=JULDAY,SOLCON=SOLCON,          &
                     ALBEDO=ALBEDO,t3d=t,t8w=t8w,TSK=TSK,              &
                     p3d=p,p8w=p8w,pi3d=pi,rho3d=rho,                  &
                     dz8w=dz8w,CLDFRA3D=CLDFRA,                        &
#if (EM_CORE == 1)
                     LRADIUS=lradius, IRADIUS=iradius,                 &
#endif
                     IS_CAMMGMP_USED=is_cammgmp_used,                  &
                     R=R_D,G=G,              &
!ckay
!                    CLDFRA_KF3D=cldfra_KF,QC_KF3D=qc_KF,QI_KF3D=qi_KF,&
                     ICLOUD=icloud,WARM_RAIN=warm_rain,                &
                     cldovrlp=cldovrlp,                                & ! J. Henderson AER: cldovrlp namelist value
                     F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                     XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                     QV3D=qv,QC3D=qc,QR3D=qr,                          &
                     QI3D=qi,QS3D=qs,QG3D=qg,                          &
                     O3INPUT=O3INPUT,O33D=O3RAD,                       &
                     AER_OPT=AER_OPT,aerod=aerod,no_src=no_src_types,  &
                     ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif,     &  !Zhenxin ssib alb comp (06/2010)
                     ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif,     &  !Zhenxin ssib alb comp (06/2010)
                     SWVISDIR=swvisdir ,SWVISDIF=swvisdif,             &  !Zhenxin ssib swr comp (06/2010)
                     SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif,             &  !Zhenxin ssib swr comp (06/2010)
                     SF_SURFACE_PHYSICS=sf_surface_physics,            &  !Zhenxin ssib sw_phy   (06/2010)
                     F_QV=f_qv,F_QC=f_qc,F_QR=f_qr,                    &
                     F_QI=f_qi,F_QS=f_qs,F_QG=f_qg,                    &
                     RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & ! G. Thompson
                     has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & ! G. Thompson
#if ( WRF_CHEM == 1 )
                     TAUAER300=tauaer300,TAUAER400=tauaer400,          & ! jcb
                     TAUAER600=tauaer600,TAUAER999=tauaer999,          & ! jcb
                     GAER300=gaer300,GAER400=gaer400,                  & ! jcb
                     GAER600=gaer600,GAER999=gaer999,                  & ! jcb
                     WAER300=waer300,WAER400=waer400,                  & ! jcb
                     WAER600=waer600,WAER999=waer999,                  & ! jcb
                     aer_ra_feedback=aer_ra_feedback,                  &
!jdfcz               progn=progn,prescribe=prescribe,                  &
                     progn=progn,                                      &
#endif
                     calc_clean_atm_diag=calc_clean_atm_diag,          &
                     QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &
                     IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                     IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                     ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,                &
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC,                &
                     tauaer3d_sw=tauaer_sw,                             & ! jararias 2013/11
                     ssaaer3d_sw=ssaaer_sw,                             & ! jararias 2013/11
                     asyaer3d_sw=asyaer_sw,                             & ! jararias 2013/11
                     swddir=swddir,swddni=swddni,swddif=swddif,         & ! jararias 2013/08/10
                     swdownc=swdownc, swddnic=swddnic, swddirc=swddirc, & ! PAJ
                     xcoszen=coszen,julian=julian,mp_physics=mp_physics ) ! jararias 2013/08/14

             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

        CASE (RRTMK_SWSCHEME)
  
             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO

#if( BUILD_RRTMG_FAST == 1)
        CASE (RRTMG_SWSCHEME_FAST)
             CALL wrf_debug(100, 'CALL rrtmg_sw_fast')
             CALL RRTMG_SWRAD_FAST(                                    &
                     RTHRATENSW=RTHRATENSW,                            &
                     SWUPT=SWUPT,SWUPTC=SWUPTC,                        &
                     SWDNT=SWDNT,SWDNTC=SWDNTC,                        &
                     SWUPB=SWUPB,SWUPBC=SWUPBC,                        &
                     SWDNB=SWDNB,SWDNBC=SWDNBC,                        &
                     SWCF=SWCF,GSW=GSW,                                &
                     XTIME=XTIME,GMT=GMT,XLAT=XLAT,XLONG=XLONG,        &
                     RADT=RADT,DEGRAD=DEGRAD,DECLIN=DECLIN,            &
                     COSZR=COSZR,JULDAY=JULDAY,SOLCON=SOLCON,          &
                     ALBEDO=ALBEDO,t3d=t,t8w=t8w,TSK=TSK,              &
                     p3d=p,p8w=p8w,pi3d=pi,rho3d=rho,                  &
                     dz8w=dz8w,CLDFRA3D=CLDFRA,                        &
#if (EM_CORE == 1)
                     LRADIUS=lradius, IRADIUS=iradius,                 &
#endif
                     IS_CAMMGMP_USED=is_cammgmp_used,                  &
                     R=R_D,G=G,              &
!ckay
!                    CLDFRA_KF3D=cldfra_KF,QC_KF3D=qc_KF,QI_KF3D=qi_KF,&
                     ICLOUD=icloud,WARM_RAIN=warm_rain,                &
                     F_ICE_PHY=F_ICE_PHY,F_RAIN_PHY=F_RAIN_PHY,        &
                     XLAND=XLAND,XICE=XICE,SNOW=SNOW,                  &
                     QV3D=qv,QC3D=qc,QR3D=qr,                          &
                     QI3D=qi,QS3D=qs,QG3D=qg,                          &
                     O3INPUT=O3INPUT,O33D=O3RAD,                       &
                     AER_OPT=AER_OPT,aerod=aerod,no_src=no_src_types,  &
                     ALSWVISDIR=alswvisdir ,ALSWVISDIF=alswvisdif,     &  !Zhenxin ssib alb comp (06/2010)
                     ALSWNIRDIR=alswnirdir ,ALSWNIRDIF=alswnirdif,     &  !Zhenxin ssib alb comp (06/2010)
                     SWVISDIR=swvisdir ,SWVISDIF=swvisdif,             &  !Zhenxin ssib swr comp (06/2010)
                     SWNIRDIR=swnirdir ,SWNIRDIF=swnirdif,             &  !Zhenxin ssib swr comp (06/2010)
                     SF_SURFACE_PHYSICS=sf_surface_physics,            &  !Zhenxin ssib sw_phy   (06/2010)
                     F_QV=f_qv,F_QC=f_qc,F_QR=f_qr,                    &
                     F_QI=f_qi,F_QS=f_qs,F_QG=f_qg,                    &
                     RE_CLOUD=re_cloud,RE_ICE=re_ice,RE_SNOW=re_snow,  & ! G. Thompson
                     has_reqc=has_reqc,has_reqi=has_reqi,has_reqs=has_reqs, & ! G. Thompson
#if ( WRF_CHEM == 1 )
                     TAUAER300=tauaer300,TAUAER400=tauaer400,          & ! jcb
                     TAUAER600=tauaer600,TAUAER999=tauaer999,          & ! jcb
                     GAER300=gaer300,GAER400=gaer400,                  & ! jcb
                     GAER600=gaer600,GAER999=gaer999,                  & ! jcb
                     WAER300=waer300,WAER400=waer400,                  & ! jcb
                     WAER600=waer600,WAER999=waer999,                  & ! jcb
                     aer_ra_feedback=aer_ra_feedback,                  &
!jdfcz               progn=progn,prescribe=prescribe,                  &
                     progn=progn,                                      &
#endif
                     QNDROP3D=qndrop,F_QNDROP=f_qndrop,                &
                     IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde,&
                     IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme,&
                     ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte,&
                     SWUPFLX=SWUPFLX,SWUPFLXC=SWUPFLXC,                &
                     SWDNFLX=SWDNFLX,SWDNFLXC=SWDNFLXC,                &
                     tauaer3d_sw=tauaer_sw,                             & ! jararias 2013/11
                     ssaaer3d_sw=ssaaer_sw,                             & ! jararias 2013/11
                     asyaer3d_sw=asyaer_sw,                             & ! jararias 2013/11
                     swddir=swddir,swddni=swddni,swddif=swddif,         & ! jararias 2013/08/10
                     swdownc=swdownc, swddnic=swddnic, swddirc=swddirc, & ! PAJ
                     xcoszen=coszen,julian=julian                       ) ! jararias 2013/08/14

             DO j=jts,jte
             DO k=kts,kte
             DO i=its,ite
                RTHRATEN(I,K,J)=RTHRATEN(I,K,J)+RTHRATENSW(I,K,J)
             ENDDO
             ENDDO
             ENDDO
#endif

        CASE (GFDLSWSCHEME)

             CALL wrf_debug (100, 'CALL gfdlsw')

             IF ( PRESENT(F_QV) .AND. PRESENT(F_QC) .AND.                     &
                  PRESENT(F_QI) .AND. (PRESENT(qi) .OR. PRESENT(qs))  .AND.                     &
                  PRESENT(qv)   .AND. PRESENT(qc)   ) THEN
               IF ( F_QV .AND. F_QC .AND. (F_QI .OR. F_QS)) THEN
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

#if ( HWRF == 1 )
      CASE (HWRFSWSCHEME)

             CALL wrf_debug (100, 'CALL hwrfsw')

             gfdl_sw = .true.
               CALL HWRFRA(explicit_convection=expl_conv, &
                        DT=dt,thraten=RTHRATEN,thratenlw=RTHRATENLW,thratensw=RTHRATENSW,pi3d=pi, &
                        XLAND=xland,P8w=p8w,DZ8w=dz8w,RHO_PHY=rho,P_PHY=p,T=t,          &
                        QV=qv,QW=qc_temp,QI=Qi,                      &
                        TSK2D=tsk,GLW=GLW,GSW=GSW,                                 &
                        TOTSWDN=swdown,TOTLWDN=glw,RSWTOA=rswtoa,RLWTOA=rlwtoa,CZMEAN=czmean,        & !Added
                        GLAT=glat,GLON=glon,HTOP=htop,HBOT=hbot,htopr=htopr,hbotr=hbotr,ALBEDO=albedo,CUPPT=cuppt,            &
                        VEGFRA=vegfra,SNOW=snow,G=g,GMT=gmt,                           & !Modified
                        NSTEPRA=stepra,NPHS=nphs,itimestep=itimestep,                       & !Modified
                        julyr=julyr,julday=julday,gfdl_lw=gfdl_lw,gfdl_sw=gfdl_sw,                &
                        CFRACL=cfracl,CFRACM=cfracm,CFRACH=cfrach,                        & !Added
                        ACFRST=acfrst,NCFRST=ncfrst,ACFRCV=acfrcv,NCFRCV=ncfrcv,                 & !Added
                        ids=ids,ide=ide, jds=jds,jde=jde, kds=kds,kde=kde,                   &
                        ims=ims,ime=ime, jms=jms,jme=jme, kms=kms,kme=kme,                   &
                        its=its,ite=ite, jts=jts,jte=jte, kts=kts,kte=kte                    )
#endif

        CASE (0)

           ! Here in case we don't want to call a sw radiation scheme
           ! For example, the Held-Suarez idealized test case
           IF (lw_physics /= HELDSUAREZ) THEN
             WRITE( wrf_err_message , * ) &
'You have selected a longwave radiation option, but not a shortwave option (sw_physics = 0, lw_physics = ',lw_physics,')'
             CALL wrf_error_fatal ( wrf_err_message )
           END IF

! -- add by Jin Kong 10/2011
!--- the following FLGSWSCHEME is for testing only
        CASE (FLGSWSCHEME)
          flg_sw = .true.
!--- No need to do anything since the short and long wave is calculted in one program
! -- end 

        CASE DEFAULT

             WRITE( wrf_err_message , * ) 'The shortwave option does not exist: sw_physics = ', sw_physics
             CALL wrf_error_fatal ( wrf_err_message )

     END SELECT swrad_select    

!NUWRF JJS 20090623  vvvvv
     IF (sw_physics .eq. goddardswscheme) THEN
          IF ( PRESENT (tswdn) ) THEN
        DO j=jts,jte
        DO i=its,ite
           tswdn(i,j) = erbe_out(i,j,5)    ! TOA SW downwelling flux [W/m2]
           tswup(i,j) = erbe_out(i,j,6)    ! TOA SW upwelling flux [W/m2]
           sswdn(i,j) = erbe_out(i,j,7)    ! surface SW downwelling flux [W/m2]
           sswup(i,j) = erbe_out(i,j,8)    ! surface SW upwelling flux [W/m2]
        ENDDO
        ENDDO
          ENDIF
     ENDIF
!NUWRF JJS 20090623  ^^^^^

     IF (sw_physics .gt. 0 .and. .not.gfdl_sw .and. .not.flg_sw) THEN
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

! jararias, 14/08/2013
     ! surface direct and diffuse SW fluxes computation. Only for schemes other than RRTMG and Goddard
     ! Backup method in case sw scheme in use does not provide surface SW direct and diffuse irradiances
     IF ((sw_physics .NE. RRTMG_SWSCHEME) &
#if( BUILD_RRTMG_FAST == 1)
           .AND. (sw_physics .NE. RRTMG_SWSCHEME_FAST) &
#endif
           .AND. (sw_physics .NE. FLGSWSCHEME) .AND. (sw_physics .NE. CAMSWSCHEME) &  ! amontornes-bcodina (2014-04-20)
           .AND. (sw_physics .NE. RRTMK_SWSCHEME) &
           .AND. (sw_physics .ne. GODDARDSWSCHEME)) THEN
        DO j=jts,jte
           DO i=its,ite
              IF (coszen(i,j).GT.1e-3) THEN
                 ioh=solcon*coszen(i,j) ! TOA irradiance
                 kt=swdown(i,j)/max(ioh,1e-3) ! clearness index
                 ! Optical air mass: Rigollier et al. (2000) doi:
                 ! 10.1016/S0038-092X(99)00055-9
                 airmass=exp(-ht(i,j)/8434.5)/(coszen(i,j)+ &
                        0.50572*(asin(coszen(i,j))*57.295779513082323+6.07995)**(-1.6364))
                 ! kt correction for air-mass at large sza: Perez et al. (1990)
                 ! doi: 10.1016/0038-092X(90)90036-C
                 kt=kt/(0.1+1.031*exp(-1.4/(0.9+(9.4/max(airmass,1e-3)))))
                 ! Diffuse fraction: Ruiz-Arias et al. (2010) (Eq 33) doi:
                 ! 10.1016/j.enconman.2009.11.024
                 kd=0.952-1.041*exp(-exp(2.300-4.702*kt))
                 swddif(i,j)=kd*swdown(i,j)
                 swddir(i,j)=(1.-kd)*swdown(i,j)
                 swddni(i,j)=swddir(i,j)/max(coszen(i,j),1e-4)
              ENDIF
           ENDDO
        ENDDO
     ENDIF

     IF ( PRESENT( diffuse_frac ) ) THEN 
         DO j=jts,jte
         DO i=its,ite
           if (swdown(i,j).gt.0.001) then
              diffuse_frac(i,j) = swddif(i,j)/swdown(i,j)
              diffuse_frac(i,j) = min(diffuse_frac(i,j),1.0)
           else
              diffuse_frac(i,j) = 0.
           endif
         ENDDO
         ENDDO
     ENDIF

 ! Restore qc & qi for any model physics configuration
      IF ( F_QC ) THEN
         DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
           qc(i,k,j) = qc_save(i,k,j)
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      IF ( F_QI ) THEN
         DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
           qi(i,k,j) = qi_save(i,k,j)
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (aercu_opt.gt.0.0) THEN
      IF ( F_QS ) THEN
         DO j=jts,jte
         DO k=kts,kte
         DO i=its,ite
           qs(i,k,j) = qs_save(i,k,j)
         ENDDO
         ENDDO
         ENDDO
      ENDIF
      END IF

      ! jararias, aug 2013, updated 2013/11
      ! parameters update for SW surface fluxes interpolation
      IF (swint_opt.EQ.1) THEN
         ! interpolation applies on all-sky fluxes (swddir, swdown)
         CALL update_swinterp_parameters(ims,ime,jms,jme,its,ite,jts,jte,   &
                                         coszen,coszen_loc,swddir,swdown,   &
                                         swddir_ref,bb,Bx,swdown_ref,gg,Gx, &
                                         coszen_ref                         )
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   IF ( associated(tauaer_sw) ) deallocate(tauaer_sw)
   IF ( associated(ssaaer_sw) ) deallocate(ssaaer_sw)
   IF ( associated(asyaer_sw) ) deallocate(asyaer_sw)

   ENDIF Radiation_step

 ! jararias, aug 2013
 ! SW surface fluxes interpolation (meaningful when not in a Radiation_step)
 if (swint_opt .eq. 1) then
    call wrf_debug(100,'SW surface irradiance interpolation')

    !---------------
    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)
    do ij = 1,num_tiles
      its = i_start(ij)
      ite = i_end(ij)
      jts = j_start(ij)
      jte = j_end(ij)
      call interp_sw_radiation(ims,ime,jms,jme,its,ite,jts,jte,  &
                               coszen_ref,coszen_loc,swddir_ref, &
                               bb,Bx,swdown_ref,gg,Gx,albedo,    &
                               swdown,swddir,swddni,swddif,gsw   )
    enddo
    !$OMP END PARALLEL DO
 end if

     accumulate_lw_select: SELECT CASE(lw_physics)

     CASE (CAMLWSCHEME,& 
           RRTMG_LWSCHEME,&
#if( BUILD_RRTMG_FAST == 1)
           RRTMG_LWSCHEME_FAST,&
#endif
           RRTMK_LWSCHEME)
   IF(PRESENT(LWUPTC))THEN
!  NMM calls the driver every RADT time steps, EM calls every DT
#if (EM_CORE == 1)
   DTaccum = DT
#else
   DTaccum = RADT*60
#endif
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACLWUPT(I,J) = ACLWUPT(I,J) + LWUPT(I,J)*DTaccum
           ACLWUPTC(I,J) = ACLWUPTC(I,J) + LWUPTC(I,J)*DTaccum
           ACLWDNT(I,J) = ACLWDNT(I,J) + LWDNT(I,J)*DTaccum
           ACLWDNTC(I,J) = ACLWDNTC(I,J) + LWDNTC(I,J)*DTaccum
           ACLWUPB(I,J) = ACLWUPB(I,J) + LWUPB(I,J)*DTaccum
           ACLWUPBC(I,J) = ACLWUPBC(I,J) + LWUPBC(I,J)*DTaccum
           ACLWDNB(I,J) = ACLWDNB(I,J) + LWDNB(I,J)*DTaccum
           ACLWDNBC(I,J) = ACLWDNBC(I,J) + LWDNBC(I,J)*DTaccum
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF
     CASE DEFAULT
     END SELECT accumulate_lw_select

     accumulate_sw_select: SELECT CASE(sw_physics)

     CASE (CAMSWSCHEME,&
           RRTMG_SWSCHEME,&
#if( BUILD_RRTMG_FAST == 1)
           RRTMG_SWSCHEME_FAST,&
#endif
           RRTMK_SWSCHEME)
   IF(PRESENT(SWUPTC))THEN
!  NMM calls the driver every RADT time steps, EM calls every DT
#if (EM_CORE == 1)
   DTaccum = DT
#else
   DTaccum = RADT*60
#endif
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,i,j,k,its,ite,jts,jte)

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
           ACSWUPT(I,J) = ACSWUPT(I,J) + SWUPT(I,J)*DTaccum
           ACSWUPTC(I,J) = ACSWUPTC(I,J) + SWUPTC(I,J)*DTaccum
           ACSWDNT(I,J) = ACSWDNT(I,J) + SWDNT(I,J)*DTaccum
           ACSWDNTC(I,J) = ACSWDNTC(I,J) + SWDNTC(I,J)*DTaccum
           ACSWUPB(I,J) = ACSWUPB(I,J) + SWUPB(I,J)*DTaccum
           ACSWUPBC(I,J) = ACSWUPBC(I,J) + SWUPBC(I,J)*DTaccum
           ACSWDNB(I,J) = ACSWDNB(I,J) + SWDNB(I,J)*DTaccum
           ACSWDNBC(I,J) = ACSWDNBC(I,J) + SWDNBC(I,J)*DTaccum
        ENDDO
        ENDDO
   ENDDO
   !$OMP END PARALLEL DO
   ENDIF

     CASE DEFAULT
     END SELECT accumulate_sw_select

! compute cloud diagnosis (random overlapping)
! by ZCX
 IF ( PRESENT ( CLDFRA ) .AND. PRESENT ( CLDT ) .AND.        &
      PRESENT ( F_QC ) .AND. PRESENT ( F_QI ) ) THEN

   DO ij = 1 , num_tiles
     its = i_start(ij)
     ite = i_end(ij)
     jts = j_start(ij)
     jte = j_end(ij)

        DO j=jts,jte
        DO i=its,ite
          cldji=1.0
          do k=kte-1,kts,-1
            cldji=cldji*(1.0-cldfra(i,k,j))
          enddo
          cldt(i,j)=1.0-cldji
!         cldlji=1.0
!         do k=kte-1,kts,-1
!         if(znu(k).ge.0.69) then
!           cldlji=cldlji*(1.0-cldfra(i,k,j))
!         endif
!         enddo
!         cldl(i,j)=1.0-cldlji
        END DO
        END DO
    END DO
 END IF

   END SUBROUTINE radiation_driver

   SUBROUTINE pre_radiation_driver ( grid, config_flags                   &
              ,itimestep, ra_call_offset                                  &
              ,XLAT, XLONG, GMT, julian, xtime, RADT, STEPRA              &
              ,ht,dx,dy,sina,cosa,shadowmask,slope_rad ,topo_shading      &
              ,shadlen,ht_shad,ht_loc                                     &
              ,ht_shad_bxs, ht_shad_bxe                                   &
              ,ht_shad_bys, ht_shad_bye                                   &
              ,nested, min_ptchsz                                         &
              ,spec_bdy_width                                             &
              ,ids, ide, jds, jde, kds, kde                               &
              ,ims, ime, jms, jme, kms, kme                               &
              ,ips, ipe, jps, jpe, kps, kpe                               &
              ,i_start, i_end                                             &
              ,j_start, j_end                                             &
              ,kts, kte                                                   &
              ,num_tiles                                                  )

   USE module_domain  , ONLY : domain
#ifdef DM_PARALLEL
   USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks,wrf_dm_minval_integer
# if (EM_CORE == 1)
   USE module_comm_dm   , ONLY : halo_toposhad_sub
# endif
#endif
   USE module_bc
   USE module_model_constants

   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::   ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                                         kts,kte, &
                                       num_tiles

   TYPE(domain)                   , INTENT(INOUT)  :: grid
   TYPE(grid_config_rec_type   ) ,   INTENT(IN   ) :: config_flags

   INTEGER, INTENT(IN  ) :: itimestep, ra_call_offset, stepra,    &
                            slope_rad, topo_shading,              &
                            spec_bdy_width

   INTEGER, INTENT(INOUT) :: min_ptchsz

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                   &
                i_start,i_end,j_start,j_end

   REAL, INTENT(IN  )   :: GMT, radt, julian, xtime, dx, dy, shadlen

   REAL, DIMENSION( ims:ime, jms:jme ),                           &
         INTENT(IN   )  ::                                  XLAT, &
                                                           XLONG, &
                                                              HT, &
                                                            SINA, &
                                                            COSA

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   REAL,  DIMENSION( jms:jme , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bxs, ht_shad_bxe
   REAL,  DIMENSION( ims:ime , kds:kde , spec_bdy_width ),        &
                      INTENT(IN   ) :: ht_shad_bys, ht_shad_bye

   INTEGER, DIMENSION( ims:ime, jms:jme ),                        &
         INTENT(INOUT)  ::                            shadowmask

   LOGICAL,      INTENT(IN   )    :: nested

!Local
! For orographic shading
   INTEGER :: niter,ni,psx,psy,idum,jdum,i,j,ij
   REAL :: DECLIN,SOLCON

! Determine minimum patch size for slope-dependent radiation
   if (itimestep .eq. 1) then
     psx = ipe-ips+1
     psy = jpe-jps+1
     min_ptchsz = min(psx,psy)
     idum = 0
     jdum = 0
   endif

# ifdef DM_PARALLEL
   if (itimestep .eq. 1) then
     call wrf_dm_minval_integer (psx,idum,jdum)
     call wrf_dm_minval_integer (psy,idum,jdum)
     min_ptchsz = min(psx,psy)
   endif
# endif

! Topographic shading
   
   if ((topo_shading.eq.1).and.(itimestep .eq. 1 .or. &
        mod(itimestep,STEPRA) .eq. 1 + ra_call_offset))  then

!---------------
! Calculate constants for short wave radiation
   
   CALL radconst(XTIME,DECLIN,SOLCON,JULIAN,DEGRAD,DPD)
   
! Make a local copy of terrain height field
     do j=jms,jme
     do i=ims,ime
       ht_loc(i,j) = ht(i,j)
     enddo
     enddo
! Determine if iterations are necessary for shadows to propagate from one patch to another
     if ((ids.eq.ips).and.(ide.eq.ipe).and.(jds.eq.jps).and.(jde.eq.jpe)) then
       niter = 1
     else
       niter = int(shadlen/(dx*min_ptchsz)+3)
     endif


    IF( nested ) THEN

      !$OMP PARALLEL DO   &
      !$OMP PRIVATE ( ij )

      DO ij = 1 , num_tiles

           CALL spec_bdyfield(ht_shad,                         &
                               ht_shad_bxs, ht_shad_bxe,       &
                               ht_shad_bys, ht_shad_bye,       &
                               'm', config_flags, spec_bdy_width, 2,&
                               ids,ide, jds,jde, 1  ,1  ,  & ! domain dims
                               ims,ime, jms,jme, 1  ,1  ,  & ! memory dims
                               ips,ipe, jps,jpe, 1  ,1  ,  & ! patch  dims
                               i_start(ij), i_end(ij),         &
                               j_start(ij), j_end(ij),         &
                               1    , 1             )
      ENDDO
    ENDIF

     do ni = 1, niter

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
         do ij = 1 , num_tiles

         call toposhad_init (ht_shad,ht_loc,                         &
                       shadowmask,nested,ni,                         &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,      &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

         enddo
   !$OMP END PARALLEL DO


   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
       do ij = 1 , num_tiles

       call toposhad (xlat,xlong,sina,cosa,xtime,gmt,radt,declin,    &
                       dx,dy,ht_shad,ht_loc,ni,                      &
                       shadowmask,shadlen,                           &
                       ids,ide, jds,jde, kds,kde,                    &
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,min(ipe,ide-1), jps,min(jpe,jde-1), kps,kpe,        &
                       i_start(ij),min(i_end(ij), ide-1),j_start(ij),&
                       min(j_end(ij), jde-1), kts, kte               )

       enddo
   !$OMP END PARALLEL DO

#if defined( DM_PARALLEL ) && (EM_CORE == 1)
#     include "HALO_TOPOSHAD.inc"
#endif
     enddo
   endif

   END SUBROUTINE pre_radiation_driver

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
   
   END SUBROUTINE radconst


   SUBROUTINE calc_coszen(ims,ime,jms,jme,its,ite,jts,jte,  &
                          julian,xtime,gmt, &
                          declin,degrad,xlon,xlat,coszen,hrang)
       ! Added Equation of Time correction : jararias, 2013/08/10
       implicit none
       integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
       real, intent(in)    :: julian,declin,xtime,gmt,degrad
       real, dimension(ims:ime,jms:jme), intent(in)    :: xlat,xlon
       real, dimension(ims:ime,jms:jme), intent(inout) :: coszen,hrang

       integer :: i,j
       real    :: da,eot,xt24,tloctm,xxlat

       da=6.2831853071795862*(julian-1)/365.
       eot=(0.000075+0.001868*cos(da)-0.032077*sin(da) &
            -0.014615*cos(2*da)-0.04089*sin(2*da))*(229.18)
       xt24=mod(xtime,1440.)+eot
       do j=jts,jte
          do i=its,ite
             tloctm=gmt+xt24/60.+xlon(i,j)/15.
             hrang(i,j)=15.*(tloctm-12.)*degrad
             xxlat=xlat(i,j)*degrad
             coszen(i,j)=sin(xxlat)*sin(declin) &
                        +cos(xxlat)*cos(declin) *cos(hrang(i,j))
          enddo
       enddo
   END SUBROUTINE calc_coszen

   subroutine update_swinterp_parameters(ims,ime,jms,jme,its,ite,jts,jte, &
                                         coszen,coszen_loc,swddir,swdown, &
                                         swddir_ref,bb,Bx,                &
                                         swdown_ref,gg,Gx,                &
                                         coszen_ref                       )
      ! Author: jararias 2013/11
      implicit None
      integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
      real, dimension(ims:ime,jms:jme), intent(in)    :: coszen,coszen_loc,swddir,swdown
      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir_ref,bb,Bx, &
                                                         swdown_ref,gg,Gx, &
                                                         coszen_ref

      integer :: i,j
      real :: swddir_0,swdown_0,coszen_0
      real, parameter :: coszen_min=1e-4

      do j=jts,jte
         do i=its,ite
            if ((coszen(i,j).gt.coszen_min) .and. (coszen_loc(i,j).gt.coszen_min)) then
               ! parameters update for DIR
               if (Bx(i,j).le.0) then
                  swddir_0 =(coszen_loc(i,j)/coszen(i,j))*swddir(i,j) ! linear first guess estimation
                  coszen_0 =coszen_loc(i,j)
               else
                  swddir_0 =swddir_ref(i,j)
                  coszen_0 =coszen_ref(i,j)
               end if
               if ((coszen(i,j)/coszen_0).lt.1.) then
                  bb(i,j) =log(max(1.,swddir(i,j))/max(1.,swddir_0)) / log(min(1.-1e-4,coszen(i,j)/coszen_0))
               elseif ((coszen(i,j)/coszen_0).gt.1) then
                  bb(i,j) =log(max(1.,swddir(i,j))/max(1.,swddir_0)) / log(max(1.+1e-4,coszen(i,j)/coszen_0))
               else
                  bb(i,j) =0.
               end if
               bb(i,j) =max(-.5,min(2.5,bb(i,j)))
               Bx(i,j) =swddir(i,j)/(coszen(i,j)**bb(i,j))

               !write(wrf_err_message,*) 'XXX I=',i,' J=',j,'  Bx=',Bx(i,j),'  bb=',bb(i,j),'  swddir=',swddir(i,j), &
               !                         '  swddir_0=',swddir_0,'  coszen=',coszen(i,j),'  coszen_0=',coszen_0
               !call wrf_debug(1,wrf_err_message)

               ! parameters update for GHI
               if (Gx(i,j).le.0) then
                  swdown_0 =(coszen_loc(i,j)/coszen(i,j))*swdown(i,j) ! linear first guess estimation
                  coszen_0 =coszen_loc(i,j)
               else
                  swdown_0 =swdown_ref(i,j)
                  coszen_0 =coszen_ref(i,j)
               end if
               if ((coszen(i,j)/coszen_0).lt.1.) then
                  gg(i,j) =log(max(1.,swdown(i,j))/max(1.,swdown_0)) / log(min(1.-1e-4,coszen(i,j)/coszen_0))
               elseif ((coszen(i,j)/coszen_0).gt.1) then
                  gg(i,j) =log(max(1.,swdown(i,j))/max(1.,swdown_0)) / log(max(1.+1e-4,coszen(i,j)/coszen_0))
               else
                  gg(i,j) =0.
               end if
               gg(i,j) =max(-.5,min(2.5,gg(i,j)))
               Gx(i,j) =swdown(i,j)/(coszen(i,j)**gg(i,j))
            else
               Bx(i,j) =0.
               bb(i,j) =0.
               Gx(i,j) =0.
               gg(i,j) =0.
            end if

            ! saving last SW run in state variables
            coszen_ref(i,j) =coszen(i,j)
            swdown_ref(i,j) =swdown(i,j)
            swddir_ref(i,j) =swddir(i,j)

            !if ((i.eq.20).and.(j.eq.20)) then
            !   write(wrf_err_message,'("   RADSTEP : tn=",I4," csz_0=",F9.6," csz=",F9.6," csz_1=",F9.6," Gx=",F14.2," gg=",F9.5,  &
            !                           " Bx=",F14.2," bb=",F9.5)') itimestep,coszen_0,coszen_loc(i,j),coszen(i,j),Gx(i,j),gg(i,j), &
            !                           Bx(i,j),bb(i,j)
            !   call wrf_debug(1,wrf_err_message)
            !end if

         end do
      end do

   end subroutine update_swinterp_parameters

   subroutine interp_sw_radiation(ims,ime,jms,jme,its,ite,jts,jte,  &
                                  coszen_ref,coszen_loc,swddir_ref, &
                                  bb,Bx,swdown_ref,gg,Gx,albedo,    &
                                  swdown,swddir,swddni,swddif,gsw   )
      ! Author: jararias 2013/11
      implicit None
      integer, intent(in) :: ims,ime,jms,jme,its,ite,jts,jte
      real, dimension(ims:ime,jms:jme), intent(in) :: coszen_ref,coszen_loc, &
                                                      swddir_ref,Bx,bb,      &
                                                      swdown_ref,Gx,gg,      &
                                                      albedo

      real, dimension(ims:ime,jms:jme), intent(inout) :: swddir,swdown, &
                                                         swddif,swddni, gsw

      integer :: i,j
      real, parameter :: coszen_min=1e-4

      do j=jts,jte
         do i=its,ite
            ! sza interpolation of surface fluxes
            if ((coszen_ref(i,j).gt.coszen_min) .and. (coszen_loc(i,j).gt.coszen_min)) then
               if ((bb(i,j).eq.-0.5).or.(bb(i,j).eq.2.5).or.(bb(i,j).eq.0.0)) then
                  swddir(i,j) =(coszen_loc(i,j)/coszen_ref(i,j))*swddir_ref(i,j)
               else
                  swddir(i,j) =Bx(i,j)*(coszen_loc(i,j)**bb(i,j))
               end if
               if ((gg(i,j).eq.-0.5).or.(gg(i,j).eq.2.5).or.(gg(i,j).eq.0.0)) then
                  swdown(i,j) =(coszen_loc(i,j)/coszen_ref(i,j))*swdown_ref(i,j)
               else
                  swdown(i,j) =Gx(i,j)*(coszen_loc(i,j)**gg(i,j))
               end if
               swddif(i,j) =swdown(i,j)-swddir(i,j)
               swddni(i,j) =swddir(i,j)/coszen_loc(i,j)
               gsw(i,j)    =swdown(i,j)*(1.-albedo(i,j))
            else
               swddir(i,j) =0.
               swdown(i,j) =0.
               swddif(i,j) =0.
               swddni(i,j) =0.
               gsw(i,j)    =0.
            end if
         end do
      end do
   end subroutine interp_sw_radiation

!---------------------------------------------------------------------
!BOP
! !IROUTINE: cal_cldfra2 - Compute cloud fraction
! !INTERFACE:
   SUBROUTINE cal_cldfra2(CLDFRA,QC,QI,F_QC,F_QI,                    &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )
     USE module_state_description, ONLY : KFCUPSCHEME, KFETASCHEME       !CuP, wig 5-Oct-2006  !BSINGH - For WRFCuP scheme 
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
   END SUBROUTINE cal_cldfra2

!BOP
! !IROUTINE: cal_cldfra1 - Compute cloud fraction
! !INTERFACE:
! cal_cldfra_xr - Compute cloud fraction.
! Code adapted from that in module_ra_gfdleta.F in WRF_v2.0.3 by James Done
!!
!!---  Cloud fraction parameterization follows Xu and Randall (JAS), 1996
!!     (see Hong et al., 1998)
!!     (modified by Ferrier, Feb '02)
!
   SUBROUTINE cal_cldfra1(CLDFRA, QV, QC, QI, QS,                    &
                         F_QV, F_QC, F_QI, F_QS, t_phy, p_phy,       &
                         F_ICE_PHY,F_RAIN_PHY,                       &
                         mp_physics, cldfra1_flag,                   &
          ids,ide, jds,jde, kds,kde,                                 &
          ims,ime, jms,jme, kms,kme,                                 &
          its,ite, jts,jte, kts,kte                                  )
     USE module_state_description, ONLY : KFCUPSCHEME, KFETASCHEME       !wig, CuP 4-Fb-2008 !BSINGH - For WRFCuP scheme

#if (HWRF == 1)
   USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT, ETAMP_HWRF 
#else
   USE module_state_description, ONLY : FER_MP_HIRES, FER_MP_HIRES_ADVECT
#endif
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------
   INTEGER,  INTENT(IN   )   ::           ids,ide, jds,jde, kds,kde, &
                                          ims,ime, jms,jme, kms,kme, &
                                          its,ite, jts,jte, kts,kte

!
   INTEGER, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) :: cldfra1_flag
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(OUT  ) ::    &
                                                             CLDFRA

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) ::    &
                                                                 QV, &
                                                                 QI, &
                                                                 QC, &
                                                                 QS, &
                                                              t_phy, &
                                                              p_phy
!                                                              p_phy, &
!                                                          F_ICE_PHY, &
!                                                         F_RAIN_PHY

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                     &
         OPTIONAL,                                                   &
         INTENT(IN   ) ::                                            &
                                                          F_ICE_PHY, &
                                                         F_RAIN_PHY
   LOGICAL,OPTIONAL,INTENT(IN) :: F_QC,F_QI,F_QV,F_QS
   INTEGER :: mp_physics

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

      ifouter: IF ( PRESENT(F_QI) .and. PRESENT(F_QC) .and. PRESENT(F_QS) ) THEN

! mji - For MP options 2, 4, 6, 7, 8, etc. (qc = liquid, qi = ice, qs = snow)
         IF ( F_QI .and. F_QC .and. F_QS) THEN
            QCLD = QI(i,k,j)+QC(i,k,j)+QS(i,k,j)
            IF (QCLD .LT. QCLDMIN) THEN
               weight = 0.
            ELSE
               weight = (QI(i,k,j)+QS(i,k,j)) / QCLD
            ENDIF
         ENDIF

! for P3, mp option 50 or 51
         IF ( F_QI .and. F_QC .and. .not. F_QS) THEN
            QCLD = QI(i,k,j)+QC(i,k,j)
            IF (QCLD .LT. QCLDMIN) THEN
               weight = 0.
            ELSE
               weight = (QI(i,k,j)) / QCLD
            ENDIF
         ENDIF

! mji - For MP options 1 and 3, (qc only)
!  For MP=1, qc = liquid, for MP=3, qc = liquid or ice depending on temperature
         IF ( F_QC .and. .not. F_QI .and. .not. F_QS ) THEN
            QCLD = QC(i,k,j)
            IF (QCLD .LT. QCLDMIN) THEN
               weight = 0.
            ELSE
               if (t_phy(i,k,j) .gt. 273.15) weight = 0.
               if (t_phy(i,k,j) .le. 273.15) weight = 1.
            ENDIF
         ENDIF

! mji - For MP option 5; (qc = liquid, qs = ice)
         IF ( F_QC .and. .not. F_QI .and. F_QS .and. PRESENT(F_ICE_PHY) ) THEN

! Mixing ratios of cloud water & total ice (cloud ice + snow).
! Mixing ratios of rain are not considered in this scheme.
! F_ICE is fraction of ice
! F_RAIN is fraction of rain

           QIMID = QS(i,k,j)
           QWMID = QC(i,k,j)
! old method
!           QIMID = QC(i,k,j)*F_ICE_PHY(i,k,j)
!           QWMID = (QC(i,k,j)-QIMID)*(1.-F_RAIN_PHY(i,k,j))
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
         ENDIF
!BSF - For HWRF MP option; (qc = liquid, qi = cloud ice+snow)
!         IF ( F_QC .and. F_QI .and. .not. F_QS ) THEN
#if (HWRF == 1)
         IF ( mp_physics .eq. FER_MP_HIRES .or. &
	      mp_physics .eq. FER_MP_HIRES_ADVECT .or. &
	      mp_physics .eq. ETAMP_HWRF) THEN
#else
         IF ( mp_physics .eq. FER_MP_HIRES .or. &
              mp_physics==fer_mp_hires_advect) THEN
#endif
           QIMID = QI(i,k,j)     !- total ice (cloud ice + snow)
           QWMID = QC(i,k,j)     !- cloud water
           QCLD=QWMID+QIMID      !- cloud water + total ice
           IF (QCLD .LT. QCLDMIN) THEN
              weight = 0.
           ELSE
              weight = QIMID/QCLD
              if (tc<-40.) weight=1.
           ENDIF
         ENDIF

      ELSE
         CLDFRA(i,k,j)=0.

      ENDIF ifouter !  IF ( F_QI .and. F_QC .and. F_QS)


      QVS_WEIGHT = (1-weight)*QVSW + weight*QVSI
      RHUM=QV(i,k,j)/QVS_WEIGHT   !--- Relative humidity
!
!--- Determine cloud fraction (modified from original algorithm)
!
      cldfra1_flag(i,k,j) = 0
      IF (QCLD .LT. QCLDMIN) THEN
!
!--- Assume zero cloud fraction if there is no cloud mixing ratio
!
        CLDFRA(i,k,j)=0.
        cldfra1_flag(i,k,j) = 1
      ELSEIF(RHUM.GE.RHGRID)THEN
!
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
!
        CLDFRA(i,k,j)=1.
        cldfra1_flag(i,k,j) = 2
      ELSE
         cldfra1_flag(i,k,j) = 3
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

   END SUBROUTINE cal_cldfra1

!+---+-----------------------------------------------------------------+
!..Cloud fraction scheme by G. Thompson (NCAR-RAL), not intended for
!.. combining with any cumulus or shallow cumulus parameterization
!.. scheme cloud fractions.  This is intended as a stand-alone for
!.. cloud fraction and is relatively good at getting widespread stratus
!.. and stratoCu without caring whether any deep/shallow Cu param schemes
!.. is making sub-grid-spacing clouds/precip.  Under the hood, this
!.. scheme follows Mocko and Cotton (1995) in applicaiton of the
!.. Sundqvist et al (1989) scheme but using a grid-scale dependent
!.. RH threshold, one each for land v. ocean points based on
!.. experiences with HWRF testing.
!+---+-----------------------------------------------------------------+
!
!+---+-----------------------------------------------------------------+

      SUBROUTINE cal_cldfra3(CLDFRA, qv, qc, qi, qs, dz,                &
     &                 p, t, XLAND, gridkm,                             &
     &                 modify_qvapor, max_relh,                         &
     &                 kts,kte, debug_flag)
!
      USE module_mp_thompson   , ONLY : rsif, rslf
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: modify_qvapor
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qv, qc, qi, cldfra
      REAL, DIMENSION(kts:kte), INTENT(IN):: p, t, dz, qs
      REAL, INTENT(IN):: gridkm, XLAND, max_relh
      LOGICAL, INTENT(IN):: debug_flag

!..Local vars.
      REAL:: RH_00L, RH_00O, RH_00
      REAL:: entrmnt=0.5
      INTEGER:: k
      REAL:: TC, qvsi, qvsw, RHUM, delz
      REAL, DIMENSION(kts:kte):: qvs, rh, rhoa

      character*512 dbg_msg

!+---+

!..Initialize cloud fraction, compute RH, and rho-air.

         DO k = kts,kte
            CLDFRA(K) = 0.0

            qvsw = rslf(P(k), t(k))
            qvsi = rsif(P(k), t(k))

            tc = t(k) - 273.15
            if (tc .ge. -12.0) then
               qvs(k) = qvsw
            elseif (tc .lt. -35.0) then
               qvs(k) = qvsi
            else
               qvs(k) = qvsw - (qvsw-qvsi)*(-12.0-tc)/(-12.0+35.)
            endif

            rh(k) = MAX(0.01, qv(k)/qvs(k))
            rhoa(k) = p(k)/(287.0*t(k))
         ENDDO


!..First cut scale-aware. Higher resolution should require closer to
!.. saturated grid box for higher cloud fraction.  Simple functions
!.. chosen based on Mocko and Cotton (1995) starting point and desire
!.. to get near 100% RH as grid spacing moves toward 1.0km, but higher
!.. RH over ocean required as compared to over land.

      DO k = kts,kte

         delz = MAX(100., dz(k))
         RH_00L = 0.65 + SQRT(1./(25.0+gridkm*gridkm*delz*0.01))
         RH_00O = 0.81 + SQRT(1./(50.0+gridkm*gridkm*delz*0.01))
         RHUM = rh(k)

         if (qc(k).gt.1.E-7 .or. qi(k).ge.1.E-7                         &
     &                    .or. (qs(k).gt.1.E-6 .and. t(k).lt.273.)) then
            CLDFRA(K) = 1.0
            qvs(k) = qv(k)
         else

            IF ((XLAND-1.5).GT.0.) THEN                                  !--- Ocean
               RH_00 = RH_00O
            ELSE                                                         !--- Land
               RH_00 = RH_00L
            ENDIF

            tc = t(k) - 273.15
            if (tc .lt. -12.0) RH_00 = RH_00L

            if (tc .ge. 20.0) then
               CLDFRA(K) = 0.0
            elseif (tc .ge. -12.0) then
               RHUM = MIN(rh(k), 1.0)
               CLDFRA(K) = MAX(0., 1.0-SQRT((1.005-RHUM)/(1.005-RH_00)))
            else
               if (max_relh.gt.1.12 .or. (.NOT.(modify_qvapor)) ) then
!..For HRRR model, the following look OK.
                  RHUM = MIN(rh(k), 1.45)
                  RH_00 = RH_00 + (1.45-RH_00)*(-12.0-tc)/(-12.0+100.)
             if (RH_00 .ge. 1.5) then
              WRITE (dbg_msg,*) ' FATAL: RH_00 too large (1.5): ', RH_00, RH_00L, tc
              CALL wrf_error_fatal (dbg_msg)
             endif
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.5-RHUM)/(1.5-RH_00)))
               else
!..but for the GFS model, RH is way lower.
                  RHUM = MIN(rh(k), 1.05)
                  RH_00 = RH_00 + (1.05-RH_00)*(-12.0-tc)/(-12.0+100.)
             if (RH_00 .ge. 1.05) then
              WRITE (dbg_msg,*) ' FATAL: RH_00 too large (1.05): ', RH_00, RH_00L, tc
              CALL wrf_error_fatal (dbg_msg)
             endif
                  CLDFRA(K) = MAX(0., 1.0-SQRT((1.05-RHUM)/(1.05-RH_00)))
               endif
            endif
            if (CLDFRA(K).gt.0.) CLDFRA(K) = MAX(0.01, MIN(CLDFRA(K),0.9))

            if (debug_flag) then
              WRITE (dbg_msg,*) 'DEBUG-GT: cloud fraction: ', RH_00, RHUM, CLDFRA(K)
              CALL wrf_debug (150, dbg_msg)
            endif

         endif
      ENDDO


      call find_cloudLayers(qvs, cldfra, T, P, Dz, entrmnt,             &
     &                      debug_flag, qc, qi, qs, kts,kte)

!..Do a final total column adjustment since we may have added more than 1mm
!.. LWP/IWP for multiple cloud decks.

      call adjust_cloudFinal(cldfra, qc, qi, rhoa, dz, kts,kte)

      if (debug_flag) then
        WRITE (dbg_msg,*) 'DEBUG-GT:  Made-up fake profile of clouds'
        CALL wrf_debug (150, dbg_msg)
        do k = kte, kts, -1
           write(dbg_msg,'(f7.2, 2x, f7.2, 2x, f6.4, 2x, f7.3, 2x,  f15.7, 2x, f15.7)') &
     &          T(k)-273.15, P(k)*0.01, rh(k), cldfra(k)*100., qc(k)*1000.,qi(k)*1000.
           CALL wrf_debug (150, dbg_msg)
        enddo
      endif


      if (modify_qvapor) then
         DO k = kts,kte
            if (cldfra(k).gt.0.20 .and. cldfra(k).lt.1.0) then
               qv(k) = qvs(k)
            endif
         ENDDO
      endif


      END SUBROUTINE cal_cldfra3

!+---+-----------------------------------------------------------------+
!..From cloud fraction array, find clouds of multi-level depth and compute
!.. a reasonable value of LWP or IWP that might be contained in that depth,
!.. unless existing LWC/IWC is already there.

      SUBROUTINE find_cloudLayers(qvs1d, cfr1d, T1d, P1d, Dz1d, entrmnt,&
     &                            debugfl, qc1d, qi1d, qs1d, kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts, kte
      LOGICAL, INTENT(IN):: debugfl
      REAL, INTENT(IN):: entrmnt
      REAL, DIMENSION(kts:kte), INTENT(IN):: qs1d,qvs1d,T1d,P1d,Dz1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: cfr1d, qc1d, qi1d

!..Local vars.
      REAL, DIMENSION(kts:kte):: theta
      REAL:: theta1, theta2, delz
      INTEGER:: k, k2, k_tropo, k_m12C, k_cldb, k_cldt, kbot
      LOGICAL:: in_cloud
      character*512 dbg_msg

!+---+

      k_m12C = 0
      DO k = kte, kts, -1
         theta(k) = T1d(k)*((100000.0/P1d(k))**(287.05/1004.))
         if (T1d(k)-273.16 .gt. -12.0 .and. P1d(k).gt.10100.0) k_m12C = MAX(k_m12C, k)
      ENDDO
      if (k_m12C .le. kts) k_m12C = kts

      if (k_m12C.gt.kte-3) then
        WRITE (dbg_msg,*) 'DEBUG-GT: WARNING, no possible way neg12C can occur this high up: ', k_m12C
        CALL wrf_debug (0, dbg_msg)
        do k = kte, kts, -1
           WRITE (dbg_msg,*) 'DEBUG-GT,  k,  P, T : ', k,P1d(k)*0.01,T1d(k)-273.15
           CALL wrf_debug (0, dbg_msg)
        enddo
        call wrf_error_fatal ('FATAL ERROR, problem in temperature profile.')
      endif

!..Find tropopause height, best surrogate, because we would not really
!.. wish to put fake clouds into the stratosphere.  The 10/1500 ratio
!.. d(Theta)/d(Z) approximates a vertical line on typical SkewT chart
!.. near typical (mid-latitude) tropopause height.  Since messy data
!.. could give us a false signal of such a transition, do the check over 
!.. three K-level change, not just a level-to-level check.  This method
!.. has potential failure in arctic-like conditions with extremely low
!.. tropopause height, as would any other diagnostic, so ensure resulting
!.. k_tropo level is above 700hPa.

      DO k = kte-3, kts, -1
         theta1 = theta(k)
         theta2 = theta(k+2)
         delz = dz1d(k) + dz1d(k+1) + dz1d(k+2)
         if ( ((((theta2-theta1)/delz) .lt. 10./1500. ) .AND.       &
     &                 (P1d(k).gt.8500.)) .or. (P1d(k).gt.70000.) ) then
            goto 86
         endif
      ENDDO
 86   continue
      k_tropo = MAX(kts+2, MIN(k+2, kte-1))

      if (k_tropo.gt.kte-2) then
        WRITE (dbg_msg,*) 'DEBUG-GT: CAUTION, tropopause appears to be very high up: ', k_tropo
        CALL wrf_debug (150, dbg_msg)
        do k = kte, kts, -1
           WRITE (dbg_msg,*) 'DEBUG-GT,   P, T : ', k,P1d(k)*0.01,T1d(k)-273.16
           CALL wrf_debug (150, dbg_msg)
        enddo
      elseif (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: FOUND TROPOPAUSE k=', k_tropo
        CALL wrf_debug (150, dbg_msg)
      endif

!..Eliminate possible fractional clouds above supposed tropopause.
      DO k = k_tropo+1, kte
         if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) then
            cfr1d(k) = 0.
         endif
      ENDDO

!..We would like to prevent fractional clouds below LCL in idealized
!.. situation with deep well-mixed convective PBL, that otherwise is
!.. likely to get clouds in more realistic capping inversion layer.

      kbot = kts+2
      DO k = kbot, k_m12C
         if ( (theta(k)-theta(k-1)) .gt. 0.025E-3*Dz1d(k)) EXIT
      ENDDO
      kbot = MAX(kts+1, k-2)
      DO k = kts, kbot
         if (cfr1d(k).gt.0.0 .and. cfr1d(k).lt.1.0) cfr1d(k) = 0.
      ENDDO


!..Starting below tropo height, if cloud fraction greater than 1 percent,
!.. compute an approximate total layer depth of cloud, determine a total 
!.. liquid water/ice path (LWP/IWP), then reduce that amount with tuning 
!.. parameter to represent entrainment factor, then divide up LWP/IWP
!.. into delta-Z weighted amounts for individual levels per cloud layer. 

      k_cldb = k_tropo
      in_cloud = .false.
      k = k_tropo
      DO WHILE (.not. in_cloud .AND. k.gt.k_m12C+1)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, k_m12C, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.k_m12C) then
                  k_cldb = k2+1
                  goto 87
               endif
            ENDDO
 87         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: An ice cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            call adjust_cloudIce(cfr1d, qi1d, qs1d, qvs1d, T1d, Dz1d,   &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A single-layer ice cloud layer is found on ', k_cldb, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qi1d(k_cldb)=0.05*qvs1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO


      k_cldb = k_m12C + 3
      in_cloud = .false.
      k = k_m12C + 2
      DO WHILE (.not. in_cloud .AND. k.gt.kbot)
         k_cldt = 0
         if (cfr1d(k).ge.0.01) then
            in_cloud = .true.
            k_cldt = MAX(k_cldt, k)
         endif
         if (in_cloud) then
            DO k2 = k_cldt-1, kbot, -1
               if (cfr1d(k2).lt.0.01 .or. k2.eq.kbot) then
                  k_cldb = k2+1
                  goto 88
               endif
            ENDDO
 88         continue
            in_cloud = .false.
         endif
         if ((k_cldt - k_cldb + 1) .ge. 2) then
      if (debugfl) then
        WRITE (dbg_msg,*) 'DEBUG-GT: A water cloud layer is found between ', k_cldt, k_cldb, P1d(k_cldt)*0.01, P1d(k_cldb)*0.01
        CALL wrf_debug (150, dbg_msg)
      endif
            call adjust_cloudH2O(cfr1d, qc1d, qvs1d, T1d, Dz1d,         &
     &                           entrmnt, k_cldb,k_cldt,kts,kte)
            k = k_cldb
         elseif ((k_cldt - k_cldb + 1) .eq. 1) then
            if (cfr1d(k_cldb).gt.0.and.cfr1d(k_cldb).lt.1.)             &
     &               qc1d(k_cldb)=0.05*qvs1d(k_cldb)
            k = k_cldb
         endif
         k = k - 1
      ENDDO


      END SUBROUTINE find_cloudLayers

!+---+-----------------------------------------------------------------+

      SUBROUTINE adjust_cloudIce(cfr,qi,qs,qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qs, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qi
      REAL:: iwc, max_iwc, tdz, this_iwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_iwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_iwc = ', max_iwc, ' over DZ=',tdz

      do k = k1, k2
         max_iwc = MAX(1.E-6, max_iwc - (qi(k)+qs(k)))
      enddo
      max_iwc = MIN(1.E-3, max_iwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_iwc = max_iwc*this_dz/tdz
         iwc = MAX(1.E-6, this_iwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.203.16) then
            qi(k) = qi(k) + cfr(k)*cfr(k)*iwc
         endif
      enddo

      END SUBROUTINE adjust_cloudIce

!+---+-----------------------------------------------------------------+

      SUBROUTINE adjust_cloudH2O(cfr, qc, qvs,T,dz,entr, k1,k2,kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: k1,k2, kts,kte
      REAL, INTENT(IN):: entr
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, qvs, T, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc
      REAL:: lwc, max_lwc, tdz, this_lwc, this_dz
      INTEGER:: k

      tdz = 0.
      do k = k1, k2
         tdz = tdz + dz(k)
      enddo
      max_lwc = ABS(qvs(k2)-qvs(k1))
!     print*, ' max_lwc = ', max_lwc, ' over DZ=',tdz

      do k = k1, k2
         max_lwc = MAX(1.E-6, max_lwc - qc(k))
      enddo
      max_lwc = MIN(1.E-3, max_lwc)

      this_dz = 0.0
      do k = k1, k2
         if (k.eq.k1) then
            this_dz = this_dz + 0.5*dz(k)
         else
            this_dz = this_dz + dz(k)
         endif
         this_lwc = max_lwc*this_dz/tdz
         lwc = MAX(1.E-6, this_lwc*(1.-entr))
         if (cfr(k).gt.0.0.and.cfr(k).lt.1.0.and.T(k).ge.253.16) then
            qc(k) = qc(k) + cfr(k)*cfr(k)*lwc
         endif
      enddo

      END SUBROUTINE adjust_cloudH2O

!+---+-----------------------------------------------------------------+

!..Do not alter any grid-explicitly resolved hydrometeors, rather only
!.. the supposed amounts due to the cloud fraction scheme.

      SUBROUTINE adjust_cloudFinal(cfr, qc, qi, Rho,dz, kts,kte)
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN):: kts,kte
      REAL, DIMENSION(kts:kte), INTENT(IN):: cfr, Rho, dz
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: qc, qi
      REAL:: lwp, iwp, xfac
      INTEGER:: k

      lwp = 0.
      iwp = 0.
      do k = kts, kte
         if (cfr(k).gt.0.0) then
            lwp = lwp + qc(k)*Rho(k)*dz(k)
            iwp = iwp + qi(k)*Rho(k)*dz(k)
         endif
      enddo

      if (lwp .gt. 1.5) then
         xfac = 1.5/lwp
         do k = kts, kte
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qc(k) = qc(k)*xfac
            endif
         enddo
      endif

      if (iwp .gt. 1.5) then
         xfac = 1.5/iwp
         do k = kts, kte
            if (cfr(k).gt.0.0 .and. cfr(k).lt.1.0) then
               qi(k) = qi(k)*xfac
            endif
         enddo
      endif

      END SUBROUTINE adjust_cloudFinal


!+---+-----------------------------------------------------------------+

   SUBROUTINE toposhad_init(ht_shad,ht_loc,shadowmask,nested,iter,   &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )

   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   LOGICAL, INTENT(IN)      :: nested

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad, ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask
   INTEGER, INTENT(IN)      :: iter

! Local variables

   INTEGER :: i, j

 if (iter.eq.1) then

! Initialize shadow mask
   do j=jts,jte
   do i=its,ite
     shadowmask(i,j) = 0
   ENDDO
   ENDDO

! Initialize shading height 

   IF ( nested ) THEN  ! Do not overwrite input from parent domain
     do j=max(jts,jds+2),min(jte,jde-3)
     do i=max(its,ids+2),min(ite,ide-3)
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ELSE
     do j=jts,jte
     do i=its,ite
       ht_shad(i,j) = ht_loc(i,j)-0.001
     ENDDO
     ENDDO
   ENDIF

   IF ( nested ) THEN  ! Check if a shadow exceeding the topography height is available at the lateral domain edge from nesting
     if (its.eq.ids) then
       do j=jts,jte
         if (ht_shad(its,j) .gt. ht_loc(its,j)) then
           shadowmask(its,j) = 1
           ht_loc(its,j) = ht_shad(its,j)
         endif
         if (ht_shad(its+1,j) .gt. ht_loc(its+1,j)) then
           shadowmask(its+1,j) = 1
           ht_loc(its+1,j) = ht_shad(its+1,j)
         endif
       enddo
     endif
     if (ite.eq.ide-1) then
       do j=jts,jte
         if (ht_shad(ite,j) .gt. ht_loc(ite,j)) then
           shadowmask(ite,j) = 1
           ht_loc(ite,j) = ht_shad(ite,j)
         endif
         if (ht_shad(ite-1,j) .gt. ht_loc(ite-1,j)) then
           shadowmask(ite-1,j) = 1
           ht_loc(ite-1,j) = ht_shad(ite-1,j)
         endif
       enddo
     endif
     if (jts.eq.jds) then
       do i=its,ite
         if (ht_shad(i,jts) .gt. ht_loc(i,jts)) then
           shadowmask(i,jts) = 1
           ht_loc(i,jts) = ht_shad(i,jts)
         endif
         if (ht_shad(i,jts+1) .gt. ht_loc(i,jts+1)) then
           shadowmask(i,jts+1) = 1
           ht_loc(i,jts+1) = ht_shad(i,jts+1)
         endif
       enddo
     endif
     if (jte.eq.jde-1) then
       do i=its,ite
         if (ht_shad(i,jte) .gt. ht_loc(i,jte)) then
           shadowmask(i,jte) = 1
           ht_loc(i,jte) = ht_shad(i,jte)
         endif
         if (ht_shad(i,jte-1) .gt. ht_loc(i,jte-1)) then
           shadowmask(i,jte-1) = 1
           ht_loc(i,jte-1) = ht_shad(i,jte-1)
         endif
       enddo
     endif
   ENDIF

 else

! Fill the local topography field at the points next to internal tile boundaries with ht_shad values
! A 2-pt halo has been applied to the ht_shad before the repeated call of this subroutine

   if ((its.ne.ids).and.(its.eq.ips)) then
     do j=jts-2,jte+2
       ht_loc(its-1,j) = max(ht_loc(its-1,j),ht_shad(its-1,j))
       ht_loc(its-2,j) = max(ht_loc(its-2,j),ht_shad(its-2,j))
     enddo
   endif
   if ((ite.ne.ide-1).and.(ite.eq.ipe)) then
     do j=jts-2,jte+2
       ht_loc(ite+1,j) = max(ht_loc(ite+1,j),ht_shad(ite+1,j))
       ht_loc(ite+2,j) = max(ht_loc(ite+2,j),ht_shad(ite+2,j))
     enddo
   endif
   if ((jts.ne.jds).and.(jts.eq.jps)) then
     do i=its-2,ite+2
       ht_loc(i,jts-1) = max(ht_loc(i,jts-1),ht_shad(i,jts-1))
       ht_loc(i,jts-2) = max(ht_loc(i,jts-2),ht_shad(i,jts-2))
     enddo
   endif
   if ((jte.ne.jde-1).and.(jte.eq.jpe)) then
     do i=its-2,ite+2
       ht_loc(i,jte+1) = max(ht_loc(i,jte+1),ht_shad(i,jte+1))
       ht_loc(i,jte+2) = max(ht_loc(i,jte+2),ht_shad(i,jte+2))
     enddo
   endif

 endif

   END SUBROUTINE toposhad_init

   SUBROUTINE toposhad(xlat,xlong,sina,cosa,xtime,gmt,radfrq,declin, &
                       dx,dy,ht_shad,ht_loc,iter,                    &
                       shadowmask,shadlen,                    &
                       ids,ide, jds,jde, kds,kde,                    & 
                       ims,ime, jms,jme, kms,kme,                    &
                       ips,ipe, jps,jpe, kps,kpe,                    &
                       its,ite, jts,jte, kts,kte                     )


   USE module_model_constants

 implicit none

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       ips,ipe, jps,jpe, kps,kpe, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,   INTENT(IN) ::      iter

   REAL, INTENT(IN)      ::        RADFRQ,XTIME,DECLIN,dx,dy,gmt,shadlen

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN)  :: XLAT, XLONG, sina, cosa

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  ::  ht_shad,ht_loc

   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT)  :: shadowmask

! Local variables

   REAL :: pi, xt24, wgt, ri, rj, argu, sol_azi, topoelev, dxabs, tloctm, hrang, xxlat, csza
   INTEGER :: gpshad, ii, jj, i1, i2, j1, j2, i, j



 XT24=MOD(XTIME+RADFRQ*0.5,1440.)
 pi = 4.*atan(1.)
 gpshad = int(shadlen/dx+1.)

 if (iter.eq.1) then  


   j_loop1: DO J=jts,jte
   i_loop1: DO I=its,ite

     TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
     HRANG=15.*(TLOCTM-12.)*DEGRAD
     XXLAT=XLAT(i,j)*DEGRAD
     CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)

     if (csza.lt.1.e-2) then   ! shadow mask does not need to be computed
     shadowmask(i,j) = 0
     ht_shad(i,j) = ht_loc(i,j)-0.001
     goto 120
     endif

! Solar azimuth angle

     argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
     if (argu.gt.1) argu = 1
     if (argu.lt.-1) argu = -1
     sol_azi = sign(acos(argu),sin(HRANG))+pi  ! azimuth angle of the sun
     if (cosa(i,j).ge.0) then
       sol_azi = sol_azi + asin(sina(i,j))  ! rotation towards WRF grid 
     else
       sol_azi = sol_azi + pi - asin(sina(i,j)) 
     endif

! Scan for higher surrounding topography

          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then ! sun is in the northern quarter

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.jpe+3).or.(i1.le.ips-3).or.(i2.ge.ipe+3)) then
!               if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  ! sun is in the eastern quarter
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.ipe+3).or.(j1.le.jps-3).or.(j2.ge.jpe+3)) then
!               if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then ! sun is in the southern quarter
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.jps-3).or.(i1.le.ips-3).or.(i2.ge.ipe+3)) then
!               if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          ! sun is in the western quarter
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.ips-3).or.(j1.le.jps-3).or.(j2.ge.jpe+3)) then
!               if (shadowmask(i,j).eq.0) shadowmask(i,j) = -1
                goto 120
              endif
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 120      continue

   ENDDO i_loop1
   ENDDO j_loop1

 else   ! iteration > 1


   j_loop2: DO J=jts,jte
   i_loop2: DO I=its,ite

!     if (shadowmask(i,j).eq.-1) then  ! this indicates that the search ended at a lateral boundary during iteration 1

       TLOCTM=GMT+XT24/60.+XLONG(i,j)/15.
       HRANG=15.*(TLOCTM-12.)*DEGRAD
       XXLAT=XLAT(i,j)*DEGRAD
       CSZA=SIN(XXLAT)*SIN(DECLIN)+COS(XXLAT)*COS(DECLIN)*COS(HRANG)

       if (csza.lt.1.e-2) then   ! shadow mask does not need to be computed
       shadowmask(i,j) = 0
       ht_shad(i,j) = ht_loc(i,j)-0.001
       goto 220
       endif

! Solar azimuth angle

       argu=(csza*sin(XXLAT)-sin(DECLIN))/(sin(acos(csza))*cos(XXLAT))
       if (argu.gt.1) argu = 1
       if (argu.lt.-1) argu = -1
       sol_azi = sign(acos(argu),sin(HRANG))+pi  ! azimuth angle of the sun
       if (cosa(i,j).ge.0) then
         sol_azi = sol_azi + asin(sina(i,j))  ! rotation towards WRF grid 
       else
         sol_azi = sol_azi + pi - asin(sina(i,j)) 
       endif

! Scan for higher surrounding topography

          if ((sol_azi.gt.1.75*pi).or.(sol_azi.lt.0.25*pi)) then ! sun is in the northern quarter

            do jj = j+1,j+gpshad
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri) 
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.ge.min(jde,jpe+3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.0.75*pi) then  ! sun is in the eastern quarter
            do ii = i+1,i+gpshad
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.ge.min(ide,ipe+3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else if (sol_azi.lt.1.25*pi) then ! sun is in the southern quarter
            do jj = j-1,j-gpshad,-1
              ri = i + (jj-j)*tan(sol_azi)
              i1 = int(ri)
              i2 = i1+1
              wgt = ri-i1
              dxabs = sqrt((dy*(jj-j))**2+(dx*(ri-i))**2)
              if ((jj.le.max(jds-1,jps-3)).or.(i1.le.max(ids-1,ips-3)).or.(i2.ge.min(ide,ipe+3))) goto 220
              topoelev=atan((wgt*ht_loc(i2,jj)+(1.-wgt)*ht_loc(i1,jj)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo

          else                          ! sun is in the western quarter
            do ii = i-1,i-gpshad,-1
              rj = j - (ii-i)*tan(pi/2.+sol_azi)
              j1 = int(rj)
              j2 = j1+1
              wgt = rj-j1
              dxabs = sqrt((dx*(ii-i))**2+(dy*(rj-j))**2)
              if ((ii.le.max(ids-1,ips-3)).or.(j1.le.max(jds-1,jps-3)).or.(j2.ge.min(jde,jpe+3))) goto 220
              topoelev=atan((wgt*ht_loc(ii,j2)+(1.-wgt)*ht_loc(ii,j1)-ht_loc(i,j))/dxabs)
              if (sin(topoelev).ge.csza) then
                shadowmask(i,j) = 1
                ht_shad(i,j) = max(ht_shad(i,j),ht_loc(i,j)+dxabs*(tan(topoelev)-tan(asin(csza))))
              endif
            enddo
          endif

 220      continue
!     endif

   ENDDO i_loop2
   ENDDO j_loop2

 endif ! iteration

   END SUBROUTINE toposhad

SUBROUTINE ozn_time_int(julday,julian,ozmixm,ozmixt,levsiz,num_months,  &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

! adapted from oznint from CAM module
!  input: ozmixm - read from physics_init
! output: ozmixt - time interpolated

!  USE module_ra_cam_support, ONLY : getfactors

   IMPLICIT NONE

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months ),      &
          INTENT(IN   ) ::                                  ozmixm

   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme ),      &
          INTENT(OUT  ) ::                                  ozmixt

   !Local
   REAL      :: intJULIAN
   integer   :: np1,np,nm,m,k,i,j
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real, parameter :: daysperyear = 365.  ! number of days in a year
   real      :: cdayozp, cdayozm
   real      :: fact1, fact2, deltat
   logical   :: finddate
   logical   :: ozncyc
   CHARACTER(LEN=256) :: msgstr

   ozncyc = .true.
   ! JULIAN starts from 0.0 at 0Z on 1 Jan.
   intJULIAN = JULIAN + 1.0       ! offset by one day
! jan 1st 00z is julian=1.0 here
   IJUL=INT(intJULIAN)
!  Note that following will drift.
!    Need to use actual month/day info to compute julian.
   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.

!  do m=1,num_months
   do m=1,12
      if(date_oz(m).gt.intjulian.and..not.finddate) then
        np1=m
        finddate=.true.
      endif
   enddo
   cdayozp=date_oz(np1)

   if(np1.gt.1) then
      cdayozm=date_oz(np1-1)
      np=np1
      nm=np-1
   else
      cdayozm=date_oz(12)
      np=np1
      nm=12
   endif

!  call getfactors(ozncyc,np1, cdayozm, cdayozp,intjulian, &
!                   fact1, fact2)
!
! Determine time interpolation factors.  Account for December-January
! interpolation if dataset is being cycled yearly.
!
   if (ozncyc .and. np1 == 1) then                      ! Dec-Jan interpolation
      deltat = cdayozp + daysperyear - cdayozm
      if (intjulian > cdayozp) then                     ! We are in December
         fact1 = (cdayozp + daysperyear - intjulian)/deltat
         fact2 = (intjulian - cdayozm)/deltat
      else                                              ! We are in January
         fact1 = (cdayozp - intjulian)/deltat
         fact2 = (intjulian + daysperyear - cdayozm)/deltat
      end if
   else
      deltat = cdayozp - cdayozm
      fact1 = (cdayozp - intjulian)/deltat
      fact2 = (intjulian - cdayozm)/deltat
   end if
!
! Time interpolation.
!
      do j=jts,jte
      do k=1,levsiz
      do i=its,ite
            ozmixt(i,k,j) = ozmixm(i,k,j,nm+1)*fact1 + ozmixm(i,k,j,np+1)*fact2
      end do
      end do
      end do

END SUBROUTINE ozn_time_int

SUBROUTINE ozn_p_int(p ,pin, levsiz, ozmixt, o3vmr, &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

!-----------------------------------------------------------------------
!
! Purpose: Interpolate ozone from current time-interpolated values to model levels
!
! Method: Use pressure values to determine interpolation levels
!
! Author: Bruce Briegleb
! WW: Adapted for general use
!
!--------------------------------------------------------------------------
   implicit none
!--------------------------------------------------------------------------
!
! Arguments
!
   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   integer, intent(in) :: levsiz              ! number of ozone layers

   real, intent(in) :: p(ims:ime,kms:kme,jms:jme)   ! level pressures (mks, bottom-up)
   real, intent(in) :: pin(levsiz)        ! ozone data level pressures (mks, top-down)
   real, intent(in) :: ozmixt(ims:ime,levsiz,jms:jme) ! ozone mixing ratio

   real, intent(out) :: o3vmr(ims:ime,kms:kme,jms:jme) ! ozone volume mixing ratio
!
! local storage
!
   real    pmid(its:ite,kts:kte)
   integer i,j                 ! longitude index
   integer k, kk, kkstart, kout! level indices
   integer kupper(its:ite)     ! Level indices for interpolation
   integer kount               ! Counter
   integer ncol, pver

   real    dpu                 ! upper level pressure difference
   real    dpl                 ! lower level pressure difference

   ncol = ite - its + 1
   pver = kte - kts + 1

   do j=jts,jte
!
! Initialize index array
!
!  do i=1, ncol
   do i=its, ite
      kupper(i) = 1
   end do
!
! Reverse the pressure array, and pin is in Pa, the same as model pmid
!
      do k = kts,kte
         kk = kte - k + kts
      do i = its,ite
         pmid(i,kk) = p(i,k,j)
      enddo
      enddo

   do k=1,pver

      kout = pver - k + 1
!     kout = k
!
! Top level we need to start looking is the top level for the previous k
! for all longitude points
!
      kkstart = levsiz
!     do i=1,ncol
      do i=its,ite
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0
!
! Store level indices for interpolation
!
      do kk=kkstart,levsiz-1
!        do i=1,ncol
         do i=its,ite
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do
!
! If all indices for this level have been found, do the interpolation and
! go to the next level
!
         if (kount.eq.ncol) then
!           do i=1,ncol
            do i=its,ite
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               o3vmr(i,kout,j) = (ozmixt(i,kupper(i),j)*dpl + &
                             ozmixt(i,kupper(i)+1,j)*dpu)/(dpl + dpu)
            end do
            goto 35
         end if
      end do
!
! If we've fallen through the kk=1,levsiz-1 loop, we cannot interpolate and
! must extrapolate from the bottom or top ozone data level for at least some
! of the longitude points.
!
!     do i=1,ncol
      do i=its,ite
         if (pmid(i,k) .lt. pin(1)) then
            o3vmr(i,kout,j) = ozmixt(i,1,j)*pmid(i,k)/pin(1)
         else if (pmid(i,k) .gt. pin(levsiz)) then
            o3vmr(i,kout,j) = ozmixt(i,levsiz,j)
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            o3vmr(i,kout,j) = (ozmixt(i,kupper(i),j)*dpl + &
                          ozmixt(i,kupper(i)+1,j)*dpu)/(dpl + dpu)
         end if
      end do

      if (kount.gt.ncol) then
!        call endrun ('OZN_P_INT: Bad ozone data: non-monotonicity suspected')
         call wrf_error_fatal ('OZN_P_INT: Bad ozone data: non-monotonicity suspected')
      end if
35    continue

   end do
   end do

   return
END SUBROUTINE ozn_p_int

SUBROUTINE aer_time_int(julday,julian,aerodm,aerodt,levsiz,num_months,no_src,  &
                              ids , ide , jds , jde , kds , kde ,     &
                              ims , ime , jms , jme , kms , kme ,     &
                              its , ite , jts , jte , kts , kte )

! adapted from oznint from CAM module
!  input: aerodm - read from physics_init
! output: aerodt - time interpolated

!  USE module_ra_cam_support, ONLY : getfactors

   IMPLICIT NONE

   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   )    ::   levsiz, num_months, no_src

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, num_months, no_src ),      &
          INTENT(IN   ) ::                                  aerodm

   INTEGER, INTENT(IN )      ::        JULDAY
   REAL,    INTENT(IN )      ::        JULIAN

   REAL,  DIMENSION( ims:ime, levsiz, jms:jme, no_src ),      &
          INTENT(OUT  ) ::                                  aerodt

   !Local
   REAL      :: intJULIAN
   integer   :: np1,np,nm,m,k,i,j,s
   integer   :: IJUL
   integer, dimension(12) ::  date_oz
   data date_oz/16, 45, 75, 105, 136, 166, 197, 228, 258, 289, 319, 350/
   real, parameter :: daysperyear = 365.  ! number of days in a year
   real      :: cdayozp, cdayozm
   real      :: fact1, fact2, deltat
   logical   :: finddate
   logical   :: ozncyc
   CHARACTER(LEN=256) :: msgstr

   ozncyc = .true.
   ! JULIAN starts from 0.0 at 0Z on 1 Jan.
   intJULIAN = JULIAN + 1.0       ! offset by one day
! jan 1st 00z is julian=1.0 here
   IJUL=INT(intJULIAN)
!  Note that following will drift.
!    Need to use actual month/day info to compute julian.
   intJULIAN=intJULIAN-FLOAT(IJUL)
   IJUL=MOD(IJUL,365)
   IF(IJUL.EQ.0)IJUL=365
   intJULIAN=intJULIAN+IJUL
   np1=1
   finddate=.false.

!  do m=1,num_months
   do m=1,12
      if(date_oz(m).gt.intjulian.and..not.finddate) then
        np1=m
        finddate=.true.
      endif
   enddo
   cdayozp=date_oz(np1)

   if(np1.gt.1) then
      cdayozm=date_oz(np1-1)
      np=np1
      nm=np-1
   else
      cdayozm=date_oz(12)
      np=np1
      nm=12
   endif

!  call getfactors(ozncyc,np1, cdayozm, cdayozp,intjulian, &
!                   fact1, fact2)
!
! Determine time interpolation factors.  Account for December-January
! interpolation if dataset is being cycled yearly.
!
   if (ozncyc .and. np1 == 1) then                      ! Dec-Jan interpolation
      deltat = cdayozp + daysperyear - cdayozm
      if (intjulian > cdayozp) then                     ! We are in December
         fact1 = (cdayozp + daysperyear - intjulian)/deltat
         fact2 = (intjulian - cdayozm)/deltat
      else                                              ! We are in January
         fact1 = (cdayozp - intjulian)/deltat
         fact2 = (intjulian + daysperyear - cdayozm)/deltat
      end if
   else
      deltat = cdayozp - cdayozm
      fact1 = (cdayozp - intjulian)/deltat
      fact2 = (intjulian - cdayozm)/deltat
   end if
!
! Time interpolation.
!
      do s=1, no_src
      do j=jts,jte
      do k=1,levsiz
      do i=its,ite
            aerodt(i,k,j,s) = aerodm(i,k,j,nm,s)*fact1 + aerodm(i,k,j,np,s)*fact2
      end do
      end do
      end do
      end do

END SUBROUTINE aer_time_int

SUBROUTINE aer_p_int(p ,pin, levsiz, aerodt, aerod, no_src, pf, totaod,   &
                     ids , ide , jds , jde , kds , kde ,     &
                     ims , ime , jms , jme , kms , kme ,     &
                     its , ite , jts , jte , kts , kte )

!-----------------------------------------------------------------------
!
! Purpose: Interpolate aerosol from current time-interpolated values to model levels
!
! Method: Use pressure values to determine interpolation levels
!
! Author: Bruce Briegleb
! WW: Adapted for general use
!
!   p:  model level pressure at half levels (Pa, bottom-up)
!   pf: model level pressure at full levles (Pa, bottom-up)
!
!--------------------------------------------------------------------------
   implicit none
!--------------------------------------------------------------------------
!
! Arguments
!
   INTEGER,    INTENT(IN) ::           ids,ide, jds,jde, kds,kde, &
                                       ims,ime, jms,jme, kms,kme, &
                                       its,ite, jts,jte, kts,kte

   integer, intent(in) :: levsiz              ! number of aerosol layers
   integer, intent(in) :: no_src              ! types of aerosol 

   real, intent(in) :: p(ims:ime,kms:kme,jms:jme)
   real, intent(in) :: pf(ims:ime,kms:kme,jms:jme)
   real, intent(in) :: pin(levsiz)        ! aerosol data level pressures (mks, top-down)
   real, intent(in) :: aerodt(ims:ime,levsiz,jms:jme,1:no_src) ! aerosol optical depth

   real, intent(out) :: aerod(ims:ime,kms:kme,jms:jme,1:no_src) ! aerosol optical depth
   real, intent(out) :: totaod(ims:ime,jms:jme)                 ! total aerosol optical depth
!
! local storage
!
   real    pmid(its:ite,kts:kte)
   integer i,j                 ! longitude index
   integer k, kk, kkstart, kout! level indices
   integer kupper(its:ite)     ! Level indices for interpolation
   integer kount               ! Counter
   integer ncol, pver, s

   real    dpu                 ! upper level pressure difference
   real    dpl                 ! lower level pressure difference
   real    dpm                 ! pressure difference in a model layer surrounding half p

   ncol = ite - its + 1
   pver = kte - kts + 1

   do s=1,no_src
   do j=jts,jte
!
! Initialize index array
!
   do i=its, ite
      kupper(i) = 1
   end do
!
! The pressure from incoming data is in hPa and top-down, 
!     while model pressure is in Pa and bottom-up
!
      do k = kts,kte
         kk = kte - k + kts
      do i = its,ite
         pmid(i,kk) = p(i,k,j)*0.01
      enddo
      enddo

   do k=1,pver

      kout = pver - k + 1
!
! Top level we need to start looking is the top level for the previous k
! for all longitude points
!
      kkstart = levsiz
      do i=its,ite
         kkstart = min0(kkstart,kupper(i))
      end do
      kount = 0
!
! Store level indices for interpolation
!
      do kk=kkstart,levsiz-1
         do i=its,ite
            if (pin(kk).lt.pmid(i,k) .and. pmid(i,k).le.pin(kk+1)) then
               kupper(i) = kk
               kount = kount + 1
            end if
         end do
!
! If all indices for this level have been found, do the interpolation and
! go to the next level
!
         if (kount.eq.ncol) then
            do i=its,ite
               dpu = pmid(i,k) - pin(kupper(i))
               dpl = pin(kupper(i)+1) - pmid(i,k)
               dpm = pf(i,kout,j) - pf(i,kout+1,j)
               aerod(i,kout,j,s) = (aerodt(i,kupper(i),j,s)*dpl + &
                             aerodt(i,kupper(i)+1,j,s)*dpu)/(dpl + dpu)
               aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
            end do
            goto 35
         end if
      end do
!
! If we've fallen through the kk=1,levsiz-1 loop, we cannot interpolate and
! must extrapolate from the bottom or top aerosol data level for at least some
! of the longitude points.
!
      do i=its,ite
         if (pmid(i,k) .lt. pin(1)) then
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = aerodt(i,1,j,s)*pmid(i,k)/pin(1)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         else if (pmid(i,k) .gt. pin(levsiz)) then
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = aerodt(i,levsiz,j,s)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         else
            dpu = pmid(i,k) - pin(kupper(i))
            dpl = pin(kupper(i)+1) - pmid(i,k)
            dpm = pf(i,kout,j) - pf(i,kout+1,j)
            aerod(i,kout,j,s) = (aerodt(i,kupper(i),j,s)*dpl + &
                          aerodt(i,kupper(i)+1,j,s)*dpu)/(dpl + dpu)
            aerod(i,kout,j,s) = aerod(i,kout,j,s)*dpm
         end if
      end do

      if (kount.gt.ncol) then
         call wrf_error_fatal ('AER_P_INT: Bad aerosol data: non-monotonicity suspected')
      end if
35    continue

   end do
   end do
   end do

   do j=jts,jte
   do i=its,ite
      totaod(i,j) = 0.
   end do
   end do

   do s=1,no_src
   do j=jts,jte
   do k=1,pver
   do i=its,ite
      totaod(i,j) = totaod(i,j) + aerod(i,k,j,s)
   end do
   end do
   end do
   end do

   return
END SUBROUTINE aer_p_int


!+---+-----------------------------------------------------------------+

      SUBROUTINE gt_aod(p_phy,DZ8W,t_phy,qvapor, nwfa,nifa, taod5503d,  &
     &             ims,ime, jms,jme, kms,kme, its,ite, jts,jte, kts,kte)

      USE module_mp_thompson, only: RSLF

      IMPLICIT NONE

      INTEGER,  INTENT(IN):: ims,ime, jms,jme, kms,kme,                 &
     &                       its,ite, jts,jte, kts,kte

      REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) ::           &
     &                                            t_phy,p_phy, DZ8W,    &
     &                                            qvapor, nifa, nwfa
      REAL,DIMENSION(ims:ime,kms:kme,jms:jme),INTENT(INOUT):: taod5503d
                                                              
      !..Local variables. 

      REAL, DIMENSION(its:ite,kts:kte,jts:jte):: AOD_wfa, AOD_ifa
      REAL:: RH, a_RH,b_RH, rh_d,rh_f, rhoa,qvsat, unit_bext1,unit_bext3
      REAL:: ntemp
      INTEGER :: i, k, j, RH_idx, RH_idx1, RH_idx2, t_idx
      INTEGER, PARAMETER:: rind=8
      REAL, DIMENSION(rind), PARAMETER:: rh_arr =                       &
     &                      (/10., 60., 70., 80., 85., 90., 95., 99.8/)
      REAL, DIMENSION(rind,4,2) :: lookup_tabl                           ! RH, temp, water-friendly, ice-friendly

      lookup_tabl(1,1,1) =  5.73936E-15  
      lookup_tabl(1,1,2) =  2.63577E-12
      lookup_tabl(1,2,1) =  5.73936E-15  
      lookup_tabl(1,2,2) =  2.63577E-12
      lookup_tabl(1,3,1) =  5.73936E-15  
      lookup_tabl(1,3,2) =  2.63577E-12
      lookup_tabl(1,4,1) =  5.73936E-15  
      lookup_tabl(1,4,2) =  2.63577E-12

      lookup_tabl(2,1,1) = 6.93515E-15  
      lookup_tabl(2,1,2) = 2.72095E-12
      lookup_tabl(2,2,1) = 6.93168E-15  
      lookup_tabl(2,2,2) = 2.72092E-12  
      lookup_tabl(2,3,1) = 6.92570E-15  
      lookup_tabl(2,3,2) = 2.72091E-12 
      lookup_tabl(2,4,1) = 6.91833E-15  
      lookup_tabl(2,4,2) = 2.72087E-12

      lookup_tabl(3,1,1) = 7.24707E-15  
      lookup_tabl(3,1,2) = 2.77219E-12
      lookup_tabl(3,2,1) = 7.23809E-15  
      lookup_tabl(3,2,2) = 2.77222E-12
      lookup_tabl(3,3,1) = 7.23108E-15  
      lookup_tabl(3,3,2) = 2.77201E-12
      lookup_tabl(3,4,1) = 7.21800E-15  
      lookup_tabl(3,4,2) = 2.77111E-12

      lookup_tabl(4,1,1) = 8.95130E-15  
      lookup_tabl(4,1,2) = 2.87263E-12
      lookup_tabl(4,2,1) = 9.01582E-15  
      lookup_tabl(4,2,2) = 2.87252E-12
      lookup_tabl(4,3,1) = 9.13216E-15  
      lookup_tabl(4,3,2) = 2.87241E-12
      lookup_tabl(4,4,1) = 9.16219E-15  
      lookup_tabl(4,4,2) = 2.87211E-12

      lookup_tabl(5,1,1) = 1.06695E-14  
      lookup_tabl(5,1,2) = 2.96752E-12
      lookup_tabl(5,2,1) = 1.06370E-14  
      lookup_tabl(5,2,2) = 2.96726E-12
      lookup_tabl(5,3,1) = 1.05999E-14  
      lookup_tabl(5,3,2) = 2.96702E-12
      lookup_tabl(5,4,1) = 1.05443E-14  
      lookup_tabl(5,4,2) = 2.96603E-12

      lookup_tabl(6,1,1) = 1.37908E-14  
      lookup_tabl(6,1,2) = 3.15081E-12
      lookup_tabl(6,2,1) = 1.37172E-14  
      lookup_tabl(6,2,2) = 3.15020E-12
      lookup_tabl(6,3,1) = 1.36362E-14  
      lookup_tabl(6,3,2) = 3.14927E-12
      lookup_tabl(6,4,1) = 1.35287E-14  
      lookup_tabl(6,4,2) = 3.14817E-12

      lookup_tabl(7,1,1) = 2.26019E-14  
      lookup_tabl(7,1,2) = 3.66798E-12
      lookup_tabl(7,2,1) = 2.24435E-14  
      lookup_tabl(7,2,2) = 3.66540E-12
      lookup_tabl(7,3,1) = 2.23254E-14  
      lookup_tabl(7,3,2) = 3.66173E-12
      lookup_tabl(7,4,1) = 2.20496E-14  
      lookup_tabl(7,4,2) = 3.65796E-12

      lookup_tabl(8,1,1) = 4.41983E-13  
      lookup_tabl(8,1,2) = 7.50091E-11
      lookup_tabl(8,2,1) = 3.93335E-13  
      lookup_tabl(8,2,2) = 6.79097E-11
      lookup_tabl(8,3,1) = 3.45569E-13  
      lookup_tabl(8,3,2) = 6.07845E-11
      lookup_tabl(8,4,1) = 2.96971E-13  
      lookup_tabl(8,4,2) = 5.36085E-11     

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               AOD_wfa(i,k,j) = 0.
               AOD_ifa(i,k,j) = 0.
            END DO
         END DO
      END DO

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               rhoa = p_phy(i,k,j)/(287.*t_phy(i,k,j))
               t_idx = MAX(1, MIN(nint(10.999-0.0333*t_phy(i,k,j)),4))
               qvsat = rslf(p_phy(i,k,j),t_phy(i,k,j))
               RH = MIN(99.1, MAX(10.1, qvapor(i,k,j)/qvsat*100.))

               !..Get the index for the RH array element

               if (RH .lt. 60) then
                  RH_idx1 = 1
                  RH_idx2 = 2
               elseif (RH .ge. 60 .AND. RH.lt.80) then
                  a_RH = 0.1
                  b_RH = -4
                  RH_idx = nint(a_RH*RH+b_RH)
                  rh_d = rh-rh_arr(rh_idx)
                  if (rh_d .lt. 0) then
                     RH_idx1 = RH_idx-1
                     RH_idx2 = RH_idx
                  else
                     RH_idx1 = RH_idx
                     RH_idx2 = RH_idx+1
                     if (RH_idx2.gt.rind) then
                        RH_idx2 = rind
                        RH_idx1 = rind-1
                     endif
                  endif
               else
                  a_RH = 0.2
                  b_RH = -12.
                  RH_idx = MIN(rind, nint(a_RH*RH+b_RH))
                  rh_d = rh-rh_arr(rh_idx)
                  if (rh_d .lt. 0) then
                     RH_idx1 = RH_idx-1
                     RH_idx2 = RH_idx
                  else
                     RH_idx1 = RH_idx
                     RH_idx2 = RH_idx+1
                     if (RH_idx2.gt.rind) then
                        RH_idx2 = rind
                        RH_idx1 = rind-1
                     endif
                  endif
               endif

               !..RH fraction to be used

               rh_f = MAX(0., MIN(1.0, (rh/(100-rh)-rh_arr(rh_idx1)     &
     &                                  /(100-rh_arr(rh_idx1)))         &
     &                        /(rh_arr(rh_idx2)/(100-rh_arr(rh_idx2))   &
     &                        -rh_arr(rh_idx1)/(100-rh_arr(rh_idx1))) ))

      
               unit_bext1 = lookup_tabl(RH_idx1,t_idx,1)                &
     &                    + (lookup_tabl(RH_idx2,t_idx,1)               &
     &                    - lookup_tabl(RH_idx1,t_idx,1))*rh_f
               unit_bext3 = lookup_tabl(RH_idx1,t_idx,2)                &
     &                    + (lookup_tabl(RH_idx2,t_idx,2)               &
     &                    - lookup_tabl(RH_idx1,t_idx,2))*rh_f

               ntemp = MAX(1., MIN(99999.E6, nwfa(i,k,j)))
               AOD_wfa(i,k,j) = unit_bext1*ntemp*dz8w(i,k,j)*rhoa

               ntemp = MAX(0.01, MIN(9999.E6, nifa(i,k,j)))
               AOD_ifa(i,k,j) = unit_bext3*ntemp*dz8w(i,k,j)*rhoa

            END DO
         END DO
      END DO

      DO j=jts,jte
         DO k=kts,kte
            DO i=its,ite
               taod5503d(i,k,j) = MAX(1.E-3, aod_wfa(i,k,j) + aod_ifa(i,k,j))
            END DO
         END DO
      END DO

      END SUBROUTINE gt_aod

!+---+-----------------------------------------------------------------+

END MODULE module_radiation_driver
